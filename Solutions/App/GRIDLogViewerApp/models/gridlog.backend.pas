//Full threaded backend. Join from controler/front by Bus.
unit gridlog.backend;

interface

uses classes, sysutils, System.SyncObjs,
     GS.common,
     GS.bus,
     GS.Stream,
     GS.Threads.pool,
     GS.GRID.Common.Protocols.KissB,
     GS.GRID.Common.types,
     GS.GRID.Client,
     GS.GRID.Client.KissB,
     GS.GRID.Client.Resolver,
     GS.GRID.Client.Transport.IndyTCP;


Type
  TBackend = class(TStackTask)
  protected
    fback : TBusClientReader;
    lOrder : TStringList;
  public
    procedure backendupdate(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
    procedure Execute(Worker : TThreadTask); override;
  end;

  //Function

  TGridDetect = class(TStackTask)
    procedure Execute(Worker : TThreadTask); Override;
  end;

  TGridConnection = class(TStackTask)
  protected
    Fclient : TGRIDClientKissB;
    fcontrol : TBusClientReader;
    fhost : String;
    fport : Integer;
    fuser,fpass : string;
    fcid : string;
    fAPIName : string;
  public
    constructor create(host : string; port : integer; user, pass, cid : string); reintroduce;
    procedure controlUpdate(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
    procedure Execute(Worker : TThreadTask); override;
  end;

var
  pool : TStackThreadPool;


  procedure startbackend;
  procedure stopbackend;
implementation

  procedure startbackend;
  begin
    pool := TStackDynamicThreadPool.Create;
    pool.Submit(TBackend.Create);
  end;

  procedure stopbackend;
  begin
    pool.Terminate;
    pool.Free;
  end;

{ TBackend }

procedure TBackend.backendupdate(Sender: TBusSystem; aReader: TBusClientReader;
  var Packet: TBusEnvelop);
var resp : TBusMessage;
    messCode : String;
begin
  lOrder.Text := Packet.ContentMessage.AsString;

  if lOrder.Count=0 then
  begin
    resp.FromStrings(['backend','err','empty code']);
    bus.Send(resp,'backend','','',false,fback.ClientBusID);
    exit
  end;

  messCode := lOrder[0];
  if  messCode = 'status' then
  begin
    resp.FromStrings(['backend_status','ok']);
    bus.Send(resp,'backend','','',false,fback.ClientBusID);
  end
  else
  if messCode = 'grid_detect' then
  begin
    pool.Submit(TGridDetect.Create);
  end
  else
  if messCode = 'grid_connection' then
  begin
    pool.Submit(TGridConnection.Create( lOrder.Values['host'],
                                        strtointdef(lOrder.Values['port'],60000),
                                        lOrder.Values['user'],
                                        lOrder.Values['pass'],
                                        lOrder.Values['cid']));
  end
  else
  begin
    resp.FromStrings(['backend','err','code "'+messcode+'" not found']);
    bus.Send(resp,'backend','','',false,fback.ClientBusID);
  end;
end;

procedure TBackend.Execute(Worker : TThreadTask);
begin
  lOrder := TStringList.Create;
  fback := Bus.Subscribe('backend',backendupdate);
  fback.ClientBusID := 'xxx';
  fback.Event :=  bus.GetNewEvent;

  while Not Worker.Terminated do
  begin
    case fback.Event.WaitFor(250) of
      wrSignaled :
      begin
        BusProcessMessages([fback]); //Listen from view.
      end;
    end;
  end;

  FreeAndNil(fback);
  FreeAndNil(lOrder);
end;


{ TGridDetect }

procedure TGridDetect.Execute(Worker : TThreadTask);
var l : TGridClientGRIDResolver;
    res : TBusMessage;
    i : integer;
begin
  l := TGridClientGRIDResolver.Create;
  try
    try
      l.Resolve;

      with TStringList.Create do
      begin
        add('grid_detect_response');
        add('ok');
        add(IntToStr(l.GRIDServerCount));
        for i := 0 to l.GRIDServerCount-1 do
        begin
          add(Format('%d_serverName=%s',[i,l.GRIDServers[i].GridServerName]));
          add(Format('%d_serverDesc=%s',[i,l.GRIDServers[i].GridServerDesc]));
          add(Format('%d_serverIP=%s',[i,l.GRIDServers[i].GridServerIP]));
          add(Format('%d_serverPort=%s',[i,l.GRIDServers[i].GridServerPort]));
        end;
        res.FromString(Text);
      end;
      bus.Send(res,'feature');

    Except
      On E : Exception do
      begin
        with TStringList.Create do
        begin
          add('grid_detect_response');
          add('error');
          add(E.Message);
          res.FromString(Text);
        end;
        bus.Send(res,'feature');
      end;
    end;
  finally
    FreeAndNil(l);
  end;
end;

{ TGridConnection }

procedure TGridConnection.controlUpdate(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
//
end;

constructor TGridConnection.create(host: string; port: integer; user, pass, cid : string);
begin
  inherited create;
  fhost := host;
  fport := port;
  fuser := user;
  fpass := pass;
  fcid := cid;
  fAPIName := 'grid_connection_response';
end;

procedure TGridConnection.Execute(Worker : TThreadTask);
var mes : TBusMessage;
    lchan : string;
    l : TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE;
    mbox : TGRIDMessages;
    logChunk : TGRIDLogChunk;
    lStream : TMemoryStream;
    ll : string;

    procedure sendInternal;
    var i : integer;
    begin
      for I := 0 to length(mBox)-1 do
      begin
        lStream.Clear;
        BytesToStream(lStream,mbox[i].PayLoad);
        lStream.Position := 0;
        try
          logChunk.Deserialize(lStream);
          ll := FormatDateTime('hhnnsszzzz',logChunk.DateTime) + '/' + Format('[%d]/%d/  %s/%s',[logChunk.ThreadID,Byte(logChunk.Category),logChunk.LogText,logChunk.Module]);
          mes.FromString(ll); //full log.
          bus.Send(mes,'feature','logresult','',false,fcontrol.ClientBusID);
        Except

        end;
      end;

    end;
begin
  Fclient := TGRIDClientKissB.Create(TGRIDTransportIndyTCP.Create(Fhost,Fport));
  l := Fclient.Connect(fuser,fpass);
  if l.Status then
  begin
    Fclient.Subscribe('.GRIDServer.System.Log');

    //connection ok, send dedicated listening channel name.
    lchan := 'GridconnectionControl_'+IntToStr(TThread.CurrentThread.threadid);
    lstream := TMemoryStream.Create;
    fcontrol := Bus.Subscribe(lchan,controlUpdate);
    try
      try
        fcontrol.ClientBusID := 'xxx'+IntToStr(TThread.CurrentThread.threadid);
        fcontrol.Event :=  bus.GetNewEvent;

        mes.FromStrings([fAPIName,'ok','host='+fHost,'port='+IntToStr(fport),'channename='+lchan,'cid='+fcid]);
        bus.Send(mes,'feature','','',false,fcontrol.ClientBusID);

        //begin loop awaiting order.
        while Not Worker.Terminated do
        begin
          case fcontrol.Event.WaitFor(250) of
            wrSignaled :
            begin
              BusProcessMessages([fcontrol]); //Listen from view.
            end;
            wrTimeout :
            begin
              if Fclient.CheckMsg(mBox) then
                sendInternal;

            end;
          end;
        end;
      except
        On E : Exception do
        begin
          mes.FromStrings([fAPIName,'err',E.Message,'cid='+fcid]);
          bus.Send(mes,'feature','','',false,fcontrol.ClientBusID);
        end;
      end;
    finally
      FreeAndNil(fcontrol);
      FreeAndNil(lStream);
    end;
  end
  else
  begin
    mes.FromStrings([fAPIName,'err',l.StatusInfo,'cid='+fcid]);
    bus.Send(mes,'feature'); //fcontrol does not exits here.
  end;
end;

end.
