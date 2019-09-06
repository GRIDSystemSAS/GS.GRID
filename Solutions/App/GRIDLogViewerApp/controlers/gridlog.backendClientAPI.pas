//used by controler to interract wuth backend throughout Bus.
unit gridlog.backendClientAPI;

interface


uses classes, sysutils,
     generics.collections,
     gs.Bus,
     gridlog.types;

const
  CST_CLIENT_NAME = '__';
type

  TGridLogFrontAPIReportError = procedure(sender : TObject; Error : String) of Object;
  TGridLogFrontAPI_OnBackendStatus = procedure(sender : TObject; status : boolean) of Object;

  TGridLogFrontAPI = class
  private
    FCode : TStringList;
    FBackendError: TGridLogFrontAPIReportError;
  protected
    procedure DoBackendError(error :string); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; Override;

    procedure process; virtual; abstract;

    property OnGenericBackendError : TGridLogFrontAPIReportError read FBackendError Write FBackendError;
  end;

  TGridLogFrontAPI_BackendRoot = class(TGridLogFrontAPI)
  private
    FReader : TBusClientReader;
    FBackendStatus: TGridLogFrontAPI_OnBackendStatus;

    procedure backendupdate(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
  protected
    procedure DoBackendStatus(status :boolean); virtual;
    procedure backend_response_process(str : String); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure process; override;

    procedure backend_askStatus;

    property OnBackendStatus : TGridLogFrontAPI_OnBackendStatus read FBackendStatus Write FBackendStatus;
  end;

  TGridLogFrontAPI_BackendGridServer_OnServerDetectResult = procedure(sender : TObject; ServerCount : Integer) of Object;
  TGridLogFrontAPI_BackendGridServer_OnServerNotify = procedure(sender : TObject; server : TGridServerItem) of Object;
  TGridLogFrontAPI_BackendGridServer_OnServerGenericError = procedure(sender : TObject; server : TGridServerItem; error : string) of Object;
  TGridLogFrontAPI_BackendGridServer_OnServerLogResult = procedure(sender : TObject; server : TGridServerItem; log : string) of Object;

  TGridLogFrontAPI_BackendGridServer = class(TGridLogFrontAPI)
  private
    fServer : TObjectList<TGridServerItem>;
    FeatureReader : TBusClientReader;
    FOnNewServer: TGridLogFrontAPI_BackendGridServer_OnServerNotify;
    FOnServerConnected: TGridLogFrontAPI_BackendGridServer_OnServerNotify;
    FOnServerGenericError: TGridLogFrontAPI_BackendGridServer_OnServerGenericError;
    FServerDetectResult: TGridLogFrontAPI_BackendGridServer_OnServerDetectResult;
    FOnServerLogResult: TGridLogFrontAPI_BackendGridServer_OnServerLogResult;

    procedure featureupdate(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
    procedure addServer(servName, servDesc, servIP, servPort : string);
  protected
    procedure DoServerDetectResult(ServerCount : Integer); virtual;
    procedure DoAddServer(Server : TGridServerItem); virtual;
    procedure DoServerConnect(Server : TGridServerItem); virtual;
    procedure DoServerGenericError(Server : TGridServerItem; error : string); virtual;
    procedure DoServerLogResult(Server : TGridServerItem; log : String); virtual;

    procedure backend_gridserverresponse(str, additional : string); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure process; override;

    procedure backend_askGridDetection;
    procedure backend_askGridConnection(host : string; port : Integer; user, pass : string; clientSideId : string);

    property OnServerDetectionResult : TGridLogFrontAPI_BackendGridServer_OnServerDetectResult read FServerDetectResult write FServerDetectResult;
    property OnNewServer : TGridLogFrontAPI_BackendGridServer_OnServerNotify read FOnNewServer Write FOnNewServer;
    property OnServerConnected : TGridLogFrontAPI_BackendGridServer_OnServerNotify read FOnServerConnected Write FOnServerConnected;
    property OnServerError : TGridLogFrontAPI_BackendGridServer_OnServerGenericError read FOnServerGenericError write FOnServerGenericError;
    property OnServerLogResult : TGridLogFrontAPI_BackendGridServer_OnServerLogResult read FOnServerLogResult Write FOnServerLogResult;
  end;


implementation


procedure TGridLogFrontAPI_BackendRoot.backendupdate(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  backend_response_process(Packet.ContentMessage.AsString);
end;

procedure TGridLogFrontAPI_BackendRoot.backend_askStatus;
var mes : TBusMessage;
begin
  mes.FromString('status');
  bus.Send(mes,'backend','','',false,Freader.ClientBusID);
end;


procedure TGridLogFrontAPI_BackendRoot.backend_response_process(str : String);
var lcode : string;
begin
  FCode.Text := str;
  if FCode.Count=0 then
  begin
    DoBackendError('empty code');
    exit;
  end;

  lcode := FCode[0];

  if lcode = 'backend_status' then
  begin
    DoBackendStatus(fcode[1] = 'ok');
  end
  else
  begin
    DoBackendError('unknown code : '+lcode);
  end;
end;


constructor TGridLogFrontAPI_BackendRoot.Create;
begin
  inherited;
  FReader := Bus.Subscribe('backend',backendupdate);
  FReader.ClientBusID := CST_CLIENT_NAME;
end;

destructor TGridLogFrontAPI_BackendRoot.Destroy;
begin
  FreeandNil(Freader);
  inherited;
end;

procedure TGridLogFrontAPI_BackendRoot.DoBackendStatus(status: boolean);
begin
  if Assigned(FBackendStatus) then
    FBackendStatus(Self,status);
end;

procedure TGridLogFrontAPI_BackendRoot.process;
begin
  BusProcessMessages([fReader]);
end;

{ TGridLogFrontAPI }

constructor TGridLogFrontAPI.Create;
begin
  inherited create;
  FCode := TStringList.Create;
end;

destructor TGridLogFrontAPI.destroy;
begin
  FreeAndNil(FCode);
  inherited;
end;

procedure TGridLogFrontAPI.DoBackendError(error: string);
begin
  if Assigned(FBackendError) then
    FBackendError(Self,error);
end;


{ TGridLogFrontAPI_BackendGridServer }


procedure TGridLogFrontAPI_BackendGridServer.addServer(servName, servDesc, servIP, servPort: string);
var l : TGridServerItem;
begin
  l := TGridServerItem.Create;
  l.ServerName := servName;
  l.ServerDesc := servDesc;
  l.ServerHost := servIP;
  l.ServerPort := StrToIntDef(servPort,60000);
  fServer.Add(l);
  DoAddServer(l);
end;

procedure TGridLogFrontAPI_BackendGridServer.backend_askGridConnection(
                                     host: string;
                                     port: Integer;
                                     user, pass : string;
                                     clientSideId : string {clientSideId is preserved by backend and send back});
var mes : TBusMessage;
begin
  mes.FromStrings(['grid_connection','host='+host,'port='+inttoStr(port),'user='+user,'pass='+pass,'cid='+clientSideId]);
  bus.Send(mes,'backend','','',false,FeatureReader.ClientBusID);
end;

procedure TGridLogFrontAPI_BackendGridServer.backend_askGridDetection;
var mes : TBusMessage;
begin
  mes.FromString('grid_detect');
  bus.Send(mes,'backend','','',false,FeatureReader.ClientBusID);
end;

procedure TGridLogFrontAPI_BackendGridServer.backend_gridserverresponse(
  str, additional: string);
var lcode : string;
    i : integer;
    ns : TGridServerItem;
begin
  FCode.Text := str;
  if (FCode.Count=0) and (length(additional)>0) then
  begin
    DoBackendError('empty code');
    exit;
  end;

  lcode := FCode[0];

  if lcode = 'grid_detect_response' then
  begin
    if FCode[1] = 'ok' then
    begin
      if StrToIntDef(FCode[2],0)>-1 then
      begin
        DoServerDetectResult(StrToInt(FCode[2]));
        for I :=0 to StrToInt(FCode[2])-1 do
        begin
          addServer( FCode.Values[inttostr(i)+'_serverName'],
                     FCode.Values[inttostr(i)+'_serverDesc'],
                     FCode.Values[inttostr(i)+'_serverIP'],
                     FCode.Values[inttostr(i)+'_serverPort']);
        end;
      end;
    end
    else
    begin
      if FCode[1] ='err' then
        DoBackendError(Fcode[2]);
    end;
  end
  else
  if lcode = 'grid_connection_response' then
  begin
    if FCode[1] = 'ok' then
    begin
      //HERE : GET cid from message and resolve server throught existing server list.
      if StrToIntDef(Fcode.Values['cid'],-1)>-1 then
      begin
        fServer.Items[StrToInt(Fcode.Values['cid'])].ConnectedChan := Fcode.Values['channename'];
        DoServerConnect(fServer.Items[StrToInt(Fcode.Values['cid'])]);
      end
      else
      begin
        //No cid on backend return means that server is a new one (not detected before).
        addServer('New Server','',Fcode.Values['host'],Fcode.Values['port']);
        fServer.Items[fServer.Count-1].ConnectedChan := Fcode.Values['channename'];
        DoServerConnect(fServer.Items[fServer.Count-1]);
        //DoBackendError('clientId must be propagate (cid="'+Fcode.Values['cid']+'" - not found)');
      end;
    end
    else
    begin
      //HERE : GET cid from message and resolve server throught existing server list.
      DoServerGenericError(nil,FCode[1]+'/'+Fcode[2]);
    end;
  end
  else
  if additional = 'logresult' then
  begin
    DoServerLogResult(fServer.Items[0], trim(FCode.Text));
  end
  else
  begin
    DoBackendError('unknown code : '+lcode);
  end;
end;

constructor TGridLogFrontAPI_BackendGridServer.create;
begin
  inherited;
  fServer := TObjectList<TGridServerItem>.Create;
  FeatureReader := Bus.Subscribe('feature',featureupdate);
  FeatureReader.ClientBusID := CST_CLIENT_NAME;
end;

destructor TGridLogFrontAPI_BackendGridServer.destroy;
begin
  FreeAndNil(fServer);
  freeAndNil(FeatureReader);
  inherited;
end;

procedure TGridLogFrontAPI_BackendGridServer.DoAddServer(Server: TGridServerItem);
begin
  if Assigned(FOnNewServer) then
    FOnNewServer(Self,Server);
end;

procedure TGridLogFrontAPI_BackendGridServer.DoServerConnect(
  Server: TGridServerItem);
begin
  if Assigned(FOnServerConnected) then
    FOnServerConnected(Self,Server);
end;

procedure TGridLogFrontAPI_BackendGridServer.DoServerDetectResult(
  ServerCount: Integer);
begin
  if Assigned(FServerDetectResult ) then
    FServerDetectResult(Self,ServerCount);
end;

procedure TGridLogFrontAPI_BackendGridServer.DoServerGenericError(
  Server: TGridServerItem; error: string);
begin
  if Assigned(FOnServerGenericError) then
    FOnServerGenericError(self,Server,error);
end;

procedure TGridLogFrontAPI_BackendGridServer.DoServerLogResult(
  Server: TGridServerItem; log: String);
begin
  if Assigned(FOnServerLogResult) then
    FOnServerLogResult(self,Server,log);
end;

procedure TGridLogFrontAPI_BackendGridServer.featureupdate(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  backend_gridserverresponse(Packet.ContentMessage.AsString,Packet.AdditionalData);
end;

procedure TGridLogFrontAPI_BackendGridServer.process;
begin
  BusProcessMessages([FeatureReader]);
end;

end.
