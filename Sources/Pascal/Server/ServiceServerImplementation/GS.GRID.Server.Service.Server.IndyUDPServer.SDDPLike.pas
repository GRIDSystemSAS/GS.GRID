/// SDDP (uPnp) rely on port 1900. this feature of CnC "mimic" this behaviour.
/// In all case, on startup and on demand,
/// it publish its presence on an UDP Broadcast, pnp compatible frame, on port 1900.
/// It try to open a server on port 1900 : If its succeed, all pnp declaration will be listened and pass to the cnc.
///  In this other case, the grid server is blind to other server : But CnC will be helped by Bonjour feature in this case.
unit GS.GRID.Server.Service.Server.IndyUDPServer.SDDPLike;

{$I GSCore.inc}
{$H+}

interface

Uses
{$IFDEF FPC}
  Classes, SysUtils,
  SyncObjs,
{$ELSE}
  System.Classes, System.SysUtils,
  System.SyncObjs, System.Threading,
{$ENDIF}
  IdSocketHandle, IdUDPServer, IdGlobal,
  IdComponent, IdBaseComponent, IdUDPBase, IdUDPClient, IdStack,
  GS.Bus,
  GS.Threads,
  GS.Bus.Services,
  GS.GRID.Server.Service.Types;

Const
  CST_SERVER_PRODUCT_NAME = 'GRID System - GRIDServer V1.0';
  CST_SSDP_ROOTDEVICE     = 'upnp:rootdevice';
  CST_SSDP_HTTPOK         = 'HTTP/1.1 200 OK';
  CST_LOG_SSDP_PREFIX     = 'SSDP Activity : ';

Type

TGRIDService_IndySSDPLike = Class
Private
  FUDPClient : TIdUDPClient;
  FUDPServer : TIdUDPServer; //Pointer.
  FService : TGridService;
  FLogAllUDPTrafic: boolean;
  FLocalGridServerPort: String; //Pointer.

  Procedure DoSendNetworkPresence(ToIP : String);
  Procedure Initialize;
  function GetLocalGridServerIP: string;
public
  Constructor Create(aService : TGridService; aUDPServer : TIdUDPServer); Reintroduce;
  destructor Destroy; override;

  //To call in a UDP Server OnUDPRead event.
  Procedure PerfomSSDPLikeProcess(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);

  Property LogAllUDPTrafic : boolean read FLogAllUDPTrafic write FLogAllUDPTrafic;
  Property LocalGridServerPort : String read FLocalGridServerPort write FLocalGridServerPort;
  property LocalGridServerIP : string read GetLocalGridServerIP;
End;


implementation

{ TGRIDService_IndySSDPLike }



constructor TGRIDService_IndySSDPLike.Create(aService: TGridService; aUDPServer : TIdUDPServer);
begin
  Assert(assigned(aService));
  Assert(assigned(aUDPServer));
  FLogAllUDPTrafic := {$IFDEF DEBUG}true{$ELSE}false{$ENDIF};
  FLocalGridServerPort := '60000';
  FService := aService;
  FUDPServer := aUDPServer;
  Assert(FUDPServer.Active);
  Initialize;
end;

destructor TGRIDService_IndySSDPLike.Destroy;
begin
  FreeAndNil(FUDPClient);
  Inherited;
end;

procedure TGRIDService_IndySSDPLike.DoSendNetworkPresence(ToIP : String);
var lUDPC : TIdUDPClient;
begin
  lUDPC := TIdUDPClient.Create(nil);
  lUDPC.IPVersion := TIdIPVersion.Id_IPv4;
  Try
    FService.Log('UDPServer Activity : Broadcasting NOTIFY',ClassName);
    //2 SSDP-Like SERVER START : PAquet BROADCAST ssdp:alive.
    lUDPC.Broadcast('NOTIFY * HTTP/1.1' + #13#10 +
       'HOST: 239.255.255.250:1900' + #13#10 +
       'LOCATION: grid://'+ToIP+ #13#10 + //Standart is http://
       'SERVER: '+CST_SERVER_PRODUCT_NAME + #13#10 +
       'NTS: ssdp:alive'+ #13#10 +
       'USN: uuid:'+FService.ServiceID+'::upnp:rootdevice' + #13#10 +
       'CACHE-CONTROL: max-age=1800' + #13#10 +
       'NT: '+CST_SSDP_ROOTDEVICE + #13#10 +
       'Content-Length: 0' + #13#10
       ,1900);
  Finally
    FreeAndNil(lUDPC);
  End;
end;


function TGRIDService_IndySSDPLike.GetLocalGridServerIP: string;
var l : TStringList;
begin
  l := TStringList.Create;
  try
    l.Text := GStack.LocalAddresses.Text;
    if l.Count>0 then
      result := l[0]
    else
      result := GStack.LocalAddress; //send loopback. :/
  finally
    FreeAndNil(l);
  end;
end;

procedure TGRIDService_IndySSDPLike.Initialize;
var tmpstr : String;
begin
  FUDPClient := TIdUDPClient.Create(Nil);
  FUDPClient.IPVersion := TIdIPVersion.Id_IPv4;
  //1 AutoTest Server : Getting Local IP (LOCATION) by broadcast process.
  Try
    //Looking for other servuer on all IP
    //Here, loop on PORT list.
    FService.Log('Broadcasting identity on port 1900...',ClassName);
    FUDPClient.BroadcastEnabled := true;
    FUDPClient.Broadcast(
       CST_SERVER_PRODUCT_NAME + #13#10 +
       FService.ServiceID,
       1900);

    tmpstr:='M-SEARCH * HTTP/1.1'#13#10
          +'HOST: 239.255.255.250:1900'#13#10
          +'MAN: "ssdp:discover"'#13#10
          +'MX: 3'#13#10
          +'ST: upnp:rootdevice'#13#10#13#10;
    FUDPClient.Broadcast(tmpStr,1900);
    FService.Log('Startup done..',ClassName);
  Except
    On e : Exception do
    begin
      FService.Log('EXCEPTION : '+e.Message,ClassName);
      //Terminate;
    end;
  End;

end;


procedure TGRIDService_IndySSDPLike.PerfomSSDPLikeProcess(
  AThread: TIdUDPListenerThread; const AData: TIdBytes;
  ABinding: TIdSocketHandle);
var i : integer;
    t : TStringList;
    linter,li : String;
    localUDPC : TIdUDPClient;
begin
  if ABinding.IPVersion = TIdIPVersion.Id_IPv6 then
    Exit; //For instance; respond only on ipv4 interface.

  //UDP READ.
  linter := BytesToString(aData);

{$IFDEF DEBUG}
  //Just for log....
  if length(linter)>10 then
    li := Copy(linter,1,10)
  else
    li := linter;
  li := ReplaceAll(li,#13,'');
  li := ReplaceAll(li,#10,'');
  FService.Log(CST_LOG_SSDP_PREFIX+IntToStr(Length(aData))+' byte(s) incoming from '+ABinding.PeerIP+':'+IntToStr(ABinding.PeerPort)+' "'+li+'[...]"',ClassName);
{$ENDIF}

  t := TStringList.Create;
  try
    t.Text := linter;
    if FLogAllUDPTrafic then
      FService.Log(t.Text,ClassName);

    if t.Count>0 then
    begin
      begin
//        if FUDPClient.Binding.IP <> ABinding.PeerIP then
        begin
          if t.Count=2 then //GRID Server style signature.
          begin
            if (t[0] = CST_SERVER_PRODUCT_NAME) And
               (t[1] = FService.ServiceID) then
            begin
              //It is us ! Signal our
              DoSendNetworkPresence(ABinding.PeerIP);
            end;
          end;
        end;

        if TextStartsWith(t[0],'NOTIFY * HTTP') then //If M_NOTIFY : Asking to a M_NOTIFY QUERY.
        begin
          FService.Log(CST_LOG_SSDP_PREFIX+'Got NOTIFY from '+ABinding.PeerIP,ClassName);
          //Check if it not myself. :)
          //if not in-memory registered yet, do it. (Record localy Name, product etc etc (image !)
          //HERE : RESEND ALL IN MEMORY OBJECT MAP AS JSON STRING.
        end
        else
        if TextStartsWith(t[0],'M-SEARCH * HTTP') then //If M_SEARCH : answer to a M_SEARCH QUERY (ssdp:discover).
        begin
          FService.Log(CST_LOG_SSDP_PREFIX+'Got M-SEARCH from '+ABinding.PeerIP,ClassName);
          localUDPC := TIdUDPClient.Create(nil);
          try
            localUDPC.Send(ABinding.PeerIP,ABinding.PeerPort,
                CST_SSDP_HTTPOK + #13#10 +
                'ST: '+CST_SSDP_ROOTDEVICE+ #13#10 +
                'EXT:'+ #13#10 +
                'LOCATION: grid://'+LocalGridServerIP+':'+LocalGridServerPort+ #13#10 +
                'SERVER: '+CST_SERVER_PRODUCT_NAME + #13#10 +
                 'USN: uuid:'+FService.ServiceID+'::'+CST_SSDP_ROOTDEVICE+ #13#10 +
                'CACHE-CONTROL: max-age=1800' + #13#10 +
                'Content-Length: 0' +
                #13#10);
              //Here LOOP on GRID functions ?
          finally
            FreeAndNil(localUDPC);
          end;
        end;
      end;
    end;

  finally
    FreeAndNil(t);
  end;
end;


end.
