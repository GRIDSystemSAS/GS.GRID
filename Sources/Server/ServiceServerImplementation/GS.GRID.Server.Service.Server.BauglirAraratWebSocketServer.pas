unit GS.GRID.Server.Service.Server.BauglirAraratWebSocketServer;

{$I GSCore.inc}

interface

uses Classes,
     SysUtils,
     syncobjs,
     customServer2, webSocket2, blcksock,
     GS.Bus,
     GS.Bus.Services,
     GS.GRID.Server.Service.Types,
     GS.GRID.Server.Service.Server,
     GS.GRID.Server.Service.Server.BasedProtocols,
     GS.GRID.Common.Protocols;

Type
  TGRIDWebSocketServer = class(TWebSocketServer)
  public
    constructor Create(aBind: string; aPort: string); Override;

    function GetWebSocketConnectionClass(
      Socket: TTCPCustomConnectionSocket;
      Header: TStringList;
      ResourceName, Host, Port, Origin, Cookie: string;
      out HttpResult: integer;
      var Protocol, Extensions: string
    ): TWebSocketServerConnections; override;
  end;

  TGridWebSocketServerConnection = class(TWebSocketServerConnection)
  private
  protected
    FUser: TGridServerUser;
  public
    constructor Create(aSocket: TTCPCustomConnectionSocket); override;

    property ReadFinal: boolean read fReadFinal;
    property ReadRes1: boolean read fReadRes1;
    property ReadRes2: boolean read fReadRes2;
    property ReadRes3: boolean read fReadRes3;
    property ReadCode: integer read fReadCode;
    property ReadStream: TMemoryStream read fReadStream;

    property WriteFinal: boolean read fWriteFinal;
    property WriteRes1: boolean read fWriteRes1;
    property WriteRes2: boolean read fWriteRes2;
    property WriteRes3: boolean read fWriteRes3;
    property WriteCode: integer read fWriteCode;
    property WriteStream: TMemoryStream read fWriteStream;
    Property ServerUser : TGridServerUser read FUser Write FUser;
 end;

TGRIDServiceWebSocketServer = class(TGRIDServiceServerBasedProtocol)
protected
  fServer : TGRIDWebSocketServer;
public
  procedure Initialize; Override;
  procedure Finalize; Override;

  Procedure OnGlobalInstructionIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Override;
  Procedure OnServerLevelInstruction(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Override;

  Function GetUserId(aData : TObject) : String; Override;
  Function GetUserFromDataObject(aData : TObject; var User : TGRIDServerUser) : Boolean; Override;
  procedure SetUserToDataObject(aData : TObject; User : TGRIDServerUser); Override;
  Function InstanciateUser(Const aUserID : UTF8String; aProtocolInstance :  TGRIDProtocol) : TGRIDServerUser; Override;
  Procedure GetIncomingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Override;
  Procedure SetOutgoingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Override;
  Function ComposeErrorString(aData : TObject; anErrorTxt : string) : String; Override;


  //Bauglir Websocket events. Server.
  procedure OnAfterAddConnection(Server: TCustomServer; aConnection: TCustomConnection);
  procedure OnBeforeAddConnection(Server: TCustomServer; aConnection: TCustomConnection; var CanAdd: boolean);
  procedure OnAfterRemoveConnection(Server: TCustomServer; aConnection: TCustomConnection);
  procedure OnBeforeRemoveConnection(Server: TCustomServer; aConnection: TCustomConnection);
  procedure OnServerSocketError(Server: TCustomServer; Socket: TTCPBlockSocket);

  //Bauglir Websocket events. client.
  procedure OnOpen(aSender: TWebSocketCustomConnection);
  procedure OnRead(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
  procedure OnWrite(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
  procedure OnClose(aSender: TWebSocketCustomConnection; aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
  procedure OnConnectionSocket(Sender: TObject; Reason: THookSocketReason; const Value: String);


  Class Function GetDefaultImplementation : TGridService; Override;

  Function AutoTestImplementation : TCustomGridServiveServerTestResult; Override;
end;

implementation
uses synsock, synachar, StrUtils, synautil;

{ TGRIDServiceWebSocketServer }

procedure TGRIDServiceWebSocketServer.Initialize;
begin
  inherited;
  fServer := TGRIDWebSocketServer.Create('127.0.0.1','8080');

  fServer.OnAfterAddConnection := OnAfterAddConnection;
  fServer.OnBeforeAddConnection := OnBeforeAddConnection;
  fServer.OnAfterRemoveConnection := OnAfterRemoveConnection;
  fServer.OnBeforeRemoveConnection := OnBeforeRemoveConnection;
  fServer.OnSocketError := OnServerSocketError;
  fServer.Start;
end;

procedure TGRIDServiceWebSocketServer.Finalize;
begin
  inherited;
  fServer.Stop;
  fServer.TerminateThread;
  //FreeAndNil(Fserver); //FServer is a thread descendant. With FreeOnTerminate enabled.
end;



function TGRIDServiceWebSocketServer.AutoTestImplementation: TCustomGridServiveServerTestResult;
begin
  result := nil;
end;

function TGRIDServiceWebSocketServer.ComposeErrorString(aData: TObject;
  anErrorTxt: string): String;
begin
  result := format('%s / %s',[className,anErrorTxt]);
end;


class function TGRIDServiceWebSocketServer.GetDefaultImplementation: TGridService;
begin
  result := TGRIDServiceWebSocketServer.Create;
end;

procedure TGRIDServiceWebSocketServer.GetIncomingStream(aData: TObject;
  var aStream: TMemoryStream; const PrefixByteCount: Boolean);
begin
  inherited;
  aStream.Clear;
  TGridWebSocketServerConnection(aData).ReadStream.Position := 0;
  aStream.LoadFromStream(TGridWebSocketServerConnection(aData).ReadStream);
//  TPipeServerClient(aData).IncomingStream.Position := 0;
//  aStream.LoadFromStream(TPipeServerClient(aData).IncomingStream);
//  TPipeServerClient(aData).IncomingStream.Clear;

end;

procedure TGRIDServiceWebSocketServer.SetOutgoingStream(aData: TObject;
  var aStream: TMemoryStream; const PrefixByteCount: Boolean);
begin
  inherited;
end;


procedure TGRIDServiceWebSocketServer.SetUserToDataObject(aData: TObject;
  User: TGRIDServerUser);
begin
  Assert(aData is TGridWebSocketServerConnection);
  Assert(Not Assigned(TGridWebSocketServerConnection(aData).ServerUser));
  TGridWebSocketServerConnection(aData).ServerUser := User;
  User.DataObjectA := TGridWebSocketServerConnection(aData);
  User.DataObjectB := Nil;
end;

function TGRIDServiceWebSocketServer.GetUserFromDataObject(aData: TObject;
  var User: TGRIDServerUser): Boolean;
begin
  Assert(aData is TGridWebSocketServerConnection);
  User := nil;
  result := false;
  if Assigned(TGridWebSocketServerConnection(aData).ServerUser) then
  begin
    result := true;
    User := TGridWebSocketServerConnection(aData).ServerUser;
  end;
end;

function TGRIDServiceWebSocketServer.GetUserId(aData: TObject): String;
begin
  result := 'ws'+IntToStr(NativeInt(aData));
end;


function TGRIDServiceWebSocketServer.InstanciateUser(const aUserID: UTF8String;
  aProtocolInstance: TGRIDProtocol): TGRIDServerUser;
begin
  Result := TGRIDServerUser.Create(aUserID, aProtocolInstance,TGRIDServerUserTransportLayer.net_ws);
end;

procedure TGRIDServiceWebSocketServer.OnAfterAddConnection(
  Server: TCustomServer; aConnection: TCustomConnection);
begin
  Log(Format('(%d) %s:%d', [aConnection.Index, aConnection.Socket.GetRemoteSinIP, aConnection.Socket.GetLocalSinPort]),ClassName);
  TWebSocketServerConnection(aConnection).OnWrite := OnWrite;
  TWebSocketServerConnection(aConnection).OnRead := OnRead;
  TWebSocketServerConnection(aConnection).OnClose := OnClose;
  TWebSocketServerConnection(aConnection).OnOpen := OnOpen;
end;

procedure TGRIDServiceWebSocketServer.OnAfterRemoveConnection(
  Server: TCustomServer; aConnection: TCustomConnection);
begin
  Log('WebSocket OnAfterRemoveConnection',ClassName);
end;

procedure TGRIDServiceWebSocketServer.OnBeforeAddConnection(
  Server: TCustomServer; aConnection: TCustomConnection; var CanAdd: boolean);
begin
  Log('WebSocket OnBeforeAddConnection',ClassName);
end;

procedure TGRIDServiceWebSocketServer.OnBeforeRemoveConnection(
  Server: TCustomServer; aConnection: TCustomConnection);
begin
  Log('WebSocket OnBeforeRemoveConnection',ClassName);
end;


procedure TGRIDServiceWebSocketServer.OnConnectionSocket(Sender: TObject;
  Reason: THookSocketReason; const Value: String);
begin
  Log('WebSocket OnConnectionSocket',ClassName);
end;


procedure TGRIDServiceWebSocketServer.OnOpen(
  aSender: TWebSocketCustomConnection);
begin
  Log(Format('OnOpen %d', [aSender.Index]),className);
end;

procedure TGRIDServiceWebSocketServer.OnRead(
  aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean;
  aCode: integer; aData: TMemoryStream);
begin
  //a Client send something.
  log(Format('OnRead %d, final: %d, ext1: %d, ext2: %d, ext3: %d, type: %d, length: %d', [aSender.Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aCode, aData.Size]),ClassName);
  ServerClientExecute(aSender);
end;

procedure TGRIDServiceWebSocketServer.OnClose(
  aSender: TWebSocketCustomConnection; aCloseCode: integer;
  aCloseReason: string; aClosedByPeer: boolean);
begin
  log(Format('OnClose %d, %d, %s, %s', [aSender.Index, aCloseCode, aCloseReason, IfThen(aClosedByPeer, 'closed by peer', 'closed by me')]),className);
end;

procedure TGRIDServiceWebSocketServer.OnWrite(
  aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean;
  aCode: integer; aData: TMemoryStream);
begin
  //Write to the client.
  log(Format('OnWrite %d, final: %d, ext1: %d, ext2: %d, ext3: %d, type: %d, length: %d', [aSender.Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aCode, aData.Size]),ClassName);
end;


procedure TGRIDServiceWebSocketServer.OnServerSocketError(Server: TCustomServer;
  Socket: TTCPBlockSocket);
begin
  log(Format('%s - %d (%s)', [FormatDateTime('yyyy-mm-dd hh:nn:ss', now), Socket.LastError, Socket.LastErrorDesc]),className);
end;





procedure TGRIDServiceWebSocketServer.OnGlobalInstructionIncoming(
  Sender: TBusSystem; aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  inherited;

end;

procedure TGRIDServiceWebSocketServer.OnServerLevelInstruction(
  Sender: TBusSystem; aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  inherited;
//Bus
end;

{ TGRIDWebSocketServer }

constructor TGRIDWebSocketServer.Create(aBind, aPort: string);
begin
  inherited Create(aBind,aPort);
  Synchronized := FALSE; //TWebSocketServer is, by default, a GUI synchronised server (?)
end;

function TGRIDWebSocketServer.GetWebSocketConnectionClass(
  Socket: TTCPCustomConnectionSocket; Header: TStringList; ResourceName, Host,
  Port, Origin, Cookie: string; out HttpResult: integer; var Protocol,
  Extensions: string): TWebSocketServerConnections;
begin
  Result:= TGRIDWebSocketServerConnection;
end;


{ TGridWebSocketServerConnection }

constructor TGridWebSocketServerConnection.Create(
  aSocket: TTCPCustomConnectionSocket);
begin
  inherited create(aSocket);
  Synchronized := FALSE;
  FUser := Nil;
end;

Initialization

//Does not work for instance (binary frame problem (continuation not handled correctly.)
//AddToServerServiceClasseRepo(TGRIDServiceWebSocketServer);

end.
