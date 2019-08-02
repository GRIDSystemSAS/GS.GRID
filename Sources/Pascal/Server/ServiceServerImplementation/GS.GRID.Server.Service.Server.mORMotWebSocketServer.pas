//GRID WebSocket feature : mORMot implementation.
//https://github.com/synopse/mORMot
//-> Be lazy, tested with http://www.lazyengineers.com/mqtt-websocket/ ;)
//-> tested with http://www.espert.io/mqtt/index.html (easyiest one, imho)
//-> HiveMQ http://www.hivemq.com/demos/websocket-client/
unit GS.GRID.Server.Service.Server.mORMotWebSocketServer;
//
///
///  *** LICENSE ****
///  Version: MPL 2
///
///  The contents of this file are subject to the Mozilla Public License Version
///  1.1 (the "License"); you may not use this file except in compliance with
///  the License. You may obtain a copy of the License at
///  http://www.mozilla.org/MPL
///
///  Software distributed under the License is distributed on an "AS IS" basis,
///  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
///  for the specific language governing rights and limitations under the License.
///
///  Portions created by the Initial Developer are Copyright (C) 2019
///  the Initial Developer. All Rights Reserved.
///
///  This license applies only to files bearing this header and does not apply
///  to files under "third party" directories (which are the creation of other
///  authors and may be under other licenses) or if file has not,
///  or has another, license header.
///  *** LICENSE ****
//

//
///
/// Promoted and sponsored by GRID SYSTEM S.A.S, France
/// VincentOnGitHub (at) grids.systems
///
//

{$I GSCore.inc}

interface

uses Classes,
     SysUtils,
     syncobjs,
     SynCommons,
     SynCrtSock,
     SynBidirSock,
     GS.Bus,
     GS.Bus.Services,
     GS.GRID.Server.Service.Types,
     GS.GRID.Server.Service.Server,
     GS.GRID.Server.Service.Server.BasedProtocols,
     GS.GRID.Common.Protocols;

Type
TGRIDServiceWebSocketServer = class;

//Taking from sample, and it is cool :)
TWebSocketProtocolEcho = class(TWebSocketProtocolChat)
protected
  procedure EchoFrame(Sender: THttpServerResp; const Frame: TWebSocketFrame);
end;

//MQTT31 protocol wrapper.
TWebSocketProtocolmqtt31 = class(TWebSocketProtocolChat)
protected
  procedure mqttFrame(Sender: THttpServerResp; const Frame: TWebSocketFrame);
end;

TGRIDWebSocketResp = class(TWebSocketServerResp)
private
protected
  FUser: TGRIDServerUser;
  FService: TGRIDServiceWebSocketServer;
public
  CurrentFrame : TWebSocketFrame;
  constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer); override;
  destructor destroy;override;

  property ServerUser : TGRIDServerUser read FUser Write FUser;
  property Service : TGRIDServiceWebSocketServer read FService;
end;

TGRIDWebSocketServer = class(TWebSocketServer)
protected
  FService: TGRIDServiceWebSocketServer;
public
  constructor Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
    const ProcessName: SockString; ServerThreadPoolCount: integer=32;
     KeepAliveTimeOut: integer=3000); Override;

  property Service : TGRIDServiceWebSocketServer read FService write FService;
end;

TGRIDServiceWebSocketServer = class(TGRIDServiceServerBasedProtocol)
protected
  FRunTimeClientCount : Uint32;
  fServer : TGRIDWebSocketServer;
public
  procedure Initialize; Override;
  procedure Finalize; Override;

  Procedure OnGlobalInstructionIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Override;
  Procedure OnServerLevelInstruction(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Override;

  Function GetUserFromDataObject(aData : TObject; var User : TGRIDServerUser) : Boolean; Override;
  procedure SetUserToDataObject(aData : TObject; User : TGRIDServerUser); Override;
  Function InstanciateUser(aData : TObject; aProtocolInstance :  TGRIDProtocol) : TGRIDServerUser; Override;
  Procedure GetIncomingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Override;
  Procedure SetOutgoingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Override;
  Function ComposeErrorString(aData : TObject; anErrorTxt : string) : String; Override;

  Class Function GetDefaultImplementation : TGridService; Override;

  Function AutoTestImplementation : TCustomGridServiveServerTestResult; Override;
end;

implementation

{ TGRIDServiceWebSocketServer }

procedure TGRIDServiceWebSocketServer.Initialize;
var p1 : TWebSocketProtocolEcho;
    p2 : TWebSocketProtocolmqtt31;
begin
  inherited;
  FRunTimeClientCount := 0;
  fServer := TGRIDWebSocketServer.Create('8080',nil,nil,'GRIDwsProc');
  fServer.Service := Self; //linked for the worse and the fort the best.

  p1 := TWebSocketProtocolEcho.Create('meow',''); //"meow" : What a better name for echo funct ? :) f
  p1.OnIncomingFrame := p1.EchoFrame;
  fServer.WebSocketProtocols.Add(p1);
  p2 := TWebSocketProtocolmqtt31.Create('mqttv3.1','mqtt');
  p2.OnIncomingFrame := p2.mqttFrame;
  p2 := TWebSocketProtocolmqtt31.Create('mqtt','');
  p2.OnIncomingFrame := p2.mqttFrame;

  fServer.WebSocketProtocols.Add(p2);
end;

procedure TGRIDServiceWebSocketServer.Finalize;
begin
  inherited;
  fServer.Free;
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
var l : TSynMemoryStream;
begin
  l := TSynMemoryStream.Create(TGRIDWebSocketResp(aData).CurrentFrame.payload);
  try
    l.Position := 0;
    aStream.Clear;
    aStream.LoadFromStream(l);
    aStream.Position := 0;
  finally
    FreeAndNil(l);
  end;
end;

procedure TGRIDServiceWebSocketServer.SetOutgoingStream(aData: TObject;
  var aStream: TMemoryStream; const PrefixByteCount: Boolean);
var tmp: TWebSocketFrame;
    l : TRawByteStringStream;
begin
  l := TRawByteStringStream.Create;
  try
    l.CopyFrom(aStream,aStream.Size);
    TGRIDWebSocketResp(aData).CurrentFrame.payload := l.DataString;
  finally
    FreeandNil(l);
  end;

//  if (self=nil) or (Sender=nil) or Sender.Terminated or
//     not (Frame.opcode in [focText,focBinary]) or
//     ((Sender.Server as TWebSocketServer).IsActiveWebSocket(Sender)<>Sender) then
//    exit;
//  tmp.opcode := frame.opcode;
//  tmp.content := frame.content;
//  SetString(tmp.payload,PAnsiChar(Pointer(frame.payload)),length(frame.payload));
  TGRIDWebSocketResp(aData).fProcess.SendFrame(TGRIDWebSocketResp(aData).CurrentFrame)
end;


procedure TGRIDServiceWebSocketServer.SetUserToDataObject(aData: TObject;
  User: TGRIDServerUser);
begin
  Assert(aData is TGRIDWebSocketResp);
  Assert(Not Assigned(TGRIDWebSocketResp(aData).ServerUser));
  Assert(Assigned(TGRIDWebSocketResp(aData).Service));
  TGRIDWebSocketResp(aData).ServerUser := User;
  User.DataObjectA := TGRIDWebSocketResp(aData);
  User.DataObjectB := Nil;
end;

function TGRIDServiceWebSocketServer.GetUserFromDataObject(aData: TObject;
  var User: TGRIDServerUser): Boolean;
begin
  Assert(aData is THttpServerResp);
  User := nil;
  result := false;
  if Assigned(TGRIDWebSocketResp(aData).ServerUser) then
  begin
    result := true;
    User := TGRIDWebSocketResp(aData).ServerUser;
  end;
end;


function TGRIDServiceWebSocketServer.InstanciateUser(aData : TObject;
  aProtocolInstance: TGRIDProtocol): TGRIDServerUser;
var ui : string;
begin
  inc(FRunTimeClientCount);
  ui := Format('ws%d.%d', [NativeInt(aData),NativeInt(FRunTimeClientCount)]);
  Result := TGRIDServerUser.Create(ui, aProtocolInstance,TGRIDServerUserTransportLayer.net_ws);
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


{ TGIDWebSocketServer }

constructor TGRIDWebSocketServer.Create(const aPort: SockString; OnStart,
  OnStop: TNotifyThreadEvent; const ProcessName: SockString;
  ServerThreadPoolCount, KeepAliveTimeOut: integer);
begin
  inherited;
  fThreadRespClass := TGRIDWebSocketResp;
end;

{ TWebSocketProtocolmqtt31 }

procedure TWebSocketProtocolmqtt31.mqttFrame(Sender: THttpServerResp;
  const Frame: TWebSocketFrame);
var i : Integer;
    lsc : TGRIDWebSocketResp;
begin
  assert(Sender is TGRIDWebSocketResp);
  lsc := TGRIDWebSocketResp(Sender);
  case Frame.opcode of
    focContinuation:
      write('Connected');
    focConnectionClose:
      write('Disconnected');
    focText,focBinary: begin
      Writeln('opcode ',IntToStr(Byte(Frame.opcode)));
      Writeln('tix ',IntToStr(Frame.tix));
      Writeln('payload len ',length(Frame.payload),' bytes');
      lsc.CurrentFrame := Frame;
      lsc.Service.ServerClientExecute(Sender);
    end;
  end;
end;

{ TGRIDWebSocketResp }

constructor TGRIDWebSocketResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer);
begin
  assert(Assigned(aServerSock));
  assert(Assigned(aServer));
  assert(aServer is TGRIDWebSocketServer);
  inherited;
  FUser := Nil;
  FService := TGRIDWebSocketServer(aServer).Service;
end;

destructor TGRIDWebSocketResp.destroy;
begin
  inherited;
end;

{ TWebSocketProtocolEcho }

procedure TWebSocketProtocolEcho.EchoFrame(Sender: THttpServerResp;
  const Frame: TWebSocketFrame);
begin
  TextColor(ccLightMagenta);
  write(GetEnumName(TypeInfo(TWebSocketFrameOpCode),ord(Frame.opcode))^,' - ');
  TextColor(ccWhite);
  case Frame.opcode of
  focContinuation:
    write('Connected');
  focConnectionClose:
    write('Disconnected');
  focText,focBinary: begin
    write('Echoing ',length(Frame.payload),' bytes');
    SendFrame(Sender,Frame);
  end;
  end;
  TextColor(ccCyan);
  writeln(' from ',Sender.ServerSock.RemoteIP,'/',PtrInt(Sender.ServerSock.Sock));
end;

Initialization

AddToServerServiceClasseRepo(TGRIDServiceWebSocketServer);

end.
