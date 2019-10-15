unit GS.GRID.Server.Service.Server.IndyUDPServer;
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

uses SysUtils,
     Classes,
     SyncObjs,
     GS.Bus,
     GS.Bus.Services,
     GS.GRID.Server.Service.Types,
     GS.GRID.Server.Service.Server,
     GS.GRID.Server.Service.Server.BasedProtocols,
     GS.GRID.Common.Protocols,
     GS.GRID.Server.Service.Server.IndyUDPServer.SDDPLike,
     IdSocketHandle, IdUDPServer, IdGlobal,
     IdComponent, IdBaseComponent, IdUDPBase, IdUDPClient, IdStack;


Const
  CST_SERVER_PRODUCT_NAME = 'GRID System - GRIDServer V1.0';
  CST_SSDP_ROOTDEVICE     = 'upnp:rootdevice';
  CST_SSDP_HTTPOK         = 'HTTP/1.1 200 OK';

Type

TGRIDServiceIndyUDPServer = class(TGRIDServiceServerBasedProtocol)
private
  FRunTimeClientCount : UInt32;
protected
  FUDPServer : TIdUDPServer;
  FSSDPLike : TGRIDService_IndySSDPLike; //SSDP Like feature.
public
  procedure Initialize; Override;
  procedure Finalize; Override;

  Procedure OnGlobalInstructionIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Override;
  Procedure OnServerLevelInstruction(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Override;


  //WARNING : In the case of indy, TGRIDServiceServer methods will be called
  //          Indy's own thread, *not* in Server context.
  Function InstanciateUser(aData : TObject; aProtocolInstance :  TGRIDProtocol) : TGRIDServerUser; Override;
  Procedure GetIncomingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Override;
  Procedure SetOutgoingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Override;
  Function ComposeErrorString(aData : TObject; anErrorTxt : string) : String; Override;

  //Called in init.
  Procedure LoadConfiguration; Override;
  Function ServerReady : Boolean; Override;

  //server entry point.
  Procedure OnIdUDPServerUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);

  Procedure OnIdServerAfterBind(Sender: TObject);

  Class Function GetDefaultImplementation : TGridService; Override;

  Function AutoTestImplementation : TCustomGridServiveServerTestResult; Override;

end;

implementation

{ TGRIDServiceIndyUDPServer }

function TGRIDServiceIndyUDPServer.AutoTestImplementation: TCustomGridServiveServerTestResult;
begin
  result := nil;
end;

function TGRIDServiceIndyUDPServer.ComposeErrorString(aData: TObject;
  anErrorTxt: string): String;
begin

end;

procedure TGRIDServiceIndyUDPServer.Finalize;
begin
  FreeAndNil(FSSDPLike);
  FreeAndNil(FUDPServer);
  inherited;
end;

class function TGRIDServiceIndyUDPServer.GetDefaultImplementation: TGridService;
begin
  Result := TGRIDServiceIndyUDPServer.Create;
end;

procedure TGRIDServiceIndyUDPServer.GetIncomingStream(aData: TObject;
  var aStream: TMemoryStream; const PrefixByteCount : Boolean = false);
begin
  inherited;

end;

procedure TGRIDServiceIndyUDPServer.Initialize;
var lExcStack : String;
begin
  Inherited; //Do not remove !
  Log('Local UDP Server activating...',ClassName);
  FRunTimeClientCount := 0;
  FUDPServer := TidUDPServer.Create(Nil);
  FUDPServer.IPVersion := TIdIPVersion.Id_IPv4;
  FUDPServer.BroadcastEnabled := True;
  FUDPServer.DefaultPort := 1900; //trying to get door 1900 ;) (In order to detect ssdp activity.
  FUDPServer.ThreadedEvent := True;
  FUDPServer.OnUDPRead := OnIdUDPServerUDPRead;
  FUDPServer.OnAfterBind := OnIdServerAfterBind;
  FUDPServer.Binding.AddMulticastMembership('239.255.255.250');
  try
    FUDPServer.Active := True;
    Log('Local UDP Server activated...',ClassName);
    Log('GUID is '+ServiceID,ClassName);
  Except
    { TODO : replace this bunch of sh*** code by give classic port list, and try all of them : Server on different device certainly succeeded to binf one of them, and so, to be detected on one of them }
    On E : Exception do
    begin
      lExcStack := e.Message;
      FUDPServer.DefaultPort := 60000; //:( trying to get next door 60000.
      try
        FUDPServer.Active := True;
      Except
        On E : Exception do
        begin
          lExcStack := lExcStack + e.Message;
          FUDPServer.DefaultPort := 60000+1; //:( trying to get next door 60000+1.
          try
            FUDPServer.Active := True;
          Except

          end;
        end;
      end;
    end;
  end;

  FSSDPLike := TGRIDService_IndySSDPLike.Create(Self,FUDPServer);
end;

function TGRIDServiceIndyUDPServer.InstanciateUser(aData : TObject; aProtocolInstance: TGRIDProtocol): TGRIDServerUser;
var ui : String;
begin
  inc(FRunTimeClientCount);
  ui := Format('idUdpTcp%d.%d', [NativeInt(aData),NativeInt(FRunTimeClientCount)]);
  result := TGRIDServerUser.Create(ui, aProtocolInstance,TGRIDServerUserTransportLayer.net_udp);
end;

procedure TGRIDServiceIndyUDPServer.LoadConfiguration;
begin
  inherited;

end;

procedure TGRIDServiceIndyUDPServer.OnGlobalInstructionIncoming(Sender: TBusSystem;
   aReader : TBusClientReader; var Packet: TBusEnvelop);
begin
  inherited;

end;

procedure TGRIDServiceIndyUDPServer.OnIdServerAfterBind(Sender: TObject);
var
   n:Integer;
   s : string;
begin
  Log('UDP Binding Local addresses : '+GStack.LocalAddresses.Text,className);
  for n:= 0 to FUDPServer.Bindings.Count-1 do
  begin
    with FUDPServer.Bindings[n] do
    begin
      if (n=0) then
      begin
        FUDPServer.DefaultPort := FUDPServer.Bindings[n].Port;
      end;
      Log('UDP Binding : ( '+ip+':'+IntToStr(Port)+' ) / Peer:' + PeerIP,ClassName);
    end;
  end;
end;

procedure TGRIDServiceIndyUDPServer.OnIdUDPServerUDPRead(
  AThread: TIdUDPListenerThread; const AData: TIdBytes;
  ABinding: TIdSocketHandle);
begin
  if Assigned(FSSDPLike) then
    FSSDPLike.PerfomSSDPLikeProcess(AThread,AData,ABinding);

//Protocol_ServerProcess()
end;

procedure TGRIDServiceIndyUDPServer.OnServerLevelInstruction(Sender: TBusSystem;
   aReader : TBusClientReader; var Packet: TBusEnvelop);
begin
  inherited;

end;


function TGRIDServiceIndyUDPServer.ServerReady: Boolean;
begin
  result := false;
  if Assigned(FUDPServer) then
    result := FUDPServer.Active;
end;

procedure TGRIDServiceIndyUDPServer.SetOutgoingStream(aData: TObject;
  var aStream: TMemoryStream; const PrefixByteCount : Boolean = false);
begin


end;

Initialization

AddToServerServiceClasseRepo(TGRIDServiceIndyUDPServer);


end.
