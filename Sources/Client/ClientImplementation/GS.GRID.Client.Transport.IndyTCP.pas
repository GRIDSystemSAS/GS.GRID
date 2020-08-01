unit GS.GRID.Client.Transport.IndyTCP;

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


interface

{$I GSCore.inc}

uses SysUtils,
     Classes,
     IdContext, IdTCPConnection, IdTCPClient, IdBaseComponent, IdComponent,
     IdSocketHandle, IdGlobal, IdIOHandler, IdStack,
     GS.Stream,
     GS.GRID.Common.Protocols,
     GS.GRID.Client.Transport;

Type

TGRIDTransportIndyTCP = Class(TGRIDTransport)
private
  fCTX : TIdTCPClient;
  FPort: UInt32;
  FHost: String;
public
  Constructor Create(const aHost : String = '127.0.0.1'; const aPort : Integer = 60000); virtual;
  Destructor Destroy; Override;

  procedure Connect; Override;
  procedure Disconnect; Override;
  procedure Send(aStream : TStream); Override;
  procedure Recv(var aStream : TStream;  const aTimeOutInMillisec : Uint32 = INFINITE); Override;

  function connected : Boolean; Override;

  function AsString : string; Override;


  Property Host : String read FHost Write FHost;
  Property Port : UInt32 read FPort Write FPort;
End;


implementation

{ TGRIDTransportIndyTCP }

function TGRIDTransportIndyTCP.AsString: string;
begin
  result := Format('TCP/IP (Indy) - Host : %s Port : %d',[Host,Port]);
end;

procedure TGRIDTransportIndyTCP.Connect;
begin
  fCTX.Connect(FHost,FPort);
end;

function TGRIDTransportIndyTCP.connected: Boolean;
begin
  result := fCTX.Connected;
end;

constructor TGRIDTransportIndyTCP.Create(const aHost : String = '127.0.0.1'; const aPort : Integer = 60000);
begin
  fCTX := TIdTCPClient.Create(Nil);
  FHost := aHost;
  FPort := aPort;
end;

destructor TGRIDTransportIndyTCP.Destroy;
begin
  FreeAndNil(fCtx);
  inherited;
end;

procedure TGRIDTransportIndyTCP.Disconnect;
begin
  fCTX.Disconnect;
end;

procedure TGRIDTransportIndyTCP.Recv(var aStream: TStream; const aTimeOutInMillisec : Uint32 = INFINITE);
begin
  repeat
    if fctx.IOHandler.InputBufferIsEmpty then
    begin
      fctx.IOHandler.CheckForDataOnSource(aTimeOutInMillisec);
      fctx.IOHandler.CheckForDisconnect(True);
    end;
    if Not fctx.IOHandler.InputBufferIsEmpty then
      fctx.IOHandler.ReadStream(aStream); //Recv *With* prefix.
  until fctx.IOHandler.InputBufferIsEmpty;
end;

procedure TGRIDTransportIndyTCP.Send(aStream: TStream);
begin
  fCTX.IOHandler.Write(aStream,0, true); //Send *With* prefix (TCP)
end;

end.

