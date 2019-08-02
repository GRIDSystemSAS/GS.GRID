unit GS.GRID.Client.Transport.NamedPipes;

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

uses SysUtils,
     Classes,
     uNamedPipesExchange, FWIOCompletionPipes,
     GS.Stream,
     Generics.Collections,
     GS.GRID.Common.Protocols,
     GS.GRID.Client.Transport;

Type

TGRIDTransportNamedPipe = Class(TGRIDTransport)
private
  fCTX : TPipeClient;
  FServerPipeName: String;
  FServerName: String;
  FRCVStream : TMemoryStream;

  Procedure OnConnect(sender : TObject);
  Procedure OnDisconnect(sender : TObject);

public
  Constructor Create; virtual;
  Destructor Destroy; Override;

  procedure Connect; Override;
  procedure Disconnect; Override;
  procedure Send(aStream : TStream); Override;
  procedure Recv(var aStream : TStream; const aTimeOutInMillisec : Uint32 = 0); Override;

  function connected : Boolean; Override;
  function AsString : string; Override;

  Property ServerName : String read FServerName Write FServerName;
  Property ServerPipeName : String read FServerPipeName Write FServerPipeName;
End;


implementation


{ TGRIDTransportNamedPipe }

function TGRIDTransportNamedPipe.AsString: string;
begin
  result := format('Named pipe client Server : %s PipeName : %s',[FServerName,FServerPipeName]);
end;

procedure TGRIDTransportNamedPipe.Connect;
begin
  fCTX.Active := true;
end;

function TGRIDTransportNamedPipe.connected: Boolean;
begin
  result := fCTX.Active;
end;

constructor TGRIDTransportNamedPipe.Create;
begin
  FServerName := '.';
  FServerPipeName := 'LocalhostPipeTest';
  FRCVStream := TMemoryStream.Create;
  fCTX := TPipeClient.Create(FServerName,FServerPipeName);
end;

destructor TGRIDTransportNamedPipe.Destroy;
begin
  FreeAndNil(FRCVStream);
  FreeAndNil(fCTX);
  inherited;
end;

procedure TGRIDTransportNamedPipe.Disconnect;
begin
  fCTX.Active := False;
end;

procedure TGRIDTransportNamedPipe.OnConnect(sender: TObject);
begin
 //Not used.
end;

procedure TGRIDTransportNamedPipe.OnDisconnect(sender: TObject);
begin
  //Not used.
end;

procedure TGRIDTransportNamedPipe.Recv(var aStream: TStream;  const aTimeOutInMillisec : Uint32 = 0);
begin
  FRCVStream.Position := 0;
  aStream.CopyFrom(FRCVStream,FRCVStream.Size);
end;

procedure TGRIDTransportNamedPipe.Send(aStream: TStream);
begin
  FRCVStream.Clear;
  fCTX.SendData(aStream,FRCVStream);
end;

end.
