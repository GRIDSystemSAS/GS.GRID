unit GS.GRID.Server.Service.Server.WinNamedPipeServer;
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
     SyncObjs,
     GS.Bus,
     GS.Bus.Services,
     GS.GRID.Server.Service.Types,
     GS.GRID.Server.Service.Server,
     GS.GRID.Server.Service.Server.BasedProtocols,
     GS.GRID.Common.Protocols,
     GS.GRID.Client.transport.NamedPipes,
     uNamedPipesExchange;

Type

TGRIDServiceWinNamedPipeServer = class(TGRIDServiceServerBasedProtocol)
protected
  FServer : TThreadPipeServer;
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

  function DefaultServerAllowPrefixByteCount : Boolean; override;

  

  Procedure LoadConfiguration; Override;

  Function ServerReady : boolean; Override;

  //Warning : Execute in Server thread context.
  Procedure OnServerExecute(Sender : TObject; FromClient : TPipeServerClient; IncommingValue: TStream; OutgoingValue: TStream);
  Procedure OnServerConnect(Sender : TObject; aClient : TPipeServerClient);
  Procedure OnServerDisconnect(Sender : TObject; aClient : TPipeServerClient);

  Procedure Disconnect(aClient :  TPipeServerClient);

  Class Function GetDefaultImplementation : TGridService; Override;

  Function AutoTestImplementation : TCustomGridServiveServerTestResult; Override;
end;

implementation

uses GS.GRID.Server.Tests,
     GS.GRID.Server.Tests.KissB,
     GS.GRID.Client.Example,
     GS.GRID.Client.KissB;


{ TGRIDServiceWinNamedPipeServer }


function TGRIDServiceWinNamedPipeServer.AutoTestImplementation: TCustomGridServiveServerTestResult;
begin
  result := TGSSTestResults_Server.Create(Self);
//  result.Tests.Add(TGSSTestItem_ServerClientKissB.Create(result,TGRIDTransportNamedPipe.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_InfoFeature.Create(result,TGRIDTransportNamedPipe.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_CPUFeature.Create(result,TGRIDTransportNamedPipe.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_SendMessageBasic.Create(result,TGRIDTransportNamedPipe.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_SendMessageBasicStress.Create(result,TGRIDTransportNamedPipe.Create));
end;

function TGRIDServiceWinNamedPipeServer.ComposeErrorString(aData: TObject;
  anErrorTxt: string): String;
begin

end;

function TGRIDServiceWinNamedPipeServer.DefaultServerAllowPrefixByteCount: Boolean;
begin
  result := true;
end;

procedure TGRIDServiceWinNamedPipeServer.Disconnect(aClient: TPipeServerClient);
begin
  FServer.Disconnect(aClient);
end;

procedure TGRIDServiceWinNamedPipeServer.Finalize;
begin
  Inherited;
  FServer.Stop;
  FreeAndNil(FServer);
end;

class function TGRIDServiceWinNamedPipeServer.GetDefaultImplementation: TGridService;
begin
  Result := TGRIDServiceWinNamedPipeServer.Create;
end;

procedure TGRIDServiceWinNamedPipeServer.GetIncomingStream(aData: TObject;
  var aStream: TMemoryStream; const PrefixByteCount : Boolean = false);
begin
  aStream.Clear;
  TPipeServerClient(aData).IncomingStream.Position := 0;
  aStream.LoadFromStream(TPipeServerClient(aData).IncomingStream);
  TPipeServerClient(aData).IncomingStream.Clear;
end;

function TGRIDServiceWinNamedPipeServer.GetUserFromDataObject(aData: TObject;
  var User: TGRIDServerUser): Boolean;
begin
  Assert(aData is TPipeServerClient);
  User := nil;
  result := false;
  if Assigned(TPipeServerClient(aData).Data) then
  begin
    result := true;
    User := TGRIDServerUser(TPipeServerClient(aData).Data);
  end;
end;

procedure TGRIDServiceWinNamedPipeServer.SetUserToDataObject(aData: TObject;
  User: TGRIDServerUser);
begin
  assert(assigned(user));
  assert(aData is TPipeServerClient);
  TPipeServerClient(aData).Data := User;
end;

procedure TGRIDServiceWinNamedPipeServer.Initialize;
begin
  inherited;
  FServer := TThreadPipeServer.Create;
  FServer.OnServerExecute := OnServerExecute;
  FServer.OnServerConnect := OnServerConnect;
  FServer.OnServerDisconnect := OnServerDisconnect;
end;

function TGRIDServiceWinNamedPipeServer.InstanciateUser(aData : TObject; aProtocolInstance: TGRIDProtocol): TGRIDServerUser;
var uid : string;
begin
  assert(aData is TPipeServerClient);
  uid := TPipeServerClient(aData).ClientId;
  Result := TGRIDServerUser.Create(uid,aProtocolInstance,TGRIDServerUserTransportLayer.net_pipe);
end;

procedure TGRIDServiceWinNamedPipeServer.LoadConfiguration;
begin
  inherited;

end;

procedure TGRIDServiceWinNamedPipeServer.OnGlobalInstructionIncoming(
  Sender: TBusSystem;  aReader : TBusClientReader; var Packet: TBusEnvelop);
begin
  inherited;

end;

procedure TGRIDServiceWinNamedPipeServer.OnServerConnect(Sender: TObject;
  aClient: TPipeServerClient);
begin
  //...
end;

procedure TGRIDServiceWinNamedPipeServer.OnServerDisconnect(Sender: TObject;
  aClient: TPipeServerClient);
begin
  //...
end;

procedure TGRIDServiceWinNamedPipeServer.OnServerExecute(Sender: TObject;
  FromClient: TPipeServerClient; IncommingValue, OutgoingValue: TStream);
var lError : String;
begin
  try
    try
      IncommingValue.Position := 0;
      FromClient.IncomingStream.CopyFrom(IncommingValue,IncommingValue.Size);
      FromCLient.OutgoingStream.Clear;
      ServerClientExecute(FromClient);
      TMemoryStream(OutgoingValue).Clear;
      FromCLient.OutgoingStream.SaveToStream(OutgoingValue);
    Except
      On E : Exception do
      begin
        lError := ComposeErrorString(FromClient,E.Message);
        MasterThread.DoLog(lError);
        FServer.Disconnect(FromClient);
      end;
    end;
  finally
  end;
end;

procedure TGRIDServiceWinNamedPipeServer.OnServerLevelInstruction(
  Sender: TBusSystem;  aReader : TBusClientReader; var Packet: TBusEnvelop);
begin
  inherited;

end;


function TGRIDServiceWinNamedPipeServer.ServerReady: boolean;
begin
  result := false;
  if Assigned(FServer) then
    result := FServer.Ready;
end;

procedure TGRIDServiceWinNamedPipeServer.SetOutgoingStream(aData: TObject;
  var aStream: TMemoryStream; const PrefixByteCount : Boolean = false);
begin
  TPipeServerClient(aData).OutgoingStream.Clear;
  aStream.Position := 0;
  TPipeServerClient(aData).OutgoingStream.loadFromStream(aStream);
end;


{ TGSSTestItem_ServerClientConnect }
{
function TGSSTestItem_ServerClientConnect.InternalExecute: Boolean;
var aClient : TGRIDTransportNamedPipe;
begin
  aClient := TGRIDTransportNamedPipe.Create;
  try
    aClient.Connect;
    result := aClient.Connected;
    aClient.Disconnect;
  finally
    FreeAndNil(aClient);
  end;
end;

function TGSSTestItem_ServerClientConnect.TitleOfItem: String;
begin
  result := 'NamedPipe : Pure transport test. (connect)';
end;
}
Initialization

//Manually added : test only.
//AddToServerServiceClasseRepo(TGRIDServiceWinNamedPipeServer);

end.
