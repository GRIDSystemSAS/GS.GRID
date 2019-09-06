unit GS.GRID.Server.Service.Server.BasedProtocols;
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

Uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 {$IFDEF USE_GENERIC}
 Generics.Collections,
 {$ENDIF}
 SyncObjs,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 {$IFDEF USE_GENERIC}
 System.Generics.Collections,
 {$ENDIF}
 System.SyncObjs,
 {$ENDIF}
 GS.Common,
 GS.Stream,
 GS.Threads,
 GS.Bus,
 GS.Bus.Services,
 GS.GRID.Common.Types,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server,
 GS.GRID.Common.Protocols,
 GS.GRID.Server.Service.CentralCnC;

Type
TGRIDServiceServerBasedProtocol = class(TGridServiceServer)
private
protected
  //to be called in Client entry point.
  function Protocol_ServerProcess( aUser : TGRIDServerUser;
                                   aInStream : TMemoryStream;
                                   aOutStream : TMemoryStream) : Boolean; override;

  procedure Initialize; Override;
  procedure Finalize; Override;

Public
  function CNCAuth(aServiceUserId, aUser,aPass : String) : TCentralCNC_Message_AuthAsk;
public
end;

implementation

uses
  //DEV NOTE : Change here the sequence to modify the negociate sequence.
  // i.e. put in first the more used protocol, for faster connection and
  // lower server's ressource consumption.
  GS.GRID.Common.Protocols.KissB                     //Base protocol def for KissB ("Keep It Simple and Stupid Bus" protocol.)
  ,GS.GRID.Server.Service.Server.Protocol.KissB       //Prod ready : KissB protocol, inter language cross platform.
  ,GS.GRID.Common.Protocols.MQTT                      //Base protocol def for MQTT 3.11 handling
  ,GS.GRID.Server.Service.Server.Protocol.MQTT        //MQTT protocol, yet ready inter-language cross platform solution.
{$IFDEF DEBUG}
  ,GS.GRID.Common.Protocols.Example                   //Base protocol def for exemple.
  ,GS.GRID.Server.Service.Server.Protocol.ChatExemple //Dev doc example : Basic chat protocol.
{$ENDIF}
  ;

{ TGRIDServiceServerBasedProtocol }


function TGRIDServiceServerBasedProtocol.CNCAuth(
    aServiceUserId, aUser, aPass: String): TCentralCNC_Message_AuthAsk;
var lMessageToCNC, lMessageCNCAnswer : TBusEnvelop;
    lStream : TMemoryStream;
    temp : TCentralCNC_Message_AuthAsk;
    CncCli : TBusClientReader;
    c : TGUID;
begin
  temp.UserName := aUser;
  temp.password := aPass;
  temp.ServiceUserID := aServiceUserId;
  Result.Agreement := false;
  Result.ServiceUserID := '';
  Result.UserName := '';
  Result.password := '';
  Result.AgreementSessionId := '';

  CreateGUID(c);
  CncCli := self.GridBus.Subscribe('Tempo_'+c.ToString,nil);
  lStream :=  TMemoryStream.Create;
  try
    temp.ToStream(lStream);
    lStream.Position := 0;
    //Ask to CNC service if this user have right.
    lMessageToCNC.TargetChannel := CST_CHANNELNAME_CNC_AUTH_GLOBAL;
    lMessageToCNC.ContentMessage.FromStream(lStream);
    if Self.GridBus.SendAndRecv(CncCli,lMessageToCNC,lMessageCNCAnswer)>0 then
    begin
      lStream.Clear;
      lMessageCNCAnswer.ContentMessage.ToStream(TStream(lStream));
      lStream.Position := 0;
      Result.FromStream(lStream);
    end;
  finally
    FreeAndNil(lStream);
    Self.GridBus.UnSubscribe(CncCli);
    FreeAndNil(CncCli);
  end;
end;


procedure TGRIDServiceServerBasedProtocol.Finalize;
begin
  inherited;
end;

procedure TGRIDServiceServerBasedProtocol.Initialize;
begin
  inherited;
  TGRIDProtocolMQTT_ServerHandling.StartServer := Now;
end;


function TGRIDServiceServerBasedProtocol.Protocol_ServerProcess(
  aUser : TGRIDServerUser; aInStream, aOutStream: TMemoryStream): Boolean;
begin
  result := false;
  aInStream.Position := 0;
  aOutStream.Clear;

  if aUser.Protocol is TGRIDProtocol_MQTT then
  begin
    TGRIDProtocolMQTT_ServerHandling.handling_MQTT_Protocol(Self, aUser, TGRIDProtocol_MQTT(aUser.Protocol),aInStream, aOutStream);
    result := true;
  end
  else
  if  aUser.Protocol is TGRIDProtocol_KissB then
  begin
    TGRIDProtocolKissB_ServerHandling.handling_Proto(Self,aUser, TGRIDProtocol_KissB(aUser.Protocol),aInStream, aOutStream);
    result := true;
  end
{$IFDEF DEBUG}
  else
  if  aUser.Protocol is TGRIDProtocol_ExampleProto_BasicChat then
  begin
    TExampleProto_BasicChat_ServerHandling.handling_Proto(Self,TGRIDProtocol_ExampleProto_BasicChat(aUser.Protocol),aInStream, aOutStream);
    result := true;
  end
  else
  begin
    log('This server does not support this protocol : "'+aUser.Protocol.ProtocolName+'"',ClassName);
  end
{$ENDIF}
 ;
end;



end.
