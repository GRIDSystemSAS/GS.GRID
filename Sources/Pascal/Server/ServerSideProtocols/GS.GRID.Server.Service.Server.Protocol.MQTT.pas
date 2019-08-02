unit GS.GRID.Server.Service.Server.Protocol.MQTT;
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
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server,
 GS.GRID.Common.Protocols,
 GS.GRID.Common.Protocols.MQTT,
 uMQTTEx,
 GS.GRID.Server.Service.Server.BasedProtocols;

Const
  MQTT_SERVER_MINVERSION = 3;

Type
TGRIDServerStat = record
  StartDateTime : TDateTime;
  SYS_broker_subscriptions_count : Int64;
  SYS_broker_publish_messages_sent : Int64;
  SYS_broker_load_publish_dropped_plus : Int64;
  SYS_broker_load_connections_plus : Int64;
  SYS_broker_messages_sent : Int64;
  SYS_broker_clients_active : Int64;
  SYS_broker_retained_messages_count : Int64;
  SYS_broker_version : Int64;
  SYS_broker_messages_received : Int64;
  SYS_broker_clients_inactive : Int64;
  SYS_broker_clients_maximum : Int64;
  SYS_broker_load_messages_received_plus : Int64;
  SYS_broker_load_publish_sent_plus : Int64;
  SYS_broker_bytes_sent : Int64;
  SYS_broker_clients_total : Int64;
  SYS_broker_uptime : Int64;
  SYS_broker_load_publish_received_plus : Int64;
  SYS_broker_connection_hash : Int64;
  SYS_broker_publish_messages_dropped : Int64;
  SYS_broker_bytes_received : Int64;
  SYS_broker_load_bytes_received_plus : Int64;
  SYS_broker_timestamp : Int64;
  SYS_broker_messages_stored : Int64;
  SYS_broker_load_messages_sent_plus : Int64;
  SYS_broker_load_sockets_plus : Int64;
  SYS_broker_messages_inflight : Int64;
  SYS_broker_publish_messages_received : Int64;
  SYS_broker_load_bytes_sent_plus : Int64;
  SYS_broker_changeset : Int64;
  SYS_broker_clients_expired : Int64;
end;


TGRIDProtocolMQTT_ServerHandling = Class
private
  class var MsgId : Int32;
  class var Stats : TGRIDServerStat;

  class procedure setUp(const aParser : TMQTTParser;
                            out aServer : TGRIDServiceServerBasedProtocol;
                            out aClient : TGRIDServerUser;
                            out aClientProto : TGRIDProtocol_MQTT;
                            out aDataOut : TMemoryStream);

  class procedure OnInternalServerConnect (Sender : TObject;
                        Protocol : UTF8String;
                        Version : byte;
                        ClientID,
                        UserName, Password : UTF8String;
                        KeepAlive : Word; Clean : Boolean);

  class procedure OnInternalServerSend( Sender : TObject;
                                        anID : Word;
                                        Retry : integer;
                                        aStream : TMemoryStream);

  class procedure OnInternalServerPublish( Sender : TObject;
                                           anID : Word;
                                           aTopic : UTF8String;
                                           aMessage : RawByteString);


//  class procedure OnInternalServerSubscription( Sender : TObject;
//                                                aTopic : UTF8String;
//                                                var RequestedQos : TMQTTQOSType);
  class procedure OnInternalServerSub( Sender : TObject; anID : Word;
                                      Topics : TStringList);
  class procedure OnInternalServerUnsub( Sender : TObject; anID : Word;
                                      Topics : TStringList);

  class procedure OnInternalServerHeader( Sender : TObject;
                                               MsgType: TMQTTMessageType;
                                               Dup: Boolean;
                                               Qos: TMQTTQOSType;
                                               Retain: Boolean);

  class procedure OnInternalServerMon( Sender : TObject; aStr : string);

  class procedure OnInternalServerSetWill( Sender : TObject;
                                        aTopic, aMessage : UTF8String;
                                        aQos : TMQTTQOSType;
                                        aRetain : boolean);

  class procedure OnInternalServerPing(Sender: TObject);
  class procedure OnInternalServerDisconnect(Sender: TObject);

protected
public
  class Var StartServer : TDateTime;
  class procedure handling_MQTT_Protocol( const Server : TGRIDServiceServerBasedProtocol;
                                          aUser : TGridServerUser;
                                          aProtocol: TGRIDProtocol_MQTT;
                                          aDataStream,
                                          aResultStream : TMemoryStream);

  //Message dispatching. Client --Message-->Server/broker --->MessageS ---> OtherClientOnSameTopics
  class procedure OnMQTTMessage(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);

  //Publish Mosquito compatible data on Server stats..
  class procedure PublishStat(aServer : TGRIDServiceServerBasedProtocol);
End;


implementation


uses GS.GRID.Server.Service.CentralCnC; //User accreditation system.



{ TGRIDProtocolMQTT_ServerHandling }

class procedure TGRIDProtocolMQTT_ServerHandling.handling_MQTT_Protocol(
  const Server: TGRIDServiceServerBasedProtocol; aUser : TGridServerUser;
  aProtocol: TGRIDProtocol_MQTT;
  aDataStream, aResultStream: TMemoryStream);
begin
  aProtocol.Parser.DataServer := Server;
  aProtocol.Parser.DataClient := aUser;
  aProtocol.Parser.DataOut := aResultStream;
  Assert(aResultStream.Size = 0);
  aDataStream.Position := 0;

  if aProtocol.Parser.ClientID<>'' then
  begin
    BusProcessMessages(aUser.ChannelsReading.ToArray);
    { TODO : cool down here if there are no message... Modifiy bus api ? }
  end
  else
  begin
    //Only on connect. (init)
    aProtocol.Parser.OnConnect := OnInternalServerConnect;
    aProtocol.Parser.OnSend := OnInternalServerSend;
    aProtocol.Parser.OnPublish := OnInternalServerPublish;
    aProtocol.Parser.OnSubscribe := OnInternalServerSub;
    aProtocol.Parser.OnUnsubscribe := OnInternalServerUnsub;
    aProtocol.Parser.OnHeader := OnInternalServerHeader;
    aProtocol.Parser.OnMon := OnInternalServerMon;
    aProtocol.Parser.OnSetWill := OnInternalServerSetWill;
    aProtocol.Parser.OnDisconnect := OnInternalServerDisconnect;
    aProtocol.Parser.OnPing := OnInternalServerPing;
  end;
  aProtocol.Parser.Parse(aDataStream)
end;



class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerConnect(
  Sender: TObject; Protocol: UTF8String; Version: byte; ClientID, UserName,
  Password: UTF8String; KeepAlive: Word; Clean: Boolean);
var
  Allowed : Boolean;
  lCnc : TCentralCNC_Message_AuthAsk;

  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream; //pointer;
begin
  Allowed := false;
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);

  lCnc :=  Server.CNCAuth(ClientID, UserName,Password);
  Allowed := lCnc.Agreement;
  if Allowed then
    begin
      if Version < MQTT_SERVER_MINVERSION then
        begin
          ClientProto.Parser.SendConnAck(rcPROTOCOL);  // identifier rejected
          //aClient.Server.Disconnect;
        end
      else
        begin
          ClientProto.Parser.Username := UserName;
          ClientProto.Parser.Password := Password;
          ClientProto.Parser.ClientID := ClientID;
          ClientProto.Parser.KeepAlive := KeepAlive;
          ClientProto.Parser.Clean := Clean;
          Server.Log ('Clean ' + ny[Clean],ClassName);
          if not Clean then
          begin
//          if Assigned (FOnRestoreSession) then
//            FOnRestoreSession (aClient, aClientID)
//          else
//            Sessions.RestoreSession (aClientID, aClient);
          end;

//          if Assigned (FOnDeleteSession) then
//            FOnDeleteSession (aClient, aClientID)
//          else
//            Sessions.DeleteSession (aClientID);

          ClientProto.Parser.SendConnAck (rcACCEPTED);
          AtomicIncrement64(Stats.SYS_broker_load_connections_plus);
          AtomicIncrement64(Stats.SYS_broker_clients_active);
          { TODO : Threads Protect this ! }
          if Stats.SYS_broker_clients_active > Stats.SYS_broker_clients_maximum  then
            Stats.SYS_broker_clients_maximum := Stats.SYS_broker_clients_active
          { TODO : Threads Protect this ! }


//          ClientProto.FGraceful := false;
//          Log ('Accepted. Is Broker ' + ny[aClient.FBroker]);
//          if Assigned (FOnClientsChange) then FOnClientsChange (Self, Threads.Count);
        end;
    end
  else
    begin
      ClientProto.Parser.SendConnAck (rcUSER);
//      ClientProto.Server.Disconnect;
    end;
end;



class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerDisconnect(
  Sender: TObject);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;
begin
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);
  AtomicDecrement64(Stats.SYS_broker_clients_active);
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerHeader(
  Sender: TObject; MsgType: TMQTTMessageType; Dup: Boolean; Qos: TMQTTQOSType;
  Retain: Boolean);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;
begin
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerMon(
  Sender: TObject; aStr: string);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;
begin
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerPing(
  Sender: TObject);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;
begin
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);
  ClientProto.Parser.SendPingResp;
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerPublish(
  Sender: TObject; anID: Word; aTopic: UTF8String; aMessage: RawByteString);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;

  MessageBus : TBusMessage;
  channelTargetWithAppSpace : string;
begin
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);

  if length(aMessage)>0 then
  begin
    //Payload is pure bytes. Do not use AsString or FromString, because of add/loss string encoding.
    SetLength(MessageBus.Buffer,length(aMessage));
    Move(aMessage[1], MessageBus.Buffer[0], Length(aMessage));
  end;

  channelTargetWithAppSpace := aTopic + Client.AppSpace;
  Server.GridBus.Send(MessageBus,channelTargetWithAppSpace,'','',false,Client.ClientId);

  AtomicIncrement64(Stats.SYS_broker_bytes_received,MessageBus.Size);
  AtomicIncrement64(Stats.SYS_broker_messages_received);
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerSend(
  Sender: TObject; anID: Word; Retry: integer; aStream: TMemoryStream);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;

  MessageBus : TBusMessage;
begin
  //This event is called by on send. All data accumulates here (dataOut).
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);

  aStream.Position := 0;
  DataOut.CopyFrom(aStream,aStream.Size);

  AtomicIncrement64(Stats.SYS_broker_bytes_sent,aStream.Size);
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerSub(
  Sender: TObject; anID: Word; Topics: TStringList);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;
  i : integer;
  l : TList_TBusClientReader;
  lc : TBusClientReader;
  channelTargetWithAppSpace : String;
begin
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);

  for I := 0 to Topics.Count-1 do
  begin
    if not(Client.ChannelsReading.IsChannelAlreadyRepresented(Topics[i])) then
    begin
      Client.ChannelsReading.Lock;
      try
        l := Client.ChannelsReading.GetLockedList;
        channelTargetWithAppSpace := Topics[i] + Client.AppSpace;
        lc := Server.GridBus.Subscribe(channelTargetWithAppSpace,OnMQTTMessage);
        lc.Data := ClientProto;
        l.Add(lc);
        ClientProto.Parser.SendSubAck(anID,[qtAT_MOST_ONCE]);
        AtomicIncrement64(Stats.SYS_broker_subscriptions_count);
      finally
        Client.ChannelsReading.Unlock;
      end;
    end;
  end;
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerUnsub(
  Sender: TObject; anID: Word; Topics: TStringList);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;
  i,j : integer;
  c : TBusClientReader;
  channelTargetWithAppSpace : String;
begin
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);

  Client.ChannelsReading.Lock;
  try
   c := nil;
   for I := 0 to Client.ChannelsReading.GetLockedList.Count-1 do
   begin
     for j := 0 to Topics.Count-1 do
     begin
       channelTargetWithAppSpace := Topics[j] + Client.AppSpace;
       if Client.ChannelsReading.GetLockedList[i].ChannelListening = channelTargetWithAppSpace then
       begin
         c := Client.ChannelsReading.GetLockedList[i];
         if Server.GridBus.UnSubscribe(c) then
         begin
           Client.ChannelsReading.GetLockedList.Remove(c);
           FreeAndNil(c);
           AtomicDecrement64(Stats.SYS_broker_subscriptions_count);
         end;
         break;
       end;
     end;
   end;
  finally
   Client.ChannelsReading.Unlock;
  end;

  ClientProto.Parser.SendUnsubAck(anID);
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnMQTTMessage(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
var l : RawByteString;
begin
  if Assigned(aReader.Data) then
  begin
    Assert(aReader.Data is TGRIDProtocol_MQTT);

    //Payload is pure bytes. Do not use AsString or FromString, because of add/loss string encoding.
    SetLength(l,Length(Packet.ContentMessage.buffer));
    Move(Packet.ContentMessage.Buffer[0],l[1],Length(Packet.ContentMessage.buffer));

    TGRIDProtocol_MQTT(aReader.Data).Parser.SendPublish(0,Packet.TargetChannel,l,TMQTTQOSType.qtAT_MOST_ONCE);
    AtomicIncrement64(Stats.SYS_broker_messages_sent);
  end;
end;

class procedure TGRIDProtocolMQTT_ServerHandling.PublishStat(aServer : TGRIDServiceServerBasedProtocol);
  procedure sendStatMessage(const topic, payload : string);
  var M : TBusMessage;
  begin
    m.FromString(payload);
    aServer.GridBus.Send(m,topic);
  end;

  Function GetUpTime : string;
  begin
    result := FormatDateTime('hh:nn:ss',Now-StartServer);
  end;
begin
  sendStatMessage('$SYS/broker/subscriptions/count',IntToStr(Stats.SYS_broker_subscriptions_count));

  sendStatMessage('$SYS/broker/publish/messages/sent',IntToStr(Stats.SYS_broker_publish_messages_sent));
  sendStatMessage('$SYS/broker/publish/messages/dropped','');
  sendStatMessage('$SYS/broker/publish/messages/received','');

  sendStatMessage('$SYS/broker/load/publish/dropped/+',IntToStr(Stats.SYS_broker_load_publish_dropped_plus));
  sendStatMessage('$SYS/broker/load/connections/+',IntToStr(Stats.SYS_broker_load_connections_plus));
  sendStatMessage('$SYS/broker/load/messages/received/+','');
  sendStatMessage('$SYS/broker/load/publish/sent/+','');
  sendStatMessage('$SYS/broker/load/publish/received/+','');
  sendStatMessage('$SYS/broker/load/bytes/received/+','');
  sendStatMessage('$SYS/broker/load/messages/sent/+','');
  sendStatMessage('$SYS/broker/load/bytes/sent/+','');
  sendStatMessage('$SYS/broker/load/sockets/+','');

  sendStatMessage('$SYS/broker/messages/sent',IntToStr(Stats.SYS_broker_messages_sent));
  sendStatMessage('$SYS/broker/messages/received',IntToStr(Stats.SYS_broker_messages_received));
  sendStatMessage('$SYS/broker/messages/inflight','');
  sendStatMessage('$SYS/broker/messages/stored','');

  sendStatMessage('$SYS/broker/clients/active',IntToStr(Stats.SYS_broker_clients_active));
  sendStatMessage('$SYS/broker/clients/inactive',IntToStr(Stats.SYS_broker_clients_inactive));
  sendStatMessage('$SYS/broker/clients/total',IntToStr(Stats.SYS_broker_clients_total));
  sendStatMessage('$SYS/broker/clients/maximum',IntToStr(Stats.SYS_broker_clients_maximum));
  sendStatMessage('$SYS/broker/clients/expired','');

  sendStatMessage('$SYS/broker/retained/messages/count',IntToStr(Stats.SYS_broker_retained_messages_count));

  sendStatMessage('$SYS/broker/version','GRID MQTT Server V0.5b');

  sendStatMessage('$SYS/broker/bytes/sent',IntToStr(Stats.SYS_broker_bytes_sent));
  sendStatMessage('$SYS/broker/bytes/received',IntToStr(Stats.SYS_broker_bytes_received));


  sendStatMessage('$SYS/broker/uptime',GetUpTime);
  sendStatMessage('$SYS/broker/timestamp','');

  sendStatMessage('$SYS/broker/connection/#','');

  sendStatMessage('$SYS/broker/changeset','');
end;

class procedure TGRIDProtocolMQTT_ServerHandling.OnInternalServerSetWill(
  Sender: TObject; aTopic, aMessage: UTF8String; aQos: TMQTTQOSType;
  aRetain: boolean);
var
  Server : TGRIDServiceServerBasedProtocol; //Pointer.
  Client : TGRIDServerUser; //Pointer.
  ClientProto : TGRIDProtocol_MQTT; //Pointer.
  DataOut : TMemoryStream;
begin
  Assert(Sender is TMQTTParser);
  setUp(TMQTTParser(Sender),Server,Client,ClientProto,DataOut);
end;

class procedure TGRIDProtocolMQTT_ServerHandling.setUp(
const aParser : TMQTTParser;
    out aServer: TGRIDServiceServerBasedProtocol;
    out aClient: TGRIDServerUser;
    out aClientProto: TGRIDProtocol_MQTT;
    out aDataOut: TMemoryStream);
begin
  Assert(aParser.DataServer is TGRIDServiceServerBasedProtocol);
  Assert(aParser.DataClient is TGRIDServerUser);
  aServer := TGRIDServiceServerBasedProtocol(aParser.DataServer);
  aClient := TGRIDServerUser(aParser.DataClient);
  aClientProto := TGRIDProtocol_MQTT(aClient.Protocol);
  aDataOut := TMemoryStream(aParser.DataOut);
end;

end.
