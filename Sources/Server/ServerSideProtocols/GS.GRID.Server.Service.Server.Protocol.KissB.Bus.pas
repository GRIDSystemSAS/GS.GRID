unit GS.GRID.Server.Service.Server.Protocol.KissB.Bus;
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
 GS.JSON,
 GS.Bus,
 GS.Bus.Services,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server,
 GS.GRID.Common.Protocols,
 GS.GRID.Common.Protocols.KissB,
 GS.GRID.Server.Service.Server.BasedProtocols,
 GS.CPUUsage;

 procedure internalHandling_Bus( const Server: TGRIDServiceServerBasedProtocol;
                                    aUser : TGridServerUser;
                                    aProtocol: TGRIDProtocol_KissB;
                                    aDataStream, aResultStream : TMemoryStream);

implementation

 procedure internalHandling_Bus( const Server: TGRIDServiceServerBasedProtocol;
                                    aUser : TGridServerUser;
                                    aProtocol: TGRIDProtocol_KissB;
                                    aDataStream, aResultStream : TMemoryStream);
 var l : TGRIDProtocol_KB_CLT_BUS_CMD;
     resp : TGRIDProtocol_KB_SRV_BUS_CMD_RESPONSE;
     mesresp : TGRIDProtocol_KB_SRV_BUS_CMD_MSG_RESPONSE;
     BusMessage : TBusMessage;
     lbox : TBusEnvelopList;
     llb : TList_PTBusEnvelop;
     c : TBusClientReader;
     channelTargetWithAppSpace : String;
     i : integer;
     lm : integer;
 begin
   if aDataStream.Size = 0 then
   begin
     mesresp.Status := true;
     mesresp.StatusInfo := '';
     mesresp.Header := TKBCltCommand_FromServer.bus_recv;
     try
       lbox :=  TBusEnvelopList.Create;
       try
         lm := server.GridBus.Recv(aUser.ChannelsReading.ToArray,lbox);
         if lm>0 then
         begin
           llb := lbox.Lock;
           try
             if llb.Count>0 then
             begin
               SetLength(mesresp.MessagesPayLoad,llb.Count);
               for i := 0 to llb.Count-1 do
               begin
                 mesresp.MessagesPayLoad[i].From := llb[i].ClientSourceId;
                 mesresp.MessagesPayLoad[i].Channel := llb[i].TargetChannel;
                 mesresp.MessagesPayLoad[i].MessagePayload := llb[i].ContentMessage.Buffer;
                 mesresp.MessagesPayLoad[i].Ticks :=  gsGetTickCount - llb[i].CreateTag; //ticks count between message creation and message output. (outside network consideration)
               end;
               mesresp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
             end;
           finally
             lbox.Unlock;
           end;
         end
         else
         begin
           //No message, no activity : Cool down thread.
           Sleep(CST_THREAD_COOLDOWN);
         end;
       finally
         FreeAndNil(lbox);
       end;
     Except
       On E : Exception do
       begin
         Server.Log('KissB internalHandling_Bus Exception Message : '+E.Message,'');
         //mesresp.Status := false;
         //mesresp.StatusInfo := E.Message;
         //mesresp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
       end;
     end;
   end
   else
   begin
     while aDataStream.Position<aDataStream.Size do
     begin
       l.Load(aDataStream,aProtocol.ProtocolNativeFormat);
       Assert(l.Header.Command = TKBCltCommand.bus);
       //AppSpace is an isolation system :
       //-> It allow to use same channename in 2 different appspace.
       //-> It isolate ALL bus features, since in modify channel entry name. (Thus, bus, DataRepo, and so on)
       //-> Security : The CST_APPSPACE_SEPARATOR prevent us to try to join an AppSpace by
       //   manipulating the channename fron client side.
       channelTargetWithAppSpace := l.ChannelInvolved + aUser.AppSpace;

       case l.CommandStructure of
         TKBCltBusCmd.none: ;
         TKBCltBusCmd.sendmsg:
         begin
           BusMessage.Buffer := l.Payload;
           Server.GridBus.Send(BusMessage,channelTargetWithAppSpace,'','',false,aUser.ClientId,'');
           Resp.Header := TKBCltCommand_FromServer.bus_send;
           Resp.Status := true;
           Resp.StatusInfo := '';
           resp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
         end;
         TKBCltBusCmd.sub:
         begin
           Resp.Header := TKBCltCommand_FromServer.bus_sub;
           Resp.Status := true;
           Resp.StatusInfo := '';
           if Not(aUser.ChannelsReading.IsChannelAlreadyRepresented(channelTargetWithAppSpace)) then
           begin
             c := Server.GridBus.Subscribe(channelTargetWithAppSpace,nil);
             c.ClientBusID := aUser.ClientId; //No Echo.
             aUser.ChannelsReading.Lock;
             try
               aUser.ChannelsReading.GetLockedList.Add(c);
             finally
               aUser.ChannelsReading.Unlock;
             end;
           end;
           resp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
         end;
         TKBCltBusCmd.unsub:
         begin
           Resp.Header := TKBCltCommand_FromServer.bus_unsub;
           Resp.Status := true;
           Resp.StatusInfo := '';
           aUser.ChannelsReading.Lock;
           try
             c := nil;
             for I := 0 to aUser.ChannelsReading.GetLockedList.Count-1 do
               if aUser.ChannelsReading.GetLockedList[i].ChannelListening = channelTargetWithAppSpace then
               begin
                 c := aUser.ChannelsReading.GetLockedList[i];
                 if Server.GridBus.UnSubscribe(c) then
                 begin
                   aUser.ChannelsReading.GetLockedList.Remove(c);
                   FreeAndNil(c);
                 end;
                 break;
               end;
           finally
             aUser.ChannelsReading.Unlock;
           end;
           resp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
         end;
         TKBCltBusCmd.openchan:; //modify chan to be a topic or a queue etc etc;
         TKBCltBusCmd.joinappspace:; //join a quit an app space.;
       end;
     end;
   end;
 end;

end.
