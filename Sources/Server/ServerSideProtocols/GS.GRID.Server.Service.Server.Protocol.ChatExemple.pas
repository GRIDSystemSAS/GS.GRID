unit GS.GRID.Server.Service.Server.Protocol.ChatExemple;
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
 GS.GRID.Common.Protocols.Example,
 GS.GRID.Server.Service.Server.BasedProtocols;

Type
TExampleProto_BasicChat_ServerHandling = Class
  //Note  : Server must be pass in proto, because a lot of task is related to server service.
  //        In fact, more abstraction here drive to a problematic technical handling
  //        inside protocol. We need to know on whiuch service we talk.
  //        Perhaps we refactore this after a more complexe protocol implmentationd (TODO)

  // Remember that this code is execute in the server's client thread context.
  class procedure handling_Proto( const Server : TGRIDServiceServerBasedProtocol;
                                        aProtocol: TGRIDProtocol_ExampleProto_BasicChat;
                                        aDataStream,
                                        aResultStream : TMemoryStream);
End;


implementation


uses GS.GRID.Server.Service.CentralCnC;


{ TExampleProto_BasicChat_ServerHandling }

class procedure TExampleProto_BasicChat_ServerHandling.handling_Proto(
  const Server: TGRIDServiceServerBasedProtocol;
  aProtocol: TGRIDProtocol_ExampleProto_BasicChat;
  aDataStream, aResultStream : TMemoryStream);

var lcontact : TGRIDProtocol_ExampleProto_BasicChat_ASK_Header;

    Procedure HandleConnect;
    var
      lCon : TGRIDProtocol_ExampleProto_BasicChat_ASK_CONNECT;
      lResp : TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT;
      lCnc : TCentralCNC_Message_AuthAsk;
    begin
      lCon.FromStream(aDataStream);
      lCnc := Server.CNCAuth('', lCon.UserName,lCon.Password);
      if lCnc.Agreement then
      begin
        //Build response stream to deliver to the client.
        lResp.Header.RecvCode := TRecvCode.connect_ok;
        lResp.Header.RecvAdditionalInfo := lCnc.AgreementSessionId;
        lResp.RoomList := 'TEST'; //Todo : Room management ;)
      end
      else
      begin
        lResp.Header.RecvCode := TRecvCode.connect_err;
        lResp.Header.RecvAdditionalInfo := 'User not granted';
        lResp.RoomList := '';
      end;
      lResp.ToStream(aResultStream);
    end;

    Procedure HandleReceiveChat;
    var
      lCon : TGRIDProtocol_ExampleProto_BasicChat_SEND_CHAT;
      lResp : TGRIDProtocol_ExampleProto_BasicChat_CHAT_ACK;
      lStream : TMemoryStream;
      la : TBusClientDataRepo;
      ll : TStringList;
    begin
      //Frankly, this data bus ressource should normaly initialize one time, since it is not stateless (C/S point of view)
      //But, as it is a demo, it show the data bus features of grid. so, 2 demo in one. :)

      //This will be really done one time only, even if it called many time.
      Server.GridBus.DeclareDataRepository('CHATSERVER');

      lCon.FromStream(aDataStream);
      //Subscription to CHATSERVER data repo.
      ll := TStringList.Create;
      lStream := TMemoryStream.Create;
      la := TBusClientDataRepo.Create(Server.GridBus,'CHATSERVER');
      try
        //Get activity of a room.
        la.GetValue(String('ROOM_'+lCon.ToRoom),lStream);
        lStream.Position := 0;
        if lStream.Size>0 then
          ll.LoadFromStream(lstream);
        //Add the new chat activity...
        ll.Add('USER:'+String(lcontact.UserId)+'/ '+String(lcon.ChatText));
        lStream.Clear;
        ll.SaveToStream(lStream);
        lStream.Position := 0;
        //Save back the activity in the room.
        la.SetValue(String('ROOM_'+lCon.ToRoom),lStream);

        //UnLock "UpdateRoom"

        //Send response back.
        lResp.Header.RecvCode := TRecvCode.chat_ack;
        lResp.Header.RecvAdditionalInfo := '';

        aResultStream.Clear;
        lResp.ToStream(aResultStream);
      finally
        FreeAndNil(ll);
        FreeAndNil(la);
        FreeAndNil(lStream);
      end;
    end;

    Procedure HandleAskChat;
    var
      lCon : TGRIDProtocol_ExampleProto_BasicChat_ASK_CHAT;
      lResp : TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT;
      lStream : TMemoryStream;
      la : TBusClientDataRepo;
      ll : TStringList;
    begin
      Server.GridBus.DeclareDataRepository('CHATSERVER');
      lCon.FromStream(aDataStream);
      //Subscription to CHATSERVER data repo.
      ll := TStringList.Create;
      lStream := TMemoryStream.Create;
      la := TBusClientDataRepo.Create(Server.GridBus,'CHATSERVER');
      try
        //Lock "UpdateRoom"
        //...?
        la.GetValue('ROOM_'+lCon.RoomName,lStream);
        lStream.Position := 0;
        if lStream.Size>0 then
          ll.LoadFromStream(lstream);
        //
        lResp.FromRoom := lCon.RoomName;
        lResp.AllRoomUserAndChat := ll.Text;

        //UnLock "UpdateRoom"

        //Send response back.
        lResp.Header.RecvCode := TRecvCode.operationOk;
        lResp.Header.RecvAdditionalInfo := '';

        aResultStream.Clear;
        lResp.ToStream(aResultStream);
      finally
        FreeAndNil(ll);
        FreeAndNil(la);
        FreeAndNil(lStream);
      end;
    end;



begin
  lcontact.FromStream(aDataStream);
  case lcontact.AskCode of
    TAskCode.connect :
    begin
      HandleConnect;
    end;
    TAskCode.chat :
    begin
      HandleReceiveChat;
    end;
    TAskCode.ask_chat :
    begin
      HandleAskChat;
    end;
  end;
end;

end.
