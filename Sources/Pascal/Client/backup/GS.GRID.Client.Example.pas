unit GS.GRID.Client.Example;

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
     GS.Stream,
     GS.GRID.Common.Protocols,
     GS.GRID.Common.Protocols.Example,
     GS.GRID.Client.Transport,
     GS.GRID.Client;
Type

TGRIDClientExampleChat = class(TGRIDClient)
private
  FUserId: String;
  FProto : TGRIDProtocol_ExampleProto_BasicChat;
  FLastConnect: TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT;
public
  Constructor Create(aTransport : TGRIDTransport); Reintroduce;
  Destructor Destroy; Override;

  function Connect(aUserName, aPassword, aGreatings : String) : TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT;
  function SendChat(ToRoom, aChatText : UTF8String) :  TGRIDProtocol_ExampleProto_BasicChat_CHAT_ACK;
  function RecvChat : TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT;

  Property UserId : String read FUserId;

  Property LastConnectData : TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT read FLastConnect;
end;


implementation


{ TGRIDClientExample }

function TGRIDClientExampleChat.Connect(aUserName, aPassword,
  aGreatings: String): TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT;
var lStream : TMemoryStream;
    lAskConnect : TGRIDProtocol_ExampleProto_BasicChat_ASK_CONNECT;
begin
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  lStream := TMemoryStream.Create;
  try
    try
      lAskConnect.Header.UserId := UserId;
      lAskConnect.Header.AskCode := TAskCode.connect;
      lAskConnect.UserName := aUserName;
      lAskConnect.Password := aPassword;
      lAskConnect.aGreatings := aGreatings;
      lAskConnect.ProtocolSignature := CST_PROTO_SIGNATURE;

      lAskConnect.ToStream(lStream);
      lStream.Position := 0;

      FTransport.Connect;
      FTransport.Send(lStream);
      lStream.Clear;
      FTransport.Recv(TStream(lStream));
      lStream.Position := 0;
      Result.FromStream(lStream);
    Except
      On E : Exception do
      begin
        Result.Header.RecvCode := TRecvCode.connect_err;
        Result.Header.RecvAdditionalInfo := E.Message;
      end;
    end;
  finally
    FLastConnect := Result;
    FreeAndNil(lStream);
  end;
end;


constructor TGRIDClientExampleChat.Create(aTransport: TGRIDTransport);
begin
  Assert(assigned(aTransport));
  inherited Create;
  FTransport := aTransport;
  FProto := TGRIDProtocol_ExampleProto_BasicChat.Create;
  FProtocol := FProto; //Avoid cast.
  FUserId := 'UserOfChat'+IntToStr(NativeInt( TThread.Current.ThreadID));
end;

destructor TGRIDClientExampleChat.Destroy;
begin
  FreeAndNil(FTransport);
  FreeAndNil(FProtocol);
  inherited;
end;

function TGRIDClientExampleChat.RecvChat: TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT;
var lStream : TMemoryStream;
    lRecvForChat : TGRIDProtocol_ExampleProto_BasicChat_ASK_CHAT;
begin
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  Assert(Transport.Connected);
  try
    lStream := TMemoryStream.Create;
    try
      lRecvForChat.Header.AskCode := TAskCode.ask_chat;
      lRecvForChat.Header.UserId := UTF8String(FUserId);
      lRecvForChat.RoomName := 'TEST';
      lRecvForChat.ToStream(lStream);
      lStream.Position := 0;
      FTransport.Send(lStream);
      lStream.Clear;
      FTransport.Recv(TStream(lStream));
      lStream.Position := 0;
      Result.FromStream(lStream);
    Except
      On E : Exception do
      begin
        Result.Header.RecvCode := TRecvCode.connect_err;
        Result.Header.RecvAdditionalInfo := E.Message;
      end;
    end;
  finally
    FreeAndNil(lStream);
  end;
end;

function TGRIDClientExampleChat.SendChat(ToRoom,
  aChatText: UTF8String): TGRIDProtocol_ExampleProto_BasicChat_CHAT_ACK;
var lStream : TMemoryStream;
    lChat : TGRIDProtocol_ExampleProto_BasicChat_SEND_CHAT;
begin
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  Assert(Transport.Connected);
  try
    lStream := TMemoryStream.Create;
    try
      lChat.Header.AskCode := TAskCode.chat;
      lChat.Header.UserId := FUserId;
      lChat.ToRoom := ToRoom;
      lChat.ChatText := aChatText;
      lChat.ToStream(lStream);
      lStream.Position := 0;
      FTransport.Send(lStream);
      lStream.Clear;
      FTransport.Recv(TStream(lStream));
      lStream.Position := 0;
      Result.FromStream(lStream);
    Except
      On E : Exception do
      begin
        Result.Header.RecvCode := TRecvCode.connect_err;
        Result.Header.RecvAdditionalInfo := E.Message;
      end;
    end;
  finally
    FreeAndNil(lStream);
  end;
end;

end.
