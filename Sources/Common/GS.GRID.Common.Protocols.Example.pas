//Simple chat protocol, for illustrate a protocol implementation.
//Note that the class is pretty simple, and let you build wahever protocol you need (P2P, C/S)
//I triyed to modelize something more advanced but encounter many limitation : So, after thoughts,
//I think it is better to build a simple class which enumarate useful ans simple  methods, and manipulate
//them directly.
//An Abstract attempts to modelize fail for me : It lock me on create "fluent" protocol.
unit GS.GRID.Common.Protocols.Example;

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
     GS.GRID.Common.Protocols;

Const
  CST_PROTO_SIGNATURE = 'ExampleProto_BasicChat_XyZ_V1'; //Just to determine which protocol we have server side. Must be send with first connection.

Type

//Sendinf as header for an ask, in all exchange.
TAskCode = (connect,chat,ask_chat);
TGRIDProtocol_ExampleProto_BasicChat_ASK_Header = Packed Record
  UserId : String;
  AskCode : TAskCode;

  Procedure ToStream(aStream : TMemoryStream);
  Procedure FromStream(aStream : TStream);
End;

TRecvCode = ( connect_ok,
              connect_err,
              chat_ack,
              chat_ack_fail,
              operationOk,
              operationFailure);

TGRIDProtocol_ExampleProto_BasicChat_RECV_Header = Packed Record
  //Sending as header reply, always.
  RecvCode : TRecvCode;
  RecvAdditionalInfo : String;

  Procedure ToStream(aStream : TMemoryStream);
  Procedure FromStream(aStream : TMemoryStream);
End;

///
///
///  FEATURES
///
///

///
///  PROTOCOL DESC
///
///

/// CLIENT                                                                       SERVER
///
/// Connect sequence
/// TGRIDProtocol_ExampleProto_BasicChat_ASK_CONNECT -------------------------->
///                                                  <-------------------------- TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT
/// Send a chat line.
/// TGRIDProtocol_ExampleProto_BasicChat_SEND_CHAT   -------------------------->
///                                                  <-------------------------- TGRIDProtocol_ExampleProto_BasicChat_CHAT_ACK
/// Ask for chat message server side
/// TGRIDProtocol_ExampleProto_BasicChat_ASK_CHAT     -------------------------->
///                                                  <-------------------------- TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT
///

//Here is functional record, business level.
TGRIDProtocol_ExampleProto_BasicChat_ASK_CONNECT = Packed Record
  Header : TGRIDProtocol_ExampleProto_BasicChat_ASK_Header;
  ProtocolSignature : String;
  UserName, Password, aGreatings : String;

  Procedure ToStream(aStream : TMemoryStream);
  Procedure FromStream(aStream : TMemoryStream; const HeaderHasBeenAlreadyRead : Boolean = true);
End;

TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT = Packed Record
  Header : TGRIDProtocol_ExampleProto_BasicChat_RECV_Header;
  RoomList : String;

  Procedure ToStream(aStream : TMemoryStream);
  Procedure FromStream(aStream : TMemoryStream);
End;

TGRIDProtocol_ExampleProto_BasicChat_SEND_CHAT = Packed Record
  Header : TGRIDProtocol_ExampleProto_BasicChat_ASK_Header;
  ToRoom : UTF8String;
  ChatText : UTF8String;

  Procedure ToStream(aStream : TMemoryStream);
  Procedure FromStream(aStream : TMemoryStream; const HeaderHasBeenAlreadyRead : Boolean = true);
End;

TGRIDProtocol_ExampleProto_BasicChat_CHAT_ACK = Packed Record
  Header : TGRIDProtocol_ExampleProto_BasicChat_RECV_Header;

  Procedure ToStream(aStream : TMemoryStream);
  Procedure FromStream(aStream : TMemoryStream);
End;

TGRIDProtocol_ExampleProto_BasicChat_ASK_CHAT = Packed Record
  Header : TGRIDProtocol_ExampleProto_BasicChat_ASK_Header;
  RoomName : String;

  Procedure ToStream(aStream : TMemoryStream);
  Procedure FromStream(aStream : TMemoryStream;  const HeaderHasBeenAlreadyRead : Boolean = true);
End;

TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT = Packed Record
  Header : TGRIDProtocol_ExampleProto_BasicChat_RECV_Header;
  FromRoom : String;
  AllRoomUserAndChat : String; //Deliver all room content once (it is just a protocol demo - a real chat will be much complicated)

  Procedure ToStream(aStream : TMemoryStream);
  Procedure FromStream(aStream : TMemoryStream);
End;

TGRIDProtocol_ExampleProto_BasicChat = class(TGRIDProtocol)
protected
public
  Constructor Create; override;
  //Needed for inheritance.
  Class function Negociate(aStream : TStream) : Boolean; Override;
  function ProtocolName : String; Override;
  function ProtocolNativeFormat : TGRIDProtocolFormat; Override;
  function ProtocolDescription : String; Override;
end;

Implementation

{ TGRIDProtocol_ExampleProto_BasicChat_SEND_CHAT }

procedure TGRIDProtocol_ExampleProto_BasicChat_SEND_CHAT.FromStream(
  aStream: TMemoryStream; const HeaderHasBeenAlreadyRead : Boolean);
begin
  Assert(Assigned(aStream));
  if not(HeaderHasBeenAlreadyRead) then
    Header.FromStream(aStream);
  ToRoom := UTF8String(ReadString(aStream));
  ChatText := UTF8String(ReadString(aStream));
end;

procedure TGRIDProtocol_ExampleProto_BasicChat_SEND_CHAT.ToStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  Header.ToStream(aStream);
  WriteString(aStream,String(ToRoom));
  WriteString(aStream,String(ChatText));
end;


constructor TGRIDProtocol_ExampleProto_BasicChat.Create;
begin
  inherited Create;
end;

class function TGRIDProtocol_ExampleProto_BasicChat.Negociate(aStream: TStream): Boolean;
var lc : TGRIDProtocol_ExampleProto_BasicChat_ASK_Header;


begin
  Assert(assigned(aStream));
  Assert(aStream.Size>0);
  //It is a connect which is presented by first connection command : Protect It ! Test it !
  //If it failed, it will be summoning on service level. (Managed, but server exception mecanism heavy task related)
  result := false;
  try
    aStream.Position := 0;
    lc.FromStream(aStream);
    result := ReadString(aStream) = CST_PROTO_SIGNATURE;
  Except
    On E : Exception do
    begin
      raise EExceptionProtocolNegotiation.Create(ClassName + ' - Protocol negociation exception : '+E.Message);
    end;
  end;
end;

function TGRIDProtocol_ExampleProto_BasicChat.ProtocolDescription: String;
begin
  result := 'Example protocol implementation';
end;

function TGRIDProtocol_ExampleProto_BasicChat.ProtocolName: String;
begin
  result := 'ExampleProto';
end;

function TGRIDProtocol_ExampleProto_BasicChat.ProtocolNativeFormat: TGRIDProtocolFormat;
begin
  result := TGRIDProtocolFormat.Binary;
end;


{ TGRIDProtocol_ExampleProto_BasicChat_ASK_Header }

Procedure TGRIDProtocol_ExampleProto_BasicChat_ASK_Header.ToStream(aStream : TMemoryStream);
begin
  Assert(Assigned(aStream));
  WriteString(aStream,UserId);
  WriteByte(aStream,Byte(AskCode));
end;

procedure TGRIDProtocol_ExampleProto_BasicChat_ASK_Header.FromStream(
  aStream: TStream);
begin
  Assert(Assigned(aStream));
  UserId := ReadString(aStream);
  AskCode := TAskCode(ReadByte(aStream));
end;


{ TGRIDProtocol_ExampleProto_BasicChat_RECV_Header }

procedure TGRIDProtocol_ExampleProto_BasicChat_RECV_Header.FromStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  RecvCode := TRecvCode(ReadByte(aStream));
  RecvAdditionalInfo := ReadString(aStream);
end;

procedure TGRIDProtocol_ExampleProto_BasicChat_RECV_Header.ToStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  WriteByte(aStream,Byte(RecvCode));
  WriteString(aStream,RecvAdditionalInfo);
end;

{ TGRIDProtocol_ExampleProto_BasicChat_ASK_CONNECT }

procedure TGRIDProtocol_ExampleProto_BasicChat_ASK_CONNECT.FromStream(
  aStream: TMemoryStream; const HeaderHasBeenAlreadyRead : Boolean);
begin
  Assert(Assigned(aStream));
  if not(HeaderHasBeenAlreadyRead) then
    Header.FromStream(aStream);
  ProtocolSignature := ReadString(aStream);
  UserName := ReadString(aStream);
  Password := ReadString(aStream);
  aGreatings := ReadString(aStream);
end;

procedure TGRIDProtocol_ExampleProto_BasicChat_ASK_CONNECT.ToStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  Header.ToStream(aStream);
  WriteString(aStream,ProtocolSignature);
  WriteString(aStream,UserName);
  WriteString(aStream,Password);
  WriteString(aStream,aGreatings);
end;

{ TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT }

procedure TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT.FromStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  Header.FromStream(aStream);
  RoomList := ReadString(aStream);
end;

procedure TGRIDProtocol_ExampleProto_BasicChat_RECV_CONNECT.ToStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  Header.ToStream(aStream);
  WriteString(aStream,RoomList);
end;

{ TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT }

procedure TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT.FromStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  Header.FromStream(aStream);
  FromRoom := ReadString(aStream);
  AllRoomUserAndChat := ReadString(aStream);
end;

procedure TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT.ToStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  Header.ToStream(aStream);
  WriteString(aStream,FromRoom);
  WriteString(aStream,AllRoomUserAndChat);
end;

{ TGRIDProtocol_ExampleProto_BasicChat_CHAT_ACK }

procedure TGRIDProtocol_ExampleProto_BasicChat_CHAT_ACK.FromStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  Header.FromStream(aStream);
end;

procedure TGRIDProtocol_ExampleProto_BasicChat_CHAT_ACK.ToStream(
  aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  Header.ToStream(aStream);
end;

{ TGRIDProtocol_ExampleProto_BasicChat_ASK_CHAT }

procedure TGRIDProtocol_ExampleProto_BasicChat_ASK_CHAT.FromStream(
  aStream: TMemoryStream;  const HeaderHasBeenAlreadyRead : Boolean);
begin
  if Not HeaderHasBeenAlreadyRead then
    Header.FromStream(aStream);
  RoomName := ReadString(aStream);
end;

procedure TGRIDProtocol_ExampleProto_BasicChat_ASK_CHAT.ToStream(
  aStream: TMemoryStream);
begin
  Header.ToStream(aStream);
  WriteString(aStream,RoomName);
end;


Initialization

GridProtocolManager.Add(TGRIDProtocol_ExampleProto_BasicChat);

end.
