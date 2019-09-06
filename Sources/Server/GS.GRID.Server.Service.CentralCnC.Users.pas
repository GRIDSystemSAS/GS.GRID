//Users basic object model and configuration file.
unit GS.GRID.Server.Service.CentralCnC.Users;
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

uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 {$ENDIF}
 GS.Common,
 GS.Bus,
 GS.Stream,
 GS.GRID.Common.Types,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server;

const
  CST_DEFAULT_USERNAME    = 'admin';
  CST_DEFAULT_PASSWORD    = 'admin';
  CST_USRLST_MARK_VERSION = 'ul0.9';
Type

TCNCUserHistory = Class
End;

TCNCUser = class
private
public
  UserName,
  Password : String;
  Aggreement : boolean;
  //  Histories :
  //TODO : History. (With by API querying system.) : this history must be saved into file, to not impact server's memory.
  // - History of Session.
  // --> History of connection
  // --> History of API call.
  //Python default configuration.
  //Python right to use ?

  Procedure SaveToStream(aStream : TStream);
  Procedure LoadFromStream(aStream : TStream);
end;

TCNCUserList = class(TKeysValues_UTF8StringToObject)
private
  function GetItemUser(Index: UInt32): TCNCUser;
public
  Constructor Create; reintroduce;
  Destructor Destroy; Override;

  Function Get(Const aUserCNCId : String; Out aUser : TCNCUser) : boolean;

  Procedure SaveToStream(aStream : TStream);
  Procedure LoadFromStream(aStream : TStream);

  Property Items[Index : UInt32] : TCNCUser read GetItemUser; default;
end;


TCNCUserConfiguration = Class
private
protected
  procedure readconf(stream : TMemoryStream);
  procedure createconf(toStream : TMemoryStream; Const NewFile : Boolean = false);
public
  UsersList : TCNCUserList;

  constructor Create; Virtual;
  destructor destroy; Override;

  procedure LoadFromFile(aFileName : String);
  procedure SaveToFile(aFileName : String);

//  procedure PublishAll(aBus : TBus); //Publish data stream into a datarep.
//  function LoadFromRepo(aBus : TBus) : boolean; //Get data back. True if found an loaded.
end;


implementation


{ TCNCUserList }

constructor TCNCUserList.Create;
begin
  inherited Create;
end;

destructor TCNCUserList.Destroy;
var i : integer;
begin
  for I := 0 to Length(FArray)-1 do
  begin
    FArray[i].Value.Free;
  end;
  FArray:= nil;
  inherited;
end;

function TCNCUserList.Get(const aUserCNCId: String;
  out aUser: TCNCUser): boolean;
begin
  result := TryGetValue(aUserCNCId,TObject(aUser));
end;


function TCNCUserList.GetItemUser(Index: UInt32): TCNCUser;
begin
  result := TCNCUser(GetItem(Index).Value);
end;

procedure TCNCUserList.LoadFromStream(aStream: TStream);
var i : integer;
    c : UInt32;
    l : TCNCUser;
begin
  Clear;
  c := ReadUINT32(aStream);
  for i := 0 to c-1 do
  begin
    l := TCNCUser.Create;
    l.LoadFromStream(aStream);
    Add(l.UserName,l);
  end;
end;

procedure TCNCUserList.SaveToStream(aStream: TStream);
var i : integer;
begin
  WriteUInt32(aStream,Count);
  for i := 0 to Count-1 do
  begin
    TCNCUser(FArray[i].Value).SaveToStream(aStream);
  end;
end;

{ TCNCUser }

procedure TCNCUser.LoadFromStream(aStream: TStream);
begin
  UserName := ReadString(aStream);
  Password := ReadString(aStream);
  Aggreement := ReadBoolean(aStream);
end;

procedure TCNCUser.SaveToStream(aStream: TStream);
begin
  WriteString(aStream,UserName);
  WriteString(aStream,Password);
  WriteBoolean(aStream,Aggreement);
end;

{ TCNCUserConfiguration }

constructor TCNCUserConfiguration.Create;
begin
  UsersList := TCNCUserList.Create;
end;

procedure TCNCUserConfiguration.createconf(toStream : TMemoryStream; Const NewFile : Boolean = false);
var ldefaultuser : TCNCUser;
begin
  if NewFile then
  begin
    UsersList.Clear;
    ldefaultuser := TCNCUser.Create;
    ldefaultuser.UserName := CST_DEFAULT_USERNAME;
    ldefaultuser.Password := CST_DEFAULT_PASSWORD;
    ldefaultuser.Aggreement := true;
    UsersList.Add(ldefaultuser.UserName,ldefaultuser);
  end;
  toStream.Size := 0;

  WriteString(toStream,CST_USRLST_MARK_VERSION);
  UsersList.SaveToStream(toStream);
  toStream.Position := 0;
  GS.Common.StreamEncrypt(toStream);
  toStream.Position := 0;
end;


procedure TCNCUserConfiguration.readconf(stream : TMemoryStream);
begin
  stream.Position := 0;
  GS.Common.StreamDecrypt(stream);
  stream.Position := 0;
  ReadString(stream); //check CST_USRLST_MARK_VERSION ?
  UsersList.LoadFromStream(stream);
end;


destructor TCNCUserConfiguration.destroy;
begin
  FreeAndNil(UsersList);
  inherited;
end;

procedure TCNCUserConfiguration.LoadFromFile(aFileName: String);
var l : TMemoryStream;
begin
  l := TMemoryStream.Create;
  try
    l.LoadFromFile(aFileName);
    l.Position := 0;
    readconf(l);
  finally
    FreeAndNil(l);
  end;
end;


procedure TCNCUserConfiguration.SaveToFile(aFileName: String);
var l : TMemoryStream;
begin
  l := TMemoryStream.Create;
  try
    createconf(l,Not FileExists(aFileName));
    l.SaveToFile(aFileName);
  finally
    FreeAndNil(l);
  end;
end;

end.
