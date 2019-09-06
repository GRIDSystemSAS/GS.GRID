unit GS.GRID.Server.Tests.KissB;
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
 SyncObjs,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 System.SyncObjs,
 {$ENDIF}
 GS.Threads,
 GS.Bus,
 GS.Bus.Services,
 GS.GRID.Server.Service.Types,
 GS.GRID.Common.Protocols,
 GS.GRID.Client,
 GS.GRID.Client.Transport.IndyTCP,
 GS.GRID.Client.KissB;


Type
///
///
///  Tests stuff.
///
TGSSTestItem_ServerClientKissB = Class(TCustomGridServiveServerTestTransportItem)
public
  Function TitleOfItem : String; override;
  Function InternalExecute : Boolean; override;
End;

TGSSTestItem_ServerClientKissB_InfoFeature = Class(TCustomGridServiveServerTestTransportItem)
public
  Function TitleOfItem : String; override;
  Function InternalExecute : Boolean; override;
End;

TGSSTestItem_ServerClientKissB_CPUFeature = Class(TCustomGridServiveServerTestTransportItem)
public
  Function TitleOfItem : String; override;
  Function InternalExecute : Boolean; override;
End;

TGSSTestItem_ServerClientKissB_SendMessageBasic = Class(TCustomGridServiveServerTestTransportItem)
public
  Function TitleOfItem : String; override;
  Function InternalExecute : Boolean; override;
End;

TGSSTestItem_ServerClientKissB_SendMessageBasicStress = Class(TCustomGridServiveServerTestTransportItem)
private
  const c = 10000;
public
  Function TitleOfItem : String; override;
  Function InternalExecute : Boolean; override;
End;


TGSSTestItem_ServerClientKissB_SendMessageStress = Class(TCustomGridServiveServerTestTransportItem)
private
  const cli = 30;
  const c = 10000;
public
  Function TitleOfItem : String; override;
  Function InternalExecute : Boolean; override;
End;

TGSSTestItem_ServerClientKissB_SendMessageStressThreaded = Class(TCustomGridServiveServerTestTransportItem)
private
  const cli = 20;
  const c = 10000;
public
  Function TitleOfItem : String; override;
  Function InternalExecute : Boolean; override;
End;




implementation

{ TGSSTestItem_ServerClientKissB }

function TGSSTestItem_ServerClientKissB.InternalExecute: Boolean;
var l : TGRIDClientKissB;
begin
  result := false;
  l := TGRIDClientKissB.Create(FTransport);
  try
    if l.Connect('admin','admin').Status then
      result := l.Connected;
  finally
    l.Disconnect;
    FreeAndNil(l);
  end;
end;

function TGSSTestItem_ServerClientKissB.TitleOfItem: String;
begin
  result := 'KissB connect';
end;

{ TGSSTestItem_ServerClientKissB_InfoFeature }

function TGSSTestItem_ServerClientKissB_InfoFeature.InternalExecute: Boolean;
var aClient : TGRIDClientKissB;
begin
  result := false;
  aClient := TGRIDClientKissB.Create(FTransport);
  try
    if aClient.Connect('admin','admin').Status then
    begin
      result := length(aClient.Infos.ServerGenuineName)<>0;
    end;
  finally
    FreeAndNil(aClient);
  end;
end;

function TGSSTestItem_ServerClientKissB_InfoFeature.TitleOfItem: String;
begin
  result := 'KissB connect and call info feature';
end;


{ TGSSTestItem_ServerClientKissB_CPUFeature }

function TGSSTestItem_ServerClientKissB_CPUFeature.InternalExecute: Boolean;
var aClient : TGRIDClientKissB;
    var i,c : integer;
        l : double;
begin
  result := false;
  aClient := TGRIDClientKissB.Create(FTransport);
  try
    if aClient.Connect('admin','admin').Status then
    begin
      l := 0.0;
      c := 100;
      for I := 1 to c do
        l := l + aClient.InfosCPULevel;
      l := l/c;
      result := l>0.0;
    end;
  finally
    FreeAndNil(aClient);
  end;
end;

function TGSSTestItem_ServerClientKissB_CPUFeature.TitleOfItem: String;
begin
  result := 'KissB connect and call CPU Level feature';
end;

{ TGSSTestItem_ServerClientKissB_SendMessageBasic }

function TGSSTestItem_ServerClientKissB_SendMessageBasic.InternalExecute: Boolean;
var aClient : TGRIDClientKissB;
begin
  result := false;
  aClient := TGRIDClientKissB.Create(FTransport);
  try
    if aClient.Connect('admin','admin').Status then
    begin
      result := aClient.SendMessage('testchan','Hello world !');
    end;
  finally
    FreeAndNil(aClient);
  end;
end;

function TGSSTestItem_ServerClientKissB_SendMessageBasic.TitleOfItem: String;
begin
  result := 'Just send a message basically.';
end;

{ TGSSTestItem_ServerClientKissB_SendMessageBasicStress }

function TGSSTestItem_ServerClientKissB_SendMessageBasicStress.InternalExecute: Boolean;
var aClient : TGRIDClientKissB;
    i : integer;
begin
  result := false;
  aClient := TGRIDClientKissB.Create(FTransport);
  try
    if aClient.Connect('admin','admin').Status then
    begin
      for I := 0 to c-1 do
        result := aClient.SendMessage('testchan','Hello world ! ('+inttostr(i)+')');
    end;
  finally
    FreeAndNil(aClient);
  end;
end;

function TGSSTestItem_ServerClientKissB_SendMessageBasicStress.TitleOfItem: String;
begin
  result := 'Just send a basic message... '+inttostr(c)+' times.';
end;

{ TGSSTestItem_ServerClientKissB_SendMessageStress }

function TGSSTestItem_ServerClientKissB_SendMessageStress.InternalExecute: Boolean;
var aClient : Array of TGRIDClientKissB;
    i,j : integer;
begin
  FTransport.Free; //Useless.

  result := false;
  SetLength(aClient,cli);
  for i := 0 to length(aClient)-1 do
  begin
    aClient[i] := TGRIDClientKissB.Create(TGRIDTransportIndyTCP.Create);
    aClient[i].Connect('admin','admin')
  end;

  try
    for j := 0 to c-1 do
      for i := 0 to Length(aClient)-1 do
        result := aClient[i].SendMessage('testchan-cli'+IntToStr(i),'Hello world ! ('+inttostr(i)+')');
    sleep(250*Length(aClient)); //let message comme from ctx to bus : else, it will be not counted.
  finally
    for i := 0 to Length(aClient)-1 do
    begin
      FreeAndNil(aClient[i]);
    end;
  end;
end;

function TGSSTestItem_ServerClientKissB_SendMessageStress.TitleOfItem: String;
begin
  result := 'Send '+inttostr(c)+' message(s) for '+intToStr(cli)+' clients. ->('+intTostr(c*cli)+' send operations)';
end;

{ TGSSTestItem_ServerClientKissB_SendMessageStressThreaded }

function TGSSTestItem_ServerClientKissB_SendMessageStressThreaded.TitleOfItem: String;
begin
  result := 'THREADED Send '+inttostr(c)+' message(s) for '+intToStr(cli)+' clients. ->('+intTostr(c*cli)+' send operations)';
end;

type
  TThreadSendMessageStress = Class(TThread)
  public
    mess : Integer;
    cliId : Integer;
    procedure execute; override;
  end;

  procedure TThreadSendMessageStress.execute;
  var aClient : TGRIDClientKissB;
      i : Integer;
  begin
    FreeOnTerminate := true;
    aClient := TGRIDClientKissB.Create(TGRIDTransportIndyTCP.Create);
    try
      aClient.Connect('admin','admin');
      if aClient.Connected then
      begin
        for i := 0 to mess-1 do
          aClient.SendMessage(Format('testchan-cli%d thread %d',[CliID,CurrentThread.ThreadID]), Format('testchan-cli%d thread %d Hi number %d !',[CliID,CurrentThread.ThreadID,i]));
      end
      else
      begin
        raise Exception.Create(aClient.LastStatusInfo);
      end;
    finally
      Sleep(3000); //For instance, need to wait that all message has been succesfully dispatch by the bus, otherwise, their flush on disconnect.
      aClient.Free;
    end;
  end;

function TGSSTestItem_ServerClientKissB_SendMessageStressThreaded.InternalExecute: Boolean;
var aClient : Array of TThreadSendMessageStress;
    i : integer;
begin
  //FTransport.Free; //Useless.
  result := false;
  SetLength(aClient,cli);
  for i := 0 to length(aClient)-1 do
  begin
    aClient[i] := TThreadSendMessageStress.Create(true);
    aClient[i].mess := c;
    aClient[i].cliId := i+1;
    aClient[i].Start;
  end;
end;

end.
