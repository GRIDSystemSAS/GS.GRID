///
///  CentralCnC = Central Command&Control
///  Here is rule all Security plan.
unit GS.GRID.Server.Service.CentralCnC;

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

uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 SyncObjs,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 System.SyncObjs,
 {$ENDIF}
 GS.Common,
 GS.Bus,
 GS.Bus.Services,
 GS.Stream,
 GS.GRID.Common.Types,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server,
 GS.GRID.Server.Service.CentralCnC.Informations,
 GS.GRID.Server.Service.CentralCnC.Users,
 GS.GRID.Server.Python.Conf,
 GS.GRID.Server.Python;

Const
  CST_CNC_FILENAME_CONF         = 'grid.cnc.conf';
  CST_CNC_FILENAME_USERCONF     = 'grid.cnc.usr.conf';
  CST_CNC_FILENAME_PYTHONCONF   = 'grid.cnc.python.conf';

Type

///
///  Message sent to CNC for external user authorization.
///
TCentralCNC_Message_AuthAsk = Packed Record
  ServiceUserID : string;
  UserName, password : String;
  Agreement : boolean;
  AgreementSessionId : String;

  procedure Clear;
  procedure SetNewSession;
  procedure ToStream(var aStream : TMemoryStream);
  procedure FromStream(const aStream : TMemoryStream);
end;




TCustomGRIDServiceCentralCnC = class(TGRIDService)
private
  FUserConf : TCNCUserConfiguration;
  FInfo : TGRIDCentralCNCInformation;
  FLogCategories: TGridLogCategories;

  procedure SetUpLogFile;
  procedure PublishCNCInfo;

  procedure InternalInstantPythonConfiguration;
  procedure InternalUserConfiguration;
protected
  FLogChannelClient : TBusClientReader;
  FAuthClient : TBusClientReader;
  FPythonUpdate : TBusClientReader;
  FUsrUpdate : TBusClientReader;
  FFileLog : TextFile;

  procedure OnLogIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Virtual;
  procedure OnAuthIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Virtual;
  procedure OnPythonConfUpdateIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Virtual;
  procedure OnUserConfUpdateIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Virtual;
public
  procedure Initialize; Override;
  Procedure Execute; Override;
  procedure Finalize; Override;

  Procedure LoadConfiguration; Override;

  property LogCategories : TGridLogCategories read FLogCategories write FLogCategories;
end;

TGRIDServiceCentralCnC = class(TCustomGRIDServiceCentralCnC)
Public
 Class Function GetDefaultImplementation : TGridService; Override;
 function AutoTestImplementation: TCustomGridServiveServerTestResult; Override;
end;


/// Testing classes
///
///
///

TGridServiveServerTestCnCResult = class(TCustomGridServiveServerTestResult)
public
  Function Title : String; Override;
  Constructor Create(aOwner : TCustomGridService); Override;
end;

TGridServiveServerCnCTestItem = class(TCustomGridServiveServerTestItem)
public
  Function TitleOfItem : String; Override;
  Function InternalExecute : Boolean; OVerride;
end;


implementation


{ TCustomGRIDServiceCentralCnC }


procedure TCustomGRIDServiceCentralCnC.Execute;
var cl : Array of TBusClientreader;
begin
  Log('Command and Control service started.',ClassName);
  Log('  | '+FInfo.GetOSAndArchAsString,ClassName);
  //Sending info into Bus KeyVal db.
  PublishCNCInfo;

  setlength(cl,5);
  cl[0] := FLogChannelClient;
  cl[1] := FAuthClient;
  cl[2] := FGlobalInstructionChannel;
  cl[3] := FUsrUpdate;
  cl[4] := FPythonUpdate;

  while Not(MasterThread.Terminated) do
  begin
    BusProcessMessages(cl);
    sleep(CST_ONE_SEC);
    //MasterThread.DoHeartBeat; Not realy used for instance.
  end;
end;

procedure TCustomGRIDServiceCentralCnC.Finalize;
begin
  inherited;
  FGridBus.UnSubscribe(FLogChannelClient);
  FGridBus.UnSubscribe(FAuthClient);
  FGridBus.UnSubscribe(FPythonUpdate);
  FreeAndNil(FLogChannelClient);
  FreeAndNil(FAuthClient);
  FreeAndNil(FPythonUpdate);
  FreeAndNil(FUsrUpdate);
  FreeAndNil(FUserConf);
  FreeAndNil(FInfo);
  TGRIDPython.Clean;
  Flush(FFileLog);
end;

procedure TCustomGRIDServiceCentralCnC.Initialize;
begin
  FLogCategories := [TGridLogCategory.glcWarning, TGridLogCategory.glcException, TGridLogCategory.glcFatal];
  FUserConf := TCNCUserConfiguration.Create;
  FInfo := TGRIDCentralCNCInformation.Create;
  MasterThread.Bus.DeclareDataRepository(CST_BUSDATAREPO_SERVERINFO);
  inherited;

  FLogChannelClient := FGridBus.Subscribe(CST_CHANNELNAME_LOGROOT,OnLogIncoming);
  FPythonUpdate := FGridBus.Subscribe(CST_CHANNELNAME_CNC_PYTHONCONFUPDATE,OnPythonConfUpdateIncoming);
  FUsrUpdate := FGridBus.Subscribe(CST_CHANNELNAME_CNC_USERCONFUPDATE,OnUserConfUpdateIncoming);
  FAuthClient := FGridBus.Subscribe(CST_CHANNELNAME_CNC_AUTH_GLOBAL,OnAuthIncoming);

  //Userconf.
  InternalUserConfiguration;
  SetUpLogFile;

  //Python conf.
  InternalInstantPythonConfiguration;
end;

procedure TCustomGRIDServiceCentralCnC.InternalInstantPythonConfiguration;
var PythonConf : TCNCPythonConfiguration;
begin
  if FileExists(CST_CNC_FILENAME_PYTHONCONF) then
  begin
    PythonConf := TCNCPythonConfiguration.Create;
    try
      try
        PythonConf.LoadFromFile(CST_CNC_FILENAME_PYTHONCONF);
        PythonConf.PublishAll(MasterThread.Bus); //publish all configuration on Datarepo. (Published on the bus, available from everywhere)
        TGRIDPython.Setup(MasterThread.Bus);
      Except
        On E : Exception do
          log('Python Engine not properly initialized ('+E.Message+')',ClassName)
      end;
    finally
      FreeAndNil(PythonConf);
    end;
  end;
end;

procedure TCustomGRIDServiceCentralCnC.InternalUserConfiguration;
begin
  if FileExists(CST_CNC_FILENAME_USERCONF) then
  begin
    try
      log('Loading User configuration file...',ClassName,'CNC');
      FUserConf.LoadFromFile(CST_CNC_FILENAME_USERCONF);
      log(Format('%d user(s) registered',[FUserConf.UsersList.Count]),ClassName,'CNC');
    Except
      On E : Exception do
        log('User configuration not properly initialized ('+E.Message+')',ClassName)
    end;
  end
  else
  begin
    FUserConf.SaveToFile(CST_CNC_FILENAME_USERCONF);
  end;
end;

procedure TCustomGRIDServiceCentralCnC.LoadConfiguration;
begin
  if FileExists(CST_CNC_FILENAME_CONF) then
  begin
    //Read File conf.
//    readconf;
  end
  else
  begin
//    createconf;
//    readconf;
    //Create default file, and read it.
  end;
end;

procedure TCustomGRIDServiceCentralCnC.OnAuthIncoming(Sender: TBusSystem;
   aReader : TBusClientReader; var Packet: TBusEnvelop);
var l : TMemoryStream;
    ls : TCentralCNC_Message_AuthAsk;
    lusr : TCNCUser;
    lResponse : TBusMessage;

begin
  //Auth ask from whatever service.
  lusr := nil;
  l := Packet.ContentMessage.AsStream;
  try
    ls.FromStream(l);
    ls.Agreement := false;
    if FUserConf.UsersList.Get(ls.UserName,lusr) then
    begin
      ls.Agreement := ls.password = lusr.Password;
      ls.SetNewSession;
    end;
    if ls.Agreement then
      log(ls.UserName+' ['+ls.ServiceUserID+'] access granted',ClassName)
    else
      log(ls.UserName+' ['+ls.ServiceUserID+'] access deny',ClassName);
    ls.UserName := '';
    ls.password := '';
    l.Clear;
    ls.ToStream(l);
    lResponse.FromStream(l);
  finally
    FreeAndNil(l);
    GridBus.Send(lResponse,Packet.ResponseChannel);
  end;
end;

procedure TCustomGRIDServiceCentralCnC.OnLogIncoming(Sender: TBusSystem;  aReader : TBusClientReader;
  var Packet: TBusEnvelop);
var ls : TGRIDLogChunk;
    l : TMemoryStream;
    ll : String;
begin
  //log from EVERYWHERE.
  l := Packet.ContentMessage.AsStream;
  try
    l.Position := 0;
    ls.Deserialize(l);
  finally
    FreeAndNil(l);
  end;

  if ls.Category in LogCategories then
  begin
    ll := FormatDateTime('hhnnsszzzz',now) + '/' + Format('[%d]/%d/  %s/%s',[ls.ThreadID,Byte(ls.Category),ls.LogText,ls.Module]);
    Writeln(FFileLog,ll);
  end;
end;

procedure TCustomGRIDServiceCentralCnC.OnPythonConfUpdateIncoming(
  Sender: TBusSystem; aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  //Python file configuration has been modify : Update conf now.
  InternalInstantPythonConfiguration;
end;

procedure TCustomGRIDServiceCentralCnC.OnUserConfUpdateIncoming(
  Sender: TBusSystem; aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  //Python file configuration has been modify : Update conf now.
  InternalUserConfiguration;
end;

procedure TCustomGRIDServiceCentralCnC.PublishCNCInfo;
var l : TBusClientDataRepo;
begin
  //As private data repo, we use masterthread bus, because it is very much less used thang gridbus.
  l := TBusClientDataRepo.Create(MasterThread.Bus,CST_BUSDATAREPO_SERVERINFO);
  try
    l.SetValue('GRIDServerName',FInfo.GetGridServerName);
    l.SetValue('OSAndArchAsString',FInfo.GetOSAndArchAsString);
    l.SetValue('OSMajorMinorBuild',FInfo.GetOSMajorMinorBuild);
    l.SetValue('OSArchitecture',FInfo.GetOSArchitecture);
    l.SetValue('OSName',FInfo.GetOSName);
    l.SetValue('OSGenuineName',FInfo.GetOSGenuineName);
    l.SetValue('GRIDArch',FInfo.GetServerArch);
    l.SetValue('GRIDCompiler',FInfo.GetCompiler);
  finally
    FreeAndNil(l);
  end;
end;

procedure TCustomGRIDServiceCentralCnC.SetUpLogFile;
var ln : string;
begin
  ln := 'GRIDLOG_'+FormatDateTime('YYYYMMDDhhnnsszzzz',now)+'.log';
  ln := ExtractFilePath(ParamStr(0))+ln;
  AssignFile(FFileLog,ln); // FFileLog := TFileStream.Create(ln, fmCreate);
  Rewrite(FFileLog);
  { TODO : delete all log file from a date ?}
end;

{ TGRIDServiceCentralCnC }


function TGRIDServiceCentralCnC.AutoTestImplementation: TCustomGridServiveServerTestResult;
begin
  Result := TGridServiveServerTestCnCResult.Create(Self);
end;

class function TGRIDServiceCentralCnC.GetDefaultImplementation: TGridService;
begin
  Result := TGRIDServiceCentralCnC.Create;
end;

{ TGridServiveServerTestCnCResult }

constructor TGridServiveServerTestCnCResult.Create(aOwner : TCustomGridService);
begin
  inherited Create(aOwner);
  Tests.Add(TGridServiveServerCnCTestItem.Create(self));
end;

function TGridServiveServerTestCnCResult.Title: String;
begin
  Result := 'Central Command&Control (aka CnC) Service test';
end;

{ TCustomGridServiveServerCnCTestItem }

function TGridServiveServerCnCTestItem.InternalExecute: Boolean;
begin
  Result := true;
end;

function TGridServiveServerCnCTestItem.TitleOfItem: String;
begin
  Result := 'Exemple test (Always true)';
end;

{ TCentralCNC_Message_AuthAsk }

procedure TCentralCNC_Message_AuthAsk.Clear;
begin
  ServiceUserID := '';
  UserName := '';
  password := '';
  Agreement := false;
  AgreementSessionId := '';
end;

procedure TCentralCNC_Message_AuthAsk.FromStream(const aStream: TMemoryStream);
begin
  ServiceUserID := ReadString(aStream);
  UserName := ReadString(aStream);
  password := ReadString(aStream);
  Agreement := ReadBoolean(aStream);
  AgreementSessionId :=  ReadString(aStream);
end;


procedure TCentralCNC_Message_AuthAsk.SetNewSession;
begin
  AgreementSessionId := TGUID.NewGuid.ToString;
end;

procedure TCentralCNC_Message_AuthAsk.ToStream(var aStream: TMemoryStream);
begin
  WriteString(aStream, ServiceUserID);
  WriteString(aStream, UserName);
  WriteString(aStream, password);
  WriteBoolean(aStream, Agreement);
  WriteString(aStream,AgreementSessionId);
end;



end.
