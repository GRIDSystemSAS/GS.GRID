unit GS.GRID.Server.Python.Conf;
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
 GS.System.CPU,
 GS.Stream,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server,
 GS.Bus;

Type

TCNCPythonConfigurationItem = class
private
public
  pythonVersion : String;
  pythonAPIVersion : String;
  pythonLibName : String;
  pythonLibPath : String;
  function key : String;
end;

TCNCPythonConfItems = class(TKeysValues_UTF8StringToObject)
  private
    function GetItemPython(Index: UInt32): TCNCPythonConfigurationItem;
public
  Destructor Destroy; Override;

  Function Get(Const aPythonId : String; Out aPythonConf : TCNCPythonConfigurationItem) : boolean;

  Property Items[Index : UInt32] : TCNCPythonConfigurationItem read GetItemPython; default;
end;

TCNCPythonConfiguration = class
private
  FDefaultPythonConfId: String;
  function GetReady: Boolean;
  function GetPythonConfAsHumanReadableString: String;
protected
public
  PythonConfigurations : TCNCPythonConfItems;

  constructor Create; Virtual;
  destructor destroy; Override;

  Procedure SaveToStream(aStream : TStream);
  Procedure LoadFromStream(aStream : TStream);

  procedure LoadFromFile(aFileName : String);
  procedure SaveToFile(aFileName : String);

  procedure PublishAll(aBus : TBus); //Publish data stream into a datarep.
  function LoadFromRepo(aBus : TBus) : boolean; //Get data back. True if found an loaded.

  Property DefaultPythonConfId : String read FDefaultPythonConfId write FDefaultPythonConfId;

  Property Ready : Boolean read GetReady;

  Property AsString : String read GetPythonConfAsHumanReadableString;
end;

implementation

{ TCNCPythonConfItems }

destructor TCNCPythonConfItems.Destroy;
var i : integer;
begin
  for I := 0 to Length(FArray)-1 do
  begin
    FArray[i].Value.Free;
  end;
  FArray:= nil;
  inherited;
end;

function TCNCPythonConfItems.Get(const aPythonId: String;
  out aPythonConf: TCNCPythonConfigurationItem): boolean;
begin
  result := TryGetValue(aPythonId,TObject(aPythonConf));
end;

function TCNCPythonConfItems.GetItemPython(
  Index: UInt32): TCNCPythonConfigurationItem;
begin
  result := TCNCPythonConfigurationItem(GetItem(Index).Value);
end;


{ TCNCPythonConfiguration }

constructor TCNCPythonConfiguration.Create;
begin
  inherited;
  PythonConfigurations := TCNCPythonConfItems.Create;
end;

destructor TCNCPythonConfiguration.destroy;
begin
  FreeAndNil(PythonConfigurations);
  inherited;
end;

function TCNCPythonConfiguration.GetPythonConfAsHumanReadableString: String;
var l : TStringList;
    i : integer;
begin
  result := 'None';
  if Ready then
  begin
    l := TStringList.Create;
    try
      l.Add('Current Default conf : "'+DefaultPythonConfId+'"');
      l.Add('');
      for i := 0 to PythonConfigurations.Count-1 do
      begin
        l.Add(Format('[ %d ] %s - %s [%s]',[ i+1,
                                             PythonConfigurations[i].key,
                                             PythonConfigurations[i].pythonLibName,
                                             PythonConfigurations[i].pythonLibPath]));
      end;
      l.Add('');
      Result := l.Text;
    finally
      FreeAndNil(l);
    end;
  end;
end;

function TCNCPythonConfiguration.GetReady: Boolean;
begin
  result := PythonConfigurations.Count>0;
end;

procedure TCNCPythonConfiguration.LoadFromFile(aFileName : String);
var l : TMemoryStream;
begin
  if FileExists(aFileName) then
  begin
    l := TMemoryStream.Create;
    try
     l.LoadFromFile(aFileName);
     l.Position := 0;
     LoadFromStream(l);
    finally
      FreeandNil(l);
    end;
  end;
end;

function TCNCPythonConfiguration.LoadFromRepo(aBus: TBus) : boolean;
var l : TBusClientDataRepo;
    ls : TMemoryStream;
begin
  Assert(Assigned(aBus));
  result := false;
  l :=  TBusClientDataRepo.Create(aBus,CST_BUSDATAREPO_SERVERINFO);
  try
    ls := TMemoryStream.Create;
    try
      result := l.GetValue(CST_BUSDATAREPO_KEY_PYTHONCONFIGURATION,ls);
      if result then
      begin
        ls.Position := 0;
        LoadFromStream(ls);
      end;
    finally
      FreeAndNil(ls);
    end;
  finally
    FreeAndNil(l);
  end;
end;

procedure TCNCPythonConfiguration.SaveToFile(aFileName : String);
var l : TMemoryStream;
begin
  l := TMemoryStream.Create;
  try
    SaveToStream(l);
    l.Position := 0;
    l.SaveToFile(aFileName);
  finally
    FreeAndNil(l);
  end;
end;


procedure TCNCPythonConfiguration.LoadFromStream(aStream: TStream);
var i, l : integer;
    pid,pver,papiver,plibname,plibpath : String;
    li : TCNCPythonConfigurationItem;
begin
  FDefaultPythonConfId := '';
  PythonConfigurations.Clear;
  if Not( ReadString(aStream) = className) then
    raise Exception.Create(ClassName+' Wrong format : Unreadable.');
  FDefaultPythonConfId := ReadString(aStream);
  l := ReadInteger(aStream);
  if l=0 then
    exit;
  for I := 0 to l-1 do
  begin
    pid := ReadString(aStream);
    pver := ReadString(aStream);
    papiver := ReadString(aStream);
    plibname := ReadString(aStream);
    plibpath := ReadString(aStream);
    li := TCNCPythonConfigurationItem.Create;
    li.pythonVersion := pver;
    li.pythonAPIVersion := papiver;
    li.pythonLibName := plibname;
    li.pythonLibPath := plibpath;
    PythonConfigurations.Add(pid,TObject(li));
  end;
end;

procedure TCNCPythonConfiguration.PublishAll(aBus : TBus);
var l : TBusClientDataRepo;
    ls : TMemoryStream;
begin
  Assert(Assigned(aBus));
  l :=  TBusClientDataRepo.Create(aBus,CST_BUSDATAREPO_SERVERINFO);
  try
    ls := TMemoryStream.Create;
    try
      SaveToStream(ls);
      l.SetValue(CST_BUSDATAREPO_KEY_PYTHONCONFIGURATION,ls);
    finally
      FreeAndNil(ls);
    end;
  finally
    FreeAndNil(l);
  end;
end;

procedure TCNCPythonConfiguration.SaveToStream(aStream: TStream);
var i : Integer;
begin
  WriteString(aStream,ClassName); //signature.
  WriteString(aStream,FDefaultPythonConfId);
  WriteInteger(aStream,PythonConfigurations.Count);
  for I := 0 to PythonConfigurations.Count-1 do
  begin
    WriteString(aStream,PythonConfigurations[i].pythonVersion+'.'+PythonConfigurations[i].pythonAPIVersion);
    WriteString(aStream,PythonConfigurations[i].pythonVersion);
    WriteString(aStream,PythonConfigurations[i].pythonAPIVersion);
    WriteString(aStream,PythonConfigurations[i].pythonLibName);
    WriteString(aStream,PythonConfigurations[i].pythonLibPath);
  end;
end;


{ TCNCPythonConfigurationItem }

function TCNCPythonConfigurationItem.key: String;
begin
  result := pythonVersion+'.'+pythonAPIVersion;
end;

end.
