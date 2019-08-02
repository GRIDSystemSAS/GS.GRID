///Unit to execute quickly a python code.
/// this code will be execute on grid level Python configuration environnement.
/// It is executed into its own thread : On a protocol (KissB) point of view
/// (I.e. It is synchrone, from the network client pov)
///
/// Limitation :
/// - Only one python configuration avalaible : Gridserver level one.
/// - No Input on stdIn.
/// - Its output (stdOut+stderr) will be gathered during its running and
///   delivered only after the end of its execution.
/// Please See GS.GRID.Task.Python to not have those limitations,
/// by the cost of more complexe system.
unit GS.GRID.SplAPI.InstantPython;
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
 GS.Bus,
 GS.GRID.Server.Service.Server,
 GS.GRID.Common.Protocols.KissB,
 GS.GRID.Server.Service.Server.BasedProtocols,
 GS.GRID.SplAPI.Base,
 GS.GRID.Server.Python.Conf,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Python,
 PythonEngine;

type
 TGRIDSimpleAPIInstantPython = Class(TGRIDSimpleAPI)
 private
   FConf : TCNCPythonConfiguration;
   FRepo : TBusClientDataRepo;
 public
   constructor create(const Server: TGRIDServiceServerBasedProtocol;
                             aUser : TGridServerUser; aProtocol: TGRIDProtocol_KissB;
                             aDataStream, aResultStream : TMemoryStream); Override;
   destructor Destroy; Override;

   function InstantPythonRun(code : UTF8String) : UTF8String;
   function Version : UTF8String;
   procedure check;
 End;

implementation

{ TGRIDSimpleAPIPython }

procedure TGRIDSimpleAPIInstantPython.check;
begin
  if not FConf.Ready then
    raise Exception.Create(ClassName+' : Python Configuration not available');
end;

constructor TGRIDSimpleAPIInstantPython.create(
  const Server: TGRIDServiceServerBasedProtocol; aUser: TGridServerUser;
  aProtocol: TGRIDProtocol_KissB; aDataStream, aResultStream: TMemoryStream);
begin
  inherited;
  FConf := TCNCPythonConfiguration.Create;
  FConf.LoadFromRepo(Server.MasterThread.Bus); //get current configuration from bus data share system.
  FRepo := TBusClientDataRepo.Create(Server.MasterThread.Bus,CST_BUSDATAREPO_INSTANTPYTHON);
end;

destructor TGRIDSimpleAPIInstantPython.Destroy;
begin
  FreeAndNil(FConf);
  FreeAndNil(FRepo);
  inherited;
end;

function TGRIDSimpleAPIInstantPython.InstantPythonRun(code: UTF8String): UTF8String;
var lcode : TStringList;
    ls : TMemoryStream;
begin
  lcode := TStringList.Create;
  ls := TMemoryStream.Create;
  try
    lcode.Text := trim(code);
    FRepo.ClearValue(IntToStr(TThread.CurrentThread.ThreadID));
    TGRIDPython.GRIDPythonEngine.ExecStrings(lcode);
    FRepo.GetValue(IntToStr(TThread.CurrentThread.ThreadID),ls);
    result := StampedStringItemsToString(StreamToStampedStringItems(ls));
  finally
    freeAndNil(lcode);
    freeAndNil(ls);
  end;
end;

function TGRIDSimpleAPIInstantPython.Version: UTF8String;
var FPythonEngine : TPythonEngine;
begin
  result := FConf.AsString;
end;

end.
