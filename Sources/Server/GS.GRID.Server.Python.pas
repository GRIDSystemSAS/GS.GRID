// This unit wrap Python4Lazarus/4Delphi. "Singleton" architecture.
// Delphi4Lazarus (origin lib) is designed as fellow : TPythonEngine must be the only instance,
// because it deals with direct callback from Python lib.
// For "InstantPython" feature, we will used this centralized solution, with direct use of centralized engine call.
// For Tasks, we will used TPhythonthread.
unit GS.GRID.Server.Python;
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
 GS.Bus,
 GS.GRID.Server.Python.Conf,
 GS.GRID.Server.Service.Types,
 PythonEngine;

Type
  TPythonDummy = class
  public
    procedure InternalPythonDataU(Sender: TObject; const Data : UnicodeString);
  end;


  //
  TGRIDPythonExecutionEnvironnement = class(TPythonThread)
  private
  public
    Code : TStringList;
    procedure ExecuteWithPython; Override;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TGRIDPython = class
  private
    Class var PythonInstance : TPythonEngine;
    Class var DataCom : TBusClientDataRepo;
    Class var OwnThreadState: PPyThreadState;
  public
    class procedure clean;
    class function Setup(aDataBus : TBus) : Boolean; //Hot configuration swap
    class function GRIDPythonEngine : TPythonEngine;
    class function GetExecutionEnvironnement : TGRIDPythonExecutionEnvironnement;
  end;

implementation

var gPythonDummy : TPythonDummy;
{ TGRIDPython }

class procedure TGRIDPython.clean;
begin
  { TODO -oVGS -cCritical : Warning : PythonEngine or Python thread may be in-use somewhere ! Do Something here... }
  if Assigned(DataCom) then
    FreeAndNil(DataCom);

  if Assigned(PythonInstance) then
  begin
    PythonInstance.IO.Free;
    FreeAndNil(PythonInstance);
  end;
end;

class function TGRIDPython.GetExecutionEnvironnement: TGRIDPythonExecutionEnvironnement;
begin
  result := TGRIDPythonExecutionEnvironnement.Create;
end;

class function TGRIDPython.GRIDPythonEngine: TPythonEngine;
begin
  if Not Assigned(PythonInstance) then
    raise Exception.Create(ClassName + ' : Python instance not available : Please check parameters server side.');
  result := PythonInstance;
end;

class function TGRIDPython.Setup(aDataBus : TBus) : Boolean;
var FPython : TCNCPythonConfigurationItem;
    FConf : TCNCPythonConfiguration;
    FconfID : String;
    lcode : TStringList;
    FO : TObject;
begin
  result := false;

  FConf := TCNCPythonConfiguration.Create;
  try
    FConf.LoadFromRepo(aDataBus);
    if FConf.PythonConfigurations.Count=0 then
    begin
      raise Exception.Create(ClassName + ' : Python configuration file empty : no configuration available. Abort.');
    end;

    try
      Clean;
    Except
      On E : Exception do
      begin
        raise Exception.Create(ClassName + ' : Failed to reset centralized Python engine : '+E.Message);
      end;
    end;

    PythonInstance := TPythonEngine.Create(nil);
    PythonInstance.IO := TPythonInputOutput.Create(nil);
    PythonInstance.IO.UnicodeIO := true;
    PythonInstance.IO.OnSendUniData := gPythonDummy.InternalPythonDataU;
    Datacom := TBusClientDataRepo.Create(aDataBus,CST_BUSDATAREPO_INSTANTPYTHON);
    try
      if FConf.DefaultPythonConfId<>'' then
        FconfID := FConf.DefaultPythonConfId
      else
        FconfID := Fconf.PythonConfigurations[0].key; //First if no configuration by default found.

      if Fconf.PythonConfigurations.TryGetValue(FConfID,TObject(FPython)) then
      begin
        PythonInstance.DllName := FPython.pythonLibName;
        PythonInstance.DllPath := FPython.pythonLibPath;
        //PythonInstance.SetPythonHome(FPython.pythonLibPath);
      end
      else
      begin
        raise Exception.Create(ClassName+' : Python configuration "'+FconfID+'" not found.');
      end;

      PythonInstance.LoadDll;

      PythonInstance.InitThreads := true;

      OwnThreadState := PythonInstance.PyThreadState_Get;
      PythonInstance.PyEval_ReleaseThread(OwnThreadState);


    Except
      On E : Exception do
      begin
        raise Exception.Create(ClassName + ' : Python configuration setting failed.');
      end;
    end;
  finally
    FreeAndNil(FConf);
  end;
end;

{ TPythonDummy }

procedure TPythonDummy.InternalPythonDataU(Sender: TObject;
  const Data: UnicodeString);
begin
  TGRIDPython.DataCom.SetValueStamped(IntToStr(TThread.CurrentThread.ThreadID),Data);
end;

{ TGRIDPythonExecutionEnvironnement }

constructor TGRIDPythonExecutionEnvironnement.Create;
begin
  inherited Create(true);
  code := TStringList.Create;
  //ThreadExecMode := emNewInterpreter;
  ThreadExecMode := emNewState;
end;

destructor TGRIDPythonExecutionEnvironnement.Destroy;
begin
  FreeAndNil(code);
  inherited;
end;

procedure TGRIDPythonExecutionEnvironnement.ExecuteWithPython;
var pyfunc: PPyObject;
begin
  //some explaination : Mutlithreads over Python dll and Pyt4Del is difficult to stabilize.
  //in fact, 2 pure running of python scripts will crash the later one, in "ChangeState" mode.
  //"We cannot use "newInterpreter" mode because of *huge* memory cunsumption (anyway, it has others probs) execution.
  //So, the better way is to prepare your python code and call a func : that's work much and seems stable enought.
  //--> If your read that, and have solution to launch "classic" python code, I'm open and curious.


  if Pos('gridmain()',code.Text)=0 then
    raise Exception.Create('Error Message - no gridmain() function define');

  TGRIDPython.PythonInstance.ExecStrings(code);
  pyfunc :=  TGRIDPython.PythonInstance.FindFunction(TGRIDPython.PythonInstance.ExecModule, 'gridmain');
  if Assigned(pyfunc) then
  begin
    try
      TGRIDPython.PythonInstance.EvalFunction(pyfunc,[]);
    finally
      TGRIDPython.PythonInstance.Py_DecRef(pyfunc);
    end;
  end;
end;

Initialization
  gPythonDummy :=  TPythonDummy.Create;
Finalization
  FreeAndNil(gPythonDummy);
end.
