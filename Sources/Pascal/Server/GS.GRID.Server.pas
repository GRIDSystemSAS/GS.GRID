unit GS.GRID.Server;
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
     GS.Bus,
     GS.Bus.Services,
     GS.GRID.Common.Types,
     GS.GRID.Server.Service.Types,
{$IFDEF HYPERVISOR_SUPPORT}
     GS.GRID.Server.Service.Hypervisor,
{$ENDIF}
     GS.GRID.Server.Service.CentralCnC,
     GS.GRID.Server.Service.Server,
     GS.GRID.Server.Python;
Type

TThreadTests = Class(TThread)
  Services : TServiceManager;
  Procedure Execute; Override;
End;

TCustomGridServer = class
protected
  FGridBus : TGridBus;
{$IFDEF HYPERVISOR_SUPPORT}
  FHypervisor : TGridHypervisor;
{$ENDIF}
  //Service Runner. (Private bus for Service system)
  FServices : TServiceManager;
  FOwnedTask: Boolean;
public
  Procedure AddGRIDService(aGridService : TGRIDService; const aStartServiceImmediately : Boolean = False;
                                                        const aWaitForStart : boolean = true);

  Procedure Start;
  Procedure Stop;

  Procedure Tests;

  Constructor Create; Virtual;
  Destructor Destroy; Override;
  Function Stats(aBus : TBus) : String;

  Procedure LogServer( const Text : String;
                   const Module : String = '';
                   const Category : TGridLogCategory = TGridLogCategory.glcInfo;
                   const aForcedClassName : String = '');

  Property GridBus : TGridBus read FGridBus;
{$IFDEF HYPERVISOR_SUPPORT}
  Property Hypervisor : TGridHypervisor read FHypervisor;
{$ENDIF}
  Property OwnedTask : Boolean read FOwnedTask Write FOwnedTask;
  Property Services : TServiceManager read FServices;

end;

TGridServer = class(TCustomGridServer)
end;

implementation

{$IF DEFINED(DCC) AND DEFINED(MSWINDOWS)}
  //Delphi Win only.
  Uses GS.GRID.Server.Service.Server.WinNamedPipeServer;
{$ENDIF}


{ TCustomGridServer }

procedure TCustomGridServer.AddGRIDService( aGridService: TGRIDService;
                                            const aStartServiceImmediately : Boolean = False;
                                            const aWaitForStart : boolean = true);
var ls : TCustomService;
begin
  Assert(Assigned(FgridBus));
  Assert(Assigned(aGridService));
  aGridService.GridBus := FGridBus;
  aGridService.Server := Self;
  ls :=  TService.Create;
  ls.Task := aGridService;
  FServices.RegisterService(ls);
  if aStartServiceImmediately then
    ls.StartService(aWaitForStart);
end;
constructor TCustomGridServer.Create;
var i : Integer;
begin
  Inherited;
  FGridBus := TGridBus.Create;
  FGridBus.Start;

  FServices := TServiceManager.Create;
  FOwnedTask := True;

  AddGRIDService(TGRIDServiceCentralCnC.GetDefaultImplementation,true); //Start CNC first, for log and system stuff.
{$IFDEF HYPERVISOR_SUPPORT}
  AddGRIDService(TGridHypervisor.GetDefaultImplementation,true);
{$ENDIF}
  logserver(IntToStr(Length(GLB_ServerService_ImplClasses))+' Server services found : ',className);
  for I := 0 to Length(GLB_ServerService_ImplClasses)-1 do
  begin
    logserver(' --> Starting Server services '+IntToStr(i+1)+' : '+GLB_ServerService_ImplClasses[i].ClassName+'...');
    AddGRIDService(GLB_ServerService_ImplClasses[i].GetDefaultImplementation,true);
  end;

  {$IFDEF DEBUG}
    {$IFDEF DCC}
      {$IFDEF WIN32}
  //Delphi Win32 only (WinPipe not compliant in 64bit - to see}
  // - test purpose only -
  AddGRIDService(TGRIDServiceWinNamedPipeServer.GetDefaultImplementation,true);
  while not(TGRIDServiceWinNamedPipeServer(Services.Services[Services.ServiceCount-1].Task).ServerReady) do;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

destructor TCustomGridServer.Destroy;
begin
  Stop;
  FServices.UnregisterAllServices(FOwnedTask);
  FreeAndNil(FServices);
  FreeandNil(FGridBus);
  inherited;
end;

procedure TCustomGridServer.LogServer( const Text : String;
                                       const Module : String = '';
                                       const Category : TGridLogCategory = TGridLogCategory.glcInfo;
                                       const aForcedClassName : String = '');
var lLogMessage : TBusMessage;
    ls : TGRIDLogChunk;
    l : TMemoryStream;
begin
  ls.DateTime := now;
  ls.ThreadID := TThread.CurrentThread.ThreadID;
  ls.LogText := Text;
  ls.Category := TGridLogCategory.glcInfo;
  if aForcedClassName<>'' then
    ls.LoggerClassName := aForcedClassName
  else
    ls.LoggerClassName := ClassName;
  ls.Module := Module;
  l := lLogMessage.AsStream;
  try
    ls.Serialize(TStream(l));
    lLogMessage.FromStream(l);
    FGridBus.Send(lLogMessage,CST_CHANNELNAME_LOGROOT);
  finally
    FreeAndNil(l);
  end;
end;

procedure TCustomGridServer.Start;
begin
  Assert(assigned(FGridBus));
  FServices.StartAllServices;
  FServices.Start;
end;

procedure TCustomGridServer.Stop;
begin
  FServices.StopAllServices;
  { TODO 1 -oVGS -cFeature :
CnC service should be the last to stop. Hence, got not all log.
To change. (perhaps put log inti GridServer level ?) }
end;


function TCustomGridServer.Stats(aBus : TBus): String;
var ls : TStringList;
    lt : String;
begin
  Assert(assigned(aBus));
  ls := TStringList.Create;
  try
    aBus.GetChannelsConfigurationAsCSV(ls);
    lt := ls.Text;
    aBus.GetSubscribtersConfigurationAsCSV(ls);
    lt := lt + ls.Text;
    result := lt;
  finally
    FreeAndNil(ls);
  end;
end;


procedure TCustomGridServer.Tests;
var l : TThreadTests;
begin
  //Put this test into a thread for Win Console compliance.
  l := TThreadTests.Create(true);
  l.FreeOnTerminate := true;
  l.Services := Services;
  l.Start;
end;

{ TThreadTests }

procedure TThreadTests.Execute;
var i : integer;
    lServices : TServiceList;
begin
  lServices := Services.ServicesLock;
  Services.ServicesUnlock;
  try
    for I := 0 to lServices.Count-1 do
    begin
      if Assigned(lServices[i].Task) then
        if lServices[i].Task is TGRIDService then
          TGRIDService(lServices[i].Task).AutoTest;
    end;
  finally
//    Services.ServicesUnlock;
  end;
end;

end.
