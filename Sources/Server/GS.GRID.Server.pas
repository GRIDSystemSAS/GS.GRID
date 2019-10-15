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

TCustomGridServer = class abstract
protected
  FGridBus : TGridBus;
  //Service Runner. (Private bus for Service system)
  FServices : TServiceManager;
  FOwnedTask: Boolean;

  Procedure Stop; //Never used : Called by detructor : Start can be called only once.
public
  function AddGRIDService(aGridService : TGRIDService; const aStartServiceImmediately : Boolean = False;
                                                        const aWaitForStart : boolean = true) : TCustomService;

  Procedure Start;
  Procedure Tests;

  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function Stats(aBus : TBus) : String;

  Procedure LogServer( const Text : String;
                   const Module : String = '';
                   const Category : TGridLogCategory = TGridLogCategory.glcInfo;
                   const aForcedClassName : String = '');

  Property GridBus : TGridBus read FGridBus;

  Property OwnedTask : Boolean read FOwnedTask Write FOwnedTask;
  Property Services : TServiceManager read FServices;
end;

//Complete Grid server for Server class application.
TGridServer = class(TCustomGridServer)
protected
  FCnc : TCustomService;
public
  Constructor Create; override;
  Destructor Destroy; Override;
end;

//Grid server for app : Only bus and hypervisor.
//No security, no service responding on mqtt or kissb protocol : Zhe minimal to
//work with hypervisor and bus services.
TCustomEmbededGridServer = class(TCustomGridServer)
public
  constructor Create; Override;
end;

implementation

{$IF DEFINED(DCC) AND DEFINED(MSWINDOWS)}
  //Delphi Win only.
  Uses GS.GRID.Server.Service.Server.WinNamedPipeServer;
{$ENDIF}


{ TCustomGridServer }

Function TCustomGridServer.AddGRIDService( aGridService: TGRIDService;
                                            const aStartServiceImmediately : Boolean = False;
                                            const aWaitForStart : boolean = true) : TCustomService;
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
  result := ls;
end;

constructor TCustomGridServer.Create;
var i : Integer;
begin
  Inherited;
  FGridBus := TGridBus.Create;
  FGridBus.Start;

  FServices := TServiceManager.Create;
  FOwnedTask := True;
end;

destructor TCustomGridServer.Destroy;
begin
  Stop; //all other "forgotten" services
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
  if FServices.Suspended then
  begin
    FServices.StartAllServices;
    FServices.Start;
  end;
end;

procedure TCustomGridServer.Stop;
begin
  FServices.StopAllServices;
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

{ TGridServer }

constructor TGridServer.Create;
var i : Integer;
begin
  Inherited; //Mandatory

  fCnc := AddGRIDService(TGRIDServiceCentralCnC.GetDefaultImplementation,true); //Start CNC first, for log and system stuff.
  Sleep(1000);
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


destructor TGridServer.Destroy;
var i : integer;
    ll : TServiceList;

begin
  LogServer('Server shuting down...');
  //Manually stop all known service (To have may of log.)
  ll := Services.ServicesLock;
  try
    for I := 0 to ll.Count-1 do
    begin
      if (ll[i] <> TCustomService(FCnc)) then
      begin
        LogServer(' tentatively stopping '+ll[i].ServiceName);
        ll[i].StopService;
        LogServer(' Service stoped : '+ll[i].ServiceName);
      end;
    end;
  finally
    Services.ServicesUnlock;
  end;
  LogServer('done.');
  //All service are stoped, cnc remain here.
  LogServer('Server stopping cnc...');
  Sleep(1000);
  FCnc.StopService;
  inherited; //Mandatory
end;


{ TCustomEmbededGridServer }

constructor TCustomEmbededGridServer.Create;
var s : TCustomService;
    fh : TGridHypervisor;
begin
  inherited;
  s := AddGRIDService(TGridHypervisor.GetDefaultImplementation,true);
  fh := TGridHypervisor(s.Task);
  while not fh.ServiceReady do;
end;

end.
