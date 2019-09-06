program GRIDServerConsole;

//
///
///  *** LICENSE *****
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
///  This licence applies only on file marked with this header, and does *NOT*
///  apply to the file under "thirdpart" directories or if file have no or other
///  licence header contents.
///
///  This licence condition could, under condition, be leverage by its authors.
///  **********
//

//
///
/// Promoted and sponsored by GRID SYSTEM S.A.S, France
/// VincentOnGitHub (at) grids.systems
///
//

{$IFDEF DCC}
  {$APPTYPE CONSOLE}
{$ENDIF }

{$I GSCore.inc}

uses
  {$IFDEF DCC}
    {$IFDEF MSWINDOWS}
      {$IFDEF DEBUG}
  FastMM4,
      {$ELSE}
  ScaleMM2,
      {$ENDIF }
    {$ENDIF }
  {$ENDIF }
  {$IFDEF FPC}
  {$IFDEF unix}
  cthreads,
  {$ENDIF }
  {$ENDIF }
  SysUtils,
  Classes,
  GS.Bus.Services,
  {$IFDEF HYPERVISOR_SUPPORT}
  GS.GRID.Server.Service.Hypervisor,
  {$ENDIF }
  GS.GRID.Server,
  GS.GRID.Server.Service.Server,
  GS.GRID.Server.Service.Types,
  GS.GRID.Server.Service.CentralCnC,
  GS.GRID.Common.Protocols,
  {$IFDEF DCC}
  {$IFDEF MSWINDOWS}
  FWIOCompletionPipes,
  uNamedPipesExchange,
  GS.GRID.Server.Service.Server.WinNamedPipeServer,
  GS.GRID.Client.Transport.NamedPipes,
  {$ENDIF }
  {$ENDIF }
  GS.GRID.Client,
  GS.GRID.Client.Transport,
  GS.GRID.Client.Transport.IndyTCP,
  GS.GRID.Server.Service.Server.IndyTCPServer,
  {$IFDEF UDP_SERVER_SUPPORT}
  GS.GRID.Server.Service.Server.IndyUDPServer,
  GS.GRID.Server.Service.Server.IndyUDPServer.SDDPLike,
  {$ENDIF }
  GS.GRID.Server.Service.Server.BasedProtocols,
  GS.GRID.Server.Service.Server.Protocol.ChatExemple,
  GS.GRID.Common.Protocols.Example,
  GS.GRID.Server.Service.CentralCnC.Informations,
  GS.GRID.Server.Service.Server.Protocol.KissB,
  uMQTTEx,
  GS.GRID.Server.Service.Server.Protocol.MQTT,
  GS.GRID.Client.Example,
  GS.GRID.Client.KissB,
  GS.GRID.SplAPI.Base,
  GS.GRID.SplAPI.GetInfos,
  GS.GRID.SplAPI.InstantPython,
  GS.GRID.SplAPI.KeyValue,
  GS.GRID.Server.Python.Conf,
  GridServerConsole.AdminMode,
  GS.GRID.Server.Python,
  GS.GRID.Server.Service.Server.Protocol.KissB.Bus,
  GS.GRID.Server.Tests,
  GS.GRID.Server.Tests.KissB,
  GS.GRID.Common.Protocols.KissB,
  GS.GRID.Common.Protocols.MQTT,
  GS.GRID.Common.Protocols.MicroService,
  GS.GRID.Server.Service.Server.MicroServices,
  {$IFDEF WEBSOCKET_SUPPORT}
  GS.GRID.Server.Service.Server.mORMotWebSocketServer,
  {$ENDIF }
  GS.GRID.Common.Types,
  os_api_unit,
  GS.GRID.Server.Service.CentralCnC.Users;

type
  TAppConsole = class
  private
    function Help : string;
  Public
   Grid : TGridServer;
   Procedure DoDisplay(Sender : TObject; Const aLogText : String);
   procedure DoDisplayTaskStats;
   procedure DoAdmin;
   Procedure Init;
   Procedure Loop;
   Procedure Close;
  end;

{ TAppConsole }

procedure TAppConsole.DoAdmin;
begin
  ConsoleAdminProcess(Grid);
end;

procedure TAppConsole.DoDisplay(Sender : TObject; Const aLogText : String);
begin
  Writeln(aLogText);
end;

procedure TAppConsole.DoDisplayTaskStats;
var l : TServiceList;
   i : Integer;
begin
  l :=Grid.Services.ServicesLock;
  try
    for I := 0 to l.Count-1 do
    begin
      if l[i].Task is TCustomGRIDServiceServer then
      begin
        writeln(l[i].Task.ClassName+' ---------------------------------------');
        writeln(TCustomGRIDServiceServer(l[i].Task).stats);
      end
      else
      begin
        writeln(l[i].Task.ClassName+' : n/a');
      end;
    end;
  finally
    Grid.Services.ServicesUnlock;
  end;
end;

procedure DoAdmin;
begin
end;

function TAppConsole.Help: string;
var ls :  TStringList;
begin
  ls := TStringList.Create;
  try
    ls.Add('GRIDServer * Command line quick doc');
    ls.Add(' "quit"      - Stop the server, and quit. (alternative : "exit" or "q")');
    ls.Add(' "help"      - This help');
    ls.Add(' "gridbus"   - GridBus statistics.');
    ls.Add(' "hbus"      - Hypervisor bus statistics.');
    ls.Add(' "hservices  - Hypervisor services statistics. (Alternative :  "hs")');
    ls.Add(' "htasks"    - Hypervisor tasks description. (Alternative :  "ht")');
    ls.Add(' "dtasks"    - Hypervisor''s services tasks description details. (Alternative :  "dt")');
    ls.Add(' "test"      - Launch service''s autotests. (Alternative :  "t")');
    ls.Add(' "admin"     - administration');
    result := ls.Text;
  finally
    FreeAndNil(ls);
  end;
end;

procedure TAppConsole.Init;
begin
  Grid := TGridServer.Create;
  Grid.Start;
end;

Procedure TAppConsole.Loop;
var ls : String;
begin
  ls :=EmptyStr;
  DoDisplay(Self,Grid.ClassName + ' - "Quit" to stop.');
  while (ls<>'quit') And (ls<>'exit')  and (ls<>'q') do
  begin
    ls := lowercase(trim(ls));
    if (ls = 'help') or (ls = 'h') then
    begin
      DoDisplay(Self,Help);
    end
    else
    if ls = 'gridbus' then
    begin
      DoDisplay(Self,Grid.Stats(Grid.GridBus));
    end
    else
    if ls = 'hbus' then
    begin
      DoDisplay(Self,Grid.Stats(Grid.Services));
    end
    else
    if (ls = 'hservices')  or (ls = 'hs') then
    begin
      DoDisplay(Self,Grid.Services.StatsServices);
    end
    else
    if (ls = 'htasks') or (ls = 'ht') then
    begin
      DoDisplay(Self,Grid.Services.StatsTask);
    end
    else
    if (ls = 'dtasks') or (ls = 'dt') then
    begin
      DoDisplayTaskStats;
    end
    else
    if (ls = 'test') or (ls = 't') then
    begin
      Grid.Tests;
    end
    else
    if (ls = 'admin') or (ls = 'a') then
    begin
      doAdmin;
    end
    else
    begin
      if ls<>EmptyStr then
        DoDisplay(Self,'Uknown command');
    end;
    Readln(ls);
  end;
  DoDisplay(Self,Grid.ClassName + ' - Terminating....');

end;

procedure TAppConsole.Close;
begin
  FreeAndNil(Grid);
end;

var App : TAppConsole;

begin
  try
    App := TAppConsole.Create;
    try
      App.Init;
      App.Loop;
      App.close;
    finally
      FreeAndNil(App);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
