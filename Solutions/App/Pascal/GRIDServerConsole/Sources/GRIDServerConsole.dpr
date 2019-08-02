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
  GS.GRID.Server.Service.Hypervisor in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Service.Hypervisor.pas',
  {$ENDIF }
  GS.GRID.Server in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.pas',
  GS.GRID.Server.Service.Server in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Service.Server.pas',
  GS.GRID.Server.Service.Types in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Service.Types.pas',
  GS.GRID.Server.Service.CentralCnC in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Service.CentralCnC.pas',
  GS.GRID.Common.Protocols in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Protocols.pas',
  {$IFDEF DCC}
  {$IFDEF MSWINDOWS}
  FWIOCompletionPipes in '..\..\..\..\..\Sources\Pascal\ThirdPart\Windows\NamedPipes\FWIOCompletionPipes.pas',
  uNamedPipesExchange in '..\..\..\..\..\Sources\Pascal\ThirdPart\Windows\NamedPipes\uNamedPipesExchange.pas',
  GS.GRID.Server.Service.Server.WinNamedPipeServer in '..\..\..\..\..\Sources\Pascal\Server\ServiceServerImplementation\GS.GRID.Server.Service.Server.WinNamedPipeServer.pas',
  GS.GRID.Client.Transport.NamedPipes in '..\..\..\..\..\Sources\Pascal\Client\ClientImplementation\GS.GRID.Client.Transport.NamedPipes.pas',
  {$ENDIF }
  {$ENDIF }
  GS.GRID.Client in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.pas',
  GS.GRID.Client.Transport in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.Transport.pas',
  GS.GRID.Client.Transport.IndyTCP in '..\..\..\..\..\Sources\Pascal\Client\ClientImplementation\GS.GRID.Client.Transport.IndyTCP.pas',
  GS.GRID.Server.Service.Server.IndyTCPServer in '..\..\..\..\..\Sources\Pascal\Server\ServiceServerImplementation\GS.GRID.Server.Service.Server.IndyTCPServer.pas',
  {$IFDEF UDP_SERVER_SUPPORT}
  GS.GRID.Server.Service.Server.IndyUDPServer in '..\..\..\..\..\Sources\Pascal\Server\ServiceServerImplementation\GS.GRID.Server.Service.Server.IndyUDPServer.pas',
  GS.GRID.Server.Service.Server.IndyUDPServer.SDDPLike in '..\..\..\..\..\Sources\Pascal\Server\ServiceServerImplementation\GS.GRID.Server.Service.Server.IndyUDPServer.SDDPLike.pas',
  {$ENDIF }
  GS.GRID.Server.Service.Server.BasedProtocols in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Service.Server.BasedProtocols.pas',
  GS.GRID.Server.Service.Server.Protocol.ChatExemple in '..\..\..\..\..\Sources\Pascal\Server\ServerSideProtocols\GS.GRID.Server.Service.Server.Protocol.ChatExemple.pas',
  GS.GRID.Common.Protocols.Example in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Protocols.Example.pas',
  GS.GRID.Server.Service.CentralCnC.Informations in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Service.CentralCnC.Informations.pas',
  GS.GRID.Server.Service.Server.Protocol.KissB in '..\..\..\..\..\Sources\Pascal\Server\ServerSideProtocols\GS.GRID.Server.Service.Server.Protocol.KissB.pas',
  uMQTTEx in '..\..\..\..\..\Sources\Pascal\ThirdPart\MQTT\uMQTTEx.pas',
  GS.GRID.Server.Service.Server.Protocol.MQTT in '..\..\..\..\..\Sources\Pascal\Server\ServerSideProtocols\GS.GRID.Server.Service.Server.Protocol.MQTT.pas',
  GS.GRID.Client.Example in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.Example.pas',
  GS.GRID.Client.KissB in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.KissB.pas',
  GS.GRID.SplAPI.Base in '..\..\..\..\..\Sources\Pascal\Server\SimpleApi\GS.GRID.SplAPI.Base.pas',
  GS.GRID.SplAPI.GetInfos in '..\..\..\..\..\Sources\Pascal\Server\SimpleApi\GS.GRID.SplAPI.GetInfos.pas',
  GS.GRID.SplAPI.InstantPython in '..\..\..\..\..\Sources\Pascal\Server\SimpleApi\GS.GRID.SplAPI.InstantPython.pas',
  GS.GRID.SplAPI.KeyValue in '..\..\..\..\..\Sources\Pascal\Server\SimpleApi\GS.GRID.SplAPI.KeyValue.pas',
  GS.GRID.Server.Python.Conf in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Python.Conf.pas',
  GridServerConsole.AdminMode in '..\..\..\..\..\Sources\Pascal\Server\ConsoleAdmin\GridServerConsole.AdminMode.pas',
  GS.GRID.Server.Python in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Python.pas',
  GS.GRID.Server.Service.Server.Protocol.KissB.Bus in '..\..\..\..\..\Sources\Pascal\Server\ServerSideProtocols\GS.GRID.Server.Service.Server.Protocol.KissB.Bus.pas',
  GS.GRID.Server.Tests in '..\..\..\..\..\Sources\Pascal\Server\Tests\GS.GRID.Server.Tests.pas',
  GS.GRID.Server.Tests.KissB in '..\..\..\..\..\Sources\Pascal\Server\Tests\GS.GRID.Server.Tests.KissB.pas',
  GS.GRID.Common.Protocols.KissB in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Protocols.KissB.pas',
  GS.GRID.Common.Protocols.MQTT in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Protocols.MQTT.pas',
  GS.GRID.Common.Protocols.MicroService in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Protocols.MicroService.pas',
  GS.GRID.Server.Service.Server.MicroServices in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Service.Server.MicroServices.pas',
  {$IFDEF WEBSOCKET_SUPPORT}
  GS.GRID.Server.Service.Server.mORMotWebSocketServer in '..\..\..\..\..\Sources\Pascal\Server\ServiceServerImplementation\GS.GRID.Server.Service.Server.mORMotWebSocketServer.pas',
  {$ENDIF }
  GS.GRID.Common.Types in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Types.pas',
  os_api_unit in '..\..\..\..\..\Sources\Pascal\ThirdPart\OSapi\os_api_unit.pas',
  GS.GRID.Server.Service.CentralCnC.Users in '..\..\..\..\..\Sources\Pascal\Server\GS.GRID.Server.Service.CentralCnC.Users.pas';

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
