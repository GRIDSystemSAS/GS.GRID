unit GridServerConsole.AdminMode;
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

uses classes, sysutils,
     GS.Bus,
     GS.GRID.Server,
     GS.GRID.Server.Service.Types,
     GS.GRID.Server.Service.CentralCnC,
     GS.GRID.Server.Python.Conf,
     GS.GRID.Server.Service.CentralCnC.Users;

Procedure ConsoleAdminProcess(aServer : TGRIDServer);

implementation

var pentry : String;

procedure WaitForEntry;
begin
  readln(pentry);
end;


procedure DisplayMenu;
begin
//http://www.network-science.de/ascii/    (font smsslant)
  writeln('');
  writeln('  _________        ___   ___  __  ________  __   ');
  writeln(' / ___/ __/ ____  / _ | / _ \/  |/  /  _/ |/ /   ');
  writeln('/ (_ /\ \  /___/ / __ |/ // / /|_/ // //    /    ');
  writeln('\___/___/       /_/ |_/____/_/  /_/___/_/|_/     ');
  writeln('');
  writeln('');
  writeln(' 1 - User management');
  writeln(' 2 - Python Management');
  writeln(' 3 - Service Install');
  writeln('');
  writeln(' x : exit');
  writeln('');
end;

Procedure DisplayPythonConfig;
var l : TCNCPythonConfiguration;
begin
  l := TCNCPythonConfiguration.Create;
  try
    if FileExists(CST_CNC_FILENAME_PYTHONCONF) then
    begin
      l.LoadFromFile(CST_CNC_FILENAME_PYTHONCONF);
      Writeln('Display python configs : ');
      Writeln('');
      if l.PythonConfigurations.Count=0 then
        writeln(' -> No configuration found in the file.')
      else
      begin
        Writeln(l.AsString);
      end;
    end
    else
    begin
        writeln(' -> No Python configuration file found.');
    end;
    writeln('');
    writeln('<enter to go back>');
    readln;
  finally
    FreeAndNil(l);
  end;
end;

procedure NotifyCNCPython(aServer : TGridServer);
var l : TBusMessage;
begin
  //No need any data, message incomming will trig.
  aServer.GridBus.Send(l,CST_CHANNELNAME_CNC_PYTHONCONFUPDATE);
end;

procedure NotifyCNCUsr(aServer : TGridServer);
var l : TBusMessage;
begin
  //No need any data, message incomming will trig.
  aServer.GridBus.Send(l,CST_CHANNELNAME_CNC_USERCONFUPDATE);
end;

procedure PythonManagement_ADD(aServer : TGridServer);
var li,o : TCNCPythonConfigurationItem;
    l : TCNCPythonConfiguration;
    a,b,c,d : String;
begin
  writeln('Enter python version (<2.5>, <2.7>, <...>)');
  Readln(a);
  writeln('Enter python API version');
  Readln(b);
  writeln('Enter python Lib name (<python3.6.dll>, <libpython2.7.so>, <...>)');
  Readln(c);
  writeln('Enter python lib path');
  Readln(d);

  l := TCNCPythonConfiguration.Create;
  try
  if FileExists(CST_CNC_FILENAME_PYTHONCONF) then
    l.LoadFromFile(CST_CNC_FILENAME_PYTHONCONF);

   if trim(a+b) = '' then
   begin
     writeln('Version or APIVersion must be provided.');
     exit;
   end;

   if not l.PythonConfigurations.TryGetValue(a+b,TObject(o)) then
   begin
     li := TCNCPythonConfigurationItem.Create;
     li.pythonVersion := a;
     li.pythonAPIVersion := b;
     li.pythonLibName := c;
     li.pythonLibPath := d;
     l.PythonConfigurations.Add(li.key,li);
   end
   else
   begin
     //Update.
     o.pythonLibName := c;
     o.pythonLibPath := d;
   end;
   if trim(l.DefaultPythonConfId)='' then
     l.DefaultPythonConfId := a+'.'+b;
   l.SaveToFile(CST_CNC_FILENAME_PYTHONCONF);
   NotifyCNCPython(aServer);
  finally
    FreeAndNil(l);
  end;

end;

procedure PythonManagement_DEL(aServer : TGridServer);
var l : TCNCPythonConfiguration;
    s : String;
    ia : integer;
begin
  l := TCNCPythonConfiguration.Create;
  try
  if FileExists(CST_CNC_FILENAME_PYTHONCONF) then
  begin
    writeln('');
    writeln('');
    writeln('Delete python conf entry : ');
    writeln('');
    l.LoadFromFile(CST_CNC_FILENAME_PYTHONCONF);
    writeln(l.AsString);
    writeln('');
    writeln('Enter number to delete : ');
    readln(s);
    ia := StrToIntDef(s,0);
    dec(ia);
    if (ia>-1) and (UInt32(ia)<l.PythonConfigurations.Count) then
    begin
      writeln('Delete number '+IntToStr(ia+1)+' - '+l.PythonConfigurations[ia].key+' - confirm (Y/n) ?');
      readln(s);
      if s='Y' then
      begin
        if l.DefaultPythonConfId = l.PythonConfigurations[ia].key then
        begin
          l.DefaultPythonConfId := '';
          writeln('WARNING - Default python configuration ID has been lost : please design one via previous menu');
        end;
        l.PythonConfigurations.Remove(ia);
        writeln('configuration removed.');
        l.SaveToFile(CST_CNC_FILENAME_PYTHONCONF);
        NotifyCNCPython(aServer);
        writeln('python configuration updated.');
      end;
    end;
    writeln('');
    writeln('<enter to go back>');
    readln;
  end;
  finally
    FreeandNil(l);
  end;
end;

procedure PythonManagement_DefaultIDDefinition(aServer : TGridServer);
var l : TCNCPythonConfiguration;
    s : String;
    ia : Integer;
begin
  l := TCNCPythonConfiguration.Create;
  try
  if FileExists(CST_CNC_FILENAME_PYTHONCONF) then
  begin
    writeln('');
    writeln('');
    writeln('Define default python conf entry : ');
    writeln('');
    l.LoadFromFile(CST_CNC_FILENAME_PYTHONCONF);
    writeln(l.AsString);
    writeln('');
    writeln('Define default ID : Enter number to proceed.');
    readln(s);
    ia := StrToIntDef(s,0);
    dec(ia);
    if (ia>-1) and (UInt32(ia)<l.PythonConfigurations.Count) then
    begin
      writeln('Number '+IntToStr(ia+1)+' - '+l.PythonConfigurations[ia].key+' - confirm (Y/n) ?');
      readln(s);
      if s='Y' then
      begin
        l.DefaultPythonConfId := l.PythonConfigurations[ia].key;
        l.SaveToFile(CST_CNC_FILENAME_PYTHONCONF);
        NotifyCNCPython(aServer);
        writeln('python configuration updated.');
      end;
    end;
    writeln('');
    writeln('<enter to go back>');
    readln;
  end;
  finally
    FreeandNil(l);
  end;
end;


procedure PythonManagement(aServer : TGRIDServer);
begin
  while true do
  begin
    writeln('');
    writeln('');
    writeln('');
    writeln('   ___       __  __               ____       __  _                ');
    writeln('  / _ \__ __/ /_/ /  ___  ___    / __ \___  / /_(_)__  ___  ___   ');
    writeln(' / ___/ // / __/ _ \/ _ \/ _ \  / /_/ / _ \/ __/ / _ \/ _ \(_-<   ');
    writeln('/_/   \_, /\__/_//_/\___/_//_/  \____/ .__/\__/_/\___/_//_/___/   ');
    writeln('     /___/                          /_/                           ');
    writeln('');
    writeln(' 1 - view current Python configurations');
    writeln(' 2 - Add Python configuration entry');
    writeln(' 3 - Delete Python configuration entry');
    writeln(' 4 - Define Default Python configuration');
    writeln('');
    writeln(' x : exit');
    writeln('');
    WaitForEntry;
    if pentry = '1' then
      DisplayPythonConfig
    else
    if pentry = '2' then
    begin
      PythonManagement_ADD(aServer);
    end
    else
    if pentry = '3' then
    begin
      PythonManagement_DEL(aServer);
    end
    else
    if pentry = '4' then
    begin
      PythonManagement_DefaultIDDefinition(aServer);
    end
    else
    if pentry = 'x' then
      break;
  end;
end;

procedure DisplayUserConfig;
var l : TCNCUserConfiguration;
    u : TCNCUser;
    i : integer;
begin
  l := TCNCUserConfiguration.Create;
  try
    //Check from file only !
    if FileExists(CST_CNC_FILENAME_USERCONF) then
      l.LoadFromFile(CST_CNC_FILENAME_USERCONF);

    writeln('User List : '+IntToStr(l.UsersList.Count)+' user(s).');
    writeln('number -- User Name ');
    for I := 0 to l.UsersList.Count-1 do
    begin
      u := l.UsersList[i];
      writeln(format('%d   -- %s',[i,u.UserName]));
    end;
  finally
    FreeAndNil(l);
  end;

end;

procedure UserManagement_ADD(aServer : TGRIDServer);
var l : TCNCUserConfiguration;
    u : TCNCUser;
    a,b,c : String;
begin
  writeln('Enter user name');
  Readln(a);
  writeln('enter security password');
  Readln(b);
  writeln('is accredited ? ("1")');
  Readln(c);

  l := TCNCUserConfiguration.Create;
  try
    //Check from file only !
    if FileExists(CST_CNC_FILENAME_USERCONF) then
      l.LoadFromFile(CST_CNC_FILENAME_USERCONF);

    a := trim(a);
    b := trim(b);
    c := trim(c);

    if l.UsersList.Get(a,u) then
    begin
      Writeln('Users "'+a+'" already exists. Abort.');
      Exit;
    end;

    u := TCNCUser.Create;
    u.UserName := trim(a);
    u.Password := trim(b);
    u.Aggreement := c = '1';

    l.UsersList.Add(a,u);

    l.SaveToFile(CST_CNC_FILENAME_USERCONF);

    writeln('User "'+a+'" added.');

    NotifyCNCusr(aServer);
  finally
    FreeAndNil(l);
  end;

end;

procedure UserManagement_DEL(aServer : TGRIDServer);
var l : TCNCUserConfiguration;
    u : TCNCUser;
    a : string;
begin
  l := TCNCUserConfiguration.Create;
  try
    //Check from file only !
    if FileExists(CST_CNC_FILENAME_USERCONF) then
      l.LoadFromFile(CST_CNC_FILENAME_USERCONF);


    writeln('User deletion : ');
    if l.UsersList.Count>0 then
    begin
      writeln('----------------------------------------------');
      DisplayUserConfig;
      writeln('----------------------------------------------');
      writeln('Enter user name to delete : ');
      Readln(a);
      if l.UsersList.Get(a,u) then
      begin
        writeln('--> Delete "'+u.UserName+' : confirm Y/n : ');
        readln(a);
        if a = 'Y' then
        begin
          l.UsersList.Remove(u.UserName);
          l.SaveToFile(CST_CNC_FILENAME_USERCONF);
          writeln('user deleted.');
        end
        else
          writeln('User''s list remain untouched.');
      end
      else
        writeln('User "'+a+'" not found : User''s list remain untouched.');
    end;
  finally

  end;

end;



procedure UserManagement(aServer : TGridServer);
begin
  while true do
  begin
    writeln('');
    writeln('');
    writeln('');
    writeln('  __  __                           __   _');
    writeln(' / / / /___ ___  ____  ___   ___  / /_ (_)___   ___   ___');
    writeln('/ /_/ /(_-</ -_)/ __/ / _ \ / _ \/ __// // _ \ / _ \ (_-<');
    writeln('\____//___/\__//_/    \___// .__/\__//_/ \___//_//_//___/');
    writeln('                          /_/');
    writeln('');
    writeln(' 1 - view current users configurations');
    writeln(' 2 - Add user configuration entry');
    writeln(' 3 - Delete users configuration entry');
    writeln('');
    writeln(' x : exit');
    writeln('');
    WaitForEntry;
    if pentry = '1' then
      DisplayUserConfig
    else
    if pentry = '2' then
    begin
      UserManagement_ADD(aServer);
    end
    else
    if pentry = '3' then
    begin
      UserManagement_DEL(aServer);
    end
    else
    if pentry = 'x' then
      break;
  end;
end;


procedure ServiceManagement(aServer : TGridServer);
begin
  while true do
  begin
    writeln('');
    writeln('');
    writeln('');
    writeln('');
    writeln('   ____                 _             ____        __   _');
    writeln('  / __/___  ____ _  __ (_)____ ___   / __ \ ___  / /_ (_)___   ___   ___');
    writeln(' _\ \ / -_)/ __/| |/ // // __// -_) / /_/ // _ \/ __// // _ \ / _ \ (_-<');
    writeln('/___/ \__//_/   |___//_/ \__/ \__/  \____// .__/\__//_/ \___//_//_//___/');
    writeln('                                         /_/');
    writeln('');
    writeln('');
    writeln('');
    writeln(' 1 - View installed service');
    writeln(' 2 - Add a service Installation'); //by web and manual.
    writeln(' 3 - Delete Installation ');
    writeln('');
    writeln(' x : exit');
    writeln('');
    WaitForEntry;
    if pentry = '1' then
      DisplayPythonConfig
    else
    if pentry = '2' then
    begin
      PythonManagement_ADD(aServer);
    end
    else
    if pentry = '3' then
    begin
      PythonManagement_DEL(aServer);
    end
    else
    if pentry = '4' then
    begin
      PythonManagement_DefaultIDDefinition(aServer);
    end
    else
    if pentry = 'x' then
      break;
  end;
end;

Procedure ConsoleAdminProcess(aServer : TGRIDServer);
var entry : string;
    goOut : Boolean;
  procedure WaitForEntry;
  begin
    readln(entry);
  end;

  procedure DispatchByEntry;
  begin
    if entry = '1' then
      UserManagement(aServer)
    else
    if entry = '2' then
      PythonManagement(aServer)
    else
    if entry = '3' then
      ServiceManagement(aServer)
    else
    if entry = 'x' then
      goOut := true;
  end;
begin
  goOut := false;
  while not(goOut) do
  begin
    displaymenu;
    WaitForEntry;
    DispatchByEntry;
  end;
  writeln('');
  writeln('');
  writeln('');
  writeln('');
  writeln('');
  writeln('Back to server root menu : <h> for help.');
  writeln('');
end;

end.
