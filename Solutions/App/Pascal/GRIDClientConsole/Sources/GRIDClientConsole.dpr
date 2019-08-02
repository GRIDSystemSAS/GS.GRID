program GRIDClientConsole;
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
{$APPTYPE CONSOLE}

{$R *.res}

{$IFDEF DCC}
  {$APPTYPE CONSOLE}
{$ENDIF }

{$IFDEF FPC}
  {$MODE DELPHI}
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
  GS.GRID.Common.Protocols.Example in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Protocols.Example.pas',
  GS.GRID.Common.Protocols in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Protocols.pas',
  GS.GRID.Client.Transport.IndyTCP in '..\..\..\..\..\Sources\Pascal\Client\ClientImplementation\GS.GRID.Client.Transport.IndyTCP.pas',
  {$IFDEF DCC}
  {$IFDEF MSWINDOWS}
  GS.GRID.Client.Transport.NamedPipes in '..\..\..\..\..\Sources\Pascal\Client\ClientImplementation\GS.GRID.Client.Transport.NamedPipes.pas',
  uNamedPipesExchange in '..\..\..\..\..\Sources\Pascal\ThirdPart\Windows\NamedPipes\uNamedPipesExchange.pas',
  FWIOCompletionPipes in '..\..\..\..\..\Sources\Pascal\ThirdPart\Windows\NamedPipes\FWIOCompletionPipes.pas',
  {$ENDIF }
  {$ENDIF }
  GS.GRID.Client in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.pas',
  GS.GRID.Client.Transport in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.Transport.pas',
  GS.GRID.Client.Example in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.Example.pas',
  GS.GRID.Client.Resolver in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.Resolver.pas',
  GS.GRID.Common.Protocols.KissB in '..\..\..\..\..\Sources\Pascal\Common\GS.GRID.Common.Protocols.KissB.pas',
  GS.GRID.Client.KissB in '..\..\..\..\..\Sources\Pascal\Client\GS.GRID.Client.KissB.pas';

function IsChatMode : Boolean;
  begin
    result := ((ParamCount = 3) or (ParamCount = 5)) and (lowercase(trim(paramstr(1)))='chat')
  end;

  function IsAdminMode : Boolean;
  begin
    result := ((ParamCount = 4) or (ParamCount = 6)) and (lowercase(trim(paramstr(1)))='admin')
  end;


  procedure DisplayHelp;
  begin
    Writeln('');
    Writeln('grid help : ');
    Writeln('chat -------------------------------------------------------------');
    Writeln('      chat is a *very* basic chat, in order to show how to ');
    Writeln('      implement a protocol on GRID. Mainly a dev''dcc !');
    Writeln('chat (autoconnect) : grid chat user pass');
    Writeln('chat (named)       : grid chat user pass host port');
    Writeln('admin ------------------------------------------------------------');
    Writeln('      admin allow adminsitrston wide operation on a GRID server');
    Writeln('admin (autoconnect) : grid admin user pass protocol');
    Writeln('admin (named)       : grid admin user pass host port protocol');
    Writeln('      protocol      : could be "json" or "binary"');
  end;

  procedure ManageChatMode;
  var lCli : TGRIDClientExampleChat;
      lr : TGridClientGRIDResolver;
      lc : String;
      lChat : TGRIDProtocol_ExampleProto_BasicChat_RECV_CHAT;
  begin
    lCli := TGRIDClientExampleChat.Create(TGRIDTransportIndyTCP.Create);
    try
    if ParamCount = 3 then
    begin
      lr := TGridClientGRIDResolver.Create;
      try
        lr.Resolve;
        if lr.GRIDServerCount>0 then
        begin
          TGRIDTransportIndyTCP(lCli.Transport).Host := lr.GRIDServers[0].GridServerIP;
          TGRIDTransportIndyTCP(lCli.Transport).Port := StrToIntDef(lr.GRIDServers[0].GridServerPort,60000);
          Writeln('Server GRID enabled found : '+IntToStr(lr.GRIDServerCount));
          Writeln('--> connecting on  GRID '+TGRIDTransportIndyTCP(lCli.Transport).Host+':'+IntToStr(TGRIDTransportIndyTCP(lCli.Transport).Port)+'...');
        end
        else
        begin
          Writeln('');
          Writeln('Error : AutoResolve does not find any GRID enabled server. Abort.');
          Exit;
        end;
      finally
        FreeAndNil(lr);
      end;
    end
    else
    begin
      TGRIDTransportIndyTCP(lCli.Transport).Host := paramStr(4);
      TGRIDTransportIndyTCP(lCli.Transport).Port := StrToIntDef(paramstr(5),60000);
    end;

    lCli.Connect(ParamStr(2),ParamStr(3),'Hi !');
    if lcli.LastConnectData.Header.RecvCode = TRecvCode.connect_ok then
    begin
      Writeln('Connected to GRID Chat protocol example !');
      Writeln('Current roomList : ' + lCli.LastConnectData.RoomList);
      Writeln('Chat now ! ');
      Writeln(' - type "<r>" for receive chat content.');
      Writeln(' - type "<q>" for quit.');
      lc := '';
      while lowercase(lc)<>'<q>' do
      begin
        readln(lc);
        if lc = '<r>' then
        begin
          lChat := lcli.RecvChat;
          writeln('----------------- CURRENT SERVER''S CHAT CONTENT FROM ROOM "'+lChat.FromRoom+'"');
          Writeln(lChat.AllRoomUserAndChat);
          writeln('-------------------------------------------------------------------');
        end
        else
        begin
          //Sending chat data...
          lcli.SendChat('TEST',UTF8String(lc)); //Yes, room is hardcoded, because it is just a demo. :)
        end;
      end;
    end
    else
    begin
      Writeln('Chat connect failed : '+lcli.LastConnectData.Header.RecvAdditionalInfo);
    end;
    finally
      FreeAndNil(lcli);
    end;
  end;


  procedure DisplayMenu;
  begin
    Writeln(' - type "q" for quit');
    Writeln(' -      "h" for this help');
    Writeln(' -      "ci" for basic client info');
    Writeln(' -      "1" for get Server Info');
    Writeln(' -      "2" for get Server CPULevel');
    Writeln(' -      "3" for InstantPython version');
    Writeln(' -      "4" for InstantPythonRun(''"hello world".upper()''');
    Writeln(' -      "5" for InstantPythonRun(''<LoadFromfile(code.py)>''');
    Writeln(' -      "6" for KeyValueSet("testchannel","Hello !")');
    Writeln(' -      "7" for SendMessage("test","H1","Hello !")');
    Writeln(' -        "7p" for SendMessage with your enter for topic and payload');
    Writeln(' -      "8" for Subs("test")');
    Writeln(' -      "9" for UnSub("test")');
    Writeln(' -      "10" for ReceiveMessage("test")');
  end;

  procedure DisplayClientInfo(c : TGRIDClientKissB);
  begin
    Writeln('Client Info : ');
    Writeln('Current transport layer :  '+c.Transport.AsString);
    Writeln('Current Protocol : '+c.Protocol.ProtocolNameSlashFormat);
  end;

  procedure ManageAdminMode;
  var lCli : TGRIDClientKissB;
      lr : TGridClientGRIDResolver;
      lc : String;
      ls : TStringList;
      Mes : TGRIDMessages;
      lConnectResp : TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE;
      lInfoResp : TGRIDProtocol_KB_SRV_PROCESS_API_INFO;
      lprotoformat : TGRIDProtocolFormat;
      lchan,lpl,lpcs : string;
      lpc : Integer;
      i : Integer;
  begin
    lCli := TGRIDClientKissB.Create(TGRIDTransportIndyTCP.Create);
    lprotoformat := TGRIDProtocolFormat.Binary;
    try
      if ParamCount = 4 then
      begin
        lr := TGridClientGRIDResolver.Create;
        try
          lr.Resolve;
          if lr.GRIDServerCount>0 then
          begin
            TGRIDTransportIndyTCP(lCli.Transport).Host := lr.GRIDServers[0].GridServerIP;
            TGRIDTransportIndyTCP(lCli.Transport).Port := StrToIntDef(lr.GRIDServers[0].GridServerPort,60000);
            Writeln('Server GRID enabled found : '+IntToStr(lr.GRIDServerCount));
            Writeln('--> connecting on  GRID '+TGRIDTransportIndyTCP(lCli.Transport).Host+':'+IntToStr(TGRIDTransportIndyTCP(lCli.Transport).Port)+'...');
          end
          else
          begin
            Writeln('');
            Writeln('Error : AutoResolve does not find any GRID enabled server. Abort.');
            Exit;
          end;
        finally
          FreeAndNil(lr);
        end;

        if ParamStr(4) = 'json' then
          lprotoformat := TGRIDProtocolFormat.json;
      end
      else
      begin
        TGRIDTransportIndyTCP(lCli.Transport).Host := paramStr(4);
        TGRIDTransportIndyTCP(lCli.Transport).Port := StrToIntDef(paramstr(5),60000);
        if ParamStr(6) = 'json' then
          lprotoformat := TGRIDProtocolFormat.json;
      end;

      lconnectResp := lCli.Connect(ParamStr(2),ParamStr(3),lprotoformat);

      if lConnectResp.Status then
      begin
        Writeln('Connected to GRID admin protocol - Welcome');
        Writeln('Protocol '+lcli.Protocol.ProtocolNameSlashFormat);
        DisplayMenu;
        lc := '';
        while lowercase(lc)<>'q' do
        begin
          readln(lc);
          lc := trim(lowercase(lc));
          if lc = 'h' then
            DisplayMenu
          else
          if lc = 'ci' then
            DisplayClientInfo(lcli)
          else
          if lc = '1' then
          begin
            lInfoResp := lCli.Infos;
            if lCli.LastStatus then
            begin
              Writeln('GRIDServerName             : '+lInfoResp.GRIDServerName+' '+lInfoResp.GRIDArch+' ('+lInfoResp.GRIDCompiler+')');
              Writeln('ServerGenuineName         -> '+lInfoResp.ServerGenuineName);
              Writeln('ServerHostCPUArchitecture -> '+lInfoResp.ServerHostCPUArchitecture);
              Writeln('ServerHostArchitecture    ->'+lInfoResp.ServerHostArchitecture);
              Writeln('ServerHostOS              ->'+lInfoResp.ServerHostOS);
              Writeln('ServerHostOSBuild         ->'+lInfoResp.ServerHostOSBuild);
            end
            else
            begin
              writeln('Info not correct : '+lCli.LastStatusInfo);
            end;
          end
          else
          if lc = '2' then
          begin
            Writeln('Current CPULevel : '+FloatToStr(lCli.InfosCPULevel));
            if not(lCli.LastStatus) then
              Writeln(lcli.LastStatusInfo);
          end
          else
          if lc = '3' then
          begin
            Writeln('InstantPythonVersion : '+lCli.instantPythonVersion);
            if not(lCli.LastStatus) then
              Writeln(lcli.LastStatusInfo);
          end
          else
          if lc = '4' then
          begin
            Writeln('InstantPythonRun : '+lCli.instantPythonRun('print("hello world".upper())'));
            if not(lCli.LastStatus) then
              Writeln(lcli.LastStatusInfo);
          end
          else
          if lc = '5' then
          begin
            if FileExists('code.py') then
            begin
              ls := TStringList.Create;
              try
                ls.loadFromFile('code.py');
                writeln('InstantPythonRun : '+lCli.instantPythonRun(ls.Text));
              finally
                FreeAndNil(ls);
              end;
              if not(lCli.LastStatus) then
                Writeln(lcli.LastStatusInfo);
            end
            else
            begin
              writeln('no file "code.pay" found. Abort.');
            end;
          end
          else
          if lc = '6' then
          begin
            Writeln('KeyValueSet : Send "Hello"...');
            lCli.KeyValueSet('testRepo','H1','Hello !');
            Writeln('KeyValueSet : ...Done.');
            if not(lCli.LastStatus) then
              Writeln(lcli.LastStatusInfo);
            if lCli.KeyValueGet('testRepo','H1',lc) then
              Writeln('KeyValueGet : '+lc);
          end
          else
          if lc = '7' then
          begin
            Writeln('SendMessage...');
            lCli.SendMessage('test','Hello !');
            Writeln('...Done.');
            if not(lCli.LastStatus) then
              Writeln(lcli.LastStatusInfo);
          end
          else
          if lc = '7p' then
          begin
            Writeln('SendMessage param...');
            Writeln('-> enter channame');
            readln(lchan);
            Writeln('-> enter payload string');
            readln(lpl);
            Writeln('-> enter message count (empty : 1)');
            readln(lpcs);
            lpc := StrToIntDef(lpcs,1);
            for i := 0 to lpc-1 do
              lCli.SendMessage(lchan,lpl);
            if lCli.LastStatus then
              Writeln('...Done ('+intToStr(lpc)+' sent).')
            else
              Writeln(lcli.LastStatusInfo);
          end
          else
          if lc = '8' then
          begin
            Writeln('Subscribte... (enter chan name, or empty for "test"');
            readln(lchan);
            if trim(lchan) = '' then lchan := 'test';
            if lCli.Subscribe(lchan) then
              Writeln('...Done.')
            else
              Writeln(lcli.LastStatusInfo);
          end
          else
          if lc = '9' then
          begin
            Writeln('Unsubscribte... (enter chan name, or empty for "test"');
            readln(lchan);
            if trim(lchan) = '' then lchan := 'test';
            if lCli.Unsubscribe(lchan) then
              Writeln('...Done.')
            else
              Writeln(lcli.LastStatusInfo);
          end
          else
          if lc = '10' then
          begin
            Writeln('ReceiveMessage...');
            if lcli.CheckMsg(Mes) then
            begin
              Writeln('...Done : ('+IntToStr(Length(Mes))+' message(s))');
              for I := 0 to Length(Mes)-1 do
              begin
                Writeln('  Message '+IntTostr(i+1)+' : ');
                Writeln('    From '+Mes[i].From+' message payload length '+IntToStr(Length(Mes[i].PayLoad)));
                Writeln('    Payload : '+Mes[i].PayloadAsString);
                Writeln('    Tentatively payload as string : '+AnsiString(StringOf(Mes[i].PayLoad)));
              end;
            end
            else
            begin
              if not(lcli.LastStatus) then
                Writeln('Error server side : '+lcli.LastStatusInfo)
              else
                Writeln('...Done : No Message');
            end;
          end;
        end;
      end
      else
      begin
        Writeln('admin connect failed : '+lConnectResp.StatusInfo);
      end;
    finally
      FreeAndNil(lcli);
    end;
  end;


{ TReadThread }


begin
  try
    if IsAdminMode then
    begin
      ManageAdminMode;
    end
    else
    if IsChatMode then
    begin
      ManageChatMode;
    end
    else
    begin
      DisplayHelp;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
