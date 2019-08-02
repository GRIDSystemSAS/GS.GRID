unit GS.GRID.Server.Service.Server.Protocol.KissB;
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
 {$IFDEF USE_GENERIC}
 Generics.Collections,
 {$ENDIF}
 SyncObjs,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 {$IFDEF USE_GENERIC}
 System.Generics.Collections,
 {$ENDIF}
 System.SyncObjs,
 {$ENDIF}
 GS.Common,
 GS.Stream,
 GS.Threads,
 GS.JSON,
 GS.Bus,
 GS.Bus.Services,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server,
 GS.GRID.Common.Protocols,
 GS.GRID.Common.Protocols.KissB,
 GS.GRID.Server.Service.Server.BasedProtocols;

Type
TGRIDProtocolKissB_ServerHandling = Class
private
  class function resolveKissBProtocol(aUser : TGridServerUser) : TGRIDProtocol_KissB;
  class procedure preProcess(aUser : TGridServerUser; aStream : TMemoryStream);
  class procedure postProcess(aUser : TGridServerUser; aStream : TMemoryStream);
protected

  //First connect phase.
  class procedure handling_Connect( const Server: TGRIDServiceServerBasedProtocol;
                                    aUser : TGridServerUser;
                                    aProtocol: TGRIDProtocol_KissB;
                                    aDataStream, aResultStream : TMemoryStream);
  //Second message for credential (optionaly compressed/cyphered)
  class procedure handling_Connectup( const Server: TGRIDServiceServerBasedProtocol;
                                    aUser : TGridServerUser;
                                    aProtocol: TGRIDProtocol_KissB;
                                    aDataStream, aResultStream : TMemoryStream);
  //ONCE CONNECTED PROPERLY
  //Process "simple" (Synchone) and KeyValue...
  class procedure handling_process_rpc_simple( const Server: TGRIDServiceServerBasedProtocol;
                                    aUser : TGridServerUser;
                                    aProtocol: TGRIDProtocol_KissB;
                                    aDataStream, aResultStream : TMemoryStream);
  //Process "asynch" and heavier way...
  class procedure handling_Process( const Server: TGRIDServiceServerBasedProtocol;
                                    aUser : TGridServerUser;
                                    aProtocol: TGRIDProtocol_KissB;
                                    aDataStream, aResultStream : TMemoryStream);
public
  class procedure handling_Proto( const Server : TGRIDServiceServerBasedProtocol;
                                        aUser : TGridServerUser;
                                        aProtocol: TGRIDProtocol_KissB;
                                        aDataStream,
                                        aResultStream : TMemoryStream);
End;


implementation


uses GS.GRID.Server.Service.CentralCnC,
     GS.GRID.SplAPI.GetInfos,
     GS.GRID.Server.Service.Server.Protocol.KissB.Bus,
     GS.GRID.SplAPI.InstantPython;


{ TGRIDProtocolKissB_ServerHandling }

class procedure TGRIDProtocolKissB_ServerHandling.handling_Connect(
  const Server: TGRIDServiceServerBasedProtocol; aUser: TGridServerUser;
  aProtocol: TGRIDProtocol_KissB; aDataStream, aResultStream: TMemoryStream);
var
  lCon : TGRIDProtocol_KB_CLT_NEGOCIATE;
  lResp : TGRIDProtocol_KB_SRV_NEGOCIATE_HALF_RESPONSE;
  lStream : TMemoryStream;
  protosc : TGRIDProtocol_KissB; //Shortcut.
begin
  protosc := resolveKissBProtocol(aUser);
  lCon.Load(aDataStream,aProtocol.ProtocolNativeFormat);
  //this first connection mainly give client info and "asking" data from client (Format, cyher, compress and son on).
  // it is processed in Protocol.IsFormatProtocolResolved.
  lResp.Header := TKBCltCommand_FromServer.connect_resp;
  lResp.Status := true;
  lResp.StatusInfo := '';
  lResp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
end;

class procedure TGRIDProtocolKissB_ServerHandling.handling_Connectup(
  const Server: TGRIDServiceServerBasedProtocol; aUser : TGridServerUser; aProtocol: TGRIDProtocol_KissB;
  aDataStream, aResultStream: TMemoryStream);
var
  lCon : TGRIDProtocol_KB_CLT_NEGOCIATE_UP;
  lResp : TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE;
  lCnc : TCentralCNC_Message_AuthAsk;
begin
  lCon.Load(aDataStream,aProtocol.ProtocolNativeFormat);

  Server.Log('User "'+aUser.ClientId+'" : Ask for agreement CNC...',ClassName);
  lCnc := Server.CNCAuth(aUser.ClientId,lCon.UserName,lCon.Password);
  Server.Log('User "'+aUser.ClientId+'" : Got Agreement Message',ClassName);

  aUser.IsAccredited := lCnc.Agreement;
  aUser.GlobalUserName := lCon.UserName;
  aUser.SessionID := lCnc.AgreementSessionId;
  if lCnc.Agreement then
  begin
    //Build response stream to deliver to the client.
    lResp.Status := true;
    lResp.StatusInfo := '';
  end
  else
  begin
    lResp.Status := false;
    lResp.StatusInfo := 'user/pass not granted';
  end;
  lResp.Header := TKBCltCommand_FromServer.connectup_resp;
  lResp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
end;

class procedure TGRIDProtocolKissB_ServerHandling.handling_process_rpc_simple(
  const Server: TGRIDServiceServerBasedProtocol; aUser : TGridServerUser; aProtocol: TGRIDProtocol_KissB;
  aDataStream, aResultStream: TMemoryStream);
var
  lCon : TGRIDProtocol_KB_CLT_PROCESS_SPL_API;
  lRespA : TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE;
  lStream : TMemoryStream;
  ltempString : string;
  lMessageToCNC, lMessageCNCAnswer : TBusEnvelop;

  procedure GetInfoProcess;
  var lResp : TGRIDProtocol_KB_SRV_PROCESS_API_INFO;
      lgi : TGRIDSimpleAPIGetInfos;
  begin
    lgi := TGRIDSimpleAPIGetInfos.Create(Server);
    try
      lresp := lgi.GetInfos;
    finally
      FreeAndNil(lgi);
    end;

    aResultStream.Clear;
    lResp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
    aResultStream.Position := 0;
    lRespA.Header := TKBCltCommand_FromServer.process_rpc_simple_srvinfo;
    lRespA.ResultPayload := StreamToBytes(aResultStream);
  end;

  procedure KeyValueProcess;
  var lReq : TGRIDProtocol_KB_CLT_PROCESS_SPL_API_KEYVALUE;
      l : TBusClientDataRepo;
      ls : TMemoryStream;
      KeyWithAppSpace : String;
  begin
    ls := TmemoryStream.Create;
    try
      BytesToStream(ls,lcon.SubCall);
      ls.Position := 0;
      lReq.Load(ls,aProtocol.ProtocolNativeFormat);
    finally
      FreeAndNil(ls);
    end;

    //AppSpace is an isolation system :
    //-> It allow to use same channename in 2 differents appspace.
    //-> It isolate ALL bus features, since in modify channel entry name. (Thus, bus, DataRepo, and so on)
    //-> Security : The CST_APPSPACE_SEPARATOR prevent us to try to join an AppSpace by
    //   manipulating the channename from client side.
    KeyWithAppSpace := lReq.Key + aUser.AppSpace;
    lRespA.Header := TKBCltCommand_FromServer.process_rpc_simple_KV;

    if lReq.GetSetCode = 'SET' then
    begin
      if (Length(lReq.Value)>0) And (length(trim(lReq.Repo))>0) And (length(trim(KeyWithAppSpace))>0) then
      begin
        l := TBusClientDataRepo.Create(Server.MasterThread.Bus,lreq.Repo);
        try
          aResultStream.Clear;
          BytesToStream(aResultStream,lReq.Value);
          aResultStream.Position := 0;
          l.SetValue(KeyWithAppSpace,aResultStream);
        finally
          FreeAndNil(l);
        end;

        //Here propagation. TODO
        if Length(lReq.OptionalPropagateChannels)>0 then
        begin
          {TODO -o:  -cGeneral : for each OptionalPropagationschannel, adapt key space !}
          //KeyWithAppSpace := ''
          raise Exception.Create(ClassName + ' : OptionalPropagateChannels feature not implmented.');
        end;

      end
      else
      begin
        lRespA.Status := false;
        lRespA.StatusInfo :='try to set empty data / empty Repo name / empty key';
      end;
    end
    else //GET.
    begin
      if (length(trim(lReq.Repo))>0) And (length(trim(lReq.Key))>0) then
      begin
        l := TBusClientDataRepo.Create(Server.MasterThread.Bus,lreq.Repo);
        try
          aResultStream.Clear;
          l.GetValue(KeyWithAppSpace,aResultStream);
          aResultStream.Position := 0;
          lRespA.ResultPayload := StreamToBytes(aResultStream);
        finally
          FreeAndNil(l);
        end;

        //Here propagation for GET ?
        if Length(lReq.OptionalPropagateChannels)>0 then
        begin

        end;
      end
      else
      begin
        lRespA.Status := false;
        lRespA.StatusInfo :='try to get data with empty Repo name or empty key';
      end;
    end;
  end;

  procedure GetInfoProcessCPULevel;
  var lcpu : Double;
      lgi : TGRIDSimpleAPIGetInfos;
  begin
    lgi := TGRIDSimpleAPIGetInfos.Create(Server);
    try
      lcpu := lgi.CPUUsagePercent;
    finally
      FreeAndNil(lgi);
    end;

    aResultStream.Clear;
    WriteDouble(aResultStream,lcpu);
    aResultStream.Position := 0;
    lRespA.Header := TKBCltCommand_FromServer.process_rpc_simple_srvinfocpulevel;
    lRespA.ResultPayload := StreamToBytes(aResultStream);
  end;


  procedure InstantPythonProcess;
  var
      lpy : TGRIDSimpleAPIInstantPython;
      l : TMemoryStream;
      lcode : UTF8String;
  begin
    l := TmemoryStream.Create;
    try
      BytesToStream(l,lcon.SubCall);
      l.Position := 0;
      lcode := ReadRAWStringUTF8(l);
    finally
      FreeAndNil(l);
    end;
    lRespA.Header := TKBCltCommand_FromServer.process_rpc_simple_InstantPythonRun;
    lpy :=TGRIDSimpleAPIInstantPython.Create(Server,aUser,aProtocol,aDataStream,aResultStream);
    try
      try
        ltempstring := lpy.InstantPythonRun(lcode);
        aResultStream.Clear;
        WriteRAWStringUTF8(aResultStream,ltempString);
        aResultStream.Position :=0;
        lRespA.Status := true;
        lRespA.StatusInfo := '';
        lRespA.ResultPayload := StreamToBytes(aResultStream);
      Except
        On E : Exception do
        begin
          lRespA.Status := false;
          lRespA.StatusInfo := e.Message;
          lRespA.ResultPayload := nil; //[];
          Server.Log(ClassName+' '+E.Message,ClassName);
        end;
      end;
    finally
      FreeAndNil(lpy);
    end;
  end;

  procedure instantPythonversion;
  var lpy : TGRIDSimpleAPIInstantPython;
  begin
    lRespA.Header := TKBCltCommand_FromServer.process_rpc_simple_InstantPythonVersion;
    lpy := TGRIDSimpleAPIInstantPython.Create(Server,aUser,aProtocol,aDataStream,aResultStream);
    try
      ltempString := lpy.Version;
      aResultStream.Clear;
      WriteRAWStringUTF8(aResultStream,ltempString);
      aResultStream.Position :=0;
      lRespA.ResultPayload := StreamToBytes(aResultStream);
    finally
      FreeAndNil(lpy);
    end;
  end;

begin
  lRespA.Status := true;
  lRespA.StatusInfo := '';
  lRespA.ResultPayload := nil; //[];
  lCon.Load(aDataStream,aProtocol.ProtocolNativeFormat);
  Assert(lCon.Header.Command = TKBCltCommand.process_rpc_simple);
  if lCon.CommandID = CST_COMMANDID_GRID_API_SrvInfo then
  begin
    getInfoProcess;
  end
  else
  if lCon.CommandID = CST_COMMANDID_GRID_API_SrvInfoCpuLevel then
  begin
    GetInfoProcessCPULevel;
  end
  else
  if lCon.CommandID = CST_COMMANDID_GRID_API_InstantPythonRun then
  begin
    InstantPythonProcess;
  end
  else
  if lCon.CommandID = CST_COMMANDID_GRID_API_InstantPythonVersion then
  begin
    InstantPythonVersion;
  end
  else
  if lCon.CommandID = CST_COMMANDID_GRID_API_KeyValue then
  begin
    KeyValueProcess;
  end
  else
  begin
    lRespA.Status := false;
    lRespA.StatusInfo := lCon.CommandID+' unknown API';
  end;

  aResultStream.Clear;
  lRespA.Save(aResultStream,aProtocol.ProtocolNativeFormat);
end;

class procedure TGRIDProtocolKissB_ServerHandling.handling_Process(
  const Server: TGRIDServiceServerBasedProtocol; aUser : TGridServerUser; aProtocol: TGRIDProtocol_KissB;
  aDataStream, aResultStream: TMemoryStream);
var
  lCon : TGRIDProtocol_KB_CLT_PROCESS_SPL_API;
  lResp : TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE;
  lStream : TMemoryStream;
begin
  lCon.Load(aDataStream,aProtocol.ProtocolNativeFormat);
  aResultStream.Clear;
  lResp.Save(aResultStream,aProtocol.ProtocolNativeFormat);
end;


class procedure TGRIDProtocolKissB_ServerHandling.handling_Proto(
  const Server: TGRIDServiceServerBasedProtocol;
  aUser : TGridServerUser; aProtocol: TGRIDProtocol_KissB;
  aDataStream, aResultStream : TMemoryStream);

  var lm : TGSJson;
      lCommand : TKBCltCommand;
      lcon : TGRIDProtocol_KB_CLT_NEGOCIATE;
      JSON : String;
begin
  if aProtocol.IsProtocolFormatResolved(aDataStream) then
  begin
    aDataStream.Position := 0;

    if aDataStream.Size = 0 then //No data -> check is there are message for the client and send it directy.
    begin
      internalHandling_Bus(Server,aUser, aProtocol,aDataStream,aResultStream);
      if aResultStream.Size>0 then
        postProcess(aUser,aResultStream);
      Exit;
    end;

    //read first command. (byte)
    lCommand := TKBCltCommand.none;
    case aProtocol.ProtocolNativeFormat of
      TGRIDProtocolFormat.Binary :
      begin
       lcommand := TKBCltCommand(ReadByte(aDataStream));
      end;
      TGRIDProtocolFormat.json :
      begin
        lm := TGSJson.Create;
        try
          JSON := ReadRAWStringUTF8(aDataStream,true);
//          With TStringList.Create do begin Text := JSON; SaveToFile('C:\serverjsonread.txt'); end;
          lm.Parse(JSON);
          lCommand := TKBCltCommand(lm['Header.Command'].AsInteger);
        finally
          FreeAndNil(lm);
        end;
      end;
      else
      begin
        Server.Log('Format not available.',ClassName)
      end;
      aDataStream.Position := 0; //reset for full reload.
    end;

    aDataStream.Position := 0;

    case lCommand of
      TKBCltCommand.none: raise Exception.Create('Format error');
      TKBCltCommand.connect:
      begin
        handling_Connect(Server,aUser,aProtocol,aDataStream,aResultStream);
      end;
      TKBCltCommand.connectup:
      begin
        preProcess(aUser,aDataStream);
        handling_Connectup(Server,aUser,aProtocol,aDataStream,aResultStream);
        postProcess(aUser,aResultStream);
      end;
      TKBCltCommand.process_rpc_simple:
      begin
        preProcess(aUser,aDataStream);
        handling_process_rpc_simple(Server,aUser, aProtocol,aDataStream,aResultStream);
        postProcess(aUser,aResultStream);
      end;
      TKBCltCommand.process:
      begin
        preProcess(aUser,aDataStream);
        handling_Process(Server,aUser,aProtocol,aDataStream,aResultStream);
        postProcess(aUser,aResultStream);
      end;
      TKBCltCommand.bus:
      begin
        preProcess(aUser,aDataStream);
        internalHandling_Bus(Server,aUser, aProtocol,aDataStream,aResultStream);
        postProcess(aUser,aResultStream);
      end;
    end;
  end;
end;

class procedure TGRIDProtocolKissB_ServerHandling.preProcess(
  aUser: TGridServerUser; aStream: TMemoryStream);
var protosc : TGRIDProtocol_KissB; //Shortcut.
begin
  protosc := resolveKissBProtocol(aUser);

  if protosc.IsCompressed then
  begin
    //Do effective decomp.
  end;

  if protosc.IsCiphered then
  begin
    //Do effective decyph.
  end;
end;

class procedure TGRIDProtocolKissB_ServerHandling.postProcess(
  aUser: TGridServerUser; aStream: TMemoryStream);
var protosc : TGRIDProtocol_KissB; //Shortcut.
begin
  protosc := resolveKissBProtocol(aUser);

  if protosc.IsCiphered then
  begin
    //Do effective cyphering.
  end;

  if protosc.IsCompressed then
  begin
    //Do effective compression.
  end;
end;


class function TGRIDProtocolKissB_ServerHandling.resolveKissBProtocol(
  aUser: TGridServerUser): TGRIDProtocol_KissB;
begin
  Assert(Assigned(aUser));
  Assert(Assigned(aUser.Protocol));
  Assert(aUser.Protocol is TGRIDProtocol_KissB);
  result := TGRIDProtocol_KissB(aUser.Protocol);
end;

end.
