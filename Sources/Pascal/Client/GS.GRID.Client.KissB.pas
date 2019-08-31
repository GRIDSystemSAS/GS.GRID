unit GS.GRID.Client.KissB;

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
     GS.Stream,
     GS.Json,
     GS.GRID.Common.Protocols,
     GS.GRID.Common.Protocols.KissB,
     GS.GRID.Client.Transport,
     GS.GRID.Client;

Type
TGRIDClientKissB = class(TGRIDClient)
private
  FUserId: String;

  FProto : TGRIDProtocol_KissB; //Pointer only.
  FStatusInfo: String;
  FStatus: Boolean;
  FUtilityStream, FUtilityStream_KV : TMemoryStream;

  //Connect
  FConnectStep_NegoceResponse : TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE;
  //Spl process
  FSplProcessStep_Response : TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE;
  FSplProcessStep_APIInfo_Response : TGRIDProtocol_KB_SRV_PROCESS_API_INFO;
  FSplProcessStep_CPUInfo : Double;
  FSplProcessStep_PythonRun : String;
  FSplProcessStep_PythonVersion : String;

  //Bus
  FBusProcessStep_Response : TGRIDProtocol_KB_SRV_BUS_CMD_RESPONSE;
  FInternalMessages : TGridMessages;

  FSendCount: Uint32;
  FSendACkCount: Uint32;

  procedure InternalGetCommandAndParse( const UntilReachCommand : Boolean = false;
                                        const CommandToReach : TKBCltCommand_FromServer = TKBCltCommand_FromServer.none;
                                        const Timeout : UInt32 = 250);



  Procedure CheckConnected;
  procedure process(const aCommandID : String);
  procedure ResetlastStatus;

  function InternalSubUnsub(const aChannel : string; const Subscribe : Boolean = true) : Boolean;
  function InternalSendMessage(const aChannel : String; aStream : TStream) : Boolean;
  function InternalKeySet(const aRepository : String; const aKey : String; const aValue : TStream) : boolean;
  function InternalKeyGet(const aRepository : string; const aKey : string; var aValue : TStream) : boolean;

public
  Constructor Create(aTransport : TGRIDTransport); Reintroduce;
  Destructor Destroy; Override;

  function Connect(const aUserName, aPassword : String; Const Format : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary) : TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE;
  function Infos : TGRIDProtocol_KB_SRV_PROCESS_API_INFO;
  function InfosCPULevel : double;

  //Instant python
  function instantPythonVersion : string;
  function instantPythonRun(aCode : string) : string;

  //Key/Value
  function KeyValueSet(const aRepository : String; const aKey : String; const aValue : String) : boolean; Overload;
  function KeyValueSet(const aRepository : String; const aKey : String; const aValue : TStream) : boolean; Overload;
  function KeyValueGet(const aRepository : string; const aKey : string; var aValue : string) : boolean; Overload;
  function KeyValueGet(const aRepository : string; const aKey : string; var aValue : TStream) : boolean; Overload;


  //Bus related.
  function SendMessage(const Channel : string; aMessage : TStream) : boolean; Overload;
  function SendMessage(const Channel : string; aMessage : String) : boolean; Overload;

  function Subscribe(const Channel : string) : boolean;
  function Unsubscribe(const Channel : String) : boolean;

  //To be called in a (thread loop...)
  //true only if msg count>0
  function CheckMsg(var MessageBox : TGRIDMessages; const TimeOutMS : Uint32 = 0) : Boolean;
  function CheckMsgWait(var MessageBox : TGRIDMessages) : Boolean;

  Property LastStatus : Boolean read FStatus;
  property LastStatusInfo : String read FStatusInfo;

  property SendCount : Uint32 read FSendCount;
  property SendAckCount : Uint32 read FSendACkCount;

end;


implementation


{ TGRIDClientExample }

procedure TGRIDClientKissB.CheckConnected;
begin
  if not(Connected) then
    raise Exception.Create(CST_NOT_CONNECTED);
end;

function TGRIDClientKissB.Connect(const aUserName, aPassword : String;
                                  Const Format : TGRIDProtocolFormat): TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE;
var lAskConnect : TGRIDProtocol_KB_CLT_NEGOCIATE;
    lAskConnectUP : TGRIDProtocol_KB_CLT_NEGOCIATE_UP;

begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  FUtilityStream.Clear;
  try
    lAskConnect.Header.Command := TKBCltCommand.connect;
    lAskConnect.Signature := CST_SIGNATURE;
    lAskConnect.ProtocolVersionMajor := CST_DEFAULT_MAJOR_PROTO_VERSION;
    lAskConnect.ProtocolVersionMinor := CST_DEFAULT_MINOR_PROTO_VERSION;

    lAskConnect.AskForFormat := Format;
    lAskConnect.AskForCompression := TKBCompression.none;
    lAskConnect.AskForCiphering := TKBCiphering.none;
    FProto.SetProtocolNativeFormat(Format);

    lAskConnect.Save(FUtilityStream, lAskConnect.AskForFormat);
    FUtilityStream.Position := 0;

    FTransport.Connect;
    FTransport.Send(FUtilityStream);

    InternalGetCommandAndParse(true,TKBCltCommand_FromServer.connect_resp,2500);

    lAskConnectUP.Header.Command := TKBCltCommand.connectup;
    lAskConnectUP.UserName := aUserName;
    lAskConnectUP.Password := aPassword;

    FUtilityStream.Clear;
    lAskConnectUP.Save(FUtilityStream, lAskConnect.AskForFormat);
    FUtilityStream.Position := 0;
    FTransport.Send(FUtilityStream);

    InternalGetCommandAndParse(true,TKBCltCommand_FromServer.connectup_resp);

    FUtilityStream.Clear;
    FUserId := aUserName;
    Result := FConnectStep_NegoceResponse;
  Except
    On E : Exception do
    begin
      Result.Status := False;
      Result.StatusInfo := 'Connect : '+E.Message;
    end;
  end;
end;


constructor TGRIDClientKissB.Create(aTransport: TGRIDTransport);
begin
  Assert(assigned(aTransport));
  inherited Create;
  FTransport := aTransport;
  FProtocol := TGRIDProtocol_KissB.Create;
  FProto := TGRIDProtocol_KissB(FProtocol); //for avoiding cast.
  FUtilityStream := TMemoryStream.Create;
  FUtilityStream_KV := TMemoryStream.Create;
  FSendCount:=0;
  FSendACkCount:=0;
end;

destructor TGRIDClientKissB.Destroy;
begin
  if Connected then
    Disconnect;
  FreeAndNil(FTransport);
  FreeAndNil(FProtocol);
  FreeAndNil(FUtilityStream);
  FreeAndNil(FUtilityStream_KV);
  inherited;
end;


function TGRIDClientKissB.Infos: TGRIDProtocol_KB_SRV_PROCESS_API_INFO;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  CheckConnected;

  FUtilityStream.Clear;
  try
    process(CST_COMMANDID_GRID_API_SrvInfo);
    InternalGetCommandAndParse(true,TKBCltCommand_FromServer.process_rpc_simple_srvinfo);
    Result := FSplProcessStep_APIInfo_Response;
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
      raise;
    end;
  end;
end;

function TGRIDClientKissB.InfosCPULevel: double;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  result := 0.0;
  CheckConnected;

  FUtilityStream.Clear;
  try
    process(CST_COMMANDID_GRID_API_SrvInfoCpuLevel);
    InternalGetCommandAndParse(true,TKBCltCommand_FromServer.process_rpc_simple_srvinfocpulevel);
    result := FSplProcessStep_CPUInfo;
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;

function TGRIDClientKissB.instantPythonRun(aCode: string): string;
var lAskConnect : TGRIDProtocol_KB_CLT_PROCESS_SPL_API;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  CheckConnected;

  FUtilityStream.Clear;
  try
    WriteRAWStringUTF8(FUtilityStream,aCode);
    FUtilityStream.Position := 0;
    lAskConnect.SubCall := StreamToBytes(FUtilityStream);
    FUtilityStream.Clear;

    lAskConnect.Header.Command := TKBCltCommand.process_rpc_simple;
    lAskConnect.CommandID := CST_COMMANDID_GRID_API_InstantPythonRun;

    lAskConnect.Save(FUtilityStream, FProtocol.ProtocolNativeFormat);
    FUtilityStream.Position := 0;

    FTransport.Send(FUtilityStream);
    InternalGetCommandAndParse(true,TKBCltCommand_FromServer.process_rpc_simple_InstantPythonRun);
    result := FSplProcessStep_PythonRun;

  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;

function TGRIDClientKissB.instantPythonVersion: string;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  CheckConnected;

  FUtilityStream.Clear;
  try
    process(CST_COMMANDID_GRID_API_InstantPythonVersion);
    InternalGetCommandAndParse(true,TKBCltCommand_FromServer.process_rpc_simple_InstantPythonVersion);
    result := FSplProcessStep_PythonVersion
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;

procedure TGRIDClientKissB.InternalGetCommandAndParse(
                                        const UntilReachCommand : Boolean = false;
                                        const CommandToReach : TKBCltCommand_FromServer = TKBCltCommand_FromServer.none;
                                        const Timeout : UInt32 = 250);
var
  response : TKBCltCommand_FromServer;
  FConnectStep_Response : TGRIDProtocol_KB_SRV_NEGOCIATE_HALF_RESPONSE;
  FBus_recv : TGRIDProtocol_KB_SRV_BUS_CMD_MSG_RESPONSE;
  i,lc,lt : Integer;
  cond :  boolean;
  lUltitmateTimeout : Uint32;
  lm : TGSJson;
  lpos, lTemp : Uint32;
begin
  //Gather server's data response.
  FUtilityStream.Clear;
  cond := True;
  response := TKBCltCommand_FromServer.None;
  while cond do
  begin
    lUltitmateTimeout := 0;
    FUtilityStream.Clear;
    if UntilReachCommand then
    begin
      //We wait explicitely (syncrone) for a network command. Wait it.
      while FUtilityStream.Size = 0 do
      begin
        ltemp := FUtilityStream.Position;
        FTransport.Recv(TStream(FUtilityStream),Timeout);
        FUtilityStream.Position := 0;
        if ltemp=FUtilityStream.Position then
        begin
          lUltitmateTimeout := lUltitmateTimeout + Timeout;
        end
        else
        begin
          lUltitmateTimeout := 0;
        end;

        if (lUltitmateTimeout>CST_GRID_CLIENT_ULTIMATE_TIMOUT_MS) then
        begin
          raise Exception.Create('Client time out error : Waiting code ['+IntToStr(Integer(CommandToReach))+']');
        end;
      end;
    end
    else
    begin
      //If we do not wait for a command, we usualy just interrogate the buffer nework line :
      // -> In this case, usualy, TimeOut should be low/such as 1ms or even 0ms. (data is in network buffer)
      // -> It the reason why CheckMsg pass 0 as a time out.
      FTransport.Recv(TStream(FUtilityStream),Timeout);
      FUtilityStream.Position := 0;
      if FUtilityStream.Size=0 then
        Exit;
    end;


    Repeat
      lpos := FUtilityStream.Position;

      case FProtocol.ProtocolNativeFormat of
        TGRIDProtocolFormat.Binary :
          response := TKBCltCommand_FromServer(ReadByte(FUtilityStream));
        TGRIDProtocolFormat.json :
        begin
          lm := TGSJson.Create;
          try
            lm.Parse(ReadRAWStringUTF8(FUtilityStream,true));
            response := TKBCltCommand_FromServer(lm['Header'].AsInteger);
          finally
            FreeAndNil(lm);
          end;
        end;
      end;

      if UntilReachCommand then
        if cond then
          cond := response <> CommandToReach;

      FUtilityStream.Position := lpos;

      case response of
        TKBCltCommand_FromServer.none: raise Exception.Create('Protocol error');
        TKBCltCommand_FromServer.connect_resp:
        begin
          FConnectStep_Response.Load(FUtilityStream,FProtocol.ProtocolNativeFormat);
          if Not FConnectStep_Response.Status then
            raise Exception.Create('Negociation failure '+FConnectStep_Response.StatusInfo);
        end;
        TKBCltCommand_FromServer.connectup_resp:
        begin
          FConnectStep_NegoceResponse.Load(FUtilityStream,FProtocol.ProtocolNativeFormat);
        end;
        TKBCltCommand_FromServer.process_rpc_simple_srvinfo:
        begin
          FSplProcessStep_Response.Load(FUtilityStream,FProto.ProtocolNativeFormat);
          FStatus := FSplProcessStep_Response.Status;
          FStatusInfo := FSplProcessStep_Response.StatusInfo;
          if FStatus then
          begin
            FUtilityStream_KV.Clear;
            BytesToStream(FUtilityStream_KV,FSplProcessStep_Response.ResultPayload);
            FUtilityStream_KV.Position :=0;
            FSplProcessStep_APIInfo_Response.Load(FUtilityStream_KV,FProtocol.ProtocolNativeFormat);
            FUtilityStream_KV.Clear;
          end
          else
          begin
            raise Exception.Create('Server error : '+FSplProcessStep_Response.StatusInfo);
          end;
        end;
        TKBCltCommand_FromServer.process_rpc_simple_srvinfocpulevel:
        begin
          FSplProcessStep_Response.Load(FUtilityStream,FProto.ProtocolNativeFormat);
          FStatus := FSplProcessStep_Response.Status;
          FStatusInfo := FSplProcessStep_Response.StatusInfo;
          if FStatus then
          begin
            FUtilityStream_KV.Clear;
            BytesToStream(FUtilityStream_KV,FSplProcessStep_Response.ResultPayload);
            FUtilityStream_KV.Position :=0;
            FSplProcessStep_CPUInfo := ReadDouble(FUtilityStream_KV);
            FUtilityStream_KV.Clear;
          end
          else
          begin
            raise Exception.Create('Server error : '+FSplProcessStep_Response.StatusInfo);
          end;
        end;
        TKBCltCommand_FromServer.process_rpc_simple_InstantPythonRun:
        begin
          FSplProcessStep_Response.Load(FUtilityStream,FProto.ProtocolNativeFormat);
          FStatus := FSplProcessStep_Response.Status;
          FStatusInfo := FSplProcessStep_Response.StatusInfo;

          FUtilityStream_KV.Clear;
          BytesToStream(FUtilityStream_KV,FSplProcessStep_Response.ResultPayload);
          FUtilityStream_KV.Position := 0;
          if FSplProcessStep_Response.Status then
          begin
            FSplProcessStep_PythonRun := ReadRAWStringUTF8(FUtilityStream_KV);
            FUtilityStream_KV.Clear;
          end
          else
          begin
            raise Exception.Create('Server error / Python run : '+string(FSplProcessStep_Response.StatusInfo));
          end;
        end;
        TKBCltCommand_FromServer.process_rpc_simple_InstantPythonVersion:
        begin
          FSplProcessStep_Response.Load(FUtilityStream,FProto.ProtocolNativeFormat);
          FStatus := FSplProcessStep_Response.Status;
          FStatusInfo := FSplProcessStep_Response.StatusInfo;
          if FStatus then
          begin
            FUtilityStream_KV.Clear;
            BytesToStream(FUtilityStream_KV,FSplProcessStep_Response.ResultPayload);
            FUtilityStream_KV.Position :=0;
            FSplProcessStep_PythonVersion := ReadRAWStringUTF8(FUtilityStream_KV);
            FUtilityStream_KV.Clear;
          end
          else
          begin
            raise Exception.Create('Server error / Python version : '+FSplProcessStep_Response.StatusInfo);
          end;
        end;
        TKBCltCommand_FromServer.process_rpc_simple_KV:
        begin
          FSplProcessStep_Response.Load(FUtilityStream,FProtocol.ProtocolNativeFormat);
          if Not(FSplProcessStep_Response.Status) then
          begin
            raise Exception.Create('Server side : '+String(FSplProcessStep_Response.StatusInfo));
          end;
        end;
        TKBCltCommand_FromServer.bus_sub:
        begin
          FBusProcessStep_Response.Load(FUtilityStream,FProtocol.ProtocolNativeFormat);
          if Not(FBusProcessStep_Response.Status) then
          begin
            raise Exception.Create('Server side / Bus sub  : '+FBusProcessStep_Response.StatusInfo);
          end;
        end;
        TKBCltCommand_FromServer.bus_unsub:
        begin
          FBusProcessStep_Response.Load(FUtilityStream,FProtocol.ProtocolNativeFormat);
          if Not(FBusProcessStep_Response.Status) then
          begin
            raise Exception.Create('Server side / Bus unsub  : '+FBusProcessStep_Response.StatusInfo);
          end;
        end;
        TKBCltCommand_FromServer.bus_send:
        begin
          FBusProcessStep_Response.Load(FUtilityStream,FProtocol.ProtocolNativeFormat);
          if Not(FBusProcessStep_Response.Status) then
          begin
            raise Exception.Create('Server side : '+FBusProcessStep_Response.StatusInfo);
          end
          else
          begin
            Inc(FSendACkCount);
          end;
        end;
        TKBCltCommand_FromServer.bus_recv :
        begin
          if FUtilityStream.Size>0 then
          begin
            FBus_recv.Load(FUtilityStream,FProtocol.ProtocolNativeFormat);
            if FBus_recv.Status then
            begin
              //Store incoming messages until chckMsg is called.
              lc := Length(FInternalMessages);
              lt := Length(FBus_recv.MessagesPayLoad);
              SetLength(FInternalMessages,lc+lt);
              for I := lc to length(FInternalMessages)-1 do
              begin
                FInternalMessages[i].From := FBus_recv.MessagesPayLoad[i-lc].From;
                FInternalMessages[i].Channel := FBus_recv.MessagesPayLoad[i-lc].Channel;
                FInternalMessages[i].PayLoad := FBus_recv.MessagesPayLoad[i-lc].MessagePayload;
                FInternalMessages[i].Ticks := FBus_recv.MessagesPayLoad[i-lc].Ticks;
              end;
            end;
          end;
        end;
      end;
    Until FUtilityStream.Position = FUtilityStream.Size;
  end;
end;

function TGRIDClientKissB.InternalKeyGet(const aRepository, aKey: string;
  var aValue: TStream): boolean;
var lAskConnect : TGRIDProtocol_KB_CLT_PROCESS_SPL_API;
    lreq : TGRIDProtocol_KB_CLT_PROCESS_SPL_API_KEYVALUE;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  Assert(trim(aRepository)<>'');
  Assert(trim(aKey)<>'');
  Assert(Assigned(aValue));
  result := false;
  CheckConnected;

  FUtilityStream.Clear;
  try
    lreq.Repo := aRepository;
    lreq.GetSetCode := 'GET';
    lreq.Key := aKey;
    lreq.Value := nil; //[];
    lreq.OptionalPropagateChannels := '';
    lreq.Save(FUtilityStream,FProtocol.ProtocolNativeFormat);
    //Write eKeyValue request into payload (subcall)
    FUtilityStream.Position := 0;
    lAskConnect.SubCall := StreamToBytes(FUtilityStream);
    FUtilityStream.Clear;

    lAskConnect.Header.Command := TKBCltCommand.process_rpc_simple;
    lAskConnect.CommandID := CST_COMMANDID_GRID_API_KeyValue;
    lAskConnect.Save(FUtilityStream,FProtocol.ProtocolNativeFormat);
    FUtilityStream.Position := 0;
    FTransport.Send(FUtilityStream);

    InternalGetCommandAndParse(true,TKBCltCommand_FromServer.process_rpc_simple_KV);
    BytesToStream(aValue,FSplProcessStep_Response.ResultPayload);
    aValue.Position := 0;
    result := FSplProcessStep_Response.Status;
    FUtilityStream.Clear;
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;

function TGRIDClientKissB.InternalKeySet(const aRepository, aKey: String;
  const aValue: TStream): boolean;
var lAskConnect : TGRIDProtocol_KB_CLT_PROCESS_SPL_API;
    lreq : TGRIDProtocol_KB_CLT_PROCESS_SPL_API_KEYVALUE;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  Assert(trim(aRepository)<>'');
  Assert(trim(aKey)<>'');
  Assert(Assigned(aValue));
  Result := false;
  CheckConnected;

  FUtilityStream.Clear;
  try
    //Build KeyValue request.
    lreq.Repo := aRepository;
    lreq.GetSetCode := 'SET';
    lreq.Key := aKey;
    aValue.Position := 0;
    lreq.Value := StreamToBytes(aValue);
    lreq.OptionalPropagateChannels := '';
    lreq.Save(FUtilityStream,FProtocol.ProtocolNativeFormat);
    //Write eKeyValue request into payload (subcall)
    FUtilityStream.Position := 0;
    lAskConnect.SubCall := StreamToBytes(FUtilityStream);
    FUtilityStream.Clear;

    lAskConnect.Header.Command := TKBCltCommand.process_rpc_simple;
    lAskConnect.CommandID := CST_COMMANDID_GRID_API_KeyValue;
    lAskConnect.Save(FUtilityStream,FProtocol.ProtocolNativeFormat);

    //Send to server.
    FUtilityStream.Position := 0;
    FTransport.Send(FUtilityStream);
    InternalGetCommandAndParse(true, TKBCltCommand_FromServer.process_rpc_simple_KV);
    Result := FSplProcessStep_Response.Status;
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;

function TGRIDClientKissB.InternalSendMessage(const aChannel: String;
  aStream: TStream): Boolean;
var lreq : TGRIDProtocol_KB_CLT_BUS_CMD;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  Assert(assigned(aStream));
  Assert(trim(aChannel)<>'');
  Result := true;
  CheckConnected;
  lreq.Header.Command := TKBCltCommand.bus;
  lreq.CommandStructure :=  TKBCltBusCmd.sendmsg;
  lreq.ChannelInvolved := aChannel;
  aStream.Position := 0;
  lreq.Payload := StreamToBytes(aStream);

  FUtilityStream.Clear;
  try
    lreq.Save(FUtilityStream,FProtocol.ProtocolNativeFormat);
    FUtilityStream.Position := 0;
    FTransport.Send(FUtilityStream);
    Inc(FSendCount);
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;

function TGRIDClientKissB.InternalSubUnsub(const aChannel: string;
  const Subscribe: Boolean): Boolean;
var lreq : TGRIDProtocol_KB_CLT_BUS_CMD;
    lsSide : TKBCltCommand_FromServer;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  Assert(trim(aChannel)<>'');
  Result := false;
  CheckConnected;
  lreq.Header.Command := TKBCltCommand.bus;
  if Subscribe then
  begin
    lreq.CommandStructure :=  TKBCltBusCmd.sub;
    lsSide := TKBCltCommand_FromServer.bus_sub;
  end
  else
  begin
    lreq.CommandStructure :=  TKBCltBusCmd.unsub;
    lsSide := TKBCltCommand_FromServer.bus_unsub;
  end;
  lreq.ChannelInvolved := aChannel;
  lreq.Payload := nil;

  FUtilityStream.Clear;
  try
    lreq.Save(FUtilityStream,FProtocol.ProtocolNativeFormat);
    FUtilityStream.Position := 0;
    FTransport.Send(FUtilityStream);
    InternalGetCommandAndParse(true,lsSide);
    result := FBusProcessStep_Response.Status;
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;

function TGRIDClientKissB.KeyValueGet(const aRepository, aKey: string;
  var aValue: string): boolean;
begin
  FUtilityStream_KV.Clear;
  aValue := '';
  result := InternalKeyGet(aRepository,aKey,TStream(FUtilityStream_KV));
  if LastStatus then
  begin
    FUtilityStream_KV.Position := 0;
    aValue := ReadRAWStringUTF8(FUtilityStream_KV);
    FUtilityStream_KV.Clear;
  end;
end;

function TGRIDClientKissB.KeyValueGet(const aRepository, aKey: string;
  var aValue: TStream): boolean;
begin
  result := InternalKeyGet(aRepository,aKey,aValue);
end;

function TGRIDClientKissB.KeyValueSet(const aRepository, aKey: String;
  const aValue: TStream): boolean;
begin
  result := InternalKeySet(aRepository,aKey,aValue);
end;

function TGRIDClientKissB.KeyValueSet( const aRepository : String;
                                       const aKey : String;
                                       const aValue: String): boolean;
begin
  FUtilityStream_KV.Clear;
  WriteRAWStringUTF8(FUtilityStream_KV,aValue);
  result := InternalKeySet(aRepository,aKey,FUtilityStream_KV);
  FUtilityStream_KV.Clear;
end;

procedure TGRIDClientKissB.process(const aCommandID: String);
var lAskConnect : TGRIDProtocol_KB_CLT_PROCESS_SPL_API;
begin
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  CheckConnected;

  FUtilityStream.Clear;
  try
    lAskConnect.Header.Command := TKBCltCommand.process_rpc_simple;
    lAskConnect.CommandID := aCommandID;
    lAskConnect.SubCall := nil;
    lAskConnect.Save(FUtilityStream, FProtocol.ProtocolNativeFormat);
    FUtilityStream.Position := 0;

    FTransport.Send(FUtilityStream);
    FUtilityStream.Clear;
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;


function TGRIDClientKissB.CheckMsg(var MessageBox : TGRIDMessages; const TimeOutMS : Uint32 = 0) : Boolean;
begin
  ResetlastStatus;
  Assert(assigned(Protocol));
  Assert(assigned(Transport));
  CheckConnected;
  result := false;
  try
    InternalGetCommandAndParse(false,TKBCltCommand_FromServer.none,TimeOutMS);
    result := Assigned(FInternalMessages) and (Length(FInternalMessages)>0);
    MessageBox := FInternalMessages;
    FInternalMessages := Nil;
  Except
    On E : Exception do
    begin
      FStatus := False;
      FStatusInfo := E.Message;
    end;
  end;
end;

procedure TGRIDClientKissB.ResetlastStatus;
begin
  FStatusInfo := '';
  FStatus := true;
end;

function TGRIDClientKissB.SendMessage(const Channel: string;
  aMessage: TStream): boolean;
begin
  result := InternalSendMessage(Channel,aMessage);
end;

function TGRIDClientKissB.SendMessage(const Channel: string;
  aMessage: String): boolean;
var l : TMemoryStream;
begin
  l :=  TMemoryStream.Create;
  try
    WriteRAWStringUTF8(l,aMessage);
    result := InternalSendMessage(Channel,l);
  finally
    FreeAndNil(l);
  end;
end;

function TGRIDClientKissB.Subscribe(const Channel: string): boolean;
begin
  result := InternalSubUnsub(Channel);
end;

function TGRIDClientKissB.Unsubscribe(const Channel: String): boolean;
begin
  Result := InternalSubUnsub(Channel,False)
end;

function TGRIDClientKissB.CheckMsgWait(var MessageBox: TGRIDMessages): Boolean;
begin
  repeat
    CheckMsg(MessageBox,50);
  until Length(MessageBox)>0;
end;

end.
