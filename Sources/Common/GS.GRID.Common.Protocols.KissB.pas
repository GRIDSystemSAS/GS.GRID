//KissB (Keep It Simple and Stupid - Bus) protocol is a simple production ready protocol for a direct
//GRID exploitation. It is keep simple and RPC oriented because of its aim : It is build to
//be cross language/techno, and must be easely natively implemented on all language, included script.

///
///
///  FEATURES                                                                   STATUS
///
///  - User/Pass acreditation                                                   100%
///  - GRID Service administration capability                                   0%
///  - GRID overwatch capability (Grid ID, Grid Process and so on)              0%
///  - Bus communication capability (Sub, Emit)                                 100%
///  - RPC Process launcher (system/Python)                                     100%
///
///
///  PROTOCOL DESC
///

/// CLIENT                                                                       SERVER
///
/// Negociate and connect.
///
/// TGRIDProtocol_KB_CLT_NEGOCIATE                   -------------------------->
///                                                  <--------------------------   TGRIDProtocol_KB_SRV_NEGOCIATE_HALF_RESPONSE
/// TGRIDProtocol_KB_CLT_NEGOCIATE_UP                -------------------------->
///                                                  <--------------------------   TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE
///
/// Handle process GRID side. : SPL_API stand for "SIMPLE_API (RPC type, one ask, one response)
/// it is handle by the connection thread, and only for "simple" API with simple response.
///
/// TGRIDProtocol_KB_CLT_PROCESS_SPL_API             -------------------------->
///                                                  <-------------------------- TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE
///
///
/// Handle process GRID side. : this is more complexe one : It will give you a key, witch will give you access to
/// bus channels to manage and get results of the corresponding process.
///
/// TGRIDProtocol_KB_CLT_PROCESS_SPL_API             -------------------------->
///                                                  <-------------------------- TGRIDProtocol_KB_SRV_PROCESS_RESPONSE
///
///
/// Handle bus features messaging
///
/// TGRIDProtocol_KB_CLT_BUS_CMD                  -------------------------->
///                                                  <-------------------------- TGRIDProtocol_KB_SRV_BUS_CMD_RESPONSE

unit GS.GRID.Common.Protocols.KissB;

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
Interface

uses SysUtils,
     Classes,
     GS.Common,
     GS.Stream,
     GS.JSON, jsons,
     GS.GRID.Common.Protocols;

Const
  CST_SIGNATURE = 'KissB_Grid_Protocol_V1';
  CST_FORMAT_NOT_SUPPORTED = 'KB Protocol encode/decode : Format not supported';

  CST_DEFAULT_MAJOR_PROTO_VERSION = 1; //*Must* be same between C/S.
  CST_DEFAULT_MINOR_PROTO_VERSION = 0; //*Must* be at least C<=S. (Indicate only features level on S.)

  //API
  CST_COMMANDID_GRID_API_SrvInfo              = 'GRID.API.SrvInfo';
  CST_COMMANDID_GRID_API_SrvInfoCpuLevel      = 'GRID.API.SrvInfoCpuLevel';
  CST_COMMANDID_GRID_API_InstantPythonRun     = 'GRID.API.InstantPythonRun';
  CST_COMMANDID_GRID_API_InstantPythonVersion = 'GRID.API.InstantPythonVersion';
  CST_COMMANDID_GRID_API_KeyValue             = 'GRID.API.KV';

  //Client side time out.
  CST_GRID_CLIENT_ULTIMATE_TIMOUT_MS = 10000;

Type

  TKBCompression = (none); //Todo.
  TKBCiphering = (none); //Todo.

  TKBCltCommand = (none, connect, connectup, process_rpc_simple, process, bus);
  TKBHeader_FromCLT = Packed record
    Command : TKBCltCommand;
  end;
  TKBCltCommand_FromServer = ( none,
                               connect_resp,
                               connectup_resp,
                               process_rpc_simple_srvinfo,
                               process_rpc_simple_srvinfocpulevel,
                               process_rpc_simple_InstantPythonRun,
                               process_rpc_simple_InstantPythonVersion,
                               process_rpc_simple_KV,
                               bus_sub,
                               bus_unsub,
                               bus_send,
                               bus_recv);


/// Negociate and connect.
///
/// TGRIDProtocol_KB_CLT_NEGOCIATE           -------------------------->
///                                          <-------------------------- TGRIDProtocol_KB_SRV_NEGOCIATE_HALF_RESPONSE
/// TGRIDProtocol_KB_CLT_NEGOCIATE_UP        -------------------------->
///                                          <-------------------------- TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE

  TGRIDProtocol_KB_CLT_NEGOCIATE = Packed Record
    Header : TKBHeader_FromCLT;
    Signature : String;
    ProtocolVersionMajor : Byte; //*Must* be same between client serveur.
    ProtocolVersionMinor : Byte; //*Must* be Client.Version<=Serveur.Version. If different, only more features server side.
    AskForFormat : TGRIDProtocolFormat;
    AskForCompression : TKBCompression;
    AskForCiphering : TKBCiphering;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  End;

  TGRIDProtocol_KB_SRV_NEGOCIATE_HALF_RESPONSE = Packed Record
    Header : TKBCltCommand_FromServer;
    Status : boolean;
    StatusInfo : String; //Error case.

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  End;

  TGRIDProtocol_KB_CLT_NEGOCIATE_UP = Packed Record //UP : UserPass. (could be compressed chyphererd at this level, depending the beginning !
    Header : TKBHeader_FromCLT;
    UserName : String;
    Password : String;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  End;

  TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE = Packed Record
    Header : TKBCltCommand_FromServer;
    Status : boolean;
    StatusInfo : String; //Error case.

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  End;

/// Handle process GRID side.
///
/// TGRIDProtocol_KB_CLT_PROCESS_SPL_API                     -------------------------->
///                                                  <-------------------------- TGRIDProtocol_KB_SRV_PROCESS_RESPONSE
///
  TGRIDProtocol_KB_CLT_PROCESS_SPL_API = Packed Record
    Header : TKBHeader_FromCLT;
    CommandID : String;                  //Command API ID or command ID (Such as GRID.Sys.CPULevel)
    SubCall : TBytes;
    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;

  //Response.
  TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE = Packed Record
    Header : TKBCltCommand_FromServer;
    Status : boolean;
    StatusInfo : String;
    ResultPayload : TBytes;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;

///
///  API
///
///
///

  //(!!!) Just after a "TGRIDProtocol_KB_CLT_PROCESS_SPL_API", this structure hold data for KeyValue API.
  TGRIDProtocol_KB_CLT_PROCESS_SPL_API_KEYVALUE = Packed Record
//    Header : TKBHeader_FromCLT; Just after an existing stream, not needed to aff new header.
    Repo : String;
    GetSetCode : String;
    Key : string;
    Value : TBytes;
    OptionalPropagateChannels : String;
    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;


///
///
/// API RESULT
///

  ///(!) Return structure for CST_COMMANDID_GRID_API_SrvInfo.
  ///(!) Returned in the payload of TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE. No need header.
  TGRIDProtocol_KB_SRV_PROCESS_API_INFO = Packed Record
    ServerGenuineName : String;
    ServerHostCPUArchitecture : String;
    ServerHostArchitecture : String;
    ServerHostOS : String;
    ServerHostOSBuild : String;
    GRIDVersion : String;
    GRIDServerName : String;
    GRIDServices : String; //Delimited list.
    GRIDArch : String;
    GRIDCompiler : String;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Clear;
  end;

///
///
///
///
///


/// Handle bus features messaging
///
/// TGRIDProtocol_KB_CLT_BUS_CMD                  -------------------------->
///                                                  <-------------------------- TGRIDProtocol_KB_SRV_BUS_CMD_RESPONSE
  TKBCltBusCmd = (none, sendmsg,sub, unsub, openchan, joinappspace);
  TGRIDProtocol_KB_CLT_BUS_CMD = Packed Record
    Header : TKBHeader_FromCLT;
    CommandStructure : TKBCltBusCmd;
    ChannelInvolved : String;
    Payload : TBytes;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;


  TGRIDProtocol_KB_SRV_BUS_CMD_RESPONSE = Packed Record
    Header : TKBCltCommand_FromServer;
    Status : boolean;
    StatusInfo : String; //Error case.

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;

    TGRIDProtocol_KB_BUS_MSG = packed record
      From,Channel : String;
      MessagePayload : TBytes;
      Ticks : Uint64;
    end;

  TGRIDProtocol_KB_SRV_BUS_CMD_MSG_RESPONSE = Packed Record
    Header : TKBCltCommand_FromServer;
    Status : boolean;
    StatusInfo : String; //Error case.
    MessagesPayLoad : Array of TGRIDProtocol_KB_BUS_MSG;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;


///
///
///
///  PROTOCOL
///
///

TGRIDProtocol_KissB = class(TGRIDProtocol)
private
protected
  FCurrentFormat : TGRIDProtocolFormat;
  FVersionMinor: Byte;
  FVersionMajor: Byte;
  FProtoFormatResolved : Boolean;
  FCiphered: Boolean;
  FCompressed: Boolean;
  FCipherDataId: String;
  FCompressDataId: String;
public
  Constructor Create; Override;
  //Needed for inheritance.
  Class Function Negociate(aStream : TStream) : Boolean; Override;
  Function IsProtocolFormatResolved(aStream : TStream) : Boolean;

  Function ProtocolName : String; Override;
  Function ProtocolNativeFormat : TGRIDProtocolFormat; Override;
  Function ProtocolDescription : String; Override;

  Procedure SetProtocolNativeFormat(aFormat : TGRIDProtocolFormat);

  function AllowEmptyInputData : Boolean; override;
  function AllowByteCountPrefix : Boolean; override;

  property ProtocolResolved : Boolean read FProtoFormatResolved;

  property IsCiphered : Boolean read FCiphered;
  property IsCompressed : Boolean read FCompressed;
  property CipherDataId : String read FCipherDataId;
  property CompressDataId : String read FCompressDataId;

  property VersionMajor : Byte read FVersionMajor;
  property VersionMinor : Byte read FVersionMinor;
end;

implementation


  function GetCipherName(aCipherType : TKBCiphering) : String;
  begin
    result := 'basicCyph'; //{ TODO : To complete. }
  end;

  function GetCompressName(aCompressType : TKBCompression) : String;
  begin
    result := 'basicComp'; //{ TODO : To complete. }
  end;

{ TGRIDProtocol_KissB }

function TGRIDProtocol_KissB.AllowByteCountPrefix: Boolean;
begin
  result := true; //Stream protocol, size is first data.
end;

function TGRIDProtocol_KissB.AllowEmptyInputData: Boolean;
begin
  result := true; //Messaging protocol, as MQTT is.
end;

constructor TGRIDProtocol_KissB.Create;
begin
  inherited Create;
  FCurrentFormat := TGRIDProtocolFormat.Binary;
  FProtoFormatResolved := false; //this will be resolved server side per client.
//  FProtocolForced := false;
end;

function TGRIDProtocol_KissB.IsProtocolFormatResolved(
  aStream: TStream): Boolean;
var la : TGRIDProtocol_KB_CLT_NEGOCIATE;
begin
  result := FProtoFormatResolved;
  if not(result) then
  begin
    //The protocol has passed the negociate, we have to determine now the client format :
    //Try to negociate in binary format first
    try
      aStream.Position := 0;
      la.Load(aStream);
      FCurrentFormat := TGRIDProtocolFormat.Binary;
      FProtoFormatResolved := true; //Next time will not pass here.
    Except
      try
        aStream.Position := 0;
        la.Load(aStream,TGRIDProtocolFormat.json);
        FCurrentFormat := TGRIDProtocolFormat.json;
        FProtoFormatResolved := true; //Next time will not pass here.
      Except
      end;
    end;
    result := FProtoFormatResolved;
    FVersionMajor := la.ProtocolVersionMajor;
    FVersionMinor := la.ProtocolVersionMinor;

    FCiphered := (la.AskForCiphering<>TKBCiphering.none);
    FCompressed := (la.AskForCompression<>TKBCompression.none);
    FCipherDataId := GetCipherName(la.AskForCiphering);
    FCompressDataId := GetCompressName(la.AskForCompression);
  end;
end;

class function TGRIDProtocol_KissB.Negociate(aStream: TStream): Boolean;
var la : TGRIDProtocol_KB_CLT_NEGOCIATE;
begin
  Assert(assigned(aStream));
  Assert(aStream.Size>0);
  result := false;

  if (aStream.Size<20) or (aStream.Size>200) then
  begin
    raise EExceptionProtocolNegotiation.Create(ClassName + ' - Protocol negociation failure [A4]');
  end;

  //Try to negociate in binary format first
  try
    aStream.Position := 0;

    la.Load(aStream);
    if la.Header.Command <> TKBCltCommand.connect then
      raise EExceptionProtocolNegotiation.Create(ClassName + ' - Protocol negociation failure [A5]');

    //it is ok, remove first int. (useless an not a part of app level protocol.
    aStream.Position := 0;
  Except
    try
      //if failed, try in json format.
       aStream.Position := 0;
       la.Load(aStream,TGRIDProtocolFormat.json);
       if la.Header.Command <> TKBCltCommand.connect then
        raise EExceptionProtocolNegotiation.Create(ClassName + ' - Protocol negociation failure [A6]');

      aStream.Position := 0;
    Except
      //Exception handled in caller. If KissB get new format, it place to be.
      raise EExceptionProtocolNegotiation.Create(ClassName + ' - Protocol negociation failure');
    end;
  end;
  result := la.Signature = CST_SIGNATURE;
end;

function TGRIDProtocol_KissB.ProtocolDescription: String;
begin
  result := 'KissB (Keep It Simple and Stupid Bus) protocol is a simple yet '+
            ' production-ready protocol for a crossplatform communication';
end;

function TGRIDProtocol_KissB.ProtocolName: String;
begin
  result := 'KissB';
end;

function TGRIDProtocol_KissB.ProtocolNativeFormat: TGRIDProtocolFormat;
begin
  Result := FCurrentFormat;
end;

procedure TGRIDProtocol_KissB.SetProtocolNativeFormat(
  aFormat: TGRIDProtocolFormat);
begin
//  FProtocolForced := true;
  FProtoFormatResolved := false;
  FCurrentFormat := aFormat;
end;

{ TGRIDProtocol_KB_CLT_NEGOCIATE }

procedure TGRIDProtocol_KB_CLT_NEGOCIATE.Load(aStream: TStream; Const aFormat : TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header.Command := TKBCltCommand(ReadByte(aStream));
      Signature := ReadString(aStream);
      ProtocolVersionMajor := ReadByte(aStream);
      ProtocolVersionMinor :=  ReadByte(aStream);
      AskForFormat := TGRIDProtocolFormat(ReadByte(aStream));
      AskForCompression := TKBCompression(ReadByte(aStream));
      AskForCiphering := TKBCiphering(ReadByte(aStream));
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header.Command := TKBCltCommand(la['Header.Command'].AsInteger);
        Signature := la['Signature'].AsString;
        ProtocolVersionMajor := la['ProtocolVersionMajor'].AsInteger;
        ProtocolVersionMinor :=  la['ProtocolVersionMinor'].AsInteger;
        AskForFormat := TGRIDProtocolFormat(la['AskForFormat'].AsInteger);
        AskForCompression := TKBCompression(la['AskForCompression'].AsInteger);
        AskForCiphering := TKBCiphering(la['AskForCiphering'].AsInteger);
      finally
        FreeAndNil(la);
      end;
    end;
  end;
end;

procedure TGRIDProtocol_KB_CLT_NEGOCIATE.Save(aStream: TStream; Const aFormat : TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteByte(aStream,Byte(Header.Command));
      WriteString(aStream,Signature);
      WriteByte(aStream,ProtocolVersionMajor);
      WriteByte(aStream,ProtocolVersionMinor);
      WriteByte(aStream,Byte(AskForFormat));
      WriteByte(aStream,Byte(AskForCompression));
      WriteByte(aStream,Byte(AskForCiphering));
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Put('Header.Command',Byte(Header.Command));
        la.Put('Signature',Signature);
        la.Put('ProtocolVersionMajor',Int64(ProtocolVersionMajor));
        la.Put('ProtocolVersionMinor',Int64(ProtocolVersionMinor));
        la.Put('AskForFormat',Int64(AskForFormat));
        la.Put('AskForCompression',Int64(AskForCompression));
        la.Put('AskForCiphering',Int64(AskForCiphering));
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;


{ TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE }

procedure TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE.Load(aStream: TStream; Const aFormat : TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header := TKBCltCommand_FromServer(ReadByte(aStream));
      Status := ReadBoolean(aStream);
      StatusInfo := ReadString(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header := TKBCltCommand_FromServer(la['Header'].AsInteger);
        Status := la['Status'].AsBoolean;
        StatusInfo := la['StatusInfo'].AsString;
      finally
        FreeAndNil(la);
      end;
    end;
  end;
end;

procedure TGRIDProtocol_KB_SRV_NEGOCIATE_RESPONSE.Save(aStream: TStream; Const aFormat : TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteByte(aStream,Byte(Header));
      WriteBoolean(aStream,Status);
      WriteString(aStream,StatusInfo);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Put('Header',Byte(Header));
        la.Put('Status',Status);
        la.Put('StatusInfo',StatusInfo);
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;


procedure TGRIDProtocol_KB_SRV_PROCESS_API_INFO.Clear;
begin
  ServerGenuineName := '';
  ServerHostCPUArchitecture := '';
  ServerHostArchitecture := '';
  ServerHostOS := '';
  ServerHostOSBuild := '';
  GRIDVersion := '';
  GRIDServerName := '';
  GRIDServices := '';
  GRIDArch := '';
  GRIDCompiler := '';
end;

procedure TGRIDProtocol_KB_SRV_PROCESS_API_INFO.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
//      Header := TKBCltCommand_FromServer(ReadByte(aStream));
      ServerGenuineName := ReadString(aStream);
      ServerHostCPUArchitecture := ReadString(aStream);
      ServerHostArchitecture := ReadString(aStream);
      ServerHostOS := ReadString(aStream);
      ServerHostOSBuild := ReadString(aStream);
      GRIDVersion := ReadString(aStream);
      GRIDServerName := ReadString(aStream);
      GRIDArch := ReadString(aStream);
      GRIDCompiler := ReadString(aStream);
      GRIDServices := ReadString(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
//        Header := TKBCltCommand_FromServer(la['Header'].AsInteger);
        ServerGenuineName := la['ServerGenuineName'].AsString;
        ServerHostCPUArchitecture := la['ServerHostCPUArchitecture'].AsString;
        ServerHostArchitecture := la['ServerHostArchitecture'].AsString;
        ServerHostOS := la['ServerHostOS'].AsString;
        ServerHostOSBuild := la['ServerHostOSBuild'].AsString;
        GRIDVersion := la['GRIDVersion'].AsString;
        GRIDServerName := la['GRIDServerName'].AsString;
        GRIDArch := la['GRIDArch'].AsString;
        GRIDCompiler := la['GRIDCompiler'].AsString;
        GRIDServices := la['GRIDServices'].AsString;
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TGRIDProtocol_KB_SRV_PROCESS_API_INFO.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      //Header
//      WriteByte(aStream,Byte(Header));
      //data
      WriteString(aStream,ServerGenuineName);
      WriteString(aStream,ServerHostCPUArchitecture);
      WriteString(aStream,ServerHostArchitecture);
      WriteString(aStream,ServerHostOS);
      WriteString(aStream,ServerHostOSBuild);
      WriteString(aStream,GRIDVersion);
      WriteString(aStream,GRIDServerName);
      WriteString(aStream,GRIDArch);
      WriteString(aStream,GRIDCompiler);
      WriteString(aStream,GRIDServices);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        //Header
//        la.Put('Header',Int64(Header));
        //data
        la.Put('ServerGenuineName',ServerGenuineName);
        la.Put('ServerHostCPUArchitecture',ServerHostCPUArchitecture);
        la.Put('ServerHostArchitecture',ServerHostArchitecture);
        la.Put('ServerHostOS',ServerHostOS);
        la.Put('ServerHostOSBuild',ServerHostOSBuild);
        la.Put('GRIDVersion',GRIDVersion);
        la.Put('GRIDServerName',GRIDServerName);
        la.Put('GRIDArch',GRIDArch);
        la.Put('GRIDCompiler',GRIDCompiler);
        la.Put('GRIDServices',GRIDServices);
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

{ TGRIDProtocol_KB_CLT_PROCESS_SPL_API }

procedure TGRIDProtocol_KB_CLT_PROCESS_SPL_API.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header.Command := TKBCltCommand(ReadByte(aStream));
      CommandID := ReadString(aStream);
      SubCall := ReadBytes(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header.Command := TKBCltCommand(la['Header.Command'].AsInteger);
        CommandID := la['CommandID'].AsString;
        SubCall := TGSJson.Base64StringToBytes(la['SubCall'].AsString);
      finally
        FreeAndNil(la);
      end;
    end;
  end;
end;

procedure TGRIDProtocol_KB_CLT_PROCESS_SPL_API.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  Header.Command := TKBCltCommand.process_rpc_simple;
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteByte(aStream,Byte(Header.Command));
      WriteString(aStream,CommandID);
      WriteBytes(aStream,SubCall);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Put('Header.Command',Byte(Header.Command));
        la.Put('CommandID',CommandID);
        la.Put('SubCall',String(TGSJSON.BytesToBase64String(SubCall)));
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

{ TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE }

procedure TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header := TKBCltCommand_FromServer(ReadByte(aStream));
      Status := ReadBoolean(aStream);
      StatusInfo := ReadString(aStream);
      ResultPayload := ReadBytes(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header := TKBCltCommand_FromServer(la['Header'].AsInteger);
        Status := la['Status'].AsBoolean;
        StatusInfo := la['StatusInfo'].AsString;
        ResultPayload := TGSJson.Base64StringToBytes(la['ResultPayload'].AsString);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TGRIDProtocol_KB_SRV_PROCESS_SPL_API_RESPONSE.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      //Header
      WriteByte(aStream,Byte(Header));
      //data
      WriteBoolean(aStream,Status);
      WriteString(aStream,StatusInfo);
      WriteBytes(aStream,ResultPayload);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        //Header
        la.Put('Header',Byte(Header));
        //data
        la.Put('Status',Status);
        la.Put('StatusInfo',StatusInfo);
        la.Put('ResultPayload',TGSJSON.BytesToBase64String(ResultPayLoad));
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;


{ TGRIDProtocol_KB_CLT_PROCESS_SPL_API_KEYVALUE }

procedure TGRIDProtocol_KB_CLT_PROCESS_SPL_API_KEYVALUE.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
//      Header.Command := TKBCltCommand(ReadByte(aStream));
      Repo := ReadString(aStream);
      GetSetCode := ReadString(aStream);
      Key := ReadString(aStream);
      Value := ReadBytes(aStream);
      OptionalPropagateChannels := ReadString(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
//        Header.Command := TKBCltCommand(la['Header.Command'].AsInteger);
        Repo := la['Repo'].AsString;
        GetSetCode := la['GetSetCode'].AsString;
        Key := la['Key'].AsString;
        Value := TGSJson.Base64StringToBytes(la['Value'].AsString);
        OptionalPropagateChannels := la['OptionalPropagateChannels'].AsString;
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TGRIDProtocol_KB_CLT_PROCESS_SPL_API_KEYVALUE.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      //Header
//      WriteByte(aStream,Byte(Header.Command));
      //data
      WriteString(aStream,Repo);
      WriteString(aStream,GetSetCode);
      WriteString(aStream,Key);
      WriteBytes(aStream,Value);
      WriteString(aStream,OptionalPropagateChannels);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        //Header
//        la.Put('Header.Command',Byte(Header.Command));
        //data
        la.Put('Repo',Repo);
        la.Put('GetSetCode',GetSetCode);
        la.Put('Key',Key);
        la.Put('Value',String(TGSJSON.BytesToBase64String(Value)));
        la.Put('OptionalPropagateChannels',OptionalPropagateChannels);
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;


{ TGRIDProtocol_KB_CLT_BUS_CMD }

procedure TGRIDProtocol_KB_CLT_BUS_CMD.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header.Command := TKBCltCommand(ReadByte(aStream));
      CommandStructure := TKBCltBusCmd(ReadByte(aStream));
      ChannelInvolved := ReadString(aStream);
      Payload := ReadBytes(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header.Command := TKBCltCommand(la['Header.Command'].AsInteger);
        CommandStructure := TKBCltBusCmd(la['CommandStructure'].AsInteger);
        ChannelInvolved := la['ChannelInvolved'].AsString;
        Payload := TGSJson.Base64StringToBytes(la['Payload'].AsString);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TGRIDProtocol_KB_CLT_BUS_CMD.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      //Header.
      WriteByte(aStream,Byte(Header.Command));
      //data.
      WriteByte(aStream,Byte(CommandStructure));
      WriteString(aStream,ChannelInvolved);
      WriteBytes(aStream,Payload);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        //Header
        la.Put('Header.Command',Byte(Header.Command));
        //data
        la.Put('CommandStructure',Byte(CommandStructure));
        la.Put('ChannelInvolved',ChannelInvolved);
        la.Put('Payload',String(TGSJSON.BytesToBase64String(Payload)));
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;


{ TGRIDProtocol_KB_SRV_BUS_CMD_RESPONSE }

procedure TGRIDProtocol_KB_SRV_BUS_CMD_RESPONSE.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header := TKBCltCommand_FromServer(ReadByte(aStream));
      Status := ReadBoolean(aStream);
      StatusInfo := ReadString(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header := TKBCltCommand_FromServer(la['Header'].AsInteger);
        Status := la['Status'].AsBoolean;
        StatusInfo := la['StatusInfo'].AsString;
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TGRIDProtocol_KB_SRV_BUS_CMD_RESPONSE.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      //Header.
      WriteByte(aStream,Byte(Header));
      //data
      WriteByte(aStream,Byte(Status));
      WriteString(aStream,StatusInfo);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        //Header
        la.Put('Header',Byte(Header));
        //data
        la.Put('Status',Status);
        la.Put('StatusInfo',StatusInfo);
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

{ TGRIDProtocol_KB_SRV_BUS_CMD_MSG_RESPONSE }

procedure TGRIDProtocol_KB_SRV_BUS_CMD_MSG_RESPONSE.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
    i,l : integer;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header := TKBCltCommand_FromServer(ReadByte(aStream));
      Status := ReadBoolean(aStream);
      StatusInfo := ReadString(aStream);
      l := ReadUInt32(aStream);
      SetLength(MessagesPayLoad,l);
      for I := 0 to l-1 do
      begin
        MessagesPayLoad[i].From := ReadString(aStream);
        MessagesPayLoad[i].Channel := ReadString(aStream);
        MessagesPayLoad[i].MessagePayload := ReadBytes(aStream);
        MessagesPayLoad[i].Ticks := ReadUInt64(aStream);
      end;
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        //Header
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header := TKBCltCommand_FromServer(la['Header'].AsInteger);
        Status := la['Status'].AsBoolean;
        StatusInfo := la['StatusInfo'].AsString;
        l := la['MessageCount'].AsInteger;
        SetLength(MessagesPayLoad,l);
        With la['Messages'].AsArray do
        begin
          for I := 0 to l-1 do
          begin
            MessagesPayLoad[i].From := TJsonObject(Items[i].asObject)['From'].asString;
            MessagesPayLoad[i].Channel := TJsonObject(Items[i].asObject)['Channel'].asString;
            MessagesPayLoad[i].MessagePayload := TGSJson.Base64StringToBytes(TJsonObject(Items[i].asObject)['PayLoad'].asString);
            MessagesPayLoad[i].Ticks := StrToInt(TJsonObject(Items[i].asObject)['Ticks'].AsString);
          end;
        end;
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TGRIDProtocol_KB_SRV_BUS_CMD_MSG_RESPONSE.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
    i : integer;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      //Header.
      WriteByte(aStream,Byte(Header));
      //data
      WriteBoolean(aStream,Status);
      WriteString(aStream,StatusInfo);
      WriteUInt32(aStream, Length(MessagesPayLoad));
      for I := 0 to Length(MessagesPayLoad)-1 do
      begin
        WriteString(aStream,MessagesPayLoad[i].From);
        WriteString(aStream,MessagesPayLoad[i].Channel);
        WriteBytes(aStream,MessagesPayLoad[i].MessagePayload);
        WriteUInt64(aStream,MessagesPayLoad[i].Ticks);
      end;
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        //Header
        la.Put('Header',Byte(Header));
        //data
        la.Put('Status',Status);
        la.Put('StatusInfo',StatusInfo);
        la.Put('MessageCount',Length(MessagesPayLoad));

        With la['Messages'].AsArray do
        begin
          for I := 0 to Length(MessagesPayLoad)-1 do
          begin
            With Add.AsObject do
            begin
              Put('From',MessagesPayLoad[i].From);
              Put('Channel',MessagesPayLoad[i].Channel);
              Put('PayLoad',String(TGSJson.BytesToBase64String(MessagesPayLoad[i].MessagePayload)));
              Put('Ticks',IntToStr(MessagesPayLoad[i].Ticks)); //unfortunately in string, to conserve Uint64 feature. Todo : base64 ?
            end;
          end;
        end;
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

{ TGRIDProtocol_KB_SRV_NEGOCIATE_HALF_RESPONSE }

procedure TGRIDProtocol_KB_SRV_NEGOCIATE_HALF_RESPONSE.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header := TKBCltCommand_FromServer(ReadByte(aStream));
      Status := ReadBoolean(aStream);
      StatusInfo := ReadString(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header := TKBCltCommand_FromServer(la['Header'].AsInteger);
        Status := la['Status'].AsBoolean;
        StatusInfo := la['StatusInfo'].AsString;
      finally
        FreeAndNil(la);
      end;
    end;
  end;
end;

procedure TGRIDProtocol_KB_SRV_NEGOCIATE_HALF_RESPONSE.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      //Header
      WriteByte(aStream,Byte(Header));
      //data
      WriteBoolean(aStream,Status);
      WriteString(aStream,StatusInfo);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        //Header
        la.Put('Header',Byte(Header));
        //data
        la.Put('Status',Status);
        la.Put('StatusInfo',StatusInfo);
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

{ TGRIDProtocol_KB_CLT_NEGOCIATE_UP }

procedure TGRIDProtocol_KB_CLT_NEGOCIATE_UP.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Header.Command := TKBCltCommand(ReadByte(aStream));
      UserName := ReadString(aStream);
      Password := ReadString(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream,true));
        Header.Command := TKBCltCommand(la['Header.Command'].AsInteger);
        UserName := la['UserName'].AsString;
        Password := la['Password'].AsString;
      finally
        FreeAndNil(la);
      end;
    end;
  end;
end;

procedure TGRIDProtocol_KB_CLT_NEGOCIATE_UP.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  Header.Command := TKBCltCommand.connectup;
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteByte(aStream,Byte(Header.Command));
      WriteString(aStream,UserName);
      WriteString(aStream,Password);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Put('Header.Command',Byte(Header.Command));
        la.Put('UserName',UserName);
        la.Put('Password',Password);
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

Initialization

GridProtocolManager.Add(TGRIDProtocol_KissB);

end.
