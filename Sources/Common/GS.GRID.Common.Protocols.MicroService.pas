unit GS.GRID.Common.Protocols.MicroService;

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


Interface

{$I GSCore.inc}

uses
  Classes,
  SysUtils,
  GS.GRID.Common.Protocols,
  GS.Stream,
  GS.JSON,
  Jsons;

{ TODO 2 -oVGS -cCritical : Make this channel protected by any manner... }
Const
  CST_PUBLIC_CHANNAME_HYPERVISOR_SERVICE_REGISTRATION = 'System.HV.SvcReg';
  CST_PUBLIC_CHANNAME_HYPERVISOR_SERVICE_LIST = 'System.HV.SvcLst';
  CST_FORMAT_NOT_SUPPORTED = 'MicroService : Protocol not supported';

Type
  TGsMSVersionStatus = (Idea, RnD, EarlyAlpha, Alpha, Beta, Test, ProdReady, Production);

  //TGsMSAvailability :                        (Impact : Server side only)
  // Disable : Service is registered but unavailable. (admin parameter)
  // Enable : Service is registered and available for call
  TGsMSAvailability = (Disable, Enable);

  //Grid System's Micro Service definition :  (Impact : Server side only)
  //TGsMSBehaviour :
  // Coop : Service Is manualy launched : GS will not tryed to start it or stop it.
  // Resident : Service is launched by GS and never shutdown.
  // OnDemand : Service is launched by GS on demand.
  TGsMSBehaviour = (Coop, Resident, OnDemand);

  //TGsMSInstanceBehaviour :                  (Impact : Server side only)
  // MonoInstance : Only one instance at a time.
  // MultiInstance : Many isntances at a time is allowed.
  TGsMSInstanceBehaviour = (MonoInstance, MultiInstance);

  //TGsMSRunningOn :                          (Impact : Server side only)
  // LocalOnly : Binary must be talk on loopback network instance only.
  // LocalOrRemote : Binary could talk wherever they are..
  // RemoteOnly : Binary must *not* talk on loopback network.
  TGsMSRunningOn = (LocalOnly, LocalOrRemote, RemoteOnly);


  //TGsMSUserManagementCapabilites :          (Impact : Server and client side)
  // MonoUser : The service is write for a mono user usage : Server will make a queue, to allow user to use it.
  //            --> OnGridServerAskForStatus is running in queue, each request after the other.
  // MultiUser : The service is able to manage multiuser channel : Server will deploy a dedicated channel per user.
  //            --> OnGridServerAskForStatus is running in a parallel way.

  TGsMSUserManagementCapabilites = (MonoUser, MultiUser);


  TGsServerData = record
    GridServerName : string;
    GridServerNetworkData : string;
  end;

  TGsUserData = record
    username : String;
    session : string;
    InData : TBytes;
  end;

  TGsServiceData = record
    OK : boolean;
    NOKDesc : String;
    IsIdle : Boolean;
    CurrentWorkDesc : String;
  end;

  //Protocol.
  // [SERVICE] -------------------------------------------------- [GRID] -------------------- [CLIENT]
  // [Registering service.
  // [SERVICE] TMsg_FromService_ServiceData --------------------> [GRID]
  // [SERVICE] <----- TMsg_FromServer_ServiceRegistrationResponse [GRID]
  //
  // Get service's list.
  //               [GRID] <-------------------- TMsg_MicroService_CLI_FromClient_ToServer_Ask [CLIENT]
  //               [GRID] TMsg_MicroService_CLI_FromServer_ToClient_AskResponse_List -------> [CLIENT]
  //
  //



  TMsg_FromService_ServiceData = packed Record
    ID : String;
    VersionMajor : UInt32;
    VersionMinor : UInt32;
    VersionRelease : UInt32;
    VersionStatus : TGsMSVersionStatus;
    Compagny : String;
    ServiceName : String;
    ServiceDesc : String;

    MicroSvcAvailability : TGsMSAvailability;
    MicroSvcBehaviour : TGsMSBehaviour;
    MicroSvcInstanceBehaviour : TGsMSInstanceBehaviour;
    MicroSvcRunningOn : TGsMSRunningOn;
    MicroSvcUserMgntCapabilities : TGsMSUserManagementCapabilites;

    ResponseChannel : String;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;

  TMsg_FromServer_ServiceRegistrationResponse = Packed record
    Status : boolean;
    StatusInfo : String;

//    ChannelName_ServiceStartNotification : String;
//    ChannelName_ServiceShutdownNotification : String;
//    ChannelName_AskForStatus : String;
    ChannelName_ServerExecute : String;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;

  TMicroService_CLI_ClientAskCode = (list); //Access, ExecuteContextSession (ala Synology style), etc etc
  TMsg_MicroService_CLI_FromClient_ToServer_Ask = Packed Record
    ClientAskCode : TMicroService_CLI_ClientAskCode;
    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;
  TMsg_MicroService_CLI_FromServer_ToClient_AskResponse_List = Packed Record
    AskedCode : TMicroService_CLI_ClientAskCode;
    Status : Boolean;
    StatusInfo : string;
    Services : Array of TMsg_FromService_ServiceData;
    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  end;


  TMsg_FromService_ServerAskStatusResponse = Packed Record
  End;

  TMsg_FromService_ShutDownResponse = Packed Record
  End;

  TMsg_FromServer_ExecuteOrder = Packed Record

    ChannelReturn : string;
    InPayload : TBytes;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  End;

  TMsg_FromService_ExecuteResponse = Packed Record
    ExecTimeElapsedMSec : UInt32;
    ExecPayloadResult : TBytes;

    procedure Save(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
    procedure Load(aStream : TStream; Const aFormat : TGRIDProtocolFormat = TGRIDProtocolFormat.Binary);
  End;


  function WithoutT(const str : String) : String; //Pascalish signature not much apreciated in outside world.

Implementation

  function WithoutT(const str : String) : String;
  begin
    if length(str)>2 then
      if str[1] = 'T' then
        result := copy(str,2,Length(str)-1);
  end;


{ TMsg_FromServer_ServiceRegistrationResponse }

procedure TMsg_FromServer_ServiceRegistrationResponse.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      Status := ReadBoolean(aStream);
      StatusInfo := ReadString(aStream);
//      ChannelName_ServiceStartNotification :=ReadString(aStream);
//      ChannelName_ServiceShutdownNotification :=ReadString(aStream);
//      ChannelName_AskForStatus :=ReadString(aStream);
      ChannelName_ServerExecute :=ReadString(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream));
        Status := la['Status'].AsBoolean;
        StatusInfo := la['StatusInfo'].AsString;
//        ChannelName_ServiceStartNotification :=la['ChannelName_ServiceStartNotification'].AsString;
//        ChannelName_ServiceShutdownNotification :=la['ChannelName_ServiceShutdownNotification'].AsString;
//        ChannelName_AskForStatus :=la['ChannelName_AskForStatus'].AsString;
        ChannelName_ServerExecute :=la['ChannelName_ServerExecute'].AsString;
      finally
        FreeAndNil(la);
      end;
    end;
  end;
end;

procedure TMsg_FromServer_ServiceRegistrationResponse.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteBoolean(aStream,Status);
      WriteString(aStream,StatusInfo);
//      WriteString(aStream,ChannelName_ServiceStartNotification);
//      WriteString(aStream,ChannelName_ServiceShutdownNotification);
//      WriteString(aStream,ChannelName_AskForStatus);
      WriteString(aStream,ChannelName_ServerExecute);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Put('Status',Status);
        la.Put('StatusInfo',StatusInfo);
//        la.Put('ChannelName_ServiceStartNotification',ChannelName_ServiceStartNotification);
//        la.Put('ChannelName_ServiceShutdownNotification',ChannelName_ServiceShutdownNotification);
//        la.Put('ChannelName_AskForStatus',ChannelName_AskForStatus);
        la.Put('ChannelName_ServerExecute',ChannelName_ServerExecute);
        WriteRAWStringUTF8(aStream,la.Stringify);
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


{ TMsg_FromService_ServiceData }

procedure TMsg_FromService_ServiceData.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      ID := ReadString(aStream);
      VersionMajor := ReadUINT32(aStream);
      VersionMinor := ReadUInt32(aStream);
      VersionRelease := ReadUInt32(aStream);
      VersionStatus := TGsMSVersionStatus(ReadByte(aStream));
      Compagny := ReadString(aStream);
      ServiceName := ReadString(aStream);
      ServiceDesc := ReadString(aStream);

      MicroSvcAvailability := TGsMSAvailability(ReadByte(aStream));
      MicroSvcBehaviour := TGsMSBehaviour(ReadByte(aStream));
      MicroSvcInstanceBehaviour := TGsMSInstanceBehaviour(ReadByte(aStream));
      MicroSvcRunningOn := TGsMSRunningOn(ReadByte(aStream));
      MicroSvcUserMgntCapabilities := TGsMSUserManagementCapabilites(ReadByte(aStream));

      ResponseChannel := ReadString(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        raise Exception.Create('to do');
        la.Parse(ReadRawStringUTF8(aStream));
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

procedure TMsg_FromService_ServiceData.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteString(aStream, ID);
      WriteUInt32(aStream, VersionMajor);
      WriteUInt32(aStream, VersionMinor);
      WriteUInt32(aStream, VersionRelease);
      WriteByte(aStream, Byte(VersionStatus));
      WriteString(aStream, Compagny);
      WriteString(aStream, ServiceName);
      WriteString(aStream, ServiceDesc);

      WriteByte(aStream, Byte(MicroSvcAvailability));
      WriteByte(aStream, Byte(MicroSvcBehaviour));
      WriteByte(aStream, Byte(MicroSvcInstanceBehaviour));
      WriteByte(aStream, Byte(MicroSvcRunningOn));
      WriteByte(aStream, Byte(MicroSvcUserMgntCapabilities));

      WriteString(aStream, ResponseChannel);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        raise Exception.Create('to do');
        WriteRAWStringUTF8(aStream,la.Stringify);
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

{ TMsg_FromServer_ExecuteOrder }

procedure TMsg_FromServer_ExecuteOrder.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      ChannelReturn := ReadString(astream);
      InPayload := ReadBytes(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream));
        ChannelReturn :=  la['ChannelReturn'].AsString;
        InPayload := TGSJson.Base64StringToBytes(la['InPayload'].AsString);
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

procedure TMsg_FromServer_ExecuteOrder.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteString(astream,ChannelReturn);
      WriteBytes(astream,InPayload);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Put('ChannelReturn',ChannelReturn);
        la.Put('InPayload',String(TGSJSON.BytesToBase64String(InPayload)));
        WriteRAWStringUTF8(aStream,la.Stringify);
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

{ TMsg_FromService_ExecuteResponse }

procedure TMsg_FromService_ExecuteResponse.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      ExecTimeElapsedMSec := ReadUINT32(astream);
      ExecPayloadResult := ReadBytes(aStream);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream));
        ExecTimeElapsedMSec :=  UInt32(la['ExecTimeElapsedMSec'].AsInteger);
        ExecPayloadResult := TGSJson.Base64StringToBytes(la['ExecPayloadResult'].AsString);
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

procedure TMsg_FromService_ExecuteResponse.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteUInt32(astream,ExecTimeElapsedMSec);
      WriteBytes(astream,ExecPayloadResult);
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Put('ExecTimeElapsedMSec',Integer(ExecTimeElapsedMSec)); { TODO -oVGS -cNiceToFix : fix that : Long time exec should turn to bug. Very long. Not an emergency. }
        la.Put('InPayload',String(TGSJSON.BytesToBase64String(ExecPayloadResult)));
        WriteRAWStringUTF8(aStream,la.Stringify);
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

{ TMsg_MicroService_CLI_FromClient_ToServer_Ask }

procedure TMsg_MicroService_CLI_FromClient_ToServer_Ask.Load(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      ClientAskCode := TMicroService_CLI_ClientAskCode(ReadByte(aStream));
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Parse(ReadRawStringUTF8(aStream));
        ClientAskCode := TMicroService_CLI_ClientAskCode(la['ClientAskCode'].AsInteger);
      finally
        FreeAndNil(la);
      end;
    end;
  end;
end;

procedure TMsg_MicroService_CLI_FromClient_ToServer_Ask.Save(aStream: TStream;
  const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      WriteByte(aStream,Byte(ClientAskCode));
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      try
        la.Put('Status',Byte(ClientAskCode));
        WriteRAWStringUTF8(aStream,la.Stringify);
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

{ TMsg_MicroService_CLI_FromServer_ToClient_AskResponse_List }

procedure TMsg_MicroService_CLI_FromServer_ToClient_AskResponse_List.Load(
  aStream: TStream; const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
    i,l : integer;
    serviceAsBytes : TBytes;
    ll : TMemoryStream;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      AskedCode := TMicroService_CLI_ClientAskCode(ReadByte(aStream));
      Status := ReadBoolean(aStream);
      StatusInfo := ReadString(aStream);
      l := ReadUInt32(aStream);
      SetLength(Services,l);
      for I := 0 to l-1 do
      begin
        Services[i].Load(aStream,aFormat);
      end;
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      ll := TMemoryStream.Create;
      try
        //Header
        la.Parse(ReadRawStringUTF8(aStream,true));
        AskedCode := TMicroService_CLI_ClientAskCode(la['AskedCode'].AsInteger);
        Status := la['Status'].AsBoolean;
        StatusInfo := la['StatusInfo'].AsString;
        l := la['ServiceCount'].AsInteger;
        SetLength(Services,l);
        With la['Services'].AsArray do
        begin
          for I := 0 to l-1 do
          begin
            serviceAsBytes := TGSJson.Base64StringToBytes(TJsonObject(Items[i].AsObject)[IntToStr(i)].AsString);
            ll.Clear;
            BytesToStream(ll,serviceAsBytes);
            ll.Position;
            Services[i].Load(ll,aFormat);
          end;
        end;
      finally
        FreeAndNil(la);
        FreeAndNil(ll);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TMsg_MicroService_CLI_FromServer_ToClient_AskResponse_List.Save(
  aStream: TStream; const aFormat: TGRIDProtocolFormat);
var la : TGSJson;
    i : integer;
    ll : TMemoryStream;
begin
  case aFormat of
    TGRIDProtocolFormat.Binary:
    begin
      //Header.
      WriteByte(aStream,Byte(AskedCode));
      //data
      WriteBoolean(aStream,Status);
      WriteString(aStream,StatusInfo);
      WriteUInt32(aStream, Length(Services));
      for I := 0 to Length(Services)-1 do
      begin
        Services[i].Save(aStream,aFormat);
      end;
    end;
    TGRIDProtocolFormat.json:
    begin
      la := TGSJson.Create;
      ll := TMemoryStream.Create;
      try
        //Header
        la.Put('AskedCode',Byte(AskedCode));
        //data
        la.Put('Status',Status);
        la.Put('StatusInfo',StatusInfo);
        la.Put('ServiceCount',Length(Services));

        With la['Services'].AsArray do
        begin
          for I := 0 to Length(Services)-1 do
          begin
            With Add.AsObject do
            begin
              ll.Clear;
              Services[i].Save(ll,aFormat);
              ll.Position := 0;
              Put(IntToStr(i),String(TGSJson.BytesToBase64String(StreamToBytes(ll))));
            end;
          end;
        end;
        WriteRAWStringUTF8(aStream,la.Stringify,true);
      finally
        FreeAndNil(la);
        FreeAndNil(ll);
      end;
    end;
    else
    begin
      raise Exception.Create(CST_FORMAT_NOT_SUPPORTED);
    end;
  end;
end;

end.
