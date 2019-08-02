//Hypervisor :
// - Manage external task to run under GRID supervision.
// - Scheduler to miminc Cron task.
unit GS.GRID.Server.Service.Hypervisor;

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

uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 SyncObjs,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 System.SyncObjs,
 {$ENDIF}
 GS.Bus,
 GS.Bus.Services,
 GS.Threads.Pool,
 GS.CPUUsage,
 GS.Stream,
 GS.TimerScheduler,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server.MicroServices,
 GS.GRID.Common.Protocols.MicroService,
 os_api_unit
 ;

Type

  TCustomGridHypervisor = Class(TGRIDService)
  private
    FCPU : TgsCPUUsage;
    FDico : TBusClientDataRepo;

    //MicroService.
    FMicroService_Registering : TBusClientReader;
    FMicroService_ClientAskForSvcList : TBusClientReader;

    //Microservice list.
    FMicroServices : TGSProtectedGridServerMicroServiceList;
  protected
    FTimerScheduler : TGSTimerSchedulerThreadContainer;
    FThreadPool :TStackThreadPool; //that will execute internal and external business task.

    //this event for external binaries (Microsrvice itself) which want to register their service.
    Procedure InternatSystemHypervisorMicroServiceRegistering(Sender : TBusSystem;aReader : TBusClientReader; Var Packet : TBusEnvelop);
    Procedure InternatSystemHypervisorMicroServiceAskList(Sender : TBusSystem;aReader : TBusClientReader; Var Packet : TBusEnvelop);

    //
    Procedure InternalSystemHypervisorTimer(Sender : TObject);
    procedure PublishSecond;
  public
    //Service...
    procedure Initialize; Override;
    Procedure Execute; Override;
    procedure Finalize; Override;

    //All task affected to service will be freed on destroy.
    property Scheduler : TGSTimerSchedulerThreadContainer read FTimerScheduler;
  End;

  TGridHypervisor = Class(TCustomGridHypervisor)
     Class Function GetDefaultImplementation : TGridService; Override;
     function AutoTestImplementation: TCustomGridServiveServerTestResult; Override;
  End;

implementation

{ TCustomGridHypervisor }


procedure TCustomGridHypervisor.Execute;
var la : TBusClientReaderArray;
begin
  setlength(la,3);
  la[0] := FGlobalInstructionChannel;
  la[1] := FMicroService_Registering;
  la[2] := FMicroService_ClientAskForSvcList;

  while Not(MasterThread.Terminated) do
  begin
    BusProcessMessages(la);
    Sleep(CST_ONE_SEC);
    //MasterThread.DoHeartBeat;
  end;

end;

procedure TCustomGridHypervisor.Finalize;
begin
  FTimerScheduler.Enabled := false;
  GridBus.UnSubscribe(FMicroService_Registering);
  GridBus.UnSubscribe(FMicroService_ClientAskForSvcList);
  FreeAndNil(FTimerScheduler);
  FreeAndNil(FThreadPool);
  FreeAndNil(FMicroService_Registering);
  FreeAndNil(FMicroService_ClientAskForSvcList);
  FreeAndNil(FCPU);
  FreeAndNil(FDICO);
  FreeAndNil(FMicroServices);
  inherited;
end;

procedure TCustomGridHypervisor.Initialize;
begin
  inherited;
  FMicroServices := TGSProtectedGridServerMicroServiceList.Create;
  FCPU := TgsCPUUsage.Create;
  FTimerScheduler := TGSTimerSchedulerThreadContainer.Create;
  FThreadPool := TStackDynamicThreadPool.Create;

  FMicroService_Registering := GridBus.Subscribe(CST_PUBLIC_CHANNAME_HYPERVISOR_SERVICE_REGISTRATION,InternatSystemHypervisorMicroServiceRegistering);
  FMicroService_Registering.Event := GridBus.GetNewEvent;

  FMicroService_ClientAskForSvcList := GridBus.Subscribe(CST_PUBLIC_CHANNAME_HYPERVISOR_SERVICE_LIST,InternatSystemHypervisorMicroServiceAskList);
  FMicroService_ClientAskForSvcList.Event := GridBus.GetNewEvent;

  //Use central timer for CPU Data.
  FTimerScheduler.OnTimer := InternalSystemHypervisorTimer;
  FTimerScheduler.Enabled := true;

  //For Posting CPU Data.
  FDico := TBusClientDataRepo.Create(Self.MasterThread.Bus,CST_BUSDATAREPO_SERVERINFO);
end;


procedure TCustomGridHypervisor.InternalSystemHypervisorTimer(Sender: TObject);
begin
  //Monitoring : Post data into repo for API querying.
  FCPU.Update;
  FDico.SetValue('GS.GRID.CPU.UsagePercent',FCPU.UsageCPUPercent);
  PublishSecond;
end;

procedure TCustomGridHypervisor.InternatSystemHypervisorMicroServiceAskList(
  Sender: TBusSystem; aReader: TBusClientReader; var Packet: TBusEnvelop);

var
  Client : TMsg_MicroService_CLI_FromClient_ToServer_Ask;
  Response : TMsg_MicroService_CLI_FromServer_ToClient_AskResponse_List;

  lStream : TMemoryStream;
  ResponseMes : TBusMessage;

  ll : TObjectList_TMicroService;
  i : integer;

begin
  lstream := Packet.ContentMessage.AsStream;
  try
    Client.load(Packet.ContentMessage.AsStream);
    case Client.ClientAskCode of
      TMicroService_CLI_ClientAskCode.list:
      begin
        Response.AskedCode := TMicroService_CLI_ClientAskCode.list;
        Response.Status := True;
        Response.StatusInfo := '';
        ll := FMicroServices.Lock;
        try
          for I := 0 to ll.Count-1 do
          begin
            Response.Services[i] := ll[i].ServiceInformations;
          end;
        finally
          FMicroServices.Unlock;
        end;
        lStream.Clear;
        Response.Save(lStream);
      end
      else
        raise Exception.Create('Error Message');
    end;
    lStream.Position := 0;
    ResponseMes.FromStream(lStream);
    GridBus.Send(ResponseMes,Packet.ResponseChannel);
  finally
    FreeAndNil(lStream);
  end;
end;

procedure TCustomGridHypervisor.InternatSystemHypervisorMicroServiceRegistering(
  Sender: TBusSystem; aReader: TBusClientReader; var Packet: TBusEnvelop);

  var lChannelService : String;
    Service : TMsg_FromService_ServiceData;
    Response : TMsg_FromServer_ServiceRegistrationResponse;

    lStream : TMEmoryStream;
    lms : TObjectList_TMicroService;
    ls : TMicroService;

    ResponseMes : TBusMessage;

begin
  lstream := Packet.ContentMessage.AsStream;
  try
    Service.load(Packet.ContentMessage.AsStream);

    lms := FMicroServices.Lock;
    try
      if NOT lms.MicroServiceById(Service.ID,ls) then
      begin
        ls := TMicroService.Create(Packet.ClientSourceId);
        ls.ServiceInformations := Service;
        ls.ChannelExecute := 'EX/'+Service.ID+'/'+WithoutT(ClassName)+'.'+ls.UserIdOwner;
        lms.Add(ls);
      end;
      ls.Registered;
    finally
      FMicroServices.Unlock;
    end;

    Response.Status := true;
    Response.StatusInfo := '';
//    Response.ChannelName_ServiceStartNotification := 'SSN/'+Service.ID+'/'+WithoutT(ClassName);
//    Response.ChannelName_ServiceShutdownNotification := 'SDN/'+Service.ID+'/'+WithoutT(ClassName);
//    Response.ChannelName_AskForStatus := 'AFS/'+Service.ID+'/'+WithoutT(ClassName);
    Response.ChannelName_ServerExecute := ls.ChannelExecute;
    lChannelService := 'SSN/'+Service.ID+'/'+WithoutT(ClassName);

    lstream.Clear;
    Response.Save(lStream);
    lStream.Position := 0;
    ResponseMes.FromStream(lStream);
    GridBus.Send(ResponseMes,Service.ResponseChannel);
  finally
    FreeAndNil(lStream);
  end;
end;

procedure TCustomGridHypervisor.PublishSecond;
var amessage : TBusMessage;
begin
  //Subscribte to this channel, and you'll get a timer. ;)
  GridBus.Send(amessage,CST_CHANNEL_NAME_SECOND_SIGNAL);
end;

{ TGridHypervisor }

function TGridHypervisor.AutoTestImplementation: TCustomGridServiveServerTestResult;
begin
  result := nil;
end;

class function TGridHypervisor.GetDefaultImplementation: TGridService;
begin
  Result := TGridHypervisor.Create;
end;

end.
