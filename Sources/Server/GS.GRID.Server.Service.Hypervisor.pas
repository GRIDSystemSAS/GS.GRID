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
 Generics.collections,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 System.SyncObjs,
 System.Generics.collections,
 {$ENDIF}
 GS.Common,
 GS.Bus,
 GS.Bus.Services,
 GS.Threads.Pool,
 GS.CPUUsage,
 GS.Stream,
 GS.TimerScheduler,
 GS.GRID.Server.Service.Types,
 GS.GRID.Common.Types,
 os_api_unit
 ;

Type

  TMicroServiceLegal = record
    Author : String;
    Company : String;
    webSite : String;
  end;

  TMicroServiceImplementationType = (internalThread,externalBinary);
  TMicroServiceImplementation = record
    address : String;
    ImplType : TMicroServiceImplementationType;
  end;

  TMicroServiceBinaryStartBehaviour = (onDemand, awakeOnGridStart);
  TMicroServiceBinaryEndBehaviour = (serviceResponsability, noMoreClient);

  TMicroServiceRunBehaviour = (onlyOnceRun, resident);

  TMicroServiceDefinition = Class
  public
    Name : String;
    Description : String;
    LegalInformations : TMicroServiceLegal;
    ServiceImplementation : TMicroServiceImplementation;
    StartBehaviour : TMicroServiceBinaryStartBehaviour;
    EndBehaviour : TMicroServiceBinaryStartBehaviour;
    RunBehaviour : TMicroServiceRunBehaviour;
  End;

  TGridHypervisorTaskDefinitionTaskStatus = (defined, starting, processing, finished);
  TGridHypervisorTaskDefinition = class
  public
    TaskID : String;
    MicroServiceDefinition : TMicroServiceDefinition;
    TaskStartOn : TDateTime;
    TaskRegisterOn : TDateTime;
    TaskFinished : Boolean;
    TaskFinisheOn : TDateTime;
    TaskStatus : TGridHypervisorTaskDefinitionTaskStatus;
    TaskOK : boolean;
    TaskKODesc : String;
    TaskProcessedTime : UInt64; //ms
    TaskHypervisorClientResponseID : String;
    stdInChan : String;
    stdOutChan :string;
    RunCount : Integer;
  end;

  //Use this if you not need input channel. (base class)
  TStackTaskHypervised = class(TStackTask)
  protected
  public
    HypervisorBus : TBus;
    TaskDefinition : TGridHypervisorTaskDefinition;

    constructor Create; virtual;
    destructor destroy; override;

    procedure log( const Text : String;
                   const Module : String = '';
                   const Category : TGridLogCategory = TGridLogCategory.glcInfo;
                   const aForcedClassName : String = '');

    procedure notifyProgress(txt : string; const percentavailable : boolean= false; const percent : single = 0.0);
  end;

  //This one will have a ready to use input channel and bus related stuff.
  // override "ClientQuery" to serve.
  TStackTaskHypervisedIO = class(TStackTaskHypervised)
  protected
    energiser : TThread; //worker.
    messagePacket : PTBusEnvelop;
    reader : TBusClientReader;
    procedure doWork(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
  public
    procedure clientQuery; virtual; abstract;
    procedure execute(Worker : TThread); override;
  end;


  TCustomGridHypervisor = Class(TGRIDService)
  private
    FCPU : TgsCPUUsage;
    FDico : TBusClientDataRepo;

    //MicroService.
    FMicroService_ClientAskByTxt : TBusClientReader;
    FM : TBusClientReaderArray;
    FStrTxtOrder : TStringList;
    FReady : boolean;

    //Microservice list.
    FMicroServices : TObjectDictionary<String,TMicroServiceDefinition>;
    //Task (Microservice execution) List.
    FTasks :  TObjectDictionary<String,TGridHypervisorTaskDefinition>;

  protected
    FTimerScheduler : TGSTimerSchedulerThreadContainer;

    FThreadPool :TStackDynamicThreadPool; //that will execute internal and external business task.

    //ThreadPool :
    procedure OnTaskStart(Const aThreadIndex : UInt32; aIStackTask : TStackTask; TaskProcessTimeValue : UInt64);
    procedure OnTaskFinished(Const aThreadIndex : UInt32; aIStackTask : TStackTask; TaskProcessTimeValue : UInt64);

    //Timer and publication.
    Procedure InternalSystemHypervisorTimer(Sender : TObject);
    procedure PublishSecond;

    function GetServiceReady: boolean; override;

    //High level service usage : This one is "drivable" by text,
    //thought KissB, MQTT, event web or even bus (embedded app)  : Text order based and response via messaging.
    Procedure InternalSystemHypervisorMicroServiceAskByText(Sender : TBusSystem;aReader : TBusClientReader; Var Packet : TBusEnvelop);
    function InternalHypervisorlaunchProcess(task : TGridHypervisorTaskDefinition) : Boolean;
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

  //Task define internally (TTask) which implemnents a "service".
  //See : GS.GHS.HelloWorldService.pas for trivial exemple implementation.
  //for working, this service need to be declared and enabled in MicroService repo (For security reason)
  TStackTaskHypervisedClass = class of TStackTaskHypervised;
  Var GLB_InternalTasksClassList : Array Of TStackTaskHypervisedClass;
implementation

{ TCustomGridHypervisor }


procedure TCustomGridHypervisor.Execute;
begin
  FReady := true;
  while Not(MasterThread.Terminated) do
  begin
    if BusProcessMessages(FM)=0 then
      Sleep(CST_THREAD_COOLDOWN);
  end;
end;

procedure TCustomGridHypervisor.Finalize;
begin
  FTimerScheduler.Enabled := false;
  GridBus.UnSubscribe(FMicroService_ClientAskByTxt);
  FreeAndNil(FTimerScheduler);
  FreeAndNil(FThreadPool);
  FreeAndNil(FMicroService_ClientAskByTxt);
  FreeAndNil(FCPU);
  FreeAndNil(FDICO);
  FreeAndNil(FMicroServices);
  FreeAndNil(FStrTxtOrder);
  FreeAndNil(FTasks);
  inherited;
end;

function TCustomGridHypervisor.GetServiceReady: boolean;
begin
  result := FReady;
end;

procedure TCustomGridHypervisor.Initialize;
begin
  inherited;
  FReady := false;
  FCPU := TgsCPUUsage.Create;
  FTimerScheduler := TGSTimerSchedulerThreadContainer.Create;
  FThreadPool := TStackDynamicThreadPool.Create;
  FThreadPool.OnTaskStart := OnTaskStart;
  FThreadPool.OnTaskFinished := OnTaskFinished;
  FThreadPool.FreeTaskOnceProcessed := true;
  FThreadPool.Synchronized := false;

  FMicroService_ClientAskByTxt := GridBus.Subscribe(CST_PUBLIC_CHANNAME_HYPERVISOR_SERVICE_ASKBYTXT,InternalSystemHypervisorMicroServiceAskByText);
  FMicroService_ClientAskByTxt.Event := GridBus.GetNewEvent;
  FStrTxtOrder := TStringList.Create;

  //Use central timer for CPU Data.
  FTimerScheduler.OnTimer := InternalSystemHypervisorTimer;
  FTimerScheduler.Enabled := true;

  //For Posting CPU Data.
  FDico := TBusClientDataRepo.Create(Self.MasterThread.Bus,CST_BUSDATAREPO_SERVERINFO);

  setlength(FM,4);
  FM[0] := FGlobalInstructionChannel;
  FM[3] := FMicroService_ClientAskByTxt;

  FTasks :=  TObjectDictionary<String,TGridHypervisorTaskDefinition>.Create([doOwnsValues]);
  FMicroServices := TObjectDictionary<String,TMicroServiceDefinition>.Create([doOwnsValues]);
end;


procedure TCustomGridHypervisor.InternalSystemHypervisorTimer(Sender: TObject);
begin
  //Monitoring : Post data into repo for API querying.
  FCPU.Update;
  FDico.SetValue('GS.GRID.CPU.UsagePercent',FCPU.UsageCPUPercent);
  PublishSecond;
end;


procedure TCustomGridHypervisor.OnTaskStart(const aThreadIndex: UInt32;
  aIStackTask: TStackTask; TaskProcessTimeValue: UInt64);
begin
  Assert(aIStackTask is TStackTaskHypervised);
  TStackTaskHypervised(aIStackTask).TaskDefinition.TaskStartOn := now;
  TStackTaskHypervised(aIStackTask).TaskDefinition.TaskStatus := TGridHypervisorTaskDefinitionTaskStatus.starting;
  { TODO : notify Task launch on system.hv.taskStart }
end;

procedure TCustomGridHypervisor.OnTaskFinished(const aThreadIndex: UInt32;
  aIStackTask: TStackTask; TaskProcessTimeValue: UInt64);
begin
  Assert(aIStackTask is TStackTaskHypervised);
  TStackTaskHypervised(aIStackTask).TaskDefinition.TaskStatus := TGridHypervisorTaskDefinitionTaskStatus.finished;
  TStackTaskHypervised(aIStackTask).TaskDefinition.TaskProcessedTime := TaskProcessTimeValue;
  TStackTaskHypervised(aIStackTask).TaskDefinition.RunCount := TStackTaskHypervised(aIStackTask).TaskDefinition.RunCount + 1;
  { TODO : notify Task launch on system.hv.taskFinish }
end;

function TCustomGridHypervisor.InternalHypervisorlaunchProcess(task : TGridHypervisorTaskDefinition) : boolean;
var  l : TStackTaskHypervised;
     i,j : integer;
begin
  result := false;
  case task.MicroServiceDefinition.ServiceImplementation.ImplType of
    TMicroServiceImplementationType.internalThread :
    begin
      j := -1;
      for i := 0 to Length(GLB_InternalTasksClassList)-1 do
      begin
        if GLB_InternalTasksClassList[i].ClassName = task.MicroServiceDefinition.ServiceImplementation.address then
        begin
          j := i;
          break;
        end;
      end;

      if j>-1 then
      begin
        l := GLB_InternalTasksClassList[j].Create;
        l.TaskDefinition := task;
        l.HypervisorBus := GridBus;
        FThreadPool.Submit(l);
        result := true;
        { TODO : notify Task launch on system.hv.taskRun }
      end
      else
      begin
        task.TaskOK:= false;
        task.TaskKODesc := 'TaskReference not found';
      end;
    end;
    TMicroServiceImplementationType.externalBinary :
    begin
      raise Exception.Create('todo');
    end;
  end;
end;

procedure TCustomGridHypervisor.InternalSystemHypervisorMicroServiceAskByText(
  Sender: TBusSystem; aReader: TBusClientReader; var Packet: TBusEnvelop);
var order : String;
    respoChan : String;
    taskID : String;
    resp : TBusMessage;
    mm : TMicroServiceDefinition;
    i : integer;
    tp : String;
    enum : TPair<string,TMicroServiceDefinition>;
    enumt : TPair<string,TGridHypervisorTaskDefinition>;
    taskdef : TGridHypervisorTaskDefinition;
    ag : TGUID;

    procedure GenerateWarningMessage(warningtxt : string);
    begin
      FStrTxtOrder.Clear;
      FStrTxtOrder.Add('response=OK');
      FStrTxtOrder.Add('messagetype=warning');
      FStrTxtOrder.Add('messagedesc='+warningtxt);
      resp.FromString(FStrTxtOrder.Text);
      GridBus.Send(resp,respoChan);
    end;

    procedure generateExceptionMessage(exceptionTxt : string);
    begin
      FStrTxtOrder.Clear;
      FStrTxtOrder.Add('response=KO');
      FStrTxtOrder.Add('messagetype=exception');
      FStrTxtOrder.Add('messagedesc='+exceptionTxt);
      resp.FromString(FStrTxtOrder.Text);
      GridBus.Send(resp,respoChan);
    end;

    procedure generateOkMessage(const okMessage : string = '');
    begin
      FStrTxtOrder.Clear;
      FStrTxtOrder.Add('response=OK');
      if okMessage<>'' then
        FStrTxtOrder.Add('messagedesc='+okMessage);
      resp.FromString(FStrTxtOrder.Text);
      GridBus.Send(resp,respoChan);
    end;

begin

  try
    FStrTxtOrder.Text := Packet.ContentMessage.AsString;
    respoChan := FStrTxtOrder.Values['srto'];
    if respoChan='' then
      respoChan := Packet.ResponseChannel;
    order := FStrTxtOrder.Values['orde'];
    if respoChan<>'' then
    begin
      if order<>'' then
      begin
        if (order = 'sl') or (order='servicelist') then
        begin
          //available service list
          FStrTxtOrder.Clear;
          FStrTxtOrder.Add('response=OK');
          FStrTxtOrder.Add('servicecount='+IntToStr(FMicroServices.Count));
          i := 1;
          for enum in FMicroServices do
          begin
            tp := 'internal';
            if enum.Value.ServiceImplementation.ImplType <> TMicroServiceImplementationType.internalThread then
              tp := 'external';
            FStrTxtOrder.Add(format('service%d="%s","%s","%s","%s"',[i,enum.Value.Name,enum.Value.Description,enum.Value.ServiceImplementation.address,tp]));
            inc(i);
          end;
          resp.FromString(FStrTxtOrder.Text);
          GridBus.Send(resp,respoChan);
        end
        else
        if (order = 'st') or (order='status') then
        begin
          FStrTxtOrder.Clear;
          FStrTxtOrder.Add('response=OK');
          FStrTxtOrder.Add('taskcount='+IntToStr(FTasks.Count));
          i := 1;
          FStrTxtOrder.Add('messagedesc=taskNumber,name,taskID,Status,Ok,NOKDesc');
          for enumt in FTasks do
          begin
            FStrTxtOrder.Add(format('task%d="%s","%s","%s","%s","%s","%d"',[i,enumt.Value.MicroServiceDefinition.Name,enumt.Value.TaskID,intToStr(integer(enumt.Value.TaskStatus)),BoolToStr(enumt.Value.TaskOK),enumt.Value.TaskKODesc, enumt.Value.RunCount]));
            inc(i);
          end;
          resp.FromString(FStrTxtOrder.Text);
          GridBus.Send(resp,respoChan);
        end
        else
        if order='defservice' then
        begin
          //define microservice by code (usually done by file, on startup.)

          if FMicroServices.TryGetValue(FStrTxtOrder.Values['name'],mm) then
          begin
           GenerateWarningMessage('microservice '+mm.Name+' already defined');
          end
          else
          begin
            mm := TMicroServiceDefinition.Create;
            mm.Name := lowercase(trim(FStrTxtOrder.Values['name']));
            mm.Description := FStrTxtOrder.Values['descriptiondesc'];
            mm.ServiceImplementation.address := FStrTxtOrder.Values['address'];
            mm.ServiceImplementation.ImplType := TMicroServiceImplementationType.internalThread;
            if FStrTxtOrder.Values['implType'] <> 'internal' then
              mm.ServiceImplementation.ImplType := TMicroServiceImplementationType.externalBinary;
            if FStrTxtOrder.Values['runBehaviour'] <> 'onlyOnceRun' then
              mm.RunBehaviour := TMicroServiceRunBehaviour.Resident;
            FMicroServices.Add(mm.Name,mm);
            FStrTxtOrder.Clear;
            FStrTxtOrder.Add('response=OK');
            resp.FromString(FStrTxtOrder.Text);
            GridBus.Send(resp,respoChan);
          end;
        end
        else
        if order='run' then
        begin
        //no order, we are expecting a taskID, previsously generated by order query.
          taskID := FStrTxtOrder.Values['taskid'];
          if FTasks.TryGetValue(taskID,taskdef) then
          begin
            respoChan := taskdef.TaskHypervisorClientResponseID;
            if respoChan<>'' then
            begin
//              case taskdef.MicroServiceDefinition.RunBehaviour of
//                TMicroServiceRunBehaviour.onlyOnceRun:
                begin
                  if InternalHypervisorlaunchProcess(taskdef) then //takeoff.
                  begin
                    generateOkMessage(format('service %s has been launched',[taskdef.MicroServiceDefinition.Name]));
                  end
                  else
                  begin
                    generateExceptionMessage(format('service %s launch error : %s',[taskdef.MicroServiceDefinition.Name,taskdef.TaskKODesc]));
                  end;
                end;
//                TMicroServiceRunBehaviour.resident:
//                begin
//                  generateExceptionMessage('RESIDENT TODO !!');
//                end;
//              end;
            end;
          end
          else
          begin
            generateExceptionMessage(format('run task "%s" launch error : %s',[taskID,'taskID not provided']));
          end
        end
        else
        if (order = '.') or (order='hi') then
        begin
          generateOkMessage('welcome on Grid Hypervisor');
        end
        else
        begin
          if FMicroServices.TryGetValue(order,mm) then //No. Is already a MicrosServices
          begin
            //Open task definition : Next ask will launch task.
            taskdef := TGridHypervisorTaskDefinition.Create;
            taskDef.TaskID := 'TID'+GUIDToString(ag.NewGuid);
            taskdef.MicroServiceDefinition := mm;
            taskdef.TaskRegisterOn := now;
            taskdef.TaskStartOn := 0.0;
            taskdef.TaskFinisheOn := 0.0;
            taskdef.TaskFinished := false;
            taskdef.TaskOK := false;
            taskdef.TaskKODesc := '';
            taskdef.TaskHypervisorClientResponseID := respoChan; //link between client and its tasks
            taskdef.stdInChan := format('HVTI/%S',[taskDef.TaskID]);
            taskdef.stdOutChan := format('HVTO/%S',[taskDef.TaskID]);
            taskdef.RunCount := 0;

            FTasks.Add(taskdef.TaskID,taskdef);

            FStrTxtOrder.Clear;
            FStrTxtOrder.Add('response=OK');
            FStrTxtOrder.Add('taskid='+taskdef.TaskID);
            FStrTxtOrder.Add('stdinchan='+taskdef.stdInChan);
            FStrTxtOrder.Add('stdoutchan='+taskdef.stdOutChan);
            resp.FromString(FStrTxtOrder.Text);
            GridBus.Send(resp,respoChan);
          end
          else
          begin
            generateExceptionMessage(format('unknown service "%s".',[order]));
          end;
        end;
      end
      else
      begin
        generateExceptionMessage('empty order.');
      end;
    end
    else
    begin
      raise Exception.Create('Hypervisor dialog protocol issue');
    end;

  Except
    On E : Exception do
    begin
      Log(E.Message,ClassName,'InternalSystemHypervisorMicroServiceAskByText',TGridLogCategory.glcException);
    end;
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

{ TStackTaskHypervised }

constructor TStackTaskHypervised.Create;
begin
  //... must be declared, in order to work on injection (GLB_InternalTasksClassList).
end;

destructor TStackTaskHypervised.destroy;
begin
  //... must be declared, in order to work on injection (GLB_InternalTasksClassList).
  inherited;
end;

procedure TStackTaskHypervised.log( const Text : String;
                   const Module : String = '';
                   const Category : TGridLogCategory = TGridLogCategory.glcInfo;
                   const aForcedClassName : String = '');
var lLogMessage : TBusMessage;
    ls : TGRIDLogChunk;
    l : TMemoryStream;
begin
  if assigned(HypervisorBus) then
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
      HypervisorBus.Send(lLogMessage,CST_CHANNELNAME_LOGROOT_HT);
    finally
      FreeAndNil(l);
    end;
  end;
end;

procedure TStackTaskHypervised.notifyProgress(txt: string;
  const percentavailable: boolean; const percent: single);
begin
  { TODO : notify Task launch on system.hv.taskprogress }
  //mes.FromString(...)
  //yourBus.Send(mes,'system.hv.taskprogress');
end;

{ TStackTaskHypervisedIO }

procedure TStackTaskHypervisedIO.doWork(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  try
    //Working !
    MessagePacket := @Packet;
    Reader := aReader;
    clientQuery;
  except
    On E : Exception do
    begin
      Energiser.Terminate; //Exit.
    end;
  end;
end;

procedure TStackTaskHypervisedIO.execute(Worker: TThread);
var inChan : TBusClientReader;
begin
  inherited;
  Energiser := Worker;
  inchan := HypervisorBus.Subscribe(TaskDefinition.stdInChan,doWork);
  inChan.Event := TEvent.Create(nil,False,False,EmptyStr);

  //waiting for client.
  while Not(TVisibilityThread(Worker).Terminated) do
  begin
    if inChan.Event.WaitFor(CST_BUSTIMER) = wrSignaled then
      BusProcessMessages(inchan);
  end;
  HypervisorBus.UnSubscribe(inChan);
  inChan.Event.Free;
  FreeandNil(inChan);
end;

end.
