unit GS.GRID.Server.Service.Types;
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
 GS.Bus,
 GS.Bus.Services,
 GS.Stream,
 GS.CPUUsage,
 GS.GRID.Client.Transport,
 GS.GRID.Common.Types;



Const
  CST_CHANNELNAME_SERVICEINSTRUCTION      = '.GRIDServer.System.GlbChan';
  CST_CHANNELNAME_SERVERLEVELINSTRUCTION  = '.GRIDServer.System.GlbChan.ServerDedicated';

  CST_CHANNELNAME_CNC_AUTH_GLOBAL         = '.GRIDServer.System.Auth';
  CST_CHANNELNAME_CNC_PYTHONCONFUPDATE    = '.GRIDServer.System.PythonConfUpdate';
  CST_CHANNELNAME_CNC_USERCONFUPDATE      = '.GRIDServer.System.UserConfUpdate';

  //Repo
  CST_BUSDATAREPO_SERVERINFO = 'ServerInfo';
  CST_BUSDATAREPO_KEY_PYTHONCONFIGURATION   = 'PythonConf.DataRepo';
  CST_BUSDATAREPO_INSTANTPYTHON = 'InstantPython.StdOut.Temp';

  //Hypervisor.
  CST_CHANNEL_NAME_TASK_INFO     =  'system.hypervisor.tasks.info';
  CST_CHANNEL_NAME_SECOND_SIGNAL =  'system.hypervisor.second';

  CST_DEFAULT_GLOBAL_USER_NAME = 'None';


Type
  TCustomGRIDService = Class;
  TCustomGridServiveServerTestResult = Class;

  TCustomGRIDBus = Class(GS.Bus.TBus)
  private
  protected
  public
    //HERE : MultiBus (x bus paralelized ?) -
    { TODO -oVGS : Multibus : X Bus threaded. }
  End;

  TGridBus = Class(TCustomGridBus)
  End;

  //Internal test result class.
  TCustomGridServiveServerTestItem = class
  private
    FEnabled: Boolean;
    FLastExecuteResult: Boolean;
    FLastExecuteFailureInfo: String;
    FLastExecuteDurationInSec: UInt64;
    FMonIndex : UInt32;
  protected
    FTestGroup: TCustomGridServiveServerTestResult;
    Function InternalExecute : Boolean; virtual; Abstract;
  public
    Constructor Create( aGridServiveServerTestResult : TCustomGridServiveServerTestResult); Reintroduce;
    Function TitleOfItem : String; Virtual; Abstract;
    Function Execute : Boolean; virtual;
    procedure SetFailureInfo(aText : String);

    property TestGroup : TCustomGridServiveServerTestResult read FTestGroup;
    property Enabled : Boolean read FEnabled Write FEnabled;
    property LastExecuteResult : Boolean read FLastExecuteResult;
    property LastExecuteFailureInfo : String read FLastExecuteFailureInfo;
    property LastExecuteDurationInSec : UInt64 read FLastExecuteDurationInSec;
  end;

  //Dedicated for testing protocol/Transport.
  TCustomGridServiveServerTestTransportItem = class(TCustomGridServiveServerTestItem)
  protected
    FTransport : TGridTransport;
  public
    Constructor Create( aGridServiveServerTestResult : TCustomGridServiveServerTestResult;
                        aTransport : TGridTransport); Reintroduce;
  end;

 {$IFDEF USE_GENERIC}
  TObjecTList_TCustomGridServiveServerTestItem = TObjecTList<TCustomGridServiveServerTestItem>;
 {$ELSE}
  TObjecTList_TCustomGridServiveServerTestItem = Class(TList_ObjectArray)
  private
    function GetItem(Index: Uint32): TCustomGridServiveServerTestItem;
    procedure SetItem(Index: Uint32;
      const Value: TCustomGridServiveServerTestItem);
  Public
    Constructor Create; Reintroduce;
    Procedure Add(aTestItem :TCustomGridServiveServerTestItem);
    Property Items[Index : Uint32] :  TCustomGridServiveServerTestItem read GetItem Write SetItem; default;
  End;
 {$ENDIF}

  TCustomGridServiveServerTestResult = class
  protected
    FOwner : TCustomGRIDService;
  public
    Tests : TObjecTList_TCustomGridServiveServerTestItem;
    Function Title : String; Virtual; Abstract;
    Constructor Create(aOwner : TCustomGRIDService); Reintroduce; Virtual;
    Destructor Destroy; Override;
  end;

  TCustomTestException = class(Exception)
  end;

  TCustomGRIDService = Class(TServiceTask)
  private
    FServer: TObject;
    function GetGlobalInstructionChannel: TBusClientReader;
  protected
    FServiceId: String;
    FGridBus: TGridBus; //Pointer;
    FGlobalInstructionChannel : TBusClientReader;
    FSharedEvent : TEvent; //Shared between all BusClientReader.

    function GetServiceReady: boolean; virtual;

    Procedure OnGlobalInstructionIncoming(Sender : TBusSystem;aReader : TBusClientReader; Var Packet : TBusEnvelop); Virtual;

    Procedure InternalExecute(Const Clients : Array of TBusClientReader);
  public
    Procedure Initialize; Override;
    Procedure Execute; Override;
    Procedure finalize; Override;

    Procedure LoadConfiguration; Virtual;

    Procedure Log( const Text : String;
                   const aClassName : string;
                   const Module : String = '';
                   const Category : TGridLogCategory = TGridLogCategory.glcInfo);


    Function AutoTestImplementation : TCustomGridServiveServerTestResult; Virtual;
    Procedure AutoTest; Virtual;
//TODO    Function AutoSanityCheck : TGridServiveServerSanityCheckResult; virtual; abstract;

    //Here is Business communication Bus
    //( Warning : <> from inherited MasterThread.Bus ! inherited MasterThread.Bus is the system Services bus.)
    Property GridBus : TGridBus read FGridBus Write FGridBus;
    Property Server : TObject read FServer write FServer;
    Property ServiceID : String read FServiceId Write FServiceId;
    property ServiceReady : boolean read GetServiceReady;

    Property GlobalInstructionChannel : TBusClientReader read GetGlobalInstructionChannel;


  End;

  TGRIDService = Class(TCustomGRIDService)
  public
    Class Function GetDefaultImplementation : TGridService; Virtual; Abstract;
  End;

implementation

uses GS.GRID.Server;

{ TCustomGRIDService }

procedure TCustomGRIDService.AutoTest;
var T : TCustomGridServiveServerTestResult;
    item : TCustomGridServiveServerTestItem;
    i : integer;
    lr : Boolean;
begin
  T := AutoTestImplementation;
  if Assigned(T) then
  begin
    Log('Autotest start',ClassName);
    Log(T.Title,ClassName);
    For i := 0 to T.Tests.Count-1 do
    begin
      lr := false;
      item := T.Tests[i];
      Log('  '+Item.TitleOfItem,ClassName);
      try
        if Item.Enabled then
          lr := Item.Execute;

        if Item.Enabled then
        begin
          if lr then
          begin
            Log('  '+Item.ClassName+'\'+Item.TitleOfItem+' : Result is TRUE - '+IntToStr(item.LastExecuteDurationInSec),ClassName);
          end
          else
          begin
            Log('  '+Item.ClassName+'\'+Item.TitleOfItem+' Result is FALSE (!) ['+Item.LastExecuteFailureInfo+']',ClassName);
          end;
        end
        else
        begin
            Log('  '+Item.ClassName+'\'+Item.TitleOfItem+' Test Is DISABLED',ClassName);
        end;
      Except
        On E : TCustomTestException do
        begin
          Log('  '+Item.ClassName+'\'+Item.TitleOfItem+' function raised exception (execute sanity check ?) : ['+E.Message+']',ClassName);
        end;
      end;
    end;
    FreeAndNil(T);
  end
  else
  begin
    Log('No Autotest for '+ClassName+' defined.',ClassName);
  end;
end;

function TCustomGRIDService.AutoTestImplementation: TCustomGridServiveServerTestResult;
begin
  Result := Nil;
  raise Exception.Create('Autotest should be implemented or override to answer nil.('+ClassName+')');
end;

procedure TCustomGRIDService.Execute;
begin
  InternalExecute([FGlobalInstructionChannel]);
end;

procedure TCustomGRIDService.finalize;
begin
  inherited;
  log('Service finalization',ClassName);
  FGridBus.UnSubscribe(FGlobalInstructionChannel);
  FreeAndNil(FGlobalInstructionChannel);
end;

function TCustomGRIDService.GetGlobalInstructionChannel: TBusClientReader;
begin
  Result := FGlobalInstructionChannel;
end;

function TCustomGRIDService.GetServiceReady: boolean;
begin
  result := false; //Must be override if used in your service.
end;

procedure TCustomGRIDService.Initialize;
begin
  inherited;
  log('Service initialization',ClassName);
  FServiceId := TGUID.NewGuid.ToString;
  FGlobalInstructionChannel := FGridBus.Subscribe(CST_CHANNELNAME_SERVICEINSTRUCTION, OnGlobalInstructionIncoming);
  FSharedEvent := FGridBus.GetNewEvent;
  FGlobalInstructionChannel.Event := FSharedEvent;
  LoadConfiguration;
end;

procedure TCustomGRIDService.InternalExecute(
  Const Clients: array of TBusClientReader);
begin
  while Not(MasterThread.Terminated) do
  begin
    try
      case GlobalInstructionChannel.Event.WaitFor(CST_CUSTOMSERVICEAWAITINGTIMEOUT) of
        wrSignaled :
        begin
          BusProcessMessages(Clients);
        end;
        wrTimeout :
        begin
          MasterThread.DoHeartBeat;
        end;
      end;
    Except
      On E : Exception do
      begin
        log(ClassName+' Internal Execute Loop Exception "'+E.Message+'"',ClassName);
      end;
    end;
  end;
end;

procedure TCustomGRIDService.LoadConfiguration;
begin
  MasterThread.DoLog('No configuration.');
end;

procedure TCustomGRIDService.Log( const Text : String;
                   const aClassName : string;
                   const Module : String = '';
                   const Category : TGridLogCategory = TGridLogCategory.glcInfo);
begin
  TGridServer(FServer).logServer(Text,Module,category,aClassName);
end;

procedure TCustomGRIDService.OnGlobalInstructionIncoming(Sender: TBusSystem; aReader : TBusClientReader;
  var Packet: TBusEnvelop);
begin
  MasterThread.DoLog(ClassName+' -  OnGlobalInstructionIncoming');
  //Here is come global instruction, as shutdown or whatever. Must be propagate to inherited.
  //Here todo : Virtual/abstract OnGlobalServiceInstruction.

  //Functions :
  //Information
  //  ID
  //  Name
  //  Commentary
  //  Internal structure and working.
  //
  //StopService.
  //ReloadConfiguration.
  //StartService.
  //ServiceHeartBeat
end;



{ TCustomGridServiveServerTestResult }

constructor TCustomGridServiveServerTestResult.Create(aOwner : TCustomGRIDService);
begin
  Assert(assigned(aOwner));
  FOwner := aOwner;
  Tests := TObjecTList_TCustomGridServiveServerTestItem.Create;
end;

destructor TCustomGridServiveServerTestResult.Destroy;
begin
  FreeAndNil(Tests);
  inherited;
end;


{ TCustomGridServiveServerTestItem }

constructor TCustomGridServiveServerTestItem.Create(
  aGridServiveServerTestResult: TCustomGridServiveServerTestResult);
begin
  inherited Create;
  Assert(assigned(aGridServiveServerTestResult));
  FTestGroup := aGridServiveServerTestResult;
  FEnabled := true;
  FMonIndex := gsNewMonitoring;
end;

function TCustomGridServiveServerTestItem.Execute: Boolean;
begin
  result := false;
  try
    FLastExecuteDurationInSec := 0;
    gsStartMonitoring(FMonIndex);
    try
      result := InternalExecute;
    finally
      FLastExecuteDurationInSec := gsStepMonitoring(FMonIndex);
    end;
    FLastExecuteResult := result;
    FLastExecuteFailureInfo := '';
    //In internalExecute, dev must use "SetInfo" to explain why it is false.
  Except
    ON E : Exception do
    begin
      FLastExecuteResult := false;
      FLastExecuteFailureInfo := E.Message;
    end;
  end;
end;

procedure TCustomGridServiveServerTestItem.SetFailureInfo(aText: String);
begin
  FLastExecuteFailureInfo := aText;
end;

{ TCustomGridServiveServerTestTransportItem }

constructor TCustomGridServiveServerTestTransportItem.Create(
  aGridServiveServerTestResult: TCustomGridServiveServerTestResult;
  aTransport: TGridTransport);
begin
  inherited Create(aGridServiveServerTestResult);
  Assert(Assigned(aTransport));
  FTransport := aTransport;
end;


{$IFNDEF USE_GENERIC}

{ TObjecTList_TCustomGridServiveServerTestItem }

procedure TObjecTList_TCustomGridServiveServerTestItem.Add(
  aTestItem: TCustomGridServiveServerTestItem);
begin
  ManagedAdd(aTestItem);
end;

constructor TObjecTList_TCustomGridServiveServerTestItem.Create;
begin
  Inherited Create(True);
end;

function TObjecTList_TCustomGridServiveServerTestItem.GetItem(
  Index: Uint32): TCustomGridServiveServerTestItem;
begin
  Result := TCustomGridServiveServerTestItem(Farray[Index]);
end;

procedure TObjecTList_TCustomGridServiveServerTestItem.SetItem(Index: Uint32;
  const Value: TCustomGridServiveServerTestItem);
begin
  ManagedSet(Index,Value);
end;

{$ENDIF}

end.
