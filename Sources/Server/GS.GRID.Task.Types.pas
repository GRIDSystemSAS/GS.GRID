unit GS.GRID.Task.Types;

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
 GS.Threads.Pool,
 GS.TimerScheduler,
 GS.GRID.Server.Service.Types;

Type

  THypervisedTask = Class(TStackTaskProc)
  protected
    FResultingChannel : UTF8String;
    FHypervisorControlChannel : UTF8String;
    FOwner : TCustomGRIDService;
  public
    //If aOwner is an hypervisor, it will gather information on task to it.
    //"ResultingChannel" coulb be given by the end client :
    //It allow the task to send information directly to the end client without passby central services.
    Constructor Create(aOwner : TCustomGRIDService; ResultingChannel : UTF8String); Reintroduce;
  End;


implementation

Uses GS.GRID.Server.Service.Hypervisor;

{ THypervisedTask }

constructor THypervisedTask.Create(aOwner: TCustomGRIDService;
  ResultingChannel: UTF8String);
begin
//  Inherited; Pure abstract (FPC not handle it.)
  Assert(Length(ResultingChannel)>0);
  Assert(Assigned(aOwner));
  FResultingChannel := ResultingChannel;
  FOwner := aOwner;
  FHypervisorControlChannel := '';

  if FOwner is TGridHypervisor then
  begin
    //On this channel, the thread emit data "log" (Typicaly, command line output).
    FHypervisorControlChannel := CST_CHANNEL_NAME_TASK_INFO; //TGridHypervisor(FOwner).ControlTaskChannel.ChannelListening;
  end;
end;

end.
