unit GS.GRID.Task.Python;

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
 GS.Common,
 GS.Bus,
 GS.Threads.Pool;

Type
  TPythonTask = Class(THypervisedTask)
  protected
    FCode : TStringList;
  public
    Procedure Execute; Override;
  End;




implementation

{ TPythonTask }

procedure TPythonTask.Execute;
begin
  FCode.Text := trim(Fcode.Text);
  if Length(FCode.Text)=0 then
    Exit;
end;

end.
