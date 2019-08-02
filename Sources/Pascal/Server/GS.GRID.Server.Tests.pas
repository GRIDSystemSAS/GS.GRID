unit GS.GRID.Server.Tests;

{$I GSCore.inc}

interface

Uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 SyncObjs,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 System.SyncObjs,
 {$ENDIF}
 GS.Threads,
 GS.Bus,
 GS.Bus.Services,
 GS.GRID.Server.Service.Types,
 GS.GRID.Common.Protocols,
 GS.GRID.Common.Protocols.Example,
 GS.GRID.Client,
 GS.GRID.Client.Example;


Type
///
///
///  Tests stuff.
///
TGSSTestItem_ServerClientConnect = Class(TCustomGridServiveServerTestItem)
public
  Function TitleOfItem : String; override;
  Function Execute : Boolean; override;
End;

TGSSTestItem_ServerClientOperation = Class(TCustomGridServiveServerTestItem)
public
  Function TitleOfItem : String; override;
  Function Execute : Boolean; override;
End;

/// Master test object.
TGSSTestResults_Server = class(TCustomGridServiveServerTestResult)
Public
  Function Title : String; Override;
  Constructor Create(aOwner : TCustomGRIDService); Override;
end;

implementation

{$IF DEFINED(DCC) AND DEFINED(MSWINDOWS)}
Uses GS.GRID.Client.Transport.NamedPipes;
{$ENDIF}

{ TGSSTestItem_ServerClientConnect }

function TGSSTestItem_ServerClientConnect.Execute: Boolean;
{$IF DEFINED(DCC) AND DEFINED(MSWINDOWS)}
var aClient : TGRIDTransportNamedPipe;
begin
  aClient := TGRIDTransportNamedPipe.Create;
  try
    aClient.Connect;
    result := aClient.Connected;
    aClient.Disconnect;
  finally
    FreeAndNil(aClient);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}
function TGSSTestItem_ServerClientConnect.TitleOfItem: String;
begin
  result := 'client connection';
end;

{ TGSSTestResults_Server }

constructor TGSSTestResults_Server.Create(aOwner : TCustomGRIDService);
begin
  inherited create(aOwner);
  Tests.Add(TGSSTestItem_ServerClientConnect.Create(Self));
  Tests.Add(TGSSTestItem_ServerClientOperation.Create(Self));
  //Tests[1].Enabled := false;
end;

function TGSSTestResults_Server.Title: String;
begin
  result := 'Server test ('+FOwner.ClassName+')';
end;


{ TGSSTestItem_ServerClientOperation }

function TGSSTestItem_ServerClientOperation.Execute: Boolean;
{$IF DEFINED(DCC) AND DEFINED(MSWINDOWS)}
var aClient : TGRIDClientExampleChat;
begin
  aClient := TGRIDClientExampleChat.Create(TGRIDTransportNamedPipe.Create);
  try
    try
      aClient.Connect('admin','admin','Hello');
      result := true;
    Except
      result := true;
    end;
  finally
    FreeAndNil(aClient);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

function TGSSTestItem_ServerClientOperation.TitleOfItem: String;
begin
  result := 'Various operation';
end;

end.
