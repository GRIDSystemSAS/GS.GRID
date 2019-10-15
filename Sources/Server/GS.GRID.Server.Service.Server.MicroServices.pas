unit GS.GRID.Server.Service.Server.MicroServices;
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
 GS.Common,
 GS.Threads,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server,
 GS.GRID.Common.Protocols.MicroService;

Type
  TMicroService = class
  private
    FServiceInfo: TMsg_FromService_ServiceData;
    FAvailable: Boolean;
    FRegisterCount: Uint32;
    FUpDateTime: TDateTime;
    FUserID: String;
    FChannelExecute: String;
  public
    Constructor Create(aOwnerUserId : String); Virtual;

    Procedure Registered;

    property ServiceInformations : TMsg_FromService_ServiceData read FServiceInfo Write FServiceInfo;
    Property Available : Boolean read FAvailable;
    Property UpDateTime : TDateTime read FUpDateTime;
    property RegisterCount : Uint32 read FRegisterCount;
    property UserIdOwner : String read FUserID;
    property ChannelExecute : String read FChannelExecute Write FChannelExecute;
  end;

{$IFDEF USE_GENERIC}
TObjectList_TMicroService = TObjectList<TMicroService>;
TList_TMicroService = TList<TMicroService>;
{$ELSE}
TList_TMicroService = Class(TList_ObjectArray)
private
  function GetItem(Index: Uint32): TMicroService;
  procedure SetItem(Index: Uint32; const Value: TMicroService);
public
  Procedure Add(aMicroService : TMicroService);
  Property Items[Index : Uint32] : TMicroService read GetItem Write SetItem; Default;
End;

TObjectList_TMicroService = Class(TList_TMicroService)
public
  Constructor Create; reintroduce;
  function MicroServiceById(ItemId : String; var aMicroService : TMicroService) : Boolean;
end;
{$ENDIF}

TGSProtectedGridServerMicroServiceList = Class(TGSProtectedObject)
Public
  constructor Create; Reintroduce;
  Function Lock : TObjectList_TMicroService; Reintroduce;
end;

implementation

{$IFNDEF USE_GENERIC}


{ TList_TMicroService }

procedure TList_TMicroService.Add(aMicroService: TMicroService);
begin
  ManagedAdd(aMicroService);
end;

function TList_TMicroService.GetItem(Index: Uint32): TMicroService;
begin
  Result := TMicroService(FArray[Index]);
end;

procedure TList_TMicroService.SetItem(Index: Uint32;
  const Value: TMicroService);
begin
  ManagedSet(Index,Value);
end;

{ TObjectList_TMicroService }

constructor TObjectList_TMicroService.Create;
begin
  Inherited Create(true);
end;

{$ENDIF}


function TObjectList_TMicroService.MicroServiceById(ItemId: String;
  var aMicroService: TMicroService): Boolean;
  var i : integer;
begin
  result := false;
  aMicroService := nil;
  for i := 0 to Count -1 do
  begin
    if ItemId = Items[i].ServiceInformations.ID then
    begin
      aMicroService := Items[i];
      result := true;
      Break;
    end;
  end;
end;

{ TGSProtectedGridServerMicroServiceList }

constructor TGSProtectedGridServerMicroServiceList.Create;
begin
  inherited Create(TObjectList_TMicroService.Create);
end;

function TGSProtectedGridServerMicroServiceList.Lock: TObjectList_TMicroService;
begin
  result := TObjectList_TMicroService(Inherited Lock);
end;

{ TMicroService }

constructor TMicroService.Create(aOwnerUserId: String);
begin
  Inherited Create;
  assert(trim(lowerCase(aOwnerUserId))<>'');
  FUserID := aOwnerUserId;
end;

procedure TMicroService.Registered;
begin
  Inc(FRegisterCount);
  FAvailable := true;
  FUpDateTime := Now;
end;

end.
