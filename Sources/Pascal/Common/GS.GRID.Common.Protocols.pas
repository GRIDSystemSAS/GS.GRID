///This unit define "protocol" communication between client and server.
/// This protocol will be resolved on conection and they stay persitant for the client ctx.
/// there is in this file a basic protocol, text based, for example.
/// Normaly, protocol is deconnected from wire : We, on term, should commuicate on TCP/UDP/HTTP by JSON, Binary and whatever.
unit GS.GRID.Common.Protocols;

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

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  {$IFDEF USE_GENERIC}
  Generics.Collections, //Starting 3.1.1 only.
  {$ENDIF}
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  {$IFDEF USE_GENERIC}
  System.Generics.Collections,
  {$ENDIF}
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Common,
  GS.Stream;


Type
EExceptionProtocolNegotiation = Class(Exception);

TGRIDProtocolFormat = (Binary, json);
const
 CGRIDProtocolFormat : Array[0..1] of string =('Binary', 'json');

Type
TGRIDProtocol = class;
TGRIDProtocolClass = Class of TGRIDProtocol;


{$IFDEF USE_GENERIC}
TList_TGridProtocolClass = TList<TGridProtocolClass>;
{$ELSE}
TList_TGridProtocolClass = Class
Private
  FArray : Array of TGridProtocolClass;
  FIndex : UInt32;
  FInitialized : Boolean;

  function GetGridProtocolClass(Index: Uint32): TGridProtocolClass;
  function GetGridProtocolClassCount: Uint32;
  procedure SetTGridProtocolClass(Index: Uint32;
                const Value: TGridProtocolClass);
Public
  constructor Create; Virtual;
  Procedure Add(aGridProtocolClass : TGridProtocolClass);
  Procedure Clear;
  Property Items[Index : Uint32] : TGridProtocolClass read GetGridProtocolClass Write SetTGridProtocolClass; default;
  Property Count : Uint32 read GetGridProtocolClassCount;
end;
{$ENDIF}

///
///
///
TGRIDProtocol = class
protected
Public
  Constructor Create; virtual;
  class function Negociate(aStream : TStream) : Boolean; virtual; Abstract;
  function ProtocolName : String; virtual; abstract;
  function ProtocolNativeFormat : TGRIDProtocolFormat; Virtual; abstract;
  function ProtocolDescription : String; Virtual; abstract;
  function ProtocolNativeFormatAsString : string;
  function ProtocolNameSlashFormat : string;

  //AllowByteCountPrefix : Indicate if outbound (server) buffer must prefixed by buffer size.
  function AllowByteCountPrefix : Boolean; virtual;
  //AllowEmptyInputData : Usualy, the server will wait data from client to begin work on it. If
  //AllowEmptyInputData send True, the server will work on client, even if there *no* data input.
  //this let a chance to manage client data, and make further send, such as generate push system (see MQTT part).
  function AllowEmptyInputData : Boolean; virtual;
end;

{ TGridProtocolManager }
TGridProtocolManager = Class
private
protected
  FProtocolList : TList_TGridProtocolClass;
  function GetProtocolCount: Uint32;
  function GetProtocol(Index: UInt32): TGridProtocolClass;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;
  procedure Add(aProtocol : TGridProtocolClass);
  property ProtocolCount : Uint32 read GetProtocolCount;
  property Protocols[Index : UInt32] : TGridProtocolClass read GetProtocol;
End;

Var
  GridProtocolManager : TGridProtocolManager;


implementation

{ TGridServerProtocolManager }

procedure TGridProtocolManager.Add(aProtocol: TGridProtocolClass);
begin
  FProtocolList.Add(aProtocol);
end;

constructor TGridProtocolManager.Create;
begin
  FProtocolList := TList_TGridProtocolClass.Create;
end;

destructor TGridProtocolManager.Destroy;
begin
  FreeAndNil(FprotocolList);
  inherited;
end;

function TGridProtocolManager.GetProtocol(Index: UInt32): TGridProtocolClass;
begin
  result := FProtocolList[Index];
end;

function TGridProtocolManager.GetProtocolCount: Uint32;
begin
  result := FProtocolList.Count;
end;

{$IFNDEF USE_GENERIC}

{ TList_TGridProtocolClass }

procedure TList_TGridProtocolClass.Add(aGridProtocolClass: TGridProtocolClass);
begin
  if FInitialized then
  begin
    if FIndex = Length(FArray)-1 then
    begin
      SetLength(FArray,Length(FArray)*2);
    end;
    FArray[FIndex] := aGridProtocolClass;
  end
  else
  begin
    FInitialized := true;
    FIndex := 0;
    FArray[FIndex] := aGridProtocolClass;
  end;
  Inc(FIndex);
end;


procedure TList_TGridProtocolClass.Clear;
begin
  FArray := nil;
  SetLength(Farray,CST_ARRAY_INIT_QTE);
  FIndex := 0;
  FInitialized := False;
end;

constructor TList_TGridProtocolClass.Create;
begin
  Clear;
end;

function TList_TGridProtocolClass.GetGridProtocolClass(
  Index: Uint32): TGridProtocolClass;
begin
  result := FArray[Index];
end;

function TList_TGridProtocolClass.GetGridProtocolClassCount: Uint32;
begin
  result := FIndex;
end;

procedure TList_TGridProtocolClass.SetTGridProtocolClass(Index: Uint32;
  const Value: TGridProtocolClass);
begin
  FArray[Index] := Value;
end;

{$ENDIF}

{ TGRIDProtocol }

function TGRIDProtocol.AllowByteCountPrefix: Boolean;
begin
  Result := true;
end;

function TGRIDProtocol.AllowEmptyInputData: Boolean;
begin
  result := false;
end;

constructor TGRIDProtocol.Create;
begin
  //Need this to be called by inherited when called TProtocolClass.Create. Go figure.
end;

function TGRIDProtocol.ProtocolNameSlashFormat: string;
begin
  result := ProtocolName+'/'+ProtocolNativeFormatAsString;
end;

function TGRIDProtocol.ProtocolNativeFormatAsString: string;
begin
  case ProtocolNativeFormat of
   TGRIDProtocolFormat.Binary : result := 'binary';
   TGRIDProtocolFormat.json   : result := 'json';
  end;
end;

Initialization
  GridProtocolManager := TGridProtocolManager.Create;

Finalization

  FreeAndNil(GridProtocolManager);

end.
