unit GS.GRID.Server.Service.Server;
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
 GS.Threads,
 GS.Bus,
 GS.Bus.Services,
 GS.GRID.Server.Service.Types,
 GS.GRID.Common.Protocols;
const
CGRIDServerUserTransportLayer : array[0..6] of string = ('none', 'net_tcp', 'net_udp', 'net_http', 'net_ws', 'win_pipe', 'net_udp_nrl');

Type
TGRIDServerUserTransportLayer = (none, net_tcp, net_udp, net_http, net_ws, net_pipe, net_udp_nrl);
TCustomGRIDServiceServer = class;
TCustomGRIDServerUser = Class
private
    procedure SetDataRecept(const Value: Uint64);
    procedure SetDataSent(const Value: Uint64);
protected
  FClientId : String; //Given by server inplementation. Unique GRID wise.
  FGlobalUserName: String; //Given by server inplementation : UserName of real user. Just for info.
  FProtocol : TGRIDProtocol;
  FTransport: TGRIDServerUserTransportLayer;
  FSessionID: string;
  FDataA: TObject;
  FDataB: TObject;
  FDataBytesSent: Uint64;
  FDataBytesRecept: Uint64;
Public
  IsAccredited : Boolean;

  Constructor Create( const aClientID : string;
                      aProtocol : TGRIDProtocol;
                      Const aTransport : TGRIDServerUserTransportLayer = TGRIDServerUserTransportLayer.none); Reintroduce; virtual;
  Destructor Destroy; Override;

  Property ClientId : String read FClientID;
  Property Protocol : TGRIDProtocol read FProtocol;
  Property Transport : TGRIDServerUserTransportLayer read FTransport;

  //Data normally setting once, on access agreement.
  Property GlobalUserName : String read FGlobalUserName Write FGlobalUserName;
  Property SessionID : string read FSessionID Write FSessionID;
  Property DataBytesRecept : Uint64 read FDataBytesRecept Write SetDataRecept;
  Property DataBytesSent : Uint64 read FDataBytesSent Write SetDataSent;

  //User object, to work with associated implentation lib (such as Indy, where, for exampel, DataA could be context).
  Property DataObjectA : TObject read FDataA Write FDataA;
  Property DataObjectB : TObject read FDataB Write FDataB;
end;

//GRIDServerIUser is basicaly a connection. It keep real clientCNCid which is a "physical user"
//It could be many TGridServerUser attached to one clientCNC.
TGRIDServerUser = class(TCustomGRIDServerUser)
protected
  FClientID : String;
Public
  AppSpace : String;
  ChannelsReading : TBusClientReaderList; //Used if needed.

  Function Stats(const aSeparator : String = ',') : string;

  Constructor Create( const aClientID : string;
                      aProtocol : TGRIDProtocol;
                      Const aTransport : TGRIDServerUserTransportLayer = TGRIDServerUserTransportLayer.none); Override;
  Destructor Destroy; Override;

  property UserID : String read FClientID;
end;

{$IFDEF USE_GENERIC}
TObjectList_TGRIDServerUser = TObjectList<TGRIDServerUser>;
TList_TGRIDServerUser = TList<TGRIDServerUser>;
{$ELSE}
TList_TGRIDServerUser = Class(TList_ObjectArray)
private
  function GetItem(Index: Uint32): TGRIDServerUser;
  procedure SetItem(Index: Uint32; const Value: TGRIDServerUser);
public
  Procedure Add(aUser : TGRIDServerUser);
  Property Items[Index : Uint32] : TGRIDServerUser read GetItem Write SetItem; Default;
End;
TObjectList_TGRIDServerUser = Class(TList_TGRIDServerUser)
public
  Constructor Create; reintroduce;
end;
{$ENDIF}

TGSProtectedGridServerUserList = Class(TGSProtectedObject)
Public
  constructor Create; Reintroduce;
  Function Lock : TObjectList_TGRIDServerUser; Reintroduce;
end;

TGRIDServerUserList = class(TGSProtectedGridServerUserList)
public
  Procedure AddNewUser(const aUser : TGridServerUser);
end;

TCustomGRIDServiceServer = class(TGRIDService)
private
protected
  FUserList : TGridServerUserList;
  FHitCount: TGSProtectedInt64;
  FServerInstrutions : TBusClientReader;
  FServerSecondTimer : TBusClientReader;

  Procedure OnServerLevelInstruction(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Virtual;
  Procedure OnHypervisorTimerSecondMessage(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Virtual;

  function GetHitCount: UInt32; Virtual;

  //This methods must be inherited in server's implementation.
  //  InstanciateUser : This let the service to build a new user, and gather service's specific information.
  Function InstanciateUser(aData : TObject; aProtocolInstance :  TGRIDProtocol) : TGRIDServerUser; Virtual; abstract;
  //  GetIncomingStream : Implement how the service communicate stream. How to get stream from client.
  //                        PrefixByteCount indicate if the client put a "byteCount" as a prefix to inicate the stream bytes amout.
  Procedure GetIncomingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Virtual; Abstract;
  //  SetIncomingStream : Idem, but how to send the stream to the client.
  Procedure SetOutgoingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Virtual; Abstract;
  //  Error Service's specific composition.
  Function ComposeErrorString(aData : TObject; anErrorTxt : string) : String; Virtual; Abstract;


  //Protocol management : this call inherited negociate, to associate a protocol to a client.
  Function GetProtocol(aStream : TMemoryStream) : TGRIDProtocolClass;
  //Protocol management : If GetProtocol is a success, Protocol_ServerProcess is called :
  // This one will manage all the stuff, in regards of the specified protocol.
  // This is the *Client* entry point.
  function Protocol_ServerProcess( aUser : TGRIDServerUser;
                                   aInStream : TMemoryStream;
                                   aOutStream : TMemoryStream) : Boolean; virtual;

  //Warning : This one could be executed in another thread context (depending of network impl.)
  Procedure ServerClientExecute(aData : TObject); Virtual;

  function DefaultServerAllowPrefixByteCount : Boolean; virtual;

public
  procedure Initialize; Override;
  Procedure Execute; Override;
  procedure Finalize; Override;

  Function ServerReady : Boolean; Virtual;

  Procedure SecondTimerEvent; Virtual;
  function Stats : String; virtual;

  Property HitCount : UInt32 read GetHitCount;

  //All thierd party network (or whatever) lib manage user by an proprietary object.
  //this 2 method let you make "link" between GridServerUser and main data object.
  Function GetUserFromDataObject(aData : TObject; var User : TGRIDServerUser) : Boolean; virtual; abstract;
  procedure SetUserToDataObject(aData : TObject; User : TGRIDServerUser); virtual; abstract;


  Function UserFromClientId(aClientID : String; Var FoundUser : TGRIDServerUser) : boolean;

  //Use field Data of TGridServerUser to match a User : Use carrefully, this is just pointer.
  Function UserFromDataA(aDataObject : TObject; var foundUser : TGridServerUser) : Boolean;
  Function UserFromDataB(aDataObject : TObject; var foundUser : TGridServerUser) : Boolean;
end;

TGridServiceServer = Class(TCustomGRIDServiceServer)
public
  class function GetDefaultImplementation: TGridService; Override;
End;

TGridServerServiceClass = class of TGridServiceServer;

Var GLB_ServerService_ImplClasses : Array of TGridServerServiceClass;

Procedure AddToServerServiceClasseRepo(aGridServerService : TGridServerServiceClass);

implementation

Procedure AddToServerServiceClasseRepo(aGridServerService : TGridServerServiceClass);
begin
  SetLength(GLB_ServerService_ImplClasses,Length(GLB_ServerService_ImplClasses)+1);
  GLB_ServerService_ImplClasses[Length(GLB_ServerService_ImplClasses)-1] := aGridServerService;
end;


{ TCustomGRIDServiceServer }

function TCustomGRIDServiceServer.DefaultServerAllowPrefixByteCount: Boolean;
begin
  result := false;
end;

procedure TCustomGRIDServiceServer.Execute;
begin
  InternalExecute([FGlobalInstructionChannel, FServerInstrutions, FServerSecondTimer]);
end;

procedure TCustomGRIDServiceServer.Finalize;
begin
  FGridBus.UnSubscribe(FServerInstrutions);
  FreeAndNil(FUserList);
  FreeAndNil(FHitCount);
  FreeAndNil(FServerInstrutions);
  FreeAndNil(FServerSecondTimer);
  inherited;
end;

function TCustomGRIDServiceServer.GetHitCount: UInt32;
begin
  result := FHitCount.Value;
end;

function TCustomGRIDServiceServer.GetProtocol(
  aStream: TMemoryStream): TGRIDProtocolClass;
var i : integer;
begin
  result := nil;
  for i := 0 to GridProtocolManager.ProtocolCount-1 do  //Readonly list.
  begin
    aStream.Position := 0;
    try
      if GridProtocolManager.Protocols[i].Negociate(aStream) then
      begin
        result := GridProtocolManager.Protocols[i];
      end;
      if Assigned(result) then
        Break;
    Except
      //We have to test here all protocol : Exception is not valuable. Even in log.
    end;
  end;
end;

procedure TCustomGRIDServiceServer.Initialize;
begin
  inherited;
  FHitCount:= TGSProtectedInt64.Create(0);
  FUserList := TGRIDServerUserList.Create;
  FServerInstrutions := FGridBus.Subscribe(CST_CHANNELNAME_SERVICEINSTRUCTION,OnServerLevelInstruction);
  FServerInstrutions.Event := FGridBus.GetNewEvent;
  FServerSecondTimer := FGridBus.Subscribe(CST_CHANNEL_NAME_SECOND_SIGNAL,OnHypervisorTimerSecondMessage);
  FServerSecondTimer.Event := FGridBus.GetNewEvent;
end;


procedure TCustomGRIDServiceServer.OnHypervisorTimerSecondMessage(
  Sender: TBusSystem; aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  SecondTimerEvent;
end;

procedure TCustomGRIDServiceServer.OnServerLevelInstruction(Sender: TBusSystem; aReader : TBusClientReader;
  var Packet: TBusEnvelop);
begin
  MasterThread.DoLog(ClassName+' -  OnServerLevelInstruction');
  //Here server dedicated instruction, such as "Globaldiscinnect, or "emergency disconnect" or whatever command level.

  //Functions :
  //  StopServer
  //  SetServerParameter (Json)
  //  StartServer
  //  ClientList
  //  UserList
end;


function TCustomGRIDServiceServer.Protocol_ServerProcess( aUser : TGRIDServerUser;
                                                          aInStream, aOutStream: TMemoryStream): Boolean;
begin
  result := false;
  //Must be overriden for take care of protocol implementation (See Example protocol)
end;

procedure TCustomGRIDServiceServer.SecondTimerEvent;
begin
  //Override me to get a timer !
end;

procedure TCustomGRIDServiceServer.ServerClientExecute(aData : TObject);
var lLocalStreamIn, lLocalStreamOut : TMemoryStream;
    lUser : TGRIDServerUser;
    lProtoClass : TGRIDProtocolClass;
    lProtocol :  TGRIDProtocol;
    lAllowEmptyInputData : boolean;
    lPrefixByteCount : boolean;
begin
  FHitCount.Inc;
  lUser := nil;
  lLocalStreamIn := TMemoryStream.Create;
  lLocalStreamOut := TMemoryStream.Create;
  lAllowEmptyInputData :=false;
  lPrefixByteCount := DefaultServerAllowPrefixByteCount;
  try
    //Resolve user. (mandatory in all case).

    if GetUserFromDataObject(aData,lUser) then
    begin
      //user resolved, refresh protocol parameter AllowPushDatadata.
      lAllowEmptyInputData := lUser.Protocol.AllowEmptyInputData;
      lPrefixByteCount := lUser.Protocol.AllowByteCountPrefix;
    end;

    GetIncomingStream(aData,lLocalStreamIn,lPrefixByteCount);

    if lLocalStreamIn.Size = 0 then
    begin
      //let go further only if protole allow push methods (and so, client yet defined :
      // --> this forbid "first connection with no data."
      if not(lAllowEmptyInputData) then
        exit;
    end;

    if Not Assigned(lUser) then
    begin
      //There are data, but not user for now : Resolve protocol and Create it.
      log('NEW client : Negotiating protocol...',ClassName);
      //Unknown yet : Determine protocol compatibility
      lProtoClass := GetProtocol(lLocalStreamIn);
      if not(Assigned(lProtoClass)) then
        raise Exception.Create('Protocol deal failed - Abort');

      //Built it.
      lProtocol := lProtoClass.Create;
      log('NEW client elligible to "'+lProtocol.ClassName+'" protocol.',ClassName);

      luser := InstanciateUser(aData, lprotocol);
      FUserList.AddNewUser(lUser);
      if Not(Assigned(lUser)) then
        raise Exception.Create('NEW client : Instanciation failed.');

      SetUserToDataObject(aData, Luser);
    end;

    //All data gathered, create user entry locally, via inherited class.
    if Protocol_ServerProcess(lUser, lLocalStreamIn, lLocalStreamOut) then
    begin
      if lLocalStreamOut.Size>0 then
      begin
        lLocalStreamOut.Position := 0;
        SetOutgoingStream(aData,lLocalStreamOut,lUser.Protocol.AllowByteCountPrefix);
      end;
    end
    else
    begin
      raise Exception.Create('Protocol_ServerProcess failed');
    end;
  finally
    FreeAndNil(lLocalStreamIn);
    FreeAndNil(lLocalStreamOut);
  end;
end;


function TCustomGRIDServiceServer.ServerReady: Boolean;
begin
  result := false;
end;

function TCustomGRIDServiceServer.Stats: String;
begin
  result := '-';
end;

function TCustomGRIDServiceServer.UserFromClientId(aClientID: String;
  var FoundUser: TGRIDServerUser): boolean;
var i : integer;
    luser : TList_TGRIDServerUser;
begin
  result := False;
  luser := FUserList.Lock;
  try
    for i := 0 to lUser.Count-1 do
    begin
      if luser[i].ClientId = aClientID then
      begin
        FoundUser := luser[i];
        Result := true;
      end;
    end;
  finally
    FUserList.Unlock;
  end;
end;


function TCustomGRIDServiceServer.UserFromDataA(aDataObject: TObject;
  var foundUser: TGridServerUser): Boolean;
var i : integer;
    luser : TList_TGRIDServerUser;
begin
  result := False;
  luser := FUserList.Lock;
  try
    for i := 0 to lUser.Count-1 do
    begin
      if luser[i].DataObjectA = aDataObject then
      begin
        FoundUser := luser[i];
        Result := true;
      end;
    end;
  finally
    FUserList.Unlock;
  end;
end;

function TCustomGRIDServiceServer.UserFromDataB(aDataObject: TObject;
  var foundUser: TGridServerUser): Boolean;
var i : integer;
    luser : TList_TGRIDServerUser;
begin
  result := False;
  luser := FUserList.Lock;
  try
    for i := 0 to lUser.Count-1 do
    begin
      if luser[i].DataObjectB = aDataObject then
      begin
        FoundUser := luser[i];
        Result := true;
      end;
    end;
  finally
    FUserList.Unlock;
  end;
end;

{ TGRIDServerUserList }

procedure TGRIDServerUserList.AddNewUser(const aUser : TGridServerUser);
begin
  Assert(assigned(aUser));
  Assert(assigned(aUser.Protocol));
  Assert(aUser.ClientId<>'');
  try
    Lock.Add(aUser);
  finally
    Unlock;
  end;
end;

{ TCustomGRIDServerUser }

constructor TCustomGRIDServerUser.Create(const aClientID : string;
  aProtocol: TGRIDProtocol; const aTransport: TGRIDServerUserTransportLayer);
begin
  Assert(Length(aClientID)>0);
  Assert(Assigned(aProtocol));
  FClientId := aClientID;
  FGlobalUserName := CST_DEFAULT_GLOBAL_USER_NAME;
  FProtocol := aProtocol;
  FTransport := aTransport;
  FSessionID := '';
  FDataA := nil;
  FDataB := nil;
  FDataBytesSent := 0;
  FDataBytesRecept := 0;
end;

destructor TCustomGRIDServerUser.Destroy;
begin
  if Assigned(FProtocol) then
    FreeAndNil(FProtocol);
  inherited;
end;


procedure TCustomGRIDServerUser.SetDataRecept(const Value: Uint64);
begin
  FDataBytesRecept := FDataBytesRecept + Value;
end;

procedure TCustomGRIDServerUser.SetDataSent(const Value: Uint64);
begin
  FDataBytesSent := FDataBytesSent + Value;
end;

{ TGridServiceServer }

class function TGridServiceServer.GetDefaultImplementation: TGridService;
begin
  raise Exception.Create(ClassName+' - this server is registered via global array, please use it instead.');
end;

{ TGSProtectedGridServerUserList }

constructor TGSProtectedGridServerUserList.Create;
begin
  Inherited Create(TObjectList_TGRIDServerUser.Create);
end;

function TGSProtectedGridServerUserList.Lock: TObjectList_TGRIDServerUser;
begin
  result := TObjectList_TGRIDServerUser(Inherited Lock);
end;

{$IFNDEF USE_GENERIC}


{ TList_TGRIDServerUser }

procedure TList_TGRIDServerUser.Add(aUser: TGRIDServerUser);
begin
  ManagedAdd(aUser);
end;

function TList_TGRIDServerUser.GetItem(Index: Uint32): TGRIDServerUser;
begin
  Result := TGRIDServerUser(FArray[Index]);
end;

procedure TList_TGRIDServerUser.SetItem(Index: Uint32;
  const Value: TGRIDServerUser);
begin
  ManagedSet(Index,Value);
end;

{ TObjectList_TGRIDServerUser }

constructor TObjectList_TGRIDServerUser.Create;
begin
  Inherited Create(true);
end;

{$ENDIF}

{ TGRIDServerUser }

constructor TGRIDServerUser.Create( const aClientID : string;
                                    aProtocol: TGRIDProtocol;
                                    const aTransport: TGRIDServerUserTransportLayer);
begin
  inherited;
  Assert(Assigned(aProtocol));
  FClientID := aClientID;
  ChannelsReading := TBusClientReaderList.Create; //Used if needed.
  AppSpace := ''; //No appspace by default.
end;

destructor TGRIDServerUser.Destroy;
var i : Integer;
begin
  ChannelsReading.Lock;
  for I := 0 to ChannelsReading.GetLockedList.Count-1 do
    ChannelsReading.GetLockedList[i].Free;
  FreeAndNil(ChannelsReading);
  inherited;
end;

function TGRIDServerUser.Stats(const aSeparator : String = ',') : string;
begin
result := ClientId + aSeparator +
                GlobalUserName + aSeparator +
                SessionID + aSeparator +
                AppSpace + aSeparator +
                Protocol.ProtocolName+'/'+
                CGRIDProtocolFormat[Integer(Protocol.ProtocolNativeFormat)] + aSeparator +
                CGRIDServerUserTransportLayer[integer(Transport)]+ aSeparator +
                BoolToStr(IsAccredited,true)+ aSeparator +
                IntToStr(DataBytesRecept)+ aSeparator +
                IntToStr(DataBytesSent);
end;

end.
