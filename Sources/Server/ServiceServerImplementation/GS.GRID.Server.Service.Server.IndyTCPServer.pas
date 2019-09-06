unit GS.GRID.Server.Service.Server.IndyTCPServer;
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

uses SysUtils,
     Classes,
     SyncObjs,
     IdContext, IdTCPConnection, IdTCPClient, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer,
     IdYarn, IdThread, IdSocketHandle, IdGlobal, IdIOHandler, IdStack,
     GS.Bus,
     GS.Stream,
     GS.Bus.Services,
     GS.GRID.Server.Service.Types,
     GS.GRID.Server.Service.Server,
     GS.GRID.Server.Service.Server.BasedProtocols,
     GS.GRID.Common.Protocols;

const
  CST_CRLF           = #13#10;
  CST_IPFILTER_LIMIT = 1000;
  CST_CHECKDATAONSOURCE_DEFAULT_DELAY = 0; //ms.
  CST_MAX_BYTES_AMOUNT_ALLOWED_BEFORE_CONNECT = 1024*1024; //One meg. allowed for connection phase, before acrreditation.
  CST_MAX_BYTES_AMOUNT_ALLOWED_ONCE_ACREDITED = CST_MAX_BYTES_AMOUNT_ALLOWED_BEFORE_CONNECT * 1000; //Bytes allowed when accredited.
  CST_MAX_UPLOAD_LIMIT_REACHED = 'Max allowed upload reached.';

Type
TGRIDContext = class(TIdServerContext)
private
  FUser : TGRIDServerUser; //Pointer.
  Function GetServerUserReady : boolean;
public
  procedure CleanContext;
  Property ServerUserReady : boolean read GetServerUserReady;
  Property ServerUser : TGridServerUser read FUser Write FUser;
end;

TGRIDServiceIndyTCPServer = class(TGRIDServiceServerBasedProtocol)
protected
  FRunTimeClientCount : Uint32;
  FServer : TIdTCPServer;
  FDesiredPort: UInt32;

  Function IPFilter(const StrIP: string): Boolean;
  Function IPClient(aContext : TidContext) : String;
  Function GetContextFromConnection(aConnection : TidTCPConnection; var aContext : TGRIDContext) : Boolean;
public
  procedure Initialize; Override;
  procedure Finalize; Override;

  Procedure OnGlobalInstructionIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Override;
  Procedure OnServerLevelInstruction(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop); Override;


  Function GetUserFromDataObject(aData : TObject; var User : TGRIDServerUser) : Boolean; Override;
  procedure SetUserToDataObject(aData : TObject; User : TGRIDServerUser); Override;

  Function InstanciateUser(aData : TObject; aProtocolInstance :  TGRIDProtocol) : TGRIDServerUser; Override;

  Function ComposeErrorString(aData : TObject; anErrorTxt : string) : String; Override;

  Procedure GetIncomingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Override;
  Procedure SetOutgoingStream(aData : TObject; var aStream : TMemoryStream; const PrefixByteCount : Boolean = false); Override;


  //Called in init.
  Procedure LoadConfiguration; Override;

  Function ServerReady : Boolean; Override;

  function Stats : String; Override;

  procedure SecondTimerEvent; Override; //come form Hypervisor. call every second.


  //Event.
  procedure OnIdServerAfterBind(Sender: TObject);
  Procedure OnIdServerConnect(AContext: TIdContext);
  procedure OnIdServerDisconnect(AContext: TIdContext);
  procedure OnIdServerException(AContext: TIdContext; AException: Exception);
  procedure OnIdServerListenException(AThread: TIdListenerThread;  AException: Exception);
  procedure OnIdContextCreated(AContext: TIdContext);

  procedure OnIdclientConnectionWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);

  //****************************************************************************
  //****************************************************************************
  //****************************************************************************
  //****************************************************************************
  // Client entry point.
  Procedure OnIdServerExecute(AContext: TIdContext);
  //****************************************************************************
  //****************************************************************************
  //****************************************************************************
  //****************************************************************************

  Property DesiredPort : UInt32 read FDesiredPort;

  Function AutoTestImplementation : TCustomGridServiveServerTestResult; Override;

  Class Function GetDefaultImplementation : TGridService; Override;
end;


implementation

uses GS.GRID.Server.Tests,
     GS.GRID.Server.Tests.KissB,
     GS.GRID.Client.Transport.IndyTCP,
     GS.GRID.Common.Protocols.MQTT,
     GS.GRID.Server.Service.Server.Protocol.MQTT;

{ TGRIDServiceIndyTCPServer }
function TGRIDServiceIndyTCPServer.IPClient(aContext: TidContext): String;
begin
  Result := ' ['+AContext.Binding.PeerIP+':'+IntToStr(AContext.Binding.PeerPort)+'] Thread# '+IntTostr(CurrentThreadID);
end;

function TGRIDServiceIndyTCPServer.GetContextFromConnection(aConnection : TidTCPConnection; var aContext : TGRIDContext) : Boolean;
var lUser : TGRIDServerUser;
    L : TList;
    I: Integer;
begin
  //Note :
  // -> See if pass by optional context list manageg in idcontext could be more thread friendly. got a doubt :)
  Result := false;
  aContext:= nil;

  if UserFromDataB(aConnection,lUser) then
  begin
    //Method if user available : By TGridServer.Datax hack : Fast, Dangerous, but safe to use in Indy's event.
    aContext := TGRIDContext(lUser.DataObjectA);
    result := true;
  end
  else
  begin
    //Method 1, if no user yet initialized : Lock context. :( Slow if connection count great. )
    L := FServer.Contexts.LockList;
    try
      for I := 0 to L.Count-1 do
      begin
        if TGRIDContext(L[i]).Connection = aConnection then
        begin
          result := true;
          aContext := TGRIDContext(L[i]);
          Break;
        end;
      end;
    finally
      FServer.Contexts.UnlockList;
    end;
  end;
end;


function TGRIDServiceIndyTCPServer.IPFilter(const StrIP: string): Boolean;
var i, j: integer;
{$IFDEF FPC}
    list: Classes.TList;
{$ELSE}
    list: TList;
{$ENDIF}
begin
  result := false;
  if Not Assigned(Fserver) then
    Exit;

  j := 0;
  list := FServer.Contexts.LockList;
  try
    for i := 0 to list.Count-1 do
    begin
      if TIdContext(list[i]).Binding.PeerIP = StrIP then
        Inc(j);
    end;
    Result := j <= CST_IPFILTER_LIMIT;
  finally
    FServer.Contexts.UnlockList;
  end;
end;


procedure TGRIDServiceIndyTCPServer.LoadConfiguration;
var lFile : TStringList;
    lFileName : String;
    ls : string;
begin
  FDesiredPort := 60000;
  lFileName := ExtractFileDir(ParamStr(0))+'\GRIDServer.configuration';
  if FileExists(lFileName) then
  begin
    lFile := TStringList.Create;
    try
      lFile.LoadFromFile(lFileName);
      ls := lFile.Values['PORT'];
      if length(ls)>0 then
        FDesiredPort := StrToIntDef(ls,0);
    finally
      FreeAndNil(lFile);
    end;
    MasterThread.DoLog('GRID TCP Server configuration loaded.');
  end
  else
  begin
    MasterThread.DoLog('GRID TCP Server configuration not found.');
  end;
end;

procedure TGRIDServiceIndyTCPServer.Finalize;
begin
  FServer.StopListening;
  FreeAndNil(FServer);
  inherited;
end;

class function TGRIDServiceIndyTCPServer.GetDefaultImplementation: TGridService;
begin
  result := TGRIDServiceIndyTCPServer.Create;
end;

function TGRIDServiceIndyTCPServer.GetUserFromDataObject(aData: TObject;
  var User: TGRIDServerUser): Boolean;
begin
  Assert(aData is TGRIDContext);
  User := nil;
  result := false;
  if Assigned(TGRIDContext(aData).ServerUser) then
  begin
    result := true;
    User := TGRIDContext(aData).ServerUser;
  end;
end;

procedure TGRIDServiceIndyTCPServer.Initialize;
{$IFDEF FPC}
var a : TIdSocketHandle;
{$ENDIF}
begin
  inherited;
  FRunTimeClientCount := 0;
  FServer := TIdTCPServer.Create(nil);
  FServer.ContextClass := TGRIDContext;
  FServer.DefaultPort := DesiredPort;
  FServer.Bindings.Clear; //One binding only.
  {$IFDEF FPC} //Linux seems to not support same binding on 2 interface (ipv6 and ipv4)
  a := FServer.Bindings.Add;
  a.IPVersion := TIdIPVersion.Id_IPv4;
  {$ENDIF}
  FServer.OnExecute := OnIdServerExecute;
  FServer.OnAfterBind := OnIdServerAfterBind; //IN MAIN THREAD.
  FServer.OnConnect := OnIdServerConnect;
  FServer.OnDisconnect := OnIdServerDisconnect;
  FServer.OnException := OnIdServerException;
  FServer.OnListenException := OnIdServerListenException;
  FServer.OnContextCreated := OnIdContextCreated;
  FServer.Active := true;
end;


function TGRIDServiceIndyTCPServer.InstanciateUser(aData : TObject; aProtocolInstance: TGRIDProtocol): TGRIDServerUser;
var ui : String;
begin
  inc(FRunTimeClientCount);
  ui := Format('idTcp%d.%d', [NativeInt(aData),NativeInt(FRunTimeClientCount)]);
  Result := TGRIDServerUser.Create(ui, aProtocolInstance,TGRIDServerUserTransportLayer.net_tcp);
end;

function TGRIDServiceIndyTCPServer.AutoTestImplementation: TCustomGridServiveServerTestResult;
begin
  result := TGSSTestResults_Server.Create(Self);
//  result.Tests.Add(TGSSTestItem_ServerClientKissB.Create(result,TGRIDTransportIndyTCP.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_InfoFeature.Create(result,TGRIDTransportIndyTCP.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_CPUFeature.Create(result,TGRIDTransportIndyTCP.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_SendMessageBasic.Create(result,TGRIDTransportIndyTCP.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_SendMessageBasicStress.Create(result,TGRIDTransportIndyTCP.Create));
//  result.Tests.Add(TGSSTestItem_ServerClientKissB_SendMessageStress.Create(result,TGRIDTransportIndyTCP.Create));
  result.Tests.Add(TGSSTestItem_ServerClientKissB_SendMessageStressThreaded.Create(result,TGRIDTransportIndyTCP.Create));
end;

Function TGRIDServiceIndyTCPServer.ComposeErrorString(aData : TObject; anErrorTxt : string) : String;
var FContext : TidContext;
    Function IPClient : String;
    begin
      Result := ' ['+FContext.Binding.PeerIP+':'+IntToStr(FContext.Binding.PeerPort)+'] Thread# '+IntTostr(TThread.CurrentThread.ThreadID);
    end;
begin
  Assert(aData is TIdContext);
  FContext :=  TIdContext(aData);
  Result := 'Context = '+IntToStr(Integer(FContext))+')'+IPClient+' : '+anErrorTxt;
end;

procedure TGRIDServiceIndyTCPServer.OnGlobalInstructionIncoming(Sender: TBusSystem;
   aReader : TBusClientReader; var Packet: TBusEnvelop);
begin
  inherited;
  //Here Global instruction.
end;

procedure TGRIDServiceIndyTCPServer.OnIdclientConnectionWork(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
var aC : TGRIDContext;

  procedure internalWorkLog(aActivity : string);
  begin
    if ac.ServerUserReady then
      Log(aC.ServerUser.ClientId+'/'+aC.ServerUser.GlobalUserName+' '+aActivity+' '+IntToStr(AWorkCount),ClassName)
    else
      Log(IntToStr(NativeInt(aC))+' '+aActivity+' '+IntToStr(AWorkCount),ClassName);
  end;

begin
  //VGS : IMHO, it is a great affort : See to how optimize that.
  if GetContextFromConnection(TIdTCPConnection(aSender),aC) then
  begin
    case AWorkMode of
      wmRead:
      begin
        if aC.ServerUserReady then
          aC.ServerUser.DataBytesRecept := AWorkCount;
{ TODO -oVGS -cNiceToHave : Add this ligne above when we are able to have a "verbose" param }
//        internalWorkLog('READ');
      end;
      wmWrite:
      begin
        if aC.ServerUserReady then
          aC.ServerUser.DataBytesSent := AWorkCount;
{ TODO -oVGS -cNiceToHave : Add this ligne above when we are able to have a "verbose" param }
//        internalWorkLog('WRITE');
      end;
    end;
  end;
end;

procedure TGRIDServiceIndyTCPServer.OnIdContextCreated(AContext: TIdContext);
begin
  TGRIDContext(AContext).FUser := Nil;
end;

procedure TGRIDServiceIndyTCPServer.OnIdServerAfterBind(Sender: TObject);

function GetLocalAddressesPatch : String; //Equivalent in Indy produce memleak (The story of "iF the TStringList is nil)
var
  LList: TIdStackLocalAddressList;
  I: Integer;
  FLocalAddresses : TStringList;
begin
  Result := '';
  FLocalAddresses := TStringList.Create;
  try
    FLocalAddresses.BeginUpdate;
    LList := TIdStackLocalAddressList.Create;
    try
      // for backwards compatibility, return only IPv4 addresses
      GStack.GetLocalAddressList(LList);
      for I := 0 to LList.Count-1 do
      begin
//        if LList[I].IPVersion = Id_IPv4 then begin
          FLocalAddresses.Add(LList[I].IPAddress);
//        end;
      end;
    finally
      LList.Free;
    end;
    result := FLocalAddresses.Text;
  finally
    FreeAndNil(FlocalAddresses);
  end;
end;


var
   n:Integer;
   s : string;
begin
  Log(ClassName+' / OnIdServerAfterBind',ClassName);
  //This run in main thread.
  s := GetLocalAddressesPatch; // + #13#10;
  for n:= 0 to FServer.Bindings.Count-1 do
  begin
    with FServer.Bindings[n] do
    begin
      if (n=0) then
      begin
        FServer.DefaultPort := FServer.Bindings[n].Port;
      end;
      s := s + ( ip+':'+IntToStr(Port) ) + ' / ' + PeerIP + #13#10;
    end;
  end;
  Log('Server listening on '+s,ClassName);
end;

procedure TGRIDServiceIndyTCPServer.OnIdServerConnect(AContext: TIdContext);
begin
  Log('ServerConnect('+IntToStr(Integer(AContext))+')',ClassName);
  AContext.Connection.OnWork := OnIdclientConnectionWork;
end;


procedure TGRIDServiceIndyTCPServer.OnIdServerDisconnect(AContext: TIdContext);
begin
  TGRIDContext(AContext).CleanContext;
  Log('ServerDisconnect('+IntToStr(Integer(AContext))+')',ClassName);
end;

procedure TGRIDServiceIndyTCPServer.OnIdServerException(AContext: TIdContext;
  AException: Exception);
begin
  TGRIDContext(AContext).CleanContext;
  //{ TODO : Allow server Level configuration of Log Exception's Verbose option }
  Log('ServerException('+IntToStr(Integer(AContext))+'*********************',ClassName);
  Log('ServerException('+IntToStr(Integer(AContext))+') "'+AException.Message+'"',ClassName);

  {$IFDEF DCC}
  if Assigned(AException.BaseException) then
    Log('ServerException('+IntToStr(Integer(AContext))+') Base Exception "'+AException.BaseException.Message+'"',ClassName);
  Log('ServerException('+IntToStr(Integer(AContext))+') Call Stack "'+AException.StackTrace+'"',ClassName);
  {$ENDIF}
end;

procedure TGRIDServiceIndyTCPServer.OnIdServerExecute(AContext: TIdContext);
begin

  if not IPFilter(AContext.Binding.PeerIP) then
  begin
    raise Exception.Create('IPFilter check negative (>'+IntToStr(CST_IPFILTER_LIMIT)+' connections at a time)');
  end;
  ServerClientExecute(aContext);
{

On Indy exception are manager on this point of server. Nothing must be here as excepet management..
  try
    try
    Except
      On E : Exception do
      begin
        lError := ComposeErrorString(aContext,E.Message);
        Log(lError);
        raise E;
      end;
    end;
  finally
  end;
}

  // alternatively, if you call Disconnect(), make sure
  // the IOHandler's InputBuffer is empty, or else
  // AContext.Connection.Connected() will continue
  // returning True!...

  //Just for documentation
  //if not IPFilter(AContext.Binding.PeerIP) then
  //begin
  //  GenFatalError('IPFilter check negative (>'+IntToStr(CST_IPFILTER_LIMIT)+' connections at a time)');
  //end
end;


procedure TGRIDServiceIndyTCPServer.OnIdServerListenException(
  AThread: TIdListenerThread; AException: Exception);
begin
  Log('ServerListenException(Thread : '+IntToStr(Integer(AThread))+') "'+AException.Message+'"',ClassName);
end;

procedure TGRIDServiceIndyTCPServer.OnServerLevelInstruction(Sender: TBusSystem;
   aReader : TBusClientReader; var Packet: TBusEnvelop);
begin
  inherited;
  //Here server instruction.
end;


procedure TGRIDServiceIndyTCPServer.SecondTimerEvent;
begin
  TGRIDProtocolMQTT_ServerHandling.PublishStat(Self);
end;

function TGRIDServiceIndyTCPServer.ServerReady: Boolean;
begin
  result := false;
  if Assigned(FServer) then
    result := FServer.Active;
end;


procedure TGRIDServiceIndyTCPServer.SetUserToDataObject(aData: TObject;
  User: TGRIDServerUser);
begin
  Assert(aData is TGRIDContext);
  Assert(Not Assigned(TGRIDContext(aData).ServerUser));
  TGRIDContext(aData).ServerUser := User;
  User.DataObjectA := TGRIDContext(aData);
  User.DataObjectB := TGRIDContext(aData).Connection;
end;

function TGRIDServiceIndyTCPServer.Stats: String;
const cSep = ';';
      cNone = ' - ';
var L : TList;
    i : Integer;
    LL : TGRIDServerUser;
    c : TidContext;
    ls : TStringList;
    lls : String;

begin
  ls := TStringList.Create;
  L := FServer.Contexts.LockList;
  try
    ls.Add('Client ID'+ cSep +
           'UserName'+ cSep +
           'SessionID'+ cSep +
           'App. Space'+ cSep +
           'Accredited'+ cSep +
           'DataRecv'+ cSep +
           'DataSend'+ cSep +
           'Connected'+ cSep +
           'Conect. info');
    for i := 0 to L.Count-1 do
    begin
      c := TidContext(L[i]);
      lls := cNone;
      if c.Connection.Connected then
        lls := c.Binding.IP+':'+IntTostr(c.Binding.Port)+'/'+IntToStr(Integer(c.Connection.Socket.IPVersion));;

      if GetUserFromDataObject(c,LL) then
      begin
        ls.Add(LL.Stats(cSep)+ cSep + BoolToStr(c.Connection.Connected)+ cSep + lls);
      end
      else
      begin
        ls.Add( cNone+cSep+
                cNone+cSep+
                cNone+cSep+
                cNone+cSep+
                cNone+cSep+
                cNone+cSep+
                cNone+cSep+
                BoolToStr(c.Connection.Connected)+cSep+lls);
      end;
    end;
    result := ls.Text;
  finally
    FServer.Contexts.UnlockList;
    FreeAndNil(ls);
  end;
end;

procedure TGRIDServiceIndyTCPServer.SetOutgoingStream(aData: TObject;
  var aStream: TMemoryStream; const PrefixByteCount : Boolean = false);
begin
  Assert(aData is TIdContext);
  Assert(Assigned(aStream));
  aStream.Position := 0;
  if PrefixByteCount then
    TidContext(aData).Connection.IOHandler.Write(aStream,0, true)
  else
    TidContext(aData).Connection.IOHandler.Write(aStream,aStream.Size,false); //Send Without prefix.
end;

Procedure TGRIDServiceIndyTCPServer.GetIncomingStream(aData : TObject;
  var aStream: TMemoryStream; const PrefixByteCount : Boolean = false);
var lUser : TGRIDServerUser;
    lMem : TMemoryStream;
    lSize : Uint32;
    lSized : boolean;
begin
  //Documentation : Indy direct access socket.
  //https://stackoverflow.com/questions/9548244/tcp-server-hand-over-socket-connection/9548673
  Assert(aData is TIdContext);
  Assert(Assigned(aStream));

  if GetUserFromDataObject(aData,lUser) then
  begin
  end;

{
  AmountMax := CST_MAX_BYTES_AMOUNT_ALLOWED_BEFORE_CONNECT;
  if GetUserFromDataObject(aData,lUser) then
  begin
    //{ TODO : Per user quota definition OR Pass by parameter (better).
    //AmountMax := lUser.MaxBytesUpload; or something else.
    AmountMax := CST_MAX_BYTES_AMOUNT_ALLOWED_ONCE_ACREDITED;
  end;
}
  with TIdContext(aData).Connection.IOHandler do
  begin
    Repeat
      CheckForDataOnSource(CST_CHECKDATAONSOURCE_DEFAULT_DELAY);
      CheckForDisconnect(True);
      if Not(InputBufferIsEmpty) then
      begin
 //       Amount := Amount + InputBuffer.Size;
 //       if Amount>AmountMax then
 //       begin
 //         raise Exception.Create(CST_MAX_UPLOAD_LIMIT_REACHED);
 //       end;

        if PrefixByteCount then
          ReadStream(aStream)
        else
        begin
          ReadStream(aStream, InputBuffer.Size); //read the data detected on wire : Receive without prefix.

          if not Assigned(lUser) then
          begin
            //For this TCP client, it is the first data sent, since it has not user attached.

            //As the GRID TCP Server wait for "no size prefix" data on the first connection (After, it is protocol regulated),
            // we have to manage the data size, if the tcp client has sent a size prefix.
            if aStream.Size>SizeOf(UInt32) then
            begin
              aStream.Position := 0;
              lSized := false;

              //Raw UInt32 test.
              lSize := GS.Stream.ReadUInt32(aStream);
              lSized := lSize = aStream.Size;

              //Raw Int32 networked test.
              if not(lSized) then
              begin
                lSize := GStack.NetworkToHost(lSize);
                lSized := lSize = aStream.Size;
              end;

              //Question : Add Int64 test ? Uint ?
              if lSize = (aStream.Size-SizeOf(lSize)) then
              begin
                Log('WARNING : Detected "sized prefix" into incoming first TCP data packet : It has been fixed.',ClassName);
                aStream.Position := 4;
                lmem := TMemoryStream.Create;
                try
                  lMem.CopyFrom(aStream,aStream.Size - aStream.Position);
                  lMem.position := 0;
                  aStream.Clear;
                  aStream.LoadFromStream(lMem);
                finally
                  FreeAndNil(lmem);
                end;
              end;
            end;
          end;
        end;
      end;
    until InputBufferIsEmpty
  end;
end;


{ TGRIDContext }

procedure TGRIDContext.CleanContext;
begin
  if ServerUserReady then
  begin
    ServerUser.DataObjectA := nil;
    ServerUser.DataObjectB := nil;
    ServerUser := Nil;
  end;
end;

Function TGRIDContext.GetServerUserReady : boolean;
begin
  result := Assigned(FUser);
end;

Initialization

AddToServerServiceClasseRepo(TGRIDServiceIndyTCPServer);


end.
