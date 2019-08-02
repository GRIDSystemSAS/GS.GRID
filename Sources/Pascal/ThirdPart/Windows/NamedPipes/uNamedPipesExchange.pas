unit uNamedPipesExchange;
/// <summary>
/// wrappers for FWIOCompletionPipes (by Rouse)
///  Allows send and receive data with MaxSize = High(integer)
///  instead of source classes with max packet size = MAXWORD
///
///  Client works in blocking mode, i.e. after excecuting SendData method
///  you receive ALL data in ReceiveStream parameter
///
///  Server works in blocking mode, i.e. after you set Server.Active:=True
///  server entered in loop and exit from from it to next line of your code
///  only after you set Active:=False
///  For this (deactivate server) use any server event, for example - OnIdle or OnReadFromPipe
///  OnReadFromPipe fires only AFTER ALL data received from client.
///
///  You can use this code without any limitations.
/// </summary>

/// <author>kami, 2016. kami-soft@yandex.ru, https://github.com/kami-soft</author>
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 Generics.Collections,
 {$ELSE}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
 {$ENDIF}
  FWIOCompletionPipes;

type
  TPipeCommand = (pcUnknown, pcNewData, pcNextDataPart);

  TPipeDataHeader = record
    Command: TPipeCommand;
    DataSize: Int64;

    procedure ReadFromBuf(const Buf: TBytes);
    procedure WriteToBuf(var Buf: TBytes);
  end;

  TConnectEvent = procedure(Sender: TObject; PipeHandle: THandle) of object;
  TExchangeEvent = procedure(Sender: TObject; PipeHandle: THandle; IncommingValue: TStream; OutgoingValue: TStream)
    of object;

  TAbstractPipeData = class(TObject)
  strict private
    FDataStream: TStream;
  strict protected
    FHeader: TPipeDataHeader;
  public
    constructor Create;
    destructor Destroy; override;

    property Header: TPipeDataHeader read FHeader;
    property DataStream: TStream read FDataStream;
  end;

  TReceivingPipeData = class(TAbstractPipeData)
  strict private
    function GetIsReceived: Boolean;
  public
    procedure WriteData(const Buf: TBytes);

    property IsReceived: Boolean read GetIsReceived;
  end;

  TSendingPipeData = class(TAbstractPipeData)
  strict private
    FIsHeaderSended: Boolean;
    function GetIsSended: Boolean;
  public
    procedure PrepareForSend(AHeaderCommand: TPipeCommand);
    function ReadData(out Buf: TBytes; const MaxBufSize: integer): integer;

    property IsSended: Boolean read GetIsSended;
  end;

  TPipeDataDictionary = class(TObjectDictionary<THandle, TAbstractPipeData>)
  end;

  TPipeServer = class(TObject)
  strict private
    FServer: TFWPipeServer;
    FOnReadFromPipe: TExchangeEvent;
    FOnConnect: TConnectEvent;
    FOnDisconnect: TConnectEvent;
    FOnIdle: TNotifyEvent;

    FOutgoingDataDict: TPipeDataDictionary;
    FIncommingDataDict: TPipeDataDictionary;

    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);

    procedure InternalConnect(Sender: TObject; PipeHandle: PFWPipeData);
    procedure InternalDisconnect(Sender: TObject; PipeHandle: PFWPipeData);
    procedure InternalRead(Sender: TObject; PipeHandle: PFWPipeData);
    procedure InternalIdle(Sender: TObject);
  private
    function GetReady: Boolean;
  public
    constructor Create(const PipeName: string);
    destructor Destroy; override;

    Procedure Disconnect(PipeHandle : THandle);

    property Active: Boolean read GetActive write SetActive;
    property Ready : Boolean read GetReady;
    property OnConnect: TConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TConnectEvent read FOnDisconnect write FOnDisconnect;
    property OnReadFromPipe: TExchangeEvent read FOnReadFromPipe write FOnReadFromPipe;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
  end;

  TPipeClient = class(TObject)
  strict private
    FClient: TFWPipeClient;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);

    procedure intOnConnected(Sender: TObject);
    procedure intOnDisconnected(Sender: TObject);
  public
    constructor Create(const ServerName, PipeName: string);
    destructor Destroy; override;

    procedure SendData(SendStream, ReceiveStream: TStream);

    property Active: Boolean read GetActive write SetActive;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

  TPipeServerClient = class
  private
    FClientId: String;
    FPipeHandle: THandle;
    FIncomingStream: TMemoryStream;
    FOutgoingStream: TMemoryStream;
    FData: TObject; //Pointer, general usage.
  public
    constructor Create(aClientId : String; aPipeHandle : THandle); Reintroduce;
    Destructor Destroy; Override;
    Property ClientId : String read FClientId;
    property PipeHandle : THandle read FPipeHandle;
    Property IncomingStream : TMemoryStream read FIncomingStream Write FIncomingStream;
    Property OutgoingStream : TMemoryStream read FOutgoingStream Write FOutgoingStream;
    Property Data : TObject read FData Write FData;
  end;

  TPipeServerExecute = Procedure(Sender : TObject; FromClient : TPipeServerClient; IncommingValue: TStream; OutgoingValue: TStream) Of Object;
  TPipeServerConnect = Procedure(Sender : TObject; aClient : TPipeServerClient) Of Object;

  TThreadPipeServer = class(TThread)
  private
    FServer: TPipeServer;
    FClients : TObjectList<TPipeServerClient>;
    FOnServerExecute: TPipeServerExecute;
    FOnServerConnect: TPipeServerConnect;
    FOnServerDisconnect: TPipeServerConnect;
    Function InternalPipeHandleToClient(PipeHandle: THandle; Var aClient : TPipeServerClient) : Boolean;
    function GetReady: Boolean;
  protected
    procedure OnReadFromPipe(Sender: TObject; PipeHandle: THandle; IncommingValue: TStream; OutgoingValue: TStream);
    procedure OnIdle(Sender: TObject);
    procedure OnConnect(Sender: TObject; PipeHandle: THandle);
    procedure OnDisconnect(Sender : TObject; PipeHandle : THandle);
  public
    procedure Execute; override;

    Procedure Disconnect(aClient : TPipeServerClient);
    Procedure Stop;

    //Warn : Event executed on thread contexts.
    property OnServerExecute : TPipeServerExecute read FOnServerExecute Write FOnServerExecute;
    property OnServerConnect : TPipeServerConnect read FOnServerConnect Write FOnServerConnect;
    property OnServerDisconnect : TPipeServerConnect read FOnServerDisconnect Write FOnServerDisconnect;

    Property Ready : Boolean read GetReady;
  end;


implementation

function Max(Value1, Value2: integer): integer; inline;
begin
  if Value1 > Value2 then
    Result := Value1
  else
    Result := Value2;
end;

function Min(Value1, Value2: integer): integer; inline;
begin
  if Value1 < Value2 then
    Result := Value1
  else
    Result := Value2;
end;

{ TDataHeader }

procedure TPipeDataHeader.ReadFromBuf(const Buf: TBytes);
begin
  if Length(Buf) < SizeOf(TPipeDataHeader) then
    raise EPipeServerException.Create('Buffer too small for read Header');
  Move(Buf[0], Self, SizeOf(TPipeDataHeader));
end;

procedure TPipeDataHeader.WriteToBuf(var Buf: TBytes);
begin
  if Length(Buf) < SizeOf(TPipeDataHeader) then
    SetLength(Buf, SizeOf(TPipeDataHeader));
  Move(Self, Buf[0], SizeOf(TPipeDataHeader));
end;

{ TPIPEServer }

constructor TPipeServer.Create(const PipeName: string);
begin
  inherited Create;
  FOutgoingDataDict := TPipeDataDictionary.Create([doOwnsValues]);
  FIncommingDataDict := TPipeDataDictionary.Create([doOwnsValues]);
  FServer := TFWPipeServer.Create(PipeName);
  FServer.OnConnect := InternalConnect;
  FServer.OnDisconnect := InternalDisconnect;
  FServer.OnNeedProcessReadAndWrite := InternalRead;
  FServer.OnIdle := InternalIdle;
end;

destructor TPipeServer.Destroy;
begin
  FreeAndNil(FServer);
  FreeAndNil(FIncommingDataDict);
  FreeAndNil(FOutgoingDataDict);
  inherited;
end;

procedure TPipeServer.Disconnect(PipeHandle: THandle);
begin
  FServer.disconnect(PipeHandle);
end;

function TPipeServer.GetActive: Boolean;
begin
  Result := FServer.Active;
end;

function TPipeServer.GetReady: Boolean;
begin
  result := false;
  if Assigned(FServer) then
    result := FServer.ServerReadyToServe;
end;

procedure TPipeServer.InternalConnect(Sender: TObject; PipeHandle: PFWPipeData);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self, PipeHandle.PipeHandle);
end;

procedure TPipeServer.InternalDisconnect(Sender: TObject; PipeHandle: PFWPipeData);
begin
  FOutgoingDataDict.Remove(PipeHandle.PipeHandle);
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, PipeHandle.PipeHandle);
end;

procedure TPipeServer.InternalIdle(Sender: TObject);
begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

procedure TPipeServer.InternalRead(Sender: TObject; PipeHandle: PFWPipeData);
  procedure WriteDataToPipe(PipeHandle: PFWPipeData; var DataBuff: TBytes; OutgoingData: TAbstractPipeData);
  begin
    SetLength(DataBuff, 0);
    TSendingPipeData(OutgoingData).ReadData(DataBuff, Length(PipeHandle.WriteBuff));
    PipeHandle.WriteBuffSize := Length(DataBuff);
    Move(DataBuff[0], PipeHandle.WriteBuff[0], PipeHandle.WriteBuffSize);
    if TSendingPipeData(OutgoingData).IsSended then
      FOutgoingDataDict.Remove(PipeHandle.PipeHandle);
  end;

var
  IncommingData: TAbstractPipeData;
  OutgoingData: TAbstractPipeData;
  DataBuff: TBytes;
begin
  if not FIncommingDataDict.TryGetValue(PipeHandle.PipeHandle, IncommingData) then
  begin
    IncommingData := TReceivingPipeData.Create;
    FIncommingDataDict.Add(PipeHandle.PipeHandle, IncommingData);
  end;
  if TReceivingPipeData(IncommingData).IsReceived then
    raise Exception.Create('Previous incomming data is active');

  SetLength(DataBuff, PipeHandle.ReadBuffSize);
  Move(PipeHandle.ReadBuff[0], DataBuff[0], PipeHandle.ReadBuffSize);
  TReceivingPipeData(IncommingData).WriteData(DataBuff);

  if TReceivingPipeData(IncommingData).IsReceived then
    try
      IncommingData.DataStream.Seek(0, soBeginning);
      case IncommingData.Header.Command of
        pcUnknown:
          ;
        pcNewData:
          begin
            if FOutgoingDataDict.ContainsKey(PipeHandle.PipeHandle) then
              raise EPipeServerException.Create('try get new data without receiving current');

            OutgoingData := TSendingPipeData.Create;
            FOutgoingDataDict.Add(PipeHandle.PipeHandle, OutgoingData);
            if Assigned(FOnReadFromPipe) then
              FOnReadFromPipe(Self, PipeHandle.PipeHandle, IncommingData.DataStream, OutgoingData.DataStream);

            TSendingPipeData(OutgoingData).PrepareForSend(pcNewData);

            WriteDataToPipe(PipeHandle, DataBuff, OutgoingData);
          end;
        pcNextDataPart:
          begin
            if not FOutgoingDataDict.TryGetValue(PipeHandle.PipeHandle, OutgoingData) then
              raise EPipeServerException.Create('Trying get unexisting outgoing data part');

            WriteDataToPipe(PipeHandle, DataBuff, OutgoingData);
          end;
      end;
    finally
      FIncommingDataDict.Remove(PipeHandle.PipeHandle);
    end
  else
    PipeHandle.WriteBuffSize := 0;
end;

procedure TPipeServer.SetActive(const Value: Boolean);
begin
  FServer.Active := Value;
end;

{ TIncommingPipeData }

function TReceivingPipeData.GetIsReceived: Boolean;
begin
  Result := (Header.Command <> pcUnknown) and (DataStream.Size = Header.DataSize);
end;

procedure TReceivingPipeData.WriteData(const Buf: TBytes);
var
  iPos: integer;
begin
  if IsReceived then
    raise EPipeServerException.Create('Trying write to completed TIncommingPipeData');

  iPos := 0;
  if Header.Command = pcUnknown then
  begin
    Header.ReadFromBuf(Buf);
    iPos := SizeOf(TPipeDataHeader);
  end;

  if Length(Buf) > iPos then
    DataStream.Write(Buf[iPos], Length(Buf) - iPos);

  if DataStream.Size > Header.DataSize then
    raise EPipeServerException.Create('Too much data writed to TIncommingPipeData');
end;

{ TAbstractPipeData }

constructor TAbstractPipeData.Create;
begin
  inherited Create;
  FillChar(FHeader, SizeOf(TPipeDataHeader), 0);
  FDataStream := TMemoryStream.Create;
end;

destructor TAbstractPipeData.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited;
end;

{ TOutgoingPipeData }

function TSendingPipeData.GetIsSended: Boolean;
begin
  Result := FIsHeaderSended and (DataStream.Position = DataStream.Size)
end;

procedure TSendingPipeData.PrepareForSend(AHeaderCommand: TPipeCommand);
begin
  FHeader.Command := AHeaderCommand;
  FHeader.DataSize := DataStream.Size;
  DataStream.Seek(0, soBeginning);
  FIsHeaderSended := False;
end;

function TSendingPipeData.ReadData(out Buf: TBytes; const MaxBufSize: integer): integer;
var
  iPos: integer;
  iBufSize: integer;
begin
  if not FIsHeaderSended then
  begin
    iBufSize := Min(MaxBufSize, DataStream.Size + SizeOf(TPipeDataHeader));
    SetLength(Buf, iBufSize);
    iPos := SizeOf(TPipeDataHeader);
    FHeader.WriteToBuf(Buf);
    FIsHeaderSended := True;
    DataStream.Seek(0, soBeginning);
  end
  else
  begin
    iBufSize := Min(MaxBufSize, DataStream.Size - DataStream.Position);
    SetLength(Buf, iBufSize);
    iPos := 0;
  end;
  Result := iBufSize;

  iBufSize := iBufSize - iPos;
  if iBufSize > 0 then
    DataStream.Read(Buf[iPos], iBufSize);
end;

{ TPipeClient }

constructor TPipeClient.Create(const ServerName, PipeName: string);
begin
  inherited Create;
  FClient := TFWPipeClient.Create(ServerName, PipeName);
  FClient.OnConnect := intOnConnected;
  FClient.OnDisconnect := intOnDisconnected;
end;

destructor TPipeClient.Destroy;
begin
  FreeAndNil(FClient);
  inherited;
end;

function TPipeClient.GetActive: Boolean;
begin
  Result := FClient.Active;
end;

procedure TPipeClient.intOnConnected(Sender: TObject);
begin
  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TPipeClient.intOnDisconnected(Sender: TObject);
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

procedure TPipeClient.SendData(SendStream, ReceiveStream: TStream);
  procedure SendDataPart(SendPipeData: TSendingPipeData; ReceivePipeData: TReceivingPipeData);
  var
    Buf: TBytes;
    OutStream, InStream: TStream;
  begin
    SetLength(Buf, 0);
    SendPipeData.ReadData(Buf, MaxBuffSize);

    OutStream := TMemoryStream.Create;
    try
      OutStream.Write(Buf[0], Length(Buf));

      InStream := TMemoryStream.Create;
      try
        FClient.SendData(OutStream, InStream);
        if SendPipeData.IsSended then
        begin
          InStream.Position := 0;
          SetLength(Buf, InStream.Size);
          InStream.Read(Buf[0], Length(Buf));

          ReceivePipeData.WriteData(Buf);
        end
        else
          if InStream.Size <> 0 then
            raise EPipeServerException.Create('Server send some data without complete receiving');
      finally
        InStream.Free;
      end;
    finally
      OutStream.Free;
    end;
  end;

var
  SendPipeData: TSendingPipeData;
  ReceivePipeData: TReceivingPipeData;
begin
  ReceivePipeData := TReceivingPipeData.Create;
  try
    SendPipeData := TSendingPipeData.Create;
    try
      SendPipeData.DataStream.CopyFrom(SendStream, 0);
      SendPipeData.PrepareForSend(pcNewData);

      while not SendPipeData.IsSended do
        SendDataPart(SendPipeData, ReceivePipeData);
    finally
      SendPipeData.Free;
    end;

    while not ReceivePipeData.IsReceived do
    begin
      SendPipeData := TSendingPipeData.Create;
      try
        SendPipeData.PrepareForSend(pcNextDataPart);

        SendDataPart(SendPipeData, ReceivePipeData);
      finally
        SendPipeData.Free;
      end;
    end;

    ReceiveStream.Size := 0;
    ReceiveStream.CopyFrom(ReceivePipeData.DataStream, 0);
  finally
    ReceivePipeData.Free;
  end;
end;

procedure TPipeClient.SetActive(const Value: Boolean);
begin
  FClient.Active := Value;
end;

{ TThreadPipeServer }

procedure TThreadPipeServer.Disconnect(aClient: TPipeServerClient);
begin
  FServer.Disconnect(aClient.PipeHandle);
end;

procedure TThreadPipeServer.Execute;
begin
  FClients := TObjectList<TPipeServerClient>.Create;
  FServer := TPIPEServer.Create('LocalhostPipeTest');
  try
    FServer.OnReadFromPipe := OnReadFromPipe;
    FServer.OnIdle := OnIdle;
    FServer.OnConnect := OnConnect;
    FServer.OnDisconnect := OnDisconnect;
    FServer.Active := True;
  finally
    FServer.Free;
    FreeAndNil(FClients);
  end;
end;

function TThreadPipeServer.GetReady: Boolean;
begin
  result := false;
  if Assigned(FServer) then
    result := FServer.Ready;
end;

Function TThreadPipeServer.InternalPipeHandleToClient(PipeHandle: THandle;
  var aClient: TPipeServerClient): Boolean;
var ctemp : TPipeServerClient;
begin
  for ctemp in FClients do
    if ctemp.PipeHandle = PipeHandle then
    begin
      aclient := ctemp;
      result := true;
      break;
    end;
end;

procedure TThreadPipeServer.OnConnect(Sender: TObject; PipeHandle: THandle);
var aClient : TPipeServerClient;
begin
  if Assigned(FOnServerConnect) then
  begin
    aClient := TPipeServerClient.Create('Cli_'+IntToStr(FClients.Count+1)+'_'+IntToStr(PipeHandle),PipeHandle);
    FClients.Add(aClient);
    FOnServerConnect(Sender,aClient);
  end;
end;

procedure TThreadPipeServer.OnDisconnect(Sender: TObject; PipeHandle: THandle);
var aClient : TPipeServerClient;
begin
  if Assigned(FOnServerDisconnect) then
  begin
    if InternalPipeHandleToClient(PipeHandle,aClient) then
    begin
      FOnServerDisconnect(Sender,aClient);
      FClients.Remove(aClient);
    end
    else
    begin

    end;
  end;
end;

procedure TThreadPipeServer.OnIdle(Sender: TObject);
begin
  if Terminated then
    TPIPEServer(Sender).Active := False;
end;

procedure TThreadPipeServer.OnReadFromPipe(Sender: TObject; PipeHandle: THandle; IncommingValue, OutgoingValue: TStream);
var aClient :  TPipeServerClient;
begin
  if Assigned(FOnServerExecute) then
  begin
    if InternalPipeHandleToClient(PipeHandle, aclient) then
    begin
      FOnServerExecute(Sender,aClient,IncommingValue,OutgoingValue);
    end
    else
    begin

    end;
  end;
end;

procedure TThreadPipeServer.Stop;
begin
  FServer.Active := False;
end;

/// Handle exemple :
///  var
///   Cmd: integer;
/// begin
///   IncommingValue.Read(Cmd, SizeOf(integer));
///   case Cmd of
///     0:
///       OutgoingValue.Size := 100;
///     1:
///       OutgoingValue.Size := 65535 * 4;
///     2:
///       OutgoingValue.CopyFrom(IncommingValue, 0);
///   end;
/// end;



{ TPipeServerClient }


constructor TPipeServerClient.Create(aClientId: String; aPipeHandle : THandle);
begin
  Assert(length(AclientID)>0);
  FClientId := aClientId;
  FPipeHandle := aPipeHandle;
  FIncomingStream := TMemoryStream.Create;
  FOutgoingStream := TMemoryStream.Create;
  FData := Nil;
end;

destructor TPipeServerClient.Destroy;
begin
  FreeAndNil(FOutgoingStream);
  FreeAndNil(FIncomingStream);
  inherited;
end;

end.
