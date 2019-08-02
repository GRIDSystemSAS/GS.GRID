unit GS.GRID.Service.MicroServices;

interface

uses
  System.Classes,
  System.SysUtils,
  GS.Stream,
  GS.GRID.Common.Protocols,
  GS.GRID.Common.Protocols.KissB,
  GS.GRID.Common.Protocols.MicroService,
  GS.GRID.Client,
  GS.GRID.Client.KissB,
  GS.GRID.Client.Transport,
  GS.GRID.Client.Transport.IndyTCP;


Type
  //This one is to override, to define your microservice.
  TGsCustomMicroService = Class
  private
  protected
    function GetID : String; virtual; abstract;
    function GetMajor: UInt32; virtual;
    function GetMinor: UInt32; virtual;
    function GetRelease: UInt32; virtual;
    function GetVersionStatus: TGsMSVersionStatus;
    function GetCompagny : String; virtual; abstract;
    function GetServiceName : String; virtual; abstract;
    function GetServiceDesc : String; virtual;
    function GetAvailability : TGsMSAvailability; virtual;
    function GetBehaviour : TGsMSBehaviour; virtual; abstract;
    function GetInstanceBehaviour : TGsMSInstanceBehaviour; virtual; abstract;
    function GetRunningOn : TGsMSRunningOn; virtual; abstract;
    function GetUserMgntCapa : TGsMSUserManagementCapabilites; virtual; abstract;
    function GetToRecord: TMsg_FromService_ServiceData;
  public
    procedure OnServiceStart(const ServerData : TGsServerData; Const StartSuccessfully : Boolean; const StartErrorDesc : String); virtual; abstract;
    procedure OnServiceShutdownNotify(const ServerData : TGsServerData); virtual; abstract;
    procedure OnGridServerAskForStatus(const ServerData : TGsServerData; var ServiceData : TGsServiceData); virtual; abstract;

    procedure OnGridServerUserExecute(const ServerData : TGsServerData; UserData : TGsUserData; var ResultData : TMemoryStream); virtual; abstract;

    property ToRecord : TMsg_FromService_ServiceData read GetToRecord;

    property ID : String read GetID;
    property VersionMajor : UInt32 read GetMajor;
    property VersionMinor : UInt32 read GetMinor;
    property VersionRelease : UInt32 read GetRelease;
    property VersionStatus : TGsMSVersionStatus read GetVersionStatus;
    property Compagny : String read GetCompagny;
    property ServiceName : String read getServiceName;
    property ServiceDesc : String read GetServiceDesc;

    property MicroSvcAvailability : TGsMSAvailability read GetAvailability;
    property MicroSvcBehaviour : TGsMSBehaviour read GetBehaviour;
    property MicroSvcInstanceBehaviour : TGsMSInstanceBehaviour read GetInstanceBehaviour;
    property MicroSvcRunningOn : TGsMSRunningOn  read GetRunningOn;
    property MicroSvcUserMgntCapabilities : TGsMSUserManagementCapabilites read GetUserMgntCapa;
  End;

  //this one is the engine : It manage connection and message driven system.
  TGsMicroServiceEngine = Class
  private
  protected
    FMicroService : TGsCustomMicroService;
    FClient : TGRIDClientKissB;

    //HOUSEWORK TODO !!!
    FChanRequest,FChanResponse : String;
    Frecv : TGRIDMessages;
    Fstr : TMemoryStream;

    FServiceData : TMsg_FromService_ServiceData;

    FprotoRegResp : TMsg_FromServer_ServiceRegistrationResponse;
    FserverData : TGsServerData;
    FuserData : TGsUserData;

    FServiceExec : TMsg_FromServer_ExecuteOrder;
    FServiceResp : TMsg_FromService_ExecuteResponse;

{ TODO -oVGS -cNiceToHave :
In a far future, in order to access via MQTT, KissBe, or whatever bus protocol,
it should be cool to make a TGRIDClientPubSub : It will then be possible to change
access methodology between MicroService and GridServer
Something like
TGRIDClientPubSub = class
public
  procedure subscribte(...) abstract;
  procedure unSub(...) abstract
  procedure send(achan : string; aMess : TBytes) abstract;
  procedure OnDataEvent
 }

    Function Terminate : Boolean; virtual;

    Procedure Start; virtual;
    procedure Loop; virtual;
    procedure Finish; virtual;
  public
    constructor Create(aMicroService : TGsCustomMicroService); Virtual;
    Destructor Destroy; Override;

    procedure process; virtual;
  End;


implementation

{ TGsCustomMicroService }

function TGsCustomMicroService.GetAvailability: TGsMSAvailability;
begin
  result := TGsMSAvailability.Enable;
end;


function TGsCustomMicroService.GetMajor: UInt32;
begin
  result := 0;
end;

function TGsCustomMicroService.GetMinor: UInt32;
begin
  result := 0;
end;

function TGsCustomMicroService.GetRelease: UInt32;
begin
  result := 0;
end;

function TGsCustomMicroService.GetServiceDesc: String;
begin
  result := '';
end;


function TGsCustomMicroService.GetToRecord: TMsg_FromService_ServiceData;
begin
  result.ID := ID;
  result.VersionMajor := VersionMajor;
  result.VersionMinor := VersionMinor;
  result.VersionRelease := VersionRelease;
  result.VersionStatus := VersionStatus;
  result.Compagny := Compagny;
  result.ServiceName := ServiceName;
  result.ServiceDesc := ServiceDesc;
  result.MicroSvcAvailability := MicroSvcAvailability;
  result.MicroSvcBehaviour := MicroSvcBehaviour;
  result.MicroSvcInstanceBehaviour := MicroSvcInstanceBehaviour;
  result.MicroSvcRunningOn := MicroSvcRunningOn;
  result.MicroSvcUserMgntCapabilities := MicroSvcUserMgntCapabilities;
end;

function TGsCustomMicroService.GetVersionStatus: TGsMSVersionStatus;
begin
  result := TGsMSVersionStatus.Beta;
end;




{ TGsMicroServiceEngine }

constructor TGsMicroServiceEngine.Create(aMicroService: TGsCustomMicroService);
begin
  Assert(Assigned(aMicroService));
  FMicroService := aMicroService;
  FClient := TGRIDClientKissB.Create(TGRIDTransportIndyTCP.Create);
  Fstr := TMemoryStream.Create;
end;

destructor TGsMicroServiceEngine.Destroy;
begin
  FreeAndNil(FClient);
  FreeAndNil(FStr);
  inherited;
end;


procedure TGsMicroServiceEngine.process;
begin
  Start;
  Loop;
  Finish;
end;

procedure TGsMicroServiceEngine.Start;
begin
  FChanRequest := CST_PUBLIC_CHANNAME_HYPERVISOR_SERVICE_REGISTRATION;
  FChanResponse := FMicroService.ID+'\'+FMicroService.ClassName+'\R';

  if not (FClient.Connect('admin','admin').Status) then
    raise Exception.Create('Error Message');

  //Step one, send the service complete configuration.
  if FClient.Subscribe(FChanResponse) then
  begin
    FServiceData := FMicroService.ToRecord;
    FServiceData.ResponseChannel := FChanResponse;
    FServiceData.Save(Fstr);
    FClient.SendMessage(FChanRequest,Fstr);
    FClient.CheckMsgWait(Frecv);
    if length(Frecv)>0 then
    begin
      //lr := StringOf(lrecv[0].PayLoad);
      Fstr.Clear;
      BytesToStream(Fstr, Frecv[0].PayLoad);
      Fstr.Position := 0;
      FprotoRegResp.load(Fstr);

      if FprotoRegResp.Status then
      begin

// THIS DATA WILL BE GIVEN BY SERVER.
//      userdata.username := 'admin';
//      userdata.session := '';
//      serInf := Client.Infos;
//      serverData.GridServerName := serInf.ServerGenuineName;
//      serverData.GridServerNetworkData := WithoutT(Client.Transport.ClassName)+'/'+protocol+'/127.0.0.1:60000';
//      serverData.GridLogChannel := 'AlogAdresseWhichIdentifyThisservice.;

        //protoRegResp.ChannelName_ServiceStartNotification Sub !!!
//          FClient.Subscribe(FprotoRegResp.ChannelName_ServiceStartNotification);
//          FClient.Subscribe(FprotoRegResp.ChannelName_ServiceShutdownNotification);
//          FClient.Subscribe(FprotoRegResp.ChannelName_AskForStatus);
        FClient.Subscribe(FprotoRegResp.ChannelName_ServerExecute);
      end;


//      FClient.SendMessage(CST_PUBLIC_CHANNAME_HYPERVISOR_SERVICE_REGISTRATION,'READY');
    end;
  end;
end;

procedure TGsMicroServiceEngine.Loop;
var i : integer;
begin
  while Not Terminate do  //TODO : Here
  begin
    Frecv := nil;
    //1000 = 1s. It did not wait 1 seconds, but wait a message durrin one second, and then loop.
    //It allow then to not lock for a long time, and if a message is presenting, it is take care immediately.
    if FClient.CheckMsg(Frecv,1000) then
    begin
      for i := 0 to length(Frecv)-1 do
      begin
        if Frecv[i].From = FprotoRegResp.ChannelName_ServerExecute  then
        begin
          Fstr.Clear;
          BytesToStream(Fstr,Frecv[i].PayLoad);
          Fstr.Position := 0;
          FServiceExec.Load(Fstr);
          Fstr.Clear;
          FuserData.InData := FServiceExec.InPayload;
          try
            FMicroService.OnGridServerUserExecute(Fserverdata,Fuserdata,Fstr);
          Except
            on E : Exception do
            begin
              FServiceResp.ExecTimeElapsedMSec := 0;
              FServiceResp.ExecPayloadResult := Bytesof(UTF8String(e.Message));
              Fstr.Clear;
              FServiceResp.Save(Fstr);
            end;
          end;
          FClient.SendMessage(FServiceExec.ChannelReturn,Fstr)
        end;
      end;
    end
    else
    begin
      //Do something else, such heartbeat, or other control.
    end;
  end;
end;

function TGsMicroServiceEngine.Terminate: Boolean;
begin
  result := false;
end;

procedure TGsMicroServiceEngine.Finish;
begin
  //Send something to server ?
  if FClient.Connected then
    FClient.Disconnect;
end;

end.
