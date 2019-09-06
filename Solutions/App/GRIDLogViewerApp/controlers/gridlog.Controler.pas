unit gridlog.Controler;

interface

uses
  System.SysUtils, System.Classes, Vcl.ExtCtrls,
  generics.collections,
  gs.bus,
  gridlog.types,
  gridlog.backendClientAPI;

Type

  TGridLogControler = class abstract
  protected
    //API Event.
    procedure OnBackendGenericError(sender : TObject; Error : String);
    procedure OnBackendStatus(sender : TObject; status : boolean);
    procedure OnBackendServerDetectionResult(sender : TObject; ServerCount : Integer);
    procedure OnBackendServerNotify_Connection(sender : TObject; server : TGridServerItem);
    procedure OnBackendServerNotify_NewServer(sender : TObject; server : TGridServerItem);
    procedure OnBackendServerError(sender : TObject; server : TGridServerItem; error : string);
    procedure OnBackendServerLogResult(sender : TObject; server : TGridServerItem; log : string);

  protected
    FrontAPI : TGridLogFrontAPI_BackendRoot;
    ServerAPI : TGridLogFrontAPI_BackendGridServer;

    //backend event. (Must be overriden);
    procedure BackendGenericError(sender : TObject; Error : String); virtual; abstract;
    procedure BackendStatus(sender : TObject; status : boolean); virtual; abstract;
    procedure BackendServerNotify_DetectionResult(sender : TObject; ServerCount : Integer); virtual; abstract;
    procedure BackendServerNotify_Connection(sender : TObject; server : TGridServerItem); virtual; abstract;
    procedure BackendServerNotify_NewServer(sender : TObject; server : TGridServerItem); virtual; abstract;
    procedure BackendServerError(sender : TObject; server : TGridServerItem; error : string); virtual; abstract;
    procedure BackendServerLogResult(sender : TObject; server : TGridServerItem; log : string); virtual; abstract;

  public

    constructor Create; virtual;
    destructor Destroy; override;

    //User trigger.
    procedure UserSelectServerSymbol(sender : TObject; server : TGridServerItem); virtual; abstract;
    procedure UserAskForServerConnection(sender : TObject; server : TGridServerItem); virtual; abstract;

  end;

implementation

{ TGridLogControler }

constructor TGridLogControler.create;
begin
  FrontAPI := TGridLogFrontAPI_BackendRoot.Create;
  FrontAPI.OnGenericBackendError := OnBackendGenericError;
  FrontAPI.OnBackendStatus := OnBackendStatus;

  ServerAPI := TGridLogFrontAPI_BackendGridServer.Create;
  ServerAPI.OnGenericBackendError := OnBackendGenericError;
  ServerAPI.OnServerError := OnBackendServerError;
  ServerAPI.OnServerDetectionResult :=OnBackendServerDetectionResult;
  ServerAPI.OnServerConnected := OnBackendServerNotify_Connection;
  ServerAPI.OnNewServer := OnBackendServerNotify_NewServer;
  ServerAPI.OnServerLogResult := OnBackendServerLogResult;

  FrontAPI.backend_askStatus;
end;

destructor TGridLogControler.Destroy;
begin
  FreeAndNil(FrontAPI);
  FreeAndNil(ServerAPI);
  inherited;
end;

procedure TGridLogControler.OnBackendGenericError(sender: TObject; Error: String);
begin
  BackendGenericError(sender,error);
end;

procedure TGridLogControler.OnBackendServerDetectionResult(sender: TObject;
  ServerCount: Integer);
begin
  BackendServerNotify_DetectionResult(sender,ServerCount);
end;

procedure TGridLogControler.OnBackendServerError(sender: TObject;
  server: TGridServerItem; error: string);
begin
  BackendServerError(sender,server,error);
end;

procedure TGridLogControler.OnBackendServerLogResult(sender: TObject;
  server: TGridServerItem; log: string);
begin
  BackendServerLogResult(sender,server,log);
end;

procedure TGridLogControler.OnBackendServerNotify_Connection(sender: TObject;
  server: TGridServerItem);
begin
  BackendServerNotify_Connection(sender,server);
end;

procedure TGridLogControler.OnBackendServerNotify_NewServer(sender: TObject;
  server: TGridServerItem);
begin
  BackendServerNotify_NewServer(sender,server);
end;

procedure TGridLogControler.OnBackendStatus(sender: TObject; status: boolean);
begin
  backendstatus(sender,status);
  if status then
    ServerAPI.backend_askGridDetection;
end;


end.
