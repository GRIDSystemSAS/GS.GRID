//App controler.
unit gridlog.Controler.VCL;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls,
  Windows,
  generics.collections,
  gridlog.types,
  gridlog.Controler,
  gridLog.fmain, gridlog.flogview, gridlog.fintro;


type
  TGridLogAppState = (init, mainview);

  TGRIDLogControlerVCL = class(TGRIDLogControler)
  private
    FState: TGridLogAppState;
    procedure SetState(const Value: TGridLogAppState);
  protected
    procedure BackendGenericError(sender : TObject; Error : String); override;
    procedure BackendStatus(sender : TObject; status : boolean); override;
    procedure BackendServerNotify_DetectionResult(sender : TObject; ServerCount : Integer); override;
    procedure BackendServerNotify_Connection(sender : TObject; server : TGridServerItem); override;
    procedure BackendServerNotify_NewServer(sender : TObject; server : TGridServerItem); override;
    procedure BackendServerError(sender : TObject; server : TGridServerItem; error : string); override;
    procedure BackendServerLogResult(sender : TObject; server : TGridServerItem; log : string); override;
  public
    constructor Create; override;

    //User selecting server symbol somewhere in the UI.
    procedure UserSelectServerSymbol(sender : TObject; server : TGridServerItem); override;
    procedure UserAskForServerConnection(sender : TObject; server : TGridServerItem); override;

    property State : TGridLogAppState read FState Write SetState;
  end;


  TdmFront = class(TDataModule)
    TimerBus: TTimer;
    procedure TimerBusTimer(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fLogViews : TDictionary<TGridServerItem,Tflogview>;
  public
    { Public declarations }
    controler : TGRIDLogControlerVCL;

    function getLogViewFromServer(server : TGridServerItem) : Tflogview;
  end;

var
  dmFront: TdmFront;

implementation


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmFront.DataModuleCreate(Sender: TObject);
begin
  fLogViews := TDictionary<TGridServerItem,Tflogview>.Create;
  controler := TGRIDLogControlerVCL.Create;
end;

procedure TdmFront.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fLogViews);
  FreeAndNil(controler);
end;

function TdmFront.getLogViewFromServer(server: TGridServerItem): Tflogview;
begin
  if not fLogViews.TryGetValue(server,Result) then
  begin
    result := Tflogview.Create(nil);
    result.Item := server;
    fLogViews.Add(server,result);
  end;
end;


procedure TdmFront.TimerBusTimer(Sender: TObject);
begin
  controler.FrontAPI.Process;
  controler.ServerAPI.Process;
end;

{ TGRIDLogControlerVCL }

procedure TGRIDLogControlerVCL.BackendGenericError(sender: TObject;
  Error: String);
begin
  case State of
    init:
    begin
      fIntro.lblInfo.Caption := Error;
    end;
    mainview:
    begin
      ShowMessage(error);
    end;
  end;
end;

procedure TGRIDLogControlerVCL.BackendServerError(sender: TObject;
  server: TGridServerItem; error: string);
var l : TTreeNode;
begin
  case State of
    init:
    begin
      fIntro.lblInfo.Caption := 'connection error : '+error+'. Please retry.';
      fIntro.showManualConnect;
    end;
    mainview:
    begin
      if assigned(server) then
      begin
        l := fMainView.gettvNodeFromServer(server);
        if Assigned(l) then
        begin
          l.ImageIndex := 0;
          l.SelectedIndex := 0;
        end;
      end;

      ShowMessage(error);
    end;
  end;
end;

procedure TGRIDLogControlerVCL.BackendServerLogResult(sender: TObject;
  server: TGridServerItem; log: string);
var b : Tflogview;
begin
  //logview pan.
  b := dmFront.getLogViewFromServer(server);
  b.Memo1.Lines.Add(log);
end;

procedure TGRIDLogControlerVCL.BackendServerNotify_Connection(sender: TObject;
  server: TGridServerItem);
var b : Tflogview;
    l : TTreeNode;
begin
  case State of
    init:
    begin
      State := TGridLogAppState.mainview;
    end ;
    mainview:
    begin
    end;
  end;

  //All case.

  //logview pan.
  b := dmFront.getLogViewFromServer(server);
  b.Button1.Caption := 'Connected';
  b.Button1.Enabled := False;

  //main view.
  l := fMainView.gettvNodeFromServer(server);
  if assigned(l) then
  begin
    l.ImageIndex := 2;
    l.SelectedIndex := 2;
  end;
end;

procedure TGRIDLogControlerVCL.BackendServerNotify_DetectionResult(
  sender: TObject; ServerCount: Integer);
begin
  case State of
    init:
    begin
      if  ServerCount=0 then
      begin
        fIntro.lblInfo.Caption := format(' %d found on local network, please connect manually !',[ServerCount]);
        fIntro.showManualConnect;
      end
      else
      begin
        fIntro.lblInfo.Caption := format(' %d found on local network !',[ServerCount]);
        State := TGridLogAppState.mainview;
      end;
    end;
    mainview: ;
  end;
end;

procedure TGRIDLogControlerVCL.BackendServerNotify_NewServer(sender: TObject;
  server: TGridServerItem);
var l : TTreeNode;
begin
  Assert(assigned(server));
  case State of
    init: ;
    mainview: ;
  end;
  //main view.
  l := fMainView.tvGridNode.Items.AddChild(nil,server.ServerName);
  l.ImageIndex := 0;
  l.Text := server.ServerName;
  l.Data := server;
  server.externalId := IntToStr(l.Index);
end;

procedure TGRIDLogControlerVCL.BackendStatus(sender: TObject; status: boolean);
begin
  case State of
    init: ;
    mainview: ;
  end;

  //
  fMainView.pnlBase.Visible := false;
  fIntro.BorderStyle := TFormBorderStyle.bsNone;
  fIntro.Parent := fMainView;
  fIntro.Align := TALign.alClient;
  fIntro.Show;
  fIntro.lblInfo.Caption := 'looking for GRID Server on near network...';
end;

constructor TGRIDLogControlerVCL.Create;
begin
  inherited;
  FState := init;
end;

procedure TGRIDLogControlerVCL.SetState(const Value: TGridLogAppState);
begin
  if (FState = TGridLogAppState.init) AND (Value =  TGridLogAppState.mainview) then
  begin
    fIntro.ImgWait.Visible := false;
    fMainView.pnlBase.Visible := true;
    AnimateWindow(fIntro.Handle,250,AW_VER_POSITIVE or AW_HIDE);
  end;
  FState := Value;
end;

procedure TGRIDLogControlerVCL.UserAskForServerConnection(sender: TObject;
  server: TGridServerItem);
var l : TTreeNode;
begin
  Assert(assigned(server));
  case State of
    init:
    begin
      ServerAPI.backend_askGridConnection(server.ServerHost,server.ServerPort,server.user,server.pass,server.externalId);
    end;
    mainview:
    begin
      l := fMainView.gettvNodeFromServer(server);
      if assigned(l) then
      begin
        l.ImageIndex := 1;
        l.SelectedIndex := 1;
      end;
      ServerAPI.backend_askGridConnection(server.ServerHost,server.ServerPort,server.user,server.pass,server.externalId);
    end;
  end;
end;

procedure TGRIDLogControlerVCL.UserSelectServerSymbol(sender: TObject;
  server: TGridServerItem);
var b : Tflogview;
begin
  b := dmFront.getLogViewFromServer(server);
  if assigned(b) then
  begin
    b.Parent := fMainView.Panel4;
    if Not b.Visible then
      b.Show;
  end;
end;


end.
