unit gridlog.fintro;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Mask, Vcl.ComCtrls, Vcl.Imaging.GIFImg, Vcl.Imaging.pngimage,
  gridlog.types;

type
  TfIntro = class(TForm)
    Image1: TImage;
    lblInfo: TLabel;
    pnlManualConnect: TPanel;
    Panel7: TPanel;
    MaskEditHost: TMaskEdit;
    Label1: TLabel;
    lblAlertIP: TLabel;
    Button1: TButton;
    edtUserName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtPassword: TEdit;
    edtPort: TEdit;
    ImgWait: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MaskEditHostExit(Sender: TObject);
  private
    { Private declarations }
    procedure ipalert(str : String);
  public
    { Public declarations }
    procedure showManualConnect;
  end;

var
  fIntro: TfIntro;

implementation

uses gridlog.Controler.VCL;

{$R *.dfm}

procedure TfIntro.Button1Click(Sender: TObject);
var s : TGridServerItem;
begin
  AnimateWindow(pnlManualConnect.Handle,0,AW_VER_POSITIVE or AW_HIDE);
  ImgWait.Visible := true;
  lblInfo.Caption := Format('Connecting to GRID Server %s:%s...',[MaskEditHost.EditText,edtPort.Text]);

  s :=  TGridServerItem.Create;
  try
    s.ServerHost := MaskEditHost.EditText;
    s.ServerPort := StrToInt(edtPort.Text);
    s.user := edtUserName.Text;
    s.pass := edtPassword.Text;
    s.externalId := '';
    dmFront.controler.UserAskForServerConnection(Self,s);
  finally
    FreeAndNil(s);
  end;
end;

procedure TfIntro.FormCreate(Sender: TObject);
begin
  lblAlertIP.Caption := '';
 (ImgWait.Picture.Graphic as TGIFImage).Animate := True;
end;

procedure TfIntro.ipalert(str: String);
begin
  lblAlertIP.Caption := str;
  lblAlertIP.Visible := true;
end;

type
  TIPv4Rec = record
    Octets: array [1..4] of Byte;
  end;

function StrToIPV4(const S: string; out R: TIPv4Rec): Boolean;
var
  i, DotPos: Integer;
  RestOfString: string;
  Value: Integer;
begin
  RestOfString := S;
  for i := Low(R.Octets) to High(R.Octets) do
  begin
    DotPos := Pos('.', RestOfString);
    if DotPos = 0 then
      DotPos := Length(RestOfString) + 1;
    if not TryStrToInt(Copy(RestOfString, 1, DotPos - 1), Value) or
      not (Value in [Low(Byte)..High(Byte)]) then
    begin
      Exit(False);
    end;
    R.Octets[i] := Value;
    Delete(RestOfString, 1, DotPos);
  end;
  Result := True;
end;


procedure TfIntro.MaskEditHostExit(Sender: TObject);
var
  S: string;
  R: TIPv4Rec;
begin
  lblAlertIP.Visible := false;
  S := StringReplace((Sender as TMaskEdit).EditText,' ','',[rfReplaceAll]);
  if not StrToIPV4(S, R) then
  begin
    ipalert(S + ' is not valid IPv4 string');
    exit;
  end;
  if not (R.Octets[1] in [1..223]) then
  begin
    ipalert('First octet must be from 1 to 223');
    exit;
  end;
  // Align right
  TMaskEdit(Sender).EditText := Format('%3d.%3d.%3d.%3d', [
    R.Octets[1], R.Octets[2], R.Octets[3], R.Octets[4]
    ]);
  // Store the R in some place
end;




procedure TfIntro.showManualConnect;
begin
  ImgWait.Visible := false;
  lblAlertIP.Visible := false;
  pnlManualConnect.Top := - pnlManualConnect.Height -50;
  pnlManualConnect.Left := round(width/2 - pnlManualConnect.Width/2);
  pnlManualConnect.Visible := true;
  AnimateWindow(pnlManualConnect.Handle,0,AW_VER_POSITIVE or AW_HIDE);
  pnlManualConnect.Top := ImgWait.Top;
  AnimateWindow(pnlManualConnect.Handle,500,AW_VER_POSITIVE OR AW_SLIDE OR AW_ACTIVATE);
end;

end.
