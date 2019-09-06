unit gridlog.flogview;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls,
  gridlog.types;

type
  Tflogview = class(TForm)
    Shape1: TShape;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FItem: TGridServerItem;
    procedure SetItem(const Value: TGridServerItem);
    { Private declarations }
  public
    { Public declarations }
    property Item : TGridServerItem read FItem write SetItem;
  end;

implementation

uses gridlog.controler.VCL;

{$R *.dfm}

procedure Tflogview.Button1Click(Sender: TObject);
begin
  Assert(Assigned(item));
  Item.user := 'admin';   //todo.
  Item.pass := 'admin';
  dmFront.controler.UserAskForServerConnection(Self,Item);
end;

procedure Tflogview.FormCreate(Sender: TObject);
begin
  Randomize;
  Shape1.Width := 20 +  Random(Width)-40;
  Shape1.Height := 20 +  Random(Height)-40;
  Shape1.Brush.Color := RGB(Random(255),Random(255),Random(255));
end;

procedure Tflogview.SetItem(const Value: TGridServerItem);
begin
  FItem := Value;
  Caption := FItem.ServerName;
end;

end.
