unit gridlog.fmain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  gridlog.types, System.ImageList, Vcl.ImgList;

type
  TfMainView = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    TimerBus: TTimer;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    tvGridNode: TTreeView;
    pnlBase: TPanel;
    ImageList1: TImageList;
    procedure tvGridNodeChange(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
  public
    { Public declarations }
    function gettvNodeFromServer(server : TGridServerItem) : TTreeNode;
  end;

var
  fMainView: TfMainView;

implementation

uses gridlog.Controler.VCL;

{$R *.dfm}


function TfMainView.gettvNodeFromServer(server: TGridServerItem): TTreeNode;
var i : integer;
begin
  Assert(assigned(server));
  for i := 0 to tvGridNode.Items.Count-1 do
  begin
    if tvGridNode.Items[i].Data = server then
    begin
      result := tvGridNode.Items[i];
      break;
    end;
  end;
end;

procedure TfMainView.tvGridNodeChange(Sender: TObject; Node: TTreeNode);
var l : TGridServerItem;
begin
  if Assigned(Node) then
  begin
    if assigned(Node.Data) then
    begin
      if TObject(Node.Data) is TGridServerItem  then
      begin
        l := TGridServerItem(Node.Data);
        dmFront.controler.UserSelectServerSymbol(self,l);
      end;
    end;
  end;
end;


end.
