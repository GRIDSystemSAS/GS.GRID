unit fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  GS.Bus,
  GS.GRID.Client,
  GS.GRID.Client.Resolver,
  GS.GRID.Client.KissB,
  GS.GRID.Common.Protocols.KissB,
  GS.GRID.Client.Transport.IndyTCP, FMX.Objects, FMX.Colors;

type
  TForm30 = class(TForm)
    pnlMain: TPanel;
    Panel3: TPanel;
    Label5: TLabel;
    Rectangle65: TRectangle;
    Image1: TImage;
    Rectangle1: TRectangle;
    Timerbus: TTimer;
    ButtonClearLEDMatrix: TButton;
    ColorPicker1: TColorPicker;
    ColorQuad1: TColorQuad;
    ColorBox1: TColorBox;
    Label6: TLabel;
    Label7: TLabel;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Rectangle5: TRectangle;
    Rectangle6: TRectangle;
    Rectangle7: TRectangle;
    Rectangle8: TRectangle;
    Rectangle9: TRectangle;
    Rectangle10: TRectangle;
    Rectangle11: TRectangle;
    Rectangle12: TRectangle;
    Rectangle13: TRectangle;
    Rectangle14: TRectangle;
    Rectangle15: TRectangle;
    Rectangle16: TRectangle;
    Rectangle17: TRectangle;
    Rectangle18: TRectangle;
    Rectangle19: TRectangle;
    Rectangle20: TRectangle;
    Rectangle21: TRectangle;
    Rectangle22: TRectangle;
    Rectangle23: TRectangle;
    Rectangle24: TRectangle;
    Rectangle25: TRectangle;
    Rectangle26: TRectangle;
    Rectangle27: TRectangle;
    Rectangle28: TRectangle;
    Rectangle29: TRectangle;
    Rectangle30: TRectangle;
    Rectangle31: TRectangle;
    Rectangle32: TRectangle;
    Rectangle33: TRectangle;
    Rectangle34: TRectangle;
    Rectangle35: TRectangle;
    Rectangle36: TRectangle;
    Rectangle37: TRectangle;
    Rectangle38: TRectangle;
    Rectangle39: TRectangle;
    Rectangle40: TRectangle;
    Rectangle41: TRectangle;
    Rectangle42: TRectangle;
    Rectangle43: TRectangle;
    Rectangle44: TRectangle;
    Rectangle45: TRectangle;
    Rectangle46: TRectangle;
    Rectangle47: TRectangle;
    Rectangle48: TRectangle;
    Rectangle49: TRectangle;
    Rectangle50: TRectangle;
    Rectangle51: TRectangle;
    Rectangle52: TRectangle;
    Rectangle53: TRectangle;
    Rectangle54: TRectangle;
    Rectangle55: TRectangle;
    Rectangle56: TRectangle;
    Rectangle57: TRectangle;
    Rectangle58: TRectangle;
    Rectangle59: TRectangle;
    Rectangle60: TRectangle;
    Rectangle61: TRectangle;
    Rectangle62: TRectangle;
    Rectangle63: TRectangle;
    Rectangle64: TRectangle;
    ButtonRefreshFromMatrix: TButton;
    Label8: TLabel;
    Panel1: TPanel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Humidity: TLabel;
    procedure RectangleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerbusTimer(Sender: TObject);
    procedure ButtonClearLEDMatrixClick(Sender: TObject);
    procedure ButtonRefreshFromMatrixClick(Sender: TObject);
  private
    { Private declarations }
  public
    Cli : TGRIDClientKissB;
    cThreadListener : TBusClientReader;

    { Public declarations }
    procedure LaunchDataAcquisition;

    //sorry, I use a bus : I can stand synchronize stuff ;)
    procedure DatafromAcquisitionThread(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
  end;

  TDataAcquisitionTask = class(TThread)
  private
    FClient : TGRIDClientKissB; //owned client.
  public
    procedure Execute; Override;
  end;

var
  Form30: TForm30;
  DataAcq : TDataAcquisitionTask;


const
 //Python code to clear the led matrix.
 CST_PYTHONCODE_CLEARLEDMATRIX =
   'from sense_hat import SenseHat' + sLineBreak +
   'def gridmain():' + sLineBreak +
   '  sense = SenseHat()' + sLineBreak +
   '  sense.clear()';

 //Python code to get temperature, pressure, and humidity. Running in a thread.
 CST_PYTHONCODE_GETSENSORDATA =
      'from sense_hat import SenseHat' + sLineBreak +
      'def gridmain():' + sLineBreak +
      '  sense = SenseHat()' + sLineBreak +
      '  t = round(sense.get_temperature(),2)' + sLineBreak +
      '  print("TEMP:{}".format(t))' + sLineBreak +
      '  print("PRESS:{}".format(round(sense.get_pressure(),1)))' + sLineBreak +
      '  print("HUMID:{}".format(round(sense.get_humidity(),2)))';

  //Python code to set pixel for a given color on the matrix. we get the color
  // after, to report to the gui the real hardware color obtained.
  CST_PYTHONCODE_SETGETPIX =
    'from sense_hat import SenseHat' + sLineBreak +
    'def gridmain():' + sLineBreak +
    '  sense = SenseHat()' + sLineBreak +
    '  sense.set_pixel(%d,%d,%d,%d,%d)' + sLineBreak +
    '  pixel = sense.get_pixel(%d,%d)' + sLineBreak +
    '  print("{} {} {}".format(pixel[0],pixel[1],pixel[2]))';

    //Python code to update gui from sense's led matric hardware value.
  CST_PYTHONCODE_REFRESH_FROM_MATRIX =
    'from sense_hat import SenseHat' + sLineBreak +
    'def gridmain():' + sLineBreak +
    '  sense = SenseHat()' + sLineBreak +
    '  for i in sense.get_pixels():' + sLineBreak +
    '    print("{} {} {}".format(i[0],i[1],i[2]))';


implementation

{$R *.fmx}

procedure TForm30.FormCreate(Sender: TObject);
var i,j,c : integer;
    r : TRectangle;
    lo : TObject;
begin
  //Start bus.
  StartStandartBus;
  //subscript to thread message.
  cThreadListener := Bus.Subscribe('HiMan',DatafromAcquisitionThread);

  //our thread. (will start if connection succeed)
  DataAcq := TDataAcquisitionTask.Create(true);


  pnlMain.Visible := false;
  Cli := TGRIDClientKissB.Create(TGRIDTransportIndyTCP.Create);

  c := 1;
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      lo := FindComponent('Rectangle'+IntToStr(c));
      if Assigned(lo) then
      begin
        r := TRectangle(lo);
        r.TagString:=intTostr(i)+','+inttoStr(j);
        r.Tag := c;
        r.OnClick := RectangleClick;
        inc(c);
      end;
    end;
end;

procedure TForm30.FormDestroy(Sender: TObject);
begin
  if Not DataAcq.Terminated then
  begin
    if not DataAcq.Suspended then
    begin
      DataAcq.Terminate;
      DataAcq.WaitFor;
    end;
  end;
  FreeAndNil(DataAcq);

  if Cli.Connected then
    cli.Disconnect;
  FreeAndNil(cli);

  cThreadListener.free;
  ReleaseStandartBus;
end;

procedure TForm30.FormShow(Sender: TObject);
var
  lr : TGridClientGRIDResolver;
begin
  if not(cli.Connected) then
  begin
    lr := TGridClientGRIDResolver.Create;
    try
      lr.Resolve;
      if lr.GRIDServerCount>0 then
      begin
        TGRIDTransportIndyTCP(Cli.Transport).Host := lr.GRIDServers[0].GridServerIP;
        TGRIDTransportIndyTCP(Cli.Transport).Port := StrToIntDef(lr.GRIDServers[0].GridServerPort,60000);

        if Cli.Connect('admin','admin').Status then
        begin
          pnlMain.Visible := true;
          LaunchDataAcquisition;
        end
        else
         raise Exception.Create(Cli.LastStatusInfo);
      end
      else
        raise Exception.Create('No server found.');
    finally
      FreeAndNil(lr);
    end;
  end;
end;

procedure TForm30.LaunchDataAcquisition;
begin
  DataAcq.Start;
end;

procedure TForm30.RectangleClick(Sender: TObject);
var lcode : string;
    cx,cy : integer;
    l : TAlphaColor;
    ll : TStringList;
begin
  cx := 0;
  cy := 0;

  ll := TStringList.Create;
  try
    ll.Delimiter := ',';
    ll.DelimitedText := TRectangle(sender).TagString;
    cx := StrToIntDef(ll[0],0);
    cy := StrToIntDef(ll[1],0);

    l := ColorBox1.Color;
    lcode := Format(CST_PYTHONCODE_SETGETPIX,[cx,cy,TAlphaColorRec(l).R,TAlphaColorRec(l).G,TAlphaColorRec(l).B,cx,cy]);
    lcode := Cli.instantPythonRun(lcode);

    //Real color of "hardware" pixel is returned to lcode. decode it.
    ll.Clear;
    ll.Delimiter := ' ';
    ll.DelimitedText := lcode;

    TAlphaColorRec(l).R := StrToIntDef(ll[0],50);
    TAlphaColorRec(l).G := StrToIntDef(ll[1],50);
    TAlphaColorRec(l).B := StrToIntDef(ll[2],50);

    TRectangle(FindComponent('Rectangle'+IntToStr(TRectangle(sender).Tag))).Fill.Color := l;
  finally
    FreeAndNil(ll);
  end;
end;

procedure TForm30.TimerbusTimer(Sender: TObject);
begin
  BusProcessMessages(cThreadListener);
end;

procedure TForm30.ButtonClearLEDMatrixClick(Sender: TObject);
begin
  Cli.instantPythonRun(CST_PYTHONCODE_CLEARLEDMATRIX)
end;

procedure TForm30.ButtonRefreshFromMatrixClick(Sender: TObject);
var lresp, ltemp : TStringList;
    i : integer;
    l : TAlphaColor;
    lo : TObject;
begin
  lresp := TStringList.Create;
  ltemp := TStringList.Create;
  try
    lresp.Text := cli.instantPythonRun(CST_PYTHONCODE_REFRESH_FROM_MATRIX);
    ltemp.Delimiter := ' ';
    for i := 0 to lresp.Count-1 do
    begin
      ltemp.DelimitedText := lresp[i];

      TAlphaColorRec(l).R := StrToIntDef(ltemp[0],50);
      TAlphaColorRec(l).G := StrToIntDef(ltemp[1],50);
      TAlphaColorRec(l).B := StrToIntDef(ltemp[2],50);
      TAlphaColorRec(l).A := 255;
      lo :=FindComponent('Rectangle'+IntToStr(i+1));
      if assigned(lo) then
        TRectangle(lo).Fill.Color := l;
    end;

  finally
    FreeandNil(lresp);
    FreeandNil(ltemp);
  end;

end;

procedure TForm30.DatafromAcquisitionThread(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
var l : TStringList;
    t : String;
begin
  //Here come message from our thread : We are in the GUI context.
  l := Packet.ContentMessage.AsStringList;
  try
    if l.Count>2 then
    begin
      Label1.Text := l[0].Replace('TEMP:','');
      Label2.Text := l[1].Replace('PRESS:','');
      Label3.Text := l[2].Replace('HUMID:','');
    end
    else
    begin
      if l.Count>0 then
      begin
        t := l[0];
        if l[0].StartsWith('ERR:') then
        begin
          t := t.Replace('ERR:','');
          ShowMessage(t);
        end
        else
        if l[0].StartsWith('INF:') then
        begin
          t := t.Replace('INF:','');
          Label6.Text := 'Remote Host '+t;
        end
        else
        if l[0].StartsWith('NME:') then
        begin
          t := t.Replace('NME:','');
          Label5.Text := 'Connected on GRID Server - Name : '+t;
        end

        else
        if l[0].StartsWith('CPU:') then
        begin
          t := t.Replace('CPU:','');
          Label7.Text := 'Average CPU load : '+t+'%';
        end;
      end;
    end;
  finally
    FreeAndNil(l);
  end;
end;

{ TDataAcquisitionTask }

procedure TDataAcquisitionTask.Execute;
var busmessage : TBusMessage;
    inf : TGRIDProtocol_KB_SRV_PROCESS_API_INFO;
    t : string;
begin
  FClient := TGRIDClientKissB.Create(TGRIDTransportIndyTCP.Create);
  TGRIDTransportIndyTCP(FClient.Transport).Host := TGRIDTransportIndyTCP(form30.Cli.Transport).Host;
  TGRIDTransportIndyTCP(FClient.Transport).Port := TGRIDTransportIndyTCP(form30.Cli.Transport).Port;
  if Not(FClient.Connect('admin','admin').Status) then
    raise Exception.Create('Error Message');

  //Step one : Get remote computer information.
  inf := FClient.Infos;
  busmessage.FromString('INF:'+inf.ServerHostCPUArchitecture);
  Bus.Send(busmessage,'HiMan');
  busmessage.FromString('NME:'+inf.GRIDServerName+' ('+inf.GRIDVersion+') - Compiled on '+inf.GRIDCompiler);
  Bus.Send(busmessage,'HiMan');

  //Step 2 : Give sensors informations every second.
  while not(Terminated) do
  begin
    busMessage.FromString('CPU:'+FloatToStr(Round(FClient.InfosCPULevel)));
    Bus.Send(busmessage,'HiMan');

    Sleep(1000);
    t := FClient.instantPythonRun(CST_PYTHONCODE_GETSENSORDATA);
    if Fclient.LastStatus then
      busmessage.FromString(t)
    else
    begin
      busmessage.FromString('ERR:'+Fclient.LastStatusInfo);
      Terminate;
    end;

    Bus.Send(busmessage,'HiMan');
  end;

  FClient.Disconnect;
  FreeAndNil(Fclient);
end;

end.
