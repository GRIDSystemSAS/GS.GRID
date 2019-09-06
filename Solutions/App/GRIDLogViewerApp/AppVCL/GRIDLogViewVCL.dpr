program GRIDLogViewVCL;









uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ELSE}
  ScaleMM2,
  {$ENDIF }
  Vcl.Forms,
  GS.Bus,
  gridlog.types in '..\common\gridlog.types.pas',
  gridlog.backend in '..\models\gridlog.backend.pas',
  gridlog.flogview in '..\views\gridlog.flogview.pas',
  gridlog.fmain in '..\views\gridlog.fmain.pas',
  gridlog.backendClientAPI in '..\controlers\gridlog.backendClientAPI.pas',
  gridlog.Controler.VCL in '..\controlers\gridlog.Controler.VCL.pas',
  gridlog.Controler in '..\controlers\gridlog.Controler.pas',
  gridlog.fintro in '..\views\gridlog.fintro.pas' {fIntro};

{$R *.res}

begin
  Gs.Bus.StartStandartBus;
  startbackend;
  Application.Initialize;
  Application.CreateForm(TfMainView, fMainView);
  Application.CreateForm(TdmFront, dmFront);
  Application.CreateForm(TfIntro, fIntro);
  Application.MainFormOnTaskbar := True;
  Application.Run;
  stopbackend;
  Gs.Bus.ReleaseStandartBus;
end.
