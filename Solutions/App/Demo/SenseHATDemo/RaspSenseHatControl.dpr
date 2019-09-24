program RaspSenseHatControl;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmain in 'fmain.pas' {Form30},
  GS.GRID.Client.KissB in '..\..\..\..\Sources\Client\GS.GRID.Client.KissB.pas',
  GS.GRID.Client in '..\..\..\..\Sources\Client\GS.GRID.Client.pas',
  GS.GRID.Client.Resolver in '..\..\..\..\Sources\Client\GS.GRID.Client.Resolver.pas',
  GS.GRID.Client.Transport in '..\..\..\..\Sources\Client\GS.GRID.Client.Transport.pas',
  GS.GRID.Client.Transport.IndyTCP in '..\..\..\..\Sources\Client\ClientImplementation\GS.GRID.Client.Transport.IndyTCP.pas',
  GS.GRID.Common.Protocols in '..\..\..\..\Sources\Common\GS.GRID.Common.Protocols.pas',
  GS.GRID.Common.Protocols.KissB in '..\..\..\..\Sources\Common\GS.GRID.Common.Protocols.KissB.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm30, Form30);
  Application.Run;
end.
