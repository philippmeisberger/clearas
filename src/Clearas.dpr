program Clearas;

{$R *.dres}

uses
  Forms,
{$IFNDEF DEBUG}
  PMCW.Application.Mutex,
{$ENDIF}
  ClearasMain in 'ClearasMain.pas' {Main},
  ClearasAPI in 'ClearasAPI.pas',
  Clearas.ListColumns in 'Clearas.ListColumns.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
  Application.Title := 'Clearas';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
