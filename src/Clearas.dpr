program Clearas;

{$R 'changelog.res' 'changelog.rc'}
{$R 'description.res' 'description.rc'}

uses
  Forms,
{$IFNDEF DEBUG}
  AdminManifest in 'AdminManifest.pas',
{$ENDIF}
  PMCWMutex in 'PMCWMutex.pas',
  ClearasMain in 'ClearasMain.pas' {Main},
  ClearasAPI in 'ClearasAPI.pas',
  ExportListThread in 'ExportListThread.pas',
  ClearasSearchThread in 'ClearasSearchThread.pas',
  PMCWAbout in 'PMCWAbout.pas',
  PMCWUpdater in 'PMCWUpdater.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clearas';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
