program Clearas;

uses
  Forms,
  PMCW.Mutex in 'PMCW.Mutex.pas',
  AdminManifest in 'AdminManifest.pas',
  ClearasMain in 'ClearasMain.pas' {Main},
  ClearasInfo in 'ClearasInfo.pas' {Info},
  ClearasAPI in 'ClearasAPI.pas',
  StartupSearchThread in 'StartupSearchThread.pas',
  ContextSearchThread in 'ContextSearchThread.pas',
  ExportListThread in 'ExportListThread.pas',
  PMCW.Updater in 'PMCW.Updater.pas' {Update};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clearas';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
