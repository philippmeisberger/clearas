program Clearas;

uses
  Forms,
  Mutex in 'Mutex.pas',
  LanguageFile in 'LanguageFile.pas',
  AdminManifest in 'AdminManifest.pas',
  ClearasMain in 'ClearasMain.pas' {Main},
  ClearasInfo in 'ClearasInfo.pas' {Info},
  Updater in 'Updater.pas' {Update},
  ClearasAPI in 'ClearasAPI.pas',
  StartupSearchThread in 'StartupSearchThread.pas',
  ContextSearchThread in 'ContextSearchThread.pas',
  ContextMenuSearchThread in 'ContextMenuSearchThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clearas';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
