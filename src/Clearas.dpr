program Clearas;

uses
  Forms,
  Mutex in 'Mutex.pas',
  ClearasMain in 'ClearasMain.pas' {Main},
  ClearasInfo in 'ClearasInfo.pas' {Info},
  Updater in 'Updater.pas' {Update},
  ClearasAPI in 'ClearasAPI.pas',
  AdminManifest in 'AdminManifest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clearas';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
