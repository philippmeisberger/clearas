program Clearas;

{$R 'changelog.res' 'changelog.rc'}
{$R 'description.res' 'description.rc'}

uses
  Forms,
  PMCW.Application.Mutex,
  ClearasMain in 'ClearasMain.pas' {Main},
  ClearasAPI in 'ClearasAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clearas';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
