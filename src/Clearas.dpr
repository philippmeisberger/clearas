program Clearas;

uses
  Forms,
  ClearasMutex in 'ClearasMutex.pas',
  ClearasInfo in 'ClearasInfo.pas' {Form2},
  ClearasAPI in 'ClearasAPI.pas',
  ClearasMain in 'ClearasMain.pas' {Form1},
  UpdateCheckThread in 'UpdateCheckThread.pas',
  ClearasUpdate in 'ClearasUpdate.pas' {Form3},
  ClearasUpdateThread in 'ClearasUpdateThread.pas',
  WinUtils in 'WinUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clearas';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
