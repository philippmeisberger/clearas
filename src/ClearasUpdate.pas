{ *********************************************************************** }
{                                                                         }
{ Clearas Download Unit                                                   }
{                                                                         }
{ Copyright (c) 2011-2013 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit ClearasUpdate;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, IdException,
  StdCtrls, ComCtrls, FileCtrl, ClearasAPI, ClearasUpdateThread, ShellApi,
  UpdateCheckThread;

type
  { Threadsteuerung Events }
  TOnCancelEvent = procedure(Sender: TObject) of object;
  TOnContinueEvent = procedure(Sender: TObject) of object;
  TOnExitEvent = procedure(Sender: TObject) of object;

  { Download Modus }
  TUpdateType = (utUpdate, utCert);

  TForm3 = class(TForm)
    pbProgress: TProgressBar;
    bFinished: TButton;
    lSize: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure bFinishedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOnCancel: TOnCancelEvent;
    FOnContinue: TOnContinueEvent;
    FOnExit: TOnExitEvent;
    FUpdateType: TUpdateType;
    FUpdateExists, FUserUpdate: Boolean;
    FFileName: string;
    procedure Cancel;
    { Download-Thread Events }
    procedure OnThreadError(Sender: TThread);
    procedure OnThreadWork(Sender: TThread; const AWorkCount: Integer);
    procedure OnThreadWorkBegin(Sender: TThread; const AWorkCountMax: Integer);
    procedure OnThreadWorkEnd(Sender: TObject; AResponseCode: Integer);
    { Updatecheck-Thread Events }
    procedure OnCheckError(Sender: TThread);
    procedure OnNoUpdateAvailable(Sender: TThread);
    procedure OnUpdateAvailable(Sender: TThread; const ANewBuild: integer);
  public
    procedure CheckForUpdate;
    procedure DoUpdate;
    procedure Initialize(AFormCaptionID: Word; AUpdateType: TUpdateType);
    property OnCancel: TOnCancelEvent read FOnCancel write FOnCancel;
    property OnContinue: TOnContinueEvent read FOnContinue write FOnContinue;
    property OnExit: TOnExitEvent read FOnExit write FOnExit;
    property UpdateExists: Boolean read FUpdateExists;
    property UserUpdate: Boolean read FUserUpdate write FUserUpdate;
  end;

var
  Form3: TForm3;

implementation

uses ClearasMain;

{$R *.dfm}


procedure TForm3.Cancel;                                           //"Abbrechen"
begin
  DeleteFile(FFileName);                                //Datei löschen
  lSize.Caption := TClearas.GetString(95);
  bFinished.Caption := TClearas.GetString(93);
  bFinished.Enabled := true;

  case FUpdateType of
    utUpdate:  Form1.mmUpdate.Enabled := true;          //falls Update-Fehler
    utCert:    Form1.mmDwnldCert.Enabled := true;       //falls Zertifikat-Fehler
  end;  //of case
end;


procedure TForm3.FormActivate(Sender: TObject);                       //Download
var
  dir : string;

  function Rename(ADirectory, AName: string; AIndex: integer): string;   //Vermeidung von gleichen Dateinamen
  begin
    if FileExists(ADirectory + AName) then
       begin
       Inc(AIndex);
       AName := '\Clearas('+IntToStr(AIndex)+').exe';   //umbenennen
       result := Rename(ADirectory, AName, AIndex);     //probieren
       end  //of begin
    else
       result := ADirectory + AName;                    //fertig
  end;

begin
  bFinished.Caption := TClearas.GetString(93);

  if SelectDirectory(TClearas.GetString(92), '', dir) then   //"Ordner wählen" - Dialog
     try
       with TDownloadThread.Create do             //init Thread (suspended!)
         begin
         OnWork := OnThreadWork;                  //Events verknüpfen
         OnWorkBegin := OnThreadWorkBegin;
         OnFinish := OnThreadWorkEnd;
         OnError := OnThreadError;

         case FUpdateType of                      //Welcher Download...
            utUpdate:
              begin
              FileName := Rename(dir, '\Clearas.exe', 0);  //Dateinamen übergeben
              Url := URL_DIR +'downloader.php?file=clearas.exe';
              end;  //of begin

            utCert:
              begin
              FileName := dir + '\Install_PMCW_Cert.reg';
              Url := URL_DIR +'downloader.php?file=cert.reg';
              end;  //of begin
         end;  //of case

         FFileName := FileName;
         Resume;                                      //Thread starten
         end;  //of with

     except
       OnThreadError(Sender as TThread);              //im Fehlerfall
     end  //of except
  else
     Cancel;                                          //"Abbrechen"-Klick
end;


procedure TForm3.FormCreate(Sender: TObject);
begin
  inherited;
  FUpdateExists := false;
  FUserUpdate := false;
  CheckForUpdate;
end;


procedure TForm3.bFinishedClick(Sender: TObject);                       //Fertig
begin
  if (bFinished.Caption = TClearas.GetString(93)) then  //falls Thread fertig...
     begin
     bFinished.Enabled := false;           //Button deaktivieren
     Hide;                                 //Download Form verstecken
     Position := poScreenCenter;           //Form zentrieren

     if (pbProgress.Position <> 0) then    //falls ProgressBar nicht auf 0...
        pbProgress.Position := 0;          //Reset ProgressBar

     lSize.Caption := '000/000KB';         //Reset Datenzählers

     with Form1 do
       begin
       Enabled := true;                    //Main Form aktivieren
       BringToFront;                       //Main Form zeigen
       end;  //of with
     end  //of begin
  else
     begin                                 //falls Thread nicht fertig...
     OnCancel(Self);                       //Thread pausieren

     if (Form1.MessageBox(89, 2, true) = IDYes) then
        begin
        OnExit(Self);                      //Thread beenden
        Cancel;
        end  //of begin
     else
        OnContinue(Sender);                //Thread fortsetzen
     end;  //of if
end;

{ Download-Thread Events }
procedure TForm3.OnThreadError(Sender: TThread);                    //Fehlerfall
begin
  with TClearas do
    Form1.MessageBox(Caption + GetString(66) +^J+^J+ GetString(67)
                     + GetString(94), 3, true);
  Cancel;
end;


procedure TForm3.OnThreadWork(Sender: TThread; const AWorkCount: Integer);
begin
  pbProgress.Position := AWorkCount;                      //Progress anzeigen
  lSize.Caption := IntToStr(Round(AWorkCount/1024)) +'/'+
                    FloatToStr(Round(pbProgress.Max/1024))+'KB';  //Dateigröße
end;


procedure TForm3.OnThreadWorkBegin(Sender: TThread; const AWorkCountMax: Integer);
begin
  pbProgress.Max := AWorkCountMax;
  bFinished.Caption := TClearas.GetString(96);
  bFinished.Enabled := true;
end;


procedure TForm3.OnThreadWorkEnd(Sender: TObject; AResponseCode: integer);
begin
  bFinished.Caption := TClearas.GetString(93);        //"Fertig!"
  bFinished.SetFocus;                                 //Button anwählen

  if (FUpdateType = utUpdate) then
     Form1.mmUpdate.Caption := TClearas.GetString(91)  //Button "Update suchen" Reset
  else
    if ((FUpdateType = utCert) and (lSize.Caption <> TClearas.GetString(95))) then //falls Zertifikat heruntergeladen wurde...
       ShellExecute(0, nil, PChar('regedit'), PChar(ExtractFileName(FFileName)),
                    PChar(ExtractFilePath(FFileName)), SW_SHOWNORMAL);            //Dialog: Datei in REG einbinden?
end;

{ Updatecheck-Thread Events }
procedure TForm3.OnCheckError(Sender: TThread);
begin
  if FUserUpdate then
     Form1.MessageBox(TClearas.GetString(104) +^J+ TClearas.GetString(105), 3, true);
end;


procedure TForm3.OnNoUpdateAvailable(Sender: TThread);
begin
  if FUserUpdate then
     Form1.MessageBox(99, 0, true);
end;


procedure TForm3.OnUpdateAvailable(Sender: TThread; const ANewBuild: integer);
begin
  FUpdateExists := true;
  Form1.mmUpdate.Caption := TClearas.GetString(100);      //Button "Update suchen" umbenennen

  if (Form1.MessageBox(98, 2, true) = IDYes) then         //Frage, ob Update heruntergeladen werden soll
     DoUpdate;
end;
{ of Threads }

{ public }
procedure TForm3.CheckForUpdate;                            //nach Update suchen
begin
  with TUpdateCheckThread.Create(TClearas.GetBuildNumber) do //init Thread (suspended!)
    begin
    OnUpdate := OnUpdateAvailable;                          //Event verknüpfen
    OnNoUpdate := OnNoUpdateAvailable;
    OnError := OnCheckError;
    Resume;                                                 //Thread starten
    end;  //of with
end;


procedure TForm3.DoUpdate;                                      //Update starten
begin
  Form1.mmUpdate.Enabled := false;
  Form1.Enabled := false;
  Initialize(100, utUpdate);
end;  //of begin


procedure TForm3.Initialize(AFormCaptionID: Word; AUpdateType: TUpdateType);  //Form anzeigen
begin
  Caption := TClearas.GetString(AFormCaptionID);
  FUpdateType := AUpdateType;
  Show;
  BringToFront;
end;

end.