{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Updater v2.3                              }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Updater;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  SysUtils, Classes, Vcl.Dialogs, PMCW.UpdateCheckThread, PMCW.DownloadThread,
  PMCW.OSUtils, PMCW.LanguageFile, PMCW.Dialogs,

{$IFDEF MSWINDOWS}
  Windows, FileCtrl, Forms, StdCtrls, ComCtrls, Controls, System.Win.TaskbarCore,
  Vcl.Taskbar;
{$ELSE}
  LCLType;
{$ENDIF}

const
  URL_DOWNLOAD = URL_DIR + 'downloader.php?file=';

type
  { IUpdateListener }
  IUpdateListener = interface
  ['{D1CDAE74-717A-4C5E-9152-15FBA4A15552}']
    procedure OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
  end;

  { TUpdateCheck }
  TUpdateCheck = class(TObject)
  private
    FLang: TLanguageFile;
    FUserUpdate: Boolean;
    FRemoteDirName: string;
    FNewBuild: Cardinal;
    { TUpdateCheckThread events }
    procedure OnCheckError(Sender: TThread; AResponseCode: Integer);
    procedure OnNoUpdateAvailable(Sender: TObject);
    procedure OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
  protected
    FListeners: TInterfaceList;
  public
    constructor Create(ARemoteDirName: string; ALang: TLanguageFile); overload;
    constructor Create(AOwner: TComponent; ARemoteDirName: string;
      ALang: TLanguageFile); overload;
    destructor Destroy; override;
    procedure AddListener(AListener: IUpdateListener);
    procedure CheckForUpdate(AUserUpdate: Boolean; ACurrentBuild: Cardinal = 0);
    procedure RemoveListener(AListener: IUpdateListener);
  end;

{$IFDEF MSWINDOWS}
  { TUpdate }
  TUpdate = class(TForm)
    pbProgress: TProgressBar;
    bFinished: TButton;
    lSize: TLabel;
    procedure bFinishedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    FOnUserCancel: TNotifyEvent;
    FThreadRuns: Boolean;
    FDownloadDirectory, FTitle, FRemoteFileName, FLocalFileName, FFileName: string;
    FLang: TLanguageFile;
    FListeners: TInterfaceList;
    FTaskBar: TTaskbar;
    procedure OnDownloadCancel(Sender: TObject);
    procedure OnDownloadError(Sender: TThread; AResponseCode: Integer);
    procedure OnDownloadFinished(Sender: TObject);
    procedure OnDownloading(Sender: TThread; ADownloadSize: Int64);
    procedure OnDownloadStart(Sender: TThread; AFileSize: Int64);
    procedure Reset();
  protected
    function Download(ARemoteFileName, ALocalFileName: string;
      ADownloadDirectory: string = ''): Boolean;
  public
    constructor Create(AOwner: TComponent; ALang: TLanguageFile);
    destructor Destroy; override;
    procedure AddListener(AListener: IUpdateListener);
    function Execute(): Boolean;
    procedure RemoveListener(AListener: IUpdateListener);
    { external }
    property DownloadDirectory: string read FDownloadDirectory write FDownloadDirectory;
    property FileNameLocal: string read FLocalFileName write FLocalFileName;
    property FileNameRemote: string read FRemoteFileName write FRemoteFileName;
    property LanguageFile: TLanguageFile read FLang write FLang;
    property Title: string read FTitle write FTitle;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}

{ TUpdateCheck }

{ public TUpdateCheck.Create

  Constructor for creating an TUpdateCheck instance. }

constructor TUpdateCheck.Create(ARemoteDirName: string; ALang: TLanguageFile);
begin
  inherited Create;
  FLang := ALang;
  FRemoteDirName := ARemoteDirName;
  FListeners := TInterfaceList.Create;
end;

{ public TUpdateCheck.Create

  Constructor for creating an TUpdateCheck instance. }

constructor TUpdateCheck.Create(AOwner: TComponent; ARemoteDirName: string;
  ALang: TLanguageFile);
begin
  Create(ARemoteDirName, ALang);

  // Add owner to list to receive events
  if Assigned(AOwner) then
    FListeners.Add(AOwner);
end;

{ public TUpdateCheck.Destroy

  Destructor for destroying an TUpdateCheck instance. }

destructor TUpdateCheck.Destroy;
begin
  FreeAndNil(FListeners);
  inherited Destroy;
end;

{ private TUpdateCheck.OnCheckError

  Event method that is called TUpdateCheckThread when error occurs while
  searching for update. }

procedure TUpdateCheck.OnCheckError(Sender: TThread; AResponseCode: Integer);
begin
  if FUserUpdate then
    if (AResponseCode <> -1) then
      FLang.ShowException(FLang.GetString([12, 13]), FLang.Format(19, [AResponseCode]))
    else
      FLang.ShowMessage(12, 13, mtError);
end;

{ private TUpdateCheck.OnNoUpdateAvailable

  Event method that is called when TUpdateCheckThread search returns no update. }

procedure TUpdateCheck.OnNoUpdateAvailable(Sender: TObject);
begin
  if FUserUpdate then
    FLang.ShowMessage(FLang.GetString(23));
end;

{ private TUpdateCheck.OnUpdateAvailable

  Event method that is called when TUpdateCheckThread search returns an update. }

procedure TUpdateCheck.OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
var
  i: Word;
  Listener: IUpdateListener;

begin
  if (FNewBuild <> ANewBuild) then
    // Store newest build
    FNewBuild := ANewBuild;

  // Notify all listeners
  for i := 0 to FListeners.Count - 1 do
    if Supports(FListeners[i], IUpdateListener, Listener) then
      Listener.OnUpdate(Self, ANewBuild);
end;

{ public TUpdateCheck.AddListener

  Adds a listener to the notification list. }

procedure TUpdateCheck.AddListener(AListener: IUpdateListener);
begin
  FListeners.Add(AListener);
end;

{ public TUpdateCheck.CheckForUpdate

  Searches for update on HTTP server. }

procedure TUpdateCheck.CheckForUpdate(AUserUpdate: Boolean; ACurrentBuild: Cardinal = 0);
begin
  FUserUpdate := AUserUpdate;

  // Update already available?
  if (FNewBuild > 0) then
  begin
    OnUpdateAvailable(nil, FNewBuild);
    Abort;
  end;  //of begin

  if (ACurrentBuild = 0) then
    ACurrentBuild := GetBuildNumber();

  // Search for update
  with TUpdateCheckThread.Create(ACurrentBuild, FRemoteDirName) do
  begin
    OnUpdate := OnUpdateAvailable;
    OnNoUpdate := OnNoUpdateAvailable;
    OnError := OnCheckError;
    Start;
  end;  //of with
end;

{ public TUpdateCheck.RemoveListener

  Removes a listener from the notification list. }

procedure TUpdateCheck.RemoveListener(AListener: IUpdateListener);
begin
  FListeners.Remove(AListener);
end;

{$IFDEF MSWINDOWS}

{ TUpdate }

{ public TUpdate.Create

  Constructor for creating an TUpdate instance. }

constructor TUpdate.Create(AOwner: TComponent; ALang: TLanguageFile);
begin
  inherited Create(AOwner);
  FLang := ALang;
  FThreadRuns := False;

  // Init list of listeners
  FListeners := TInterfaceList.Create;

  // Add owner to list to receive events
  if Assigned(AOwner) then
    FListeners.Add(AOwner);

  FTaskBar := TTaskbar.Create(Self);
end;

{ public TUpdate.Destroy

  Destructor for destroying an TUpdate instance. }

destructor TUpdate.Destroy;
begin
  FTaskBar.ProgressState := TTaskBarProgressState.None;
  FTaskBar.Free;
  FreeAndNil(FListeners);
  inherited Destroy;
end;

{ private TUpdate.FormShow

  Event that is called when form is shown. }

procedure TUpdate.FormShow(Sender: TObject);
begin
  Caption := FTitle;
end;

{ private TUpdate.OnDownloadCancel

  Event method that is called by TDownloadThread when user canceled downlad. }

procedure TUpdate.OnDownloadCancel(Sender: TObject);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Error;
  pbProgress.State := TProgressBarState.pbsError;
  Reset();
  FLang.ShowMessage(FLang.GetString(30));
  bFinished.ModalResult := mrCancel;
end;

{ private TUpdate.OnDownloadError

  Event method that is called by TDownloadThread when an error occurs while
  downloading the update. }

procedure TUpdate.OnDownloadError(Sender: TThread; AResponseCode: Integer);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Error;
  pbProgress.State := TProgressBarState.pbsError;
  Reset();
  FLang.ShowException(Caption + FLang.GetString(18), FLang.Format(19, [AResponseCode]));
  bFinished.ModalResult := mrAbort;
end;

{ private TUpdate.OnDownloadFinished

  Event method that is called by TDownloadThread when download is finished. }

procedure TUpdate.OnDownloadFinished(Sender: TObject);
begin
  // Caption "finished"
  bFinished.Caption := FLang.GetString(8);
  bFinished.SetFocus;
  FThreadRuns := False;
  FTaskBar.ProgressState := TTaskBarProgressState.None;

{$IFDEF MSWINDOWS}
  // Show dialog to add certificate
  if (ExtractFileExt(FFileName) = '.reg') then
    ShowAddRegistryDialog('"'+ FFileName +'"');
{$ENDIF}
  FlashWindow(Application.Handle, True);
  bFinished.ModalResult := mrOk;
end;

{ private TUpdate.OnDownloading

  Event method that is called by TDownloadThread when download is in progress. }

procedure TUpdate.OnDownloading(Sender: TThread; ADownloadSize: Int64);
begin
  pbProgress.Position := ADownloadSize;
  FTaskBar.ProgressValue := ADownloadSize;
  lSize.Caption := Format('%d/%d KB', [ADownloadSize, pbProgress.Max]);
end;

{ private TUpdate.OnDownloadStart

  Event method that is called by TDownloadThread when download starts. }

procedure TUpdate.OnDownloadStart(Sender: TThread; AFileSize: Int64);
begin
  FTaskBar.ProgressMaxValue := AFileSize;
  FTaskBar.ProgressState := TTaskBarProgressState.Normal;
  pbProgress.Max := AFileSize;
  BringToFront;
end;

{ private TUpdate.Reset

  Resets Update GUI. }

procedure TUpdate.Reset();
begin
  lSize.Caption := FLang.GetString(7);
  bFinished.Caption := FLang.GetString(8);
  FThreadRuns := False;
end;

{ protected TUpdate.Download

  Starts downloading a file. }

function TUpdate.Download(ARemoteFileName, ALocalFileName: string;
  ADownloadDirectory: string = ''): Boolean;
var
  Url: string;
  Continue: Boolean;

begin
  FRemoteFileName := ARemoteFileName;
  FLocalFileName := ALocalFileName;

  // Download folder still set?
  if (ADownloadDirectory <> '') then
    Continue := True
  else
    // Show select directory dialog
    Continue := SelectDirectory(FLang.GetString(9), '', ADownloadDirectory);

  if Continue then
  begin
    Url := URL_DOWNLOAD + FRemoteFileName;
    FFileName := IncludeTrailingPathDelimiter(ADownloadDirectory) + FLocalFileName;

    // Try to init thread
    try
      with TDownloadThread.Create(Url, FFileName) do
      begin
        // Link download events
        FOnUserCancel := OnUserCancel;

        // Link TProgressBar events and start download thread
        OnDownloading := Self.OnDownloading;
        OnCancel := OnDownloadCancel;
        OnStart := OnDownloadStart;
        OnFinish := OnDownloadFinished;
        OnError := OnDownloadError;
        Start;
      end;  //of with

      // Caption "cancel"
      bFinished.Caption := FLang.GetString(6);
      FThreadRuns := True;

    except
      OnDownloadError(nil, -2);
    end;  //of try
  end  //of begin
  else
    // Cancel clicked
    Reset();

  BringToFront;
  ShowModal;
  Result := (bFinished.ModalResult = mrOk);
end;

{ public TUpdate.AddListener

  Adds a listener to the notification list. }

procedure TUpdate.AddListener(AListener: IUpdateListener);
begin
  FListeners.Add(AListener);
end;

{ public TUpdate.Execute

  Executes the dialog. }

function TUpdate.Execute(): Boolean;
begin
  if ((FRemoteFileName = '') or (FLocalFileName = '')) then
    raise EInvalidArgument.Create('Missing argument: "RemoteFileName" or "LocalFileName"!');

  Result := Download(FRemoteFileName, FLocalFileName, FDownloadDirectory);
end;

{ public TUpdate.RemoveListener

  Removes a listener from the notification list. }

procedure TUpdate.RemoveListener(AListener: IUpdateListener);
begin
  FListeners.Remove(AListener);
end;

{ TUpdate.bFinishedClick

  Cancels download or closes update form when user clicks. }

procedure TUpdate.bFinishedClick(Sender: TObject);
begin
  Close;
end;

{ TUpdate.FormCloseQuery

  VCL event that is called before destructor is called. }

procedure TUpdate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Download still in progress?
  if FThreadRuns then
  begin
    // Cancel download
    FOnUserCancel(Self);
    CanClose := False;
  end  //of begin
  else
    CanClose := True;
end;
{$ENDIF}

end.
