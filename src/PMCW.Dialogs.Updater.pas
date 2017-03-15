{ *********************************************************************** }
{                                                                         }
{ PM Code Works Updater v3.1                                              }
{                                                                         }
{ Copyright (c) 2011-2017 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs.Updater;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Dialogs, Vcl.Forms,
{$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
{$WARN UNIT_PLATFORM ON}
  Vcl.StdCtrls, Vcl.ComCtrls, System.UITypes, System.Win.TaskbarCore, Vcl.Consts,
  System.Win.Registry, Vcl.Taskbar, System.Net.HttpClient, System.NetConsts,
  System.Net.URLClient, PMCW.LanguageFile, PMCW.FileSystem, PMCW.CA;
{$ENDIF}

const
  /// <summary>
  ///   URL to the website.
  /// </summary>
  URL_BASE           = 'http://www.pm-codeworks.de/';

  /// <summary>
  ///   URL to the download (base) directory on website.
  /// </summary>
  URL_DIR            = URL_BASE + 'media/';

  /// <summary>
  ///   URL to the PHP web downloader script.
  /// </summary>
  URL_DOWNLOAD       = URL_DIR + 'downloader.php?file=';

  /// <summary>
  ///   URL to the report bug formular on the website.
  /// </summary>
  URL_CONTACT        = URL_BASE +'kontakt.html';

{$IFDEF MSWINDOWS}
  /// <summary>
  ///   The version of the updater.
  /// </summary>
  UPDATER_VERSION    = '3.1';

  /// <summary>
  ///   The used user-agent string during the HTTP(S) connection.
  /// </summary>
  UPDATER_USER_AGENT = 'Updater/'+ UPDATER_VERSION +' (PM Code Works Update Utility)';

type
  /// <summary>
  ///   Occurs when something went wrong during the HTTP connection.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="AResponseCode">
  ///   The HTTP response code.
  /// </param>
  /// <param name="AResponseText">
  ///   The HTTP response text.
  /// </param>
  THttpErrorEvent = procedure(Sender: TObject; const AResponseCode: Integer;
    const AResponseText: string) of object;

  /// <summary>
  ///   Occurs when an update is available.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="ANewBuild">
  ///   The newest build number.
  /// </param>
  TUpdateAvailableEvent = procedure(Sender: TObject; const ANewBuild: Cardinal) of object;

  /// <summary>
  ///   A <c>TUpdateCheckThread</c> downloads the version.txt from the website
  ///   and notifies about update status.
  /// </summary>
  TUpdateCheckThread = class(TThread)
  private
    FHttpClient: THttpClient;
    FResponseCode: Integer;
    FResponseText: string;
    FOnFinish: TUpdateAvailableEvent;
    FOnError: THttpErrorEvent;
    FRemoteDirName: string;
    FRemoteBuild: Cardinal;
    procedure NotifyOnError();
    procedure NotifyOnFinish();
  protected
    procedure Execute(); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TUpdateCheckThread</c> instance.
    /// </summary>
    /// <param name="ACurrentBuild">
    ///   The build number of the current program.
    /// </param>
    /// <param name="ARemoteDirName">
    ///   The directory on website which contains the version.txt file.
    /// </param>
    constructor Create(const ARemoteDirName: string);

    /// <summary>
    ///   Destructor for destroying a <c>TUpdateCheckThread</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Occurs when search for update fails.
    /// </summary>
    property OnError: THttpErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Occurs when checking has finished.
    /// </summary>
    property OnFinish: TUpdateAvailableEvent read FOnFinish write FOnFinish;
  end;

  /// <summary>
  ///   The <c>TUpdateCheck</c> is a light-weight async update notificator to
  ///   inform a user that a new version of the current PMCW project is available.
  ///   It uses HTTP to download a version.txt from the website that contains
  ///   the latest build number and checks if it matches the current build.
  ///   If the webside build number is higher than current build the
  ///   <see cref="OnUpdate"/> event occurs. If the build numbers match a
  ///   message is shown saying that no update is available. It supports
  ///   different UI translations.
  /// </summary>
  TUpdateCheck = class(TObject)
  private
    FLanguageFile: TLanguageFile;
    FNotifyNoUpdate: Boolean;
    FRemoteDirName: string;
    FOnUpdate: TUpdateAvailableEvent;
    FCurrentBuild,
    FRemoteBuild: Cardinal;
    FThread: TUpdateCheckThread;
    procedure HttpError(Sender: TObject; const AResponseCode: Integer;
      const AResponseText: string);
    procedure CheckFinished(Sender: TObject; const ARemoteBuild: Cardinal);
  public
    /// <summary>
    ///    Constructor for creating a <c>TUpdateCheck</c> instance.
    /// </summary>
    /// <param name="ARemoteDirName">
    ///   The directory on website which contains the version.txt file.
    /// </param>
    /// <param name="ALanguageFile">
    ///   The specific user interface translation file to use.
    /// </param>
    constructor Create(const ARemoteDirName: string; ALanguageFile: TLanguageFile);

    /// <summary>
    ///   Searches for update on an HTTP server.
    /// </summary>
    procedure CheckForUpdate();

    /// <summary>
    ///   Set to <c>True</c> to show a message if no update is available.
    ///   Otherwise no message is shown. Comes in handy when update should be
    ///   searched on startup of application: User would get always the message
    ///   that no update is avaiable. This could be annoying.
    /// </summary>
    property NotifyNoUpdate: Boolean read FNotifyNoUpdate write FNotifyNoUpdate;

    /// <summary>
    ///   Occurs when an update is available.
    /// </summary>
    property OnUpdate: TUpdateAvailableEvent read FOnUpdate write FOnUpdate;
  end;

const
  /// <summary>
  ///   Error saying that something went wrong with server certificate validation.
  /// </summary>
  ERROR_CERTIFICATE_VALIDATION = -2;

type
  /// <summary>
  ///   Occurs when download is in progress.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="AContentLength">
  ///   The total size (in KB) of the update.
  /// </param>
  /// <param name="AReadCount">
  ///   The already downloaded KB of the update.
  /// </param>
  TDownloadingEvent = procedure(Sender: TObject; AContentLength, AReadCount: Int64) of object;

  /// <summary>
  ///   A <c>TDownloadThread</c> downloads a file from an URL using TLS (default).
  ///   In case of error a precise description will be returned in the
  ///   <see cref="OnError"/> event. The progress of the download can be seen
  ///   in the <see cref="OnDownloading"/> event. Of course the download can be
  ///   canceled: Just use the <c>Terminate</c> method.
  /// </summary>
  TDownloadThread = class(TThread)
  private
    FHttpClient: THttpClient;
    FOnDownloading: TDownloadingEvent;
    FOnError: THttpErrorEvent;
    FOnFinish,
    FOnCancel: TNotifyEvent;
    FContentLength,
    FReadCount: Int64;
    FFileName,
    FUrl,
    FResponseText: string;
    FResponseCode: Integer;
    FTLSEnabled: Boolean;
    procedure NotifyOnCancel();
    procedure NotifyOnDownloading();
    procedure NotifyOnError();
    procedure NotifyOnFinish();
    procedure Downloading(const Sender: TObject; AContentLength, AReadCount: Int64;
      var AAbort: Boolean);
    procedure ValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const ACertificate: TCertificate; var AAccepted: Boolean);
    procedure SetTlsEnabled(const ATlsEnabled: Boolean);
  protected
    procedure Execute(); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TDownloadThread</c> instance.
    /// </summary>
    /// <param name="AUrl">
    ///   The complete URL to the file that should be downloaded.
    /// </param>
    /// <param name="AFileName">
    ///   The filename under which the downloaded file should be stored.
    /// </param>
    /// <param name="AAllowOverwrite">
    ///   If set to <c>True</c> and the file was already downloaded then this
    ///   will be overwritten. Otherwise the existing file is kept and a new
    ///   file with a number suffix is created.
    /// </param>
    constructor Create(const AUrl, AFileName: string; AAllowOverwrite: Boolean = False);

    /// <summary>
    ///   Destructor for destroying a <c>TDownloadThread</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Creates an unique filename to be sure downloading to a non-existing
    ///   file. If the file already exists a number suffix is appended to this
    ///   filename.
    /// </summary>
    /// <returns>
    ///   The unique filename
    /// </returns>
    function GetUniqueFileName(const AFileName: string): string;

    /// <summary>
    ///   Occurs when download has been canceled by user.
    /// </summary>
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;

    /// <summary>
    ///   Occurs when download is in progress.
    /// </summary>
    property OnDownloading: TDownloadingEvent read FOnDownloading write FOnDownloading;

    /// <summary>
    ///   Occurs when an error occurs while downloading.
    /// </summary>
    property OnError: THttpErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Occurs when the download has finished.
    /// </summary>
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;

    /// <summary>
    ///   Gets or sets the usage of TLS.
    /// </summary>
    property TLSEnabled: Boolean read FTLSEnabled write SetTlsEnabled;
  end;

  /// <summary>
  ///   <c>TUpdateDialog</c> is a dialog which downloads a file from the website
  ///   and shows the progress in a <c>TProgressBar</c> and on the taskbar. The
  ///   download is encrypted using TLS per default.
  /// </summary>
  TUpdateDialog = class(TCommonDialog)
  private
    FForm: TForm;
    FProgressBar: TProgressBar;
    FButtonFinished: TButton;
    FLabelStatistic: TLabel;
    FThread: TDownloadThread;
    FDownloadDirectory,
    FRemoteFileName,
    FLocalFileName,
    FFileName: string;
    FLanguageFile: TLanguageFile;
    FTaskBar: TTaskbar;
    procedure FinishedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DownloadCanceled(Sender: TObject);
    procedure Downloading(Sender: TObject; AContentLength, AReadCount: Int64);
    procedure Reset();
    procedure DownloadFinished(Sender: TObject);
    procedure DownloadError(Sender: TObject; const AResponseCode: Integer;
      const AResponseText: string);
    function GetTitle: string;
    procedure SetTitle(const ATitle: string);
  public
    /// <summary>
    ///   Constructor for creating a <c>TUpdate</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   The owner.
    /// </param>
    constructor Create(AOwner: TComponent); overload; override;

    /// <summary>
    ///   Constructor for creating a <c>TUpdate</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   The owner.
    /// </param>
    /// <param name="ALanguageFile">
    ///   The user interface translation file to use.
    /// </param>
    constructor Create(AOwner: TComponent; ALanguageFile: TLanguageFile); reintroduce; overload;

    /// <summary>
    ///   Destructor for destroying a <c>TUpdate</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Executes the update progress.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if downloading was sucessful or <c>False</c> otherwise.
    /// </returns>
    function Execute(ParentHwnd: HWND): Boolean; override;

    /// <summary>
    ///   Launches the downloaded setup.
    /// </summary>
    procedure LaunchSetup();

    /// <summary>
    ///   Download the file into this directory.
    /// </summary>
    property DownloadDirectory: string read FDownloadDirectory write FDownloadDirectory;

    /// <summary>
    ///   The filename to use for the downloaded file.
    /// </summary>
    property FileNameLocal: string read FLocalFileName write FLocalFileName;

    /// <summary>
    ///   The filename of the file on website.
    /// </summary>
    property FileNameRemote: string read FRemoteFileName write FRemoteFileName;

    /// <summary>
    ///   The specific user interface translation file to use.
    /// </summary>
    property LanguageFile: TLanguageFile read FLanguageFile write FLanguageFile;

    /// <summary>
    ///   Gets or sets the title to use in the dialog caption.
    /// </summary>
    property Title: string read GetTitle write SetTitle;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{ TUpdateCheckThread }

constructor TUpdateCheckThread.Create(const ARemoteDirName: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FRemoteDirName := ARemoteDirName;

  // Initialize HTTP connection
  FHttpClient := THttpClient.Create;

  with FHttpClient do
  begin
    CustomHeaders['Connection'] := 'close';
    Accept := 'text/plain';
    UserAgent := UPDATER_USER_AGENT;
  end;  //of begin
end;

destructor TUpdateCheckThread.Destroy;
begin
  FreeAndNil(FHttpClient);
  inherited Destroy;
end;

procedure TUpdateCheckThread.Execute;
var
  Response: IHTTPResponse;
  Build: Integer;

begin
  try
    // Download version file for application
    Response := FHttpClient.Get(URL_DIR + FRemoteDirName +'/version.txt');
    FResponseCode := Response.StatusCode;

    // Error occured?
    if (FResponseCode <> 200) then
      raise ENetHTTPException.Create(PChar(Response.StatusText));

    // Retrieve build number
    Build := -1;

    // Invalid response?
    // Note: Also occurs when connection to update server fails
    if not (TryStrToInt(Response.ContentAsString(), Build) and (Build >= 0)) then
      raise EConvertError.Create('Error while parsing response!');

    FRemoteBuild := Build;
    Synchronize(NotifyOnFinish);

  except
    on E: EConvertError do
    begin
      FResponseCode := 406;
      FResponseText := E.Message;
      Synchronize(NotifyOnError);
    end;

    on E: Exception do
    begin
      FResponseText := E.Message;
      Synchronize(NotifyOnError);
    end;
  end;  //of try
end;

procedure TUpdateCheckThread.NotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FResponseCode, FResponseText);
end;

procedure TUpdateCheckThread.NotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self, FRemoteBuild);
end;


{ TUpdateCheck }

constructor TUpdateCheck.Create(const ARemoteDirName: string;
  ALanguageFile: TLanguageFile);
begin
  inherited Create;
  FLanguageFile := ALanguageFile;
  FRemoteDirName := ARemoteDirName;
  FNotifyNoUpdate := False;
end;

procedure TUpdateCheck.HttpError(Sender: TObject; const AResponseCode: Integer;
  const AResponseText: string);
begin
  if FNotifyNoUpdate then
  begin
    if (AResponseCode > 0) then
    begin
      FLanguageFile.ShowException(FLanguageFile.GetString([LID_UPDATE_NO_CONNECTION,
        LID_UPDATE_CHECK_CONNECTION]), AResponseText + Format(' (%d)', [AResponseCode]));
    end  //of begin
    else
    begin
      TaskMessageDlg(FLanguageFile.Strings[LID_UPDATE_NO_CONNECTION],
        FLanguageFile.Strings[LID_UPDATE_CHECK_CONNECTION], mtError, [mbOK], 0);
    end;  //of if
  end;  //of begin
end;

procedure TUpdateCheck.CheckFinished(Sender: TObject; const ARemoteBuild: Cardinal);
begin
  FRemoteBuild := ARemoteBuild;

  // Update available?
  if (FRemoteBuild > FCurrentBuild) then
  begin
    if Assigned(FOnUpdate) then
      FOnUpdate(Self, FRemoteBuild);
  end  //of begin
  else
    if FNotifyNoUpdate then
    begin
      MessageDlg(FLanguageFile.GetString(LID_UPDATE_NOT_AVAILABLE), mtInformation,
        [mbOk], 0);
    end;  //of begin

  // Clear lock (NOTE: thread is ref-counted)
  FThread := nil;
end;

procedure TUpdateCheck.CheckForUpdate();
var
  FileVersion: TFileVersion;

begin
  if Assigned(FThread) then
    Exit;

  // Update already available?
  if (FRemoteBuild > 0) then
  begin
    CheckFinished(Self, FRemoteBuild);
    Exit;
  end;  //of begin

  // Get build number of application
  if ((FCurrentBuild = 0) and FileVersion.FromFile(Application.ExeName)) then
    FCurrentBuild := FileVersion.Build;

  // Search for update
  FThread := TUpdateCheckThread.Create(FRemoteDirName);

  with FThread do
  begin
    OnError := HttpError;
    OnFinish := CheckFinished;
    Start();
  end;  //of with
end;


{ TDownloadThread }

constructor TDownloadThread.Create(const AUrl, AFileName: string;
  AAllowOverwrite: Boolean = False);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FUrl := AUrl;
  FTLSEnabled := AUrl.StartsWith('https://');

  // Rename file if already exists?
  if AAllowOverwrite then
    FFileName := AFileName
  else
    FFileName := GetUniqueFileName(AFileName);

  // Initialize HTTP connection
  FHttpClient := THttpClient.Create;

  with FHttpClient do
  begin
    OnReceiveData := Downloading;
    OnValidateServerCertificate := ValidateServerCertificate;
    Accept := 'application/*';
    UserAgent := UPDATER_USER_AGENT;
  end;  //of begin
end;

destructor TDownloadThread.Destroy;
begin
  FreeAndNil(FHttpClient);
  inherited Destroy;
end;

procedure TDownloadThread.NotifyOnCancel();
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TDownloadThread.NotifyOnDownloading();
begin
  if Assigned(FOnDownloading) then
    FOnDownloading(Self, FContentLength, FReadCount);
end;

procedure TDownloadThread.NotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FResponseCode, FResponseText);
end;

procedure TDownloadThread.NotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

procedure TDownloadThread.Downloading(const Sender: TObject; AContentLength,
  AReadCount: Int64; var AAbort: Boolean);
begin
  // Convert Bytes to KB
  FContentLength := AContentLength div 1024;
  FReadCount := AReadCount div 1024;

  // Notify progress
  Synchronize(NotifyOnDownloading);

  // Abort download if user canceled
  AAbort := Terminated;
end;

procedure TDownloadThread.ValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const ACertificate: TCertificate; var AAccepted: Boolean);
begin
  // Anything went wrong: Do not accept server SSL certificate!
  AAccepted := False;
end;

procedure TDownloadThread.SetTlsEnabled(const ATlsEnabled: Boolean);
begin
  // Use secure https instead of plain http
  if (ATlsEnabled and FUrl.StartsWith('http://')) then
    FUrl := FUrl.Replace('http://', 'https://')
  else
    // Use plain http instead of secure https
    if (not ATlsEnabled and FUrl.StartsWith('https://')) then
      FUrl := FUrl.Replace('https://', 'http://');

  FTLSEnabled := ATlsEnabled;
end;

procedure TDownloadThread.Execute();
var
  FileStream: TFileStream;
  Response: IHTTPResponse;

begin
  try
    // Init file stream
    FileStream := TFileStream.Create(FFileName, fmCreate or fmOpenWrite);

    // Try to download file
    try
      Response := FHttpClient.Get(FUrl, FileStream);
      FResponseCode := Response.StatusCode;

      // Error occured?
      if (FResponseCode <> 200) then
        raise ENetHTTPException.Create(PChar(Response.StatusText));

      // User canceled?
      if Terminated then
        Abort;

      // Download successful!
      Synchronize(NotifyOnFinish);

    finally
      FileStream.Free;
    end;  //of try

  except
    on E: EAbort do
    begin
      DeleteFile(PChar(FFileName));
      Synchronize(NotifyOnCancel);
    end;

    on E: Exception do
    begin
      // Certificate error?
      if (E is ENetHTTPCertificateException) then
        FResponseCode := ERROR_CERTIFICATE_VALIDATION;

      FResponseText := E.Message;
      DeleteFile(PChar(FFileName));
      Synchronize(NotifyOnError);
    end;
  end;  //of try
end;

function TDownloadThread.GetUniqueFileName(const AFileName: string): string;
var
  i: Integer;

begin
  Result := AFileName;
  i := 1;

  while FileExists(Result) do
  begin
    Result := Format(ChangeFileExt(AFileName, '') +' (%d)'+ ExtractFileExt(AFileName), [i]);
    Inc(i);
  end;  //of while
end;


{ TUpdateDialog }

constructor TUpdateDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := TForm.Create(Self);

  with FForm do
  begin
    BorderIcons := [biSystemMenu];
    BorderStyle := bsDialog;
    Caption := 'Update';
    ClientHeight := 110;
    ClientWidth := 362;
    OldCreateOrder := False;
    Position := poScreenCenter;
    OnCloseQuery := FormCloseQuery;
  end;  //of with

  FLabelStatistic := TLabel.Create(Self);

  with FLabelStatistic do
  begin
    Parent := FForm;
    Left := 308;
    Top := 16;
    Width := 29;
    Height := 14;
    Alignment := taRightJustify;
    Caption := '0/0 KB';
    Transparent := True;
  end;  //of with

  FProgressBar := TProgressBar.Create(Self);

  with FProgressBar do
  begin
    Parent := FForm;
    Left := 24;
    Top := 32;
    Width := 313;
    Height := 25;
    TabOrder := 0;
  end;  //of with

  FButtonFinished := TButton.Create(Self);

  with FButtonFinished do
  begin
    Parent := FForm;
    Left := 259;
    Top := 64;
    Width := 78;
    Height := 33;
    Cancel := True;
    Caption := SCancelButton;
    Default := True;
    TabOrder := 1;
    OnClick := FinishedClick;
  end;  //of with

  FTaskBar := TTaskbar.Create(FForm);
end;

constructor TUpdateDialog.Create(AOwner: TComponent; ALanguageFile: TLanguageFile);
begin
  Create(AOwner);
  FLanguageFile := ALanguageFile;
  FForm.Caption := FLanguageFile.GetString(LID_UPDATE);
end;

destructor TUpdateDialog.Destroy;
begin
  FTaskBar.ProgressState := TTaskBarProgressState.None;
  FreeAndNil(FTaskBar);
  FreeAndNil(FButtonFinished);
  FreeAndNil(FProgressBar);
  FreeAndNil(FLabelStatistic);
  FreeAndNil(FForm);
  inherited Destroy;
end;

function TUpdateDialog.GetTitle(): string;
begin
  Result := FForm.Caption;
end;

procedure TUpdateDialog.DownloadCanceled(Sender: TObject);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Error;
  FProgressBar.State := TProgressBarState.pbsError;
  Reset();
  MessageDlg(FLanguageFile.GetString(LID_UPDATE_CANCELED), mtInformation, [mbOK], 0);
  FForm.ModalResult := mrCancel;
end;

procedure TUpdateDialog.DownloadError(Sender: TObject; const AResponseCode: Integer;
  const AResponseText: string);
var
  MessageText: string;

begin
  FTaskBar.ProgressState := TTaskBarProgressState.Error;
  FProgressBar.State := TProgressBarState.pbsError;
  Reset();

  // Certificate validation error?
  if (AResponseCode = ERROR_CERTIFICATE_VALIDATION) then
    MessageText := Format(AResponseText +'! Please visit the <a href="%s'
      +'neuigkeiten.html">website</a> for more information.', [URL_BASE])
  else
    // HTTP error?
    MessageText := Format('HTTP/1.1 %d '+ AResponseText, [AResponseCode]);

  FLanguageFile.ShowException(FLanguageFile.GetString([LID_UPDATE_DOWNLOAD,
    LID_IMPOSSIBLE]), MessageText);
  FForm.ModalResult := mrAbort;
end;

procedure TUpdateDialog.DownloadFinished(Sender: TObject);
begin
  FThread := nil;
  FTaskBar.ProgressState := TTaskBarProgressState.Normal;
  FButtonFinished.Caption := FLanguageFile.GetString(LID_FINISHED);
  FButtonFinished.SetFocus;
  FForm.ModalResult := mrOk;
end;

procedure TUpdateDialog.Downloading(Sender: TObject; AContentLength, AReadCount: Int64);
begin
  FProgressBar.Max := AContentLength;
  FProgressBar.Position := AReadCount;
  FTaskBar.ProgressMaxValue := AContentLength;
  FTaskBar.ProgressValue := AReadCount;
  FLabelStatistic.Caption := Format('%d/%d KB', [AReadCount, AContentLength]);
end;

procedure TUpdateDialog.Reset();
begin
  FLabelStatistic.Caption := FLanguageFile.GetString(LID_CANCELED);
  FButtonFinished.Caption := FLanguageFile.GetString(LID_FINISHED);
  FThread := nil;
end;

procedure TUpdateDialog.SetTitle(const ATitle: string);
begin
  FForm.Caption := ATitle;
end;

function TUpdateDialog.Execute(ParentHwnd: HWND): Boolean;
var
  UseTls, DirectorySelected: Boolean;

begin
  Assert(FRemoteFileName <> '', 'RemoteFileName is not set!');
  Assert(FLocalFileName <> '', 'LocalFileName is not set!');
  Assert(Assigned(FLanguageFile), 'LanguageFile property not assigned!');
  UseTls := True;

  // Certificate not installed?
  if not CertificateExists() then
  begin
    // Ask user to install the certificate
    if (TaskMessageDlg(FLanguageFile.GetString(LID_UPDATE_SECURE),
      FLanguageFile.GetString([LID_UPDATE_SECURE_DESCRIPTION1,
      LID_UPDATE_SECURE_DESCRIPTION2, NEW_LINE, LID_CERTIFICATE_INSTALL_CONFIRM]),
      mtConfirmation, mbYesNo, 0, mbYes) = idYes) then
    begin
      try
        InstallCertificate();

      except
        on E: EOSError do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          UseTls := False;
        end;
      end;  //of try
    end  //of begin
    else
      UseTls := False;
  end;  //of begin

  DirectorySelected := (FDownloadDirectory <> '');

  // Download folder not set yet?
  if not DirectorySelected then
  begin
    // Show select directory dialog
    DirectorySelected := SelectDirectory(FLanguageFile.GetString(LID_UPDATE_SELECT_DIR),
      '', FDownloadDirectory);
  end;  //of begin

  if DirectorySelected then
  begin
    FFileName := IncludeTrailingPathDelimiter(FDownloadDirectory) + FLocalFileName;
    FButtonFinished.Caption := FLanguageFile.GetString(LID_CANCEL);

    // Initialize thread
    FThread := TDownloadThread.Create(URL_DOWNLOAD + FRemoteFileName, FLocalFileName);

    with FThread do
    begin
      OnDownloading := Self.Downloading;
      OnCancel := DownloadCanceled;
      OnFinish := DownloadFinished;
      OnError := DownloadError;

      // Use HTTPS?
      if UseTls then
      begin
        TLSEnabled := True;
        FForm.Caption := FLanguageFile.GetString(LID_UPDATE_SECURE);
      end;  //of begin

      Start();
    end;  //of with
  end  //of begin
  else
    // Cancel clicked
    Reset();

  Result := (FForm.ShowModal() = mrOk);
end;

procedure TUpdateDialog.LaunchSetup();
begin
  ExecuteProgram(FFileName);
end;

procedure TUpdateDialog.FinishedClick(Sender: TObject);
begin
  FForm.Close;
end;

procedure TUpdateDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Download still in progress?
  if Assigned(FThread) then
  begin
    // Cancel download
    FThread.Terminate;
    CanClose := False;
  end  //of begin
  else
    CanClose := True;
end;
{$ENDIF}

end.
