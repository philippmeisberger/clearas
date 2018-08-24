{ *********************************************************************** }
{                                                                         }
{ PM Code Works Updater v3.1                                              }
{                                                                         }
{ Copyright (c) 2011-2018 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs.Updater;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Dialogs, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ComCtrls, System.UITypes, Vcl.Consts, System.Net.HttpClient,
  System.NetConsts, System.Net.URLClient, Winapi.ShellAPI, PMCW.SysUtils,
  PMCW.LanguageFile, PMCW.Dialogs,
{$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl;
{$WARN UNIT_PLATFORM ON}

const
  /// <summary>
  ///   URL to the download (base) directory on website.
  /// </summary>
  URL_DIR                      = URL_BASE +'media/';

  /// <summary>
  ///   Error saying that something went wrong with server certificate validation.
  /// </summary>
  ERROR_CERTIFICATE_VALIDATION = -2;

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
  ///   Generic HTTP thread.
  /// </summary>
  THttpThread = class abstract(TThread)
  private
    FUrl,
    FResponseText: string;
    FOnError: THttpErrorEvent;
    FTLSEnabled: Boolean;
    FContentLength,
    FReadCount: Int64;
    FOnDownloading: TDownloadingEvent;
    FOnCancel: TNotifyEvent;
    procedure NotifyOnCancel();
    procedure NotifyOnDownloading();
    procedure NotifyOnError();
    procedure Downloading(const Sender: TObject; AContentLength, AReadCount: Int64;
      var AAbort: Boolean);
    procedure ValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const ACertificate: TCertificate; var AAccepted: Boolean);
    procedure SetTlsEnabled(const ATlsEnabled: Boolean);
  protected
    FHttpClient: THTTPClient;
    FResponseCode: Integer;
    procedure CheckResponse(const AResponse: IHTTPResponse);
    procedure DoExecute(); virtual; abstract;
    procedure Execute(); override; final;
  public
    /// <summary>
    ///   Constructor for creating a <c>THttpThread</c> instance.
    /// </summary>
    /// <param name="AUrl">
    ///   An URL.
    /// </param>
    constructor Create(const AUrl: string);

    /// <summary>
    ///   Destructor for destroying a <c>THttpThread</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Occurs when download has been canceled by user.
    /// </summary>
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;

    /// <summary>
    ///   Occurs when download is in progress.
    /// </summary>
    property OnDownloading: TDownloadingEvent read FOnDownloading write FOnDownloading;

    /// <summary>
    ///   Occurs when something went wrong during the HTTP connection.
    /// </summary>
    property OnError: THttpErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Gets or sets the usage of TLS.
    /// </summary>
    property TLSEnabled: Boolean read FTLSEnabled write SetTlsEnabled;
  end;

  /// <summary>
  ///   A <c>TUpdateCheckThread</c> downloads the version.txt from the website
  ///   and notifies about update status.
  /// </summary>
  TUpdateCheckThread = class(THttpThread)
  private
    FRemoteBuild: Integer;
  protected
    procedure DoExecute(); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TUpdateCheckThread</c> instance.
    /// </summary>
    /// <param name="ARemoteDirName">
    ///   The directory on website which contains the version.txt file.
    /// </param>
    constructor Create(const ARemoteDirName: string);

    /// <summary>
    ///   Holds the remote build number after thread has finished.
    /// </summary>
    property RemoteBuild: Integer read FRemoteBuild;
  end;

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
    procedure CheckFinished(Sender: TObject);
    procedure HttpError(Sender: TObject; const AResponseCode: Integer;
      const AResponseText: string);
    procedure NotifyOnUpdate();
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

  /// <summary>
  ///   A <c>TDownloadThread</c> downloads a file from an URL using TLS (default).
  ///   In case of error a precise description will be returned in the
  ///   <see cref="OnError"/> event. The progress of the download can be seen
  ///   in the <see cref="OnDownloading"/> event. Of course the download can be
  ///   canceled: Just use the <c>Terminate</c> method.
  /// </summary>
  TDownloadThread = class(THttpThread)
  private
    FOnFinish: TNotifyEvent;
    FFileName: string;
    procedure NotifyOnFinish();
  protected
    procedure DoExecute(); override;
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
    constructor Create(const AUrl, AFileName: string);

    /// <summary>
    ///   Occurs when the download has finished.
    /// </summary>
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  /// <summary>
  ///   <c>TUpdateDialog</c> is a dialog which downloads a file specified by
  ///   <see cref="FileNameRemote"/> from the website and shows the progress in
  ///   a <c>TProgressBar</c>. The download is encrypted using TLS per default
  ///   if certificate is installed. The file is downloaded to directory specified
  ///   by <see cref="DownloadDirectory"/> and stored under name specified by
  ///   <see cref="FileNameLocal"/> which is per default <see cref="FileNameRemote"/>.
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DownloadCanceled(Sender: TObject);
    procedure Downloading(Sender: TObject; AContentLength, AReadCount: Int64);
    procedure DownloadFinished(Sender: TObject);
    procedure DownloadTerminated(Sender: TObject);
    procedure HttpError(Sender: TObject; const AResponseCode: Integer;
      const AResponseText: string);
    function GetTitle(): string;
    procedure SetTitle(const ATitle: string);
    procedure StartDownload(const AUseTLS: Boolean);
  public
    /// <summary>
    ///   Constructor for creating a <c>TUpdateDialog</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   The owner.
    /// </param>
    constructor Create(AOwner: TComponent); overload; override;

    /// <summary>
    ///   Constructor for creating a <c>TUpdateDialog</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   The owner.
    /// </param>
    /// <param name="ALanguageFile">
    ///   The user interface translation file to use.
    /// </param>
    constructor Create(AOwner: TComponent; ALanguageFile: TLanguageFile); reintroduce; overload;

    /// <summary>
    ///   Destructor for destroying a <c>TUpdateDialog</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Executes the dialog.
    /// </summary>
    /// <param name="AParentHwnd">
    ///   The parent window.
    /// </param>
    /// <returns>
    ///   <c>True</c> if downloading was sucessful or <c>False</c> otherwise.
    /// </returns>
    function Execute(AParentHwnd: HWND): Boolean; override;

    /// <summary>
    ///   The downloaded file.
    /// </summary>
    property DownloadedFile: string read FFileName;

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

implementation

{ THttpThread }

constructor THttpThread.Create(const AUrl: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FUrl := AUrl;
  FTLSEnabled := AUrl.StartsWith('https://');

  // Initialize HTTP connection
  FHttpClient := THttpClient.Create;

  with FHttpClient do
  begin
    UserAgent := 'Updater/3.1 (PM Code Works Update Utility)';
    CustomHeaders['Connection'] := 'close';
    OnValidateServerCertificate := ValidateServerCertificate;
    OnReceiveData := Downloading;
  end;  //of begin
end;

destructor THttpThread.Destroy;
begin
  FreeAndNil(FHttpClient);
  inherited Destroy;
end;

procedure THttpThread.CheckResponse(const AResponse: IHTTPResponse);
begin
  FResponseCode := AResponse.StatusCode;

  if (FResponseCode <> 200) then
    raise ENetHTTPException.Create(PChar(AResponse.StatusText));
end;

procedure THttpThread.Downloading(const Sender: TObject; AContentLength,
  AReadCount: Int64; var AAbort: Boolean);
begin
  // Convert Bytes to KB
  FContentLength := AContentLength div 1024;
  FReadCount := AReadCount div 1024;

  // Notify progress
  Synchronize(NotifyOnDownloading);

  // Abort download if user canceled
  AAbort := Terminated;

  if Terminated then
    Synchronize(NotifyOnCancel);
end;

procedure THttpThread.Execute();
begin
  try
    DoExecute();

  except
    on E: ENetHTTPCertificateException do
    begin
      FResponseCode := ERROR_CERTIFICATE_VALIDATION;
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

procedure THttpThread.NotifyOnCancel();
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure THttpThread.NotifyOnDownloading();
begin
  if Assigned(FOnDownloading) then
    FOnDownloading(Self, FContentLength, FReadCount);
end;

procedure THttpThread.NotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FResponseCode, FResponseText);
end;

procedure THttpThread.SetTlsEnabled(const ATlsEnabled: Boolean);
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

procedure THttpThread.ValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const ACertificate: TCertificate;
  var AAccepted: Boolean);
begin
  // Anything went wrong: Do not accept server SSL certificate!
  AAccepted := False;
end;


{ TUpdateCheckThread }

constructor TUpdateCheckThread.Create(const ARemoteDirName: string);
begin
  inherited Create(URL_DIR + ARemoteDirName +'/version.txt');
  FHttpClient.Accept := 'text/plain';
  FRemoteBuild := -1;
end;

procedure TUpdateCheckThread.DoExecute();
var
  Response: IHTTPResponse;

begin
  try
    // Download version file for application
    Response := FHttpClient.Get(FUrl);
    CheckResponse(Response);

    // Invalid response?
    // Note: Also occurs when connection to update server fails
    if (not TryStrToInt(Response.ContentAsString(), FRemoteBuild) and (FRemoteBuild >= 0)) then
      raise EConvertError.Create('Error while parsing response!');

  except
    on E: EConvertError do
    begin
      FResponseCode := 406;
      raise;
    end;
  end;  //of try
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
  if not FNotifyNoUpdate then
    Exit;

  if (AResponseCode > 0) then
  begin
    ExceptionDlg(FLanguageFile, FLanguageFile.GetString([LID_UPDATE_NO_CONNECTION,
      LID_UPDATE_CHECK_CONNECTION]), AResponseText + Format(' (%d)', [AResponseCode]));
  end  //of begin
  else
  begin
    TaskMessageDlg(FLanguageFile.Strings[LID_UPDATE_NO_CONNECTION],
      FLanguageFile.Strings[LID_UPDATE_CHECK_CONNECTION], mtError, [mbOK], 0);
  end;  //of if
end;

procedure TUpdateCheck.NotifyOnUpdate();
begin
  // Update available?
  if (FRemoteBuild > FCurrentBuild) then
  begin
    if Assigned(FOnUpdate) then
      FOnUpdate(Self, FRemoteBuild);
  end  //of begin
  else
    if FNotifyNoUpdate then
      MessageDlg(FLanguageFile.GetString(LID_UPDATE_NOT_AVAILABLE), mtInformation, [mbOk], 0);
end;

procedure TUpdateCheck.CheckFinished(Sender: TObject);
begin
  // No error during update check?
  if ((Sender as TUpdateCheckThread).RemoteBuild > 0) then
  begin
    FRemoteBuild := (Sender as TUpdateCheckThread).RemoteBuild;
    NotifyOnUpdate();
  end;  //of begin

  // Clear lock (NOTE: thread is ref-counted)
  FThread := nil;
end;

procedure TUpdateCheck.CheckForUpdate();
var
  FileVersion: TFileVersion;

begin
  // Thread pending?
  if Assigned(FThread) then
    Exit;

  // Update already available?
  if (FRemoteBuild > 0) then
  begin
    NotifyOnUpdate();
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
    OnTerminate := CheckFinished;

    // Disable TLS as it is too much overhead to download a build number
    TLSEnabled := False;
    Start();
  end;  //of with
end;


{ TDownloadThread }

constructor TDownloadThread.Create(const AUrl, AFileName: string);
begin
  inherited Create(AUrl);
  FFileName := AFileName;
  FHttpClient.Accept := 'application/*';
end;

procedure TDownloadThread.NotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

procedure TDownloadThread.DoExecute();
var
  FileStream: TFileStream;
  Response: IHTTPResponse;

begin
  try
    FileStream := TFileStream.Create(FFileName, fmCreate or fmOpenWrite);

    // Try to download file
    try
      Response := FHttpClient.Get(FUrl, FileStream);
      CheckResponse(Response);

      // User canceled?
      if Terminated then
        Abort;

    finally
      FileStream.Free;
    end;  //of try

    Synchronize(NotifyOnFinish);

  except
    on E: EAbort do
      DeleteFile(FFileName);

    on E: Exception do
    begin
      DeleteFile(FFileName);
      raise;
    end;
  end;  //of try
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
    TabOrder := 0;
    ModalResult := mrCancel;
  end;  //of with
end;

constructor TUpdateDialog.Create(AOwner: TComponent; ALanguageFile: TLanguageFile);
begin
  Create(AOwner);
  Assert(Assigned(ALanguageFile), 'ALanguageFile is not assigned!');
  FLanguageFile := ALanguageFile;
  Title := FLanguageFile.GetString(LID_UPDATE);
end;

destructor TUpdateDialog.Destroy;
begin
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
  FProgressBar.State := TProgressBarState.pbsError;
  FLabelStatistic.Caption := FLanguageFile.GetString(LID_CANCELED);
  MessageDlg(FLanguageFile.GetString(LID_UPDATE_CANCELED), mtInformation, [mbOK], 0);
end;

procedure TUpdateDialog.HttpError(Sender: TObject; const AResponseCode: Integer;
  const AResponseText: string);
begin
  FProgressBar.State := TProgressBarState.pbsError;
  FLabelStatistic.Caption := FLanguageFile.GetString(LID_ERROR);

  // Certificate validation error?
  if (AResponseCode = ERROR_CERTIFICATE_VALIDATION) then
  begin
    if (TaskMessageDlg('Certificate validation failed', 'Secure update is not possible!',
      mtWarning, [mbRetry, mbAbort], 0) = idRetry) then
      StartDownload(False);
  end  //of begin
  else
  begin
    ExceptionDlg(FLanguageFile, FLanguageFile.GetString([LID_UPDATE_DOWNLOAD,
      LID_IMPOSSIBLE]), Format('HTTP %d '+ AResponseText, [AResponseCode]));
  end;  //of if
end;

procedure TUpdateDialog.DownloadFinished(Sender: TObject);
begin
  // Clear lock (NOTE: thread is ref-counted)
  FThread := nil;

  // Download successful: Close form automatically
  FForm.ModalResult := mrOk;

  // Launch setup?
  if FRemoteFileName.Contains('setup') then
    ShellExecute(0, 'open', PChar(FFileName), nil, nil, SW_SHOWNORMAL);
end;

procedure TUpdateDialog.Downloading(Sender: TObject; AContentLength, AReadCount: Int64);
begin
  FProgressBar.Max := AContentLength;
  FProgressBar.Position := AReadCount;
  FLabelStatistic.Caption := Format('%d/%d KB', [AReadCount, AContentLength]);
end;

procedure TUpdateDialog.DownloadTerminated(Sender: TObject);
begin
  FButtonFinished.Caption := FLanguageFile.GetString(LID_FINISHED);

  // Clear lock (NOTE: thread is ref-counted)
  FThread := nil;
end;

procedure TUpdateDialog.SetTitle(const ATitle: string);
begin
  FForm.Caption := ATitle;
end;

procedure TUpdateDialog.StartDownload(const AUseTLS: Boolean);
begin
  FThread := TDownloadThread.Create(URL_DIR + 'downloader.php?file='
    + FRemoteFileName, FFileName);

  with FThread do
  begin
    OnDownloading := Self.Downloading;
    OnCancel := DownloadCanceled;
    OnFinish := DownloadFinished;
    OnTerminate := DownloadTerminated;
    OnError := HttpError;
    TLSEnabled := AUseTLS;
    Start();
  end;  //of with
end;

function TUpdateDialog.Execute(AParentHwnd: HWND): Boolean;

  function GetUniqueFileName(const AFileName: string): string;
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

begin
  Assert(FRemoteFileName <> '', 'RemoteFileName is not set!');
  Assert(Assigned(FLanguageFile), 'LanguageFile property not assigned!');

  if (FLocalFileName = '') then
    FLocalFileName := FRemoteFileName;

  // Download folder not set yet?
  if (FDownloadDirectory = '') then
  begin
    // Show select directory dialog
    if not SelectDirectory(FLanguageFile.GetString(LID_UPDATE_SELECT_DIR), '',
      FDownloadDirectory) then
      Exit(False);
  end;  //of begin

  FFileName := GetUniqueFileName(IncludeTrailingPathDelimiter(FDownloadDirectory)
    + FLocalFileName);
  FButtonFinished.Caption := FLanguageFile.GetString(LID_CANCEL);
  StartDownload(True);
  Result := (FForm.ShowModal() = mrOk);
end;

procedure TUpdateDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Download still in progress?
  if Assigned(FThread) then
  begin
    // Cancel download
    FThread.Terminate();
    CanClose := False;
  end  //of begin
  else
    CanClose := True;
end;

end.
