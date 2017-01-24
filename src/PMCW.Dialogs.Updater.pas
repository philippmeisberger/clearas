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
  ///   Receive update notfication when a newer version is available on website.
  ///   Must be implemented by classes that use the <see cref="TUpdateCheck"/>.
  /// </summary>
  IUpdateListener = interface
  ['{D1CDAE74-717A-4C5E-9152-15FBA4A15552}']
    /// <summary>
    ///   Is called when a new version is available on website.
    /// </summary>
    /// <param name="ANewBuild">
    ///    The new build number which is available.
    /// </param>
    procedure OnUpdate(const ANewBuild: Cardinal);
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
  TOnUpdateAvailableEvent = procedure(Sender: TThread; const ANewBuild: Cardinal) of object;

  /// <summary>
  ///   Occurs when checking for update has failed.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="AResponseCode">
  ///   The response code (usually a HTTP error code).
  /// </param>
  /// <param name="AResponseText">
  ///   The message of the response code.
  /// </param>
  TOnUpdateCheckErrorEvent = procedure(Sender: TThread; AResponseCode: Integer;
    const AResponseText: string) of object;

  /// <summary>
  ///   A <c>TUpdateCheckThread</c> downloads the version.txt from the website
  ///   and notifies about update status.
  /// </summary>
  TUpdateCheckThread = class(TThread)
  private
    FHttp: THttpClient;
    FResponseCode: Integer;
    FResponseText: string;
    FOnUpdate: TOnUpdateAvailableEvent;
    FOnError: TOnUpdateCheckErrorEvent;
    FOnNoUpdate: TNotifyEvent;
    FCurrentBuild,
    FNewBuild: Cardinal;
    FRemoteDirName: string;
    procedure DoNotifyOnError;
    procedure DoNotifyOnNoUpdate;
    procedure DoNotifyOnUpdate;
  protected
    procedure Execute; override;
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
    constructor Create(ACurrentBuild: Cardinal; const ARemoteDirName: string); reintroduce;

    /// <summary>
    ///   Destructor for destroying a <c>TUpdateCheckThread</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Occurs when search for update fails.
    /// </summary>
    property OnError: TOnUpdateCheckErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Occurs when no update is available.
    /// </summary>
    property OnNoUpdate: TNotifyEvent read FOnNoUpdate write FOnNoUpdate;

    /// <summary>
    ///   Occurs when an update is available.
    /// </summary>
    property OnUpdate: TOnUpdateAvailableEvent read FOnUpdate write FOnUpdate;
  end;

  /// <summary>
  ///   The <c>TUpdateCheck</c> is a light-weight async update notificator to
  ///   inform a user that a new version of the current PMCW project is available.
  ///   It uses HTTP to download a version.txt from the website that contains
  ///   the latest build number and checks if it matches the current build.
  ///   If the webside build number is higher all classes that implement
  ///   <see cref="IUpdateListener"/> and are registered with
  ///   <see cref="TUpdateCheck.AddListener"/> receive the <c>OnUpdate</c> event.
  ///   If the build numbers match a message is shown saying that no update is
  ///   available. It supports different UI translations.
  /// </summary>
  TUpdateCheck = class(TObject)
  private
    FLanguageFile: TLanguageFile;
    FListeners: TInterfaceList;
    FNotifyNoUpdate: Boolean;
    FRemoteDirName: string;
    FNewBuild: Cardinal;
    procedure OnCheckError(Sender: TThread; AResponseCode: Integer;
      const AResponseText: string);
    procedure OnNoUpdateAvailable(Sender: TObject);
    procedure OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
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
    constructor Create(const ARemoteDirName: string;
      ALanguageFile: TLanguageFile); reintroduce; overload;

    /// <summary>
    ///    Constructor for creating a <c>TUpdateCheck</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   A listener which implements the <see cref="IUpdateListener"/> interface.
    /// </param>
    /// <param name="ARemoteDirName">
    ///   The directory on website which contains the version.txt file.
    /// </param>
    /// <param name="ALanguageFile">
    ///   The specific user interface translation file to use.
    /// </param>
    constructor Create(AOwner: IUpdateListener; const ARemoteDirName: string;
      ALanguageFile: TLanguageFile); reintroduce; overload;

    /// <summary>
    ///   Destructor for destroying an <c>TUpdateCheck</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Adds a listener to the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IUpdateListener"/> interface.
    /// </param>
    procedure AddListener(AListener: IUpdateListener);

    /// <summary>
    ///   Searches for update on an HTTP server.
    /// </summary>
    /// <param name="ANotifyNoUpdate">
    ///   Set to <c>True</c> to show a message if no update is available.
    ///   Otherwise no message is shown. Comes in handy when update should be
    ///   searched on startup of application: User would get always the message
    ///   that no update is avaiable. This could be annoying!
    /// </param>
    procedure CheckForUpdate(ANotifyNoUpdate: Boolean = False);

    /// <summary>
    ///   Removes a listener from the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IUpdateListener"/> interface.
    /// </param>
    procedure RemoveListener(AListener: IUpdateListener);
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
  TDownloadingEvent = procedure(Sender: TThread; AContentLength, AReadCount: Int64) of object;

  /// <summary>
  ///   Occurs when an error occurs while downloading.
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
  TRequestErrorEvent = procedure(Sender: TThread; const AResponseCode: Integer;
    const AResponseText: string) of object;

  /// <summary>
  ///   Occurs when the download has finished.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="AFileName">
  ///   The downloaded file.
  /// </param>
  TDownloadFinishedEvent = procedure(Sender: TThread; const AFileName: string) of object;

  /// <summary>
  ///   A <c>TDownloadThread</c> downloads a file from an URL using TLS (default).
  ///   In case of error a precise description will be returned in the
  ///   <see cref="OnError"/> event. The progress of the download can be seen
  ///   in the <see cref="OnDownloading"/> event. Of course the download can be
  ///   canceled: Just use the <c>Terminate</c> method.
  /// </summary>
  TDownloadThread = class(TThread)
  private
    FHttp: THttpClient;
    FOnDownloading: TDownloadingEvent;
    FOnError: TRequestErrorEvent;
    FOnFinish: TDownloadFinishedEvent;
    FOnCancel: TNotifyEvent;
    FContentLength,
    FReadCount: Int64;
    FFileName,
    FUrl,
    FResponseText: string;
    FResponseCode: Integer;
    FTLSEnabled: Boolean;
    procedure DoNotifyOnCancel;
    procedure DoNotifyOnDownloading;
    procedure DoNotifyOnError;
    procedure DoNotifyOnFinish;
    procedure Downloading(const Sender: TObject; AContentLength, AReadCount: Int64;
      var AAbort: Boolean);
    procedure OnValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const ACertificate: TCertificate; var AAccepted: Boolean);
    procedure SetTlsEnabled(const ATlsEnabled: Boolean);
  protected
    procedure Execute; override;
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
    property OnError: TRequestErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Occurs when the download has finished.
    /// </summary>
    property OnFinish: TDownloadFinishedEvent read FOnFinish write FOnFinish;

    /// <summary>
    ///   Gets or sets the usage of TLS.
    /// </summary>
    property TLSEnabled: Boolean read FTLSEnabled write SetTlsEnabled;
  end;

  /// <summary>
  ///   <c>TUpdateDialog</c> is a dialog which downloads a file from the website and
  ///   shows the progress in a <c>TProgressBar</c> and on the taskbar. The
  ///   download is encrypted using TLS per default.
  /// </summary>
  TUpdateDialog = class(TCommonDialog)
  private
    FForm: TForm;
    FProgressBar: TProgressBar;
    FButtonFinished: TButton;
    FLabelStatistic: TLabel;
    FThread: TThread;
    FDownloadDirectory,
    FTitle,
    FRemoteFileName,
    FLocalFileName,
    FFileName: string;
    FLanguageFile: TLanguageFile;
    FListeners: TInterfaceList;
    FTaskBar: TTaskbar;
    procedure FinishedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure OnDownloadCancel(Sender: TObject);
    procedure OnDownloadError(Sender: TThread; const AResponseCode: Integer;
      const AResponseText: string);
    procedure OnDownloadFinished(Sender: TThread; const AFileName: string);
    procedure OnDownloading(Sender: TThread; AContentLength, AReadCount: Int64);
    procedure Reset();
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
    ///   Adds a listener to the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IUpdateListener"/> interface.
    /// </param>
    procedure AddListener(AListener: IUpdateListener);

    /// <summary>
    ///   Checks if the certificate exists in Windows certificate store.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if certificate exists or <c>False</c> otherwise.
    /// </returns>
    function CertificateExists(): Boolean;

    /// <summary>
    ///   Executes the update progress.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if downloading was sucessful or <c>False</c> otherwise.
    /// </returns>
    function Execute(ParentHwnd: HWND): Boolean; override;

    /// <summary>
    ///   Installs the certificate for SSL updates and code signing verification.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if installing was sucessful or <c>False</c> otherwise.
    /// </returns>
    function InstallCertificate(): Boolean;

    /// <summary>
    ///   Launches the downloaded setup.
    /// </summary>
    procedure LaunchSetup();

    /// <summary>
    ///   Removes a listener from the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IUpdateListener"/> interface.
    /// </param>
    procedure RemoveListener(AListener: IUpdateListener);

    /// <summary>
    ///   Shows a dialog where user has the choice to install the certificate.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if installation was sucessful or <c>False</c> otherwise.
    /// </returns>
    function ShowInstallCertificateDialog(): Boolean;

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
    property Title: string read FTitle write FTitle;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{ TUpdateCheckThread }

constructor TUpdateCheckThread.Create(ACurrentBuild: Cardinal;
  const ARemoteDirName: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FCurrentBuild := ACurrentBuild;
  FRemoteDirName := ARemoteDirName;

  // Init HTTP component dynamically
  FHttp := THttpClient.Create;

  // Setup some HTTP options
  with FHttp do
  begin
    CustomHeaders['Connection'] := 'close';
    Accept := 'text/plain';
    UserAgent := UPDATER_USER_AGENT;
  end;  //of begin
end;

destructor TUpdateCheckThread.Destroy;
begin
  FreeAndNil(FHttp);
  inherited Destroy;
end;

procedure TUpdateCheckThread.Execute;
var
  VersionText: string;
  Response: IHTTPResponse;
  Build: Integer;

begin
  try
    // Download version file for application
    Response := FHttp.Get(URL_DIR + FRemoteDirName +'/version.txt');
    FResponseCode := Response.StatusCode;

    // Error occured?
    if (FResponseCode <> 200) then
      raise ENetHTTPException.Create(PChar(Response.StatusText));

    // Retrieve build number
    VersionText := Response.ContentAsString();
    Build := -1;

    // Invalid response?
    // Note: Also occurs when connection to update server fails
    if not (TryStrToInt(VersionText, Build) and (Build >= 0)) then
      raise EConvertError.Create('Error while parsing response!');

    FNewBuild := Build;

    // Update available?
    if (FNewBuild > FCurrentBuild) then
      Synchronize(DoNotifyOnUpdate)
    else
      Synchronize(DoNotifyOnNoUpdate);

  except
    on E: EConvertError do
    begin
      FResponseCode := 406;
      FResponseText := E.Message;
      Synchronize(DoNotifyOnError);
    end;

    on E: Exception do
    begin
      FResponseText := E.Message;
      Synchronize(DoNotifyOnError);
    end;
  end;  //of try
end;

procedure TUpdateCheckThread.DoNotifyOnError;
begin
  if Assigned(FOnError) then
    FOnError(Self, FResponseCode, FResponseText);
end;

procedure TUpdateCheckThread.DoNotifyOnNoUpdate;
begin
  if Assigned(FOnNoUpdate) then
    FOnNoUpdate(Self);
end;

procedure TUpdateCheckThread.DoNotifyOnUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self, FNewBuild);
end;


{ TUpdateCheck }

constructor TUpdateCheck.Create(const ARemoteDirName: string;
  ALanguageFile: TLanguageFile);
begin
  inherited Create;
  FLanguageFile := ALanguageFile;
  FRemoteDirName := ARemoteDirName;
  FListeners := TInterfaceList.Create;
end;

constructor TUpdateCheck.Create(AOwner: IUpdateListener; const ARemoteDirName: string;
  ALanguageFile: TLanguageFile);
begin
  Create(ARemoteDirName, ALanguageFile);

  // Add owner to list to receive events
  if Assigned(AOwner) then
    FListeners.Add(AOwner);
end;

destructor TUpdateCheck.Destroy;
begin
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TUpdateCheck.OnCheckError(Sender: TThread; AResponseCode: Integer;
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
      FLanguageFile.ShowMessage(LID_UPDATE_NO_CONNECTION, LID_UPDATE_CHECK_CONNECTION, mtError);
  end;  //of begin
end;

procedure TUpdateCheck.OnNoUpdateAvailable(Sender: TObject);
begin
  if FNotifyNoUpdate then
    FLanguageFile.ShowMessage(FLanguageFile.GetString(LID_UPDATE_NOT_AVAILABLE));
end;

procedure TUpdateCheck.OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
var
  i: Integer;
  Listener: IUpdateListener;

begin
  FNewBuild := ANewBuild;

  // Notify all listeners
  for i := 0 to FListeners.Count - 1 do
    if Supports(FListeners[i], IUpdateListener, Listener) then
      Listener.OnUpdate(ANewBuild);
end;

procedure TUpdateCheck.AddListener(AListener: IUpdateListener);
begin
  FListeners.Add(AListener);
end;

procedure TUpdateCheck.CheckForUpdate(ANotifyNoUpdate: Boolean = False);
var
  FileVersion: TFileVersion;

begin
  FNotifyNoUpdate := ANotifyNoUpdate;

  // Update already available?
  if (FNewBuild > 0) then
  begin
    OnUpdateAvailable(nil, FNewBuild);
    Abort;
  end;  //of begin

  // Get build number of application
  if not FileVersion.FromFile(Application.ExeName) then
    FileVersion.Build := 0;

  // Search for update
  with TUpdateCheckThread.Create(FileVersion.Build, FRemoteDirName) do
  begin
    OnUpdate := OnUpdateAvailable;
    OnNoUpdate := OnNoUpdateAvailable;
    OnError := OnCheckError;
    Start();
  end;  //of with
end;

procedure TUpdateCheck.RemoveListener(AListener: IUpdateListener);
begin
  FListeners.Remove(AListener);
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

  // Init HTTP component dynamically
  FHttp := THttpClient.Create;

  // Setup some HTTP options
  with FHttp do
  begin
    OnReceiveData := Downloading;
    OnValidateServerCertificate := Self.OnValidateServerCertificate;
    Accept := 'application/*';
    UserAgent := UPDATER_USER_AGENT;
  end;  //of begin
end;

destructor TDownloadThread.Destroy;
begin
  FreeAndNil(FHttp);
  inherited Destroy;
end;

procedure TDownloadThread.DoNotifyOnCancel;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TDownloadThread.DoNotifyOnDownloading;
begin
  if Assigned(FOnDownloading) then
    FOnDownloading(Self, FContentLength, FReadCount);
end;

procedure TDownloadThread.DoNotifyOnError;
begin
  if Assigned(FOnError) then
    FOnError(Self, FResponseCode, FResponseText);
end;

procedure TDownloadThread.DoNotifyOnFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self, FFileName);
end;

procedure TDownloadThread.Downloading(const Sender: TObject; AContentLength,
  AReadCount: Int64; var AAbort: Boolean);
begin
  // Abort download if user canceled
  AAbort := Terminated;

  // Convert Bytes to KB
  FContentLength := AContentLength div 1024;
  FReadCount := AReadCount div 1024;

  // Notify progress
  Synchronize(DoNotifyOnDownloading);
end;

procedure TDownloadThread.OnValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const ACertificate: TCertificate; var AAccepted: Boolean);
begin
  // Anything went wrong: Do not accept server SSL certificate!
  AAccepted := False;
end;

procedure TDownloadThread.SetTlsEnabled(const ATlsEnabled: Boolean);
begin
  // Use secure https instead of plain http
  if (ATlsEnabled and FUrl.StartsWith('http://')) then
    FUrl := 'https://'+ Copy(FUrl, 8, Length(FUrl) - 7)
  else
    // Use plain http instead of secure https
    if (not ATlsEnabled and FUrl.StartsWith('https://')) then
      FUrl := 'http://'+ Copy(FUrl, 9, Length(FUrl) - 8);

  FTLSEnabled := ATlsEnabled;
end;

procedure TDownloadThread.Execute;
var
  FileStream: TFileStream;
  Response: IHTTPResponse;

begin
  try
    // Init file stream
    FileStream := TFileStream.Create(FFileName, fmCreate or fmOpenWrite);

    // Try to download file
    try
      Response := FHttp.Get(FUrl, FileStream);
      FResponseCode := Response.StatusCode;

      // Error occured?
      if (FResponseCode <> 200) then
        raise ENetHTTPException.Create(PChar(Response.StatusText));

      // User canceled?
      if Terminated then
        Abort;

      // Download successful!
      Synchronize(DoNotifyOnFinish);

    finally
      FileStream.Free;
    end;  //of try

  except
    on E: EAbort do
    begin
      DeleteFile(PChar(FFileName));
      Synchronize(DoNotifyOnCancel);
    end;

    on E: Exception do
    begin
      // Certificate error?
      if (E is ENetHTTPCertificateException) then
        FResponseCode := ERROR_CERTIFICATE_VALIDATION;

      FResponseText := E.Message;
      DeleteFile(PChar(FFileName));
      Synchronize(DoNotifyOnError);
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
  FListeners := TInterfaceList.Create;

  // Add owner to list to receive events
  if Assigned(AOwner) and Supports(AOwner, IUpdateListener) then
    AddListener(AOwner as IUpdateListener);

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
    OnShow := FormShow;
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
end;

destructor TUpdateDialog.Destroy;
begin
  FTaskBar.ProgressState := TTaskBarProgressState.None;
  FreeAndNil(FTaskBar);
  FreeAndNil(FButtonFinished);
  FreeAndNil(FProgressBar);
  FreeAndNil(FLabelStatistic);
  FreeAndNil(FListeners);
  FreeAndNil(FForm);
  inherited Destroy;
end;

procedure TUpdateDialog.FormShow(Sender: TObject);
begin
  if (FTitle <> '') then
    FForm.Caption := FTitle
  else
    FForm.Caption := FLanguageFile.GetString(LID_UPDATE);

  if Assigned(FThread) then
    FButtonFinished.Caption := FLanguageFile.GetString(LID_CANCEL)
  else
    FButtonFinished.Caption := FLanguageFile.GetString(LID_FINISHED);
end;

procedure TUpdateDialog.OnDownloadCancel(Sender: TObject);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Error;
  FProgressBar.State := TProgressBarState.pbsError;
  Reset();
  FLanguageFile.ShowMessage(FLanguageFile.GetString(LID_UPDATE_CANCELED));
  FForm.ModalResult := mrCancel;
end;

procedure TUpdateDialog.OnDownloadError(Sender: TThread; const AResponseCode: Integer;
  const AResponseText: string);
var
  MessageText: string;

begin
  FTaskBar.ProgressState := TTaskBarProgressState.Error;
  FProgressBar.State := TProgressBarState.pbsError;
  Reset();

  // Certificate validation error?
  if (AResponseCode = ERROR_CERTIFICATE_VALIDATION) then
    MessageText := Format(AResponseText +'! Please visit the %s for more information.',
    ['<a href="http://www.pm-codeworks.de/neuigkeiten.html">website</a>'])
  else
    // HTTP error?
    MessageText := Format('HTTP/1.1 %d '+ AResponseText, [AResponseCode]);

  FLanguageFile.ShowException(FLanguageFile.GetString([LID_UPDATE_DOWNLOAD,
    LID_IMPOSSIBLE]), MessageText);
  FForm.ModalResult := mrAbort;
end;

procedure TUpdateDialog.OnDownloadFinished(Sender: TThread; const AFileName: string);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Normal;
  FButtonFinished.Caption := FLanguageFile.GetString(LID_FINISHED);
  FButtonFinished.SetFocus;
  FThread := nil;
  FForm.ModalResult := mrOk;
end;

procedure TUpdateDialog.OnDownloading(Sender: TThread; AContentLength, AReadCount: Int64);
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

procedure TUpdateDialog.AddListener(AListener: IUpdateListener);
begin
  FListeners.Add(AListener);
end;

function TUpdateDialog.CertificateExists(): Boolean;
const
  KEY_CERTIFICATE_STORE = 'Software\Microsoft\SystemCertificates\ROOT\Certificates\';

var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    // Check user root certificate store
    Reg.RootKey := HKEY_CURRENT_USER;
    Result := (Reg.OpenKeyReadOnly(KEY_CERTIFICATE_STORE) and Reg.KeyExists(
      CERTIFICATE_FINGERPRINT_SHA1));

  finally
    Reg.CloseKey;
    Reg.Free;
  end;  //of try
end;

function TUpdateDialog.Execute(ParentHwnd: HWND): Boolean;
var
  Url: string;
  UseTls, DirectorySelected: Boolean;

begin
  if ((FRemoteFileName = '') or (FLocalFileName = '')) then
    raise EArgumentException.Create('Missing argument: "RemoteFileName" or "LocalFileName"!');

  if not Assigned(FLanguageFile) then
    raise EAssertionFailed.Create('LanguageFile property not assigned!');

  FRemoteFileName := FRemoteFileName;
  FLocalFileName := FLocalFileName;
  UseTls := True;

  // Certificate not installed?
  if not CertificateExists() then
    UseTls := ShowInstallCertificateDialog();

  DirectorySelected := True;

  // Download folder not set yet?
  if (FDownloadDirectory = '') then
    // Show select directory dialog
    DirectorySelected := SelectDirectory(FLanguageFile.GetString(LID_UPDATE_SELECT_DIR),
      '', FDownloadDirectory);

  if DirectorySelected then
  begin
    Url := URL_DOWNLOAD + FRemoteFileName;
    FFileName := IncludeTrailingPathDelimiter(FDownloadDirectory) + FLocalFileName;
    FThread := TDownloadThread.Create(Url, FFileName);

    with TDownloadThread(FThread) do
    begin
      // Link events
      OnDownloading := Self.OnDownloading;
      OnCancel := OnDownloadCancel;
      OnFinish := OnDownloadFinished;
      OnError := OnDownloadError;

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

function TUpdateDialog.InstallCertificate(): Boolean;
var
  ResourceStream: TResourceStream;
  FileName: string;

begin
  Result := False;
  ResourceStream := TResourceStream.Create(HInstance, 'ca', RT_RCDATA);
  FileName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +'CA.crt';

  try
    // Extract certificate from resource
    ResourceStream.SaveToFile(FileName);

    // Install certificate
    // TODO: Better use Windows API
    Result := ExecuteProgram('certutil.exe', '-user -addstore ROOT "'+ FileName +'"', SW_HIDE);

    if not Result then
    begin
      FLanguageFile.ShowMessage(FLanguageFile.GetString([LID_CERTIFICATE_INSTALL,
        LID_IMPOSSIBLE]), FLanguageFile.GetString(LID_CERTIFICATE_NO_CERTUTIL), mtError);
    end;  //of begin

  finally
    ResourceStream.Free;
  end;  //of try
end;

function TUpdateDialog.ShowInstallCertificateDialog(): Boolean;
var
  Answer: Integer;

begin
  Result := False;

  // Ask user to install the certificate
  Answer := TaskMessageDlg(FLanguageFile.GetString(LID_UPDATE_SECURE),
    FLanguageFile.GetString([LID_UPDATE_SECURE_DESCRIPTION1,
    LID_UPDATE_SECURE_DESCRIPTION2, NEW_LINE, LID_CERTIFICATE_INSTALL_CONFIRM]),
    mtConfirmation, mbYesNoCancel, 0, mbYes);

  case Answer of
    IDYES:
      Result := InstallCertificate();

    IDCANCEL:
      Abort;
  end;  //of case
end;

procedure TUpdateDialog.LaunchSetup();
begin
  ExecuteProgram(FFileName);
end;

procedure TUpdateDialog.RemoveListener(AListener: IUpdateListener);
begin
  FListeners.Remove(AListener);
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
