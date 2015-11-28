{ *********************************************************************** }
{                                                                         }
{ PM Code Works Updater v3.0.2                                            }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWUpdater;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  SysUtils, Classes, Dialogs, PMCWUpdateCheckThread, PMCWLanguageFile, Forms,
{$IFDEF MSWINDOWS}
  PMCWDownloadThread, Windows, FileCtrl, StdCtrls, ComCtrls, Controls,
  System.Win.TaskbarCore, Vcl.Taskbar, UITypes, Registry, ShellAPI;
{$ELSE}
  LCLType, Resource, ElfReader, VersionResource, LResources, VersionTypes;
{$ENDIF}

const
  /// <summary>
  ///   URL to the PHP web downloader script.
  /// </summary>
  URL_DOWNLOAD = URL_DIR + 'downloader.php?file=';

  /// <summary>
  ///   Extract the major version information out of a <see cref="TFileProductVersion"/>.
  /// </summary>
  VERSION_MAJOR   = 0;

  /// <summary>
  ///   Extract the minor version information out of a <see cref="TFileProductVersion"/>.
  /// </summary>
  VERSION_MINOR   = 1;

  /// <summary>
  ///   Extract the service version information out of a <see cref="TFileProductVersion"/>.
  /// </summary>
  VERSION_SERVICE = 2;

  /// <summary>
  ///   Extract the build version information out of a <see cref="TFileProductVersion"/>.
  /// </summary>
  VERSION_BUILD   = 3;

{$IFDEF MSWINDOWS}
{$I Certificate.inc}
{$ENDIF}

type
{$IFDEF MSWINDOWS}
  /// <summary>
  ///   A <c>TFileProductVersion</c> contains version information: major, minor,
  ///   service and build. To access each version information use the <c>VERSION_*</c>
  ///   constants above.
  /// </summary>
  TFileProductVersion = array[VERSION_MAJOR..VERSION_BUILD] of Cardinal;
{$ENDIF}

  /// <summary>
  ///   Receive update notfication when a newer version is available on website.
  ///   Must be implemented by classes that use the <see cref="TUpdateCheck"/>.
  /// </summary>
  IUpdateListener = interface
  ['{D1CDAE74-717A-4C5E-9152-15FBA4A15552}']
    /// <summary>
    ///   Is called when a new version is available on website.
    /// </summary>
    /// <param name="Sender">
    ///    The caller.
    /// </param>
    /// <param name="ANewBuild">
    ///    The new build number which is available.
    /// </param>
    procedure OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
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
    FLang: TLanguageFile;
    FListeners: TInterfaceList;
    FUserUpdate: Boolean;
    FRemoteDirName: string;
    FNewBuild: Cardinal;
    { TUpdateCheckThread events }
    procedure OnCheckError(Sender: TThread; AResponseCode: Integer;
      AResponseText: string);
    procedure OnNoUpdateAvailable(Sender: TObject);
    procedure OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
  public
    /// <summary>
    ///    Constructor for creating a <c>TUpdateCheck</c> instance.
    /// </summary>
    /// <param name="ARemoteDirName">
    ///   The directory on website which contains the version.txt file.
    /// </param>
    /// <param name="ALang">
    ///   The specific user interface translation file to use.
    /// </param>
    constructor Create(ARemoteDirName: string; ALang: TLanguageFile); reintroduce; overload;

    /// <summary>
    ///    Constructor for creating a <c>TUpdateCheck</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   A listener which implements the <see cref="IUpdateListener"/> interface.
    /// </param>
    /// <param name="ARemoteDirName">
    ///   The directory on website which contains the version.txt file.
    /// </param>
    /// <param name="ALang">
    ///   The specific user interface translation file to use.
    /// </param>
    constructor Create(AOwner: IUpdateListener; ARemoteDirName: string;
      ALang: TLanguageFile); reintroduce; overload;

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
    /// <param name="AUserUpdate">
    ///   The search is initiated by user or program. If set to <c>True</c>
    ///   a message is shown if no update is available. Otherwise no message is
    ///   shown. Comes in handy when update should be searched on startup of
    ///   application: User would get always the message that no update is avaiable.
    ///   This can be annoying!
    /// </param>
    procedure CheckForUpdate(AUserUpdate: Boolean);

    /// <summary>
    ///   Gets the build number of current running program.
    /// </summary>
    /// <returns>
    ///   The build number.
    /// </returns>
    class function GetBuildNumber(): Cardinal;

    /// <summary>
    ///   Gets the complete version information of a file.
    /// </summary>
    /// <param name="AFileName">
    ///   The file.
    /// </param>
    /// <param name="AVersionInfo">
    ///   A <see cref="TFileProductVersion"/> version information that should
    ///   receive the information.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the version information of the file could be retrieved
    ///   successfully or <c>False</c> otherwise.
    /// </returns>
    class function GetFileVersion(const AFileName: string;
      var AVersionInfo: TFileProductVersion): Boolean;

    /// <summary>
    ///   Removes a listener from the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IUpdateListener"/> interface.
    /// </param>
    procedure RemoveListener(AListener: IUpdateListener);
  end;

{$IFDEF MSWINDOWS}
  /// <summary>
  ///   <c>TUpdate</c> is a form which downloads a file from the website and
  ///   shows the progress in a <c>TProgressBar</c> and on the taskbar. The
  ///   download is encrypted using TLS per default.
  /// </summary>
  TUpdate = class(TForm)
    pbProgress: TProgressBar;
    bFinished: TButton;
    lSize: TLabel;
    procedure bFinishedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    FThread: TThread;
    FDownloadDirectory,
    FTitle,
    FRemoteFileName,
    FLocalFileName,
    FFileName: string;
    FLang: TLanguageFile;
    FListeners: TInterfaceList;
    FTaskBar: TTaskbar;
    procedure OnDownloadCancel(Sender: TObject);
    procedure OnDownloadError(Sender: TThread; const AResponseCode: Integer;
      const AResponseText: string);
    procedure OnDownloadFinished(Sender: TThread; const AFileName: string);
    procedure OnDownloading(Sender: TThread; AContentLength, AReadCount: Int64);
    procedure Reset();
  protected
    /// <summary>
    ///   Starts downloading a file from the website.
    /// </summary>
    /// <param name="ARemoteFileName">
    ///   The filename of the file on website.
    /// </param>
    /// <param name="ALocalFileName">
    ///   The filename to use for the downloaded file.
    /// </param>
    /// <param name="ADownloadDirectory">
    ///   Download the file into this directory. If an empty string is passed
    ///   (default) a "search folder" dialog is displayed.
    /// </param>
    /// <param name="AUseTls">
    ///   If set to <c>True</c> (default) use encrypted HTTPS instead of plain
    ///   HTTP.
    /// </param>
    /// <returns>
    ///   <c>True</c> if downloading was sucessful or <c>False</c> otherwise.
    /// </returns>
    function Download(ARemoteFileName, ALocalFileName: string;
      ADownloadDirectory: string = ''; AUseTls: Boolean = True): Boolean;
  public
    /// <summary>
    ///   Constructor for creating a <c>TUpdate</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   The owner of the dialog which must implement the
    ///   <see cref="IUpdateListener"/> interface.
    /// </param>
    /// <param name="ALang">
    ///   The specific user interface translation file to use.
    /// </param>
    constructor Create(AOwner: TComponent; ALang: TLanguageFile); reintroduce;

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
    function Execute(): Boolean;

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
    property LanguageFile: TLanguageFile read FLang write FLang;

    /// <summary>
    ///   Gets or sets the title to use in the dialog caption.
    /// </summary>
    property Title: string read FTitle write FTitle;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$R CA.res}
{$ENDIF}

{ TUpdateCheck }

constructor TUpdateCheck.Create(ARemoteDirName: string; ALang: TLanguageFile);
begin
  inherited Create;
  FLang := ALang;
  FRemoteDirName := ARemoteDirName;
  FListeners := TInterfaceList.Create;
end;

constructor TUpdateCheck.Create(AOwner: IUpdateListener; ARemoteDirName: string;
  ALang: TLanguageFile);
begin
  Create(ARemoteDirName, ALang);

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
  AResponseText: string);
begin
  if FUserUpdate then
    if (AResponseCode > 0) then
      FLang.ShowException(FLang.GetString([LID_UPDATE_NO_CONNECTION,
        LID_UPDATE_CHECK_CONNECTION]), AResponseText + Format(' (%d)', [AResponseCode]))
    else
      FLang.ShowMessage(LID_UPDATE_NO_CONNECTION, LID_UPDATE_CHECK_CONNECTION, mtError);
end;

procedure TUpdateCheck.OnNoUpdateAvailable(Sender: TObject);
begin
  if FUserUpdate then
    FLang.ShowMessage(FLang.GetString(LID_UPDATE_NOT_AVAILABLE));
end;

procedure TUpdateCheck.OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
var
  i: Integer;
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

procedure TUpdateCheck.AddListener(AListener: IUpdateListener);
begin
  FListeners.Add(AListener);
end;

procedure TUpdateCheck.CheckForUpdate(AUserUpdate: Boolean);
begin
  FUserUpdate := AUserUpdate;

  // Update already available?
  if (FNewBuild > 0) then
  begin
    OnUpdateAvailable(nil, FNewBuild);
    Abort;
  end;  //of begin

  // Search for update
  with TUpdateCheckThread.Create(GetBuildNumber(), FRemoteDirName) do
  begin
    OnUpdate := OnUpdateAvailable;
    OnNoUpdate := OnNoUpdateAvailable;
    OnError := OnCheckError;
    Start();
  end;  //of with
end;

class function TUpdateCheck.GetFileVersion(const AFileName: string;
  var AVersionInfo: TFileProductVersion): Boolean;
{$IFDEF MSWINDOWS}
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;

begin
  Result := False;
  VerInfoSize := GetFileVersionInfoSize(PChar(AFileName), Dummy);

  if (VerInfoSize <> 0) then
  begin
    GetMem(VerInfo, VerInfoSize);

    try
      GetFileVersionInfo(PChar(AFileName), 0, VerInfoSize, VerInfo);

      if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
      begin
        AVersionInfo[VERSION_MAJOR] := LongRec(VerValue.dwFileVersionMS).Hi;
        AVersionInfo[VERSION_MINOR] := LongRec(VerValue.dwFileVersionMS).Lo;
        AVersionInfo[VERSION_SERVICE] := LongRec(VerValue.dwFileVersionLS).Hi;
        AVersionInfo[VERSION_BUILD] := LongRec(VerValue.dwFileVersionLS).Lo;
        Result := True;
      end;  //of begin

    finally
      FreeMem(VerInfo, VerInfoSize);
    end;  //of try
  end;  //of begin
end;
{$ELSE}
var
  RS: TResources;
  E: TElfResourceReader;
  VerValue: TVersionResource;
  i: Cardinal;

begin
  Result := False;
  RS := TResources.Create;
  VerValue := nil;
  i := 0;

  try
    E := TElfResourceReader.Create;
    Rs.LoadFromFile(AFileName, E);
    E.Free;

    while (VerValue = nil) and (i < RS.Count) do
    begin
      if RS.Items[i] is TVersionResource then
        VerValue := TVersionResource(RS.Items[i]);
      Inc(i);
    end;  //of while

    if Assigned(VerValue) then
    begin
      AVersionInfo := VerValue.FixedInfo.FileVersion;
      Result := True;
    end;  //of begin

  finally
    RS.FRee;
  end;  //of try
end;
{$ENDIF}

class function TUpdateCheck.GetBuildNumber(): Cardinal;
var
  VersionInfo: TFileProductVersion;

begin
  Result := 0;

  if GetFileVersion(Application.ExeName, VersionInfo) then
    Result := VersionInfo[VERSION_BUILD];
end;

procedure TUpdateCheck.RemoveListener(AListener: IUpdateListener);
begin
  FListeners.Remove(AListener);
end;

{$IFDEF MSWINDOWS}

{ TUpdate }

constructor TUpdate.Create(AOwner: TComponent; ALang: TLanguageFile);
begin
  inherited Create(AOwner);
  FLang := ALang;
  FListeners := TInterfaceList.Create;

  // Add owner to list to receive events
  if Assigned(AOwner) then
    AddListener(AOwner as IUpdateListener);

  FTaskBar := TTaskbar.Create(Self);
end;

destructor TUpdate.Destroy;
begin
  FTaskBar.ProgressState := TTaskBarProgressState.None;
  FreeAndNil(FTaskBar);
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TUpdate.FormShow(Sender: TObject);
begin
  if (FTitle <> '') then
    Caption := FTitle
  else
    Caption := FLang.GetString(LID_UPDATE);

  bFinished.Caption := FLang.GetString(LID_CANCEL);
end;

procedure TUpdate.OnDownloadCancel(Sender: TObject);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Error;
  pbProgress.State := TProgressBarState.pbsError;
  Reset();
  FLang.ShowMessage(FLang.GetString(LID_UPDATE_CANCELED));
  bFinished.ModalResult := mrCancel;
end;

procedure TUpdate.OnDownloadError(Sender: TThread; const AResponseCode: Integer;
  const AResponseText: string);
var
  MessageText: string;

begin
  FTaskBar.ProgressState := TTaskBarProgressState.Error;
  pbProgress.State := TProgressBarState.pbsError;
  Reset();

  // Certificate validation error?
  if (AResponseCode = ERROR_CERTIFICATE_VALIDATION) then
    MessageText := Format(AResponseText +'! Please visit the %s for more information.',
    ['<a href="http://www.pm-codeworks.de/neuigkeiten.html">website</a>'])
  else
    // HTTP error?
    MessageText := Format('HTTP/1.1 %d '+ AResponseText, [AResponseCode]);

  FLang.ShowException(FLang.GetString([LID_UPDATE_DOWNLOAD, LID_IMPOSSIBLE]), MessageText);
  bFinished.ModalResult := mrAbort;
end;

procedure TUpdate.OnDownloadFinished(Sender: TThread; const AFileName: string);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Normal;
  bFinished.Caption := FLang.GetString(LID_FINISHED);
  bFinished.SetFocus;
  FThread := nil;
  bFinished.ModalResult := mrOk;
end;

procedure TUpdate.OnDownloading(Sender: TThread; AContentLength, AReadCount: Int64);
begin
  pbProgress.Max := AContentLength;
  pbProgress.Position := AReadCount;
  FTaskBar.ProgressMaxValue := AContentLength;
  FTaskBar.ProgressValue := AReadCount;
  lSize.Caption := Format('%d/%d KB', [AReadCount, AContentLength]);
end;

procedure TUpdate.Reset();
begin
  lSize.Caption := FLang.GetString(LID_CANCELED);
  bFinished.Caption := FLang.GetString(LID_FINISHED);
  FThread := nil;
end;

function TUpdate.Download(ARemoteFileName, ALocalFileName: string;
  ADownloadDirectory: string = ''; AUseTls: Boolean = True): Boolean;
var
  Url: string;
  UseTls, Continue: Boolean;

begin
  FRemoteFileName := ARemoteFileName;
  FLocalFileName := ALocalFileName;
  UseTls := AUseTls;

  // Certificate not installed?
  if not CertificateExists() then
    UseTls := ShowInstallCertificateDialog();

  Continue := True;

  // Download folder not set yet?
  if (ADownloadDirectory = '') then
    // Show select directory dialog
    Continue := SelectDirectory(FLang.GetString(LID_UPDATE_SELECT_DIR), '', FDownloadDirectory);

  if Continue then
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
        Caption := FLang.GetString(LID_UPDATE_SECURE);
      end;  //of begin

      Start();
    end;  //of with
  end  //of begin
  else
    // Cancel clicked
    Reset();

  ShowModal;
  Result := (bFinished.ModalResult = mrOk);
end;

procedure TUpdate.AddListener(AListener: IUpdateListener);
begin
  FListeners.Add(AListener);
end;

function TUpdate.CertificateExists(): Boolean;
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

function TUpdate.Execute(): Boolean;
begin
  if ((FRemoteFileName = '') or (FLocalFileName = '')) then
    raise EArgumentException.Create('Missing argument: "RemoteFileName" or "LocalFileName"!');

  Result := Download(FRemoteFileName, FLocalFileName, FDownloadDirectory);
end;

function TUpdate.InstallCertificate(): Boolean;
var
  ResourceStream: TResourceStream;
  FileName: string;

begin
  Result := False;
  ResourceStream := TResourceStream.Create(HInstance, 'ca', RT_RCDATA);
  FileName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('temp')) +'CA.crt';

  try
    // Extract certificate from resource
    ResourceStream.SaveToFile(FileName);

    // Install certificate
    if (ShellExecute(Handle, 'open', 'certutil.exe', PChar('-user '+
      '-addstore ROOT "'+ FileName +'"'), nil, SW_HIDE) > 32) then
      Result := True
    else
      FLang.ShowMessage(FLang.GetString([LID_CERTIFICATE_INSTALL, LID_IMPOSSIBLE]),
        FLang.GetString(LID_CERTIFICATE_NO_CERTUTIL), mtError);

  finally
    ResourceStream.Free;
  end;  //of try
end;

function TUpdate.ShowInstallCertificateDialog(): Boolean;
var
  Answer: Integer;

begin
  Result := False;

  // Ask user to install the certificate
  Answer := TaskMessageDlg(FLang.GetString(LID_UPDATE_SECURE), FLang.GetString(
    [LID_UPDATE_SECURE_DESCRIPTION1, LID_UPDATE_SECURE_DESCRIPTION2, NEW_LINE,
    LID_CERTIFICATE_INSTALL_CONFIRM]), mtConfirmation, mbYesNoCancel, 0, mbYes);

  case Answer of
    IDYES:
      Result := InstallCertificate();

    IDCANCEL:
      Abort;
  end;  //of case
end;

procedure TUpdate.LaunchSetup();
begin
  ShellExecute(0, 'open', PChar(FFileName), nil, nil, SW_SHOWNORMAL);
end;

procedure TUpdate.RemoveListener(AListener: IUpdateListener);
begin
  FListeners.Remove(AListener);
end;

procedure TUpdate.bFinishedClick(Sender: TObject);
begin
  Close;
end;

procedure TUpdate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
