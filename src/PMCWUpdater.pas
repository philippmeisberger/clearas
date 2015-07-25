{ *********************************************************************** }
{                                                                         }
{ PM Code Works Updater v3.0                                              }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWUpdater;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  SysUtils, Classes, Dialogs, PMCWUpdateCheckThread, PMCWLanguageFile,
{$IFDEF MSWINDOWS}
  PMCWDownloadThread, Windows, FileCtrl, Forms, StdCtrls, ComCtrls, Controls,
  System.Win.TaskbarCore, Vcl.Taskbar, Registry, ShellAPI;
{$ELSE}
  LCLType, Resource, ElfReader, VersionResource, LResources;
{$ENDIF}

const
  URL_DOWNLOAD = URL_DIR + 'downloader.php?file=';

  { TFileProductVersion indices }
  VERSION_MAJOR   = 0;
  VERSION_MINOR   = 1;
  VERSION_SERVICE = 2;
  VERSION_BUILD   = 3;

{$IFDEF MSWINDOWS}
{$I Certificate.inc}
{$ENDIF}

type
{$IFDEF MSWINDOWS}
  TFileProductVersion = array[0..3] of Cardinal;
{$ENDIF}

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
    procedure OnCheckError(Sender: TThread; AResponseCode: Integer;
      AResponseText: string);
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
    class function GetBuildNumber(): Cardinal;
    class function GetFileVersion(const AFileName: string;
      var AVersionInfo: TFileProductVersion): Boolean;
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
    FUnzip,
    FThreadRuns: Boolean;
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
    procedure OnUnzipArchive(Sender: TObject);
    procedure Reset();
  protected
    function Download(ARemoteFileName, ALocalFileName: string;
      ADownloadDirectory: string = ''; AUseTls: Boolean = True): Boolean;
  public
    constructor Create(AOwner: TComponent; ALang: TLanguageFile);
    destructor Destroy; override;
    procedure AddListener(AListener: IUpdateListener);
    function CertificateExists(): Boolean;
    function Execute(): Boolean;
    function InstallCertificate(): Boolean;
    procedure LaunchSetup();
    procedure RemoveListener(AListener: IUpdateListener);
    function ShowInstallCertificateDialog(): Boolean;
    { external }
    property DownloadDirectory: string read FDownloadDirectory write FDownloadDirectory;
    property FileNameLocal: string read FLocalFileName write FLocalFileName;
    property FileNameRemote: string read FRemoteFileName write FRemoteFileName;
    property LanguageFile: TLanguageFile read FLang write FLang;
    property Title: string read FTitle write FTitle;
    property Unzip: Boolean read FUnzip write FUnzip;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$R CA.res}
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

procedure TUpdateCheck.OnCheckError(Sender: TThread; AResponseCode: Integer;
  AResponseText: string);
begin
  if FUserUpdate then
    if (AResponseCode > 0) then
      FLang.ShowException(FLang.GetString([12, 13]), AResponseText)
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
    Start();
  end;  //of with
end;

{ TUpdateCheck.GetFileVersion

  Returns complete file version information. }

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

{ TUpdateCheck.GetBuildNumber

  Returns build number of current running program. }

class function TUpdateCheck.GetBuildNumber(): Cardinal;
var
  VersionInfo: TFileProductVersion;

begin
  Result := 0;

  if GetFileVersion(Application.ExeName, VersionInfo) then
    Result := VersionInfo[VERSION_BUILD];
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
  if (FTitle <> '') then
    Caption := FTitle
  else
    Caption := FLang.GetString(5);

  bFinished.Caption := FLang.GetString(6);
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

  FLang.ShowException(FLang.GetString([41, 18]), MessageText);
  bFinished.ModalResult := mrAbort;
end;

{ private TUpdate.OnDownloadFinished

  Event method that is called by TDownloadThread when download is finished. }

procedure TUpdate.OnDownloadFinished(Sender: TThread; const AFileName: string);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Normal;
  bFinished.Caption := FLang.GetString(8);
  bFinished.SetFocus;
  FThreadRuns := False;
  bFinished.ModalResult := mrOk;
end;

{ private TUpdate.OnDownloading

  Event method that is called by TDownloadThread when download is in progress. }

procedure TUpdate.OnDownloading(Sender: TThread; AContentLength, AReadCount: Int64);
begin
  pbProgress.Max := AContentLength;
  pbProgress.Position := AReadCount;
  FTaskBar.ProgressMaxValue := AContentLength;
  FTaskBar.ProgressValue := AReadCount;
  lSize.Caption := Format('%d/%d KB', [AReadCount, AContentLength]);
end;

{ private TUpdate.OnUnzipArchive

  Event method that is called by TDownloadThread when .zip archive gets unzipped. }

procedure TUpdate.OnUnzipArchive(Sender: TObject);
begin
  FTaskBar.ProgressState := TTaskBarProgressState.Indeterminate;
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
    Continue := SelectDirectory(FLang.GetString(9), '', FDownloadDirectory);

  if Continue then
  begin
    Url := URL_DOWNLOAD + FRemoteFileName;
    FFileName := IncludeTrailingPathDelimiter(FDownloadDirectory) + FLocalFileName;

    with TDownloadThread.Create(Url, FFileName) do
    begin
      // Link events
      FOnUserCancel := Cancel;
      OnDownloading := Self.OnDownloading;
      OnCancel := OnDownloadCancel;
      OnFinish := OnDownloadFinished;
      OnError := OnDownloadError;

      // Remote file is an .zip archive?
      if FUnzip then
        OnUnzip := OnUnzipArchive;

      // Use HTTPS?
      if UseTls then
      begin
        TLSEnabled := True;
        Caption := FLang.GetString([33, 5]);
      end;  //of begin

      Start();
    end;  //of with

    FThreadRuns := True;
  end  //of begin
  else
    // Cancel clicked
    Reset();

  ShowModal;
  Result := (bFinished.ModalResult = mrOk);
end;

{ public TUpdate.AddListener

  Adds a listener to the notification list. }

procedure TUpdate.AddListener(AListener: IUpdateListener);
begin
  FListeners.Add(AListener);
end;

{ public TUpdate.CertificateExists

  Returns if the certificate exists in Windows certificate store. }

function TUpdate.CertificateExists(): Boolean;
const
  KEY_CERTIFICATE_STORE = 'Software\Microsoft\SystemCertificates\ROOT\Certificates\';

var
  Reg: TRegistry;

begin
  Result := False;
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

{ public TUpdate.Execute

  Executes the dialog. }

function TUpdate.Execute(): Boolean;
begin
  if ((FRemoteFileName = '') or (FLocalFileName = '')) then
    raise EArgumentException.Create('Missing argument: "RemoteFileName" or "LocalFileName"!');

  Result := Download(FRemoteFileName, FLocalFileName, FDownloadDirectory);
end;

{ public TUpdate.InstallCertificate

  Installs the certificate for SSL updates and code signing verification. }

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
    ShellExecute(Handle, 'open', 'certutil.exe', PChar('-user -addstore ROOT "'+
      FileName +'"'), nil, SW_HIDE);
    Result := True;

  finally
    ResourceStream.Free;
  end;  //of try
end;

{ public TUpdate.ShowInstallCertificateDialog

  Shows a dialog where user has the choice to install the certificate. }

function TUpdate.ShowInstallCertificateDialog(): Boolean;
var
  Answer: Integer;

begin
  Result := False;

  // Ask user to install the certificate
  Answer := TaskMessageDlg(FLang.GetString(37), FLang.GetString([38, 39,
    NEW_LINE, 40]), mtConfirmation, mbYesNoCancel, 0, mbYes);

  case Answer of
    IDYES:
      Result := InstallCertificate();

    IDCANCEL:
      Abort;
  end;  //of case
end;

{ public TUpdate.LaunchSetup

  Launches the downloaded setup. }

procedure TUpdate.LaunchSetup();
begin
  ShellExecute(0, 'open', PChar(FFileName), nil, nil, SW_SHOWNORMAL);
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
