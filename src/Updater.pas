{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Updater v2.2                              }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit Updater;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  SysUtils, Classes, UpdateCheckThread, DownloadThread, LanguageFile, OSUtils,

{$IFDEF MSWINDOWS}
  Windows, FileCtrl, Forms, StdCtrls, ComCtrls, Controls;
{$ELSE}
  LCLType;
{$ENDIF}

const
  URL_DOWNLOAD = URL_DIR + 'downloader.php?file=';
  
type
  { IUpdateListener }
  IUpdateListener = interface
  ['{D1CDAE74-717A-4C5E-9152-15FBA4A15552}']
    procedure AfterUpdate(Sender: TObject; ADownloadedFileName: string);
    procedure BeforeUpdate(Sender: TObject; const ANewBuild: Cardinal);
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
  private
    FOnUserCancel: TNotifyEvent;
    FThreadRuns: Boolean;
    FRemoteFileName, FLocalFileName, FFileName: string;
    FLang: TLanguageFile;
    FListeners: TInterfaceList;
    procedure Reset();
    { TDownloadThread events }
    procedure OnDownloadCancel(Sender: TObject);
    procedure OnDownloadError(Sender: TThread; AResponseCode: Integer);
    procedure OnDownloadFinished(Sender: TObject);
    procedure OnDownloading(Sender: TThread; const ADownloadSize: Integer);
    procedure OnDownloadStart(Sender: TThread; const AFileSize: Integer);
  public
    constructor Create(AOwner: TComponent; ALangFile: TLanguageFile;
      AFormCaption: string = ''); reintroduce; overload;
    constructor Create(AOwner: TComponent; ALangFile: TLanguageFile;
      ARemoteFileName, ALocalFileName: string; AFormCaption: string = ''); reintroduce; overload;
    destructor Destroy; override;
    procedure AddListener(AListener: IUpdateListener);
    procedure Download(ADownloadDirectory: string = ''); overload;
    procedure Download(ARemoteFileName, ALocalFileName: string;
      ADownloadDirectory: string = ''); overload;
    procedure DownloadCertificate();
    procedure RemoveListener(AListener: IUpdateListener);
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
    with FLang do
      MessageBox(GetString(12) +^J+ GetString(13) +' '+ Format(GetString(19),
        [AResponseCode]), mtError, True);
end;

{ private TUpdateCheck.OnNoUpdateAvailable

  Event method that is called when TUpdateCheckThread search returns no update. }

procedure TUpdateCheck.OnNoUpdateAvailable(Sender: TObject);
begin
  if FUserUpdate then
    FLang.MessageBox(23, mtInfo, True);
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
  for i := 0 to FListeners.Count -1 do
    if Supports(FListeners[i], IUpdateListener, Listener) then
      Listener.BeforeUpdate(Self, ANewBuild);
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
    ACurrentBuild := TOSUtils.GetBuildNumber();

  // Search for update
  with TUpdateCheckThread.Create(ACurrentBuild, FRemoteDirName) do
  begin
    OnUpdate := OnUpdateAvailable;
    OnNoUpdate := OnNoUpdateAvailable;
    OnError := OnCheckError;
  {$IFDEF MSWINDOWS}
    Resume;
  {$ELSE}
    Start;
  {$ENDIF}
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

constructor TUpdate.Create(AOwner: TComponent; ALangFile: TLanguageFile;
  AFormCaption: string = '');
begin
  inherited Create(AOwner);
  FLang := ALangFile;
  FThreadRuns := False;

  if (AFormCaption <> '') then
    Self.Caption := AFormCaption;

  // Init list of listeners
  FListeners := TInterfaceList.Create;

  // Add owner to list to receive events
  if Assigned(AOwner) then
    FListeners.Add(AOwner);
end;

{ public TUpdate.Create

  Constructor for creating an TUpdate instance. }

constructor TUpdate.Create(AOwner: TComponent; ALangFile: TLanguageFile;
  ARemoteFileName, ALocalFileName: string; AFormCaption: string = '');
begin
  Create(AOwner, ALangFile, AFormCaption);
  FRemoteFileName := ARemoteFileName;
  FLocalFileName := ALocalFileName;
end;

{ public TUpdate.Destroy

  Destructor for destroying an TUpdate instance. }

destructor TUpdate.Destroy;
begin
  FreeAndNil(FListeners);
  inherited Destroy;
end;

{ private TUpdate.Reset

  Resets Update GUI. }

procedure TUpdate.Reset();
begin
  // Reset ProgressBar
  pbProgress.Position := 0;

  lSize.Caption := FLang.GetString(7);
  bFinished.Caption := FLang.GetString(8);
  FThreadRuns := False;
end;

{ private TUpdate.OnDownloadCancel

  Event method that is called by TDownloadThread when user canceled downlad. }
  
procedure TUpdate.OnDownloadCancel(Sender: TObject);
begin
  Reset();
  FLang.MessageBox(30, mtInfo);
end;

{ private TUpdate.OnDownloadError

  Event method that is called by TDownloadThread when an error occurs while
  downloading the update. }

procedure TUpdate.OnDownloadError(Sender: TThread; AResponseCode: Integer);
begin
  with FLang do
    MessageBox(Caption + GetString(18) +' '+ Format(GetString(19), [AResponseCode]),
      mtError, True);

  Reset();
end;

{ private TUpdate.OnDownloadFinished

  Event method that is called by TDownloadThread when download is finished. }

procedure TUpdate.OnDownloadFinished(Sender: TObject);
var
  i: Word;
  Listener: IUpdateListener;

begin
  // Caption "finished"
  bFinished.Caption := FLang.GetString(8);
  bFinished.SetFocus;
  FThreadRuns := False;

{$IFDEF MSWINDOWS}
  // Show dialog to add certificate
  if (ExtractFileExt(FFileName) = '.reg') then
    if TOSUtils.ShowAddRegistryDialog(FFileName) then
      DeleteFile(PChar(FFileName));
{$ENDIF}

  // Notify all listeners
  for i := 0 to FListeners.Count -1 do
    if Supports(FListeners[i], IUpdateListener, Listener) then
      Listener.AfterUpdate(Self, FFileName);
end;

{ private TUpdate.OnDownloading

  Event method that is called by TDownloadThread when download is in progress. }

procedure TUpdate.OnDownloading(Sender: TThread; const ADownloadSize: Integer);
begin
  pbProgress.Position := ADownloadSize;
  lSize.Caption := IntToStr(ADownloadSize) +'/'+ IntToStr(pbProgress.Max) +'KB';
end;

{ private TUpdate.OnDownloadStart

  Event method that is called by TDownloadThread when download starts. }

procedure TUpdate.OnDownloadStart(Sender: TThread; const AFileSize: Integer);
begin
  pbProgress.Max := AFileSize;
end;

{ public TUpdate.AddListener

  Adds a listener to the notification list. }

procedure TUpdate.AddListener(AListener: IUpdateListener);
begin
  FListeners.Add(AListener);
end;

{ public TUpdate.Download

  Starts downloading a file. }

procedure TUpdate.Download(ADownloadDirectory: string = '');
begin
  if ((FRemoteFileName = '') or (FLocalFileName = '')) then
    raise Exception.Create('Missing argument: "RemoteFileName" or "LocalFileName"!');

  Download(FRemoteFileName, FLocalFileName, ADownloadDirectory);
end;

{ public TUpdate.Download

  Starts downloading a file. }

procedure TUpdate.Download(ARemoteFileName, ALocalFileName: string; 
  ADownloadDirectory: string = '');
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
    // Show SelectDirectory dialog
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
        Resume;      
      end;  //of with

      // Caption "cancel"
      bFinished.Caption := FLang.GetString(6);
      FThreadRuns := True;

    except
      OnDownloadError(nil, 0);
    end;  //of try
  end  //of begin
  else
    // Cancel clicked
    Reset();
end;

{ TUpdate.DownloadCertificate

  Starts downloading the PMCW certificate. }

procedure TUpdate.DownloadCertificate();
begin
  Download('cert.reg', 'Install_PMCW_Cert.reg', TOSUtils.GetTempDir());
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
    // Close form
    CanClose := True;
end;
{$ENDIF}

end.