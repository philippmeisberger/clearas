{ *********************************************************************** }
{                                                                         }
{ PM Code Works Download Thread v3.0                                      }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWDownloadThread;

interface

uses
  Classes, SysUtils, System.Net.HttpClientComponent, System.Net.HttpClient,
  System.Net.URLClient, StrUtils, ComObj, ActiveX;

const
  ERROR_CERTIFICATE_VALIDATION = -2;

type
  { Events }
  TDownloadingEvent = procedure(Sender: TThread; AContentLength, AReadCount: Int64) of object;
  TRequestErrorEvent = procedure(Sender: TThread; const FResponseCode: Integer;
    const FResponseText: string) of object;
  TDownloadFinishedEvent = procedure(Sender: TThread; const AFileName: string) of object;

  { TDownloadThread }
  TDownloadThread = class(TThread)
  private
    FHttp: THttpClient;
    FOnDownloading: TDownloadingEvent;
    FOnError: TRequestErrorEvent;
    FOnFinish: TDownloadFinishedEvent;
    FOnUnzip,
    FOnCancel: TNotifyEvent;
    FContentLength,
    FReadCount: Int64;
    FFileName,
    FUrl,
    FResponseText: string;
    FResponseCode: Integer;
    FTLSEnabled,
    FAbort: Boolean;
    { Synchronized events }
    procedure DoNotifyOnCancel;
    procedure DoNotifyOnDownloading;
    procedure DoNotifyOnError;
    procedure DoNotifyOnFinish;
    procedure DoNotifyOnUnzip;
    procedure Downloading(const Sender: TObject; AContentLength, AReadCount: Int64;
      var AAbort: Boolean);
    procedure OnValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const ACertificate: TCertificate; var AAccepted: Boolean);
    function UnzipArchive(): Boolean;
    procedure SetTlsEnabled(const AValue: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(const AUrl, AFileName: string; AAllowOverwrite: Boolean = False);
    destructor Destroy; override;
    procedure Cancel(Sender: TObject);
    function GetUniqueFileName(const AFileName: string): string;
    { external }
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnDownloading: TDownloadingEvent read FOnDownloading write FOnDownloading;
    property OnError: TRequestErrorEvent read FOnError write FOnError;
    property OnFinish: TDownloadFinishedEvent read FOnFinish write FOnFinish;
    property OnUnzip: TNotifyEvent read FOnUnzip write FOnUnzip;
    property TLSEnabled: Boolean read FTLSEnabled write SetTlsEnabled;
  end;

implementation

{ TDownloadThread }

{ public TDownloadThread.Create

  Constructor for creating a TDownloadThread instance. }

constructor TDownloadThread.Create(const AUrl, AFileName: string;
  AAllowOverwrite: Boolean = False);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FAbort := False;
  FUrl := AUrl;
  FTLSEnabled := AnsiStartsStr('https://', AUrl);

  // Rename file if already exists?
  if AAllowOverwrite then
    FFileName := AFileName
  else
    FFileName := GetUniqueFileName(AFileName);

  // Init HTTP component dynamically
  FHttp := THttpClient.Create();

  // Setup some HTTP options
  with FHttp do
  begin
    OnReceiveData := Downloading;
    OnValidateServerCertificate := Self.OnValidateServerCertificate;
    Accept := 'application/*';
    UserAgent := 'Updater/3.0 (PM Code Works Update Utility)';
  end;  //of begin
end;

{ public TDownloadThread.Destroy

  Destructor for destroying a TDownloadThread instance. }

destructor TDownloadThread.Destroy;
begin
  FHttp.Free;
  inherited Destroy;
end;

{ private TDownloadThread.Cancel

  Cancels the current download. }

procedure TDownloadThread.Cancel(Sender: TObject);
begin
  FAbort := True;
end;

{ private TDownloadThread.DoNotifyOnCancel

  Synchronizable event method that is called when download has been canceled
  by user. }

procedure TDownloadThread.DoNotifyOnCancel;
begin
  if Assigned(FOnCancel) then
    OnCancel(Self);
end;

{ private TDownloadThread.DoNotifyOnDownloading

  Synchronizable event method that is called when download is in progress. }

procedure TDownloadThread.DoNotifyOnDownloading;
begin
  if Assigned(FOnDownloading) then
    OnDownloading(Self, FContentLength, FReadCount);
end;

{ private TDownloadThread.DoNotifyOnError

  Synchronizable event method that is called when an error occurs while downloading. }

procedure TDownloadThread.DoNotifyOnError;
begin
  if Assigned(FOnError) then
    OnError(Self, FResponseCode, FResponseText);
end;

{ private TDownloadThread.DoNotifyOnFinish

  Synchronizable event method that is called when download is finished. }

procedure TDownloadThread.DoNotifyOnFinish;
begin
  if Assigned(OnFinish) then
    OnFinish(Self, FFileName);
end;

{ private TDownloadThread.DoNotifyOnUnzip

  Synchronizable event method that is called when zip file gets unzipped. }

procedure TDownloadThread.DoNotifyOnUnzip;
begin
  if Assigned(FOnUnzip) then
    OnUnzip(Self);
end;

{ private TDownloadThread.Downloading

  Event method that is called when download is in progress. }

procedure TDownloadThread.Downloading(const Sender: TObject; AContentLength,
  AReadCount: Int64; var AAbort: Boolean);
begin
  // Abort download if user canceled
  AAbort := FAbort;

  // Convert Bytes to KB
  FContentLength := AContentLength div 1024;
  FReadCount := AReadCount div 1024;

  // Notify progress
  Synchronize(DoNotifyOnDownloading);
end;

{ private TDownloadThread.OnValidateServerCertificate

  Event method that is called when certificate could not be validated. }

procedure TDownloadThread.OnValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const ACertificate: TCertificate; var AAccepted: Boolean);
begin
  // Anything went wrong: Do not accept server SSL certificate!
  AAccepted := False;
end;

{ private TDownloadThread.SetTlsEnabled

  Changes the used URL protocol to HTTPS or HTTP. }

procedure TDownloadThread.SetTlsEnabled(const AValue: Boolean);
begin
  // Use secure https instead of plain http
  if (AValue and AnsiStartsStr('http://', FUrl)) then
    FUrl := 'https://'+ Copy(FUrl, 8, Length(FUrl) - 7)
  else
    // Use plain http instead of secure https
    if (not AValue and AnsiStartsStr('https://', FUrl)) then
      FUrl := 'http://'+ Copy(FUrl, 9, Length(FUrl) - 8);

  FTLSEnabled := AValue;
end;

{ private TDownloadThread.Unzip

  Unzips the downloaded .zip archive. }

function TDownloadThread.UnzipArchive(): Boolean;
const
  SHCONTCH_NOPROGRESSBOX   = 4;
  SHCONTCH_AUTORENAME      = 8;
  SHCONTCH_RESPONDYESTOALL = 16;
  SHCONTF_FOLDERS          = 32;
  SHCONTF_NONFOLDERS       = 64;

var
  ShellObj, Source, Destination, Items: OleVariant;
  Flags: Byte;

  function GetNameSpace(Ole: OleVariant): OleVariant;
  begin
    Result := ShellObj.NameSpace(Ole);
  end;

begin
  Result := False;
  Synchronize(DoNotifyOnUnzip);
  CoInitialize(nil);

  try
    ShellObj := CreateOleObject('Shell.Application');

    Source := GetNameSpace(FFileName);
    Destination := GetNameSpace(ExtractFileDir(FFileName));
    Items := Source.Items;

    // Setup flags
    Flags := SHCONTCH_NOPROGRESSBOX;

    // Unzip files
    Destination.CopyHere(Items, Flags);
    Result := True;

  finally
    CoUninitialize();
  end;  //of try
end;

{ protected TDownloadThread.Execute

  Thread main method that downloads a file from an HTTP source. }
  
procedure TDownloadThread.Execute;
var
  FileStream: TFileStream;
  Response: IHTTPResponse;

begin
  try
    // Init file stream
    FileStream := TFileStream.Create(FFileName, fmCreate);

    // Try to download file
    try
      Response := FHttp.Get(FUrl, FileStream);

    finally
      FileStream.Free;
    end;  //of try

    FResponseCode := Response.StatusCode;

    // Error occured?
    if (Response.StatusCode <> 200) then
    begin
      FResponseText := StrPas(PChar(Response.StatusText));
      raise Exception.Create(FResponseText);
    end;  //of begin

    // User canceled?
    if FAbort then
      Abort;

    // Unzip archive?
    if Assigned(FOnUnzip) then
      // Unzip successful?
      if UnzipArchive() then
        DeleteFile(FFileName);

    // Download successful!
    Synchronize(DoNotifyOnFinish);

  except
    on E: EAbort do
    begin
      DeleteFile(FFileName);
      Synchronize(DoNotifyOnCancel);
    end;

    on E: Exception do
    begin
      // Certificate error?
      if (E is ENetHTTPCertificateException) then
        FResponseCode := ERROR_CERTIFICATE_VALIDATION;

      FResponseText := E.Message;
      DeleteFile(FFileName);
      Synchronize(DoNotifyOnError);
    end;
  end;  //of try
end;

{ public TDownloadThread.GetUniqueFileName

  Returns an unique file name to be sure downloading to an non-existing file. }

function TDownloadThread.GetUniqueFileName(const AFileName: string): string;
var
  i: Word;
  RawName, FilePath, NewFileName, Ext: string;

begin
  NewFileName := AFileName;
  Ext := ExtractFileExt(AFileName);
  FilePath := ExtractFilePath(NewFileName);
  RawName := ExtractFileName(NewFileName);
  RawName := Copy(RawName, 0, Length(RawName) - 4);
  i := 1;

  while FileExists(NewFileName) do
  begin
    NewFileName := FilePath + RawName +' ('+ IntToStr(i) +')'+ Ext;
    Inc(i);
  end;  //of while

  Result := NewFileName;
end;

end.
