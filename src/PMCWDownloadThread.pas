{ *********************************************************************** }
{                                                                         }
{ PM Code Works Download Thread v3.0.1                                    }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWDownloadThread;

interface

uses
  Classes, SysUtils, System.Net.HttpClientComponent, System.Net.HttpClient,
  System.Net.URLClient, System.NetConsts, StrUtils;

const
  /// <summary>
  ///   Error saying that something went wrong with server certificate validation.
  /// </summary>
  ERROR_CERTIFICATE_VALIDATION = -2;

type
  { Events }
  TDownloadingEvent = procedure(Sender: TThread; AContentLength, AReadCount: Int64) of object;
  TRequestErrorEvent = procedure(Sender: TThread; const FResponseCode: Integer;
    const FResponseText: string) of object;
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
    procedure SetTlsEnabled(const AValue: Boolean);
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
    ///   Occurs when download has finished.
    /// </summary>
    property OnFinish: TDownloadFinishedEvent read FOnFinish write FOnFinish;

    /// <summary>
    ///   Gets or sets the usage of TLS.
    /// </summary>
    property TLSEnabled: Boolean read FTLSEnabled write SetTlsEnabled;
  end;

implementation

{ TDownloadThread }

constructor TDownloadThread.Create(const AUrl, AFileName: string;
  AAllowOverwrite: Boolean = False);
begin
  inherited Create(True);
  FreeOnTerminate := True;
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

destructor TDownloadThread.Destroy;
begin
  FHttp.Free;
  inherited Destroy;
end;

procedure TDownloadThread.DoNotifyOnCancel;
begin
  if Assigned(FOnCancel) then
    OnCancel(Self);
end;

procedure TDownloadThread.DoNotifyOnDownloading;
begin
  if Assigned(FOnDownloading) then
    OnDownloading(Self, FContentLength, FReadCount);
end;

procedure TDownloadThread.DoNotifyOnError;
begin
  if Assigned(FOnError) then
    OnError(Self, FResponseCode, FResponseText);
end;

procedure TDownloadThread.DoNotifyOnFinish;
begin
  if Assigned(OnFinish) then
    OnFinish(Self, FFileName);
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
    if Terminated then
      Abort;

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
