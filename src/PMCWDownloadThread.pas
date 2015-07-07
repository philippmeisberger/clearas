{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Download Thread v2.3                      }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWDownloadThread;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  Classes, SysUtils, IdComponent, IdHTTP, IdSSLOpenSSL, IdSSLOpenSSLHeaders,
  IdCTypes, {$IFDEF MSWINDOWS}ComObj, ActiveX,{$ENDIF} StrUtils;

type
  { Thread events }
  TOnDownloadEvent = procedure(Sender: TThread; AFileSize: Int64) of object;
  TOnDownloadErrorEvent = procedure(Sender: TThread; AResponseCode: Integer;
    AResponseMessage: string) of object;

  { TDownloadThread }
  TDownloadThread = class(TThread)
  private
    FHttp: TIdHTTP;
    FOnStart,
    FOnDownloading: TOnDownloadEvent;
    FOnFinish,
  {$IFDEF MSWINDOWS}
    FOnUnzip,
  {$ENDIF}
    FOnCancel: TNotifyEvent;
    FOnError: TOnDownloadErrorEvent;
    FFileSize,
    FDownloadSize: Int64;
    FFileName,
    FUrl,
    FResponseText: string;
    FAllowOverwrite,
  {$IFDEF MSWINDOWS}
    FUnzip,
  {$ENDIF}
    FTLSEnabled: Boolean;
    procedure DoNotifyOnCancel;
    procedure DoNotifyOnDownloading;
    procedure DoNotifyOnError;
    procedure DoNotifyOnFinish;
    procedure DoNotifyOnStart;
  {$IFDEF MSWINDOWS}
    procedure DoNotifyOnUnzip;
  {$ENDIF}
    procedure OnSSLStatusInfoEx(ASender: TObject; const ASslSocket: PSSL;
      const AWhere, ARet: TIdC_INT; const AType, AMsg: string);
    function OnVerifyServerCertificate(ACertificate: TIdX509; AOk: Boolean;
      ADepth, AError: Integer): Boolean;
  {$IFDEF MSWINDOWS}
    function UnzipArchive(): Boolean;
  {$ENDIF}
  protected
    procedure Execute; override;
  public
    constructor Create(const AUrl, AFileName: string; AAllowOverwrite: Boolean = False;
      ACreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Downloading(Sender: TObject; AWorkMode: TWorkMode; ADownloadSize: Int64);
    procedure DownloadStart(Sender: TObject; AWorkMode: TWorkMode; AFileSize: Int64);
    function GetUniqueFileName(const AFileName: string): string;
    procedure OnUserCancel(Sender: TObject);
    { external }
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnDownloading: TOnDownloadEvent read FOnDownloading write FOnDownloading;
    property OnError: TOnDownloadErrorEvent read FOnError write FOnError;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnStart: TOnDownloadEvent read FOnStart write FOnStart;
  {$IFDEF MSWINDOWS}
    property OnUnzip: TNotifyEvent read FOnUnzip write FOnUnzip;
  {$ENDIF}
    property TLSEnabled: Boolean read FTLSEnabled;
  {$IFDEF MSWINDOWS}
    property Unzip: Boolean read FUnzip write FUnzip;
  {$ENDIF}
  end;

implementation

{ TDownloadThread }

{ public TDownloadThread.Create

  Constructor for creating a TDownloadThread instance. }

constructor TDownloadThread.Create(const AUrl, AFileName: string;
  AAllowOverwrite: Boolean = False; ACreateSuspended: Boolean = True);
begin
  inherited Create(ACreateSuspended);
  FreeOnTerminate := True;
  FUnzip := False;
  FAllowOverwrite := AAllowOverwrite;

  // Rename file if already exists?
  if AAllowOverwrite then
    FFileName := AFileName
  else
    FFileName := GetUniqueFileName(AFileName);

  FUrl := AUrl;

  // Init IdHTTP component dynamically
  FHttp := TIdHTTP.Create(nil);

  // Setup some HTTP options
  with FHttp do
  begin
    // OpenSSL libraries exist?
    if LoadOpenSSLLibrary() then
    begin
      // Use TLS encrypted connection
      IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHttp);

      with (IOHandler as TIdSSLIOHandlerSocketOpenSSL) do
      begin
      {$IFDEF MSWINDOWS}
        SSLOptions.Method := sslvTLSv1_2;
      {$ENDIF}
        SSLOptions.VerifyMode := [sslvrfPeer];
        OnVerifyPeer := OnVerifyServerCertificate;
        OnStatusInfoEx := OnSSLStatusInfoEx;
      end;  //of with

      // Use secure https instead of plain http
      if AnsiStartsStr('http://', AUrl) then
        FUrl := 'https://'+ Copy(AUrl, 8, Length(AUrl) - 7);

      FTLSEnabled := True;
    end  //of begin
    else
    begin
      // Use plain http instead of secure https
      if AnsiStartsStr('https://', AUrl) then
        FUrl := 'http://'+ Copy(AUrl, 9, Length(AUrl) - 8);

      FTLSEnabled := False;
    end;  //of if

    HandleRedirects := True;

    // Link HTTP events
    OnWorkBegin := DownloadStart;
    OnWork := Downloading;

    // Set the user-agent because of some issues with default
    Request.UserAgent := 'Updater/2.3 (PM Code Works Update Utility)';

    // Close connection after completion of the response
    Request.Connection := 'close';
  end;  //of begin
end;

{ public TDownloadThread.Destroy

  Destructor for destroying a TDownloadThread instance. }

destructor TDownloadThread.Destroy;
begin
  FHttp.Free;
  inherited Destroy;
end;

{ private TDownloadThread.DoNotifyOnCancel

  Synchronizable event method that is called when download has been canceled
  by user. }

procedure TDownloadThread.DoNotifyOnCancel;
begin
  if Assigned(OnCancel) then
    OnCancel(Self);
end;

{ private TDownloadThread.DoNotifyOnDownloading

  Synchronizable event method that is called when download is in progress. }
  
procedure TDownloadThread.DoNotifyOnDownloading;
begin
  if Assigned(OnDownloading) then
    OnDownloading(Self, FDownloadSize);
end;

{ private TDownloadThread.DoNotifyOnError

  Synchronizable event method that is called when an error occurs while download
  is in progress. }
  
procedure TDownloadThread.DoNotifyOnError;
begin
  if Assigned(OnError) then
    OnError(Self, FHttp.ResponseCode, FResponseText);
end;

{ private TDownloadThread.DoNotifyOnFinish

  Synchronizable event method that is called when download is finished. }
  
procedure TDownloadThread.DoNotifyOnFinish;
begin
  if Assigned(OnFinish) then
    OnFinish(Self);
end;

{ private TDownloadThread.DoNotifyOnStart

  Synchronizable event method that is called when download starts. }

procedure TDownloadThread.DoNotifyOnStart;
begin
  if Assigned(OnStart) then
    OnStart(Self, FFileSize);
end;

{$IFDEF MSWINDOWS}
{ private TDownloadThread.DoNotifyOnUnzip

  Synchronizable event method that is called when zip file gets unzipped. }

procedure TDownloadThread.DoNotifyOnUnzip;
begin
  if Assigned(FOnUnzip) then
    OnUnzip(Self);
end;
{$ENDIF}

{ private TDownloadThread.OnSSLStatusInfoEx

  Event method that is called when connection status information are available. }

procedure TDownloadThread.OnSSLStatusInfoEx(ASender: TObject;
  const ASslSocket: PSSL; const AWhere, ARet: TIdC_INT; const AType, AMsg: string);
begin
  // Explict set hostname for server name identification (SNI) support
  SSL_set_tlsext_host_name(ASslSocket, FHttp.Request.Host);
end;

{ private TDownloadThread.OnVerifyServerCertificate

  Event method that is called when OpenSSL retrieves the peer certicate to run
  checks against it. }

function TDownloadThread.OnVerifyServerCertificate(ACertificate: TIdX509;
  AOk: Boolean; ADepth, AError: Integer): Boolean;
var
  CertificateFields: TStringList;
  CommonName: string;

begin
  Result := False;
  CertificateFields := TStringList.Create;

  // Extract common name (CN) from subject
  try
    CertificateFields.Delimiter := '/';
    CertificateFields.DelimitedText := ACertificate.Subject.OneLine;
    CommonName := Trim(CertificateFields.Values['CN']);

  finally
    CertificateFields.Free;
  end;  //of try

  // Common name matches hostname?
  if (CommonName[1] = '*') then
  begin
    CommonName := Copy(CommonName, 2, Length(CommonName) - 1);
    Result := AnsiEndsText(CommonName, FHttp.Request.Host);
  end  //of begin
  else
    Result := AnsiSameText(CommonName, FHttp.Request.Host);

  // Certificate not expired?
  Result := (Result and (ACertificate.notAfter > Now()));
end;

{$IFDEF MSWINDOWS}
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

    if FAllowOverwrite then
      Flags := Flags or SHCONTCH_RESPONDYESTOALL;

    // Unzip files
    Destination.CopyHere(Items, Flags);
    Result := True;

  finally
    CoUninitialize();
  end;  //of try
end;
{$ENDIF}

{ protected TDownloadThread.Execute

  Thread main method that downloads a file from an HTTP source. }

procedure TDownloadThread.Execute;
var
  FileStream: TFileStream;

begin
  try
    // Init file stream
    FileStream := TFileStream.Create(FFileName, fmCreate);

    // Try to download file
    try
      FHttp.Get(FUrl, FileStream);

    finally
      FileStream.Free;
    end;  //of try

    // Check if download was successful?
    if (FHttp.ResponseCode = 200) then
    begin
    {$IFDEF MSWINDOWS}
      // Unzip downloaded archive?
      if (FUnzip and (ExtractFileExt(FFileName) = '.zip')) then
      begin
        if UnzipArchive() then
          DeleteFile(FFileName);
      end;  //of begin
    {$ENDIF}
      Synchronize(DoNotifyOnFinish);
    end  //of begin
    else
      raise Exception.Create('Error while downloading! HTTP response code was not 200!');

  except
    on E: EAbort do
    begin
      DeleteFile(FFileName);
      Synchronize(DoNotifyOnCancel);
    end;  //of begin

    on E: Exception do
    begin
      FResponseText := E.Message;
      DeleteFile(FFileName);
      Synchronize(DoNotifyOnError);
    end;  //of begin
  end;  //of try
end;

{ public TDownloadThread.DownloadStart

  Event that is called by TIdHttp when download starts. }

procedure TDownloadThread.DownloadStart(Sender: TObject; AWorkMode: TWorkMode;
  AFileSize: Int64);
begin
  // Convert Byte into Kilobyte (KB = Byte/1024)
  FFileSize := AFileSize div 1024;
  Synchronize(DoNotifyOnStart);
end;

{ public TDownloadThread.Downloading

  Event that is called by TIdHttp while download is in progress. }

procedure TDownloadThread.Downloading(Sender: TObject; AWorkMode: TWorkMode;
  ADownloadSize: Int64);
begin
  if (not Self.Terminated) then
  begin
    // Convert Byte into Kilobyte (KB = Byte/1024)
    FDownloadSize := ADownloadSize div 1024;
    Synchronize(DoNotifyOnDownloading);
  end  //of begin
  else
    Abort;
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

{ public TDownloadThread.OnUserCancel

  Cancels downloading file if user clicks "cancel". }

procedure TDownloadThread.OnUserCancel(Sender: TObject);
begin
  Terminate;
end;

end.