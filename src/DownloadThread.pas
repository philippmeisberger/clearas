{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Update Thread v2.2                        }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit DownloadThread;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  Classes, SysUtils, IdException, IdComponent, IdHTTP;

type
  { Thread events }
  TOnDownloadStartEvent = procedure(Sender: TThread;
    {$IFDEF MSWINDOWS}const AFileSize: Integer{$ELSE}AFileSize: Int64{$ENDIF}) of object;
  TOnDownloadingEvent = procedure(Sender: TThread;
    {$IFDEF MSWINDOWS}const ADownloadSize: Integer{$ELSE}ADownloadSize: Int64{$ENDIF}) of object;
  TOnDownloadErrorEvent = procedure(Sender: TThread; AResponseCode: Integer) of object;

  { TDownloadThread }
  TDownloadThread = class(TThread)
  private
    FHttp: TIdHTTP;
    FOnStart: TOnDownloadStartEvent;
    FOnDownloading: TOnDownloadingEvent;
    FOnFinish, FOnCancel: TNotifyEvent;
    FOnError: TOnDownloadErrorEvent;
    FFileSize, FDownloadSize: {$IFDEF MSWINDOWS}Integer{$ELSE}Int64{$ENDIF};
    FFileName, FUrl: string;
    { Synchronized events }
    procedure DoNotifyOnCancel;
    procedure DoNotifyOnDownloading;
    procedure DoNotifyOnError;
    procedure DoNotifyOnFinish;
    procedure DoNotifyOnStart;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUrl, AFileName: string; AAllowOverwrite: Boolean = False;
      ACreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Downloading(Sender: TObject; AWorkMode: TWorkMode;
      {$IFDEF MSWINDOWS}const ADownloadSize: Integer{$ELSE}ADownloadSize: Int64{$ENDIF});
    procedure DownloadStart(Sender: TObject; AWorkMode: TWorkMode;
      {$IFDEF MSWINDOWS}const AFileSize: Integer{$ELSE}AFileSize: Int64{$ENDIF});
    function GetUniqueFileName(const AFileName: string): string;
    procedure OnUserCancel(Sender: TObject);
    { Externalized events }
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnDownloading: TOnDownloadingEvent read FOnDownloading write FOnDownloading;
    property OnError: TOnDownloadErrorEvent read FOnError write FOnError;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnStart: TOnDownloadStartEvent read FOnStart write FOnStart;
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
    // Link HTTP events
    OnWorkBegin := DownloadStart;
    OnWork := Downloading;

    // Set the user-agent because of some issues with default
    Request.UserAgent := 'Updater/2.2 (PM Code Works Update Utility)';

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
      Synchronize(DoNotifyOnFinish);

  except
    on E: EAbort do
    begin
      DeleteFile(FFileName);
      Synchronize(DoNotifyOnCancel);
    end;  //of begin

    on E: Exception do
    begin
      DeleteFile(FFileName);
      Synchronize(DoNotifyOnError);
    end;  //of begin
  end;  //of try
end;

{ public TDownloadThread.DownloadStart

  Event that is called by TIdHttp when download starts. }

procedure TDownloadThread.DownloadStart(Sender: TObject; AWorkMode: TWorkMode;
  {$IFDEF MSWINDOWS}const AFileSize: Integer{$ELSE}AFileSize: Int64{$ENDIF});
begin
  // Convert Byte into Kilobyte (KB = Byte/1024)
  FFileSize := AFileSize div 1024;
  Synchronize(DoNotifyOnStart);
end;

{ public TDownloadThread.Downloading

  Event that is called by TIdHttp while download is in progress. }

procedure TDownloadThread.Downloading(Sender: TObject; AWorkMode: TWorkMode;
  {$IFDEF MSWINDOWS}const ADownloadSize: Integer{$ELSE}ADownloadSize: Int64{$ENDIF});
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

  result := NewFileName;
end;

{ public TDownloadThread.OnUserCancel

  Cancels downloading file if user clicks "cancel". }

procedure TDownloadThread.OnUserCancel(Sender: TObject);
begin
  Terminate;
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
    OnError(Self, FHttp.ResponseCode);
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

end.