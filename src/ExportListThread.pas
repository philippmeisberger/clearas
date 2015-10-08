{ *********************************************************************** }
{                                                                         }
{ Clearas export list thread                                              }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ExportListThread;

interface

uses
  Classes, SysUtils, ClearasAPI;

type
  { TExportListThread }
  TExportListThread = class(TThread)
  private
    FList: TRootList<TRootItem>;
    FFileName,
    FErrorMessage: string;
    FPageControlIndex: Byte;
    FOnFinish,
    FOnStart: TSearchEvent;
    FOnError: TSearchErrorEvent;
    procedure DoNotifyOnError();
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
  protected
    procedure Execute; override;
  public
    constructor Create(AList: TRootList<TRootItem>; const AFileName: string;
      APageControlIndex: Byte);
    { external }
    property OnError: TSearchErrorEvent read FOnError write FOnError;
    property OnFinish: TSearchEvent read FOnFinish write FOnFinish;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
  end;

implementation

{ TExportListThread }

{ public TExportListThread.Create

  Constructor for creating a TExportListThread instance. }

constructor TExportListThread.Create(AList: TRootList<TRootItem>;
  const AFileName: string; APageControlIndex: Byte);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FList := AList;
  FFileName := AFileName;
  FPageControlIndex := APageControlIndex;
end;

{ private TExportListThread.DoNotifyOnError

  Synchronizable event method that is called when an error has occured. }

procedure TExportListThread.DoNotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FErrorMessage);
end;

{ private TExportListThread.DoNotifyOnFinish

  Synchronizable method that is called when thread has finished. }

procedure TExportListThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self, FPageControlIndex);
end;

{ private TExportListThread.DoNotifyOnStart

  Synchronizable method that is called when thread has started. }

procedure TExportListThread.DoNotifyOnStart();
begin
  if Assigned(FOnStart) then
    FOnStart(Self, FPageControlIndex);
end;

{ protected TExportListThread.Execute

  Exports a TRootRegList as .reg file. }

procedure TExportListThread.Execute;
begin
  try
    Synchronize(DoNotifyOnStart);

    try
      FList.ExportList(FFileName);

    finally
      Synchronize(DoNotifyOnFinish);
    end;  //of try

  except
    on E: Exception do
    begin
      FErrorMessage := Format('%s: %s', [ToString(), E.Message]);
      Synchronize(DoNotifyOnError);
    end;
  end;  //of try
end;

end.
 