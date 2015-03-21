{ *********************************************************************** }
{                                                                         }
{ Clearas export list thread                                              }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit ExportListThread;

interface

uses
  Classes, ClearasAPI;

type
  { TExportListThread }
  TExportListThread = class(TThread)
  private
    FList: TRootRegList;
    FFileName: string;
    FPageControlIndex: Byte;
    FOnFinish, FOnStart: TSearchEvent;
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
  protected
    procedure Execute; override;
  public
    constructor Create(AList: TRootRegList; const AFileName: string;
      APageControlIndex: Byte);
    { external }
    property OnFinish: TSearchEvent read FOnFinish write FOnFinish;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
  end;

implementation

{ TExportListThread }

{ public TExportListThread.Create

  Constructor for creating a TExportListThread instance. }

constructor TExportListThread.Create(AList: TRootRegList; const AFileName: string;
  APageControlIndex: Byte);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FList := AList;
  FFileName := AFileName;
  FPageControlIndex := APageControlIndex;
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
  Synchronize(DoNotifyOnStart);
  FList.ExportList(FFileName);
  Synchronize(DoNotifyOnFinish);
end;

end.
 