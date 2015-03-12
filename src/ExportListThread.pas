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
    FOnFinish: TNotifyEvent;
    procedure DoNotifyOnFinish();
  protected
    procedure Execute; override;
  public
    constructor Create(AList: TRootRegList; const AFileName: string);
    { external }
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

implementation

{ TExportListThread }

{ public TExportListThread.Create

  Constructor for creating a TExportListThread instance. }

constructor TExportListThread.Create(AList: TRootRegList; const AFileName: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FList := AList;
  FFileName := AFileName;
end;

{ private TExportListThread.DoNotifyOnFinish

  Synchronizable method that is called when thread has finished. }

procedure TExportListThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

{ protected TExportListThread.Execute

  Exports a TRootRegList as .reg file. }

procedure TExportListThread.Execute;
begin
  FList.ExportList(FFileName);
  Synchronize(DoNotifyOnFinish);
end;

end.
 