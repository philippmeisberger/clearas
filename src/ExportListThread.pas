{ *********************************************************************** }
{                                                                         }
{ Clearas export list thread                                              }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ExportListThread;

interface

uses
  Classes, SysUtils, ClearasAPI;

type
  /// <summary>
  ///   The list export event.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="APageControlIndex">
  ///   The index of the <c>TPageControl</c> on which the export was invoked.
  /// </param>
  TExportListEvent = procedure(Sender: TObject; APageControlIndex: Integer) of object;

  /// <summary>
  ///   Exports a <see cref="TRootList"/> as .reg file.
  /// </summary>
  TExportListThread = class(TThread)
  private
    FSelectedList: TRootList<TRootItem>;
    FFileName,
    FErrorMessage: string;
    FPageControlIndex: Integer;
    FOnFinish,
    FOnStart: TExportListEvent;
    FOnError: TSearchErrorEvent;
    procedure DoNotifyOnError();
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
  protected
    procedure Execute; override; final;
  public
    /// <summary>
    ///   Constructor for creating a <c>TExportListThread</c> instance.
    /// </summary>
    /// <param name="ASelectedList">
    ///   A <c>TRootList</c> to be filled.
    /// </param>
    /// <param name="ALock">
    ///   The mutex.
    /// </param>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </param>
    constructor Create(ASelectedList: TRootList<TRootItem>; const AFileName: string;
      APageControlIndex: Integer);

    /// <summary>
    ///   Occurs when search has failed.
    /// </summary>
    property OnError: TSearchErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Occurs when the export has finished.
    /// </summary>
    property OnFinish: TExportListEvent read FOnFinish write FOnFinish;

    /// <summary>
    ///   Occurs when the export has started.
    /// </summary>
    property OnStart: TExportListEvent read FOnStart write FOnStart;
  end;

implementation

{ TExportListThread }

constructor TExportListThread.Create(ASelectedList: TRootList<TRootItem>;
  const AFileName: string; APageControlIndex: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSelectedList := ASelectedList;
  FFileName := AFileName;
  FPageControlIndex := APageControlIndex;
end;

procedure TExportListThread.DoNotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FErrorMessage);
end;

procedure TExportListThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self, FPageControlIndex);
end;

procedure TExportListThread.DoNotifyOnStart();
begin
  if Assigned(FOnStart) then
    FOnStart(Self, FPageControlIndex);
end;

procedure TExportListThread.Execute;
begin
  try
    Synchronize(DoNotifyOnStart);

    try
      FSelectedList.ExportList(FFileName);

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
 