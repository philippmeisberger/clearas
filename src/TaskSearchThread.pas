{ *********************************************************************** }
{                                                                         }
{ Clearas task search thread                                              }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit TaskSearchThread;

interface

uses
  Classes, SyncObjs, Task, ClearasAPI;

type
  TTaskSearchThread = class(TThread)
  private
    FTaskList: TTaskList;
    FIncludeHidden, FIncludeSubFolders, FWin64: Boolean;
    FOnSearching, FOnStart: TSearchEvent;
    FOnFinish: TNotifyEvent;
    FLock: TCriticalSection;
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnSearching();
    procedure DoNotifyOnStart();
  protected
    procedure Execute; override;
  public
    constructor Create(ATaskList: TTaskList; ALock: TCriticalSection);
    { external }
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
    property Win64: Boolean read FWin64 write FWin64;
    property IncludeSubFolders: Boolean read FIncludeSubFolders write FIncludeSubFolders;
    property IncludeHidden: Boolean read FIncludeHidden write FIncludeHidden;
  end;

implementation

{ TTaskSearchThread }

{ public TTaskSearchThread.Create

  Constructor for creating a TTaskSearchThread instance. }

constructor TTaskSearchThread.Create(ATaskList: TTaskList; ALock: TCriticalSection);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTaskList := ATaskList;
  FLock := ALock;
end;

{ private TTaskSearchThread.DoNotifyOnFinish

  Synchronizable event method that is called when search has finished. }

procedure TTaskSearchThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

{ private TTaskSearchThread.DoNotifyOnSearching

  Synchronizable event method that is called when search is in progress. }

procedure TTaskSearchThread.DoNotifyOnSearching();
begin
  if Assigned(FOnSearching) then
    FOnSearching(Self, 1);
end;

{ private TTaskSearchThread.DoNotifyOnStart

  Synchronizable event method that is called when search has started. }

procedure TTaskSearchThread.DoNotifyOnStart();
begin
  if Assigned(FOnStart) then
    FOnStart(Self, 1);
end;

{ protected TTaskSearchThread.Execute

  Searches for task items in specific file system folder. }

procedure TTaskSearchThread.Execute;
begin
  FLock.Acquire;

  // Notify start of search
  Synchronize(DoNotifyOnStart);
  Synchronize(DoNotifyOnSearching);
  
  // Search for tasks
  FTaskList.LoadTasks('\', FIncludeSubFolders, FIncludeHidden);

  // Notify end of search
  Synchronize(DoNotifyOnFinish);
  FLock.Release;
end;

end.
 