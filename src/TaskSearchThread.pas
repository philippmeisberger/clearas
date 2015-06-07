{ *********************************************************************** }
{                                                                         }
{ Clearas task search thread                                              }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit TaskSearchThread;

interface

uses
  Classes, ActiveX, Variants, SyncObjs, Taskschd, Task, ClearasAPI;

type
  { TTaskSearchThread }
  TTaskSearchThread = class(TThread)
  private
    FTaskList: TTaskList;
    FTaskService: ITaskService;
    FPath: string;
    FIncludeHidden, FIncludeSubFolders, FWin64: Boolean;
    FOnSearching, FOnStart: TSearchEvent;
    FOnFinish: TNotifyEvent;
    FLock: TCriticalSection;
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnSearching();
    procedure DoNotifyOnStart();
    procedure LoadTasks(APath: string);
  protected
    procedure Execute; override;
  public
    constructor Create(ATaskList: TTaskList; ATaskService: ITaskService;
      ALock: TCriticalSection);
    { external }
    property IncludeSubFolders: Boolean read FIncludeSubFolders write FIncludeSubFolders;
    property IncludeHidden: Boolean read FIncludeHidden write FIncludeHidden;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
    property Path: string read FPath write FPath default '\';
    property Win64: Boolean read FWin64 write FWin64;
  end;

implementation

{ TTaskSearchThread }

{ public TTaskSearchThread.Create

  Constructor for creating a TTaskSearchThread instance. }

constructor TTaskSearchThread.Create(ATaskList: TTaskList;
  ATaskService: ITaskService; ALock: TCriticalSection);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTaskList := ATaskList;
  FTaskService := ATaskService;
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

{ private TTaskList.LoadTasks

  Searches for task items in specific folder and adds them to the list. }

procedure TTaskSearchThread.LoadTasks(APath: string);
var
  FolderCollection: ITaskFolderCollection;
  Folders: IEnumVariant;
  TaskFolder: ITaskFolder;
  FolderItem: OleVariant;
  Fetched: Cardinal;

begin
  // Open current folder
  if Failed(FTaskService.GetFolder(Addr(APath[1]), TaskFolder)) then
    raise ETaskException.Create('Could not open task folder!');

  // Add tasks in folder to list
  FTaskList.LoadTasks(TaskFolder, FIncludeHidden);

  // Include subfolders?
  if FIncludeSubFolders then
  begin
    // Read subfolders
    if Failed(TaskFolder.GetFolders(0, FolderCollection)) then
      raise ETaskException.Create('Could not read subfolders!');

    Folders := FolderCollection._NewEnum as IEnumVariant;

    // Search for tasks in subfolders
    while (Folders.Next(1, FolderItem, Fetched) = S_OK) do
    begin
      TaskFolder := IDispatch(FolderItem) as ITaskFolder;
      LoadTasks(TaskFolder.Path);
    end;  //of while
  end;  //of begin
end;

{ protected TTaskSearchThread.Execute

  Searches for task items in specific file system folder. }

procedure TTaskSearchThread.Execute;
begin
  FLock.Acquire;

  // Clear data
  FTaskList.Clear;

  // Notify start of search
  Synchronize(DoNotifyOnStart);
  Synchronize(DoNotifyOnSearching);

  // Start searching for tasks
  LoadTasks(FPath);

  // Notify end of search
  Synchronize(DoNotifyOnFinish);
  FLock.Release;
end;

end.
