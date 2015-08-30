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
  SysUtils, ActiveX, Variants, SyncObjs, Taskschd, ClearasSearchThread,
  ClearasAPI;

type
  { TTaskSearchThread }
  TTaskSearchThread = class(TClearasSearchThread)
  private
    FTaskList: TTaskList;
    FTaskService: ITaskService;
    FPath: string;
    FIncludeHidden,
    FIncludeSubFolders,
    FWin64: Boolean;
    procedure LoadTasks(APath: string);
  protected
    procedure Execute; override;
  public
    constructor Create(ATaskList: TTaskList; ATaskService: ITaskService;
      ALock: TCriticalSection);
    { external }
    property IncludeSubFolders: Boolean read FIncludeSubFolders write FIncludeSubFolders;
    property IncludeHidden: Boolean read FIncludeHidden write FIncludeHidden;
    property Path: string read FPath write FPath;
    property Win64: Boolean read FWin64 write FWin64;
  end;

implementation

{ TTaskSearchThread }

{ public TTaskSearchThread.Create

  Constructor for creating a TTaskSearchThread instance. }

constructor TTaskSearchThread.Create(ATaskList: TTaskList;
  ATaskService: ITaskService; ALock: TCriticalSection);
begin
  inherited Create(ALock);
  FTaskList := ATaskList;
  FTaskService := ATaskService;
  FPath := '\';
end;

{ private TTaskSearchThread.LoadTasks

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

  try
    try
      // Clear data
      FTaskList.Clear;

      // Notify start of search
      Synchronize(DoNotifyOnStart);
      Synchronize(DoNotifyOnSearching);

      // Start searching for tasks
      LoadTasks(FPath);

    finally
      // Notify end of search
      Synchronize(DoNotifyOnFinish);
      FLock.Release;
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
