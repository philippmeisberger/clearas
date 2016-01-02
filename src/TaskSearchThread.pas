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
  SysUtils, ActiveX, Variants, ComObj, SyncObjs, Taskschd, ClearasSearchThread,
  ClearasAPI, PMCWOSUtils;

type
  TTaskSearchThread = class(TClearasSearchThread)
  private
    FTaskService: ITaskService;
    FPath: string;
    FIncludeHidden,
    FIncludeSubFolders,
    FWin64: Boolean;
    procedure LoadTasks(APath: string);
  protected
    procedure Execute; override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TTaskSearchThread</c> instance.
    /// </summary>
    /// <param name="ATaskList">
    ///   A <see cref="TTaskList"/> to be filled.
    /// </param>
    /// <param name="ATaskService">
    ///   The task service.
    /// </param>
    /// <param name="ALock">
    ///   The mutex.
    /// </param>
    constructor Create(ATaskList: TTaskList; ATaskService: ITaskService;
      ALock: TCriticalSection);

    /// <summary>
    ///   Include tasks in subfolders.
    /// </summary>
    property IncludeSubFolders: Boolean read FIncludeSubFolders write FIncludeSubFolders;

    /// <summary>
    ///   Include hidden tasks.
    /// </summary>
    property IncludeHidden: Boolean read FIncludeHidden write FIncludeHidden;

    /// <summary>
    ///   The path to the task folder.
    /// </summary>
    property Path: string read FPath write FPath;

    /// <summary>
    ///   Use the WOW64 technology.
    /// </summary>
    property Win64: Boolean read FWin64 write FWin64;
  end;

implementation

{ TTaskSearchThread }

constructor TTaskSearchThread.Create(ATaskList: TTaskList;
  ATaskService: ITaskService; ALock: TCriticalSection);
begin
  inherited Create(TRootList<TRootItem>(ATaskList), ALock);
  FTaskService := ATaskService;
end;

procedure TTaskSearchThread.LoadTasks(APath: string);
var
  FolderCollection: ITaskFolderCollection;
  Folders: IEnumVariant;
  TaskFolder: ITaskFolder;
  FolderItem: OleVariant;
  Fetched: Cardinal;

begin
  // Open current folder
  OleCheck(FTaskService.GetFolder(PChar(APath), TaskFolder));

  // Add tasks in folder to list
  TTaskList(FSelectedList).LoadTasks(TaskFolder, FIncludeHidden);

  // Include subfolders?
  if FIncludeSubFolders then
  begin
    // Read subfolders
    OleCheck(TaskFolder.GetFolders(0, FolderCollection));
    Folders := (FolderCollection._NewEnum as IEnumVariant);

    // Search for tasks in subfolders
    while (Folders.Next(1, FolderItem, Fetched) = 0) do
    begin
      TaskFolder := (IDispatch(FolderItem) as ITaskFolder);
      LoadTasks(TaskFolder.Path);
    end;  //of while
  end;  //of begin
end;

procedure TTaskSearchThread.Execute;
begin
  FLock.Acquire();

  try
    try
      // Clear data
      FSelectedList.Clear;

      // Notify start of search
      Synchronize(DoNotifyOnStart);

    {$IFDEF WIN32}
      // Deny WOW64 redirection on 64 Bit Windows
      if FWin64 then
        Wow64FsRedirection(True);
    {$ENDIF}

      // Start searching for tasks
      LoadTasks(FPath);

    finally
    {$IFDEF WIN32}
      // Allow WOW64 redirection on 64 Bit Windows again
      if Win64 then
        Wow64FsRedirection(False);
    {$ENDIF}
      FLock.Release();

      // Notify end of search
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
