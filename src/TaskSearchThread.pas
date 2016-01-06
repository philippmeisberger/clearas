{ *********************************************************************** }
{                                                                         }
{ Clearas task search thread                                              }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
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
    procedure LoadTasks(const APath: string);
  protected
    procedure DoExecute; override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TTaskSearchThread</c> instance.
    /// </summary>
    /// <param name="ATaskList">
    ///   A <see cref="TTaskList"/> to be filled.
    /// </param>
    /// <param name="ALock">
    ///   The mutex.
    /// </param>
    /// <param name="ATaskService">
    ///   The task service.
    /// </param>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> tasks in subfolders and those who are hidden are
    ///   included.
    /// </param>
    constructor Create(ATaskList: TTaskList; ALock: TCriticalSection;
      ATaskService: ITaskService; AExpertMode: Boolean = False);

    /// <summary>
    ///   The path to the task folder.
    /// </summary>
    property Path: string read FPath write FPath;
  end;

implementation

{ TTaskSearchThread }

constructor TTaskSearchThread.Create(ATaskList: TTaskList; ALock: TCriticalSection;
  ATaskService: ITaskService; AExpertMode: Boolean = False);
begin
  inherited Create(TRootList<TRootItem>(ATaskList), ALock, AExpertMode);
  FTaskService := ATaskService;
end;

procedure TTaskSearchThread.LoadTasks(const APath: string);
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
  TTaskList(FSelectedList).LoadTasks(TaskFolder, FExpertMode);

  // Include subfolders?
  if FExpertMode then
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

procedure TTaskSearchThread.DoExecute;
begin
  try
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
  end;  //of try
end;

end.
