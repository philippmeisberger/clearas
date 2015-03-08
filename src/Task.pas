unit Task;

interface

uses
  Windows, SysUtils, Classes, ShlObj, ActiveX, Variants, SyncObjs, Taskschd,
  OSUtils, ClearasAPI;

type
  { Exception }
  ETaskException = class(Exception);

  { TTaskItem }
  TTaskListItem = class(TRootItem)
  private
    FTaskDefinition: ITaskDefinition;
    FTaskFolder: ITaskFolder;
    function UpdateTask(ATaskDefinition: ITaskDefinition): Boolean;
  protected
    function GetFullLocation(): string; override;
  public
    constructor Create(AIndex: Word; AEnabled: Boolean; ATaskFolder: ITaskFolder);
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    function Delete(): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    class function GetTaskFolder(): string;
    { external }
    property Definition: ITaskDefinition read FTaskDefinition write FTaskDefinition;
  end;

  { TTaskList }
  TTaskList = class(TRootList)
  private
    FTaskService: ITaskService;
    function GetSelectedItem(): TTaskListItem;
    function ItemAt(AIndex: Word): TTaskListItem;
  protected
    function AddTaskItem(ATask: IRegisteredTask; ATaskFolder: ITaskFolder): Integer;
    function ReadUnicodeFile(const AFileName: string): WideString;
  public
    constructor Create;
    destructor Destroy; override;
    function ImportBackup(const AFileName: string): Boolean;
    procedure LoadTasks(ATaskFolder: ITaskFolder; AIncludeHidden: Boolean); overload;
    procedure LoadTasks(APath: WideString = '\'; ARecursive: Boolean = False;
      AIncludeHidden: Boolean = False); overload;
    { external }
    property Item: TTaskListItem read GetSelectedItem;
    property Items[AIndex: Word]: TTaskListItem read ItemAt; default;
  end;


implementation

uses TaskSearchThread;

{ TTaskListItem }

{ public TTaskListItem.Create

  Constructor for creating a TTaskListItem instance. }

constructor TTaskListItem.Create(AIndex: Word; AEnabled: Boolean;
  ATaskFolder: ITaskFolder);
begin
  inherited Create(AIndex, AEnabled);
  FTaskFolder := ATaskFolder;
end;

{ private TTaskListItem.UpdateTask

  Updates a task with a new definition. }

function TTaskListItem.UpdateTask(ATaskDefinition: ITaskDefinition): Boolean;
var
  NewTask: IRegisteredTask;

begin
  // Update task
  if Failed(FTaskFolder.RegisterTaskDefinition(StringToOleStr(Name),
    FTaskDefinition, Ord(TASK_UPDATE), Null, Null,
    FTaskDefinition.Principal.LogonType, Null, NewTask)) then
    raise ETaskException.Create('Could not update task definition!');

  Result := True;
end;

{ protected TTaskListItem.GetFullLocation

  Returns the full file path to a TTaskListItem. }

function TTaskListItem.GetFullLocation(): string;
begin
  Result := FilePath;
end;

{ public TTaskListItem.ChangeFilePath

  Changes the file path of an TTaskListItem item. }

function TTaskListItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  Actions: IEnumVariant;
  Action: IAction;
  ActionItem: OleVariant;
  ExecAction: IExecAction;
  Fetched: Cardinal;

begin
  Result := False;
  Actions := FTaskDefinition.Actions._NewEnum as IEnumVariant;

  // Try to find executable command in task
  while (Actions.Next(1, ActionItem, Fetched) = S_OK) do
  begin
    Action := IDispatch(ActionItem) as IAction;

    // Task has an executable?
    if (Action.ActionType = TASK_ACTION_EXEC) then
    begin
      ExecAction := Action as IExecAction;

      // Change path + arguments
      ExecAction.Path := StringToOleStr(ExtractPathToFile(ANewFilePath));
      ExecAction.Arguments := StringToOleStr(ExtractArguments(ANewFilePath));

      // Update task
      Result := UpdateTask(FTaskDefinition);
      FilePath := ANewFilePath;
      Break;
    end;  //of begin
  end;  //of while
end;

{ public TTaskListItem.Delete

  Deletes a TTaskListItem object and returns True if successful. }

function TTaskListItem.Delete(): Boolean;
begin
  // Delete task
  if Failed(FTaskFolder.DeleteTask(StringToOleStr(Name), 0)) then
    raise ETaskException.Create('Could not read task!');

  Result := True;
end;

{ public TTaskListItem.Disable

  Disables an TTaskListItem object and returns True if successful. }

function TTaskListItem.Disable(): Boolean;
begin
  FTaskDefinition.Settings.Enabled := False;
  Result := UpdateTask(FTaskDefinition);

  // Update status
  FEnabled := False;
end;

{ public TTaskListItem.Enable

  Enables an TTaskListItem object and returns True if successful. }

function TTaskListItem.Enable(): Boolean;
begin
  FTaskDefinition.Settings.Enabled := True;
  Result := UpdateTask(FTaskDefinition);

  // Update status
  FEnabled := True;
end;

{ public TTaskListItem.ExportItem

  Exports a TTaskListItem object as .xml backup file. }

procedure TTaskListItem.ExportItem(const AFileName: string);
var
  FsLocation: string;
  Win64: Boolean;

begin
  Win64 := TOSUtils.IsWindows64();
  FsLocation := GetTaskFolder() + IncludeTrailingPathDelimiter(Location) + Name;

  // Deny WOW64 redirection on 64 Bit Windows
  if Win64 then
    TOSUtils.Wow64FsRedirection(True);

  try
    // Copy file
    if not CopyFile(PChar(FsLocation), PChar(AFileName), False) then
      raise ETaskException.Create('Task does not exist!');

  finally
    // Allow WOW64 redirection on 64 Bit Windows again
    if Win64 then
      TOSUtils.Wow64FsRedirection(False);
  end;  //of try
end;

{ public TTaskListItem.GetTaskFolder

  Returns the path to the task folder. }

class function TTaskListItem.GetTaskFolder(): string;
const
  CSIDL_SYSTEM = $0025;

var
  ItemIDs: PItemIDList;
  Path: PChar;

begin
  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_SYSTEM, ItemIDs)) then
  begin
    Path := StrAlloc(MAX_PATH);
    SHGetPathFromIDList(ItemIDs, Path);
    Result := string(Path) +'\Tasks';
  end  //of begin
  else
    Result := '';
end;


{ TTaskList }

{ public TTaskList.Create

  General constructor for creating a TTaskList instance. }

constructor TTaskList.Create;
begin
  inherited Create;
  CoInitialize(nil);

  if Failed(CoInitializeSecurity(nil, -1, nil, nil, RPC_C_AUTHN_LEVEL_PKT_PRIVACY,
    RPC_C_IMP_LEVEL_IMPERSONATE, nil, 0, nil)) then
    raise ETaskException.Create('Could not register security values for process!');

  if Failed(CoCreateInstance(CLSID_TaskScheduler, nil, CLSCTX_INPROC_SERVER,
    IID_ITaskService, FTaskService)) then
    raise ETaskException.Create('Could not create task service!');

  if Failed(FTaskService.Connect(Null, Null, Null, Null)) then
    raise ETaskException.Create('Could not connect to task service!');
end;

{ public TTaskList.Destroy

  Destructor for destroying a TTaskList instance. }

destructor TTaskList.Destroy;
begin
  CoUninitialize;
  inherited Destroy;
end;

{ private TTaskList.GetSelectedItem

  Returns the current selected item as TTaskListItem. }

function TTaskList.GetSelectedItem(): TTaskListItem;
begin
  Result := TTaskListItem(Selected);
end;

{ private TTaskList.ItemAt

  Returns a TTaskList object at index. }

function TTaskList.ItemAt(AIndex: Word): TTaskListItem;
begin
  Result := TTaskListItem(RootItemAt(AIndex));
end;

{ protected TTaskList.AddTaskItem

  Adds a task item to the list. }

function TTaskList.AddTaskItem(ATask: IRegisteredTask; ATaskFolder: ITaskFolder): Integer;
var
  Item: TTaskListItem;
  Action: IAction;
  ExecAction: IExecAction;
  Actions: IEnumVariant;
  ActionItem: OleVariant;
  Fetched: Cardinal;

begin
  Item := TTaskListItem.Create(Count, ATask.Enabled, ATaskFolder);

  with Item do
  begin
    Name := ATask.Name;
    Location := ExtractFileDir(ATask.Path);
    Definition := ATask.Definition;

    // Try to find executable command in task
    Actions := Definition.Actions._NewEnum as IEnumVariant;

    while (Actions.Next(1, ActionItem, Fetched) = S_OK) do
    begin
      Action := IDispatch(ActionItem) as IAction;

      // Task has an executable?
      if (Action.ActionType = TASK_ACTION_EXEC) then
      begin
        ExecAction := Action as IExecAction;
        FilePath := ExecAction.Path;

        // Append arguments?
        if (ExecAction.Arguments <> '') then
          FilePath := FilePath +' '+ ExecAction.Arguments;

        Break;
      end;  //of begin
    end;  //of while

    TypeOf := 'Task';
  end;  //of with

  // Update active counter
  if Item.Enabled then
    Inc(FActCount);

  Result := Add(Item);
end;

{ protected TTaskList.ReadUnicodeFile

  Reads an Unicode encoded file. }

function TTaskList.ReadUnicodeFile(const AFileName: string): WideString;
var
  Stream: TMemoryStream;
  WideCharPtr: PWideChar;
  s: AnsiString;
  DestLength, SourceLength: Integer;

begin
  Stream := TMemoryStream.Create;

  try
    Stream.LoadFromFile(AFileName);
    WideCharPtr := PWideChar(Stream.Memory);
    DestLength := Stream.Size div SizeOf(WideChar);

    if ((DestLength >= 1) and (PWord(WideCharPtr)^ = $FEFF)) then
    begin
      Inc(WideCharPtr);
      Dec(DestLength);
    end;  //of begin

    SourceLength := WideCharToMultiByte(0, 0, WideCharPtr, DestLength,
      nil, 0, nil, nil);

    if (SourceLength > 0) then
    begin
      SetLength(s, SourceLength);
      WideCharToMultiByte(0, 0, WideCharPtr, DestLength, PAnsiChar(s),
        SourceLength, nil, nil);
    end;  //of begin

    Result := s;

  finally
    Stream.Free;
  end;  //of try
end;

{ public TTaskList.ImportBackup

  Imports an exported task item as .xml file and adds it to the list. }

function TTaskList.ImportBackup(const AFileName: string): Boolean;
var
  Path: WideString;
  Win64: Boolean;
  TaskFolder: ITaskFolder;
  NewTask: IRegisteredTask;
  XmlText: WideString;

begin
  Path := '\'+ ChangeFileExt(ExtractFileName(AFileName), '');
  Win64 := TOSUtils.IsWindows64();

  // Deny WOW64 redirection on 64 Bit Windows
  if Win64 then
    TOSUtils.Wow64FsRedirection(True);

  try
    // Read .xml task file
    XmlText := ReadUnicodeFile(AFileName);

  finally
    // Allow WOW64 redirection on 64 Bit Windows again
    if Win64 then
      TOSUtils.Wow64FsRedirection(False);
  end;  //of try

  // Open root folder
  if Failed(FTaskService.GetFolder('\', TaskFolder)) then
    raise ETaskException.Create('Could not open task folder!');

  // Task exists?
  if Succeeded(TaskFolder.GetTask(Addr(Path[1]), NewTask)) then
    raise EWarning.Create('Task already exists!');

  // Register new task
  if Failed(TaskFolder.RegisterTask(Addr(Path[1]), Addr(XmlText[1]),
    Ord(TASK_CREATE), Null, Null, TASK_LOGON_INTERACTIVE_TOKEN, Null,
    NewTask)) then
    raise ETaskException.Create('Could not register task!');

  // Add new task to list
  AddTaskItem(NewTask, TaskFolder);

  // Notify import
  if Assigned(OnChanged) then
    OnChanged(Self);

  Result := True;
end;

{ public TTaskList.LoadTasks

  Adds task items to the list. }

procedure TTaskList.LoadTasks(ATaskFolder: ITaskFolder; AIncludeHidden: Boolean);
var
  TaskCollection: IRegisteredTaskCollection;
  Task: IRegisteredTask;
  Tasks: IEnumVariant;
  TaskItem: OleVariant;
  Fetched: Cardinal;
  Flag: Byte;

begin
  // Show hidden?
  if AIncludeHidden then
    Flag := Ord(TASK_ENUM_HIDDEN)
  else
    Flag := 0;

  // Add tasks in current folder
  if Failed(ATaskFolder.GetTasks(Flag, TaskCollection)) then
    raise ETaskException.Create('Could not read tasks!');

  Tasks := TaskCollection._NewEnum as IEnumVariant;

  // Add tasks to list
  while (Tasks.Next(1, TaskItem, Fetched) = S_OK) do
    try
      Task := IDispatch(TaskItem) as IRegisteredTask;
      AddTaskItem(Task, ATaskFolder);

    except
      on E: ESafecallException do
        // Task currupted: Skip it!
        Continue;
    end;  //of try
end;

{ public TTaskList.LoadTasks

  Searches for task items in specific folder and adds them to the list. }

procedure TTaskList.LoadTasks(APath: WideString = '\'; ARecursive: Boolean = False;
  AIncludeHidden: Boolean = False);
var
  SearchThread: TTaskSearchThread;

begin
  // Invalid path?
  if ((APath = '') or (APath[1] <> '\')) then
    raise ETaskException.Create('Invalid path: Must start with a backslash!');

  SearchThread := TTaskSearchThread.Create(Self, FTaskService, FLock);

  with SearchThread do
  begin
    Path := APath;
    Win64 := TOSUtils.IsWindows64();
    IncludeSubFolders := ARecursive;
    IncludeHidden := AIncludeHidden;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    OnFinish := OnSearchFinish;
    Resume;
  end;  //of with
end;

end.
