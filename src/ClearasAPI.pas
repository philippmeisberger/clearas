{ *********************************************************************** }
{                                                                         }
{ Clearas API Interface Unit                                              }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasAPI;

interface

uses
  Windows, WinSvc, Classes, SysUtils, Registry, ShlObj, ActiveX, ComObj,
  CommCtrl, ShellAPI, Contnrs, SyncObjs, StrUtils, Variants, Taskschd,
  PMCWOSUtils, PMCWLanguageFile, PMCWIniFileParser;

const
  { Startup Registry keys }
  KEY_STARTUP_DISABLED      = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupreg\';
  KEY_STARTUP_USER_DISABLED = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupfolder\';
  KEY_STARTUP_RUN           = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP_RUNONCE       = 'SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce';

  { Redirected 32-Bit Registry keys by WOW64 }
  KEY_STARTUP_RUN32         = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP_RUNONCE32     = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce';

  { Service Registry keys }
  KEY_SERVICE_DISABLED      = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\services';
  KEY_SERVICE_ENABLED       = 'SYSTEM\CurrentControlSet\services\';

  { Context menu Registry subkeys + values}
  KEY_USERCHOICE            = 'Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\%s\UserChoice';
  CM_SHELL                  = 'Shell';
  CM_SHELL_DISABLED         = 'LegacyDisable';
  CM_SHELLEX                = 'ShellEx';
  CM_SHELLEX_HANDLERS       = CM_SHELLEX +'\ContextMenuHandlers';
  CM_SHELLEX_FILE           = 'CLSID\%s\InProcServer32';
  CM_SHELLNEW               = 'ShellNew';
  CM_SHELLNEW_DISABLED      = '_'+ CM_SHELLNEW;
  CM_LOCATIONS_DEFAULT      = 'Directory, Folder, *, Drive';

  { Extensions of backup files }
  EXT_COMMON                = '.CommonStartup';
  EXT_USER                  = '.Startup';

  { Description type of startup user items }
  TYPE_COMMON               = 'Startup Common';
  TYPE_COMMON_XP            = 'Common Startup';
  TYPE_USER                 = 'Startup User';
  TYPE_USER_XP              = 'Startup';

type
  { TLnkFile }
  TLnkFile = class(TObject)
  private
    FFileName, FExeFileName, FArguments, FBackupExt: string;
    function GetBackupLnk(): string;
    function GetFullPath(): string;
    function GetFullPathEscaped(): string;
  public
    constructor Create(AFileName, ABackupExtension: string); overload;
    constructor Create(AName: string; AAllUsers: Boolean); overload;
    function BackupExists(): Boolean;
    function CreateBackup(): Boolean;
    function Delete(): Boolean;
    function DeleteBackup(): Boolean;
    function Exists(): Boolean;
    class function GetBackupDir(): string;
    class function GetStartUpDir(AAllUsers: Boolean): string;
    function HasArguments(): Boolean;
    function ReadLnkFile(): Boolean;
    function WriteLnkFile(): Boolean; overload;
    function WriteLnkFile(AFileName, AExeFileName: string;
      AArguments: string = ''): Boolean; overload;
    { external }
    property Arguments: string read FArguments write FArguments;
    property BackupExt: string read FBackupExt write FBackupExt;
    property BackupLnk: string read GetBackupLnk;
    property ExeFileName: string read FExeFileName write FExeFileName;
    property FileName: string read FFileName write FFileName;
    property FullPath: string read GetFullPath;
    property FullPathEscaped: string read GetFullPathEscaped;
  end;

  { Exception classes }
  EInvalidItem = class(EAccessViolation);
  EListBlocked = class(EAbort);
  EWarning = class(EAbort);

  { TRootItem }
  TRootItem = class(TObject)
  private
    FIndex: Word;
    FName, FType, FFileName: string;
    function GetArguments(): string;
    function GetFileNameOnly(): string;
    function GetIcon(): HICON;
  protected
    FEnabled: Boolean;
    FLocation: string;
    function DeleteQuoteChars(const APath: string): string;
    function ExtractArguments(const APath: string): string;
    function ExtractPathToFile(const APath: string): string;
    function GetFullLocation(): string; virtual; abstract;
  public
    constructor Create(AIndex: Word; AEnabled: Boolean);
    function ChangeFilePath(const ANewFileName: string): Boolean; virtual; abstract;
    function ChangeStatus(): Boolean; virtual;
    function Delete(): Boolean; virtual; abstract;
    function Disable(): Boolean; virtual; abstract;
    function Enable(): Boolean; virtual; abstract;
    procedure ExportItem(const AFileName: string); virtual; abstract;
    function FileExists(): Boolean;
    function GetStatus(ALangFile: TLanguageFile): string;
    procedure OpenInExplorer();
    { external }
    property Arguments: string read GetArguments;
    property Enabled: Boolean read FEnabled;
    property FileName: string read FFileName write FFileName;
    property FileNameOnly: string read GetFileNameOnly;
    property Icon: HICON read GetIcon;
    property ItemIndex: Word read FIndex;
    property Location: string read FLocation write FLocation;
    property LocationFull: string read GetFullLocation;
    property Name: string read FName write FName;
    property TypeOf: string read FType write FType;
  end;

  { TRootRegItem }
  TRootRegItem = class(TRootItem)
  protected
    FWow64: Boolean;
    function DeleteKey(ARootKey: TRootKey; AKeyPath, AKeyName: string;
      AFailIfNotExists: Boolean = True): Boolean;
    function GetRootKey(): HKEY; virtual; abstract;
    function GetWow64Key(): string;
    procedure OpenInRegEdit(AWow64: Boolean); overload;
    function WriteTimestamp(AReg: TRegistry): string;
  public
    constructor Create(AIndex: Word; AEnabled, AWow64: Boolean);
    function GetTimestamp(AReg: TRegistry): string;
    procedure OpenInRegEdit(); overload; virtual;
    { external }
    property RootKey: HKEY read GetRootKey;
    property Wow64: Boolean read FWow64;
    property Wow64Location: string read GetWow64Key;
  end;

  { Search event }
  TSearchEvent = procedure(Sender: TObject; const ACount: Cardinal) of object;
  TSearchErrorEvent = procedure(Sender: TObject; AErrorMessage: string) of object;

  { TRootList }
  TRootList = class(TObjectList)
  private
    FItem: TRootItem;
    FOnSearchStart,
    FOnSearching: TSearchEvent;
    FOnSearchFinish: TNotifyEvent;
    FOnSearchError: TSearchErrorEvent;
    FOnChanged: TNotifyEvent;
  protected
    FActCount: Word;
    FLock: TCriticalSection;
    function RootItemAt(AIndex: Word): TRootItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(ARootItem: TRootItem): Integer; virtual;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean; virtual;
    function ChangeItemStatus(): Boolean; virtual;
    procedure Clear; override;
    function DeleteItem(): Boolean; virtual;
    function DisableItem(): Boolean; virtual;
    function EnableItem(): Boolean; virtual;
    procedure ExportItem(const AFileName: string); virtual;
    function First(): TRootItem; virtual;
    function IndexOf(AItemName: string): Integer; overload;
    function IndexOf(AItemName: string; AEnabled: Boolean): Integer; overload;
    function IsLocked(): Boolean;
    function Remove(ARootItem: TRootItem): Integer; virtual;
    function Last(): TRootItem; virtual;
    procedure Load(); virtual; abstract;
    { external }
    property Enabled: Word read FActCount;
    property Items[AIndex: Word]: TRootItem read RootItemAt; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnSearchError: TSearchErrorEvent read FOnSearchError write FOnSearchError;
    property OnSearchFinish: TNotifyEvent read FOnSearchFinish write FOnSearchFinish;
    property OnSearchStart: TSearchEvent read FOnSearchStart write FOnSearchStart;
    property Selected: TRootItem read FItem write FItem;
  end;

  { TRootRegList }
  TRootRegList = class(TRootList)
  public
    procedure ExportList(const AFileName: string); virtual; abstract;
  end;

  { Exception class }
  EStartupException = class(Exception);

  { TStartupListItem }
  TStartupListItem = class(TRootRegItem)
  private
    FRootKey: HKEY;
    FTime: string;
  protected
    function GetFullLocation(): string; override;
    function GetRootKey(): HKEY; override;
  public
    function ChangeFilePath(const ANewFileName: string): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    procedure OpenInRegEdit(); override;
    { external }
    property RootKey: HKEY read GetRootKey write FRootKey;
    property Time: string read FTime write FTime;
  end;

  { TStartupItem }
  TStartupItem = class(TStartupListItem)
  public
    function Delete(): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  end;

  { TStartupUserItem }
  TStartupUserItem = class(TStartupListItem)
  private
    FLnkFile: TLnkFile;
    function AddCircumflex(const AName: string): string;
  protected
    function GetFullLocation(): string; override;
  public
    destructor Destroy; override;
    function ChangeFilePath(const ANewFileName: string): Boolean; override;
    function Delete(): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    { external }
    property LnkFile: TLnkFile read FLnkFile write FLnkFile;
  end;

  { TStartupList }
  TStartupList = class(TRootRegList)
  private
    FDeleteBackup: Boolean;
    function DeleteBackupFile(): Boolean;
    function ItemAt(AIndex: Word): TStartupListItem;
    function GetSelectedItem(): TStartupListItem;
    function GetStartupUserType(AReg: TRegistry): string; overload;
    function GetStartupUserType(AAllUsers: Boolean): string; overload;
  protected
    function AddItemDisabled(AReg: TRegistry; AWow64: Boolean): Integer;
    function AddItemEnabled(AHKey: HKEY; AKeyPath, AName,
      AFileName: string; AWow64: Boolean): Integer;
    function AddNewStartupUserItem(AName, AFilePath: string;
      AArguments: string = ''; AAllUsers: Boolean = False): Boolean;
    function AddUserItemDisabled(AReg: TRegistry): Integer;
    function AddUserItemEnabled(ALnkFile: TLnkFile; AAllUsers: Boolean): Integer;
  public
    constructor Create;
    function Add(AFileName, AArguments, ACaption: string): Boolean; reintroduce;
    function BackupExists(): Boolean;
    function ChangeItemStatus(): Boolean; override;
    function DeleteItem(): Boolean; override;
    function EnableItem(): Boolean; override;
    procedure ExportList(const AFileName: string); override;
    function ImportBackup(const AFileName: string): Boolean;
    procedure Load(); override;
    procedure LoadDisabled(AStartupUser: Boolean; AWow64: Boolean = False);
    procedure LoadEnabled(AAllUsers: Boolean); overload;
    procedure LoadEnabled(AHKey: HKEY; ARunOnce: Boolean = False;
      AWow64: Boolean = False); overload;
    procedure LoadStartup(AIncludeRunOnce: Boolean);
    { external }
    property DeleteBackup: Boolean read FDeleteBackup write FDeleteBackup;
    property Item: TStartupListItem read GetSelectedItem;
    property Items[AIndex: Word]: TStartupListItem read ItemAt; default;
  end;

  { Alias class }
  TAutostart = TStartupList;

  { Exception class }
  EContextMenuException = class(Exception);

  TShellItemType = (stShell, stShellEx, stShellNew);

  { TContextListItem }
  TContextListItem = class(TRootRegItem)
  private
    FCaption: string;
    function GetKeyPath(): string; virtual; abstract;
  protected
    function GetFullLocation(): string; override;
    function GetRootKey(): HKEY; override;
  public
    function Delete(): Boolean; override;
    function DeleteUserChoice(AFileExtension: string): Boolean;
    function UserChoiceExists(AFileExtension: string): Boolean;
    { external }
    property Caption: string read FCaption write FCaption;
    property Location: string read GetKeyPath;
    property LocationRoot: string read FLocation write FLocation;
  end;

  { TShellItem }
  TShellItem = class(TContextListItem)
  private
    function GetKeyPath(): string; override;
  public
    function ChangeFilePath(const ANewFileName: string): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
  end;

  { TShellExItem }
  TShellExItem = class(TContextListItem)
  private
    function ChangeStatus(ANewStatus: Boolean): Boolean;
    function GetKeyPath(): string; override;
  public
    function ChangeFilePath(const ANewFileName: string): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
  end;

  { TShellNewItem }
  TShellNewItem = class(TContextListItem)
  private
    function ChangeStatus(ANewStatus: Boolean): Boolean;
    function GetKeyPath(): string; override;
  public
    function ChangeFilePath(const ANewFileName: string): Boolean; override;
    function Delete(): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
  end;

  { TContextList }
  TContextList = class(TRootRegList)
  private
    function GetSelectedItem(): TContextListItem;
    function ItemAt(AIndex: Word): TContextListItem;
  protected
    function AddShellItem(const AName, ALocationRoot, AFileName, ACaption: string;
      AEnabled, AWow64: Boolean): Integer;
    function AddShellExItem(const AName, ALocationRoot, AFileName: string;
      AEnabled, AWow64: Boolean): Integer;
    function AddShellNewItem(const AName, ALocationRoot, ACaption: string;
      AEnabled, AWow64: Boolean): Integer;
  public
    constructor Create;
    function Add(AFileName, AArguments, ALocationRoot, ACaption: string;
      AExtended: Boolean = False): Boolean; reintroduce;
    procedure ExportList(const AFileName: string); override;
    function IndexOf(AName, ALocationRoot: string): Integer; overload;
    procedure LoadContextmenu(const ALocationRoot: string;
      AWow64: Boolean); overload;
    procedure Load(); override;
    procedure LoadContextmenu(const ALocationRoot: string;
      AShellItemType: TShellItemType; AWow64: Boolean); overload;
    procedure LoadContextMenus(ALocationRootCommaList: string = '');
    { external }
    property Item: TContextListItem read GetSelectedItem;
    property Items[AIndex: Word]: TContextListItem read ItemAt; default;
  end;

  { Exception class }
  EServiceException = class(Exception);

  { Service enums }
  TServiceStart = (ssBoot, ssSystem, ssAutomatic, ssManual, ssDisabled);

  { TServiceListItem }
  TServiceListItem = class(TRootRegItem)
  private
    FServiceManager: SC_HANDLE;
    FCaption, FTime: string;
    FServiceStart: TServiceStart;
    function GetLocation(): string;
    function GetHandle(AAccess: DWORD): SC_HANDLE;
  protected
    function GetFullLocation(): string; override;
    function GetRootKey(): HKEY; override;
  public
    constructor Create(AIndex: Word; AEnabled: Boolean;
      AServiceManager: SC_HANDLE);
    function ChangeFilePath(const ANewFileName: string): Boolean; override;
    function Delete(): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    function GetStartText(ALangFile: TLanguageFile): string;
    { external }
    property Caption: string read FCaption write FCaption;
    property Location: string read GetLocation;
    property Manager: SC_HANDLE read FServiceManager write FServiceManager;
    property Start: TServiceStart read FServiceStart write FServiceStart;
    property Time: string read FTime write FTime;
  end;

  { TServiceList }
  TServiceList = class(TRootRegList)
  private
    FManager: SC_HANDLE;
    function ItemAt(AIndex: Word): TServiceListItem;
    function GetSelectedItem(): TServiceListItem;
  protected
    function AddServiceDisabled(AName, ACaption, AFileName: string;
      AReg: TRegistry): Integer;
    function AddServiceEnabled(AName, ACaption, AFileName: string;
      AStart: TServiceStart): Integer;
  public
    constructor Create;
    destructor Destroy(); override;
    function Add(AFileName, AArguments, ACaption: string): Boolean; reintroduce;
    procedure ExportList(const AFileName: string); override;
    function IndexOf(const ACaptionOrName: string): Integer;
    procedure Load(); override;
    function LoadService(AName: string; AService: SC_HANDLE;
      AIncludeDemand: Boolean = False): Integer;
    procedure LoadServices(AIncludeShared: Boolean);
    { external }
    property Item: TServiceListItem read GetSelectedItem;
    property Items[AIndex: Word]: TServiceListItem read ItemAt; default;
  end;

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
    { external }
    property Definition: ITaskDefinition read FTaskDefinition write FTaskDefinition;
  end;

  { TTaskList }
  TTaskList = class(TRootRegList)
  private
    FTaskService: ITaskService;
    function GetSelectedItem(): TTaskListItem;
    function ItemAt(AIndex: Word): TTaskListItem;
  protected
    function AddTaskItem(ATask: IRegisteredTask; ATaskFolder: ITaskFolder): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function ImportBackup(const AFileName: string): Boolean;
    procedure LoadTasks(ATaskFolder: ITaskFolder; AIncludeHidden: Boolean); overload;
    procedure LoadTasks(APath: string = '\'; ARecursive: Boolean = False;
      AIncludeHidden: Boolean = False); overload;
    { external }
    property Item: TTaskListItem read GetSelectedItem;
    property Items[AIndex: Word]: TTaskListItem read ItemAt; default;
  end;


implementation

uses StartupSearchThread, ContextSearchThread, ServiceSearchThread, TaskSearchThread;

{ TLnkFile }

{ public TLnkFile.Create

  Constructor for creating a TLnkFile instance. }

constructor TLnkFile.Create(AFileName, ABackupExtension: string);
begin
  inherited Create;
  FFileName := AFileName;
  FBackupExt := ABackupExtension;
end;

{ public TLnkFile.Create

  Constructor for creating a TLnkFile instance. }

constructor TLnkFile.Create(AName: string; AAllUsers: Boolean);
begin
  inherited Create;
  FFileName := GetStartUpDir(AAllUsers) + AName;

  if AAllUsers then
    FBackupExt := EXT_COMMON
  else
    FBackupExt := EXT_USER;
end;

{ private TLnkFile.GetBackupLnk

  Returns the absoulte path to the backup lnk file. }

function TLnkFile.GetBackupLnk(): string;
begin
  Result := GetBackupDir() + ExtractFileName(FFileName) + FBackupExt;
end;

{ private TLnkFile.GetFullPath

  Returns the concatenation of file name and arguments. }

function TLnkFile.GetFullPath(): string;
begin
  if HasArguments() then
    Result := FExeFileName +' '+ FArguments
  else
    Result := FExeFileName;
end;

{ private TLnkFile.GetFullPathEscaped

  Returns the concatenation of file name and arguments escaped with quotes. }

function TLnkFile.GetFullPathEscaped(): string;
begin
  if HasArguments() then
    Result := '"'+ FFileName +'" '+ FArguments
  else
    Result := '"'+ FFileName +'"';
end;

{ public TLnkFile.BackupExists

  Returns True if the backup .lnk file in C:\Windows\pss\ exists. }

function TLnkFile.BackupExists(): Boolean;
begin
  Result := FileExists(GetBackupLnk());
end;

{ public TLnkFile.CreateBackup

  Creates a backup .lnk file in C:\Windows\pss\. }

function TLnkFile.CreateBackup(): Boolean;
begin
  Result := CopyFile(PChar(FFileName), PChar(GetBackupLnk()), False);
end;

{ public TLnkFile.Delete

  Deletes the .lnk file. }

function TLnkFile.Delete(): Boolean;
begin
  Result := DeleteFile(FFileName);
end;

{ public TLnkFile.DeleteBackup

  Deletes the backup .lnk file. }

function TLnkFile.DeleteBackup(): Boolean;
begin
  Result := DeleteFile(GetBackupLnk());
end;

{ public TLnkFile.Exists

  Returns True if the .lnk file exists. }

function TLnkFile.Exists(): Boolean;
begin
  Result := FileExists(FFileName);
end;

{ public TLnkFile.GetBackupDir

  Returns the path to the backup directory. }

class function TLnkFile.GetBackupDir(): string;
begin
  Result := GetWinDir() +'\pss\';
end;

{ public TLnkFile.GetStartUpDir

  Returns the file system startup location of current user or all. }

class function TLnkFile.GetStartUpDir(AAllUsers: Boolean): string;
var
  ItemIDs: PItemIDList;
  Path: PChar;
  Folder: Cardinal;

begin
  if AAllUsers then
    Folder := CSIDL_COMMON_STARTUP
  else
    Folder := CSIDL_STARTUP;

  if Succeeded(SHGetSpecialFolderLocation(0, Folder, ItemIDs)) then
  begin
    Path := StrAlloc(MAX_PATH);
    SHGetPathFromIDList(ItemIDs, Path);
    Result := IncludeTrailingBackslash(string(Path));
  end  //of begin
  else
    Result := '';
end;

{ public TLnkFile.HasArguments

  Returns if arguments are specified. }

function TLnkFile.HasArguments(): Boolean;
begin
  Result := (FArguments <> '');
end;

{ public TLnkFile.ReadLnkFile

  Reads the .exe file path and arguments from a .lnk file. }

function TLnkFile.ReadLnkFile(): Boolean;
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  FileInfo: TWin32FindData;
  Path, Arguments: string;

begin
  Result := False;

  try
    CoInitialize(nil);

    if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
      IID_IShellLink, ShellLink)) then
    begin
      PersistFile := (ShellLink as IPersistFile);

      // Try to .lnk read file
      if Failed(PersistFile.Load(StringToOleStr(FFileName), STGM_READ)) then
        raise Exception.Create('Lnk file does not exist!');

      SetLength(Path, MAX_PATH + 1);

      // Try to read path from .lnk
      if Failed(ShellLink.GetPath(PChar(Path), MAX_PATH, FileInfo, SLR_ANY_MATCH)) then
        raise Exception.Create('Could not read path from .lnk file!');

      FExeFileName := PChar(Path);
      SetLength(Arguments, MAX_PATH + 1);

      // Try to read arguments from .lnk file
      if Succeeded(ShellLink.GetArguments(PChar(Arguments), MAX_PATH)) then
        FArguments := PChar(Arguments);

      Result := True;
    end;  //of begin

  finally
    CoUninitialize();
  end;  //of try
end;

{ public TLnkFile.WriteLnkFile

  Creates a new .lnk file. }

function TLnkFile.WriteLnkFile(): Boolean;
begin
  Result := WriteLnkFile(FFileName, FExeFileName, FArguments);
end;

{ public TLnkFile.WriteLnkFile

  Creates a new .lnk file. }

function TLnkFile.WriteLnkFile(AFileName, AExeFileName: string;
  AArguments: string = ''): Boolean;
var
  ShellLink : IShellLink;
  PersistFile : IPersistFile;

begin
  Result := False;

  if (AFileName = '') then
    raise EArgumentException.Create('File name for .lnk file must not be empty!');

  if (AExeFileName = '') then
    raise EArgumentException.Create('File path to .exe must not be empty!');

  try
    CoInitialize(nil);

    if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
      IID_IShellLink, ShellLink)) then
    begin
      // Set path to .exe
      if Failed(ShellLink.SetPath(PChar(AExeFileName))) then
        raise Exception.Create('Could not write path to .lnk file!');

      // Set arguments if specified
      if (AArguments <> '') then
        if Failed(ShellLink.SetArguments(PChar(AArguments))) then
          raise Exception.Create('Could not write arguments to .lnk file!');

      // Set working directory
      ShellLink.SetWorkingDirectory(PChar(ExtractFilePath(AExeFileName)));

      // Save .lnk
      if Succeeded(ShellLink.QueryInterface(IPersistFile, PersistFile)) then
        Result := Succeeded(PersistFile.Save(PChar(AFileName), True));
    end; //of begin

  finally
    CoUninitialize();
  end;  //of try
end;


{ TRootItem }

{ public TRootItem.Create

  General constructor for creating a TRootItem instance. }

constructor TRootItem.Create(AIndex: Word; AEnabled: Boolean);
begin
  inherited Create;
  FIndex := AIndex;
  FEnabled := AEnabled;
end;

{ private TRootItem.GetArguments

  Returns the arguments of the item file path. }

function TRootItem.GetArguments(): string;
begin
  Result := DeleteQuoteChars(ExtractArguments(FFileName));
end;

{ private TRootItem.GetFileNameOnly

  Returns only the file name of the item (without arguments). }

function TRootItem.GetFileNameOnly(): string;
var
  Path: string;

begin
  Path := DeleteQuoteChars(ExtractPathToFile(FFileName));

  // Path has to be expanded?
  if ((Path <> '') and (Path[1] = '%')) then
    ExpandEnvironmentVar(Path);

  Result := Path;
end;

{ private TRootItem.GetIcon

  Returns the icon handle to the item file path. }

function TRootItem.GetIcon(): HICON;
var
  FileInfo: SHFILEINFO;
{$IFDEF WIN32}
  Win64: Boolean;
{$ENDIF}

begin
{$IFDEF WIN32}
  Win64 := (TOSVersion.Architecture = arIntelX64);

  // Deny WOW64 redirection only on 64bit Windows
  if Win64 then
    Wow64FsRedirection(True);
{$ENDIF}

  if Succeeded(SHGetFileInfo(PChar(GetFileNameOnly()), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_ICON or SHGFI_SMALLICON)) then
    Result := FileInfo.hIcon
  else
    Result := 0;

{$IFDEF WIN32}
  // Allow WOW64 redirection only on 64bit Windows
  if Win64 then
    Wow64FsRedirection(False);
{$ENDIF}
end;

{ protected TRootItem.DeleteQuoteChars

  Deletes quote chars from a file path. }

function TRootItem.DeleteQuoteChars(const APath: string): string;
begin
  Result := StringReplace(APath , '"', '', [rfReplaceAll]);
end;

{ protected TRootItem.ExtractArguments

  Extracts the arguments from a file path. }

function TRootItem.ExtractArguments(const APath: string): string;
var
  ExtWithArguments: string;
  SpaceDelimiter: Integer;

begin
  // Cut path from extension until end
  ExtWithArguments := ExtractFileExt(APath);

  // Find space delimter between extension and arguments
  SpaceDelimiter := AnsiPos(' ', ExtWithArguments);

  // No space char after extension: no arguments!
  if (SpaceDelimiter = 0) then
    Exit;

  // Copy arguments without entension and space char at front and end
  Result := Trim(Copy(ExtWithArguments, SpaceDelimiter, Length(ExtWithArguments)));
end;

{ protected TRootItem.ExtractPathToFile

  Extracts the absolute file path + name without arguments from a path. }

function TRootItem.ExtractPathToFile(const APath: string): string;
var
  ArgumentsIndex: Integer;

begin
  // Find index of arguments
  ArgumentsIndex := AnsiPos(ExtractArguments(APath), APath);

  // Copy path without arguments
  if (ArgumentsIndex > 0) then
    Result := Trim(Copy(APath, 0, ArgumentsIndex - 1))
  else
    Result := APath;

  // Add missing quote
  if ((Result = '"') and (Result[Length(Result)] <> '"')) then
    Result := Result +'"';
end;

{ public TRootItem.ChangeStatus

  Changes the item status. }

function TRootItem.ChangeStatus(): Boolean;
begin
  if FEnabled then
    Result := Disable()
  else
    Result := Enable();
end;

{ public TRootItem.FileExists

  Returns True if the file exists. }

function TRootItem.FileExists(): Boolean;
{$IFDEF WIN32}
var
  Win64: Boolean;
{$ENDIF}

begin
{$IFDEF WIN32}
  // 64bit Windows?
  Win64 := (TOSVersion.Architecture = arIntelX64);

  // Deny WOW64 redirection only on 64bit Windows
  if Win64 then
    Wow64FsRedirection(True);
{$ENDIF}

  Result := SysUtils.FileExists(GetFileNameOnly());

{$IFDEF WIN32}
  // Allow WOW64 redirection only on 64bit Windows
  if Win64 then
    Wow64FsRedirection(False);
{$ENDIF}
end;

{ public TRootItem.GetStatus

  Returns the item status as text. }

function TRootItem.GetStatus(ALangFile: TLanguageFile): string;
begin
  if FEnabled then
    Result := ALangFile.GetString(102)
  else
    Result := ALangFile.GetString(103);
end;

{ public TRootItem.OpenInExplorer

  Opens an TRootItem object in Explorer. }

procedure TRootItem.OpenInExplorer();
var
  PreparedFileName: string;
{$IFDEF WIN32}
  Win64: Boolean;
{$ENDIF}

begin
  // Extract the file path only (without arguments and quote chars)
  PreparedFileName := GetFileNameOnly();

{$IFDEF WIN32}
  // 64bit Windows?
  Win64 := (TOSVersion.Architecture = arIntelX64);

  // Deny WOW64 redirection only on 64bit Windows
  if Win64 then
    Wow64FsRedirection(True);
{$ENDIF}

  // Open file in explorer
  if ((PreparedFileName <> '') and SysUtils.FileExists(PreparedFileName)) then
    ExecuteProgram('explorer.exe', '/select, '+ PreparedFileName)
  else
    raise EWarning.Create('File "'+ PreparedFileName +'" does not exist!');

{$IFDEF WIN32}
  // Allow WOW64 redirection only on 64bit Windows
  if Win64 then
    Wow64FsRedirection(False);
{$ENDIF}
end;


{ TRootRegItem }

{ public TRootRegItem.Create

  General constructor for creating a TRootRegItem instance. }

constructor TRootRegItem.Create(AIndex: Word; AEnabled, AWow64: Boolean);
begin
  inherited Create(AIndex, AEnabled);
  FWow64 := AWow64;
end;

{ protected TRootRegItem.DeleteKey

  Deletes a Registry key. }

function TRootRegItem.DeleteKey(ARootKey: TRootKey; AKeyPath, AKeyName: string;
  AFailIfNotExists: Boolean = True): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := StrToHKey(ARootKey);

    if not Reg.OpenKey(AKeyPath, False) then
      raise Exception.Create('Key does not exist!');

    if Reg.KeyExists(AKeyName) then
      Result := Reg.DeleteKey(AKeyName)
    else
      if AFailIfNotExists then
        raise EWarning.Create('Key already deleted!')
      else
        Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ protected TRootRegItem.GetWow64Key

  Returns the virtualized Registry key by WOW64. }

function TRootRegItem.GetWow64Key(): string;
begin
  if (FEnabled and FWow64) then
  begin
    if (ExtractFileName(FLocation) = 'RunOnce') then
      Result := KEY_STARTUP_RUNONCE32
    else
      Result := KEY_STARTUP_RUN32;
  end  //of begin
  else
    Result := FLocation;
end;

{ protected TRootRegItem.OpenInRegEdit

  Opens a TRootRegItem object in either 32- or 64-Bit RegEdit. }

procedure TRootRegItem.OpenInRegEdit(AWow64: Boolean);
const
  KEY_REGEDIT = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Applets\Regedit';

var
  Reg: TRegistry;
{$IFDEF WIN64}
  SystemWOW64: string;
{$ENDIF}

begin
  if AWow64 then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(KEY_REGEDIT, True);

    // Set the Registry key to show
    Reg.WriteString('LastKey', 'Computer\'+ GetFullLocation());

    // Redirected 32-Bit item?
    if AWow64 then
    begin
      // Execute 32-Bit RegEdit
    {$IFDEF WIN64}
      GetSystemWow64Directory(SystemWOW64);
      ExecuteProgram(SystemWOW64 +'regedit.exe');
    {$ELSE}
      ExecuteProgram('regedit.exe');
    {$ENDIF}
    end  //of begin
    else
    begin
      // Execute 64-Bit RegEdit
      Wow64FsRedirection(True);
      ExecuteProgram('regedit.exe');
      Wow64FsRedirection(False);
    end;  //of if

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ protected TRootRegItem.WriteTimestamp

  Writes the deactivation timestamp. }

function TRootRegItem.WriteTimestamp(AReg: TRegistry): string;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  TimeNow: TDateTime;

begin
  try
    // Read current time and update current deactivation timestamp
    TimeNow := Now();
    Result := FormatDateTime('c', TimeNow);

    // Split current date
    DecodeDate(TimeNow, Year, Month, Day);

    // Split current time
    DecodeTime(TimeNow, Hour, Min, Sec, MSec);

    // Write time stamp
    with AReg do
    begin
      WriteInteger('YEAR', Year);
      WriteInteger('MONTH', Month);
      WriteInteger('DAY', Day);
      WriteInteger('HOUR', Hour);
      WriteInteger('MINUTE', Min);
      WriteInteger('SECOND', Sec);
    end;  //of with

  except
    on E: Exception do
    begin
      E.Message := 'Error while writing deactivation timestamp: '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TRootRegItem.OpenInRegEdit

  Opens a TRootRegItem object in RegEdit. }

procedure TRootRegItem.OpenInRegEdit();
begin
  OpenInRegEdit(FWow64);
end;

{ public TRootRegItem.GetTimestamp

  Returns the deactivation timestamp. }

function TRootRegItem.GetTimestamp(AReg: TRegistry): string;
var
  Year, Month, Day, Hour, Min, Sec: Word;
  Date, Time: string;

begin
  // Deactivation timestamp only available for disabled items
  if FEnabled then
  begin
    Result := '';
    Exit;
  end;  //of begin

  try
    // At least one valid date entry exists?
    if AReg.ValueExists('YEAR') then
    begin
      Year := AReg.ReadInteger('YEAR');
      Month := AReg.ReadInteger('MONTH');
      Day := AReg.ReadInteger('DAY');
      Hour := AReg.ReadInteger('HOUR');
      Min := AReg.ReadInteger('MINUTE');
      Sec := AReg.ReadInteger('SECOND');
      Date := FormatDateTime('c', EncodeDate(Year, Month, Day));
      Time := FormatDateTime('tt', EncodeTime(Hour, Min, Sec, 0));
      Result := Date +'  '+ Time;
    end;  //of if

  except
    // Do not raise exception: Corrupted date is not fatal!
    Result := '';
  end;  //of try
end;


{ TRootList }

{ public TRootList.Create

  General constructor for creating a TRootList instance. }

constructor TRootList.Create;
begin
  inherited Create;
  FActCount := 0;
  FLock := TCriticalSection.Create;
end;

{ public TRootList.Destroy

  General destructor for destroying a TRootList instance. }

destructor TRootList.Destroy;
begin
  FLock.Free;
  Clear();
  inherited Destroy;
end;

{ protected TRootList.RootItemAt

  Returns a TRootItem object at index. }

function TRootList.RootItemAt(AIndex: Word): TRootItem;
begin
  Result := TRootItem(inherited Items[AIndex]);
end;

{ public TRootList.Clear

  Deletes all items in the list. }

procedure TRootList.Clear;
begin
  inherited Clear;
  FActCount := 0;
  FItem := nil;
end;

{ public TRootList.Add

  Adds a TRootItem to the list. }

function TRootList.Add(ARootItem: TRootItem): Integer;
begin
  Result := inherited Add(ARootItem);
end;

{ public TRootList.ChangeItemFilePath

  Changes the file path of an item. }

function TRootList.ChangeItemFilePath(const ANewFilePath: string): Boolean;
var
  Changed: Boolean;

begin
  Result := False;

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    // Invalid item?
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    // Change item file path
    Changed := FItem.ChangeFilePath(ANewFilePath);

    // Notify changed
    if (Changed and Assigned(FOnChanged)) then
      FOnChanged(Self);

    Result := Changed;

  finally
    FLock.Release();
  end;  //of try
end;

{ public TRootList.ChangeItemStatus

  Changes the item status. }

function TRootList.ChangeItemStatus(): Boolean;
var
  Changed: Boolean;

begin
  Result := False;

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    // Change the status
    Changed := FItem.ChangeStatus();

    // Successful?
    if Changed then
    begin
      // Item has been enabled?
      if FItem.Enabled then
        Inc(FActCount)
      else
        Dec(FActCount);

      // Notify changed
      if Assigned(FOnChanged) then
        FOnChanged(Self);
    end;  //of begin

    Result := Changed;

  finally
    FLock.Release();
  end;  //of try
end;

{ public TRootList.DeleteItem

  Deletes an item from location and list. }

function TRootList.DeleteItem(): Boolean;
var
  Deleted: Boolean;

begin
  Result := False;

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    // Delete task from filesystem
    Deleted := FItem.Delete();

    // Successful?
    if Deleted then
    begin
      // Item was enabled
      if FItem.Enabled then
        // Update active counter
        Dec(FActCount);

      // Remove item from list
      Remove(FItem);
      FItem := nil;

      // Notify delete
      if Assigned(FOnChanged) then
        FOnChanged(Self);
    end;  //of begin

    Result := Deleted;

  finally
    FLock.Release();
  end;  //of try
end;

{ public TRootList.DisableItem

  Disables the current selected item. }

function TRootList.DisableItem(): Boolean;
var
  Disabled: Boolean;

begin
  Result := False;

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    if not FItem.Enabled then
      raise EWarning.Create('Item already disabled!');

    // Disable item
    Disabled := FItem.Disable();

    if Disabled then
    begin
      // Update active counter
      Dec(FActCount);

      // Notify disable
      if Assigned(FOnChanged) then
        FOnChanged(Self);
    end;  //of begin

    Result := Disabled;

  finally
    FLock.Release();
  end;  //of try
end;

{ public TRootList.EnableItem

  Enables the current selected item. }

function TRootList.EnableItem(): Boolean;
var
  Enabled: Boolean;

begin
  Result := False;

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    if FItem.Enabled then
      raise EWarning.Create('Item already enabled!');

    // Enable item
    Enabled := FItem.Enable();

    if Enabled then
    begin
      // Update active counter
      Inc(FActCount);

      // Notify enable
      if Assigned(FOnChanged) then
        FOnChanged(Self);
    end;  //of begin

    Result := Enabled;

  finally
    FLock.Release();
  end;  //of try
end;

{ public TRootList.ExportItem

  Exports an item as file. }

procedure TRootList.ExportItem(const AFileName: string);
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  FItem.ExportItem(AFileName);
end;

{ public TRootList.First

  Returns the first TRootItem object in list. }

function TRootList.First(): TRootItem;
begin
  Result := TRootItem(inherited First());
end;

{ public TRootList.IndexOf

  Returns the index of an item checking name only. }

function TRootList.IndexOf(AItemName: string): Integer;
var
  i: Integer;

begin
  Result := -1;

  for i := 0 to Count -1 do
    if (RootItemAt(i).Name = AItemName) then
    begin
      Result := i;
      Break;
    end;  //of begin
end;

{ public TRootList.IndexOf

  Returns the index of an item checking name and status. }

function TRootList.IndexOf(AItemName: string; AEnabled: Boolean): Integer;
var
  i: Integer;
  Item: TRootItem;

begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    Item := RootItemAt(i);

    if ((Item.Name = AItemName) and (Item.Enabled = AEnabled)) then
    begin
      Result := i;
      Break;
    end;  //of begin
  end;  //of for
end;

{ public TRootList.Last

  Returns the last item of list. }

function TRootList.Last(): TRootItem;
begin
  Result := TRootItem(inherited Last());
end;

{ public TRootList.IsLocked

  Checks if the list is currently locked. }

function TRootList.IsLocked(): Boolean;
var
  Entered: Boolean;

begin
  Entered := FLock.TryEnter();

  if Entered then
    FLock.Release();

  Result := not Entered;
end;

{ public TRootList.Remove

  Removes a TRootItem object from the list. }

function TRootList.Remove(ARootItem: TRootItem): Integer;
begin
  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    Result := inherited Remove(ARootItem);

  finally
    FLock.Release();
  end;  //of try
end;


{ TStartupListItem }

{ protected TStartupListItem.GetFullLocation

  Returns the full Registry path to a TStartupListItem. }

function TStartupListItem.GetFullLocation(): string;
begin
  Result := HKeyToStr(FRootKey) +'\'+ FLocation;
end;

{ protected TStartupListItem.GetRootKey

  Returns the HKEY of an TStartupListItem. }

function TStartupListItem.GetRootKey(): HKEY;
begin
  Result := FRootKey;
end;

{ public TStartupListItem.ChangeFilePath

  Changes the file path of an TStartupListItem. }

function TStartupListItem.ChangeFilePath(const ANewFileName: string): Boolean;
var
  Reg: TRegistry;
  ItemName: string;

begin
  Result := False;

  if (FEnabled and FWow64) then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := FRootKey;

    // Invalid key?
    if not Reg.OpenKey(FLocation, False) then
      raise EStartupException.Create('Key does not exist!');

    if FEnabled then
      ItemName := Name
    else
      ItemName := 'command';

    // Value must exist!
    if not Reg.ValueExists(ItemName) then
      raise EStartupException.Create('Value does not exist!');

    // Change path
    Reg.WriteString(ItemName, ANewFileName);
    FileName := ANewFileName;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TStartupListItem.ExportItem

  Exports an list item as .reg file. }

procedure TStartupListItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    if FEnabled then
      RegFile.ExportReg(FRootKey, GetWow64Key(), Name)
    else
      RegFile.ExportReg(FRootKey, FLocation, False);

  finally
    RegFile.Free;
  end;  //of try
end;


{ TStartupItem }

{ public TStartupItem.Delete

  Deletes a TStartupItem object and returns True if successful. }

function TStartupItem.Delete(): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;

  if (FEnabled and FWow64) then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    if FEnabled then
    begin
      Reg.RootKey := FRootKey;

      // Key invalid?
      if not Reg.OpenKey(FLocation, False) then
        raise EStartupException.Create('Key does not exist!');

      // Delete value
      if not Reg.DeleteValue(Name) then
        raise EStartupException.Create('Could not delete value!');
    end  //of begin
    else
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;

      // Key invalid?
      if not Reg.OpenKey(KEY_STARTUP_DISABLED, False) then
        raise EStartupException.Create('Key does not exist!');

      // Delete key
      if not Reg.DeleteKey(Name) then
        raise EStartupException.Create('Could not delete key!');
    end;  //of if

    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TStartupItem.Disable

  Disables an TStartupItem object and returns True if successful. }

function TStartupItem.Disable(): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Failed to create new key?
    if not Reg.OpenKey(KEY_STARTUP_DISABLED + Name, True) then
      raise EStartupException.Create('Could not create key!');

    // Write redirected key
    if FWow64 then
    begin
      if (FLocation = KEY_STARTUP_RUNONCE) then
        FLocation := KEY_STARTUP_RUNONCE32
      else
        FLocation := KEY_STARTUP_RUN32;
    end;  //of begin

    // Write values
    Reg.WriteString('hkey', HKeyToStr(FRootKey, False));
    Reg.WriteString('key', FLocation);
    Reg.WriteString('item', Name);
    Reg.WriteString('command', FileName);
    Reg.WriteString('inimapping', '0');

    // Windows >= Vista?
    if TOSVersion.Check(6) then
      // Save deactivation timestamp
      FTime := WriteTimestamp(Reg);

    // Open startup location
    Reg.CloseKey();

    // Redirect to 32 Bit key?
    if FWow64 then
      Reg.Access := KEY_WOW64_32KEY or KEY_READ or KEY_WRITE;

    Reg.RootKey := FRootKey;
    Reg.OpenKey(FLocation, False);

    // Delete old value, but do not fail if old value does not exist!
    if (Reg.ValueExists(Name) and not Reg.DeleteValue(Name)) then
      raise EStartupException.Create('Could not delete value!');

    // Update information
    FRootKey := HKEY_LOCAL_MACHINE;
    FLocation := KEY_STARTUP_DISABLED + Name;
    FEnabled := False;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TStartupItem.Enable

  Enables an TStartupItem object and returns True if successful. }

function TStartupItem.Enable(): Boolean;
var
  Reg: TRegistry;
  NewHKey, NewKeyPath: string;
  Access64: Cardinal;

begin
  Result := False;
  Access64 := KEY_WOW64_64KEY or KEY_READ;
  Reg := TRegistry.Create(Access64);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey(FLocation, False) then
      raise EStartupException.Create('Key does not exist!');

    if (not Reg.ValueExists('hkey') or not Reg.ValueExists('key')) then
      raise EStartupException.Create('Missing destination Registry values '
        +'''hkey'' or ''key''!');

    // Set new values
    NewHKey := Reg.ReadString('hkey');
    NewKeyPath := Reg.ReadString('key');

    if ((NewHKey = '') or (NewKeyPath = '')) then
      raise EStartupException.Create('Invalid destination Registry values for '
        +'hkey or key!');

    Reg.CloseKey;

    // Redirect to 32 Bit key?
    if FWow64 then
    begin
      // RunOnce item?
      if (ExtractFileName(NewKeyPath) = 'RunOnce') then
        NewKeyPath := KEY_STARTUP_RUNONCE
      else
        NewKeyPath := KEY_STARTUP_RUN;

      Reg.Access := KEY_WOW64_32KEY or KEY_READ or KEY_WRITE;
    end  //of begin
    else
      Reg.Access := Access64 or KEY_WRITE;

    Reg.RootKey := StrToHKey(NewHKey);

    // Failed to create new key?
    if not Reg.OpenKey(NewKeyPath, True) then
      raise EStartupException.Create('Could not create startup key!');

    // Write startup entry
    Reg.WriteString(Name, FileName);

    // Delete old key
    Reg.CloseKey();
    Reg.Access := Access64 or KEY_WRITE;
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey(KEY_STARTUP_DISABLED, True) then
      raise EStartupException.Create('Could not open Key '''+ KEY_STARTUP_DISABLED +'''!');

    // Do not abort if old key does not exist!
    if (Reg.KeyExists(Name) and not Reg.DeleteKey(Name)) then
      raise EStartupException.Create('Could not delete old key '''+ Name +'''!');

    // Update information
    FRootKey := StrToHKey(NewHKey);
    FLocation := NewKeyPath;
    FEnabled := True;
    FTime := '';
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;


{ TStartupUserItem }

{ public TStartupUserItem.Destroy

  General destructor for destroying a TStartupUserItem instance. }

destructor TStartupUserItem.Destroy;
begin
  FLnkFile.Free;
  inherited Destroy;
end;

{ private TStartupUserItem.AddCircumflex

  Replaces all backslashes in a path by circumflex. }

function TStartupUserItem.AddCircumflex(const AName: string): string;
begin
  Result := StringReplace(AName, '\', '^', [rfReplaceAll]);
end;

{ protected TStartupUserItem.GetFullLocation

  Returns the file path of a TStartupUserItem. }

function TStartupUserItem.GetFullLocation(): string;
begin
  if FEnabled then
    Result := FLocation
  else
    Result := inherited GetFullLocation();
end;

{ public TStartupUserItem.ChangeFilePath

  Changes the file path of a TStartupUserItem. }

function TStartupUserItem.ChangeFilePath(const ANewFileName: string): Boolean;
var
  NewFilePath, Arguments: string;

begin
  if not FEnabled then
    inherited ChangeFilePath(ANewFileName);

  NewFilePath := DeleteQuoteChars(ExtractPathToFile(ANewFileName));
  Arguments := DeleteQuoteChars(ExtractArguments(ANewFileName));

  // Failed to create new .lnk file?
  if (FEnabled and not FLnkFile.WriteLnkFile(FLocation, NewFilePath, Arguments)) then
    raise EStartupException.Create('Could not create .lnk file!');

  // Update information
  FileName := ANewFileName;
  FLnkFile.ExeFileName := NewFilePath;
  FLnkFile.Arguments := Arguments;

  // Rewrite backup
  if (not FEnabled and FLnkFile.BackupExists()) then
    if not FLnkFile.CreateBackup() then
      raise EStartupException.Create('Backup could not be created!');

  Result := True;
end;

{ public TStartupUserItem.Delete

  Deletes a TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Delete(): Boolean;
begin
  if FEnabled then
  begin
    // Could not delete .lnk?
    if not FLnkFile.Delete() then
      raise EStartupException.Create('Could not delete .lnk file!');

    Result := True;
  end  //of begin
  else
    Result := DeleteKey('HKLM', KEY_STARTUP_USER_DISABLED, AddCircumflex(FLnkFile.FileName));
end;

{ public TStartupUserItem.Disable

  Disables an TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Disable(): Boolean;
var
  Reg: TRegistry;
  KeyName: string;

begin
  Result := False;

  // Create backup directory if not exist
  if not DirectoryExists(TLnkFile.GetBackupDir()) then
    ForceDirectories(TLnkFile.GetBackupDir());

  // .lnk does not exist
  if not FLnkFile.Exists() then
    raise EStartupException.Create('Lnk file '''+ FLnkFile.FileName +''' does not exist!');

  // Create backup by copying original .lnk
  if not FLnkFile.CreateBackup() then
    raise EStartupException.Create('Could not create backup file!');

  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    KeyName := AddCircumflex(FLocation);

    if not Reg.OpenKey(KEY_STARTUP_USER_DISABLED + KeyName, True) then
      raise EStartupException.Create('Could not create key'''+
        KEY_STARTUP_USER_DISABLED + KeyName +'''!');

    Reg.WriteString('path', FLocation);
    Reg.WriteString('item', ChangeFileExt(ExtractFileName(Name), ''));
    Reg.WriteString('command', FileName);
    Reg.WriteString('backup', FLnkFile.BackupLnk);

    // Special Registry entries only for Windows >= Vista
    if TOSVersion.Check(6) then
    begin
      Reg.WriteString('backupExtension', FLnkFile.BackupExt);
      Reg.WriteString('location', ExtractFileDir(FLocation));
      FTime := WriteTimestamp(Reg);
    end  //of begin
    else
      Reg.WriteString('location', TypeOf);

    // Delete original .lnk
    if not FLnkFile.Delete() then
      raise EStartupException.Create('Could not delete .lnk file '''+ FLnkFile.FileName +'''!');

    // Update information
    FLocation := KEY_STARTUP_USER_DISABLED + KeyName;
    FRootKey := HKEY_LOCAL_MACHINE;
    FEnabled := False;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TStartupUserItem.Enable

  Enables an TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Enable(): Boolean;
begin
  // Backup file exists?
  if FLnkFile.BackupExists() then
  begin
    // Failed to restore backup file?
    if not CopyFile(PChar(FLnkFile.BackupLnk), PChar(FLnkFile.FileName), True) then
      raise EStartupException.Create('Could not restore backup .lnk file!');
  end  //of begin
  else
    begin
      // Failed to create new .lnk file?
      if not FLnkFile.WriteLnkFile(FLnkFile.FileName, GetFileNameOnly(),
        GetArguments()) then
        raise EStartupException.Create('Could not create .lnk file!');
    end;  //of if

  // Do not abort if old key could not be deleted
  if not DeleteKey('HKLM', KEY_STARTUP_USER_DISABLED, AddCircumflex(FLnkFile.FileName), False) then
    raise EStartupException.Create('Could not delete key!');

  // Update information
  FLocation := FLnkFile.FileName;
  FRootKey := 0;
  FEnabled := True;
  Result := True;
end;

{ public TStartupUserItem.ExportItem

  Exports a TStartupUserItem object as .reg or .lnk backup file. }

procedure TStartupUserItem.ExportItem(const AFileName: string);
begin
  if not FEnabled then
    inherited ExportItem(AFileName)
  else
    if not FLnkFile.CreateBackup() then
      raise EStartupException.Create('Could not create backup file!');
end;


{ TStartupList }

{ public TStartupList.Create

  General constructor for creating a TStartupList instance. }

constructor TStartupList.Create;
begin
  inherited Create;
  FDeleteBackup := True;
end;

{ private TStartupList.DeleteBackupFile

  Deletes the backup file of a TStartupUserItem. }

function TStartupList.DeleteBackupFile(): Boolean;
begin
  Result := False;

  if (FDeleteBackup and (Selected is TStartupUserItem)) then
    Result := (Selected as TStartupUserItem).LnkFile.DeleteBackup();
end;

{ private TStartupList.ItemAt

  Returns a TStartupListItem object at index. }

function TStartupList.ItemAt(AIndex: Word): TStartupListItem;
begin
  Result := TStartupListItem(RootItemAt(AIndex));
end;

{ private TStartupList.GetSelectedItem

  Returns the current selected item as TStartupListItem. }

function TStartupList.GetSelectedItem(): TStartupListItem;
begin
  Result := TStartupListItem(Selected);
end;

{ private TStartupList.GetStartupUserType

  Returns the startup item type. }

function TStartupList.GetStartupUserType(AReg: TRegistry): string;
var
  StartupType: string;

begin
  // Windows >= Vista?
  if TOSVersion.Check(6) then
  begin
    StartupType := AReg.ReadString('backupExtension');

    if AnsiSameText(StartupType, EXT_COMMON) then
      Result := TYPE_COMMON
    else
      Result := TYPE_USER;
  end  //of begin
  else
    Result := AReg.ReadString('location');
end;

{ private TStartupList.GetStartupUserType

  Returns the startup item type. }

function TStartupList.GetStartupUserType(AAllUsers: Boolean): string;
begin
  // Windows >= Vista?
  if TOSVersion.Check(6) then
  begin
    if AAllUsers then
      Result := TYPE_COMMON
    else
      Result := TYPE_USER;
  end  //of begin
  else
    if AAllUsers then
      Result := TYPE_COMMON_XP
    else
      Result := TYPE_USER_XP;
end;

{ protected TStartupList.AddItemDisabled

  Adds a disabled default startup item to the list. }

function TStartupList.AddItemDisabled(AReg: TRegistry; AWow64: Boolean): Integer;
var
  Item: TStartupListItem;

begin
  Result := -1;
  Item := TStartupItem.Create(Count, False, AWow64);

  try
    with Item do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      FLocation := AReg.CurrentPath;
      Name := ExtractFileName(FLocation);
      FileName := AReg.ReadString('command');
      Time := GetTimestamp(AReg);

      // RunOnce item?
      if (ExtractFileName(AReg.ReadString('key')) = 'RunOnce') then
        TypeOf := 'RunOnce'
      else
        TypeOf := AReg.ReadString('hkey');

      if AWow64 then
        TypeOf := TypeOf +'32'; 
    end;  //of with

    Result := inherited Add(Item);

  except
    Item.Free;
  end;  //of try
end;

{ protected TStartupList.AddItemEnabled

  Adds a enabled default startup item to the list. }

function TStartupList.AddItemEnabled(AHKey: HKEY; AKeyPath, AName,
  AFileName: string; AWow64: Boolean): Integer;
var
  Item: TStartupListItem;
  HKey: string;

begin
  Result := -1;
  Item := TStartupItem.Create(Count, True, AWow64);

  try
    case AHKey of
      HKEY_LOCAL_MACHINE: HKey := 'HKLM';
      HKEY_CURRENT_USER:  HKey := 'HKCU';
      else
        raise EStartupException.Create('Invalid startup key!');
    end;  //of case

    with Item do
    begin
      RootKey := AHKey;
      FLocation := AKeyPath;
      Name := AName;
      FileName := AFileName;
      Time := '';

      // RunOnce item?
      if (ExtractFileName(AKeyPath) = 'RunOnce') then
        TypeOf := 'RunOnce'
      else
        TypeOf := HKey;

      if AWow64 then
        TypeOf := TypeOf +'32';
    end;  //of with

    Inc(FActCount);
    Result := inherited Add(Item);

  except
    Item.Free;
  end;  //of try
end;

{ protected TStartupList.AddNewStartupUserItem

  Adds a new startup user item to the autostart. }

function TStartupList.AddNewStartupUserItem(AName, AFilePath: string;
  AArguments: string = ''; AAllUsers: Boolean = False): Boolean;
var
  LnkFile: TLnkFile;

begin
  if (ExtractFileExt(AName) <> '.lnk') then
    AName := AName +'.lnk';

  // Init .lnk file
  LnkFile := TLnkFile.Create(AName, AAllUsers);

  // Link file created successfully?
  if not LnkFile.WriteLnkFile(LnkFile.FileName, AFilePath, AArguments) then
    raise EStartupException.Create('Could not create .lnk file '''+ LnkFile.FileName +'''!');

  // Add item to list
  Result := (AddUserItemEnabled(LnkFile, AAllUsers) <> -1);
end;

{ protected TStartupList.AddUserItemDisabled

  Adds a disabled startup user item to the list. }

function TStartupList.AddUserItemDisabled(AReg: TRegistry): Integer;
var
  Item: TStartupListItem;
  Path, Ext: string;

begin
  Item := TStartupUserItem.Create(Count, False, False);

  try
    with (Item as TStartupUserItem) do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      FLocation := AReg.CurrentPath;
      Name := ExtractFileName(StringReplace(FLocation, '^', '\', [rfReplaceAll]));
      FileName := AReg.ReadString('command');
      Time := GetTimestamp(AReg);
      TypeOf := GetStartupUserType(AReg);
      Path := AReg.ReadString('path');

      if ((TypeOf = TYPE_USER) or (TypeOf = TYPE_USER_XP)) then
        Ext := EXT_USER
      else
        Ext := EXT_COMMON;

      // Setup .lnk
      LnkFile := TLnkFile.Create(Path, Ext);
      LnkFile.ExeFileName := ExtractPathToFile(FileName);
      LnkFile.Arguments := ExtractArguments(FileName);
    end;  //of with

    Result := inherited Add(Item);

  except
    Item.Free;
    Result := -1;
  end;  //of try
end;

{ protected TStartupList.AddUserItemEnabled

  Adds a enabled startup user item to the list. }

function TStartupList.AddUserItemEnabled(ALnkFile: TLnkFile;
  AAllUsers: Boolean): Integer;
var
  Item: TStartupListItem;

begin
  Item := TStartupUserItem.Create(Count, True, False);

  try
    // Read .lnk file
    ALnkFile.ReadLnkFile();

    with (Item as TStartupUserItem) do
    begin
      RootKey := 0;
      FLocation := ALnkFile.FileName;
      FileName := ALnkFile.FullPath;
      LnkFile := ALnkFile;
      Name := ExtractFileName(ALnkFile.FileName);
      Time := '';
      TypeOf := GetStartupUserType(AAllUsers);
    end;  //of with

    Inc(FActCount);
    Result := inherited Add(Item);

  except
    Item.Free;
    Result := -1;
  end;  //of try
end;

{ public TStartupList.Add

  Adds a new startup item to autostart. }

function TStartupList.Add(AFileName, AArguments, ACaption: string): Boolean;
var
  Name, Ext, FullPath: string;
  i: Word;
  Reg: TRegistry;

begin
  Result := False;
  Name := ExtractFileName(AFileName);
  Ext := ExtractFileExt(Name);

  // Check invalid extension
  if ((Ext <> '.exe') and (Ext <> '.bat')) then
    raise EArgumentException.Create('Invalid program extension! Must be ''.exe'''
      +' or ''.bat''!');

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    // File path already exists in another item?
    for i := 0 to Count - 1 do
      if AnsiContainsStr(ItemAt(i).FileName, AFileName) then
        Exit;

    // Add new startup user item?
    if (Ext = '.exe') then
    begin
      if (ACaption <> '') then
        Name := ACaption
      else
        Name := ChangeFileExt(Name, '');

      Result := AddNewStartupUserItem(Name, AFileName, AArguments);
    end  //of begin
    else
    begin
      Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

      // Try to add new startup item to Registry
      try
        Reg.RootKey := HKEY_CURRENT_USER;
        Reg.OpenKey(KEY_STARTUP_RUN, True);

        // Escape space char using quotes
        FullPath := '"'+ AFileName +'"';

        // Append arguments if used
        if (AArguments <> '') then
          FullPath := FullPath +' '+ AArguments;

        Reg.WriteString(ACaption, FullPath);

        // Adds item to list
        Result := (AddItemEnabled(HKEY_CURRENT_USER, KEY_STARTUP_RUN, ACaption,
          FullPath, False) <> -1);

      finally
        Reg.CloseKey();
        Reg.Free;
      end;  //of try
    end;  //of begin

  finally
    FLock.Release();
  end;  //of try
end;

{ public TStartupList.BackupExists

  Checks if a backup file already exists. }

function TStartupList.BackupExists(): Boolean;
begin
  if (not Assigned(Selected) or (IndexOf(Selected) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if (Selected is TStartupUserItem) then
    Result := (Selected as TStartupUserItem).LnkFile.BackupExists()
  else
    Result := False;
end;

{ public TStartupList.ChangeItemStatus

  Changes the item status. }

function TStartupList.ChangeItemStatus(): Boolean;
begin
  Result := inherited ChangeItemStatus();

  // Only delete backup if item has been enabled!
  if (Result and Selected.Enabled) then
    DeleteBackupFile();
end;

{ public TStartupList.DeleteItem

  Deletes an item from Registry and list. }

function TStartupList.DeleteItem(): Boolean;
begin
  DeleteBackupFile();
  Result := inherited DeleteItem();
end;

{ public TStartupList.EnableItem

  Enables the current selected item. }

function TStartupList.EnableItem(): Boolean;
begin
  Result := inherited EnableItem();

  if Result then
    DeleteBackupFile();
end;

{ public TStartupList.ExportList

  Exports the complete list as .reg file. }

procedure TStartupList.ExportList(const AFileName: string);
var
  i: Integer;
  RegFile: TRegistryFile;
  Item: TStartupListItem;

begin
  FLock.Acquire;

  // Init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    for i := 0 to Count - 1 do
    begin
      Item := ItemAt(i);

      // Skip enabled startup user items (not in Registry)!
      if ((Item is TStartupUserItem) and Item.Enabled) then
        Continue;

      RegFile.ExportKey(Item.RootKey, Item.GetWow64Key(), True);
    end;  //of for

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
    FLock.Release();
  end;  //of try
end;

{ public TStartupList.ImportBackup

  Imports a startup user backup file and adds it to the list. }

function TStartupList.ImportBackup(const AFileName: string): Boolean;
var
  Name, Ext: string;
  i: Integer;
  LnkFile: TLnkFile;

begin
  Result := False;
  Ext := ExtractFileExt(AFileName);

  // Check invalid extension
  if ((Ext <> EXT_COMMON) and (Ext <> EXT_USER)) then
    raise EArgumentException.Create('Invalid backup file extension! Must be '''
      + EXT_COMMON +''' or '''+ EXT_USER +'''!');

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  // Init new .lnk file
  LnkFile := TLnkFile.Create(AFileName, Ext);

  // Set the name of item
  Name := ExtractFileName(ChangeFileExt(AFileName, ''));

  // Append extension if not exist
  if (ExtractFileExt(Name) = '') then
    Name := Name +'.lnk';

  try
    // Extract path to .exe
    if not LnkFile.ReadLnkFile() then
      raise EStartupException.Create('Could not read backup file!');

    // File path already exists in another item?
    for i := 0 to Count - 1 do
      if (ItemAt(i).FileName = LnkFile.ExeFileName) then
        Exit;

    // Create .lnk file and add it to list
    Result := AddNewStartupUserItem(Name, LnkFile.ExeFileName, LnkFile.Arguments,
      (Ext = EXT_COMMON));

  finally
    LnkFile.Free;
    FLock.Release();
  end;  //of try
end;

{ public TStartupList.Load

  Uses default settings to load items into list. }

procedure TStartupList.Load();
begin
  LoadStartup(False);
end;

{ public TStartupList.LoadDisabled

  Searches for disabled items in AKeyPath and adds them to the list. }

procedure TStartupList.LoadDisabled(AStartupUser: Boolean; AWow64: Boolean = False);
var
  Reg: TRegistry;
  Items: TStringList;
  KeyPath: string;
  i: Integer;
  Wow64: Boolean;

begin
  Items := TStringList.Create;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);
  Reg.RootKey := HKEY_LOCAL_MACHINE;

  if AStartupUser then
    KeyPath := KEY_STARTUP_USER_DISABLED
  else
    KeyPath := KEY_STARTUP_DISABLED;

  try
    Reg.OpenKey(KeyPath, False);
    Reg.GetKeyNames(Items);

    for i := 0 to Items.Count - 1 do
    begin
      Reg.CloseKey();
      Reg.OpenKey(KeyPath + Items[i], False);

      if not AStartupUser then
      begin
        if AWow64 then
          Wow64 := AnsiContainsText(Reg.ReadString('key'), 'Wow6432Node')
        else
          Wow64 := False;

        AddItemDisabled(Reg, Wow64);
      end  //of begin
      else
        AddUserItemDisabled(Reg);
    end;  //of for

  finally
    Reg.CloseKey();
    Reg.Free;
    Items.Free;
  end;  //of try
end;

{ public TStartupList.LoadEnabled

  Searches for enabled startup user items and adds them to the list. }

procedure TStartupList.LoadEnabled(AAllUsers: Boolean);
var
  LnkFiles: TStringList;
  SearchResult: TSearchRec;
  Folder: string;
  i: Integer;
  LnkFile: TLnkFile;

begin
  LnkFiles := TStringList.Create;

  // Retrieve a list containing all activated startup user .lnk files
  try
    Folder := TLnkFile.GetStartUpDir(AAllUsers);

    if (FindFirst(Folder +'*.lnk', faAnyFile, SearchResult) = 0) then
      try
        repeat
          // .lnk file found?
          if (SearchResult.Attr <> faDirectory) then
            LnkFiles.Add(Folder + SearchResult.Name);

        until FindNext(SearchResult) <> 0;

      finally
        FindClose(SearchResult);
      end;  //of try

    // Add every file to list
    for i := 0 to LnkFiles.Count - 1 do
    begin
      LnkFile := TLnkFile.Create(ExtractFileName(LnkFiles[i]), AAllUsers);
      AddUserItemEnabled(LnkFile, AAllUsers);
    end;  //of begin

  finally
    LnkFiles.Free;
  end;  //of try
end;

{ public TStartupList.LoadEnabled

  Searches for enabled items in ARootKey and AKeyPath and adds them to the list. }

procedure TStartupList.LoadEnabled(AHKey: HKEY; ARunOnce: Boolean = False;
  AWow64: Boolean = False);
var
  Reg: TRegistry;
  Items: TStringList;
  i: Integer;
  KeyPath: string;

begin
  Items := TStringList.Create;

  // Allow WOW64 redirection?
  if AWow64 then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  // Set startup location
  if ARunOnce then
    KeyPath := KEY_STARTUP_RUNONCE
  else
    KeyPath := KEY_STARTUP_RUN;

  try
    Reg.RootKey := AHKey;
    Reg.OpenKey(KeyPath, False);
    Reg.GetValueNames(Items);

    for i := 0 to Items.Count - 1 do
      // Read path to .exe and add item to list
      AddItemEnabled(AHKey, KeyPath, Items[i], Reg.ReadString(Items[i]), AWow64);

  finally
    Reg.CloseKey();
    Reg.Free;
    Items.Free;
  end;  //of finally
end;

{ public TStartupList.LoadStartup

  Searches for startup items at different locations. }

procedure TStartupList.LoadStartup(AIncludeRunOnce: Boolean);
var
  StartupSearchThread: TStartupSearchThread;

begin
  // Init search thread
  StartupSearchThread := TStartupSearchThread.Create(Self, FLock);

  with StartupSearchThread do
  begin
    Win64 := (TOSVersion.Architecture = arIntelX64);
    IncludeRunOnce := AIncludeRunOnce;
    OnError := OnSearchError;
    OnFinish := OnSearchFinish;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    Start;
  end;  // of with
end;

{ public TStartupListItem.OpenInRegEdit

  Opens a TStartupListItem object in RegEdit. }

procedure TStartupListItem.OpenInRegEdit();
begin
  // Disabled items are in 64-Bit Registry!
  if not Enabled then
    OpenInRegEdit(False)
  else
    inherited OpenInRegEdit();
end;


{ TContextListItem }

{ protected TContextListItem.GetFullLocation

  Returns the Registry path to a TContextListItem. }

function TContextListItem.GetFullLocation(): string;
begin
  Result := HKeyToStr(HKEY_CLASSES_ROOT) +'\'+ GetKeyPath();
end;

{ protected TContextListItem.GetRootKey

  Returns the HKEY of an TContextListItem. }

function TContextListItem.GetRootKey(): HKEY;
begin
  Result := HKEY_CLASSES_ROOT;
end;

{ public TContextListItem.Delete

  Deletes a TContextListItem object and returns True if successful. }

function TContextListItem.Delete(): Boolean;
begin
  if not DeleteKey('HKCR', ExtractFileDir(GetKeyPath()), Name) then
    raise EContextMenuException.Create('Could not delete key!');

  Result := True;
end;

{ public TContextListItem.DeleteUserChoice

  Deletes a user defined program association and returns True if successful. }

function TContextListItem.DeleteUserChoice(AFileExtension: string): Boolean;
begin
  Result := (RegDeleteKey(HKEY_CURRENT_USER, PChar(Format(KEY_USERCHOICE,
    [AFileExtension]))) = ERROR_SUCCESS);
end;

{ public TContextListItem.UserChoiceExists

  Returns True if a user defined program association exists. }

function TContextListItem.UserChoiceExists(AFileExtension: string): Boolean;
var
  UserChoice: string;

begin
  UserChoice := GetRegStringValue(Format(KEY_USERCHOICE, [AFileExtension]),
    'ProgID', HKEY_CURRENT_USER);
  Result := AnsiStartsText('Applications\', UserChoice);
end;


{ TShellItem }

{ private TShellItem.GetKeyPath

  Returns the Registry path of a TShellItem item. }

function TShellItem.GetKeyPath(): string;
begin
  Result := FLocation +'\'+ CM_SHELL +'\'+ Name;
end;

{ public TShellItem.ChangeFilePath

  Changes the file path of an TShellItem item. }

function TShellItem.ChangeFilePath(const ANewFileName: string): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetKeyPath() +'\command', False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Change path
    case Reg.GetDataType('') of
      rdExpandString: Reg.WriteExpandString('', ANewFileName);
      rdString:       Reg.WriteString('', ANewFileName);
      else
                      raise EContextMenuException.Create('Invalid data type!');
    end;  //of case

    FileName := ANewFileName;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TShellItem.Disable

  Disables a TShellItem object and returns True if successful. }

function TShellItem.Disable(): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(GetKeyPath(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    Reg.WriteString(CM_SHELL_DISABLED, '');

    // Update status
    FEnabled := False;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TShellItem.Enable

  Enables a TShellItem object and returns True if successful. }

function TShellItem.Enable(): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key invalid?
    if not Reg.OpenKey(GetKeyPath(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Delete disable value, but do not fail if value does not exist!
    if (Reg.ValueExists(CM_SHELL_DISABLED) and not Reg.DeleteValue(CM_SHELL_DISABLED)) then
      raise EStartupException.Create('Could not delete value!');

    // Update status
    FEnabled := True;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TShellItem.ExportItem

  Exports a TShellItem object as .reg file. }

procedure TShellItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    RegFile.ExportReg(HKEY_CLASSES_ROOT, GetKeyPath(), True);

  finally
    RegFile.Free;
  end;  //of try
end;


{ TShellExItem }

{ private TShellExItem.ChangeStatus

  Enables or disables a ShellEx item. }

function TShellExItem.ChangeStatus(ANewStatus: Boolean): Boolean;
var
  Reg: TRegistry;
  OldValue, NewValue: string;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(GetKeyPath(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Value does not exist?
    if not Reg.ValueExists('') then
      raise EContextMenuException.Create('Value does not exist!');

    OldValue := Reg.ReadString('');

    if (Trim(OldValue) = '') then
      raise EContextMenuException.Create('Value must not be empty!');

    // Item disabled?
    if (ANewStatus and (OldValue[1] <> '{')) then
    begin
      NewValue := Copy(OldValue, 2, Length(OldValue));
      Reg.WriteString('', NewValue);
    end  //of begin
    else
      // Item enabled?
      if (OldValue[1] = '{') then
      begin
        NewValue := '-'+ OldValue;
        Reg.WriteString('', NewValue);
      end;  //of if

    // Update status
    FEnabled := ANewStatus;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ private TShellExItem.GetKeyPath

  Returns the Registry path to a TShellExItem. }

function TShellExItem.GetKeyPath(): string;
begin
  Result := FLocation +'\'+ CM_SHELLEX_HANDLERS +'\'+ Name;
end;

{ public TShellExItem.ChangeFilePath

  Changes the file path of an TShellExItem item. }

function TShellExItem.ChangeFilePath(const ANewFileName: string): Boolean;
var
  Reg: TRegistry;
  ProgramKeyPath: string;

begin
  Result := False;

  if FWow64 then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetKeyPath(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Read GUID and setup key of program
    ProgramKeyPath := Format(CM_SHELLEX_FILE, [Reg.ReadString('')]);
    Reg.CloseKey;

    // Invalid program key?
    if not Reg.OpenKey(ProgramKeyPath, False) then
      raise EContextMenuException.Create('Program key does not exist!');

    // Change path
    case Reg.GetDataType('') of
      rdExpandString: Reg.WriteExpandString('', ANewFileName);
      rdString:       Reg.WriteString('', ANewFileName);
      else
                      raise EContextMenuException.Create('Invalid data type!');
    end;  //of case

    // Update path
    FileName := ANewFileName;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TShellExItem.Disable

  Disables a TShellExItem object and returns True if successful. }

function TShellExItem.Disable(): Boolean;
begin
  Result := ChangeStatus(False);
end;

{ public TShellExItem.Enable

  Enables a TShellExItem object and returns True if successful. }

function TShellExItem.Enable(): Boolean;
begin
  Result := ChangeStatus(True);
end;

{ public TShellExItem.ExportItem

  Exports a TShellExItem object as .reg file. }

procedure TShellExItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;
  Reg: TRegistry;
  Key: string;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  if FWow64 then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if Reg.OpenKey(GetKeyPath(), False) then
    begin
      Key := 'CLSID\'+ Reg.ReadString('');
      Reg.CloseKey();

      // Key exists?
      if Reg.OpenKey(Key, False) then
        RegFile.ExportKey(HKEY_CLASSES_ROOT, Key, True);
    end;  //of begin

    RegFile.ExportKey(HKEY_CLASSES_ROOT, GetKeyPath(), True);
    RegFile.Save();

  finally
    Reg.CloseKey();
    Reg.Free;
    RegFile.Free;
  end;  //of try
end;


{ TShellNewItem }

{ private TShellNewItem.ChangeStatus

  Enables or disables a ShellNew item. }

function TShellNewItem.ChangeStatus(ANewStatus: Boolean): Boolean;
var
  Reg: TRegistry;
  OldKeyName, NewKeyName: string;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(FLocation, False) then
      raise EContextMenuException.Create('Key '''+ FLocation +''' does not exist!');

    if ANewStatus then
    begin
      OldKeyName := CM_SHELLNEW_DISABLED;
      NewKeyName := CM_SHELLNEW;
    end  //of begin
    else
    begin
      OldKeyName := CM_SHELLNEW;
      NewKeyName := CM_SHELLNEW_DISABLED;
    end;  //of if

    // Old key does not exist?
    if not Reg.KeyExists(OldKeyName) then
      raise EContextMenuException.Create('Key '''+ OldKeyName +''' does not exist!');

    // New key already exists?
    if Reg.KeyExists(NewKeyName) then
      raise EContextMenuException.Create('Key '''+ NewKeyName +''' already exists!');

    // Rename key
    Reg.MoveKey(OldKeyName, NewKeyName, True);

    // Update status
    FEnabled := ANewStatus;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ private TShellNewItem.GetKeyPath

  Returns the Registry path to a TShellExItem. }

function TShellNewItem.GetKeyPath(): string;
begin
  if FEnabled then
    Result := FLocation +'\'+ CM_SHELLNEW
  else
    Result := FLocation +'\'+ CM_SHELLNEW_DISABLED;
end;

{ public TShellNewItem.ChangeFilePath

  Changes the file path of an TShellExItem item. }

function TShellNewItem.ChangeFilePath(const ANewFileName: string): Boolean;
begin
  // Impossible!
  Result := False;
end;

{ public TShellNewItem.Delete

  Deletes a TShellNewItem object and returns True if successful. }

function TShellNewItem.Delete(): Boolean;
begin
  if not DeleteKey('HKCR', ExtractFileDir(GetKeyPath()), ExtractFileName(GetKeyPath())) then
    raise EContextMenuException.Create('Could not delete key!');

  Result := True;
end;

{ public TShellNewItem.Disable

  Disables a TShellExItem object and returns True if successful. }

function TShellNewItem.Disable(): Boolean;
begin
  Result := ChangeStatus(False);
end;

{ public TShellNewItem.Enable

  Enables a TShellExItem object and returns True if successful. }

function TShellNewItem.Enable(): Boolean;
begin
  Result := ChangeStatus(True);
end;

{ public TShellNewItem.ExportItem

  Exports a TShellNewItem object as .reg file. }

procedure TShellNewItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    RegFile.ExportReg(HKEY_CLASSES_ROOT, GetKeyPath(), True);

  finally
    RegFile.Free;
  end;  //of try
end;


{ TContextList }

{ public TContextList.Create

  General constructor for creating a TContextList instance. }

constructor TContextList.Create;
begin
  inherited Create;
  FActCount := 0;
end;

{ private TContextList.GetSelectedItem

  Returns the current selected item as TContextListItem. }

function TContextList.GetSelectedItem(): TContextListItem;
begin
  Result := TContextListItem(Selected);
end;

{ private TContextList.ItemAt

  Returns a TContextListItem object at index. }

function TContextList.ItemAt(AIndex: Word): TContextListItem;
begin
  Result := TContextListItem(RootItemAt(AIndex));
end;

{ protected TContextList.AddShellItem

  Adds a shell item to list. }

function TContextList.AddShellItem(const AName, ALocationRoot, AFileName,
  ACaption: string; AEnabled, AWow64: Boolean): Integer;
var
  Item: TContextListItem;

begin
  Item := TShellItem.Create(Count, AEnabled, AWow64);

  with Item do
  begin
    Name := AName;
    LocationRoot := ALocationRoot;
    FileName := AFileName;
    Caption := ACaption;
    TypeOf := CM_SHELL;

    if AEnabled then
      Inc(FActCount);
  end;  //of with

  Result := inherited Add(Item);
end;

{ protected TContextList.AddShellExItem

  Adds a shellex item to list. }

function TContextList.AddShellExItem(const AName, ALocationRoot, AFileName: string;
  AEnabled, AWow64: Boolean): Integer;
var
  Item: TContextListItem;

begin
  Item := TShellExItem.Create(Count, AEnabled, AWow64);

  with Item do
  begin
    Name := AName;
    LocationRoot := ALocationRoot;
    FileName := AFileName;
    TypeOf := CM_SHELLEX;

    if AEnabled then
      Inc(FActCount);
  end;  //of with

  Result := inherited Add(Item);
end;

{ protected TContextList.AddShellItem

  Adds a shell item to list. }

function TContextList.AddShellNewItem(const AName, ALocationRoot, ACaption: string;
  AEnabled, AWow64: Boolean): Integer;
var
  Item: TContextListItem;

begin
  Item := TShellNewItem.Create(Count, AEnabled, AWow64);

  with Item do
  begin
    Name := AName;
    LocationRoot := ALocationRoot;
    Caption := ACaption;
    TypeOf := CM_SHELLNEW;

    if AEnabled then
      Inc(FActCount);
  end;  //of with

  Result := inherited Add(Item);
end;

{ public TContextList.Add

  Adds a new contextmenu entry. }

function TContextList.Add(AFileName, AArguments, ALocationRoot, ACaption: string;
  AExtended: Boolean = False): Boolean;
var
  Name, Ext, FullPath, LocationRoot, FileType, KeyPath, UserChoice: string;
  Reg: TRegistry;

begin
  Result := False;
  LocationRoot := Trim(ALocationRoot);

  // Valid location?
  if ((LocationRoot <> '*') and (Length(LocationRoot) <= 2)) then
    raise EArgumentException.Create('Invalid location: Expected at least two characters or ''*''!');

  Ext := ExtractFileExt(AFileName);
  Name := ChangeFileExt(ExtractFileName(AFileName), '');

  // Check invalid extension
  if ((Ext <> '.exe') and (Ext <> '.bat')) then
    raise EArgumentException.Create('Invalid program extension! Must be ''.exe'''
      +' or ''.bat''!');

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    // File path already exists in another item?
    if (IndexOf(Name, LocationRoot) <> -1) then
      Exit;

    // Escape space char using quotes
    FullPath := '"'+ AFileName +'"';

    // Append arguments if used
    if (AArguments <> '') then
      FullPath := FullPath +' '+ AArguments;

    // Init Registry access
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

    try
      Reg.RootKey := HKEY_CLASSES_ROOT;

      if not Reg.OpenKey(LocationRoot, True) then
        raise EContextMenuException.Create('Could not create key: '''+ LocationRoot +'''!');

      // Location is a file extension?
      if (LocationRoot[1] = '.') then
      begin
        // Read default associated file type
        FileType := Reg.ReadString('');

        // Associated file type not found?
        if (FileType = '') then
        begin
          // Add new file association
          FileType := Copy(LocationRoot, 2, Length(LocationRoot)) +'file';
          Reg.WriteString('', FileType);
        end;  //of begin
      end  //of begin
      else
        FileType := LocationRoot;

      Reg.CloseKey();
      KeyPath := FileType +'\'+ CM_SHELL +'\'+ Name;

      // Adds new context item to Registry
      if not Reg.OpenKey(KeyPath, True) then
        raise EContextMenuException.Create('Could not open key: '''+ KeyPath +'''!');

      // Set caption of item
      if (Trim(ACaption) <> '') then
        Reg.WriteString('', ACaption);

      // Extended: Item is only visible with shift + right click
      if AExtended then
        Reg.WriteString('Extended', '');

      Reg.CloseKey();
      KeyPath := KeyPath +'\command';

      if not Reg.OpenKey(KeyPath, True) then
        raise EContextMenuException.Create('Could not create key: '''+ KeyPath +'''!');

      // Write command of item
      Reg.WriteString('', FullPath);

      // Adds item to list
      AddShellItem(Name, FileType, FullPath, ACaption, True, False);
      Result := True;

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try

  finally
    FLock.Release();
  end;  //of try
end;

{ public TContextList.ExportList

  Exports the complete list as .reg file. }

procedure TContextList.ExportList(const AFileName: string);
var
  i: Word;
  RegFile: TRegistryFile;
  Item: TContextListItem;

begin
  FLock.Acquire;

  // Init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    for i := 0 to Count - 1 do
    begin
      Item := ItemAt(i);
      RegFile.ExportKey(HKEY_CLASSES_ROOT, Item.Location, True);
    end;  //of for

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
    FLock.Release();
  end;  //of try
end;

{ public TContextList.IndexOf

  Returns the index of an item checking name and location. }

function TContextList.IndexOf(AName, ALocationRoot: string): Integer;
var
  i: Word;
  Item: TContextListItem;

begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    Item := ItemAt(i);

    if (((Item.Name = AName) or (Item.Caption = AName)) and
      (Item.LocationRoot = ALocationRoot)) then
    begin
      Result := i;
      Break;
    end;  //of begin
  end;  //of for
end;

{ public TContextList.Load

  Uses default settings to load items into list. }

procedure TContextList.Load();
begin
  LoadContextMenus(CM_LOCATIONS_DEFAULT);
end;

{ public TContextList.LoadContextmenu

  Searches for Shell and ShellEx context menu entries in specific Registry key
  and adds them to the list. }

procedure TContextList.LoadContextmenu(const ALocationRoot: string;
  AWow64: Boolean);
begin
  LoadContextmenu(ALocationRoot, stShell, AWow64);
  LoadContextmenu(ALocationRoot, stShellEx, AWow64);
  LoadContextmenu(ALocationRoot, stShellNew, AWow64);
end;

{ public TContextList.LoadContextmenu

  Searches for context menu items in Registry key and adds them to the list. }

procedure TContextList.LoadContextmenu(const ALocationRoot: string;
  AShellItemType: TShellItemType; AWow64: Boolean);
var
  Reg: TRegistry;
  i, DelimiterPos: Integer;
  List: TStringList;
  Item, Key, FilePath, GuID, Caption: string;
  Enabled, Wow64: Boolean;
  Access64: Cardinal;

begin
  Access64 := KEY_WOW64_64KEY or KEY_READ;
  Reg := TRegistry.Create(Access64);
  List := TStringList.Create;

  case AShellItemType of
    stShell:    Key := ALocationRoot +'\'+ CM_SHELL;
    stShellEx:  Key := ALocationRoot +'\'+ CM_SHELLEX_HANDLERS;
    stShellNew: Key := ALocationRoot;
  end;  //of case

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Skip invalid key
    if not Reg.OpenKey(Key, False) then
      Exit;

    // Add ShellNew item?
    if (AShellItemType = stShellNew) then
    begin
      // TODO: "ShellNew" and "_ShellNew" in same key are possible!
      if Reg.KeyExists(CM_SHELLNEW) then
        Enabled := True
      else
        if Reg.KeyExists(CM_SHELLNEW_DISABLED) then
          Enabled := False
        else
          Exit;

      DelimiterPos := AnsiPos(PathDelim, ALocationRoot);

      // ShellNew item inside a subkey?
      if (DelimiterPos > 0) then
      begin
        Reg.CloseKey;
        Reg.OpenKey(Copy(ALocationRoot, 1, DelimiterPos - 1), False);
      end;  //of begin

      // Get associated key
      Key := Reg.ReadString('');
      Reg.CloseKey;

      // Open associated key failed?
      if not Reg.OpenKey(Key, False) then
        Exit;

      // Get description of file extension
      Caption := Reg.ReadString('');

      // Caption can be empty!
      // Fallback 1: associated key
      if (Caption = '') then
        Caption := ExtractFileName(Key);

      // Fallback 2: subkey
      if (Caption = '') then
        Caption := ExtractFileName(ALocationRoot);

      // Add item to list
      AddShellNewItem(Key, ALocationRoot, Caption, Enabled, False);
      Exit;
    end;  //of if

    // Read out all keys
    Reg.GetKeyNames(List);

    for i := 0 to List.Count - 1 do
    begin
      Reg.CloseKey();
      Item := List[i];
      Reg.OpenKey(Key +'\'+ Item, False);
      FilePath := '';

      // Filter items with GUID in name
      if (Item[1] = '{') then
        Continue;

      // Search for shell entries?
      if (AShellItemType = stShell) then
      begin
        Caption := Reg.ReadString('');

        // Filter unreadable Shell items
        if ((Caption <> '') and (Caption[1] = '@')) then
          Continue;

        // Get status and caption of Shell item
        Enabled := not Reg.ValueExists(CM_SHELL_DISABLED);

        // Filter Shell items without command
        if not Reg.OpenKey('command', False) then
          Continue;

        // Filter important Shell items
        if Reg.ValueExists('DelegateExecute') then
          Continue;

        // Get file path of command
        if not (Reg.GetDataType('') in [rdString, rdExpandString]) then
          Continue;

        FilePath := Reg.ReadString('');

        // Add item to list
        AddShellItem(Item, ALocationRoot, FilePath, Caption, Enabled, False);
      end  //of begin
      else
        if (AShellItemType = stShellEx) then
        begin
          GuID := Reg.ReadString('');

          // Filter empty and unreadable ShellEx items
          if ((GuID = '') or (GuID[1] = '@')) then
            Continue;

          // Get status and GUID of ShellEx item
          Enabled := (GuID[1] = '{');

          // Disabled ShellEx items got "-" before GUID!
          if not Enabled then
            GUID := Copy(GuID, 2, Length(GUID));

          Reg.CloseKey();
          Wow64 := False;

          // Get file path of command
          if Reg.OpenKey(Format(CM_SHELLEX_FILE, [GuID]), False) then
          begin
            if not (Reg.GetDataType('') in [rdString, rdExpandString]) then
              Continue;

            FilePath := Reg.ReadString('');
          end  //of begin
          else
            if AWow64 then
            begin
              Reg.Access := KEY_READ;
              Wow64 := True;

              if Reg.OpenKey(Format(CM_SHELLEX_FILE, [GuID]), False) then
              begin
                if not (Reg.GetDataType('') in [rdString, rdExpandString]) then
                  Continue;

                FilePath := Reg.ReadString('');
              end;  //of begin

              Reg.Access := Access64;
            end;  //of if

          // Add item to list
          AddShellExItem(Item, ALocationRoot, FilePath, Enabled, Wow64);
        end;  //of begin
    end;  //of for

  finally
    List.Free;
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TContextList.LoadContextMenus

  Searches for context menu entries at different locations. }

procedure TContextList.LoadContextMenus(ALocationRootCommaList: string = '');
var
  SearchThread: TContextSearchThread;

begin
  // Init search thread
  SearchThread := TContextSearchThread.Create(Self, FLock);

  with SearchThread do
  begin
    Locations.CommaText := ALocationRootCommaList;
    Win64 := (TOSVersion.Architecture = arIntelX64);
    OnError := OnSearchError;
    OnFinish := OnSearchFinish;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    Start;
  end;  // of with
end;


{ TServiceListItem }

{ public TServiceListItem.Create

  Constructor for creating a TServiceListItem instance. }

constructor TServiceListItem.Create(AIndex: Word; AEnabled: Boolean;
  AServiceManager: SC_HANDLE);
begin
  inherited Create(AIndex, AEnabled, False);
  FServiceManager := AServiceManager;
end;

{ private TServiceListItem.GetHandle

  Returns the handle to a service. }

function TServiceListItem.GetHandle(AAccess: DWORD): SC_HANDLE;
var
  Handle: SC_HANDLE;

begin
  if (FServiceManager = 0) then
    raise EServiceException.Create('Service manager not initialized!');
  
  Handle := OpenService(FServiceManager, PChar(Name), AAccess);

  // Error occured?
  if (Handle = 0) then
    raise EServiceException.Create(SysErrorMessage(GetLastError()));

  Result := Handle;
end;

{ private TServiceListItem.GetLocation

  Returns the Registry path to a TServiceListItem. }

function TServiceListItem.GetLocation(): string;
begin
  Result := KEY_SERVICE_ENABLED + Name;
end;

{ protected TServiceListItem.GetFullLocation

  Returns the full Registry path to a TServiceListItem. }

function TServiceListItem.GetFullLocation(): string;
begin
  Result := HKeyToStr(HKEY_LOCAL_MACHINE) +'\'+ GetLocation();
end;

{ protected TServiceListItem.GetRootKey

  Returns the HKEY of an TServiceListItem. }

function TServiceListItem.GetRootKey(): HKEY;
begin
  Result := HKEY_LOCAL_MACHINE;
end;

{ public TServiceListItem.ChangeFilePath

  Changes the file path of an TServiceListItem item. }

function TServiceListItem.ChangeFilePath(const ANewFileName: string): Boolean;
var
  Service: SC_HANDLE;

begin
  Result := False;
  Service := GetHandle(SERVICE_CHANGE_CONFIG);

  try
    // Change path
    if not ChangeServiceConfig(Service, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE,
      SERVICE_NO_CHANGE, PChar(ANewFileName), nil, nil, nil, nil, nil, nil) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    // Update information
    FileName := ANewFileName;
    Result := True;

  finally
    CloseServiceHandle(Service);
  end;  //of try
end;

{ public TServiceListItem.Delete

  Deletes a TServiceListItem object and returns True if successful. }

function TServiceListItem.Delete(): Boolean;
const
  SERVICE_DELETE = $00010000;

var
  Service: SC_HANDLE;
  Reg: TRegistry;

begin
  Result := False;
  Service := GetHandle(SERVICE_DELETE);
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    // Delete service
    if not DeleteService(Service) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    // Delete key or value of disabled service
    if not FEnabled then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey(KEY_SERVICE_DISABLED, False);

      // Windows >= Vista?
      if TOSVersion.Check(6) then
        Result := Reg.DeleteKey(Name)
      else
        Result := Reg.DeleteValue(Name);
    end  //of begin
    else
      Result := True;
    
  finally
    Reg.CloseKey();
    Reg.Free;
    CloseServiceHandle(Service);
  end;  //of try
end;

{ public TServiceListItem.Disable

  Disables an TServiceListItem object and returns True if successful. }

function TServiceListItem.Disable(): Boolean;
var
  Service: SC_HANDLE;
  Reg: TRegistry;

begin
  Result := False;
  Service := GetHandle(SERVICE_CHANGE_CONFIG);
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    // Disable service
    if not ChangeServiceConfig(Service, SERVICE_NO_CHANGE, SERVICE_DISABLED,
      SERVICE_NO_CHANGE, nil, nil, nil, nil, nil, nil, nil) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    // Save disable key ...
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Windows >= Vista?
    if TOSVersion.Check(6) then
    begin
      // Write disable key
      if not Reg.OpenKey(KEY_SERVICE_DISABLED +'\'+ Name, True) then
        raise EServiceException.Create('Could not create disable key!');

      // Save deactivation timestamp
      FTime := WriteTimestamp(Reg);
    end  //of begin
    else
      // Write disable key
      if not Reg.OpenKey(KEY_SERVICE_DISABLED, True) then
        raise EServiceException.Create('Could not create disable key!');

    // Write last status
    Reg.WriteInteger(Name, Ord(Start));

    // Update information
    FEnabled := False;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
    CloseServiceHandle(Service);
  end;  //of try
end;

{ public TServiceListItem.Enable

  Enables an TServiceListItem object and returns True if successful. }

function TServiceListItem.Enable(): Boolean;
var
  Service: SC_HANDLE;
  Reg: TRegistry;

begin
  Result := False;
  Service := GetHandle(SERVICE_CHANGE_CONFIG);
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Windows >= Vista?
    if TOSVersion.Check(6) then
    begin
      if not Reg.OpenKey(KEY_SERVICE_DISABLED +'\'+ Name, False) then
        EServiceException.Create('Disable key does not exist!');
    end  //of begin
    else
      Reg.OpenKey(KEY_SERVICE_DISABLED, True);

    // Last status exists?
    if not Reg.ValueExists(Name) then
      raise EStartupException.Create('Last status does not exist');

    // Enable service
    if not ChangeServiceConfig(Service, SERVICE_NO_CHANGE, Reg.ReadInteger(Name),
      SERVICE_NO_CHANGE, nil, nil, nil, nil, nil, nil, nil) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    // Windows >= Vista?
    if TOSVersion.Check(6) then
    begin
      Reg.CloseKey();
      Reg.OpenKey(KEY_SERVICE_DISABLED, True);

      // Delete disable key
      if (Reg.KeyExists(Name) and not Reg.DeleteKey(Name)) then
        raise EServiceException.Create('Could not delete disabled key!');
    end  //of begin
    else
      // Delete last status
      if not Reg.DeleteValue(Name) then
        raise EServiceException.Create('Could not delete last status!');

    // Update information
    FEnabled := True;
    FTime := '';
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
    CloseServiceHandle(Service);
  end;  //of try
end;

{ public TServiceListItem.ExportItem

  Exports a list item as .reg file. }

procedure TServiceListItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    RegFile.ExportReg(HKEY_LOCAL_MACHINE, GetLocation(), True);

    if not FEnabled then
    begin
      // Windows >= Vista?
      if TOSVersion.Check(6) then
      begin
        RegFile.ExportKey(HKEY_LOCAL_MACHINE, KEY_SERVICE_DISABLED +'\'+ Name, False);
        RegFile.ExportKey(HKEY_LOCAL_MACHINE, GetLocation(), True);
        RegFile.Save();
      end  //of begin
      else
        RegFile.ExportReg(HKEY_LOCAL_MACHINE, KEY_SERVICE_DISABLED, Name);
    end;  //of begin

  finally
    RegFile.Free;
  end;  //of try
end;

{ public TServiceListItem.ExportItem

  Returns the start type of service as string. }

function TServiceListItem.GetStartText(ALangFile: TLanguageFile): string;
begin
  case FServiceStart of
    ssAutomatic: Result := ALangFile.GetString(61);
    ssManual:    Result := ALangFile.GetString(62);
    else
                 Result := TypeOf;
  end;  //of case
end;


{ TServiceList }

{ public TServiceList.Create

  Constructor for creating a TServiceList instance. }

constructor TServiceList.Create();
begin
  inherited Create;
  FManager := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE,
    SC_MANAGER_ENUMERATE_SERVICE or SC_MANAGER_CREATE_SERVICE);
end;

{ public TServiceList.Destroy

  Destructor for destroying a TServiceList instance. }

destructor TServiceList.Destroy();
begin
  CloseServiceHandle(FManager);
  inherited Destroy;
end;

{ private TServiceList.ItemAt

  Returns a TServiceListItem object at index. }

function TServiceList.ItemAt(AIndex: Word): TServiceListItem;
begin
  Result := TServiceListItem(RootItemAt(AIndex));
end;

{ private TServiceList.GetSelectedItem

  Returns the current selected item as TServiceListItem. }

function TServiceList.GetSelectedItem(): TServiceListItem;
begin
  Result := TServiceListItem(Selected);
end;

{ protected TServiceList.AddServiceDisabled

  Adds a disabled service item to list. }

function TServiceList.AddServiceDisabled(AName, ACaption, AFileName: string;
  AReg: TRegistry): Integer;
var
  Item: TServiceListItem;

begin
  Result := -1;
  Item := TServiceListItem.Create(Count, False, FManager);

  try
    with Item do
    begin
      Name := AName;
      Caption := ACaption;
      FileName := AFileName;
      Start := TServiceStart(AReg.ReadInteger(AName));
      Time := Item.GetTimestamp(AReg);
      TypeOf := 'Service';
    end;  //of with

    Result := inherited Add(Item);

  except
    Item.Free;
  end;  //of try
end;

{ protected TServiceList.AddServiceEnabled

  Adds an enabled service item to list. }

function TServiceList.AddServiceEnabled(AName, ACaption, AFileName: string;
  AStart: TServiceStart): Integer;
var
  Item: TServiceListItem;

begin
  Result := -1;
  Item := TServiceListItem.Create(Count, (AStart <> ssDisabled), FManager);

  try
    with Item do
    begin
      Name := AName;
      Caption := ACaption;
      FileName := AFileName;
      Start := AStart;
      TypeOf := 'Service';
    end;  //of with

    Inc(FActCount);
    Result := inherited Add(Item);

  except
    Item.Free;
  end;  //of try
end;

{ public TServiceList.Add

  Adds a new service item to services location. }

function TServiceList.Add(AFileName, AArguments, ACaption: string): Boolean;
var
  Name, FullPath: string;
  Service: SC_Handle;
  LastError: Cardinal;

begin
  Result := False;
  Name := ExtractFileName(AFileName);

  // Check invalid extension
  if (ExtractFileExt(Name) <> '.exe') then
    raise EArgumentException.Create('Invalid program extension! Must be ''.exe''!');

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    // Escape path using quotes
    FullPath := '"'+ AFileName +'"';

    // Append arguments if used
    if (AArguments <> '') then
      FullPath := FullPath +' '+ AArguments;

    Name := ChangeFileExt(Name, '');

    // Create a new service
    Service := CreateService(FManager, PChar(Name), PChar(ACaption),
      STANDARD_RIGHTS_READ, SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START,
      SERVICE_ERROR_NORMAL, PChar(FullPath), nil, nil, nil, nil, nil);

    // Error occured?
    if (Service = 0) then
    begin
      LastError := GetLastError();

      // Service already exists?
      if (LastError = ERROR_SERVICE_EXISTS) then
        Exit;

      raise EServiceException.Create(SysErrorMessage(LastError));
    end;  //of begin

    CloseServiceHandle(Service);

    // Adds service to list
    Result := (AddServiceEnabled(Name, ACaption, FullPath, ssAutomatic) <> -1);

  finally
    FLock.Release();
  end;  //of try
end;

{ public TServiceList.ExportList

  Exports the complete list as .reg file. }

procedure TServiceList.ExportList(const AFileName: string);
var
  RegFile: TRegistryFile;
  i: Integer;
  Item: TServiceListItem;

begin
  FLock.Acquire;

  // Init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    // Export enabled services
    for i := 0 to Count - 1 do
    begin
      Item := ItemAt(i);
      RegFile.ExportKey(HKEY_LOCAL_MACHINE, Item.Location, True);
    end;  //of for

    // Export disabled services
    RegFile.ExportKey(HKEY_LOCAL_MACHINE, KEY_SERVICE_DISABLED, True);

    // Save .reg file
    RegFile.Save();

  finally
    RegFile.Free;
    FLock.Release();
  end;  //of try
end;

{ public TServiceList.IndexOf

  Returns the index of an item checking caption only. }

function TServiceList.IndexOf(const ACaptionOrName: string): Integer;
var
  i: Integer;
  Item: TServiceListItem;

begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    Item := ItemAt(i);

    // Item name or caption matches?
    if ((Item.Caption = ACaptionOrName) or (Item.Name = ACaptionOrName)) then
    begin
      Result := i;
      Break;
    end;  //of begin
  end;  //of for
end;

{ public TServiceList.Load

  Uses default settings to load items into list. }

procedure TServiceList.Load();
begin
  LoadServices(False);
end;

{ public TServiceList.LoadService

  Loads and adds a service item to the list. }

function TServiceList.LoadService(AName: string; AService: SC_HANDLE;
  AIncludeDemand: Boolean = False): Integer;
var
  ServiceConfig: LPQUERY_SERVICE_CONFIG;
  BytesNeeded, LastError: Cardinal;
  ServiceStart: TServiceStart;
  Reg: TRegistry;

begin
  Result := -1;
  ServiceConfig := nil;

  // Determine the required size for buffer
  QueryServiceConfig(AService, ServiceConfig, 0, BytesNeeded);
  LastError := GetLastError();
  
  // ERROR_INSUFFICIENT_BUFFER will be fired normally
  if (LastError <> ERROR_INSUFFICIENT_BUFFER) then
    raise EServiceException.Create(SysErrorMessage(LastError));

  GetMem(ServiceConfig, BytesNeeded);

  try
    // Read service config
    if not QueryServiceConfig(AService, ServiceConfig, BytesNeeded, BytesNeeded) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    // Determine status and filter services
    case ServiceConfig^.dwStartType of
      SERVICE_AUTO_START:   ServiceStart := ssAutomatic;
      SERVICE_DISABLED:     ServiceStart := ssDisabled;
      SERVICE_DEMAND_START: if AIncludeDemand then
                              ServiceStart := ssManual
                            else
                              Exit;
      else                  Exit;
    end;

    // Read last status of disabled service
    if (ServiceStart = ssDisabled) then
    begin
      Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;

        // Windows >= Vista?
        if TOSVersion.Check(6) then
          Reg.OpenKey(KEY_SERVICE_DISABLED +'\'+ AName, False)
        else
          Reg.OpenKey(KEY_SERVICE_DISABLED, False);

        // Last status exists?
        if not Reg.ValueExists(AName) then
          Exit;

        // Skip demand started services
        if (not AIncludeDemand and (Reg.ReadInteger(AName) = Ord(ssManual))) then
          Exit;

        // Add disabled item to list
        Result := AddServiceDisabled(AName, ServiceConfig^.lpDisplayName,
          ServiceConfig^.lpBinaryPathName, Reg);

      finally
        Reg.CloseKey();
        Reg.Free;
      end;  //of try
    end  //of begin
    else
      // Add enabled item to list
      Result := AddServiceEnabled(AName, ServiceConfig^.lpDisplayName,
        ServiceConfig^.lpBinaryPathName, ServiceStart);

  finally
    FreeMem(ServiceConfig);
  end;  //of try
end;

{ public TServiceList.LoadServices

  Searches for service items. }

procedure TServiceList.LoadServices(AIncludeShared: Boolean);
var
  SearchThread: TServiceSearchThread;

begin
  SearchThread := TServiceSearchThread.Create(Self, FManager, FLock);

  with SearchThread do
  begin
    IncludeShared := AIncludeShared;
    OnError := OnSearchError;
    OnFinish := OnSearchFinish;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    Start;
  end;  //of with
end;


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
  if Failed(FTaskFolder.RegisterTaskDefinition(PChar(Name), FTaskDefinition,
    Ord(TASK_UPDATE), Null, Null, FTaskDefinition.Principal.LogonType, Null, NewTask)) then
    raise ETaskException.Create('Could not update task definition!');

  Result := True;
end;

{ protected TTaskListItem.GetFullLocation

  Returns the full file path to a TTaskListItem. }

function TTaskListItem.GetFullLocation(): string;
begin
  Result := FileName;
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
      ExecAction.Path := PChar(ExtractPathToFile(ANewFilePath));
      ExecAction.Arguments := PChar(ExtractArguments(ANewFilePath));

      // Update task
      Result := UpdateTask(FTaskDefinition);
      FileName := ANewFilePath;
      Break;
    end;  //of begin
  end;  //of while
end;

{ public TTaskListItem.Delete

  Deletes a TTaskListItem object and returns True if successful. }

function TTaskListItem.Delete(): Boolean;
begin
  // Delete task
  if Failed(FTaskFolder.DeleteTask(PChar(Name), 0)) then
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
  Win64 := (TOSVersion.Architecture = arIntelX64);

  if not GetKnownFolderPath(FOLDERID_System, FsLocation) then
    raise Exception.Create('SHGetKnownFolderPath: Could not get system directory!');

  FsLocation := FsLocation + IncludeTrailingPathDelimiter(Location) + Name;

  // Deny WOW64 redirection on 64 Bit Windows
  if Win64 then
    Wow64FsRedirection(True);

  try
    // Copy file
    if not CopyFile(PChar(FsLocation), PChar(AFileName), False) then
      raise ETaskException.Create('Task does not exist!');

  finally
    // Allow WOW64 redirection on 64 Bit Windows again
    if Win64 then
      Wow64FsRedirection(False);
  end;  //of try
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

    if (Actions.Next(1, ActionItem, Fetched) = S_OK) then
    begin
      Action := IDispatch(ActionItem) as IAction;

      // Task has an executable?
      if (Action.ActionType = TASK_ACTION_EXEC) then
      begin
        ExecAction := Action as IExecAction;
        FileName := ExecAction.Path;

        // Append arguments?
        if (ExecAction.Arguments <> '') then
          FileName := FileName +' '+ ExecAction.Arguments;
      end;  //of begin
    end;  //of while

    TypeOf := 'Task';
  end;  //of with

  // Update active counter
  if Item.Enabled then
    Inc(FActCount);

  Result := Add(Item);
end;

{ public TTaskList.ImportBackup

  Imports an exported task item as .xml file and adds it to the list. }

function TTaskList.ImportBackup(const AFileName: string): Boolean;
var
  Path, XmlText: string;
  TaskFile: TStringList;
  TaskFolder: ITaskFolder;
  NewTask: IRegisteredTask;
{$IFDEF WIN32}
  Win64: Boolean;
{$ENDIF}

begin
  Path := '\'+ ChangeFileExt(ExtractFileName(AFileName), '');
{$IFDEF WIN32}
  Win64 := (TOSVersion.Architecture = arIntelX64);

  // Deny WOW64 redirection on 64 Bit Windows
  if Win64 then
    Wow64FsRedirection(True);
{$ENDIF}
  TaskFile := TStringList.Create;

  try
    // Read .xml task file
    TaskFile.LoadFromFile(AFileName);
    XmlText := TaskFile.Text;

  finally
    TaskFile.Free;
  {$IFDEF WIN32}
    // Allow WOW64 redirection on 64 Bit Windows again
    if Win64 then
      Wow64FsRedirection(False);
  {$ENDIF}
  end;  //of try

  // Open root folder
  if Failed(FTaskService.GetFolder('\', TaskFolder)) then
    raise ETaskException.Create('Could not open task folder!');

  // Task exists?
  if Succeeded(TaskFolder.GetTask(PChar(Path), NewTask)) then
    raise EWarning.Create('Task already exists!');

  // Register new task
  if Failed(TaskFolder.RegisterTask(PChar(Path), PChar(XmlText), Ord(TASK_CREATE),
    Null, Null, TASK_LOGON_INTERACTIVE_TOKEN, Null, NewTask)) then
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
  Flags: Byte;

begin
  // Show hidden?
  if AIncludeHidden then
    Flags := Ord(TASK_ENUM_HIDDEN)
  else
    Flags := 0;

  // Add tasks in current folder
  if Failed(ATaskFolder.GetTasks(Flags, TaskCollection)) then
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

procedure TTaskList.LoadTasks(APath: string = '\'; ARecursive: Boolean = False;
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
    Win64 := (TOSVersion.Architecture = arIntelX64);
    IncludeSubFolders := ARecursive;
    IncludeHidden := AIncludeHidden;
    OnError := OnSearchError;
    OnFinish := OnSearchFinish;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    Start;
  end;  //of with
end;

end.
