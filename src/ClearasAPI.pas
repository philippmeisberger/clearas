{ *********************************************************************** }
{                                                                         }
{ Clearas API Interface Unit v4.1                                         }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit ClearasAPI;

interface

uses
  Windows, Classes, SysUtils, Registry, ShlObj, ActiveX, ComObj, CommCtrl,
  ShellAPI, Contnrs, SyncObjs, StrUtils, OSUtils, LanguageFile, IniFileParser;

const
  { Startup Registry keys }
  KEY_DEACT = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupreg\';
  KEY_DEACT_FOLDER = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupfolder\';
  KEY_STARTUP_RUN = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP_RUNONCE = 'SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce';

  { Virtualized WOW64 keys }
  KEY_STARTUP_RUN32 = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP_RUNONCE32 = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce';

  { Context menu Registry subkeys + values}
  CM_SHELL = '\shell';
  CM_SHELL_DISABLE = 'LegacyDisable';
  CM_SHELLEX = '\shellex\ContextMenuHandlers';
  CM_SHELLEX_FILE = 'CLSID\%s\InProcServer32';
  CM_LOCATIONS_DEFAULT = 'Directory, Folder, *, Drive';

  { Extensions of backup files }
  EXT_COMMON = '.CommonStartup';
  EXT_USER = '.Startup';

  { Description type of startup user items }
  TYPE_COMMON = 'Startup Common';
  TYPE_COMMON_XP = 'Common Startup';
  TYPE_USER = 'Startup User';
  TYPE_USER_XP = 'Startup';

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
  EWarning = class(EAbort);

  { TRootItem }
  TRootItem = class(TObject)
  private
    FIndex: Word;
    FName, FType, FFilePath: string;
    function GetArguments(): string;
    function GetFilePath(): string;
    function GetIcon(): HICON;
  protected
    FEnabled, FWow64: Boolean;
    FLocation: string;
    function DeleteQuoteChars(const APath: string): string;
    function ExtractArguments(const APath: string): string;
    function ExtractPathToFile(const APath: string): string;
    function GetFullLocation(): string; virtual; abstract;
    function GetTimestamp(AReg: TRegistry): string;
    function WriteTimestamp(AReg: TRegistry): string;
  public
    constructor Create(AIndex: Word; AEnabled, AWow64: Boolean);
    function ChangeFilePath(const ANewFilePath: string): Boolean; virtual; abstract;
    function ChangeStatus(): Boolean; virtual;
    function Delete(): Boolean; virtual; abstract;
    function Disable(): Boolean; virtual; abstract;
    function Enable(): Boolean; virtual; abstract;
    procedure ExportItem(const AFileName: string); virtual; abstract;
    function GetStatus(ALangFile: TLanguageFile): string;
    procedure OpenInExplorer();
    { external }
    property Arguments: string read GetArguments;
    property Enabled: Boolean read FEnabled;
    property FilePath: string read FFilePath write FFilePath;
    property FilePathOnly: string read GetFilePath;
    property Icon: HICON read GetIcon;
    property ItemIndex: Word read FIndex;
    property Location: string read FLocation write FLocation;
    property LocationFull: string read GetFullLocation;
    property Name: string read FName write FName;
    property TypeOf: string read FType write FType;
    property Wow64: Boolean read FWow64;
  end;

  { TRootRegItem }
  TRootRegItem = class(TRootItem)
  protected
    function DeleteKey(ARootKey, AKeyPath, AKeyName: string;
      AFailIfNotExists: Boolean = True): Boolean;
    function GetWow64Key(): string;
  public
    procedure OpenInRegEdit(); virtual;
    { external }
    property Wow64Location: string read GetWow64Key;
  end;

  { Search event }
  TSearchEvent = procedure(Sender: TObject; const ACount: Cardinal) of object;

  { TRootList }
  TRootList = class(TObjectList)
  private
    FItem: TRootItem;
    FOnSearchStart, FOnSearching: TSearchEvent;
    FOnSearchFinish: TNotifyEvent;
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
    function Remove(ARootItem: TRootItem): Integer; virtual;
    function Last(): TRootItem; virtual;
    { external }
    property Enabled: Word read FActCount;
    property Items[AIndex: Word]: TRootItem read RootItemAt; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnSearchStart: TSearchEvent read FOnSearchStart write FOnSearchStart;
    property OnSearchFinish: TNotifyEvent read FOnSearchFinish write FOnSearchFinish;
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
    FRootKey: TRootKey;
    FTime: string;
  protected
    function GetFullLocation(): string; override;
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    { external }
    property RootKey: TRootKey read FRootKey write FRootKey;
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
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
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
    function AddItem(AFileName, AArguments: string;
      ADisplayedName: string = ''): Boolean;
    function BackupExists(): Boolean;
    function ChangeItemStatus(): Boolean; override;
    function DeleteItem(): Boolean; override;
    function EnableItem(): Boolean; override;
    procedure ExportList(const AFileName: string); override;
    function ImportBackup(const AFileName: string): Boolean;
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

  { TContextListItem }
  TContextListItem = class(TRootRegItem)
  private
    FCaption: string;
    function GetKeyPath(): string; virtual; abstract;
  protected
    function GetFullLocation(): string; override;
  public
    function Delete(): Boolean; override;
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
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
  end;

  { TShellExItem }
  TShellExItem = class(TContextListItem)
  private
    function GetKeyPath(): string; override;
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
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
    function AddShellItem(const AName, ALocationRoot, AFilePath, ACaption: string;
      AEnabled, AWow64: Boolean): Integer;
    function AddShellExItem(const AName, ALocationRoot, AFilePath: string;
      AEnabled, AWow64: Boolean): Integer;
  public
    constructor Create;
    function AddItem(AFilePath, AArguments, ALocationRoot,
      ADisplayedName: string): Boolean;
    procedure ExportList(const AFileName: string); override;
    function IndexOf(AName, ALocationRoot: string): Integer; overload;
    procedure LoadContextmenu(const ALocationRoot: string;
      AWow64: Boolean); overload;
    procedure LoadContextmenu(const ALocationRoot: string;
      ASearchForShellItems: Boolean; AWow64: Boolean); overload;
    procedure LoadContextMenus(ALocationRootCommaList: string = '');
    { external }
    property Item: TContextListItem read GetSelectedItem;
    property Items[AIndex: Word]: TContextListItem read ItemAt; default;
  end;


implementation

uses StartupSearchThread, ContextSearchThread;

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
  Result := TOSUtils.GetWinDir +'\pss\';
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
      IID_IShellLinkA, ShellLink)) then
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
  Name : PWideChar;

begin
  Result := False;

  if (AFileName = '') then
    raise EInvalidArgument.Create('File name for .lnk file must not be empty!');

  if (AExeFileName = '') then
    raise EInvalidArgument.Create('File path to .exe must not be empty!');

  try
    CoInitialize(nil);

    if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
      IID_IShellLinkA, ShellLink)) then
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

      if Succeeded(ShellLink.QueryInterface(IPersistFile, PersistFile)) then
      begin
        GetMem(Name, MAX_PATH * 2);

        try
          // Set up information
          MultiByteToWideChar(CP_ACP, 0, PChar(AFileName), -1, Name, MAX_PATH);

          // Save .lnk
          Result := Succeeded(PersistFile.Save(Name, True));

        finally
          FreeMem(Name, MAX_PATH * 2);
        end; //of finally
      end; //of begin
    end; //of begin

  finally
    CoUninitialize();
  end;  //of try
end;


{ TRootItem }

{ public TRootItem.Create

  General constructor for creating a TRootItem instance. }

constructor TRootItem.Create(AIndex: Word; AEnabled, AWow64: Boolean);
begin
  inherited Create;
  FIndex := AIndex;
  FEnabled := AEnabled;
  FWow64 := AWow64;
end;

{ private TRootItem.GetArguments

  Returns the arguments of the item file path. }

function TRootItem.GetArguments(): string;
begin
  Result := DeleteQuoteChars(ExtractArguments(FFilePath));
end;

{ private TRootItem.GetFilePath

  Returns only the file path (without arguments) of the item. }

function TRootItem.GetFilePath(): string;
var
  Path: string;

begin
  Path := DeleteQuoteChars(ExtractPathToFile(FFilePath));

  // Path has to be expanded?
  if ((Path <> '') and (Path[1] = '%')) then
    Path := TOSUtils.ExpandEnvironmentVar(Path);

  Result := Path;
end;

{ private TRootItem.GetIcon

  Returns the icon handle to the item file path. }

function TRootItem.GetIcon(): HICON;
var
  FileInfo: SHFILEINFO;
  Win64: Boolean;

begin
  Win64 := TOSUtils.IsWindows64();
  
  // Deny WOW64 redirection only on 64bit Windows
  if Win64 then
    TOSUtils.Wow64FsRedirection(True);

  if Succeeded(SHGetFileInfo(PChar(GetFilePath()), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_ICON or SHGFI_SMALLICON)) then
    Result := FileInfo.hIcon
  else
    Result := 0;

  // Allow WOW64 redirection only on 64bit Windows
  if Win64 then
    TOSUtils.Wow64FsRedirection(False);
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

{ protected TRootItem.GetTimestamp

  Returns the deactivation timestamp. }

function TRootItem.GetTimestamp(AReg: TRegistry): string;
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

{ protected TRootItem.WriteTimestamp

  Writes the deactivation timestamp. }

function TRootItem.WriteTimestamp(AReg: TRegistry): string;
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

{ public TRootItem.ChangeStatus

  Changes the item status. }

function TRootItem.ChangeStatus(): Boolean;
begin
  if FEnabled then
    Result := Disable()
  else
    Result := Enable();
end;

{ public TRootItem.GetStatus

  Returns the item status as text. }

function TRootItem.GetStatus(ALangFile: TLanguageFile): string;
begin
  if FEnabled then
    Result := ALangFile.GetString(31)
  else
    Result := ALangFile.GetString(32);
end;

{ public TRootItem.OpenInExplorer

  Opens an TRootItem object in Explorer. }

procedure TRootItem.OpenInExplorer();
var
  PreparedFileName: string;
  Win64: Boolean;

begin
  // Extract the file path only (without arguments and quote chars)
  PreparedFileName := GetFilePath();

  // 64bit Windows?
  Win64 := TOSUtils.IsWindows64();

  // Deny WOW64 redirection only on 64bit Windows
  if Win64 then
    TOSUtils.Wow64FsRedirection(True);

  // Open file in explorer
  if ((PreparedFileName <> '') and FileExists(PreparedFileName)) then
    TOSUtils.ExecuteProgram('explorer.exe', '/select, '+ PreparedFileName)
  else
    raise EWarning.Create('File "'+ PreparedFileName +'" does not exist!');

  // Allow WOW64 redirection only on 64bit Windows
  if Win64 then
    TOSUtils.Wow64FsRedirection(False);
end;


{ TRootRegItem }

{ protected TRootRegItem.DeleteKey

  Deletes a Registry key. }

function TRootRegItem.DeleteKey(ARootKey, AKeyPath, AKeyName: string;
  AFailIfNotExists: Boolean = True): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    Reg.RootKey := TOSUtils.StrToHKey(ARootKey);

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

{ public TRootRegItem.OpenInRegEdit

  Opens a TRootRegItem object in RegEdit. }

procedure TRootRegItem.OpenInRegEdit();
const
  KEY_REGEDIT = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Applets\Regedit';

var
  Reg: TRegistry;

begin
  if FWow64 then
    Reg := TRegistry.Create(KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if not Reg.OpenKey(KEY_REGEDIT, True) then
      raise Exception.Create('Could not open key!');

    // Set the Registry key to show
    Reg.WriteString('LastKey', 'Computer\'+ GetFullLocation());

    // Deny WOW64 redirection only on 64 Bit Windows for 64 Bit items
    if (TOSUtils.IsWindows64() xor (FEnabled and FWow64)) then
    begin
      TOSUtils.Wow64FsRedirection(True);
      TOSUtils.ExecuteProgram('regedit.exe');
      TOSUtils.Wow64FsRedirection(False);
    end  //of begin
    else
      TOSUtils.ExecuteProgram('regedit.exe');

  finally
    Reg.CloseKey();
    Reg.Free;
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
begin
  // Invalid item?
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  // Change item file path
  Result := FItem.ChangeFilePath(ANewFilePath);

  // Notify changed
  if (Result and Assigned(FOnChanged)) then
    FOnChanged(Self);
end;

{ public TRootList.ChangeItemStatus

  Changes the item status. }

function TRootList.ChangeItemStatus(): Boolean;
var
  Changed: Boolean;

begin
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
end;

{ public TRootList.DeleteItem

  Deletes an item from location and list. }

function TRootList.DeleteItem(): Boolean;
var
  Deleted: Boolean;

begin
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
end;

{ public TRootList.DisableItem

  Disables the current selected item. }

function TRootList.DisableItem(): Boolean;
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if not FItem.Enabled then
    raise EWarning.Create('Item already disabled!');

  // Disable item
  Result := FItem.Disable();

  if Result then
  begin
    // Update active counter
    Dec(FActCount);

    // Notify disable
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;  //of begin
end;

{ public TRootList.EnableItem

  Enables the current selected item. }

function TRootList.EnableItem(): Boolean;
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if FItem.Enabled then
    raise EWarning.Create('Item already enabled!');

  // Enable item
  Result := FItem.Enable();

  if Result then
  begin
    // Update active counter
    Inc(FActCount);

    // Notify enable
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;  //of begin
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

{ public TRootList.Remove

  Removes a TRootItem object from the list. }

function TRootList.Remove(ARootItem: TRootItem): Integer;
begin
  Result := inherited Remove(ARootItem);
end;


{ TStartupListItem }

{ protected TStartupListItem.GetFullLocation

  Returns the full Registry path to a TStartupListItem. }

function TStartupListItem.GetFullLocation(): string;
begin
  Result := TOSUtils.HKeyToStr(TOSUtils.StrToHKey(FRootKey)) +'\'+ FLocation;
end;

{ public TStartupListItem.ChangeFilePath

  Changes the file path of an TStartupListItem item. }

function TStartupListItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  Reg: TRegistry;
  ItemName: string;

begin
  Result := False;

  if (FEnabled and FWow64) then
    Reg := TRegistry.Create(KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := TOSUtils.StrToHKey(FRootKey);

    // Invalid key?
    if not Reg.OpenKey(FLocation, False) then
      raise Exception.Create('Key does not exist!');

    if FEnabled then
      ItemName := Name
    else
      ItemName := 'command';

    // Value must exist!
    if not Reg.ValueExists(ItemName) then
      raise Exception.Create('Value does not exist!');

    // Change path
    Reg.WriteString(ItemName, ANewFilePath);
    FilePath := ANewFilePath;
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
      RegFile.ExportReg(TOSUtils.StrToHKey(FRootKey), GetWow64Key(), Name)
    else
      RegFile.ExportReg(TOSUtils.StrToHKey(FRootKey), FLocation, False);

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
    Reg := TRegistry.Create(KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    if FEnabled then
    begin
      Reg.RootKey := TOSUtils.StrToHKey(FRootKey);

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
      if not Reg.OpenKey(KEY_DEACT, False) then
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
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Failed to create new key?
    if not Reg.OpenKey(KEY_DEACT + Name, True) then
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
    Reg.WriteString('hkey', FRootKey);
    Reg.WriteString('key', FLocation);
    Reg.WriteString('item', Name);
    Reg.WriteString('command', FilePath);
    Reg.WriteString('inimapping', '0');

    // Windows >= Vista?
    if TOSUtils.WindowsVistaOrLater() then
      // Save deactivation timestamp
      FTime := WriteTimestamp(Reg);

    // Open startup location
    Reg.CloseKey();

    // Allow redirection to 32 Bit key
    if FWow64 then
      Reg.Access := KEY_READ or KEY_WRITE;

    Reg.RootKey := TOSUtils.StrToHKey(FRootKey);
    Reg.OpenKey(FLocation, False);

    // Delete old value, but do not fail if old value does not exist!
    if (Reg.ValueExists(Name) and not Reg.DeleteValue(Name)) then
      raise EStartupException.Create('Could not delete value!');

    // Update information
    FRootKey := 'HKLM';
    FLocation := KEY_DEACT + Name;
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
  Access64 := TOSUtils.DenyWOW64Redirection(KEY_READ);
  Reg := TRegistry.Create(Access64);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey(FLocation, False) then
      raise EStartupException.Create('Key does not exist!');

    if (not Reg.ValueExists('hkey') or not Reg.ValueExists('key')) then
      raise EStartupException.Create('Missing destination Registry values '
        +'"hkey" or "key"!');

    // Set new values
    NewHKey := Reg.ReadString('hkey');
    NewKeyPath := Reg.ReadString('key');

    if ((NewHKey = '') or (NewKeyPath = '')) then
      raise EStartupException.Create('Invalid destination Registry values for '
        +'"hkey" or "key"!');

    Reg.CloseKey;

    // Allow redirection to 32 Bit key
    if FWow64 then
    begin
      // RunOnce item?
      if (ExtractFileName(NewKeyPath) = 'RunOnce') then
        NewKeyPath := KEY_STARTUP_RUNONCE
      else
        NewKeyPath := KEY_STARTUP_RUN;

      Reg.Access := KEY_READ or KEY_WRITE;
    end  //of begin
    else
      Reg.Access := Access64 or KEY_WRITE;

    Reg.RootKey := TOSUtils.StrToHKey(NewHKey);

    // Failed to create new key?
    if not Reg.OpenKey(NewKeyPath, True) then
      raise EStartupException.Create('Could not open startup key!');

    // Write startup entry
    Reg.WriteString(Name, FilePath);

    // Delete old key
    Reg.CloseKey();
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_DEACT, False);

    // Do not abort if old key does not exist!
    if (Reg.KeyExists(Name) and not Reg.DeleteKey(Name)) then
      raise EStartupException.Create('Could not delete old key!');

    // Update information
    FRootKey := NewHKey;
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

  Changes the file path of a TStartupUserItem item. }

function TStartupUserItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  NewFilePath, Arguments: string;

begin
  if not FEnabled then
    inherited ChangeFilePath(ANewFilePath);

  NewFilePath := DeleteQuoteChars(ExtractPathToFile(ANewFilePath));
  Arguments := DeleteQuoteChars(ExtractArguments(ANewFilePath));

  // Failed to create new .lnk file?
  if (FEnabled and not FLnkFile.WriteLnkFile(FLocation, NewFilePath, Arguments)) then
    raise EStartupException.Create('Could not create .lnk file!');

  // Update information
  FilePath := ANewFilePath;
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
    Result := DeleteKey('HKLM', KEY_DEACT_FOLDER, AddCircumflex(FLnkFile.FileName));
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
    raise EStartupException.Create('Lnk file does not exist!');

  // Create backup by copying original .lnk
  if not FLnkFile.CreateBackup() then
    raise EStartupException.Create('Could not create backup file!');

  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_WRITE));

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    KeyName := AddCircumflex(FLocation);

    if not Reg.OpenKey(KEY_DEACT_FOLDER + KeyName, True) then
      raise EStartupException.Create('Could not create key!');

    Reg.WriteString('path', FLocation);
    Reg.WriteString('item', ChangeFileExt(ExtractFileName(Name), ''));
    Reg.WriteString('command', FilePath);
    Reg.WriteString('backup', FLnkFile.BackupLnk);

    // Special Registry entries only for Windows >= Vista
    if TOSUtils.WindowsVistaOrLater() then
    begin
      Reg.WriteString('backupExtension', FLnkFile.BackupExt);
      Reg.WriteString('location', ExtractFileDir(FLocation));
      FTime := WriteTimestamp(Reg);
    end  //of begin
    else
      Reg.WriteString('location', TypeOf);

    // Delete original .lnk
    if not FLnkFile.Delete() then
      raise EStartupException.Create('Could not delete .lnk file!');

    // Update information
    FLocation := KEY_DEACT_FOLDER + KeyName;
    FRootKey := 'HKLM';
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
      if not FLnkFile.WriteLnkFile(FLnkFile.FileName, GetFilePath(), GetArguments()) then
        raise EStartupException.Create('Could not create .lnk file!');
    end;  //of if

  // Do not abort if old key could not be deleted
  if not DeleteKey('HKLM', KEY_DEACT_FOLDER, AddCircumflex(FLnkFile.FileName), False) then
    raise EStartupException.Create('Could not delete key!');

  // Update information
  FLocation := FLnkFile.FileName;
  FRootKey := '';
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
  if TOSUtils.WindowsVistaOrLater() then
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
  if TOSUtils.WindowsVistaOrLater() then
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
  Item := TStartupItem.Create(Count, False, AWow64);

  try
    with Item do
    begin
      RootKey := 'HKLM';
      FLocation := AReg.CurrentPath;
      Name := ExtractFileName(FLocation);
      FilePath := AReg.ReadString('command');
      Time := GetTimestamp(AReg);

      // RunOnce item?
      if (ExtractFileName(AReg.ReadString('key')) = 'RunOnce') then
        TypeOf := 'RunOnce'
      else
        TypeOf := AReg.ReadString('hkey');
    end;  //of with

    Result := Add(Item);

  except
    Item.Free;
    Result := -1;
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
      RootKey := HKey;
      FLocation := AKeyPath;
      Name := AName;
      FilePath := AFileName;
      Time := '';

      // RunOnce item?
      if (ExtractFileName(AKeyPath) = 'RunOnce') then
        TypeOf := 'RunOnce'
      else
        TypeOf := HKey;
    end;  //of with

    Inc(FActCount);
    Result := Add(Item);

  except
    Item.Free;
    Result := -1;
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
    raise EStartupException.Create('Could not create .lnk file!');

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
      RootKey := 'HKLM';
      FLocation := AReg.CurrentPath;
      Name := ExtractFileName(StringReplace(FLocation, '^', '\', [rfReplaceAll]));
      FilePath := AReg.ReadString('command');
      Time := GetTimestamp(AReg);
      TypeOf := GetStartupUserType(AReg);
      Path := AReg.ReadString('path');

      if ((TypeOf = TYPE_USER) or (TypeOf = TYPE_USER_XP)) then
        Ext := EXT_USER
      else
        Ext := EXT_COMMON;

      // Setup .lnk
      LnkFile := TLnkFile.Create(Path, Ext);
      LnkFile.ExeFileName := ExtractPathToFile(FilePath);
      LnkFile.Arguments := ExtractArguments(FilePath);
    end;  //of with

    Result := Add(Item);

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
      RootKey := '';
      FLocation := ALnkFile.FileName;
      FilePath := ALnkFile.FullPath;
      LnkFile := ALnkFile;
      Name := ExtractFileName(ALnkFile.FileName);
      Time := '';
      TypeOf := GetStartupUserType(AAllUsers);
    end;  //of with

    Inc(FActCount);
    Result := Add(Item);

  except
    Item.Free;
    Result := -1;
  end;  //of try
end;

{ public TStartupList.AddItem

  Adds a new startup item to autostart. }

function TStartupList.AddItem(AFileName, AArguments: string;
  ADisplayedName: string = ''): Boolean;
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
    raise EInvalidArgument.Create('Invalid program extension! Must be ".exe"'
      +' or ".bat"!');

  // File path already exists in another item?
  for i := 0 to Count - 1 do
    if AnsiContainsStr(ItemAt(i).FilePath, AFileName) then
      Exit;

  // Add new startup user item?
  if (Ext = '.exe') then
  begin
    if (ADisplayedName <> '') then
      Name := ADisplayedName
    else
      Name := ChangeFileExt(Name, '');

    Result := AddNewStartupUserItem(Name, AFileName, AArguments);
  end  //of begin
  else
  begin
    Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_WRITE));

    // Try to add new startup item to Registry
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(KEY_STARTUP_RUN, True);
      FullPath := AFileName;

      // Append arguments if used
      if (AArguments <> '') then
        FullPath := FullPath +' '+ AArguments;

      Reg.WriteString(ADisplayedName, FullPath);

      // Adds item to list
      Result := (AddItemEnabled(HKEY_CURRENT_USER, KEY_STARTUP_RUN, ADisplayedName,
        FullPath, False) <> -1);

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try
  end;  //of begin
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
  // Init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    for i := 0 to Count - 1 do
    begin
      Item := ItemAt(i);

      // Skip enabled startup user items (not in Registry)!
      if ((Item is TStartupUserItem) and Item.Enabled) then
        Continue;

      RegFile.ExportKey(TOSUtils.StrToHKey(Item.RootKey), Item.GetWow64Key(), True);
    end;  //of for

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
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
    raise EInvalidArgument.Create('Invalid backup file extension! Must be "'
      + EXT_COMMON +'" or "'+ EXT_USER+'"!');

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
      if (ItemAt(i).FilePath = LnkFile.ExeFileName) then
        Exit;

    // Create .lnk file and add it to list
    Result := AddNewStartupUserItem(Name, LnkFile.ExeFileName, LnkFile.Arguments,
      (Ext = EXT_COMMON));

  finally
    LnkFile.Free;
  end;  //of try
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
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));
  Reg.RootKey := HKEY_LOCAL_MACHINE;

  if AStartupUser then
    KeyPath := KEY_DEACT_FOLDER
  else
    KeyPath := KEY_DEACT;

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
    Reg := TRegistry.Create(KEY_READ)
  else
    Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));

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
    Win64 := TOSUtils.IsWindows64();
    IncludeRunOnce := AIncludeRunOnce;
    OnStart := FOnSearchStart;
    OnSearching := FOnSearching;
    OnFinish := FOnSearchFinish;
    Resume;
  end;  // of with
end;


{ TContextListItem }

{ protected TContextListItem.GetFullLocation

  Returns the Registry path to a TStartupListItem. }

function TContextListItem.GetFullLocation(): string;
begin
  Result := TOSUtils.HKeyToStr(HKEY_CLASSES_ROOT) +'\'+ GetKeyPath();
end;

{ public TContextListItem.Delete

  Deletes a TContextListItem object and returns True if successful. }

function TContextListItem.Delete(): Boolean;
begin
  if not DeleteKey('HKCR', ExtractFileDir(GetKeyPath()), Name) then
    raise EContextMenuException.Create('Could not delete key!');

  Result := True;
end;


{ TShellItem }

{ private TShellItem.GetKeyPath

  Returns the Registry path of a TShellItem item. }

function TShellItem.GetKeyPath(): string;
begin
  Result := FLocation + CM_SHELL +'\'+ Name;
end;

{ public TShellItem.ChangeFilePath

  Changes the file path of an TShellItem item. }

function TShellItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(TWinWOW64.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetKeyPath() +'\command', False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Change path
    case Reg.GetDataType('') of
      rdExpandString: Reg.WriteExpandString('', ANewFilePath);
      rdString:       Reg.WriteString('', ANewFilePath);
      else
                      raise EContextMenuException.Create('Invalid data type!');
    end;  //of case

    FilePath := ANewFilePath;
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
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(GetKeyPath(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    Reg.WriteString(CM_SHELL_DISABLE, '');

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
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key invalid?
    if not Reg.OpenKey(GetKeyPath(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Delete disable value, but do not fail if value does not exist!
    if (Reg.ValueExists(CM_SHELL_DISABLE) and not Reg.DeleteValue(CM_SHELL_DISABLE)) then
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

{ private TShellExItem.GetKeyPath

  Returns the Registry path to a TShellExItem. }

function TShellExItem.GetKeyPath(): string;
begin
  Result := FLocation + CM_SHELLEX +'\'+ Name;
end;

{ public TShellExItem.ChangeFilePath

  Changes the file path of an TShellExItem item. }

function TShellExItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  Reg: TRegistry;
  ProgramKeyPath: string;

begin
  Result := False;

  if FWow64 then
    Reg := TRegistry.Create(KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

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
      rdExpandString: Reg.WriteExpandString('', ANewFilePath);
      rdString:       Reg.WriteString('', ANewFilePath);
      else
                      raise EContextMenuException.Create('Invalid data type!');
    end;  //of case

    // Update path
    FilePath := ANewFilePath;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TShellExItem.Disable

  Disables a TShellExItem object and returns True if successful. }

function TShellExItem.Disable(): Boolean;
var
  Reg: TRegistry;
  OldValue, NewValue: string;

begin
  Result := False;
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

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

      // Item enabled?
    if (OldValue[1] = '{') then
    begin
      // Set up new value and write it
      NewValue := '-'+ OldValue;
      Reg.WriteString('', NewValue);
    end;  //of begin

    // Update status
    FEnabled := False;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TShellExItem.Enable

  Enables a TShellExItem object and returns True if successful. }

function TShellExItem.Enable(): Boolean;
var
  Reg: TRegistry;
  OldValue, NewValue: string;

begin
  Result := False;
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

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

    // Item really disabled?
    if (OldValue[1] <> '{') then
    begin
      // Set up new value and write it
      NewValue := Copy(OldValue, 2, Length(OldValue));
      Reg.WriteString('', NewValue);
    end;  //of begin

    // Update status
    FEnabled := True;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
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
    Reg := TRegistry.Create(KEY_READ)
  else
    Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));

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

function TContextList.AddShellItem(const AName, ALocationRoot, AFilePath,
  ACaption: string; AEnabled, AWow64: Boolean): Integer;
var
  Item: TContextListItem;

begin
  Item := TShellItem.Create(Count, AEnabled, AWow64);

  with Item do
  begin
    Name := AName;
    LocationRoot := ALocationRoot;
    FilePath := AFilePath;
    Caption := ACaption;
    TypeOf := 'Shell';

    if AEnabled then
      Inc(FActCount);
  end;  //of with

  Result := Add(Item);
end;

{ protected TContextList.AddShellExItem

  Adds a shellex item to list. }

function TContextList.AddShellExItem(const AName, ALocationRoot, AFilePath: string;
  AEnabled, AWow64: Boolean): Integer;
var
  Item: TContextListItem;

begin
  Item := TShellExItem.Create(Count, AEnabled, AWow64);

  with Item do
  begin
    Name := AName;
    LocationRoot := ALocationRoot;
    FilePath := AFilePath;
    TypeOf := 'ShellEx';

    if AEnabled then
      Inc(FActCount);
  end;  //of with

  Result := Add(Item);
end;

{ public TContextList.AddItem

  Adds a new contextmenu entry. }

function TContextList.AddItem(AFilePath, AArguments, ALocationRoot,
  ADisplayedName: string): Boolean;
var
  Name, Ext, FullPath, KeyName: string;
  Reg: TRegistry;

begin
  Result := False;

  if ((Trim(ALocationRoot) = '') or (Trim(ADisplayedName) = '')) then
    raise EInvalidArgument.Create('Missing argument!');

  Ext := ExtractFileExt(AFilePath);
  Name := ChangeFileExt(ExtractFileName(AFilePath), '');

  // Check invalid extension
  if ((Ext <> '.exe') and (Ext <> '.bat')) then
    raise EInvalidArgument.Create('Invalid program extension! Must be ".exe"'
      +' or ".bat"!');

  // File path already exists in another item?
  if (IndexOf(Name, ALocationRoot) <> -1) then
    Exit;

  // Escape space char using quotes
  FullPath := '"'+ AFilePath +'"';

  // Append arguments if used
  if (AArguments <> '') then
    FullPath := FullPath +' '+ AArguments;

  // Build Registry key name
  KeyName := ALocationRoot + CM_SHELL +'\'+ Name;

  // Adds new context item to Registry
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_WRITE));

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if not Reg.OpenKey(KeyName, True) then
      raise EContextMenuException.Create('Could not create key!');

    // Write caption of item
    Reg.WriteString('', ADisplayedName);
    Reg.CloseKey();

    if not Reg.OpenKey(KeyName +'\command', True) then
      raise EContextMenuException.Create('Could not create "command" key!');

    // Write command of item
    Reg.WriteString('', FullPath);

    // Adds item to list
    AddShellItem(Name, ALocationRoot, FullPath, ADisplayedName, True, False);
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
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

{ public TContextList.LoadContextmenu

  Searches for Shell and ShellEx context menu entries in specific Registry key
  and adds them to the list. }

procedure TContextList.LoadContextmenu(const ALocationRoot: string;
  AWow64: Boolean);
begin
  LoadContextmenu(ALocationRoot, True, AWow64);
  LoadContextmenu(ALocationRoot, False, AWow64);
end;

{ public TContextList.LoadContextmenu

  Searches for either Shell or ShellEx context menu entries in specific
  Registry key and adds them to the list. }

procedure TContextList.LoadContextmenu(const ALocationRoot: string;
  ASearchForShellItems: Boolean; AWow64: Boolean);
var
  Reg: TRegistry;
  i: Integer;
  List: TStringList;
  Item, Key, FilePath, GuID, Caption: string;
  Enabled, Wow64: Boolean;
  Access64: Cardinal;

begin
  Access64 := TOSUtils.DenyWOW64Redirection(KEY_READ);
  Reg := TRegistry.Create(Access64);
  List := TStringList.Create;

  if ASearchForShellItems then
    Key := ALocationRoot + CM_SHELL
  else
    Key := ALocationRoot + CM_SHELLEX;

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(Key, False);

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
      if ASearchForShellItems then
      begin
        Caption := Reg.ReadString('');

        // Filter unreadable Shell items
        if ((Caption <> '') and (Caption[1] = '@')) then
          Continue;

        // Get status and caption of Shell item
        Enabled := not Reg.ValueExists(CM_SHELL_DISABLE);

        // Filter Shell items without command
        if not Reg.OpenKey('command', False) then
          Continue;

        // Filter important Shell items
        if Reg.ValueExists('DelegateExecute') then
          Continue;

        // Get file path of command
        FilePath := Reg.ReadString('');

        // Add item to list
        AddShellItem(Item, ALocationRoot, FilePath, Caption, Enabled, False);
      end  //of begin
      else
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
          FilePath := Reg.ReadString('')
        else
          if AWow64 then
          begin
            Reg.Access := KEY_READ;
            Wow64 := True;

            if Reg.OpenKey(Format(CM_SHELLEX_FILE, [GuID]), False) then
              FilePath := Reg.ReadString('');

            Reg.Access := Access64;
          end;  //of if

        // Add item to list
        AddShellExItem(Item, ALocationRoot, FilePath, Enabled, Wow64);
      end;  //of if
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
    Win64 := TOSUtils.IsWindows64();
    OnStart := FOnSearchStart;
    OnSearching := FOnSearching;
    OnFinish := FOnSearchFinish;
    Resume;
  end;  // of with
end;

end.
