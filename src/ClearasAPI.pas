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
  ShellAPI, Contnrs, OSUtils, LanguageFile, IniFileParser, SyncObjs;

const
  { Registry keys }
  KEY_DEACT = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupreg\';
  KEY_DEACT_FOLDER = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupfolder\';
  KEY_RECYCLEBIN = 'CLSID\{645FF040-5081-101B-9F08-00AA002F954E}\shell';
  KEY_RUNONCE = 'SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce';
  KEY_RUNONCE32 = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce';
  KEY_STARTUP = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP32 = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';
  KEY_REGEDIT = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Applets\Regedit';

  { Context menu Registry subkeys }
  CONTEXTMENU_SHELL = '\shell';
  CONTEXTMENU_SHELLEX = '\shellex\ContextMenuHandlers';
  CONTEXTMENU_SHELLEX_FILE = 'CLSID\%s\InProcServer32';

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

  { TRegUtils }
  TRegUtils = class(TOSUtils)
  protected
    class function DeleteKey(AMainKey, AKeyPath, AKeyName: string): Boolean;
    class function DeleteValue(AMainKey, AKeyName, AValueName: string): Boolean;
  public
    class function DeleteExt(AName: string): string;
    class function GetKeyValue(AMainKey, AKeyPath, AValueName: string): string;
    class function RegisterInContextMenu(ACheck: Boolean): Boolean;
    class function UpdateContextPath(): Boolean; overload;
    class function UpdateContextPath(ALangFile: TLanguageFile): Boolean; overload; deprecated;
    class procedure WriteStrValue(AMainKey, AKeyName, AName, AValue: string);
  end;

  { TRootItem }
  TRootItem = class(TObject)
  private
    FIndex: Word;
    FEnabled: Boolean;
    FName, FType, FFilePath: string;
    function GetArguments(): string;
    function GetFilePath(): string;
    function GetIcon(): HICON;
  protected
    function ExtractArguments(const APath: string): string;
    function ExtractPathToFile(const APath: string): string;
  public
    constructor Create(AIndex: Word; AEnabled: Boolean);
    function ChangeFilePath(const ANewFilePath: string): Boolean; virtual; abstract;
    function ChangeStatus(): Boolean; virtual;
    function Delete(): Boolean; virtual; abstract;
    function Disable(): Boolean; virtual; abstract;
    function Enable(): Boolean; virtual; abstract;
    procedure ExportItem(const AFileName: string); virtual; abstract;
    function GetFullKeyPath(): string; virtual; abstract;
    function GetStatus(ALangFile: TLanguageFile): string;
    procedure OpenInRegEdit(); virtual;
    procedure OpenInExplorer(); virtual;
    { external }
    property Arguments: string read GetArguments;
    property Enabled: Boolean read FEnabled;
    property FilePath: string read FFilePath write FFilePath;
    property FilePathOnly: string read GetFilePath;
    property Icon: HICON read GetIcon;
    property ItemIndex: Word read FIndex;
    property Name: string read FName write FName;
    property TypeOf: string read FType write FType;
  end;

  { TRootList }
  TRootList = class(TObjectList)
  private
    FActCount: Word;
  protected
    function RootItemAt(AIndex: Word): TRootItem;
  public
    constructor Create;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean; virtual; abstract;
    function ChangeItemStatus(): Boolean; virtual; abstract;
    procedure Clear; override;
    function DeleteItem(): Boolean; virtual; abstract;
    function DisableItem(): Boolean; virtual; abstract;
    function EnableItem(): Boolean; virtual; abstract;
    procedure ExportItem(const AFileName: string); virtual; abstract;
    procedure ExportList(const AFileName: string); virtual; abstract;
    function IndexOf(const AItemName: string): Integer; overload;
    function IndexOf(AItemName: string; AEnabled: Boolean): Integer; overload;
    { external }
    property ActCount: Word read FActCount;
  end;

  { Exception class }
  EInvalidItem = class(EAccessViolation);
  EStartupException = class(Exception);

  { TStartupListItem }
  TStartupListItem = class(TRootItem)
  private
    FRootKey, FKeyPath, FTime: string;
    function GetTime(): string;
    procedure WriteTime(const AKeyPath: string);
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    function GetFullKeyPath(): string; override;
    { external }
    property KeyPath: string read FKeyPath write FKeyPath;
    property RootKey: string read FRootKey write FRootKey;
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
  public
    destructor Destroy; override;
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    function Delete(): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    function GetFullKeyPath(): string; override;
    { external }
    property LnkFile: TLnkFile read FLnkFile write FLnkFile;
  end;

  { Event }
  TSearchEvent = procedure(Sender: TObject; ACount: Cardinal) of object;

  { TStartupList }
  TStartupList = class(TRootList)
  private
    FItem: TStartupListItem;
    FDeleteBackup: Boolean;
    FOnSearchStart, FOnSearching: TSearchEvent;
    FOnSearchFinish: TNotifyEvent;
    FLock: TCriticalSection;
    function Add(AItem: TStartupListItem): Word; virtual;
    function DelCircumflex(AName: string): string;
    function ItemAt(AIndex: Word): TStartupListItem;
    procedure GetLnkFileNames(AFileList: TStrings; AAllUsers: Boolean);
    function GetStartupUserType(const AKeyPath: string): string; overload;
    function GetStartupUserType(const AAllUsers: Boolean): string; overload;
  protected
    function AddItemDisabled(const AKeyPath: string): Word;
    function AddItemEnabled(const ARootKey, AKeyPath, AName, AFilePath: string): Word;
    function AddNewStartupUserItem(AName, AFilePath: string;
      AArguments: string = ''; AAllUsers: Boolean = False): Boolean;
    function AddUserItemDisabled(const AKeyPath: string): Word;
    function AddUserItemEnabled(ALnkFile: TLnkFile; AAllUsers: Boolean): Word;
  public
    constructor Create;
    destructor Destroy; override;
    function AddProgram(AFilePath, AArguments: string;
      ADisplayedName: string = ''): Boolean;
    function BackupExists(): Boolean;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean; override;
    function ChangeItemStatus(): Boolean; override;
    function DeleteItem(): Boolean; override;
    function DisableItem(): Boolean; override;
    function EnableItem(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    procedure ExportList(const AFileName: string); override;
    function ImportBackup(const AFilePath: string): Boolean;
    procedure LoadAutostart(AIncludeRunOnce: Boolean);
    procedure LoadDisabled(const AKeyPath: string);
    procedure LoadEnabled(const AAllUsers: Boolean); overload;
    procedure LoadEnabled(const ARootKey, AKeyPath: string); overload;
    { external }
    property DeleteBackup: Boolean read FDeleteBackup write FDeleteBackup;
    property Items[AIndex: Word]: TStartupListItem read ItemAt; default;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnSearchStart: TSearchEvent read FOnSearchStart write FOnSearchStart;
    property OnSearchFinish: TNotifyEvent read FOnSearchFinish write FOnSearchFinish;
    property Selected: TStartupListItem read FItem write FItem;
  end;

  { Alias class }
  TAutostart = TStartupList;

  { Exception class }
  EContextMenuException = class(Exception);

  { TContextListItem }
  TContextListItem = class(TRootItem)
  private
    FLocation, FCaption: string;
    function GetKeyPath(): string; virtual; abstract;
  public
    function Delete(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    function GetFullKeyPath(): string; override;
    { external }
    property Caption: string read FCaption write FCaption;
    property KeyPath: string read GetKeyPath;
    property Location: string read FLocation write FLocation;
  end;

  { TShellItem }
  TShellItem = class(TContextListItem)
  private
    function GetKeyPath(): string; override;
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  end;

  { TShellExItem }
  TShellExItem = class(TContextListItem)
  private
    function GetKeyPath(): string; override;
    function GetProgramPathKey(): string;
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  end;

  { TContextList }
  TContextList = class(TRootList)
  private
    FItem: TContextListItem;
    FOnSearchStart, FOnSearching: TSearchEvent;
    FOnSearchFinish: TNotifyEvent;
    FLock: TCriticalSection;
    function Add(AItem: TContextListItem): Word; virtual;
    function FindDouble(AName, ALocation: string): Boolean;
    function ItemAt(AIndex: Word): TContextListItem;
  protected
    function AddShellItem(const AName, ALocation, AFilePath, ACaption: string;
      AEnabled: Boolean): Word;
    function AddShellExItem(const AName, ALocation, AFilePath: string;
      AEnabled: Boolean): Word;
    procedure LoadContextmenu(const ALocation: string;
      ASearchForShellItems: Boolean); overload;
  public
    constructor Create;
    destructor Destroy; override;
    function AddEntry(const AFilePath, AArguments, ALocation,
      ADisplayedName: string): Boolean;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean; override;
    function ChangeItemStatus(): Boolean; override;
    function DeleteItem(): Boolean; override;
    function DisableItem(): Boolean; override;
    function EnableItem(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    procedure ExportList(const AFileName: string); override;
    function IndexOf(AName, ALocation: string): Integer; overload;
    procedure LoadContextmenu(); overload;
    procedure LoadContextmenu(const ALocation: string); overload;
    procedure LoadContextMenus(ALocations: string = 'Directory, Folder, *, Drive');
    { external }
    property Items[AIndex: Word]: TContextListItem read ItemAt; default;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnSearchStart: TSearchEvent read FOnSearchStart write FOnSearchStart;
    property OnSearchFinish: TNotifyEvent read FOnSearchFinish write FOnSearchFinish;
    property Selected: TContextListItem read FItem write FItem;
  end;

implementation

uses StrUtils, StartupSearchThread, ContextSearchThread;

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
  Result := WriteLnkFile(GetBackupLnk(), FExeFileName, FArguments);
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
      IShellLink, ShellLink)) then
    begin
      PersistFile := ShellLink as IPersistFile;

      if Succeeded(PersistFile.Load(StringToOleStr(FFileName), STGM_READ)) then
        with ShellLink do
        begin
          SetLength(Path, MAX_PATH + 1);

          if Succeeded(GetPath(PChar(Path), MAX_PATH, FileInfo, SLR_ANY_MATCH)) then
          begin
            FExeFileName := PChar(Path);
            SetLength(Arguments, MAX_PATH + 1);

            if Succeeded(GetArguments(PChar(Arguments), MAX_PATH)) then
              FArguments := PChar(Arguments);

            Result := True;
          end;  //of begin
        end; //of with
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

  if (AFileName = '') or (AExeFileName = '') then
    raise EStartupException.Create('Arguments for .lnk file must not be empty!');

  try
    CoInitialize(nil);

    if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_inPROC_SERVER,
      IID_IShellLinkA, ShellLink)) then
    begin
      // Set path to .exe
      ShellLink.SetPath(PChar(AExeFileName));

      // Set arguments if specified
      if (AArguments <> '') then
        ShellLink.SetArguments(PChar(AArguments));

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


{ TRegUtils }

{ public TRegUtils.DeleteKey

  Deletes a Registry key. }

class function TRegUtils.DeleteKey(AMainKey, AKeyPath, AKeyName: string): Boolean;
var
  reg: TRegistry;

begin
  Result := False;
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    reg.RootKey := StrToHKey(AMainKey);

    if not reg.OpenKey(AKeyPath, False) then
      raise Exception.Create('Key does not exist!');

    Result := reg.DeleteKey(AKeyName);

  finally
    reg.Free;
  end;  //of try
end;

{ public TRegUtils.DeleteValue

  Deletes a Registry value. }

class function TRegUtils.DeleteValue(AMainKey, AKeyName, AValueName: string): Boolean;
var
  reg: TRegistry;

begin
  Result := False;
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    reg.RootKey := StrToHKey(AMainKey);

    if not reg.OpenKey(AKeyName, False) then
      raise Exception.Create('Key does not exist!');

    if reg.ValueExists(AValueName) then
      Result := reg.DeleteValue(AValueName)
    else
      Result := True;

  finally
    reg.Free;
  end;  //of try
end;

{ public TRegUtils.DeleteExt

  Deletes the file extension of a file name. }

class function TRegUtils.DeleteExt(AName: string): string;
var
  Index: Integer;
  Ext: string;

begin
  // Extract extension
  Ext := ExtractFileExt(AName);

  // Search for given extension
  Index := Pos(Ext, AName);

  // Extension found?
  if (Index <> 0) then
    Delete(AName, Index, Index + Length(Ext) - 1);

  Result := AName;
end;

{ public TRegUtils.GetKeyValue

  Returns a Registry value as string. }

class function TRegUtils.GetKeyValue(AMainKey, AKeyPath, AValueName: string): string;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ));

  try
    reg.RootKey := StrToHKey(AMainKey);

    if reg.OpenKey(AKeyPath, False) then
    begin
      // Only read if value exists
      if (reg.ValueExists(AValueName)) then
        Result := reg.ReadString(AValueName)
      else
        Result := '';
    end  //of begin
    else
      Result := '';

  finally
    reg.Free;
  end;  //of try
end;

{ public TRegUtils.RegisterInContextMenu

  Adds or deletes "Clearas" in recycle bin context menu. }

class function TRegUtils.RegisterInContextMenu(ACheck: Boolean): Boolean;
begin
  // Checkbox checked?
  if ACheck then
    // Remove recycle bin context menu entry
    Result := DeleteKey('HKCR', KEY_RECYCLEBIN, 'Clearas')
  else
    begin
      // Add recycle bin context menu entry
      WriteStrValue('HKCR', KEY_RECYCLEBIN +'\Clearas\command', '', ParamStr(0));
      Result := True;
    end;  //of begin
end;

{ public TRegUtils.UpdateContextPath

  Updates "Open Clearas" in recycle bin context menu. }

class function TRegUtils.UpdateContextPath(): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE));
  reg.RootKey := HKEY_CLASSES_ROOT;

  try
    // Only update if context menu entry exists
    if reg.OpenKey(KEY_RECYCLEBIN +'\Clearas\command', False) then
    begin
      reg.WriteString('', ParamStr(0));
      Result := True;
    end  //of begin
    else
      Result := False;

  finally
    reg.Free;
  end;  //of try
end;

{ public TRegUtils.UpdateContextPath

  Updates "Open Clearas" in recycle bin context menu. }

class function TRegUtils.UpdateContextPath(ALangFile: TLanguageFile): Boolean;
var
  reg: TRegistry;
  ClearasKey: string;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_ALL_ACCESS));
  reg.RootKey := HKEY_CLASSES_ROOT;

  try
    reg.OpenKey(KEY_RECYCLEBIN, False);
    ClearasKey := ALangFile.GetString(37);

    // Only update if context menu entry exists
    if (reg.KeyExists(ClearasKey)) then
    begin
      // Delete old context menu key
      if not reg.DeleteKey(ClearasKey) then
        raise Exception.Create('Could not delete key: '+ ClearasKey);
    end;  //of begin

  finally
    reg.Free;
    Result := UpdateContextPath();
  end;  //of try
end;

{ public TRegUtils.WriteStrValue

  Writes a string value to Registry }

class procedure TRegUtils.WriteStrValue(AMainKey, AKeyName, AName, AValue: string);
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE));

  try
    try
      reg.RootKey := StrToHKey(AMainKey);
      reg.OpenKey(AKeyName, True);
      reg.WriteString(AName, AValue);

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Could not write "'+ AName +'" with value "'+ AValue +'": '+ E.Message;
      raise;
    end;  //of begin
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
  Result := StringReplace(ExtractArguments(FFilePath), '"', '', [rfReplaceAll]);
end;

{ private TRootItem.GetFilePath

  Returns only the file path (without arguments) of the item. }

function TRootItem.GetFilePath(): string;
begin
  Result := StringReplace(ExtractPathToFile(FFilePath), '"', '', [rfReplaceAll]);
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
    Result := Disable
  else
    Result := Enable;
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

  // Variable has to be expanded?
  if ((PreparedFileName <> '') and (PreparedFileName[1] = '%')) then
    PreparedFileName := TOSUtils.ExpandEnvironmentVar(PreparedFileName);

  // 64bit Windows?
  Win64 := TOSUtils.IsWindows64();

  // Deny WOW64 redirection only on 64bit Windows
  if Win64 then
    TOSUtils.Wow64FsRedirection(True);

  // Open file in explorer
  if ((PreparedFileName <> '') and FileExists(PreparedFileName)) then
    TOSUtils.ExecuteProgram('explorer.exe', '/select, '+ PreparedFileName)
  else
    raise EAbort.Create('File "'+ PreparedFileName +'" does not exist!');

  // Allow WOW64 redirection only on 64bit Windows
  if Win64 then
    TOSUtils.Wow64FsRedirection(False);
end;

{ public TRootItem.OpenInRegEdit

  Opens an TRootItem object in RegEdit. }

procedure TRootItem.OpenInRegEdit();
begin
  // Set the Registry key to show
  TRegUtils.WriteStrValue('HKCU', KEY_REGEDIT, 'LastKey', 'Computer\'
    + GetFullKeyPath());

  // Deny WOW64 redirection only on 64bit Windows
  if TOSUtils.IsWindows64() then
  begin
    TOSUtils.Wow64FsRedirection(True);
    TOSUtils.ExecuteProgram('regedit.exe');
    TOSUtils.Wow64FsRedirection(False);
  end  //of begin
  else
    TOSUtils.ExecuteProgram('regedit.exe');
end;


{ TRootList }

{ public TRootList.Create

  General constructor for creating a TRootList instance. }

constructor TRootList.Create;
begin
  inherited Create;
  FActCount := 0;
end;

{ protected TRootList.RootItemAt

  Returns a TRootItem object at index. }

function TRootList.RootItemAt(AIndex: Word): TRootItem;
begin
  Result := TRootItem(Self.Items[AIndex]);
end;

{ public TRootList.Clear

  Deletes all items in the list. }

procedure TRootList.Clear;
begin
  inherited Clear;
  FActCount := 0;
end;

{ public TRootList.IndexOf

  Returns the index of an item checking name only. }

function TRootList.IndexOf(const AItemName: string): Integer;
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

  for i := 0 to Count -1 do
  begin
    Item := RootItemAt(i);

    if ((Item.Name = AItemName) and (Item.Enabled = AEnabled)) then
    begin
      Result := i;
      Break;
    end;  //of begin
  end;  //of for
end;


{ TStartupListItem }

{ private TStartupListItem.GetTime

  Returns the deactivation time stamp. }

function TStartupListItem.GetTime(): string;
var
  reg: TRegistry;
  Year, Month, Day, Hour, Min, Sec: Word;
  Date, Time: string;

begin
  Result := '';

  // Deactivation timestamp only available for disabled items
  if FEnabled then
    Exit;

  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_READ));

  try
    reg.RootKey := TRegUtils.StrToHKey(FRootKey);
    reg.OpenKey(FKeyPath, False);

    // At least one valid date entry exists?
    if reg.ValueExists('YEAR') then
    begin
      Year := reg.ReadInteger('YEAR');
      Month := reg.ReadInteger('MONTH');
      Day := reg.ReadInteger('DAY');
      Hour := reg.ReadInteger('HOUR');
      Min := reg.ReadInteger('MINUTE');
      Sec := reg.ReadInteger('SECOND');
      Date := FormatDateTime('c', EncodeDate(Year, Month, Day));
      Time := FormatDateTime('tt', EncodeTime(Hour, Min, Sec, 0));
      Result := Date +'  '+ Time;
    end;  //of if

  finally
    reg.Free;
  end;  //of try
end;

{ private TStartupListItem.WriteTime

  Writes the deactivation timestamp. }

procedure TStartupListItem.WriteTime(const AKeyPath: string);
var
  reg: TRegistry;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  TimeNow: TDateTime;

begin
  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_WRITE));
  reg.RootKey := HKEY_LOCAL_MACHINE;

  try
    try
      reg.OpenKey(AKeyPath, True);

      // Read current time
      TimeNow := Now();
      FTime := FormatDateTime('c', TimeNow);

      // Split current date
      DecodeDate(TimeNow, Year, Month, Day);

      // Split current time
      DecodeTime(TimeNow, Hour, Min, Sec, MSec);

      // Write time stamp
      with reg do
      begin
        WriteInteger('YEAR', Year);
        WriteInteger('MONTH', Month);
        WriteInteger('DAY', Day);
        WriteInteger('HOUR', Hour);
        WriteInteger('MINUTE', Min);
        WriteInteger('SECOND', Sec);
      end;  //of with

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while writing deactivation time to "'+ AKeyPath +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupListItem.ChangeFilePath

  Changes the file path of an TStartupListItem item. }

function TStartupListItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  reg: TRegistry;
  ItemName: string;

begin
  Result := False;
  reg := TRegistry.Create(TWinWOW64.DenyWOW64Redirection(KEY_ALL_ACCESS));

  try
    try
      reg.RootKey := TOSUtils.StrToHKey(FRootKey);

      // Invalid key?
      if not reg.OpenKey(FKeyPath, False) then
        raise Exception.Create('Key does not exist!');

      if FEnabled then
        ItemName := FName
      else
        ItemName := 'command';

      // Value must exist!
      if not reg.ValueExists(ItemName) then
        raise Exception.Create('Value does not exist!');

      // Change path
      reg.WriteString(ItemName, ANewFilePath);
      FFilePath := ANewFilePath;
      Result := True;

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while changing file path of "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
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
      RegFile.ExportReg(TRegUtils.StrToHKey(FRootKey), FKeyPath, FName)
    else
      RegFile.ExportReg(TRegUtils.StrToHKey(FRootKey), FKeyPath, False);

  finally
    RegFile.Free;
  end;  //of try
end;

{ public TStartupListItem.GetFullKeyPath

  Returns the Registry path to a TStartupListItem. }

function TStartupListItem.GetFullKeyPath(): string;
begin
  Result := TRegUtils.HKeyToStr(TRegUtils.StrToHKey(FRootKey)) +'\'+ FKeyPath;
end;


{ TStartupItem }

{ public TStartupItem.Delete

  Deletes a TStartupItem object and returns True if successful. }

function TStartupItem.Delete(): Boolean;
begin
  try
    if FEnabled then
    begin
      if not TRegUtils.DeleteValue(FRootKey, FKeyPath, FName) then
        raise EStartupException.Create('Could not delete value!');
    end  //of begin
    else
      if not TRegUtils.DeleteKey('HKLM', KEY_DEACT, FName) then
        raise EStartupException.Create('Could not delete key!');

    Result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while deleting "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupItem.Disable

  Disables an TStartupItem object and returns True if successful. }

function TStartupItem.Disable(): Boolean;
var
  reg: TRegistry;

begin
  Result := False;
  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_WRITE));

  try
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;

      // Successfully deleted old entry?
      if not TRegUtils.DeleteValue(FRootKey, FKeyPath, FName) then
        raise EStartupException.Create('Could not delete value!');

      // Successfully created new key?
      if (reg.OpenKey(KEY_DEACT + FName, True)) then
      begin
        // Write values
        reg.WriteString('hkey', FRootKey);
        reg.WriteString('key', FKeyPath);
        reg.WriteString('item', FName);
        reg.WriteString('command', FFilePath);
        reg.WriteString('inimapping', '0');

        // Windows >= Vista?
        if TOSUtils.WindowsVistaOrLater() then
          // Save deactivation time stamp
          WriteTime(KEY_DEACT + FName);

        // Update information
        FRootKey := 'HKLM';
        FKeyPath := KEY_DEACT + FName;
        FEnabled := False;
      end  //of begin
      else
        raise EStartupException.Create('Could not create "'+ KEY_DEACT + FName +'"!');

      Result := True;

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while disabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupItem.Enable

  Enables an TStartupItem object and returns True if successful. }

function TStartupItem.Enable(): Boolean;
var
  reg: TRegistry;
  NewHKey, NewKeyPath: string;

begin
  Result := False;
  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_ALL_ACCESS));

  try
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;

      if not reg.OpenKey(FKeyPath, False) then
        raise EStartupException.Create('Key does not exist!');

      // Set new values
      NewHKey := reg.ReadString('hkey');
      NewKeyPath := reg.ReadString('key');
      reg.CloseKey;

      if ((NewHKey = '') or (NewKeyPath = '')) then
        raise EStartupException.Create('Missing destination Registry values '
          +'"hkey" and "key"!');

      reg.RootKey := TRegUtils.StrToHKey(NewHKey);

      // Successfully created new key?
      if reg.OpenKey(NewKeyPath, True) then
      begin
        // Write startup entry
        reg.WriteString(FName, FFilePath);

        // Delete old key
        Result := TRegUtils.DeleteKey('HKLM', KEY_DEACT, FName);

        // Update information
        FRootKey := NewHKey;
        FKeyPath := NewKeyPath;
        FEnabled := True;
        FTime := '';
      end  //of begin
      else
        raise EStartupException.Create('Could not create key "'+ NewKeyPath +'"!');

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
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

{ public TStartupUserItem.ChangeFilePath

  Changes the file path of a TStartupUserItem item. }

function TStartupUserItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  NewFilePath, Arguments: string;

begin
  // Must be outside try-except because inherited call catches exception already
  if not FEnabled then
    inherited ChangeFilePath(ANewFilePath);

  try
    NewFilePath := StringReplace(ExtractPathToFile(ANewFilePath), '"', '', [rfReplaceAll]);
    Arguments := ExtractArguments(ANewFilePath);

    // Failed to create new .lnk file?
    if (FEnabled and not FLnkFile.WriteLnkFile(FKeyPath, NewFilePath, Arguments)) then
      raise EStartupException.Create('Could not create .lnk file!');

    // Update information
    FFilePath := ANewFilePath;
    FLnkFile.ExeFileName := NewFilePath;
    FLnkFile.Arguments := Arguments;

    // Rewrite backup
    if (not FEnabled and FLnkFile.BackupExists()) then
      if not FLnkFile.CreateBackup() then
        raise EStartupException.Create('Backup could not be created!');

    Result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while changing file path of "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupUserItem.Delete

  Deletes a TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Delete(): Boolean;

  function GetKeyName(): string;
  begin
    Result := AddCircumflex(TLnkFile.GetStartUpDir(AnsiContainsText('Common', FType)) + FName);
  end;

begin
  try
    if FEnabled then
    begin
      // Could not delete .lnk?
      if not DeleteFile(FKeyPath) then
        raise EStartupException.Create('Could not delete .lnk "'+ FKeyPath +'"!');
    end  //of begin
    else
      // Could not delete key?
      if not TRegUtils.DeleteKey('HKLM', KEY_DEACT_FOLDER, GetKeyName()) then
        raise EStartupException.Create('Could not delete key "'+ FKeyPath +'"!');

    Result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while deleting "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupUserItem.Disable

  Disables an TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Disable(): Boolean;
var
  reg: TRegistry;
  KeyName: string;

begin
  Result := False;
  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_WRITE));

  try
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      KeyName := AddCircumflex(FKeyPath);

      if not FLnkFile.ReadLnkFile() then
        raise EStartupException.Create('Could not read .lnk file!');

      if not reg.OpenKey(KEY_DEACT_FOLDER + KeyName, True) then
        raise EStartupException.Create('Could not create key "'+ FName +'"!');

      reg.WriteString('path', FKeyPath);
      reg.WriteString('item', TRegUtils.DeleteExt(ExtractFileName(FName)));
      reg.WriteString('command', FFilePath);
      reg.WriteString('backup', FLnkFile.BackupLnk);

      // Special Registry entries only for Windows >= Vista
      if TOSUtils.WindowsVistaOrLater() then
      begin
        reg.WriteString('backupExtension', FLnkFile.BackupExt);
        reg.WriteString('location', ExtractFileDir(FKeyPath));
        WriteTime(KEY_DEACT_FOLDER + KeyName);
      end  //of begin
      else
        reg.WriteString('location', FType);

      // Create backup directory if not exist
      if not DirectoryExists(TLnkFile.GetBackupDir()) then
        ForceDirectories(TLnkFile.GetBackupDir());

      // Create backup by copying original .lnk
      if not CopyFile(PChar(FKeyPath), PChar(FLnkFile.BackupLnk), False) then
        raise EStartupException.Create('Could not create backup file!');

      // Delete original .lnk
      if not FLnkFile.Delete() then
        raise EStartupException.Create('Could not delete .lnk file!');

      // Update information
      FKeyPath := KEY_DEACT_FOLDER + KeyName;
      FRootKey := 'HKLM';
      FEnabled := False;
      Result := True;

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while disabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupUserItem.Enable

  Enables an TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Enable(): Boolean;
begin
  try
    // Backup file exists?
    if FLnkFile.BackupExists() then
    begin
      // Failed to restore backup file?
      if not CopyFile(PChar(FLnkFile.BackupLnk), PChar(FLnkFile.FileName), True) then
        raise EStartupException.Create('Could not create .lnk file!');
    end  //of begin
    else
      begin
        // Failed to create new .lnk file?
        if not FLnkFile.WriteLnkFile(FLnkFile.FileName, GetFilePath(), GetArguments()) then
          raise EStartupException.Create('Could not create .lnk file!');
      end;  //of if

    // Could not delete old key?
    if not TRegUtils.DeleteKey('HKLM', KEY_DEACT_FOLDER, AddCircumflex(FLnkFile.FileName)) then
      raise EStartupException.Create('Could not delete key "'+ FKeyPath +'"!');

    // Update information
    FKeyPath := FLnkFile.FileName;
    FRootKey := '';
    FEnabled := True;
    Result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
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

{ public TStartupUserItem.GetFullKeyPath

  Returns the file path of a TStartupUserItem. }

function TStartupUserItem.GetFullKeyPath(): string;
begin
  if FEnabled then
    Result := FKeyPath
  else
    Result := inherited GetFullKeyPath()
end;


{ TStartupList }

{ public TStartupList.Create

  General constructor for creating a TStartupList instance. }

constructor TStartupList.Create;
begin
  inherited Create;
  FDeleteBackup := True;
  FLock := TCriticalSection.Create;
end;

{ public TStartupList.Destroy

  Destructor for destroying a TStartupList instance. }

destructor TStartupList.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

{ private TStartupList.Add

  Adds a TStartupListItem to the list and returns position. }

function TStartupList.Add(AItem: TStartupListItem): Word;
begin
  Result := inherited Add(AItem);
end;

{ private TStartupList.DelCircumflex

  Replaces all circumflexes by backslashes. }

function TStartupList.DelCircumflex(AName: string): string;
begin
  Result := StringReplace(AName, '^', '\', [rfReplaceAll]);
end;

{ private TStartupList.ItemAt

  Returns a TStartupListItem object at index. }

function TStartupList.ItemAt(AIndex: Word): TStartupListItem;
begin
  Result := TStartupListItem(RootItemAt(AIndex));
end;

{ private TStartupList.GetLnkFileNames

  Returns a list containing all .lnk files. The boolean argument decides to
  find .lnk autostart files of current user or all users. }

procedure TStartupList.GetLnkFileNames(AFileList: TStrings; AAllUsers: Boolean);
var
  SearchResult: TSearchRec;
  RootFolder: string;

begin
  RootFolder := TLnkFile.GetStartUpDir(AAllUsers);

  if (FindFirst(RootFolder + '*.lnk', faAnyFile, SearchResult) = 0) then
    try
      repeat
        // File found?
        if (SearchResult.Attr <> faDirectory) then
          AFileList.Add(RootFolder + SearchResult.Name);
      until FindNext(SearchResult) <> 0;

    finally
      FindClose(SearchResult);
    end;  //of try
end;

{ private TStartupList.GetStartupUserType

  Returns the startup item type. }

function TStartupList.GetStartupUserType(const AKeyPath: string): string;
var
  st: string;

begin
  // Windows >= Vista?
  if TOSUtils.WindowsVistaOrLater() then
  begin
    st := TRegUtils.GetKeyValue('HKLM', AKeyPath, 'backupExtension');

    if (st = EXT_COMMON) then
      Result := TYPE_COMMON
    else
      Result := TYPE_USER;
  end  //of begin
  else
    Result := TRegUtils.GetKeyValue('HKLM', AKeyPath, 'location');
end;

{ private TStartupList.GetStartupUserType

  Returns the startup item type. }

function TStartupList.GetStartupUserType(const AAllUsers: Boolean): string;
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

function TStartupList.AddItemDisabled(const AKeyPath: string): Word;
var
  Item: TStartupListItem;

begin
  Item := TStartupItem.Create(Count, False);

  with Item do
  begin
    RootKey := 'HKLM';
    KeyPath := AKeyPath;
    Name := ExtractFileName(AKeyPath);
    FilePath := TRegUtils.GetKeyValue('HKLM', AKeyPath, 'command');
    Time := GetTime();
    TypeOf := TRegUtils.GetKeyValue('HKLM', AKeyPath, 'hkey');
  end;  //of with

  Result := Add(Item);
end;

{ protected TStartupList.AddItemEnabled

  Adds a enabled default startup item to the list. }

function TStartupList.AddItemEnabled(const ARootKey, AKeyPath, AName,
  AFilePath: string): Word;
var
  Item: TStartupListItem;

begin
  Item := TStartupItem.Create(Count, True);

  with Item do
  begin
    RootKey := ARootKey;
    KeyPath := AKeyPath;
    Name := AName;
    FilePath := AFilePath;
    Time := '';

    if ((AKeyPath = KEY_RUNONCE) or (AKeyPath = KEY_RUNONCE32)) then
      TypeOf := 'RunOnce'
    else
      TypeOf := ARootKey;
  end;  //of with

  Inc(FActCount);
  Result := Add(Item);
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

  AddUserItemEnabled(LnkFile, AAllUsers);
  Result := True;
end;

{ protected TStartupList.AddUserItemDisabled

  Adds a disabled startup user item to the list. }

function TStartupList.AddUserItemDisabled(const AKeyPath: string): Word;
var
  Item: TStartupListItem;
  Path, Ext: string;

begin
  Item := TStartupUserItem.Create(Count, False);
  Path := TRegUtils.GetKeyValue('HKLM', AKeyPath, 'path');

  with (Item as TStartupUserItem) do
  begin
    RootKey := 'HKLM';
    KeyPath := AKeyPath;
    Name := ExtractFileName(DelCircumflex(AKeyPath));
    FilePath := TRegUtils.GetKeyValue('HKLM', AKeyPath, 'command');
    Time := GetTime();
    TypeOf := GetStartupUserType(AKeyPath);

    if ((TypeOf = TYPE_USER) or (TypeOf = TYPE_USER_XP)) then
      Ext := EXT_USER
    else
      Ext := EXT_COMMON;

    LnkFile := TLnkFile.Create(Path, Ext);
    LnkFile.ReadLnkFile();
  end;  //of with

  Result := Add(Item);
end;

{ protected TStartupList.AddUserItemEnabled

  Adds a enabled startup user item to the list. }

function TStartupList.AddUserItemEnabled(ALnkFile: TLnkFile;
  AAllUsers: Boolean): Word;
var
  Item: TStartupListItem;

begin
  Item := TStartupUserItem.Create(Count, True);

  // Read .lnk file
  ALnkFile.ReadLnkFile();

  with (Item as TStartupUserItem) do
  begin
    RootKey := '';
    KeyPath := ALnkFile.FileName;
    FilePath := ALnkFile.FullPath;
    LnkFile := ALnkFile;
    Name := ExtractFileName(ALnkFile.FileName);
    Time := '';
    TypeOf := GetStartupUserType(AAllUsers);
  end;  //of with

  Inc(FActCount);
  Result := Add(Item);
end;

{ public TStartupList.AddProgram

  Adds a new startup item to autostart. }

function TStartupList.AddProgram(AFilePath, AArguments: string;
  ADisplayedName: string = ''): Boolean;
var
  Name, Ext: string;
  i: Word;

begin
  Result := False;
  Name := ExtractFileName(AFilePath);
  Ext := ExtractFileExt(Name);

  // Check invalid extension
  if ((Ext <> '.exe') and (Ext <> '.bat')) then
    raise EInvalidArgument.Create('Invalid program extension! Must be ".exe"'
      +' or ".bat"!');

  // File path already exists in another item?
  for i := 0 to Count -1 do
    if AnsiContainsStr(ItemAt(i).FilePath, AFilePath) then
      Exit;

  // Add new startup user item?
  if (Ext = '.exe') then
  begin
    if (ADisplayedName <> '') then
      Name := ADisplayedName
    else
      Name := TRegUtils.DeleteExt(Name);

    Result := AddNewStartupUserItem(Name, AFilePath, AArguments);
  end  //of begin
  else
    begin
      // Append arguments if used
      if (AArguments <> '') then
        AFilePath := AFilePath +' '+ AArguments;

      // Adds new startup item to Registry
      TRegUtils.WriteStrValue('HKCU', KEY_STARTUP, ADisplayedName, AFilePath);

      // Adds this item to list
      AddItemEnabled('HKCU', KEY_STARTUP, ADisplayedName, AFilePath);
      Result := True;
    end;  //of begin
end;

{ public TStartupList.BackupExists

  Checks if a backup file already exists. }

function TStartupList.BackupExists(): Boolean;
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if (FItem is TStartupUserItem) then
    Result := (FItem as TStartupUserItem).LnkFile.BackupExists()
  else
    Result := False;
end;

{ public TStartupList.ChangeItemFilePath

  Changes the file path of an item. }

function TStartupList.ChangeItemFilePath(const ANewFilePath: string): Boolean;
begin
  // Invalid item?
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  Result := FItem.ChangeFilePath(ANewFilePath);
end;

{ public TStartupList.ChangeItemStatus

  Changes the item status. }

function TStartupList.ChangeItemStatus(): Boolean;
var
  Changed: Boolean;

begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  // Change the status
  Changed := FItem.ChangeStatus();

  // Successful?
  if Changed then
    // Item has been enabled?
    if FItem.Enabled then
    begin
      // Decide to delete backup
      if (FDeleteBackup and (FItem is TStartupUserItem)) then
        (FItem as TStartupUserItem).LnkFile.DeleteBackup();

      // Update active counter
      Inc(FActCount);
    end  //of begin
  else
    Dec(FActCount);

  Result := Changed;
end;

{ public TStartupList.DisableItem

  Disables the current selected item. }

function TStartupList.DisableItem(): Boolean;
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if not FItem.Enabled then
    raise EAbort.Create('Item already disabled!');

  Result := FItem.Disable();

  if Result then
    Dec(FActCount);
end;

{ public TStartupList.EnableItem

  Enables the current selected item. }

function TStartupList.EnableItem(): Boolean;
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if FItem.Enabled then
    raise EAbort.Create('Item already enabled!');

  Result := FItem.Enable();

  if Result then
  begin
    // Decide to delete backup
    if (FDeleteBackup and (FItem is TStartupUserItem)) then
      (FItem as TStartupUserItem).LnkFile.DeleteBackup();

    // Update active counter
    Inc(FActCount);
  end;  //of begin
end;

{ public TStartupList.DeleteItem

  Deletes an item from Registry and list. }

function TStartupList.DeleteItem(): Boolean;
var
  Deleted: Boolean;

begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  // Delete item from Registry
  Deleted := FItem.Delete();

  // Successful?
  if Deleted then
  begin
    // Decide to delete backup
    if (FDeleteBackup and (FItem is TStartupUserItem)) then
      (FItem as TStartupUserItem).LnkFile.DeleteBackup();

    // Item was enabled
    if FItem.Enabled then
      // Update active counter
      Dec(FActCount);

    // Remove item from list
    inherited Remove(FItem);
    FItem := nil;
  end;  //of begin

  Result := Deleted;
end;

{ public TStartupList.ExportItem

  Exports an item as .reg file. }

procedure TStartupList.ExportItem(const AFileName: string);
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  FItem.ExportItem(AFileName);
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
      RegFile.ExportKey(TOSUtils.StrToHKey(Item.RootKey), Item.KeyPath, True);
    end;  //of for

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
  end;  //of try
end;

{ public TStartupList.ImportBackup

  Imports a startup user backup file and adds it to the list. }

function TStartupList.ImportBackup(const AFilePath: string): Boolean;
var
  Name, Ext: string;
  i: Integer;
  LnkFile: TLnkFile;

begin
  Result := False;
  Ext := ExtractFileExt(AFilePath);

  // Check invalid extension
  if ((Ext <> EXT_COMMON) and (Ext <> EXT_USER)) then
    raise EInvalidArgument.Create('Invalid backup file extension! Must be "'
      + EXT_COMMON +'" or "'+ EXT_USER+'"!');

  // Init new .lnk file
  LnkFile := TLnkFile.Create(AFilePath, Ext);

  // Set the name of item
  Name := ExtractFileName(TRegUtils.DeleteExt(AFilePath));

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

{ public TStartupList.LoadAutostart

  Searches for startup items at different locations. }

procedure TStartupList.LoadAutostart(AIncludeRunOnce: Boolean);
var
  StartupSearchThread: TStartupSearchThread;

begin
  // Init search thread
  StartupSearchThread := TStartupSearchThread.Create(Self,
    TOSUtils.IsWindows64(), AIncludeRunOnce, FLock);

  with StartupSearchThread do
  begin
    OnStart := FOnSearchStart;
    OnSearching := FOnSearching;
    OnFinish := FOnSearchFinish;
    Resume;
  end;  // of with
end;

{ public TStartupList.LoadDisabled

  Searches for disabled items in AKeyPath and adds them to the list. }

procedure TStartupList.LoadDisabled(const AKeyPath: string);
var
  reg: TRegistry;
  List: TStringList;
  i: Integer;

begin
  List := TStringList.Create;
  reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));
  reg.RootKey := HKEY_LOCAL_MACHINE;

  try
    reg.OpenKey(AKeyPath, True);
    reg.GetKeyNames(List);

    for i := 0 to List.Count -1 do
      if (AKeyPath = KEY_DEACT) then
        AddItemDisabled(AKeyPath + List[i])
      else
        AddUserItemDisabled(AKeyPath + List[i]);

  finally
    reg.Free;
    List.Free;
  end;  //of finally
end;

{ public TStartupList.LoadEnabled

  Searches for enabled startup user items and adds them to the list. }

procedure TStartupList.LoadEnabled(const AAllUsers: Boolean);
var
  List: TStringList;
  LnkFile: TLnkFile;
  i: integer;

begin
  List := TStringList.Create;

  try
    // Retrieve a list containing all activated startup user .lnk files
    GetLnkFileNames(List, AAllUsers);

    // Add every file to list
    for i := 0 to List.Count - 1 do
    begin
      LnkFile := TLnkFile.Create(ExtractFileName(List[i]), AAllUsers);
      AddUserItemEnabled(LnkFile, AAllUsers);
    end;  //of begin

  finally
    List.Free;
  end;  //of finally
end;

{ public TStartupList.LoadEnabled

  Searches for enabled items in ARootKey and AKeyPath and adds them to the list. }

procedure TStartupList.LoadEnabled(const ARootKey, AKeyPath: string);
var
  reg: TRegistry;
  List: TStringList;
  i: Integer;

begin
  List := TStringList.Create;
  reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));

  try
    reg.RootKey := TOSUtils.StrToHKey(ARootKey);
    reg.OpenKey(AKeyPath, True);
    reg.GetValueNames(List);

    for i := 0 to List.Count - 1 do
      // Read path to .exe and add item to list
      AddItemEnabled(ARootKey, AKeyPath, List[i], reg.ReadString(List[i]));

  finally
    reg.Free;
    List.Free;
  end;  //of finally
end;


{ TContextItem }

{ public TContextListItem.Delete

  Deletes a TContextListItem object and returns True if successful. }

function TContextListItem.Delete(): Boolean;
begin
  try
    if not TRegUtils.DeleteKey('HKCR', ExtractFileDir(KeyPath), FName) then
      raise EStartupException.Create('Could not delete key!')
    else
      Result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while deleting "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TContextListItem.ExportItem

  Exports an list item as .reg file. }

procedure TContextListItem.ExportItem(const AFileName: string);
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

{ public TContextListItem.GetFullKeyPath

  Returns the Registry path to a TStartupListItem. }

function TContextListItem.GetFullKeyPath(): string;
begin
  Result := TRegUtils.HKeyToStr(HKEY_CLASSES_ROOT) +'\'+ GetKeyPath();
end;


{ TShellItem }

{ private TShellItem.GetKeyPath

  Returns the Registry path of a TShellItem. }

function TShellItem.GetKeyPath(): string;
begin
  Result := FLocation +'\'+ FType +'\'+ FName;
end;

{ public TShellItem.ChangeFilePath

  Changes the file path of an TShellItem item. }

function TShellItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  reg: TRegistry;

begin
  Result := False;
  reg := TRegistry.Create(TWinWOW64.DenyWOW64Redirection(KEY_ALL_ACCESS));

  try
    try
      reg.RootKey := HKEY_CLASSES_ROOT;

      // Invalid key?
      if not reg.OpenKey(GetKeyPath() +'\command', False) then
        raise Exception.Create('Key does not exist!');

      // Change path
      reg.WriteString('', ANewFilePath);
      Result := True;

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while changing file path of "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TShellItem.Disable

  Disables a TShellItem object and returns True if successful. }

function TShellItem.Disable(): Boolean;
var
  reg: TRegistry;

begin
  Result := False;
  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    try
      reg.RootKey := HKEY_CLASSES_ROOT;

      // Key does not exist?
      if not reg.OpenKey(GetKeyPath(), False) then
        raise EContextMenuException.Create('Key does not exist!');

      reg.WriteString('LegacyDisable', '');

      // Update status
      FEnabled := False;
      Result := True;

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while disabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;

{ public TShellItem.Enable

  Enables a TShellItem object and returns True if successful. }

function TShellItem.Enable(): Boolean;
begin
  try
    if not TRegUtils.DeleteValue('HKCR', GetKeyPath(), 'LegacyDisable') then
      raise EContextMenuException.Create('Could not delete value!');

    // Update status
    FEnabled := True;
    Result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;


{ TShellExItem }

{ private TShellExItem.GetKeyPath

  Returns the Registry path to a TShellExItem. }

function TShellExItem.GetKeyPath(): string;
begin
  Result := FLocation + CONTEXTMENU_SHELLEX +'\'+ FName;
end;

{ private TShellExItem.GetProgramPathKey

  Returns the Registry key of the correspondending program. }

function TShellExItem.GetProgramPathKey(): string;
var
  GUID: string;

begin
  GUID := TRegUtils.GetKeyValue('HKCR', GetKeyPath(), '');
  Result := Format(CONTEXTMENU_SHELLEX_FILE, [GUID]);
end;

{ public TShellExItem.ChangeFilePath

  Changes the file path of an TShellExItem item. }

function TShellExItem.ChangeFilePath(const ANewFilePath: string): Boolean;
begin
  TRegUtils.WriteStrValue('HKCR', GetProgramPathKey(), '', ANewFilePath);
  Result := True;
end;

{ public TShellExItem.Disable

  Disables a TShellExItem object and returns True if successful. }

function TShellExItem.Disable(): Boolean;
var
  reg: TRegistry;
  OldValue, NewValue: string;

begin
  Result := False;
  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    try
      reg.RootKey := HKEY_CLASSES_ROOT;

      // Key does not exist?
      if not reg.OpenKey(GetKeyPath(), False) then
        raise EContextMenuException.Create('Key does not exist!');

      // Value does not exist?
      if not reg.ValueExists('') then
        raise EContextMenuException.Create('Value does not exist!');

      OldValue := reg.ReadString('');

      if (Trim(OldValue) = '') then
        raise EContextMenuException.Create('Value must not be empty!');

      // Item enabled?
      if (OldValue[1] = '{') then
      begin
        // Set up new value and write it
        NewValue := '-'+ OldValue;
        reg.WriteString('', NewValue);
      end;  //of begin

      // Update status
      FEnabled := False;
      Result := True;

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while disabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;

{ public TShellExItem.Enable

  Enables a TShellExItem object and returns True if successful. }

function TShellExItem.Enable(): Boolean;
var
  reg: TRegistry;
  OldValue, NewValue: string;

begin
  Result := False;
  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    try
      reg.RootKey := HKEY_CLASSES_ROOT;

      // Key does not exist?
      if not reg.OpenKey(GetKeyPath(), False) then
        raise EContextMenuException.Create('Key does not exist!');

      // Value does not exist?
      if not reg.ValueExists('') then
        raise EContextMenuException.Create('Value does not exist!');

      OldValue := reg.ReadString('');

      if (Trim(OldValue) = '') then
        raise EContextMenuException.Create('Value must not be empty!');

      // Item disabled?
      if (OldValue[1] <> '{') then
      begin
        // Set up new value and write it
        NewValue := Copy(OldValue, 2, Length(OldValue));
        reg.WriteString('', NewValue);
      end;  //of begin

      // Update status
      FEnabled := True;
      Result := True;

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;


{ TContextList }

{ public TContextList.Create

  General constructor for creating a TContextList instance. }

constructor TContextList.Create;
begin
  inherited Create;
  FActCount := 0;
  FLock := TCriticalSection.Create;
end;

{ public TContextList.Destroy

  Destructor for destroying a TContextList instance. }

destructor TContextList.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

{ private TContextList.Add

  Adds a TContextListItem to the list and returns position. }

function TContextList.Add(AItem: TContextListItem): Word;
begin
  Result := inherited Add(AItem);
end;

{ public TContextList.AddEntry

  Adds a new contextmenu entry. }

function TContextList.AddEntry(const AFilePath, AArguments, ALocation,
  ADisplayedName: string): Boolean;
var
  Name, Ext, FullPath: string;

begin
  Result := False;
  Ext := ExtractFileExt(AFilePath);
  Name := TRegUtils.DeleteExt(ExtractFileName(AFilePath));

  // Check invalid extension
  if ((Ext <> '.exe') and (Ext <> '.bat')) then
    raise EInvalidArgument.Create('Invalid program extension! Must be ".exe"'
      +' or ".bat"!');

  // File path already exists in another item?
  if (IndexOf(Name, ALocation) <> -1) then
    Exit;

  // Escape space char using quotes
  FullPath := '"'+ AFilePath +'"';

  // Append arguments if used
  if (AArguments <> '') then
    FullPath := FullPath +' '+ AArguments;

  // Adds new context item to Registry
  TRegUtils.WriteStrValue('HKCR', ALocation + CONTEXTMENU_SHELL +'\'+ Name, '', ADisplayedName);
  TRegUtils.WriteStrValue('HKCR', ALocation + CONTEXTMENU_SHELL + Name +'\command', '', FullPath);

  // Adds this item to list
  AddShellItem(Name, ALocation, FullPath, ADisplayedName, True);
  Result := True;
end;

{ private TContextList.FindDouble

  Checks if an TContextListItem already exists in list. }

function TContextList.FindDouble(AName, ALocation: string): Boolean;
begin
  Result := False;

  if ((Count > 0) and (IndexOf(AName) <> -1)) then
    Result := (ItemAt(IndexOf(AName)).Location = ALocation);
end;

{ private TContextList.ItemAt

  Returns a TContextListItem object at index. }

function TContextList.ItemAt(AIndex: Word): TContextListItem;
begin
  Result := TContextListItem(RootItemAt(AIndex));
end;

{ protected TContextList.AddShellItem

  Adds a shell item to list. }

function TContextList.AddShellItem(const AName, ALocation, AFilePath,
  ACaption: string; AEnabled: Boolean): Word;
var
  Item: TContextListItem;

begin
  Item := TShellItem.Create(Count, AEnabled);

  with Item do
  begin
    Name := AName;
    Location := ALocation;
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

function TContextList.AddShellExItem(const AName, ALocation, AFilePath: string;
  AEnabled: Boolean): Word;
var
  Item: TContextListItem;

begin
  Item := TShellExItem.Create(Count, AEnabled);

  with Item do
  begin
    Name := AName;
    Location := ALocation;
    FilePath := AFilePath;
    TypeOf := 'ShellEx';

    if AEnabled then
      Inc(FActCount);
  end;  //of with

  Result := Add(Item);
end;

{ protected TContextList.LoadContextmenu

  Searches for either Shell or ShellEx context menu entries in specific
  Registry key and adds them to the list. }

procedure TContextList.LoadContextmenu(const ALocation: string;
  ASearchForShellItems: Boolean);
var
  reg: TRegistry;
  i: Integer;
  List: TStringList;
  Item, Key, KeyName, FilePath, GUID, Caption: string;
  Enabled: Boolean;

begin
  reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));
  List := TStringList.Create;

  if ASearchForShellItems then
    Key := ALocation + CONTEXTMENU_SHELL
  else
    Key := ALocation + CONTEXTMENU_SHELLEX;

  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.OpenKey(Key, False);

    // Read out all keys
    reg.GetKeyNames(List);

    for i := 0 to List.Count -1 do
    begin
      reg.CloseKey;
      Item := List[i];
      reg.OpenKey(Key +'\'+ Item, False);
      FilePath := '';

      // Default value of key is a GUID for ShellEx items and caption for Shell
      GUID := reg.ReadString('');
      Caption := GUID;

      // Filter empty, important and double entries
      if ((Caption <> '') and (Item[1] <> '{') and (GUID[1] <> '@')
        and not FindDouble(Item, ALocation)) then
      begin
        // Search for shell entries?
        if ASearchForShellItems then
        begin
          // Get status and caption
          Enabled := not reg.ValueExists('LegacyDisable');

          // Get file path of command
          if reg.OpenKey('command', False) then
            FilePath := reg.ReadString('');

          // Add item to list
          AddShellItem(Item, ALocation, FilePath, Caption, Enabled);
        end  //of begin
        else
          // Search for shellex entries
          begin
            // Get status and GUID
            Enabled := (GUID[1] = '{');

            // Disabled ShellEx items got "-" before GUID!
            if not Enabled then
              GUID := Copy(GUID, 2, Length(GUID));

            // Set up Registry key
            KeyName := Format(CONTEXTMENU_SHELLEX_FILE, [GUID]);
            reg.CloseKey();

            // Get file path of command
            if reg.OpenKey(KeyName, False) then
              FilePath := reg.ReadString('');

            // Add item to list
            AddShellExItem(Item, ALocation, FilePath, Enabled);
          end  //of begin
        end;  //of begin
    end;  //of for

  finally
    List.Free;
    reg.Free;
  end;  //of try
end;

{ public TContextList.ChangeItemFilePath

  Changes the file path of an item. }

function TContextList.ChangeItemFilePath(const ANewFilePath: string): Boolean;
begin
  // Invalid item?
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  Result := FItem.ChangeFilePath(ANewFilePath);
end;

{ public TContextList.ChangeItemStatus

  Changes the item status. }

function TContextList.ChangeItemStatus(): Boolean;
var
  Changed: Boolean;

begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  // Change status of item
  Changed := FItem.ChangeStatus;

  // Successful?
  if Changed then
    // Item has been enabled?
    if FItem.Enabled then
      Inc(FActCount)
    else
      Dec(FActCount);

  Result := Changed;
end;

{ public TContextList.DeleteItem

  Deletes an item from Registry and list. }

function TContextList.DeleteItem(): Boolean;
var
  Deleted: Boolean;

begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  // Delete item from Registry
  Deleted := FItem.Delete();

  // Successful?
  if Deleted then
  begin
    // Item was enabled?
    if FItem.Enabled then
      Dec(FActCount);

    // Remove item from list
    inherited Remove(FItem);
    FItem := nil;
  end;  //of begin

  Result := Deleted;
end;

{ public TContextList.DisableItem

  Disables the current selected item. }

function TContextList.DisableItem(): Boolean;
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if not FItem.Enabled then
    raise EAbort.Create('Item already disabled!');

  Result := FItem.Disable();

  if Result then
    Dec(FActCount);
end;

{ public TContextList.EnableItem

  Enables the current selected item. }

function TContextList.EnableItem(): Boolean;
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if FItem.Enabled then
    raise EAbort.Create('Item already enabled!');

  Result := FItem.Enable();

  if Result then
    Inc(FActCount);
end;

{ public TContextList.ExportItem

  Exports an item as .reg file. }

procedure TContextList.ExportItem(const AFileName: string);
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  FItem.ExportItem(AFileName);
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
    for i := 0 to Count -1 do
    begin
      Item := ItemAt(i);
      RegFile.ExportKey(HKEY_CLASSES_ROOT, Item.KeyPath, True);
    end;  //of for

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
  end;  //of try
end;

{ public TContextList.IndexOf

  Returns the index of an item checking name and location. }

function TContextList.IndexOf(AName, ALocation: string): Integer;
var
  i: Integer;
  Item: TContextListItem;

begin
  Result := -1;

  for i := 0 to Count -1 do
  begin
    Item := ItemAt(i);

    if (((Item.Name = AName) or (Item.Caption = AName))
      and (Item.Location = ALocation)) then
    begin
      Result := i;
      Break;
    end;  //of begin
  end;  //of for
end;

{ public TContextList.LoadContextmenu

  Searches for context menu entries and adds them to the list. }

procedure TContextList.LoadContextmenu();
begin
  LoadContextMenus('');
end;

{ public TContextList.LoadContextmenu

  Searches for Shell/ShellEx context menu entries in specific Registry key
  and adds them to the list. }

procedure TContextList.LoadContextmenu(const ALocation: string);
begin
  LoadContextmenu(ALocation, True);
  LoadContextmenu(ALocation, False);
end;

{ public TContextList.LoadContextMenus

  Searches for context menu entries at different locations. }

procedure TContextList.LoadContextMenus(ALocations: string = 'Directory, Folder, *, Drive');
var
  SearchThread: TContextSearchThread;

begin
  // Init search thread
  SearchThread := TContextSearchThread.Create(Self, FLock);

  with SearchThread do
  begin
    Locations.CommaText := ALocations;
    OnStart := FOnSearchStart;
    OnSearching := FOnSearching;
    OnFinish := FOnSearchFinish;
    Resume;
  end;  // of with
end;

end.
