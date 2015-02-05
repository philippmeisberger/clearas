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
  Contnrs, OSUtils, LanguageFile, IniFileParser;

const
  { Registry keys }
  KEY_CONTEXTMENU = '\shellex\ContextMenuHandlers';
  KEY_DEACT = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupreg\';
  KEY_DEACT_FOLDER = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupfolder\';
  KEY_RECYCLEBIN = 'CLSID\{645FF040-5081-101B-9F08-00AA002F954E}\shell';
  KEY_RUNONCE = 'SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce';
  KEY_RUNONCE32 = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce';
  KEY_STARTUP = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP32 = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';
  KEY_REGEDIT = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Applets\Regedit';

  { Extensions of backup files }
  EXT_COMMON = '.CommonStartup';
  EXT_USER = '.Startup';

  TYPE_COMMON = 'Common Startup';
  TYPE_USER = 'Startup User';

type
  { TLnkFile }
  TLnkFile = class(TObject)
  private
    FFileName, FExeFileName, FArguments, FBackupExt: string;
    function GetBackupLnk(): string;
    function GetFullPath(): string;
    function GetFullPathEscaped(): string;
  public
    constructor Create(AFileName: string); overload;
    constructor Create(AName: string; AAllUsers: Boolean); overload;
    constructor Create(AFileName, AExeFileName: string; AArguments: string = ''); overload;
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
    FName, FType: string;
  protected
    function Disable(): Boolean; virtual; abstract;
    function Enable(): Boolean; virtual; abstract;
  public
    constructor Create(AIndex: Word; AEnabled: Boolean);
    function ChangeFilePath(const ANewFilePath: string): Boolean; virtual; abstract;
    function ChangeStatus(): Boolean; virtual;
    function Delete(): Boolean; virtual; abstract;
    procedure ExportItem(const AFileName: string); virtual; abstract;
    function GetFullKeyPath(): string; virtual; abstract;
    procedure GetItemInfo(var AProperties: string; ALangFile: TLanguageFile); virtual; abstract;
    function GetStatus(ALangFile: TLanguageFile): string;
    procedure Open(); virtual;
    { external }
    property Enabled: Boolean read FEnabled;
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
    destructor Destroy; override;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean; virtual; abstract;
    function ChangeItemStatus(): Boolean; virtual; abstract;
    procedure Clear; override;
    function DeleteItem(): Boolean; virtual; abstract;
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
    FRootKey, FFilePath, FKeyPath, FTime: string;
    function GetTime(): string;
    function StartupUserType(): Boolean; virtual; abstract;
    procedure WriteTime(const AKeyPath: string);
  public
    function CreateBackup(): Boolean; virtual; abstract;
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    function GetFullKeyPath(): string; override;
    { external }
    property FilePath: string read FFilePath write FFilePath;
    property KeyPath: string read FKeyPath write FKeyPath;
    property RootKey: string read FRootKey write FRootKey;
    property StartupUser: Boolean read StartupUserType;
    property Time: string read FTime write FTime;
  end;

  { TStartupItem }
  TStartupItem = class(TStartupListItem)
  private
    function StartupUserType(): Boolean; override;
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  public
    function CreateBackup(): Boolean; override;
    function Delete(): Boolean; override;
    procedure GetItemInfo(var AProperties: string; ALangFile: TLanguageFile); override;
  end;

  { TStartupUserItem }
  TStartupUserItem = class(TStartupListItem)
  private
    FLnkFile: TLnkFile;
    function AddCircumflex(const AName: string): string;
    function GetExtension(): string;
    function StartupUserType(): Boolean; override;
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    function CreateBackup(): Boolean; override;
    function Delete(): Boolean; override;
    function GetBackupLnk(): string;
    procedure GetItemInfo(var AProperties: string; ALangFile: TLanguageFile); override;
    procedure Open(); override;
  end;

  { TStartupList }
  TStartupList = class(TRootList)
  private
    FItem: TStartupListItem;
    FDeleteBackup: Boolean;
    function Add(AItem: TStartupListItem): Word; virtual;
    function DelCircumflex(AName: string): string;
    function DeleteBackupLnk(): Boolean;
    function ItemAt(AIndex: Word): TStartupListItem;
    procedure GetLnkFileNames(AFileList: TStrings; AAllUsers: Boolean);
    function GetStartupUserType(const AKeyPath: string): string;
  protected
    function AddItemDisabled(const AKeyPath: string): Word;
    function AddItemEnabled(const ARootKey, AKeyPath, AName, AFilePath: string): Word;
    function AddNewStartupUserItem(AName, AFilePath: string;
      AArguments: string = ''; AAllUsers: Boolean = False): Boolean;
    function AddUserItemDisabled(const AKeyPath: string): Word;
    function AddUserItemEnabled(const ALnkFile: string; AAllUsers: Boolean): Word;
  public
    constructor Create;
    function CreateBackup(): Boolean;
    function AddProgram(AFilePath, AArguments: string;
      ADisplayedName: string = ''): Boolean;
    function BackupExists(): Boolean;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean; override;
    function ChangeItemStatus(): Boolean; override;
    function DeleteItem(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    procedure ExportList(const AFileName: string); override;
    function GetBackupLnk(): string;
    function ImportBackup(const AFilePath: string): Boolean;
    procedure LoadAutostart(AIncludeRunOnce: Boolean);
    procedure LoadDisabled(const AKeyPath: string);
    procedure LoadEnabled(const AAllUsers: Boolean); overload;
    procedure LoadEnabled(const ARootKey, AKeyPath: string); overload;
    { external }
    property DeleteBackup: Boolean read FDeleteBackup write FDeleteBackup;
    property Items[AIndex: Word]: TStartupListItem read ItemAt; default;
    property Selected: TStartupListItem read FItem write FItem;
  end;

  { Alias class }
  TAutostart = TStartupList;

  { Exception class }
  EContextMenuException = class(Exception);

  { TContextListItem }
  TContextListItem = class(TRootItem)
  private
    FLocation: string;
    function GetFilePath(): string; virtual; abstract;
    function GetKeyPath(): string; virtual; abstract;
  public
    function Delete(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    procedure GetItemInfo(var AProperties: string; ALangFile: TLanguageFile); override;
    function GetFullKeyPath(): string; override;
    { external }
    property KeyPath: string read GetKeyPath;
    property Location: string read FLocation;
    property FilePath: string read GetFilePath;
  end;

  { TShellItem }
  TShellItem = class(TContextListItem)
  private
    function GetFilePath(): string; override;
    function GetKeyPath(): string; override;
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
  end;

  { TShellExItem }
  TShellExItem = class(TContextListItem)
  private
    function GetFilePath(): string; override;
    function GetKeyPath(): string; override;
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
  end;

  { Search event }
  TOnSearchEvent = procedure(Sender: TObject; ACount: Cardinal) of object;

  { TContextList }
  TContextList = class(TRootList)
  private
    FItem: TContextListItem;
    FWorkCount, FWorkCountMax: TOnSearchEvent;
    FContextCount: TNotifyEvent;
    function Add(AItem: TContextListItem): Word; virtual;
    function FindDouble(AName, AKeyName: string): Boolean;
    function ItemAt(AIndex: Word): TContextListItem;
  protected
    function AddShellItem(const AName, ALocation: string; AEnabled: Boolean): Word;
    function AddShellExItem(const AName, ALocation: string; AEnabled: Boolean): Word;
    procedure LoadContextmenu(const AKeyName: string; AShell: Boolean); overload;
  public
    constructor Create;
    function AddEntry(const AFilePath, AArguments, ALocation,
      ADisplayedName: string): Boolean;
    procedure LoadContextmenu(); overload;
    procedure LoadContextmenu(const AKeyName: string); overload;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean; override;
    function ChangeItemStatus(): Boolean; override;
    function DeleteItem(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    procedure ExportList(const AFileName: string); override;
    function IndexOf(AName, ALocation: string): Integer; overload;
    procedure LoadContextMenus();
    { external }
    property Items[AIndex: Word]: TContextListItem read ItemAt; default;
    property Selected: TContextListItem read FItem write FItem;
    property OnSearch: TOnSearchEvent read FWorkCount write FWorkCount;
    property OnSearchBegin: TOnSearchEvent read FWorkCountMax write FWorkCountMax;
    property OnSearchEnd: TNotifyEvent read FContextCount write FContextCount;
  end;

implementation

uses StrUtils;

{ TLnkFile }

{ public TLnkFile.Create

  Constructor for creating a TLnkFile object. }

constructor TLnkFile.Create(AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

{ public TLnkFile.Create

  Constructor for creating a TLnkFile object. }

constructor TLnkFile.Create(AName: string; AAllUsers: Boolean);
begin
  Create(GetStartUpDir(AAllUsers) + AName);

  if AAllUsers then
    FBackupExt := EXT_COMMON
  else
    FBackupExt := EXT_USER;
end;

{ public TLnkFile.Create

  General constructor for creating a TLnkFile object. }

constructor TLnkFile.Create(AFileName, AExeFileName: string; AArguments: string = '');
begin
  Create(AFileName);
  FExeFileName := AExeFileName;
  FArguments := AArguments;
end;

{ private TLnkFile.GetBackupLnk

  Returns the absoulte path to the backup lnk file. }

function TLnkFile.GetBackupLnk(): string;
begin
  result := GetBackupDir() + ExtractFileName(FFileName) + FBackupExt;
end;

{ private TLnkFile.GetFullPath

  Returns the concatenation of file name and arguments. }

function TLnkFile.GetFullPath(): string;
begin
  if HasArguments() then
    result := FFileName +' '+ FArguments
  else
    result := FFileName;
end;

{ private TLnkFile.GetFullPathEscaped

  Returns the concatenation of file name and arguments escaped with quotes. }

function TLnkFile.GetFullPathEscaped(): string;
begin
  if HasArguments() then
    result := '"'+ FFileName +'" '+ FArguments
  else
    result := '"'+ FFileName +'"';
end;

{ public TLnkFile.BackupExists

  Returns True if the backup .lnk file in C:\Windows\pss\ exists. }

function TLnkFile.BackupExists(): Boolean;
begin
  result := FileExists(GetBackupLnk());
end;

{ public TLnkFile.CreateBackup

  Creates a backup .lnk file in C:\Windows\pss\. }

function TLnkFile.CreateBackup(): Boolean;
begin
  result := WriteLnkFile(FFileName, GetBackupLnk());
end;

{ public TLnkFile.Delete

  Deletes the .lnk file. }

function TLnkFile.Delete(): Boolean;
begin
  result := DeleteFile(FFileName);
end;

{ public TLnkFile.DeleteBackup

  Deletes the backup .lnk file. }

function TLnkFile.DeleteBackup(): Boolean;
begin

end;

{ public TLnkFile.Exists

  Returns True if the .lnk file exists. }

function TLnkFile.Exists(): Boolean;
begin
  result := FileExists(FFileName);
end;

{ public TLnkFile.GetBackupDir

  Returns the path to the backup directory. }

class function TLnkFile.GetBackupDir(): string;
begin
  result := TOSUtils.GetWinDir + '\pss\';
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
    result := IncludeTrailingBackslash(string(Path));
  end  //of begin
  else
    result := '';
end;

{ public TLnkFile.HasArguments

  Returns if arguments are specified. }

function TLnkFile.HasArguments(): Boolean;
begin
  result := (FArguments <> '');
end;

{ public TLnkFile.ReadLnkFile

  Returns the path of a .exe out of a .lnk file. }

function TLnkFile.ReadLnkFile(): Boolean;
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  FileInfo: TWin32FindData;
  Path, Arguments: string;

begin
  result := False;

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
          FFileName := PChar(Path);
          SetLength(Arguments, MAX_PATH + 1);

          if Succeeded(GetArguments(PChar(Arguments), MAX_PATH)) then
            FArguments := PChar(Arguments);

          result := True;
        end;  //of begin
      end; //of with
  end;  //of begin
end;

{ public TLnkFile.WriteLnkFile

  Creates a new .lnk file. }

function TLnkFile.WriteLnkFile(): Boolean;
begin
  result := WriteLnkFile(FFileName, FExeFileName, FArguments);
end;

{ public TLnkFile.WriteLnkFile

  Creates a new .lnk file. }

function TLnkFile.WriteLnkFile(AFileName, AExeFileName: string; AArguments: string = ''): Boolean;
var
  ShellLink : IShellLink;
  PersistFile : IPersistFile;
  Name : PWideChar;

begin
  result := False;

  if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_inPROC_SERVER,
    IID_IShellLinkA, ShellLink)) then
  begin
    // Set path to .exe
    ShellLink.SetPath(PChar(AExeFileName));

    // Set arguments if specified
    if (AArguments <> '') then
      ShellLink.SetArguments(PChar(FArguments));

    // Set working directory
    ShellLink.SetWorkingDirectory(PChar(ExtractFilePath(AExeFileName)));

    if Succeeded(ShellLink.QueryInterface(IPersistFile, PersistFile)) then
    begin
      GetMem(Name, MAX_PATH * 2);

      try
        // Set up information
        MultiByteToWideChar(CP_ACP, 0, PChar(AFileName), -1, Name, MAX_PATH);

        // Save .lnk
        result := Succeeded(PersistFile.Save(Name, True));

      finally
        FreeMem(Name, MAX_PATH * 2);
      end; //of finally
    end; //of begin
  end; //of begin
end;


{ TRegUtils }

{ public TRegUtils.DeleteKey

  Deletes a Registry key. }

class function TRegUtils.DeleteKey(AMainKey, AKeyPath, AKeyName: string): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ or KEY_WRITE));

  try
    reg.RootKey := StrToHKey(AMainKey);
    reg.OpenKey(AKeyPath, False);
    result := reg.DeleteKey(AKeyName);

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
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE));

  try
    reg.RootKey := StrToHKey(AMainKey);
    reg.OpenKey(AKeyName, False);
    result := reg.DeleteValue(AValueName);

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
    Delete(AName, Index, Index + Length(Ext) -1);

  result := AName;
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
        result := reg.ReadString(AValueName)
      else
        result := '';
    end  //of begin
    else
      result := '';

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
    result := DeleteKey('HKCR', KEY_RECYCLEBIN, 'Clearas')
  else
    begin
      // Add recycle bin context menu entry
      WriteStrValue('HKCR', KEY_RECYCLEBIN +'\Clearas\command', '', ParamStr(0));
      result := True;
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
      result := True;
    end  //of begin
    else
      result := False;

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
    result := UpdateContextPath();
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

{ public TRootItem.ChangeStatus

  Changes the item status. }

function TRootItem.ChangeStatus(): Boolean;
begin
  if FEnabled then
    result := Disable
  else
    result := Enable;
end;

{ public TRootItem.GetStatus

  Returns the item status as text. }

function TRootItem.GetStatus(ALangFile: TLanguageFile): string;
begin
  if FEnabled then
    result := ALangFile.GetString(31)
  else
    result := ALangFile.GetString(32);
end;

{ public TRootItem.Open

  Opens an TRootItem object in RegEdit. }

procedure TRootItem.Open();
begin
  // Set the Registry key to show
  TRegUtils.WriteStrValue('HKCU', KEY_REGEDIT, 'LastKey', 'Computer\'+ GetFullKeyPath());

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

{ public TRootList.Destroy

  General destructor for destroying a TRootList instance. }

destructor TRootList.Destroy;
begin
  inherited Destroy;
end;

{ protected TRootList.RootItemAt

  Returns a TRootItem object at index. }

function TRootList.RootItemAt(AIndex: Word): TRootItem;
begin
  result := TRootItem(Self.Items[AIndex]);
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
  result := -1;

  for i := 0 to Count -1 do
    if (RootItemAt(i).Name = AItemName) then
    begin
      result := i;
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
  result := -1;

  for i := 0 to Count -1 do
  begin
    Item := RootItemAt(i);

    if ((Item.Name = AItemName) and (Item.Enabled = AEnabled))then
    begin
      result := i;
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
  result := '';

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
      result := Date +'  '+ Time;
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
  result := False;
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
      result := True;

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
  result := TRegUtils.HKeyToStr(TRegUtils.StrToHKey(FRootKey)) +'\'+ FKeyPath;
end;


{ TStartupItem }

{ private TStartupItem.StartupUserType

  Returns if the basic type is "Startup User". }

function TStartupItem.StartupUserType(): Boolean;
begin
  result := False;
end;

{ protected TStartupItem.Disable

  Disables an TStartupItem object and returns True if successful. }

function TStartupItem.Disable(): Boolean;
var
  reg: TRegistry;

begin
  result := False;
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

      result := True;

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

{ protected TStartupItem.Enable

  Enables an TStartupItem object and returns True if successful. }

function TStartupItem.Enable(): Boolean;
var
  reg: TRegistry;
  NewHKey, NewKeyPath: string;

begin
  result := False;
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
        result := TRegUtils.DeleteKey('HKLM', KEY_DEACT, FName);

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

{ public TStartupItem.CreateBackup

  Creates a backup file and returns True if successful. }

function TStartupItem.CreateBackup(): Boolean;
begin
  result := False;
end;

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

    result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while deleting "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupItem.GetItemInfo

  Returns the name and path of an item as formatted text. }

procedure TStartupItem.GetItemInfo(var AProperties: string; ALangFile: TLanguageFile);
begin
  AProperties := ALangFile.Format(46, [FName, GetFullKeyPath()]);
end;


{ TStartupUserItem }

{ private TStartupUserItem.AddCircumflex

  Replaces all backslashes in a path by circumflex. }

function TStartupUserItem.AddCircumflex(const AName: string): string;
begin
  result := StringReplace(AName, '\', '^', [rfReplaceAll]);
end;

{ private TStartupUserItem.GetExtension

  Returns the extension of an item used at the end of a backup file. }

function TStartupUserItem.GetExtension(): string;
begin
  if AnsiContainsText(FType, 'Common') then
    result := EXT_COMMON
  else
    result := EXT_USER;
end;

{ private TStartupUserItem.StartupUserType

  Returns if the basic type is "Startup User". }

function TStartupUserItem.StartupUserType(): Boolean;
begin
  result := True;
end;

{ protected TStartupUserItem.Disable

  Disables an TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Disable(): Boolean;
var
  reg: TRegistry;
  KeyName, BackupLnk: string;
  LnkFile: TLnkFile;

begin
  result := False;
  reg := TRegistry.Create(TRegUtils.DenyWOW64Redirection(KEY_WRITE));
  LnkFile := TLnkFile.Create(FKeyPath);

  try
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      KeyName := AddCircumflex(FKeyPath);
      BackupLnk := GetBackupLnk();

      if not LnkFile.ReadLnkFile() then
        raise EStartupException.Create('Could not read .lnk file!');

      if not reg.OpenKey(KEY_DEACT_FOLDER + KeyName, True) then
        raise EStartupException.Create('Could not create key "'+ FName +'"!');

      reg.WriteString('path', FKeyPath);
      reg.WriteString('item', TRegUtils.DeleteExt(ExtractFileName(FName)));
      reg.WriteString('command', FFilePath);
      reg.WriteString('backup', BackupLnk);

      // Special Registry entries only for Windows >= Vista
      if TOSUtils.WindowsVistaOrLater() then
      begin
        reg.WriteString('backupExtension', GetExtension());
        reg.WriteString('location', ExtractFileDir(FKeyPath));
        WriteTime(KEY_DEACT_FOLDER + KeyName);
      end  //of begin
      else
        reg.WriteString('location', FType);

      // Create backup directory if not exist
      if not DirectoryExists(TLnkFile.GetBackupDir()) then
        ForceDirectories(TLnkFile.GetBackupDir());

      // Create backup by copying original .lnk
      if not CopyFile(PChar(FKeyPath), PChar(BackupLnk), False) then
        raise EStartupException.Create('Could not create backup file!');

      // Delete original .lnk
      if not DeleteFile(FKeyPath) then
        raise EStartupException.Create('Could not delete .lnk file!');

      // Update information
      FKeyPath := KEY_DEACT_FOLDER + KeyName;
      FRootKey := 'HKLM';
      FEnabled := False;
      result := True;

    finally
      LnkFile.Free;
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

{ protected TStartupUserItem.Enable

  Enables an TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Enable(): Boolean;
var
  Path, BackupLnk: string;
  LnkFile: TLnkFile;

begin
  try
    Path := TRegUtils.GetKeyValue('HKLM', FKeyPath, 'path');
    BackupLnk := GetBackupLnk();

    // Backup file exists?
    if FileExists(BackupLnk) then
    begin
      // Failed to restore backup file?
      if not CopyFile(PChar(BackupLnk), PChar(Path), True) then
        raise EStartupException.Create('Could not create .lnk file!');
    end  //of begin
    else
      begin
        LnkFile := TLnkFile.Create();

        try
          // Failed to create new .lnk file?
          if not LnkFile.WriteLnkFile(Path, FFilePath) then
            raise EStartupException.Create('Could not create .lnk file!');

        finally
          LnkFile.Free;
        end;  //of try
      end;  //of if

    // Could not delete old key?
    if not TRegUtils.DeleteKey('HKLM', KEY_DEACT_FOLDER, AddCircumflex(Path)) then
      raise EStartupException.Create('Could not delete key "'+ FKeyPath +'"!');

    // Update information
    FKeyPath := Path;
    FRootKey := '';
    FEnabled := True;
    result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupUserItem.ChangeFilePath

  Changes the file path of a TStartupUserItem item. }

function TStartupUserItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  LnkFile: TLnkFile;

begin
  result := False;

  if not FEnabled then
  begin
    result := inherited ChangeFilePath(ANewFilePath);
    Exit;
  end;  //of begin

  LnkFile := TLnkFile.Create();

  try
    try
      // Failed to create new .lnk file?
      if not LnkFile.WriteLnkFile(FKeyPath, ANewFilePath) then
        raise EStartupException.Create('Could not create .lnk file!');

      // Update file path
      FFilePath := ANewFilePath;
      result := True;

    finally
      LnkFile.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      E.Message := 'Error while changing file path of "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupUserItem.CreateBackup

  Creates a backup file and returns True if successful. }

function TStartupUserItem.CreateBackup(): Boolean;
var
  LnkFile: TLnkFile;

begin
  // result := FLnkFile.CreateBackup();
  LnkFile := TLnkFile.Create(FFilePath, GetBackupLnk());

  try
    result := LnkFile.WriteLnkFile();

  finally
    LnkFile.Free;
  end;  //of try
end;

{ public TStartupUserItem.Delete

  Deletes a TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Delete(): Boolean;

  function GetKeyName(): string;
  begin
    result := AddCircumflex(TLnkFile.GetStartUpDir(AnsiContainsText('Common', FType)) + FName);
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

    result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while deleting "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupUserItem.GetBackupLnk

  Returns the path to the backup file. }

function TStartupUserItem.GetBackupLnk(): string;
begin
  result := TLnkFile.GetBackupDir() + FName + GetExtension();
end;

{ public TStartupUserItem.GetItemInfo

  Returns the name and path of an item as formatted text. }

procedure TStartupUserItem.GetItemInfo(var AProperties: string;
  ALangFile: TLanguageFile);
begin
  if FEnabled then
    AProperties := ALangFile.Format(45, [FName, FKeyPath])
  else
    AProperties := ALangFile.Format(46, [FName, GetFullKeyPath()]);
end;

{ public TStartupUserItem.Open

  Opens an TStartupUserItem object in Explorer. }

procedure TStartupUserItem.Open();
begin
  if FEnabled then
    TOSUtils.ExecuteProgram('explorer.exe', '/select, '+ FKeyPath)
  else
    inherited Open();
end;


{ TStartupList }

{ public TStartupList.Create

  General constructor for creating a TStartupList instance. }

constructor TStartupList.Create;
begin
  inherited Create;
  FDeleteBackup := True;
end;

{ private TStartupList.Add

  Adds a TStartupListItem to the list and returns position. }

function TStartupList.Add(AItem: TStartupListItem): Word;
begin
  result := inherited Add(AItem);
end;

{ private TStartupList.DelCircumflex

  Replaces all circumflexes by backslashes. }

function TStartupList.DelCircumflex(AName: string): string;
begin
  result := StringReplace(AName, '^', '\', [rfReplaceAll]);
end;

{ private TStartupList.DeleteBackupLnk

  Deletes the backup file and returns True if successful. }

function TStartupList.DeleteBackupLnk(): Boolean;
begin
  result := DeleteFile(GetBackupLnk());
end;

{ private TStartupList.ItemAt

  Returns a TStartupListItem object at index. }

function TStartupList.ItemAt(AIndex: Word): TStartupListItem;
begin
  result := TStartupListItem(RootItemAt(AIndex));
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
       result := TYPE_COMMON
     else
       result := TYPE_USER;
  end  //of begin
  else
    result := TRegUtils.GetKeyValue('HKLM', AKeyPath, 'location');
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

  result := Add(Item);
end;

{ protected TStartupList.AddItemEnabled

  Adds a enabled default startup item to the list. }

function TStartupList.AddItemEnabled(const ARootKey, AKeyPath, AName, AFilePath: string): Word;
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
  result := Add(Item);
end;

{ protected TStartupList.AddNewStartupUserItem

  Adds a new startup user item to the autostart. }

function TStartupList.AddNewStartupUserItem(AName, AFilePath: string;
  AArguments: string = ''; AAllUsers: Boolean = False): Boolean;
var
  LnkFile: TLnkFile;

begin
  result := False;

  if (ExtractFileExt(AName) <> '.lnk') then
    AName := AName +'.lnk';

  // Init .lnk file
  LnkFile := TLnkFile.Create(AName, AAllUsers);

  try
    // Link file created successfully?
    if not LnkFile.WriteLnkFile(AFilePath, AArguments) then
      raise EStartupException.Create('Could not create .lnk file!');

    AddUserItemEnabled(LnkFile.FullPath, AAllUsers);
    result := True;

  finally
    LnkFile.Free;
  end;  //of try
end;

{ protected TStartupList.AddUserItemDisabled

  Adds a disabled startup user item to the list. }

function TStartupList.AddUserItemDisabled(const AKeyPath: string): Word;
var
  Item: TStartupListItem;

begin
  Item := TStartupUserItem.Create(Count, False);

  with Item do
  begin
    RootKey := 'HKLM';
    KeyPath := AKeyPath;
    Name := ExtractFileName(DelCircumflex(AKeyPath));
    FilePath := TRegUtils.GetKeyValue('HKLM', AKeyPath, 'command');
    Time := GetTime();
    TypeOf := GetStartupUserType(AKeyPath);
  end;  //of with

  result := Add(Item);
end;

{ protected TStartupList.AddUserItemEnabled

  Adds a enabled startup user item to the list. }

function TStartupList.AddUserItemEnabled(const ALnkFile: string; AAllUsers: Boolean): Word;
var
  Item: TStartupListItem;
  LnkFile: TLnkFile;
  Path: string;

begin
  LnkFile := TLnkFile.Create(ALnkFile);
  Item := TStartupUserItem.Create(Count, True);
  // Item := TStartupUserItem.Create(LnkFile, Count, True);

  // Try to read .lnk file
  try
    LnkFile.ReadLnkFile();
    Path := LnkFile.FullPath;

  finally
    LnkFile.Free;
  end;  //of try

  with Item do
  begin
    RootKey := '';
    KeyPath := ALnkFile;
    FilePath := Path;
    Name := ExtractFileName(ALnkFile);

    if AAllUsers then
      TypeOf := TYPE_COMMON
    else
      TypeOf := TYPE_USER;

    Time := '';
  end;  //of with

  Inc(FActCount);
  result := Add(Item);
end;

{ public TStartupList.CreateBackup

  Creates a backup file and returns True if successful. }

function TStartupList.CreateBackup(): Boolean;
begin
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  result := FItem.CreateBackup();
end;

{ public TStartupList.AddProgram

  Adds a new startup item to autostart. }

function TStartupList.AddProgram(AFilePath, AArguments: string;
  ADisplayedName: string = ''): Boolean;
var
  Name, Ext: string;
  i: Word;

begin
  result := False;
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

    result := AddNewStartupUserItem(Name, AFilePath, AArguments);
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
      result := True;
    end;  //of begin
end;

{ public TStartupList.BackupExists

  Checks if a backup file already exists. }

function TStartupList.BackupExists(): Boolean;
begin
  result := False;

  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if FItem.StartupUser then
    result := FileExists(GetBackupLnk());
end;

{ public TStartupList.ChangeItemFilePath

  Changes the file path of an item. }

function TStartupList.ChangeItemFilePath(const ANewFilePath: string): Boolean;
begin
  // Invalid item?
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  result := FItem.ChangeFilePath(ANewFilePath);
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
      if (FDeleteBackup and FItem.StartupUser) then
        DeleteBackupLnk();

      // Update active counter
      Inc(FActCount);
    end  //of begin
  else
    Dec(FActCount);

  result := Changed;
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
    if (FDeleteBackup and FItem.StartupUser and BackupExists()) then
      DeleteBackupLnk();

    // Item was enabled
    if FItem.Enabled then
      // Update active counter
      Dec(FActCount);

    // Remove item from list
    inherited Remove(FItem);
    FItem := nil;
  end;  //of begin

  result := Deleted;
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
  i: Word;
  RegFile: TRegistryFile;
  Item: TStartupListItem;

begin
  // Init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    for i := 0 to Count -1 do
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

{ public TStartupList.GetBackupLnk

  Returns the path to the backup file. }

function TStartupList.GetBackupLnk(): string;
begin
  result := '';

  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if (FItem is TStartupUserItem) then
    result := (FItem as TStartupUserItem).GetBackupLnk();
end;

{ public TStartupList.ImportBackup

  Imports a startup user backup file and adds it to the list. }

function TStartupList.ImportBackup(const AFilePath: string): Boolean;
var
  Name, Ext: string;
  i: Word;
  LnkFile: TLnkFile;

begin
  result := False;
  LnkFile := TLnkFile.Create(AFilePath);
  Ext := ExtractFileExt(AFilePath);

  // Check invalid extension
  if ((Ext <> EXT_COMMON) and (Ext <> EXT_USER)) then
    raise EInvalidArgument.Create('Invalid backup file extension! Must be "'
      + EXT_COMMON +'" or "'+ EXT_USER+'"!');

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
    for i := 0 to Count -1 do
      if (ItemAt(i).FilePath = LnkFile.ExeFileName) then
        Exit;

    // Create .lnk file and add it to list
    result := AddNewStartupUserItem(Name, LnkFile.ExeFileName, LnkFile.Arguments,
      (Ext = EXT_COMMON));

  finally
    LnkFile.Free;
  end;  //of try
end;

{ public TStartupList.LoadAutostart

  Searches for startup items at different locations. }

procedure TStartupList.LoadAutostart(AIncludeRunOnce: Boolean);
begin
  LoadEnabled('HKLM', KEY_STARTUP);

  // Load WOW6432 Registry key only on 64bit Windows
  if TRegUtils.IsWindows64() then
    LoadEnabled('HKLM', KEY_STARTUP32);

  // Read RunOnce entries?
  if AIncludeRunOnce then
  begin
    LoadEnabled('HKLM', KEY_RUNONCE);
    LoadEnabled('HKCU', KEY_RUNONCE);

    // Load WOW6432 Registry keys only on 64bit Windows
    if TRegUtils.IsWindows64() then
      begin
        LoadEnabled('HKLM', KEY_RUNONCE32);
        LoadEnabled('HKCU', KEY_RUNONCE32);
      end;  //of begin
  end;  //of begin

  LoadEnabled('HKCU', KEY_STARTUP);
  LoadEnabled(True);
  LoadEnabled(False);
  LoadDisabled(KEY_DEACT);
  LoadDisabled(KEY_DEACT_FOLDER);
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
  i: integer;

begin
  List := TStringList.Create;

  try
    // Retrieve a list containing all activated startup user .lnk files
    GetLnkFileNames(List, AAllUsers);

    // Add every file to list
    for i := 0 to List.Count -1 do
      AddUserItemEnabled(List[i], AAllUsers);

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
  FilePath: string;

begin
  List := TStringList.Create;
  reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));

  try
    reg.RootKey := TOSUtils.StrToHKey(ARootKey);
    reg.OpenKey(AKeyPath, True);
    reg.GetValueNames(List);

    for i := 0 to List.Count -1 do
    begin
      // Read path to .exe
      FilePath := reg.ReadString(List[i]);

      // Add to list
      AddItemEnabled(ARootKey, AKeyPath, List[i], FilePath);
    end;  //of for

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
      result := True;

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

{ public TContextListItem.GetItemInfo

  Returns the name and path of an item as formatted text. }

procedure TContextListItem.GetItemInfo(var AProperties: string; ALangFile: TLanguageFile);
var
  Text: string;

begin
  Text := TRegUtils.GetKeyValue('HKCR', GetKeyPath(), '');
  AProperties := ALangFile.Format(47, [FName, Text]);
end;

{ public TContextListItem.GetFullKeyPath

  Returns the Registry path to a TStartupListItem. }

function TContextListItem.GetFullKeyPath(): string;
begin
  result := TRegUtils.HKeyToStr(HKEY_CLASSES_ROOT) +'\'+ GetKeyPath();
end;


{ TShellItem }

{ private TShellItem.GetFilePath

  Returns the command of a Shell entry. }

function TShellItem.GetFilePath(): string;
begin
  result := TRegUtils.GetKeyValue('HKCR', GetKeyPath() +'\command', '');
end;

{ private TShellItem.GetKeyPath

  Returns the Registry path of a TShellItem. }

function TShellItem.GetKeyPath(): string;
begin
  result := FLocation +'\'+ FType +'\'+ FName;
end;

{ protected TShellItem.Disable

  Disables a TShellItem object and returns True if successful. }

function TShellItem.Disable(): Boolean;
begin
  try
    TRegUtils.WriteStrValue('HKCR', KeyPath, 'LegacyDisable', '');
    FEnabled := False;
    result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while disabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;

{ protected TShellItem.Enable

  Enables a TShellItem object and returns True if successful. }

function TShellItem.Enable(): Boolean;
begin
  try
    if not TRegUtils.DeleteValue('HKCR', KeyPath, 'LegacyDisable') then
      raise EStartupException.Create('Could not delete value "'+ KeyPath + '\LegacyDisable' +'"!');

    FEnabled := True;
    result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;

{ public TShellItem.ChangeFilePath

  Changes the file path of an TShellItem item. }

function TShellItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  reg: TRegistry;

begin
  result := False;
  reg := TRegistry.Create(TWinWOW64.DenyWOW64Redirection(KEY_ALL_ACCESS));

  try
    try
      reg.RootKey := HKEY_CLASSES_ROOT;

      // Invalid key?
      if not reg.OpenKey(GetKeyPath() +'\command', False) then
        raise Exception.Create('Key does not exist!');

      // Change path
      reg.WriteString('', ANewFilePath);
      result := True;

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


{ TShellExItem }

{ private TShellExItem.GetFilePath

  Returns the command of a Shell entry. }

function TShellExItem.GetFilePath(): string;
begin
  result := '';
end;

{ private TShellExItem.GetKeyPath

  Returns the Registry path to a TShellExItem. }

function TShellExItem.GetKeyPath(): string;
begin
  result := FLocation + KEY_CONTEXTMENU +'\'+ FName;
end;

{ protected TShellExItem.Disable

  Disables a TShellExItem object and returns True if successful. }

function TShellExItem.Disable(): Boolean;
var
  OldValue: string;

begin
  try
    OldValue := TRegUtils.GetKeyValue('HKCR', KeyPath, '');
    TRegUtils.WriteStrValue('HKCR', KeyPath, '', '-' + OldValue);
    FEnabled := False;
    result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while disabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;

{ protected TShellExItem.Enable

  Enables a TShellExItem object and returns True if successful. }

function TShellExItem.Enable(): Boolean;
var
  OldValue, NewValue: string;

begin
  try
    OldValue := TRegUtils.GetKeyValue('HKCR', KeyPath, '');
    NewValue := Copy(OldValue, 2, Length(OldValue));
    TRegUtils.WriteStrValue('HKCR', KeyPath, '', NewValue);
    FEnabled := True;
    result := True;

  except
    on E: Exception do
    begin
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;

{ public TShellExItem.ChangeFilePath

  Changes the file path of an TShellExItem item. }

function TShellExItem.ChangeFilePath(const ANewFilePath: string): Boolean;
begin
  result := False;
end;


{ TContextList }

{ public TContextList.Create

  General constructor for creating a TContextList instance. }

constructor TContextList.Create;
begin
  inherited Create;
  FActCount := 0;
end;

{ private TContextList.Add

  Adds a TContextListItem to the list and returns position. }

function TContextList.Add(AItem: TContextListItem): Word;
begin
  result := inherited Add(AItem);
end;

{ public TContextList.AddEntry

  Adds a new contextmenu entry. }

function TContextList.AddEntry(const AFilePath, AArguments, ALocation,
  ADisplayedName: string): Boolean;
var
  Name, Ext, FullPath: string;

begin
  result := False;
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
  TRegUtils.WriteStrValue('HKCR', ALocation +'\Shell\'+ Name, '', ADisplayedName);
  TRegUtils.WriteStrValue('HKCR', ALocation +'\Shell\'+ Name +'\command', '', FullPath);

  // Adds this item to list
  AddShellItem(Name, ALocation, True);
  result := True;
end;

{ private TContextList.Add

  Checks if an TContextListItem already exists in list. }

function TContextList.FindDouble(AName, AKeyName: string): Boolean;
begin
  result := False;

  if ((Count > 0) and (IndexOf(AName) <> -1)) then
    result := (ItemAt(IndexOf(AName)).TypeOf = AKeyName);
end;

{ private TContextList.ItemAt

  Returns a TContextListItem object at index. }

function TContextList.ItemAt(AIndex: Word): TContextListItem;
begin
  result := TContextListItem(RootItemAt(AIndex));
end;

{ protected TContextList.AddShellItem

  Adds a shell item to list. }

function TContextList.AddShellItem(const AName, ALocation: string;
  AEnabled: Boolean): Word;
var
  Item: TContextListItem;

begin
  Item := TShellItem.Create(Count, AEnabled);

  with Item do
  begin
    FName := AName;
    FLocation := ALocation;
    FType := 'Shell';

    if AEnabled then
      Inc(FActCount);
  end;  //of with

  result := Add(Item);
end;

{ protected TContextList.AddShellExItem

  Adds a shellex item to list. }

function TContextList.AddShellExItem(const AName, ALocation: string;
  AEnabled: Boolean): Word;
var
  Item: TContextListItem;

begin
  Item := TShellExItem.Create(Count, AEnabled);

  with Item do
  begin
    FName := AName;
    FLocation := ALocation;
    FType := 'ShellEx';

    if AEnabled then
      Inc(FActCount);
  end;  //of with

  result := Add(Item);
end;

{ protected TContextList.AddEntry

  Adds a context item to list. }

procedure TContextList.LoadContextmenu(const AKeyName: string; AShell: Boolean);
var
  reg: TRegistry;
  i: Integer;
  List: TStringList;
  Item, Key: string;
  Enabled: Boolean;

begin
  reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));
  List := TStringList.Create;

  if AShell then
    Key := AKeyName +'\shell'
  else
    Key := AKeyName + KEY_CONTEXTMENU;

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

      // Filter empty, important and double entries
      if ((reg.ReadString('') <> '') and (Item[1] <> '{') and
        (reg.ReadString('')[1] <> '@') and not FindDouble(Item, AKeyName)) then
      begin
        // Search for shell entries
        if AShell then
          begin
            // Get status
            Enabled := not reg.ValueExists('LegacyDisable');

            // Add item to list
            AddShellItem(Item, AKeyName, Enabled);
          end  //of begin
        else
          // Search for shellex entries
          begin
            // Get status
            Enabled := reg.ReadString('')[1] <> '-';

            // Add item to list
            AddShellExItem(Item, AKeyName, Enabled);
          end  //of begin
        end;  //of begin
    end;  //of for

  finally
    List.Free;
    reg.Free;
  end;  //of try
end;

{ protected TContextList.LoadContextmenus

  Searches for context menu entries and adds them to the list. }

procedure TContextList.LoadContextmenu();
var
  reg: TRegistry;
  i, j, k: integer;
  Hkcr, Temp, Shellex: TStringList;
  Progress, CountMax: Cardinal;

begin
  reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));
  Hkcr := TStringList.Create;
  Temp := TStringList.Create;
  Shellex := TStringList.Create;

  try
    reg.RootKey := HKEY_CLASSES_ROOT;

    // Bad workaround to prevent an annoying bug of TRegistry
    if not reg.KeyExists('$$$_auto_file') then
      reg.CreateKey('$$$_auto_file');

    // Read out all keys
    reg.OpenKey('', False);
    reg.GetKeyNames(Hkcr);
    CountMax := Hkcr.Count;

    // Notify start of search
    FWorkCountMax(Self, CountMax);
    Progress := 0;

    for i := 0 to Hkcr.Count -1 do
    begin
      // Refresh current progress
      if Assigned(FContextCount) then
      begin
        Inc(Progress);
        FWorkCount(Self, Progress);
      end;  //of begin

      reg.CloseKey;
      reg.OpenKey(Hkcr[i], False);

      if reg.HasSubKeys then
      begin
        Temp.Clear;
        reg.GetKeyNames(Temp);

        for j := 0 to Temp.Count -1 do
          if ((Temp[j] = 'shellex') or (Temp[j] = 'ShellEx')) then
          begin
            reg.CloseKey;
            reg.OpenKey(Hkcr[i] +'\'+ Temp[j], False);

            if reg.HasSubKeys then
            begin
              Shellex.Clear;
              reg.GetKeyNames(Shellex);

              for k := 0 to Shellex.Count -1 do
                if (Shellex[k] = 'ContextMenuHandlers') then
                begin
                  reg.CloseKey;
                  reg.OpenKey(Hkcr[i] +'\'+ Temp[j] +'\'+ Shellex[k], False);

                  if reg.HasSubKeys then
                    LoadContextmenu(Hkcr[i]);
                end;  //of for
            end;  //of begin
          end;  //of for
      end;  //of begin
    end;  //of for

  finally
    Temp.Free;
    Hkcr.Free;
    Shellex.Free;
    reg.Free;

    // Notify end of search
    if Assigned(FContextCount) then
      FContextCount(Self);
  end;  //of finally
end;

{ public TContextList.AddEntry

  Searches for context menu entries in specific AKeyName and adds them
  to the list. }

procedure TContextList.LoadContextmenu(const AKeyName: string);
begin
  LoadContextmenu(AKeyName, True);
  LoadContextmenu(AKeyName, False);
end;

{ public TContextList.ChangeItemFilePath

  Changes the file path of an item. }

function TContextList.ChangeItemFilePath(const ANewFilePath: string): Boolean;
begin
  // Invalid item?
  if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  result := FItem.ChangeFilePath(ANewFilePath);
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

  result := Changed;
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

  result := Deleted;
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
  result := -1;

  for i := 0 to Count -1 do
  begin
    Item := ItemAt(i);

    if ((Item.Name = AName) and (Item.Location = ALocation)) then
    begin
      result := i;
      Break;
    end;  //of begin
  end;  //of for
end;

{ public TContextList.LoadContextMenus

  Searches for context menu entries at different locations. }

procedure TContextList.LoadContextMenus();
begin
  LoadContextmenu('AllFilesystemObjects');
  LoadContextmenu('Directory');
  LoadContextmenu('Folder');
  LoadContextmenu('Drive');
end;

end.
