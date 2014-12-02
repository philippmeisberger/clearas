{ *********************************************************************** }
{                                                                         }
{ Clearas API Interface Unit v4.1                                         }
{                                                                         }
{ Copyright (c) 2011-2014 P.Meisberger (PM Code Works)                    }
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
  KEY_STARTUP = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP32 = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';

  { Extensions of backup files }
  EXT_COMMON = '.CommonStartup';
  EXT_USER = '.Startup';

  TYPE_COMMON = 'Common Startup';
  TYPE_USER = 'Startup User';

type
  { TClearas }
  TClearas = class(TOSUtils)
  protected
    class function DeleteKey(AMainKey, AKeyPath, AKeyName: string): Boolean;
    class function DeleteValue(AMainKey, AKeyName, AValueName: string): Boolean;
  public
    class function CreateLnk(const AExeFilename, ALinkFilename: string): Boolean;
    class function DeleteExt(AName: string): string;
    class function GetBackupDir(): string;
    class function GetKeyValue(AMainKey, AKeyPath, AValueName: string): string;
    class function GetStartUpDir(AAllUsers: Boolean): string;
    class function ReadLnkFile(const ALnkFileName: string; out APath: string): Boolean;
    class function RegisterInContextMenu(ACheck: Boolean): Boolean;
    class function UpdateContextPath(): Boolean; overload;
    class function UpdateContextPath(ALangFile: TLanguageFile): Boolean; overload; deprecated;
    class procedure WriteStrValue(AMainKey, AKeyName, AName, AValue: string);
  end;

  { TRootItem }
  TRootItem = class(TClearas)
  private
    FIndex: Word;
    FEnabled: Boolean;
    FName, FType: string;
  protected
    function Disable(): Boolean; virtual; abstract;
    function Enable(): Boolean; virtual; abstract;
  public
    constructor Create(AIndex: Word; AEnabled: Boolean);
    destructor Destroy; override;
    function ChangeStatus(): Boolean; virtual;
    function Delete(): Boolean; virtual; abstract;
    procedure ExportItem(const AFileName: string); virtual; abstract;
    procedure GetItemInfo(var AProperties: string; ALangFile: TLanguageFile); virtual; abstract;
    function GetStatus(ALangFile: TLanguageFile): string;
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
  public
    constructor Create;
    destructor Destroy; override;
    function ChangeItemStatus(): Boolean; virtual; abstract;
    procedure Clear; override;
    function DeleteItem(): Boolean; virtual; abstract;
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
    procedure ExportItem(const AFileName: string); override;
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
    function AddCircumflex(const AName: string): string;
    function GetBackupLnk(): string;
    function GetExtension(): string;
    function StartupUserType(): Boolean; override;
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  public
    function CreateBackup(): Boolean; override;
    function Delete(): Boolean; override;
    procedure GetItemInfo(var AProperties: string; ALangFile: TLanguageFile); override;
  end;

  { TStartupList }
  TStartupList = class(TRootList)
  private
    FItem: TStartupListItem;
    FDeleteBackup: Boolean;
    function Add(AItem: TStartupListItem): Word; virtual;
    function DelCircumflex(AName: string): string;
    function DeleteBackupLnk(): Boolean;
    function GetBackupLnk(): string;
    function GetItem(AIndex: Word): TStartupListItem;
    procedure GetLnkFileNames(AFileList: TStrings; AAllUsers: Boolean);
    function GetStartupUserType(const AKeyPath: string): string;
  protected
    function AddItemDisabled(const AKeyPath: string): Integer;
    function AddItemEnabled(const ARootKey, AKeyPath, AName, AFilePath: string): Integer;
    function AddNewStartupUserItem(AName, AFilePath: string;
      AAllUsers: Boolean = False): Boolean;
    function AddUserItemDisabled(const AKeyPath: string): Integer;
    function AddUserItemEnabled(const ALnkFile: string; AAllUsers: Boolean): Integer;
  public
    constructor Create;
    function CreateBackup(): Boolean;
    function AddProgram(const AFilePath: string; ANickName: string = ''): Boolean;
    function BackupExists(): Boolean;
    function ChangeItemStatus(): Boolean; override;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean;
    function DeleteItem(): Boolean; override;
    procedure ExportItem(const AFileName: string; ARegFile: Boolean = True);
    procedure ExportList(const AFileName: string);
    function ImportBackup(const AFilePath: string): Boolean;
    function IndexOf(AName: string): Integer; overload;
    function IndexOf(AName: string; AEnabled: Boolean): Integer; overload;
    procedure LoadAutostart(AIncludeRunOnce: Boolean);
    procedure LoadDisabled(const AKeyPath: string);
    procedure LoadEnabled(const AAllUsers: Boolean); overload;
    procedure LoadEnabled(const ARootKey, AKeyPath: string); overload;
    { external }
    property DeleteBackup: Boolean read FDeleteBackup write FDeleteBackup;
    property Items[AIndex: Word]: TStartupListItem read GetItem; default;
    property Item: TStartupListItem read FItem write FItem;
  end;

  { Alias class }
  TAutostart = TStartupList;

  { Exception class }
  EContextMenuException = class(Exception);

  { TContextListItem }
  TContextListItem = class(TRootItem)
  private
    FLocation: string;
    function GetKeyPath(): string; virtual; abstract;
  public
    function Delete(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    procedure GetItemInfo(var AProperties: string; ALangFile: TLanguageFile); override;
    { external }
    property KeyPath: string read GetKeyPath;
    property Location: string read FLocation;
  end;

  { TShellItem }
  TShellItem = class(TContextListItem)
  private
    function GetKeyPath(): string; override;
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
  end;

  { TShellExItem }
  TShellExItem = class(TContextListItem)
  private
    function GetKeyPath(): string; override;
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
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
    function GetItem(AIndex: Word): TContextListItem;
  protected
    function AddShellItem(const AName,  ALocation: string; AEnabled: Boolean): Word;
    function AddShellExItem(const AName, ALocation: string; AEnabled: Boolean): Word;
    procedure AddEntry(const AKeyName: string; AShell: Boolean); overload;
  public
    constructor Create;
    procedure AddEntry(); overload;
    procedure AddEntry(const AKeyName: string); overload;
    function ChangeItemStatus(): Boolean; override;
    function DeleteItem(): Boolean; override;
    procedure ExportItem(const AFileName: string);
    function IndexOf(AName: string): Integer; overload;
    function IndexOf(AName, ALocation: string): Integer; overload;
    procedure LoadContextMenus();
    { external }
    property Items[AIndex: Word]: TContextListItem read GetItem; default;
    property Item: TContextListItem read FItem write FItem;
    property OnSearch: TOnSearchEvent read FWorkCount write FWorkCount;
    property OnSearchBegin: TOnSearchEvent read FWorkCountMax write FWorkCountMax;
    property OnSearchEnd: TNotifyEvent read FContextCount write FContextCount;
  end;

implementation

uses Math;

{ TClearas }

{ public TClearas.DeleteKey

  Deletes a Registry key. }

class function TClearas.DeleteKey(AMainKey, AKeyPath, AKeyName: string): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE));

  try
    reg.RootKey := StrToHKey(AMainKey);
    reg.OpenKey(AKeyPath, False);
    result := reg.DeleteKey(AKeyName);

  finally
    reg.Free;
  end;  //of try
end;

{ public TClearas.DeleteValue

  Deletes a Registry value. }

class function TClearas.DeleteValue(AMainKey, AKeyName, AValueName: string): Boolean;
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

{ public TClearas.CreateLnk

  Creates a new .lnk file. }

class function TClearas.CreateLnk(const AExeFilename, ALinkFilename: string): Boolean;
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

    // Set working directory
    ShellLink.SetWorkingDirectory(PChar(ExtractFilePath(AExeFileName)));

    if Succeeded(ShellLink.QueryInterface(IPersistFile, PersistFile)) then
    begin
      GetMem(Name, MAX_PATH * 2);

      try
        // Write information
        MultiByteToWideChar(CP_ACP, 0, PChar(ALinkFilename), -1, Name, MAX_PATH);

        // Save .lnk
        PersistFile.Save(Name, True);
        result := True;

      finally
        FreeMem(Name, MAX_PATH*2);
      end; //of finally
    end; //of begin
  end; //of begin
end;

{ public TClearas.DeleteExt

  Deletes the file extension of a file name. }

class function TClearas.DeleteExt(AName: string): string;
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
    Delete(AName, Index, Index + Length(Ext)-1);

  result := AName;
end;

{ public TClearas.GetBackupDir

  Returns the path to the backup directory. }

class function TClearas.GetBackupDir(): string;
begin
  result := TOSUtils.GetWinDir + '\pss\';
end;

{ public TClearas.GetKeyValue

  Returns a Registry value as string. }

class function TClearas.GetKeyValue(AMainKey, AKeyPath, AValueName: string): string;
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

{ public TClearas.GetStartUpDir

  Returns the file system startup location of current user or all. }

class function TClearas.GetStartUpDir(AAllUsers: Boolean): string;
var
  ItemIDs: PItemIDList;
  Path: PChar;
  Folder: Cardinal;

begin
  if AAllUsers then
    folder := CSIDL_COMMON_STARTUP
  else
    folder := CSIDL_STARTUP;

  if Succeeded(SHGetSpecialFolderLocation(0, Folder, ItemIDs)) then
  begin
    Path := StrAlloc(MAX_PATH);
    SHGetPathFromIDList(ItemIDs, Path);
    result := string(Path);

    if (result[length(result)] <> '\') then
      result := result +'\';
  end;  //of begin
end;

{ public TClearas.ReadLnkFile

  Returns the path of a .exe out of a .lnk file. }

class function TClearas.ReadLnkFile(const ALnkFileName: string; out APath: string): Boolean;
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  FileInfo: TWin32FindData;

begin
  result := False;

  if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IShellLink, ShellLink)) then
  begin
    PersistFile := ShellLink as IPersistFile;

    if Succeeded(PersistFile.Load(StringToOleStr(ALnkFileName), STGM_READ)) then
      with ShellLink do
      begin
        SetLength(APath, MAX_PATH + 1);

        if Succeeded(GetPath(PChar(APath), MAX_PATH, FileInfo, SLR_ANY_MATCH)) then
        begin
          APath := PChar(APath);
          result := True;
        end;  //of begin
      end; //of with
  end;  //of begin
end;

{ public TClearas.RegisterInContextMenu

  Adds or deletes "Clearas" in recycle bin context menu. }

class function TClearas.RegisterInContextMenu(ACheck: Boolean): Boolean;
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

{ public TClearas.UpdateContextPath

  Updates "Open Clearas" in recycle bin context menu. }

class function TClearas.UpdateContextPath(): Boolean;
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

{ public TClearas.UpdateContextPath

  Updates "Open Clearas" in recycle bin context menu. }

class function TClearas.UpdateContextPath(ALangFile: TLanguageFile): Boolean;
var
  reg: TRegistry;
  ClearasKey: string;

begin
  result := False;
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

      reg.CloseKey;

      // Create new context menu key
      if reg.OpenKey(KEY_RECYCLEBIN +'\Clearas\command', True) then
      begin
        reg.WriteString('', ParamStr(0));
        result := True;
      end;  //of begin
    end  //of begin
    else
      result := False;

  finally
    reg.Free;
  end;  //of try
end;

{ public TClearas.WriteStrValue

  Writes a string value to Registry }

class procedure TClearas.WriteStrValue(AMainKey, AKeyName, AName, AValue: string);
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

{ public TRootItem.Destroy

  General destructor for destroying a TRootItem instance. }

destructor TRootItem.Destroy;
begin
  inherited Destroy;
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

{ public TRootList.Clear

  Deletes all items in the list. }

procedure TRootList.Clear;
begin
  inherited Clear;
  FActCount := 0;
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
  // Deactivation timestamp only available for disabled items
  if FEnabled then
  begin
    result := '';
    Exit;
  end;  //of begin

  result := '00.00.0000 00:00:00';
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ));

  try
    reg.RootKey := StrToHKey(FRootKey);
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
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE));
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

{ public TStartupListItem.ExportItem

  Exports an list item as .reg or .ini. }

procedure TStartupListItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    if FEnabled then
      RegFile.ExportReg(StrToHKey(FRootKey), FKeyPath, FName)
    else
      RegFile.ExportReg(StrToHKey(FRootKey), FKeyPath, False);

  finally
    RegFile.Free;
  end;  //of try
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
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE));

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    // Successfully deleted old entry?
    if not DeleteValue(FRootKey, FKeyPath, FName) then
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
      if CheckWindows then
        // Save deactivation time stamp
        WriteTime(KEY_DEACT + FName);

      // Update information
      FRootKey := 'HKLM';
      FKeyPath := KEY_DEACT + FName;
      FEnabled := False;
      reg.Free;
      result := True;
    end  //of begin
    else
      raise EStartupException.Create('Could not create "'+ KEY_DEACT + FName +'"!');

  except
    on E: Exception do
    begin
      reg.Free;
      result := False;
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
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_ALL_ACCESS));

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(FKeyPath, False);

    // Set new values
    NewHKey := reg.ReadString('hkey');
    NewKeyPath := reg.ReadString('key');
    reg.CloseKey;

    if ((NewHKey = '') or (NewKeyPath = '')) then
       raise EStartupException.Create('Could not access registry!');

    reg.RootKey := StrToHKey(NewHKey);

    // Successfully created new key?
    if reg.OpenKey(NewKeyPath, True) then
    begin
      // Write startup entry
      reg.WriteString(FName, FFilePath);

      // Delete old key
      result := DeleteKey('HKLM', KEY_DEACT, FName);

      // Update information
      FRootKey := NewHKey;
      FKeyPath := NewKeyPath;
      FEnabled := True;
      FTime := '';
      reg.Free;
    end  //of begin
    else
      raise EStartupException.Create('Could not create key "'+ NewKeyPath +'"!');

  except
    on E: Exception do
    begin
      reg.Free;
      result := False;
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupItem.CreateBackup

  Creates a backup file and returns True if successful. }

function TStartupItem.CreateBackup(): Boolean;
begin
  result := True;
end;

{ public TStartupItem.Delete

  Deletes a TStartupItem object and returns True if successful. }

function TStartupItem.Delete(): Boolean;
begin
  result := True;

  try
    if FEnabled then
    begin
      if not DeleteValue(FRootKey, FKeyPath, FName) then
        raise EStartupException.Create('Could not delete value "'+ FName +'"!');
    end  //of begin
    else
      if not DeleteKey('HKLM', KEY_DEACT, FName) then
        raise EStartupException.Create('Could not delete key "'+ FName +'"!');

  except
    on E: Exception do
    begin
      result := False;
      E.Message := 'Error while deleting "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupItem.GetItemInfo

  Returns the name and path of an item as formatted text. }

procedure TStartupItem.GetItemInfo(var AProperties: string; ALangFile: TLanguageFile);
begin
  AProperties := ALangFile.Format(46, [FName, HKeyToStr(StrToHKey(FRootKey)) +'\'+ FKeyPath]);
end;


{ TStartupUserItem }

{ private TStartupUserItem.AddCircumflex

  Replaces all backslashes in a path by circumflex. }

function TStartupUserItem.AddCircumflex(const AName: string): string;
begin
  result := StringReplace(AName, '\', '^', [rfReplaceAll]);
end;

{ private TStartupUserItem.GetBackupLnk

  Returns the path to the backup file. }

function TStartupUserItem.GetBackupLnk(): string;
begin
  result := GetBackupDir() + FName + GetExtension();
end;

{ private TStartupUserItem.GetExtension

  Returns the extension of an item used at the end of a backup file. }

function TStartupUserItem.GetExtension(): string;
begin
  if (Pos('Common', FType) <> 0) then
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
  Path, KeyName, BackupLnk: string;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE));

  try
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      KeyName := AddCircumflex(FKeyPath);
      BackupLnk := GetBackupLnk();

      if (reg.OpenKey(KEY_DEACT_FOLDER + KeyName, True) and
        ReadLnkFile(FKeyPath, Path)) then
      begin
        reg.WriteString('path', FKeyPath);
        reg.WriteString('item', DeleteExt(ExtractFileName(FName)));
        reg.WriteString('command', FFilePath);
        reg.WriteString('backup', BackupLnk);

        // Special Registry entries only for Windows >= Vista
        if CheckWindows() then
        begin
          reg.WriteString('backupExtension', GetExtension());
          reg.WriteString('location', ExtractFileDir(FKeyPath));
          WriteTime(KEY_DEACT_FOLDER + KeyName);
        end  //of begin
        else
          reg.WriteString('location', FType);

        // Create backup directory if not exist
        if not DirectoryExists(GetBackupDir()) then
          ForceDirectories(GetBackupDir());

        // Create backup .lnk
        CreateLnk(FFilePath, BackupLnk);

        // Delete original .lnk
        DeleteFile(FKeyPath);

        // Update information
        FKeyPath := KEY_DEACT_FOLDER + KeyName;
        FRootKey := 'HKLM';
        FEnabled := False;
        result := True;
      end  //of begin
      else
        raise EStartupException.Create('Could not create key "'+ FName +'"!');

    finally
      reg.Free;
    end;  //of try

  except
    on E: Exception do
    begin
      result := False;
      E.Message := 'Error while disabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ protected TStartupUserItem.Enable

  Enables an TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Enable(): Boolean;
var
  Path: string;

begin
  try
    Path := GetKeyValue('HKLM', FKeyPath, 'path');

    // Successfully created .lnk?
    if CreateLnk(FFilePath, Path) then
    begin
      // Could not delete old key?
      if not DeleteKey('HKLM', KEY_DEACT_FOLDER, AddCircumflex(Path)) then
        raise EStartupException.Create('Could not delete key "'+ FKeyPath +'"!');

      // Update information
      FKeyPath := Path;
      FRootKey := '';
      FEnabled := True;
      result := True;
    end  //of begin
    else
      raise EStartupException.Create('Could not create .lnk file!');

  except
    on E: Exception do
    begin
      result := False;
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupUserItem.CreateBackup

  Creates a backup file and returns True if successful. }

function TStartupUserItem.CreateBackup(): Boolean;
begin
  result := TClearas.CreateLnk(FFilePath, GetBackupLnk);
end;

{ public TStartupUserItem.Delete

  Deletes a TStartupUserItem object and returns True if successful. }

function TStartupUserItem.Delete(): Boolean;

  function GetKeyName(): string;
  begin
    result := AddCircumflex(TClearas.GetStartUpDir(Pos('Common', FType) <> 0) + FName);
  end;

begin
  result := True;

  try
    if FEnabled then
    begin
      // Could not delete .lnk?
      if not DeleteFile(FKeyPath) then 
        raise EStartupException.Create('Could not delete .lnk "'+ FKeyPath +'"!');
    end  //of begin
    else
      // Could not delete key?
      if not DeleteKey('HKLM', KEY_DEACT_FOLDER, GetKeyName()) then
        raise EStartupException.Create('Could not delete key "'+ FKeyPath +'"!');

  except
    on E: Exception do
    begin
      result := False;
      E.Message := 'Error while deleting "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TStartupItem.GetItemInfo

  Returns the name and path of an item as formatted text. }

procedure TStartupUserItem.GetItemInfo(var AProperties: string; ALangFile: TLanguageFile);
begin
  if FEnabled then
    AProperties := ALangFile.Format(45, [FName, FKeyPath])
  else
    AProperties := ALangFile.Format(46, [FName, HKeyToStr(StrToHKey(FRootKey)) +'\'+ FKeyPath]);
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

{ private TStartupList.GetBackupLnk

  Returns the path to the backup file. }

function TStartupList.GetBackupLnk(): string;
var
  Pss: string;

begin
  if not FItem.Enabled then
    result := TClearas.GetKeyValue('HKLM', FItem.KeyPath, 'backup')
  else
    begin
      Pss := TClearas.GetBackupDir();

      // Create backup directory only if not exists
      if not DirectoryExists(Pss) then
        ForceDirectories(Pss);

      if (Pos('Common', FItem.TypeOf) <> 0) then
        result := Pss +'\'+ FItem.Name + EXT_COMMON
      else
        result := Pss +'\'+ FItem.Name + EXT_USER;
    end;  //of begin
end;

{ private TStartupList.GetItem

  Returns a TStartupListItem object at index. }

function TStartupList.GetItem(AIndex: Word): TStartupListItem;
begin
  result := TStartupListItem(inherited Items[AIndex]);
end;

{ private TStartupList.GetLnkFileNames

  Returns a list containing all .lnk files. The boolean argument decides to
  find .lnk autostart files of current user or all users. }

procedure TStartupList.GetLnkFileNames(AFileList: TStrings; AAllUsers: Boolean);
var
  SR: TSearchRec;
  RootFolder: string;

begin
  RootFolder := TClearas.GetStartUpDir(AAllUsers);

  if (FindFirst(RootFolder + '*.lnk', faAnyFile, SR) = 0) then
    try
      while (FindNext(SR) = 0) do
        // File found?
        if (SR.Attr and faDirectory <> faDirectory) then
          AFileList.Add(RootFolder + SR.Name);

    finally
      FindClose(SR);
    end;  //of try
end;

{ private TStartupList.GetStartupUserType

  Returns the startup item type. }

function TStartupList.GetStartupUserType(const AKeyPath: string): string;
var
  st: string;

begin
  // Windows >= Vista?
  if TOSUtils.CheckWindows() then
  begin
    st := TClearas.GetKeyValue('HKLM', AKeyPath, 'backupExtension');

     if (st = EXT_COMMON) then
        result := TYPE_COMMON
     else
        result := TYPE_USER;
  end  //of begin
  else
     result := TClearas.GetKeyValue('HKLM', AKeyPath, 'location');
end;

{ protected TStartupList.AddItemDisabled

  Adds a disabled default startup item to the list. }

function TStartupList.AddItemDisabled(const AKeyPath: string): Integer;
var
  Item: TStartupListItem;

begin
  Item := TStartupItem.Create(Count, False);

  with Item do
  begin
    RootKey := 'HKLM';
    KeyPath := AKeyPath;
    Name := ExtractFileName(AKeyPath);
    FilePath := TClearas.GetKeyValue('HKLM', AKeyPath, 'command');
    Time := GetTime();
    TypeOf := TClearas.GetKeyValue('HKLM', AKeyPath, 'hkey');
  end;  //of with

  result := Add(Item);
end;

{ protected TStartupList.AddItemEnabled

  Adds a enabled default startup item to the list. }

function TStartupList.AddItemEnabled(const ARootKey, AKeyPath, AName, AFilePath: string): Integer;
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

    if (AKeyPath = KEY_RUNONCE) then
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
  AAllUsers: Boolean = False): Boolean;
var
  LnkFilePath: string;

begin
  result := False;

  if (ExtractFileExt(AName) <> '.lnk') then
    AName := AName +'.lnk';

  // Set startup location
  LnkFilePath := TClearas.GetStartUpDir(AAllUsers) + AName;

  // Link file created successfully?
  if TClearas.CreateLnk(AFilePath, LnkFilePath) then
  begin
    AddUserItemEnabled(LnkFilePath, AAllUsers);
    result := True;
  end  //of begin
  else
    raise EStartupException.Create('Could not create .lnk file!');
end;

{ protected TStartupList.AddUserItemDisabled

  Adds a disabled startup user item to the list. }

function TStartupList.AddUserItemDisabled(const AKeyPath: string): Integer;
var
  Item: TStartupListItem;

begin
  Item := TStartupUserItem.Create(Count, False); 

  with Item do 
  begin
    RootKey := 'HKLM';
    KeyPath := AKeyPath;
    Name := ExtractFileName(DelCircumflex(AKeyPath));
    FilePath := TClearas.GetKeyValue('HKLM', AKeyPath, 'command');
    Time := GetTime();
    TypeOf := GetStartupUserType(AKeyPath);
  end;  //of with

  result := Add(Item);
end;

{ protected TStartupList.AddUserItemEnabled

  Adds a enabled startup user item to the list. }

function TStartupList.AddUserItemEnabled(const ALnkFile: string; AAllUsers: Boolean): Integer;
var
  Item: TStartupListItem;
  ExeFile: string;

begin
  Item := TStartupUserItem.Create(Count, True);

  with Item do
  begin
    RootKey := '';
    ReadLnkFile(ALnkFile, ExeFile);
    KeyPath := ALnkFile;
    FilePath := ExeFile;
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
  result := FItem.CreateBackup();
end;

{ public TStartupList.AddProgram

  Checks if a backup file already exists. }

function TStartupList.AddProgram(const AFilePath: string;
  ANickName: string = ''): Boolean;
var
  Name, Ext: string;

begin
  result := False;
  Name := ExtractFileName(AFilePath);
  Ext := ExtractFileExt(Name);

  // Check invalid extension
  if ((Ext <> '.exe') or (Ext <> '.lnk') or (Ext <> '.bat')) then
    raise EInvalidArgument.Create('Invalid program extension! Must be ".exe"'
      +', ".lnk" or ".bat"!');

  // Entry already exists?
  if (IndexOf(Name) <> -1) then
    Exit;

  if ((Ext = '.exe') or (Ext = '.lnk')) then
  begin
    if (ANickName <> '') then
      Name := ANickName
    else
      Name := TClearas.DeleteExt(Name);

    result := AddNewStartupUserItem(Name, AFilePath);
  end  //of begin
  else
    begin
      // Adds new startup item to Registry
      TClearas.WriteStrValue('HKCU', KEY_STARTUP, ANickName, AFilePath);

      // Adds this item to list
      AddItemEnabled('HKCU', KEY_STARTUP, ANickName, AFilePath);
      result := True;
    end;  //of begin
end;


{ public TStartupList.BackupExists

  Checks if a backup file already exists. }

function TStartupList.BackupExists(): Boolean;
begin
  result := False;

  if not Assigned(FItem) then
    raise EInvalidItem.Create('No item selected!');

  if FItem.StartupUser then
    result := FileExists(GetBackupLnk());
end;

{ public TStartupList.ChangeItemStatus

  Changes the item status. }

function TStartupList.ChangeItemStatus(): Boolean;
var
  Changed: Boolean;

begin
  result := False;

  if not Assigned(FItem) then
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

{ public TStartupList.ChangeItemFilePath

  Changes the item file path. }

function TStartupList.ChangeItemFilePath(const ANewFilePath: string): Boolean;
begin
  result := False;

  if FItem.Enabled then
    TClearas.WriteStrValue(FItem.RootKey, FItem.KeyPath, FItem.Name, ANewFilePath)
  else
    begin
      TClearas.WriteStrValue(FItem.RootKey, FItem.KeyPath, 'Command', ANewFilePath);
      TClearas.WriteStrValue(FItem.RootKey, FItem.KeyPath, 'Item', Item.Name);
    end;  //of if

  result := True;
end;

{ public TStartupList.DeleteItem

  Deletes an item from Registry and list. }

function TStartupList.DeleteItem(): Boolean;
var
  Deleted: Boolean;

begin
  result := False;

  if not Assigned(FItem) then
    raise EInvalidItem.Create('No item selected!');

  // Delete item from Registry
  Deleted := FItem.Delete();

  // Successful?
  if Deleted then
  begin
    // Item was enabled
    if FItem.Enabled then
    begin
      // Decide to delete backup
      if (FDeleteBackup and FItem.StartupUser) then
        DeleteBackupLnk();

      // Update active counter
      Dec(FActCount);
    end;  //of begin

    // Remove item from list
    inherited Remove(FItem);
    FItem := nil;
  end;  //of begin

  result := Deleted;
end;

{ public TStartupList.ExportItem

  Exports an item as .reg or .ini file. }

procedure TStartupList.ExportItem(const AFileName: string; ARegFile: Boolean = True);
begin
  if not Assigned(FItem) then
    raise EInvalidItem.Create('No item selected!');

  FItem.ExportItem(AFileName);
end;

{ public TStartupList.ExportList

  Exports the complete list as .reg or .ini file. }

procedure TStartupList.ExportList(const AFileName: string);
var
  i: Word;
  RegFile: TRegistryFile;
  Item: TStartupListItem;

begin
  //init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    for i := 0 to Count -1 do
    begin
      Item := GetItem(i);
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
  Path, Name, Ext: string;

begin
  result := False;
  Ext := ExtractFileExt(AFilePath);

  // Check invalid extension
  if ((Ext <> EXT_COMMON) or (Ext <> EXT_USER)) then
    raise EInvalidArgument.Create('Invalid backup file extension! Must be "'
      + EXT_COMMON +'" or "'+ EXT_USER+'"!');

  // Set the name of item
  Name := ExtractFileName(TClearas.DeleteExt(AFilePath)) +'.lnk';

  // Entry already exists?
  if (IndexOf(Name) <> -1) then
    Exit;

  // Extract path to .exe
  if not TClearas.ReadLnkFile(AFilePath, Path) then
    raise EStartupException.Create('Could not read backup file!');

  // Create .lnk file and add it to list
  result := AddNewStartupUserItem(Name, Path, Ext = EXT_COMMON);
end;

{ public TStartupList.IndexOf

  Returns the index of an item checking name only. }

function TStartupList.IndexOf(AName: string): Integer;
var
  i: Integer;

begin
  result := -1;

  for i := 0 to Count -1 do
    if (GetItem(i).Name = AName) then
    begin
      result := i;
      Break;
    end;  //of begin
end;

{ public TStartupList.IndexOf

  Returns the index of an item checking name and status. }

function TStartupList.IndexOf(AName: string; AEnabled: Boolean): Integer;
var
  i: Integer;

begin
  result := -1;

  for i := 0 to Count -1 do
    if ((GetItem(i).Name = AName) and (GetItem(i).Enabled = AEnabled))then
    begin
      result := i;
      Break;
    end;  //of begin
end;

{ public TStartupList.LoadAutostart

  Searches for startup items at different locations. }

procedure TStartupList.LoadAutostart(AIncludeRunOnce: Boolean);
begin
  LoadEnabled('HKLM', KEY_STARTUP);

  if TClearas.IsWindows64() then
    LoadEnabled('HKLM', KEY_STARTUP32);

  // Read RunOnce entries?
  if AIncludeRunOnce then
  begin
    LoadEnabled('HKLM', KEY_RUNONCE);
    LoadEnabled('HKCU', KEY_RUNONCE);
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
    if not DeleteKey('HKCR', ExtractFileDir(KeyPath), FName) then
      raise EStartupException.Create('Could not delete key!')
    else
      result := True;

  except
    on E: Exception do
    begin
      result := False;
      E.Message := 'Error while deleting "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

{ public TContextListItem.ExportItem

  Exports an list item as .reg or .ini. }

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
  Text := TClearas.GetKeyValue('HKCR', GetKeyPath(), '');
  AProperties := ALangFile.Format(47, [FName, Text]);
end;


{ TShellItem }

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
    WriteStrValue('HKCR', KeyPath, 'LegacyDisable', '');
    FEnabled := False;
    result := True;

  except
    on E: Exception do
    begin
      result := False;
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
    if not DeleteValue('HKCR', KeyPath, 'LegacyDisable') then
      raise EStartupException.Create('Could not delete value "'+ KeyPath + '\LegacyDisable' +'"!')
    else
       result := True;

    FEnabled := True;

  except
    on E: Exception do
    begin
      result := False;
      E.Message := 'Error while enabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //try
end;


{ TShellExItem }

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
    OldValue := GetKeyValue('HKCR', KeyPath, '');
    WriteStrValue('HKCR', KeyPath, '', '-' + OldValue);
    FEnabled := False;
    result := True;

  except
    on E: Exception do
    begin
      result := False;
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
    OldValue := GetKeyValue('HKCR', KeyPath, '');
    NewValue := Copy(OldValue, 2, Length(OldValue));
    WriteStrValue('HKCR', KeyPath, '', NewValue);
    FEnabled := True;
    result := True;

  except
    on E: Exception do
    begin
      result := False;
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
end;

{ private TContextList.Add

  Adds a TContextListItem to the list and returns position. }

function TContextList.Add(AItem: TContextListItem): Word;
begin
  result := inherited Add(AItem);
end;

{ private TContextList.Add

  Checks if an TContextListItem already exists in list. }

function TContextList.FindDouble(AName, AKeyName: string): Boolean;
begin
  result := False;

  if ((Count > 0) and (IndexOf(AName) <> -1)) then
    result := (GetItem(IndexOf(AName)).TypeOf = AKeyName);
end;

{ private TContextList.GetItem

  Returns a TContextListItem object at index. }

function TContextList.GetItem(AIndex: Word): TContextListItem;
begin
  result := TContextListItem(inherited Items[AIndex]);
end;

{ protected TContextList.AddShellItem

  Adds a shell item to list. }

function TContextList.AddShellItem(const AName, ALocation: string; AEnabled: Boolean): Word;
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

function TContextList.AddShellExItem(const AName, ALocation: string; AEnabled: Boolean): Word;
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

procedure TContextList.AddEntry(const AKeyName: string; AShell: Boolean);
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

{ protected TContextList.AddEntry

  Searches for context menu entries and adds them to the list. }

procedure TContextList.AddEntry();
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
                    AddEntry(Hkcr[i]);
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

procedure TContextList.AddEntry(const AKeyName: string);
begin
  AddEntry(AKeyName, True);
  AddEntry(AKeyName, False);
end;

{ public TContextList.ChangeItemStatus

  Changes the item status. }

function TContextList.ChangeItemStatus(): Boolean;
var
  Changed: Boolean;

begin
  result := False;

  if not Assigned(FItem) then
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
  result := False;

  if not Assigned(FItem) then
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

  Exports an item as .reg or .ini file. }

procedure TContextList.ExportItem(const AFileName: string);
begin
  if not Assigned(FItem) then
    raise EInvalidItem.Create('No item selected!');

  FItem.ExportItem(AFileName);
end;

{ public TContextList.IndexOf

  Returns the index of an item checking name only. }

function TContextList.IndexOf(AName: string): Integer;
var
  i: Integer;

begin
  result := -1;

  for i := 0 to Count -1 do
    if (GetItem(i).Name = AName) then
    begin
      result := i;
      Break;
    end;  //of begin
end;

{ public TContextList.IndexOf

  Returns the index of an item checking name and location. }

function TContextList.IndexOf(AName, ALocation: string): Integer;
var
  i: Integer;

begin
  result := -1;

  for i := 0 to Count -1 do
    if ((GetItem(i).Name = AName) and (GetItem(i).Location = ALocation)) then
    begin
      result := i;
      Break;
    end;  //of begin
end;

{ public TContextList.LoadContextMenus

  Searches for context menu entries at different locations. }

procedure TContextList.LoadContextMenus();
begin
  AddEntry('AllFilesystemObjects');
  AddEntry('Directory');
  AddEntry('Folder');
  AddEntry('Drive');
  //AddEntry('CLSID\{645FF040-5081-101B-9F08-00AA002F954E}', True);
end;

end.
