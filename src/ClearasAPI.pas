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
  Contnrs, OSUtils, LanguageFile;

const
  { Registry Keys }
  KEY_RECYCLEBIN = 'CLSID\{645FF040-5081-101B-9F08-00AA002F954E}\shell';
  KEY_STARTUP = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP32 = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';
  KEY_RUNONCE = 'SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce';
  KEY_DEACT = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupreg\';
  KEY_DEACT_FOLDER = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupfolder\';
  KEY_CONTEXTMENU = '\shellex\ContextMenuHandlers';

  { URL }
  URL_BASE = 'http://www.pm-codeworks.de/';
  URL_CONTACT = URL_BASE +'kontakt.html';

  { Extensions von Backup-Dateien }
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
    class function ExpandHKey(const ARootKey: string): string;
    class function GetKeyValue(AMainKey, AKeyPath, AValueName: string): string;
    class function GetStartUpDir(AAllUsers: Boolean): string;
    class function ReadLnkFile(const ALnkFileName: string; out APath: string): Boolean;
    class function RegisterInContextMenu(ACheck: Boolean; ALangFile: TLanguageFile): Boolean;
    class function UpdateContextPath(ALangFile: TLanguageFile): Boolean;
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
    procedure GetItemInfo(var AName, APath: string; ALangFile: TLanguageFile); virtual; abstract;
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
    procedure GetItemInfo(var AName, APath: string; ALangFile: TLanguageFile); override;
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
    procedure GetItemInfo(var AName, APath: string; ALangFile: TLanguageFile); override;
  end;

  { TStartupList }
  TStartupList = class(TRootList)
  private
    FActAppIndex: PInt;
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
    function AddUserItemDisabled(const AKeyPath: string): Integer;
    function AddUserItemEnabled(const ALnkFile: string; AAllUsers: Boolean): Integer;
  public
    constructor Create;
    function CreateBackup(): Boolean;
    procedure AddDisabled(const AKeyPath: string);
    procedure AddEnabled(const AAllUsers: Boolean); overload;
    procedure AddEnabled(const ARootKey, AKeyPath: string); overload;
    function AddNewStartupItem(const AName, AFilePath: string): Boolean;
    function AddNewStartupUserItem(const AName, AFilePath: string;
      AAllUsers: Boolean = False): Boolean;
    function BackupExists(): Boolean;
    procedure ExportItem(const AFileName: string; ARegFile: Boolean = True);
    procedure ExportList(const AFileName: string);
    function ChangeItemStatus(): Boolean; override;
    function ChangeItemFilePath(const ANewFilePath: string): Boolean;
    function DeleteItem(): Boolean; override;
    function IndexOf(AName: string): Integer; overload;
    function IndexOf(AName: string; AEnabled: Boolean): Integer; overload;
    procedure Insert(AIndex: Integer; AItem: TStartupListItem);
    procedure Load(ARunOnce: Boolean);
    { external }
    property AppIndex: PInt read FActAppIndex write FActAppIndex;
    property DeleteBackup: Boolean read FDeleteBackup write FDeleteBackup;
    property Items[AIndex: Word]: TStartupListItem read GetItem; default;
    property Item: TStartupListItem read FItem write FItem;
  end;

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
    procedure GetItemInfo(var AName, APath: string; ALangFile: TLanguageFile); override;
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

  { Search events }
  TOnSearchBeginEvent = procedure(Sender: TObject; AWorkCountMax: Integer) of object;
  TOnSearchEvent = procedure(Sender: TObject; AWorkCount: Integer) of object;
  TOnSearchEndEvent = procedure(Sender: TObject) of object;

  { TContextList }
  TContextList = class(TRootList)
  private
    FItem: TContextListItem;
    FCountMax, FProgress: Integer;
    FWorkCount: TOnSearchEvent;
    FWorkCountMax: TOnSearchBeginEvent;
    FContextCount: TOnSearchEndEvent;
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
    procedure Insert(AIndex: Integer; AItem: TContextListItem); virtual;
    procedure Load;
    { external }
    property Items[AIndex: Word]: TContextListItem read GetItem; default;
    property Item: TContextListItem read FItem write FItem;
    property OnSearch: TOnSearchEvent read FWorkCount write FWorkCount;
    property OnSearchBegin: TOnSearchBeginEvent read FWorkCountMax write FWorkCountMax;
    property OnSearchEnd: TOnSearchEndEvent read FContextCount write FContextCount;
  end;

implementation

{ TClearas }

{ public TClearas.DeleteKey

  Deletes a Registry key. }

class function TClearas.DeleteKey(AMainKey, AKeyPath, AKeyName: string): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(SetKeyAccessMode);

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
  reg := TRegistry.Create(SetKeyAccessMode);

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

{ public TClearas.ExpandHKey

  Converts a short form HKEY string represantation to long form. }

class function TClearas.ExpandHKey(const ARootKey: string): string;
begin
  result := HKeyToStr(StrToHKey(ARootKey));
end;

{ public TClearas.GetKeyValue

  Returns a Registry value as string. }

class function TClearas.GetKeyValue(AMainKey, AKeyPath, AValueName: string): string;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(SetKeyAccessMode);

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

{ public TClearas.GetStartUpDir

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

  Registers "Open Clearas" in recycle bin context menu. }

class function TClearas.RegisterInContextMenu(ACheck: Boolean;
  ALangFile: TLanguageFile): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(SetKeyAccessMode);
  reg.RootKey := HKEY_CLASSES_ROOT;
  reg.OpenKey(KEY_RECYCLEBIN, False);

  if not ACheck then
  begin
    try
      with reg do
      begin
        CreateKey(ALangFile.GetString(71));
        CloseKey;
        OpenKey(KEY_RECYCLEBIN +'\'+ ALangFile.GetString(71), True);
        CreateKey('command');
        CloseKey;
        OpenKey(KEY_RECYCLEBIN + ALangFile.GetString(72), True);
        WriteString('', ParamStr(0));
        CloseKey;
      end;  //of with

      result := True;

    except
      result := False;
    end;  //of try
  end  //of begin
  else
    begin
      try
        reg.DeleteKey(ALangFile.GetString(71));
        reg.CloseKey;
        result := False;

      except
        result := True;
      end;  //of try
    end;  //of if

  reg.Free;
end;

{ public TClearas.UpdateContextPath

  Updates "Open Clearas" in recycle bin context menu. }

class function TClearas.UpdateContextPath(ALangFile: TLanguageFile): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(SetKeyAccessMode);
  reg.RootKey := HKEY_CLASSES_ROOT;

  try
    reg.OpenKey(KEY_RECYCLEBIN, False);

    // Only update if context menu entry exists
    if (reg.KeyExists(ALangFile.GetString(71))) then
    begin
      reg.CloseKey;
      reg.OpenKey(KEY_RECYCLEBIN + ALangFile.GetString(72), False);
      reg.WriteString('', ParamStr(0));
      result := True;
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
  reg := TRegistry.Create(SetKeyAccessMode);

  try
    reg.RootKey := StrToHKey(AMainKey);
    reg.OpenKey(AKeyName, True);
    reg.WriteString(AName, AValue);
    reg.Free;

  except
    on E: Exception do
    begin
      reg.Free;
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

{ public TStartupListItem.GetTime

  Returns the deactivation time stamp. }

function TStartupListItem.GetTime(): string;
var
  reg: TRegistry;
  Year, Month, Day, Hour, Min, Sec: Word;
  Date, Time: string;

begin
  reg := TRegistry.Create(SetKeyAccessMode);             //init reg-Object

  try
    if not FEnabled then
       begin
       reg.RootKey := StrToHKey(FRootKey);               //HKEY öffnen
       reg.OpenKey(FKeyPath, False);                     //SubKey öffnen

       if not reg.ValueExists('YEAR') then               //Wert existiert?
          result := '00.00.0000 00:00:00'
       else
          begin
          Year := reg.ReadInteger('YEAR');               //auslesen...
          Month := reg.ReadInteger('MONTH');
          Day := reg.ReadInteger('DAY');
          Hour := reg.ReadInteger('HOUR');
          Min := reg.ReadInteger('MINUTE');
          Sec := reg.ReadInteger('SECOND');
          Date := FormatDateTime('c', EncodeDate(Year, Month, Day));
          Time := FormatDateTime('tt', EncodeTime(Hour, Min, Sec, 0));
          result := Date +'  '+ Time;
          end;  //of if
       end;  //of if

    reg.Free;

  except
    reg.Free;
    result := '00.00.0000 00:00:00';
  end;  //of try
end;

{ public TStartupListItem.WriteTime

  Writes the deactivation time stamp. }

procedure TStartupListItem.WriteTime(const AKeyPath: string);
var
  reg: TRegistry;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  TimeNow: TDateTime;

begin
  reg := TRegistry.Create(SetKeyAccessMode);      //init REG-Object
  reg.RootKey := HKEY_LOCAL_MACHINE;              //HKEY öffnen

  try
    reg.OpenKey(AKeyPath, True);                  //SubKey öffnen
    TimeNow := Now();
    FTime := FormatDateTime('c', TimeNow);        //Deaktivierungsdatum
    DecodeDate(TimeNow, Year, Month, Day);        //Datum auslesen
    DecodeTime(TimeNow, Hour, Min, Sec, MSec);    //Zeit auslesen

    with reg do
      begin                                       //Deaktivierungsdatum speichern
      WriteInteger('YEAR', Year);
      WriteInteger('MONTH', Month);
      WriteInteger('DAY', Day);
      WriteInteger('HOUR', Hour);
      WriteInteger('MINUTE', Min);
      WriteInteger('SECOND', Sec);
      end;  //of with

    reg.Free;

  except
    on E: Exception do
    begin
      reg.Free;
      E.Message := 'Error while writing deactivation time to "'+ AKeyPath +'": '+ E.Message;
      raise;
    end;  //of begin
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
  deleted, created: Boolean;

begin
  reg := TRegistry.Create(SetKeyAccessMode);

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    // Eintrag löschen
    deleted := DeleteValue(FRootKey, FKeyPath, FName);

    // Neuen Schlüssel erstellen
    if deleted then
      created := reg.OpenKey(KEY_DEACT + FName, True)
    else
      created := False;

    // Autostarteintrag gelöscht und neuer Schlüssel erstellt?
    if ( deleted and created ) then
      begin
      reg.WriteString('hkey', FRootKey);             //Werte schreiben...
      reg.WriteString('key', FKeyPath);
      reg.WriteString('item', FName);
      reg.WriteString('command', FFilePath);
      reg.WriteString('inimapping', '0');

      if CheckWindows then                           //nur ab Windows Vista:
         WriteTime(KEY_DEACT + FName);               //Deaktivierungsdatum speichern

      FRootKey := 'HKLM';                            //Daten aktualisieren
      FKeyPath := KEY_DEACT + FName;
      FEnabled := False;                             //Status aktualisieren
      reg.Free;                                      //Objekt freigeben
      result := True;
      end  //of begin
    else
      raise EStartupException.Create('Could not create "'+ KEY_DEACT + FName +'"!');

  except
    on E: Exception do
    begin
      reg.Free;                                      //Objekt freigeben
      result := False;
      E.Message := 'Error while disabling "'+ FName +'": '+ E.Message;
      raise;
    end;  //of begin
  end;  //of except
end;

{ protected TStartupItem.Enable

  Enables an TStartupItem object and returns True if successful. }

function TStartupItem.Enable(): Boolean;
var
  reg: TRegistry;
  NewHKey, NewKeyPath: string;

begin
  reg := TRegistry.Create(SetKeyAccessMode);      //init REG-Object

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(FKeyPath, False);                 //alten Schlüssel öffnen
    NewHKey := reg.ReadString('hkey');            //neuer HKEY auslesen
    NewKeyPath := reg.ReadString('key');          //neuer Pfad auslesen
    reg.CloseKey;

    if ((NewHKey = '') or (NewKeyPath = '')) then //Fehlererkennung
       raise EStartupException.Create('Could not access registry!')
    else
       begin
       reg.RootKey := StrToHKey(NewHKey);

       if reg.OpenKey(NewKeyPath, True) then
          begin
          reg.WriteString(FName, FFilePath);      //Autostarteintrag schreiben
          result := DeleteKey('HKLM', KEY_DEACT, FName);  //alten Schlüssel löschen

          FRootKey := NewHKey;                    //Daten aktualisieren
          FKeyPath := NewKeyPath;
          FEnabled := True;                       //Status aktualisieren
          FTime := '';
          reg.Free;                               //freigeben
          end  //of begin
       else
          raise EStartupException.Create('Could not create key "'+ NewKeyPath +'"!')
       end;  //of if

  except
    on E: Exception do
    begin
      reg.Free;                                   //freigeben
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

procedure TStartupItem.GetItemInfo(var AName, APath: string; ALangFile: TLanguageFile);
begin
  APath := ALangFile.GetString(61) +^J+^J+ ExpandHKey(FRootKey) +'\'+ FKeyPath;
  AName := ALangFile.GetString(62) +'"'+ FName +'"';
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
  result := GetWinDir +'\pss\'+ FName + GetExtension();
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
  Path, KeyName, PssDir, BackupLnk: string;

begin
  reg := TRegistry.Create(SetKeyAccessMode);            //init REG-Zugriff

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;                  //RootKey setzen
    KeyName := AddCircumflex(FKeyPath);
    PssDir := GetWinDir +'\pss';
    BackupLnk := GetBackupLnk();

    if (reg.OpenKey(KEY_DEACT_FOLDER + KeyName, True) and
       ReadLnkFile(FKeyPath, Path)) then
       begin                                            //Daten schreiben
       reg.WriteString('path', FKeyPath);
       reg.WriteString('item', DeleteExt(ExtractFileName(FName)));
       reg.WriteString('command', FFilePath);
       reg.WriteString('backup', BackupLnk);

       if CheckWindows then                             //nur ab Windows Vista
          begin
          reg.WriteString('backupExtension', GetExtension());
          reg.WriteString('location', ExtractFileDir(FKeyPath));
          WriteTime(KEY_DEACT_FOLDER + KeyName);            //Deaktivierungsdatum speichern
          end  //of begin
       else                                             // <= Windows XP
          reg.WriteString('location', FType);

       if not DirectoryExists(PssDir) then              //existiert Dir nicht?
          ForceDirectories(PssDir);                     //dann Dir erstellen

       CreateLnk(FFilePath, BackupLnk);                 //Backup *.lnk erstellen
       DeleteFile(FKeyPath);                            //*.lnk löschen
       FKeyPath := KEY_DEACT_FOLDER + KeyName;              //Reg-Pfad aktualisieren
       FRootKey := 'HKLM';
       FEnabled := False;
       reg.Free;                                        //freigeben
       result := True;
       end  //of begin
    else
       raise EStartupException.Create('Could not create key "'+ FName +'"!');

  except
    on E: Exception do
    begin
      reg.Free;                                         //freigeben
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

    if CreateLnk(FFilePath, Path) then                     //Verknüpfung anlegen
       begin
       if not DeleteKey('HKLM', KEY_DEACT_FOLDER, AddCircumflex(Path)) then
          raise EStartupException.Create('Could not delete key "'+ FKeyPath +'"!')
       else
          begin
          FKeyPath := Path;                                //aktualisieren
          FRootKey := '';
          FEnabled := True;
          result := True;
          end;  //of if
       end  //of begin
    else
       raise EStartupException.Create('Could not create link!');

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
    if FEnabled then                           //Schlüssel oder Datei löschen
       begin
       if not DeleteFile(FKeyPath) then        //*.lnk löschen
          raise EStartupException.Create('Could not delete link "'+ FKeyPath +'"!');
       end  //of begin
    else
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

procedure TStartupUserItem.GetItemInfo(var AName, APath: string; ALangFile: TLanguageFile);
begin
  if FEnabled then
     APath := ALangFile.GetString(60) +^J+^J+ FKeyPath
  else
     APath := ALangFile.GetString(61) +^J+^J+ ExpandHKey(FRootKey) +'\'+ FKeyPath;

  AName := ALangFile.GetString(62) +'"'+ FName +'"';
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
     Pss := TClearas.GetWinDir +'\pss';

     if not DirectoryExists(Pss) then      //existiert Dir nicht?
        ForceDirectories(Pss);             //dann Dir erstellen

     if (Pos('Common', FItem.TypeOf) <> 0) then
        result := Pss +'\'+ FItem.Name + EXT_COMMON
     else
        result := Pss +'\'+ FItem.Name + EXT_USER;
     end;
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
        // File found
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
  if not TClearas.CheckWindows then                //falls <= WinXP
     result := TClearas.GetKeyValue('HKLM', AKeyPath, 'location')
  else                                             //falls >= Win Vista
     begin
     st := TClearas.GetKeyValue('HKLM', AKeyPath, 'backupExtension');

     if (st = EXT_COMMON) then
        result := TYPE_COMMON
     else
        result := TYPE_USER;
     end;  //of if
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

{ protected TStartupList.AddUserItemDisabled

  Adds a disabled startup user item to the list. }

function TStartupList.AddUserItemDisabled(const AKeyPath: string): Integer;
var
  Item: TStartupListItem;

begin
  Item := TStartupUserItem.Create(Count, False);      //neues Objekt

  with Item do                                        //Daten übergeben
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

{ public TStartupList.AddDisabled

  Searches for disabled items in AKeyPath and adds them to the list. }

procedure TStartupList.AddDisabled(const AKeyPath: string);
var
  reg: TRegistry;
  List: TStringList;
  i: Integer;

begin
  List := TStringList.Create;
  reg := TRegistry.Create(TClearas.SetKeyAccessMode); //init REG-Objekt
  reg.RootKey := HKEY_LOCAL_MACHINE;                //Root Key setzen

  try
    reg.OpenKey(AKeyPath, True);                    //Key öffnen
    reg.GetKeyNames(List);                          //enthaltene Schlüsselnamen auslesen

    for i := 0 to List.Count -1 do
      if (AKeyPath = KEY_DEACT) then                //auslesen + eintragen
         AddItemDisabled(AKeyPath + List[i])
      else
         AddUserItemDisabled(AKeyPath + List[i]);

  finally
    reg.Free;
    List.Free;
  end;  //of finally
end;

{ public TStartupList.AddEnabled

  Searches for enabled startup user items and adds them to the list. }

procedure TStartupList.AddEnabled(const AAllUsers: Boolean);
var
  List: TStringList;
  i: integer;

begin
  List := TStringList.Create;

  try
    GetLnkFileNames(List, AAllUsers);             //aktivierte Autostart-Programme auslesen

    for i := 0 to List.Count -1 do
      AddUserItemEnabled(List[i], AAllUsers);     //Programm in Liste schreiben

  finally
    List.Free;
  end;  //of finally
end;

{ public TStartupList.AddEnabled

  Searches for enabled items in ARootKey and AKeyPath and adds them to the list. }

procedure TStartupList.AddEnabled(const ARootKey, AKeyPath: string);
var
  reg: TRegistry;
  List: TStringList;
  i: Integer;
  FilePath: string;

begin
  List := TStringList.Create;
  reg := TRegistry.Create(TClearas.SetKeyAccessMode(AKeyPath));  //init REG-Objekt

  try
    reg.RootKey := TClearas.StrToHKey(ARootKey);    //Root Key setzen
    reg.OpenKey(AKeyPath, True);                    //Key öffnen
    reg.GetValueNames(List);                        //enthaltene Einträge auslesen

    for i := 0 to List.Count -1 do
    begin
      FilePath := reg.ReadString(List[i]);          //Pfad zur *.exe auslesen
      AddItemEnabled(ARootKey, AKeyPath, List[i], FilePath);  //Programm auslesen + eintragen
    end;  //of for

  finally
    reg.Free;
    List.Free;
  end;  //of finally
end;

{ public TStartupList.AddNewStartupItem

  Adds a new default startup item to the autostart. }

function TStartupList.AddNewStartupItem(const AName, AFilePath: string): Boolean;
begin
  // Try to add new startup item to registry
  try
    TClearas.WriteStrValue('HKCU', KEY_STARTUP, AName, AFilePath);
    AddItemEnabled('HKCU', KEY_STARTUP, AName, AFilePath);

  except
    result := False;
  end;  //of try
end;

{ public TStartupList.AddNewStartupItem

  Adds a new startup user item to the autostart. }

function TStartupList.AddNewStartupUserItem(const AName, AFilePath: string;
  AAllUsers: Boolean = False): Boolean;
var
  LnkFilePath: string;

begin
  // Try to add new startup user item to startup folder
  try
    LnkFilePath := TClearas.GetStartUpDir(AAllUsers) + AName;

    // Link file created successfully?
    if TClearas.CreateLnk(AFilePath, LnkFilePath) then
       begin
       AddUserItemEnabled(LnkFilePath, AAllUsers);
       result := True;
       end  //of begin
    else
       raise EStartupException.Create('Link file could not be created!');

  except
    result := False;
  end;  //of try
end;

{ public TStartupList.BackupExists

  Checks if a backup file already exists. }

function TStartupList.BackupExists(): Boolean;
begin
  if FItem.StartupUser then
    result := FileExists(GetBackupLnk())
  else
    result := False;
end;

{ public TStartupList.ChangeItemStatus

  Changes the item status. }

function TStartupList.ChangeItemStatus(): Boolean;
var
  Changed: Boolean;

begin
  try
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

  except
    result := False;
    raise;
  end;  //of try
end;

{ public TStartupList.ChangeItemFilePath

  Changes the item file path. }

function TStartupList.ChangeItemFilePath(const ANewFilePath: string): Boolean;
begin
  try
    if FItem.Enabled then
       TClearas.WriteStrValue(FItem.RootKey, FItem.KeyPath, FItem.Name, ANewFilePath)
    else
       begin
       TClearas.WriteStrValue(FItem.RootKey, FItem.KeyPath, 'Command', ANewFilePath);
       TClearas.WriteStrValue(FItem.RootKey, FItem.KeyPath, 'Item', Item.Name);
       end;  //of if

    result := True;

  except
    result := False;
  end;  //of try
end;

{ public TStartupList.DeleteItem

  Deletes an item from Registry and list. }

function TStartupList.DeleteItem(): Boolean;
var
  Deleted: Boolean;

begin
  try
    // Delete item from Registry
    Deleted := FItem.Delete();

    // Successful?
    if Deleted then
      // Item was enabled
      if FItem.Enabled then
      begin
        // Decide to delete backup
        if (FDeleteBackup and FItem.StartupUser) then
          DeleteBackupLnk();

        // Update active counter
        Dec(FActCount);

        // Remove item from list
        inherited Remove(FItem);
        FItem := nil;
      end;  //of begin

    result := Deleted;

  except
    result := False;
    raise;
  end;  //of try
end;

{ public TStartupList.ExportItem

  Exports an item as .reg or .ini file. }

procedure TStartupList.ExportItem(const AFileName: string; ARegFile: Boolean = True);
begin
  FItem.ExportItem(AFileName);
end;

{ public TStartupList.ExportList

  Exports the complete list as .reg or .ini file. }

procedure TStartupList.ExportList(const AFileName: string);
var
  i: Word;
  RegFile: TRegistryFile;
  Item: RootItem;

begin
  //init Reg file
  RegFile := TRegistryFile.Create(AFileName);

  try
    LastItem := nil;

    for i := 0 to FCount -1 do
    begin
      Item := GetItem(i);
      RegFile.ExportKey(Item.RootKey, Item.KeyPath, True);
    end;  //of for

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
  end;  //of try
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

{ public TStartupList.Insert

  Inserts a TStartupListItem object at index. }

procedure TStartupList.Insert(AIndex: Integer; AItem: TStartupListItem);
begin
  inherited Insert(AIndex, AItem);
end;

{ public TStartupList.Load

  Searches for startup items at different locations. }

procedure TStartupList.Load(ARunOnce: Boolean);
begin
  AddEnabled('HKLM', KEY_STARTUP);

  if TOSUtils.IsWindows64() then
    AddEnabled('HKLM', KEY_STARTUP32);

  // Read RunOnce entries?
  if ARunOnce then
  begin
    AddEnabled('HKLM', KEY_RUNONCE);
    AddEnabled('HKCU', KEY_RUNONCE);
  end;  //of begin

  AddEnabled('HKCU', KEY_STARTUP);
  AddEnabled(True);
  AddEnabled(False);
  AddDisabled(KEY_DEACT);
  AddDisabled(KEY_DEACT_FOLDER);
end;


{ TContextItem }

{ public TContextListItem.Delete

  Deletes a TContextListItem object and returns True if successful. }

function TContextListItem.Delete(): Boolean;
begin
  try
    if not DeleteKey('HKCR', ExtractFileDir(KeyPath), FName) then
       begin
       result := False;
       raise EStartupException.Create('Could not delete key!');
       end  //of begin
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

{ public TContextListItem.GetItemInfo

  Returns the name and path of an item as formatted text. }

procedure TContextListItem.GetItemInfo(var AName, APath: string; ALangFile: TLanguageFile);
var
  Text: string;

begin
  Text := TClearas.GetKeyValue('HKCR', GetKeyPath(), '');
  APath := ALangFile.GetString(87) +':'+ ^J+^J +'"'+ Text +'"';
  AName := ALangFile.GetString(62) +'"'+ FName +'"';
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
       begin
       result := False;
       raise EStartupException.Create('Could not delete value "'+ KeyPath + '\LegacyDisable' +'"!');
       end  //of begin
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

  if (Count > 0) then
     if (IndexOf(AName) <> -1) then
        result := GetItem(IndexOf(AName)).TypeOf = AKeyName;
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
  Key: string;
  Enabled: Boolean;

begin
  reg := TRegistry.Create(TClearas.SetKeyAccessMode);
  List := TStringList.Create;

  if AShell then
     Key := AKeyName +'\shell'
  else
     Key := AKeyName + KEY_CONTEXTMENU;

  try
    reg.RootKey := HKEY_CLASSES_ROOT;                     //Root Key setzen
    reg.OpenKey(Key, False);                              //Key öffnen
    reg.GetKeyNames(List);                                //alle Einträge im Key auslesen

    for i := 0 to List.Count -1 do
      begin
      reg.CloseKey;
      reg.OpenKey(Key +'\'+ List[i], False);

      if ((reg.ReadString('') <> '') and (List[i][1] <> '{') and
         (reg.ReadString('')[1] <> '@') and not FindDouble(List[i], AKeyName)) then  //wichtige, leere und doppelte Einträge filtern!
         begin
         if AShell then                                   //nach "Shell" suchen
            begin
            Enabled := not reg.ValueExists('LegacyDisable');  //Status
            AddShellItem(List[i], AKeyName, Enabled);     //neues Item in Liste
            end  //of begin
         else                                             //sonst nach "ShellEx" suchen
            begin
            Enabled := reg.ReadString('')[1] <> '-';      //Status
            AddShellExItem(List[i], AKeyName, Enabled);   //neues Item in Liste
            end  //of begin
         end;  //of begin
      end;  //of for

  finally                                                 //freigeben
    List.Free;
    reg.Free;
  end;  //of finally
end;

{ protected TContextList.AddEntry

  Searches for context menu entries and adds them to the list. }

procedure TContextList.AddEntry();
var
  reg: TRegistry;
  i, j, k: integer;
  Hkcr, Temp, Shellex: TStringList;

begin
  reg := TRegistry.Create(KEY_READ);             //init REG-Object
  Hkcr := TStringList.Create;                    //init Liste HKCR
  Temp := TStringList.Create;                    //init Liste zum Suchen
  Shellex := TStringList.Create;                 //init Liste ShellEx

  try
    reg.RootKey := HKEY_CLASSES_ROOT;            //Root Key setzen

    if not reg.KeyExists('$$$_auto_file') then   //falls "Dummy Eintrag" nicht existiert...
       reg.CreateKey('$$$_auto_file');           //erstellen, um Bug zu vermeiden

    reg.OpenKey('', False);                      //Key öffnen
    reg.GetKeyNames(Hkcr);                       //alle Einträge im Key auslesen
    FCountMax := Hkcr.Count;                     //Max auf Anzahl der Schlüssel
    FWorkCountMax(Self, FCountMax);              //"Beginn der Suche" melden
    FProgress := 0;                              //Progress-Bar Status-Reset

    for i := 0 to Hkcr.Count -1 do
      begin
      Inc(FProgress);                            //Progress-Zähler
      FWorkCount(Self, FProgress);               //Progress anzeigen

      reg.CloseKey;
      reg.OpenKey(Hkcr[i], False);               //Key öffnen

      if reg.HasSubKeys then                     //existiert Unterschlüssel?
         begin
         Temp.Clear;
         reg.GetKeyNames(Temp);

         for j := 0 to Temp.Count -1 do
           if ((Temp[j] = 'shellex') or (Temp[j] = 'ShellEx')) then
              begin
              reg.CloseKey;
              reg.OpenKey(Hkcr[i] +'\'+ Temp[j], False); //Key öffnen

              if reg.HasSubKeys then             //existiert Unterschlüssel?
                 begin
                 Shellex.Clear;
                 reg.GetKeyNames(Shellex);

                 for k := 0 to Shellex.Count -1 do
                   if (Shellex[k] = 'ContextMenuHandlers') then
                      begin
                      reg.CloseKey;
                      reg.OpenKey(Hkcr[i] +'\'+ Temp[j] +'\'+ Shellex[k], False);

                      if reg.HasSubKeys then     //existiert Unterschlüssel...
                         AddEntry(Hkcr[i]);      //dann Eintrag gefunden und hinzufügen
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
    FContextCount(Self);                         //"Ende der Suche" melden
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
  try
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

  except
    result := False;
    raise;
  end;  //of try
end;

{ public TContextList.DeleteItem

  Deletes an item from Registry and list. }

function TContextList.DeleteItem(): Boolean;
var
  Deleted: Boolean;

begin
  try
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

  except
    result := False;
    raise;
  end;  //of try
end;

{ public TContextList.ExportItem

  Exports an item as .reg or .ini file. }

procedure TContextList.ExportItem(const AFileName: string);
begin
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

{ public TContextList.Insert

  Inserts a TContextListItem object at index. }

procedure TContextList.Insert(AIndex: Integer; AItem: TContextListItem);
begin
  inherited Insert(AIndex, AItem);
end;

{ public TContextList.Load

  Searches for context menu entries at different locations. }

procedure TContextList.Load;
begin
  AddEntry('AllFilesystemObjects');
  AddEntry('Directory');
  AddEntry('Folder');
  AddEntry('Drive');
  //AddEntry('CLSID\{645FF040-5081-101B-9F08-00AA002F954E}', True);
end;

end.
