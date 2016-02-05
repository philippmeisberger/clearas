{ *********************************************************************** }
{                                                                         }
{ Clearas API Interface Unit                                              }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasAPI;

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Windows, WinSvc, Classes, SysUtils, Registry, ShlObj, ActiveX, ComObj, Zip,
  Graphics, CommCtrl, ShellAPI, SyncObjs, StrUtils, Variants, Generics.Collections,
  Taskschd, PMCWOSUtils, PMCWLanguageFile, PMCWIniFileParser, KnownFolders;

type
  /// <summary>
  ///   A <c>TLnkFile</c> represents a symbolic link file.
  /// </summary>
  TLnkFile = class(TObject)
  private
    FFileName,
    FExeFileName: TFileName;
    FArguments: string;
    function GetFullPath(): string;
    function GetFullPathEscaped(): string;
    procedure SetArguments(const AArguments: string);
    procedure SetExeFileName(const AExeFileName: TFileName);
    procedure SetFileName(const AFileName: TFileName);
  protected
    /// <summary>
    ///   Saves a .lnk file.
    /// </summary>
    /// <param name="AFileName">
    ///   The filename to a non-existent .lnk file.
    /// </param>
    /// <param name="AExeFileName">
    ///   The filename to a .exe file.
    /// </param>
    /// <param name="AArguments">
    ///  Optional arguments to a .exe file.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the .lnk file was successfully written or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Save(AFileName, AExeFileName: TFileName;
      AArguments: string = ''): Boolean; overload;
  public
    /// <summary>
    ///   Constructor for creating a <c>TLnkFile</c> instance.
    /// </summary>
    /// <param name="AFileName">
    ///   The filename of the .lnk file.
    /// </param>
    constructor Create(AFileName: TFileName); reintroduce;

    /// <summary>
    ///   Deletes the .lnk file.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the .lnk file was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean;

    /// <summary>
    ///   Checks if the .lnk file exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the .lnk file exists or <c>False</c> otherwise.
    /// </returns>
    function Exists(): Boolean;

    /// <summary>
    ///   Checks if arguments are specified.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if arguments are specified or <c>False</c> otherwise.
    /// </returns>
    function HasArguments(): Boolean;

    /// <summary>
    ///    Reads the contents from a .lnk file. Only after all properties can be
    ///    used.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the .lnk file was successfully read or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Read(): Boolean;

    /// <summary>
    ///   Saves the .lnk file.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the .lnk file was successfully written or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Save(): Boolean; overload;

    /// <summary>
    ///   Gets or sets the command line arguments to the target .exe file.
    /// </summary>
    property Arguments: string read FArguments write SetArguments;

    /// <summary>
    ///   Gets or sets the filename to the .exe file.
    /// </summary>
    property ExeFileName: TFileName read FExeFileName write SetExeFileName;

    /// <summary>
    ///   Gets or sets the filename to the .lnk file.
    /// </summary>
    property FileName: TFileName read FFileName write SetFileName;

    /// <summary>
    ///   Returns the concatenation of filename and arguments.
    /// </summary>
    property FullPath: string read GetFullPath;

    /// <summary>
    ///   Returns the concatenation of file name and arguments escaped in quotes.
    /// </summary>
    property FullPathEscaped: string read GetFullPathEscaped;
  end;

  /// <summary>
  ///   A <c>TStartupUserLnkFile</c> represents a startup link file.
  /// </summary>
  TStartupLnkFile = class(TLnkFile)
  strict private
    FStartupUser: Boolean;
    function GetBackupLnk(): string; deprecated 'Since Windows 8';
    function GetBackupExt(): string;
  public
    /// <summary>
    ///   Constructor for creating a <c>TStartupLnkFile</c> instance.
    /// </summary>
    /// <param name="AFileName">
    ///   The filename of the .lnk file.
    /// </param>
    constructor Create(AFileName: TFileName); reintroduce; overload;

    /// <summary>
    ///   Constructor for creating a <c>TStartupLnkFile</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The name of the .lnk file.
    /// </param>
    /// <param name="AStartupUser">
    ///   If set to <c>True</c> the startup location for the current user is
    ///   used. Otherwise the startup location for all users is used.
    /// </param>
    constructor Create(const AName: string; AStartupUser: Boolean); overload;

    /// <summary>
    ///   Constructor for creating a <c>TStartupLnkFile</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The name of the .lnk file.
    /// </param>
    /// <param name="AStartupUser">
    ///   If set to <c>True</c> the startup location for the current user is
    ///   used. Otherwise the startup location for all users is used.
    /// </param>
    /// <param name="AExeFileName">
    ///   The absolute .exe filename.
    /// </param>
    /// <param name="AArguments">
    ///   Optional arguments passed to the .exe.
    /// </param>
    constructor Create(const AName: string; AStartupUser: Boolean;
      AExeFileName: TFileName; AArguments: string = ''); overload;

    /// <summary>
    ///   Checks if the backup .lnk file in <c>C:\Windows\pss\</c> exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the backup exists or <c>False</c> otherwise.
    /// </returns>
    function BackupExists(): Boolean; deprecated 'Since Windows 8';

    /// <summary>
    ///   Creates a backup .lnk file in <c>C:\Windows\pss\</c>.
    /// </summary>
    procedure CreateBackup(); deprecated 'Since Windows 8';

    /// <summary>
    ///   Deletes the backup .lnk file in <c>C:\Windows\pss\</c>.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the backup was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function DeleteBackup(): Boolean; deprecated 'Since Windows 8';

    /// <summary>
    ///   Gets the path to the backup directory.
    /// </summary>
    /// <returns>
    ///   The backup directory.
    /// </returns>
    class function GetBackupDir(): string; static; deprecated 'Since Windows 8';

    /// <summary>
    ///   Gets the filesystem startup location.
    /// </summary>
    /// <param name="AStartupUser">
    ///   If set to <c>True</c> the startup location for the current user is
    ///   returned. Otherwise the startup location for all users is returned.
    /// </param>
    /// <returns>
    ///   The startup directory.
    /// </returns>
    class function GetStartUpDir(AStartupUser: Boolean): string; static;

    /// <summary>
    ///   Gets extension of the backup file.
    /// </summary>
    property BackupExt: string read GetBackupExt;

    /// <summary>
    ///   Gets the path to the backup .lnk file in <c>C:\Windows\pss\</c>.
    /// </summary>
    /// <remarks>
    ///   Deprecated since Windows 8!
    /// </remarks>
    property BackupLnk: string read GetBackupLnk;

    /// <summary>
    ///   The .lnk file is located in the startup folder of the current user
    ///   or located in startup folder of all users.
    /// </summary>
    property StartupUser: Boolean read FStartupUser;
  end;

  { Exception classes }
  EInvalidItem = class(EAccessViolation);
  EListBlocked = class(EAbort);
  EWarning = class(EAbort);

  /// <summary>
  ///   A <c>TRootItem</c> represents an basic list item that can be added to a
  ///   <c>TRootList<c/>.
  /// </summary>
  /// <remarks>
  ///   This class is intended to be only ancenstor for items.
  /// </remarks>
  TRootItem = class(TObject)
  private
    function GetArguments(): string;
    function GetFileNameOnly(): string;
  protected
    FEnabled: Boolean;
    FFileName: string;
    FName: string;
    FLocation: string;
    FCaption: string;
    procedure ChangeFilePath(const ANewFileName: string); virtual;
    procedure ChangeStatus(const ANewStatus: Boolean); virtual;
    function DeleteQuoteChars(const APath: string): string;
    function ExtractArguments(const APath: string): string;
    function ExtractPathToFile(const APath: string): string;
    function GetFullLocation(): string; virtual; abstract;
    function GetFileDescription(AFileName: TFileName): string;
    function GetIcon(): HICON; overload; virtual;
    function GetIcon(AExeFileName: TFileName): HICON; overload;
    function GetLocation(): string; virtual;
    procedure Rename(const ANewName: string); virtual;
  public
    /// <summary>
    ///   Constructor for creating a <c>TRootItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="AFileName">
    ///   The filename to an .exe file.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    constructor Create(const AName, ACaption, AFileName, ALocation: string;
      AEnabled: Boolean); reintroduce;

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; virtual; abstract;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); virtual; abstract;

    /// <summary>
    ///   Checks if the .exe file exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the file in property <see cref="FileName"/> exists or
    ///   <c>False</c> otherwise.
    /// </returns>
    function FileExists(): Boolean;

    /// <summary>
    ///   Gets the filter for file export.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The filter formatted as <c>"File type *.ext|*.ext"</c>
    /// </returns>
    function GetExportFilter(ALanguageFile: TLanguageFile): string; virtual; abstract;

    /// <summary>
    ///   Gets the item status as translated text.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The status as text.
    /// </returns>
    function GetStatus(ALanguageFile: TLanguageFile): string;

    /// <summary>
    ///   Opens the folder that contains the .exe file in Windows Explorer and
    ///   selects this file. If the file does not exist a <see cref="EWarning"/>
    ///   is raised.
    /// </summary>
    procedure OpenInExplorer();

    /// <summary>
    ///   Extracts the arguments to the .exe file in <see cref="FileName"/>.
    /// </summary>
    property Arguments: string read GetArguments;

    /// <summary>
    ///   Gets or sets the display name.
    /// </summary>
    property Caption: string read FCaption;

    /// <summary>
    ///   Gets or sets the enabled status.
    /// </summary>
    property Enabled: Boolean read FEnabled write ChangeStatus;

    /// <summary>
    ///   Gets or sets the filename with arguments.
    /// </summary>
    property FileName: string read FFileName write ChangeFilePath;

    /// <summary>
    ///   Gets the filename without arguments.
    /// </summary>
    property FileNameOnly: string read GetFileNameOnly;

    /// <summary>
    ///   Gets the icon of the .exe file.
    /// </summary>
    property Icon: HICON read GetIcon;

    /// <summary>
    ///   Gets the store location. Usually a Registry key.
    /// </summary>
    property Location: string read GetLocation;

    /// <summary>
    ///   Gets the complete store location.
    /// </summary>
    property LocationFull: string read GetFullLocation;

    /// <summary>
    ///   Gets or sets the internal name.
    /// </summary>
    property Name: string read FName write Rename;
  end;

  /// <summary>
  ///   A <c>TRegistryItem</c> represents a basic list item that is located in
  ///   the Windows Registry. This class extends the capabilities of
  ///   <see cref="TRootItem"/>.
  /// </summary>
  TRegistryItem = class(TRootItem)
  strict private
    FWow64: Boolean;
  protected
    function DeleteKey(AHKey: HKEY; AKeyPath, AKeyName: string;
      AFailIfNotExists: Boolean = True): Boolean;
    function GetFullLocation(): string; override;
    function GetRootKey(): TRootKey; virtual; abstract;

    /// <summary>
    ///   Opens the item location in either 32 or 64 bit RegEdit.
    /// </summary>
    /// <param name="AWow64">
    ///   If set to <c>True</c> use the 32 bit RegEdit otherwise use the 64 bit.
    /// </param>
    procedure OpenInRegEdit(AWow64: Boolean); overload;

    /// <summary>
    ///   Writes the deactivation timestamp to the Registry and returns it.
    /// </summary>
    /// <param name="AReg">
    ///   A <c>TRegistry</c> instance.
    /// </param>
    /// <returns>
    ///   The deactivation timestamp.
    /// </returns>
    function WriteTimestamp(const AReg: TRegistry): TDateTime;
  public
    /// <summary>
    ///   Constructor for creating a <c>TRegistryItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="AFileName">
    ///   The filename to an .exe file.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AWow64">
    ///   Use the WOW64 technology.
    /// </param>
    constructor Create(const AName, ACaption, AFileName, ALocation: string;
      AEnabled, AWow64: Boolean);

    /// <summary>
    ///   Gets the filter for file export.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The filter formatted as <c>"File type *.ext|*.ext"</c>
    /// </returns>
    function GetExportFilter(ALanguageFile: TLanguageFile): string; override;

    /// <summary>
    ///   Gets the deactivation timestamp which is stored in the Registry.
    /// </summary>
    /// <param name="AReg">
    ///   A <c>TRegistry</c> instance.
    /// </param>
    /// <returns>
    ///   The deactivation timestamp.
    /// </returns>
    function GetTimestamp(AReg: TRegistry): TDateTime;

    /// <summary>
    ///   Opens the item location in RegEdit.
    /// </summary>
    procedure OpenInRegEdit(); overload; virtual;

    /// <summary>
    ///   Gets the root Registry key.
    /// </summary>
    /// <returns>
    ///    The root Registry key.
    /// </returns>
    property RootKey: TRootKey read GetRootKey;

    /// <summary>
    ///   Item is redirected by WOW64.
    /// </summary>
    property Wow64: Boolean read FWow64;
  end;

  /// <summary>
  ///   The possible basically item changes.
  /// </summary>
  TItemStatus = (
    stEnabled, stDisabled, stDeleted
  );

  { Events }
  TItemChangeEvent = procedure(Sender: TObject; ANewStatus: TItemStatus) of object;
  TSearchEvent = procedure(Sender: TObject; AProgress, AProgressMax: Cardinal) of object;
  TSearchErrorEvent = procedure(Sender: TObject; AErrorMessage: string) of object;

  /// <summary>
  ///   This interface declares the capability to import an exported backup file.
  /// </summary>
  IImportableList = interface
  ['{19B8FB63-483A-4F54-80C4-A25FBBAC7891}']
    /// <summary>
    ///   Imports a backup file.
    /// </summary>
    /// <param name="AFileName">
    ///   The backup file.
    ///  </param>
    /// <returns>
    ///   <c>True</c> if the import was successful or <c>False</c> otherwise.
    /// </returns>
    function ImportBackup(const AFileName: TFileName): Boolean;

    /// <summary>
    ///   Gets the file filter for an <c>TOpenDialogy</c> from a <c>TLanguageFile</c>.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The import file filter.
    /// </returns>
    function GetImportFilter(ALanguageFile: TLanguageFile): string;
  end;

  /// <summary>
  ///   A <c>TRootList</c> is the basic list that contains a set of
  ///   <see cref="TRootItem"/>s. The list is thread-safe and supports locking.
  ///   You can use the <see cref="IsLocked"/> method to see if the list is
  ///   locked.
  /// </summary>
  /// <remarks>
  ///   This class is intended to be used only as ancestor for other classes.
  /// </remarks>
  TRootList<T: TRootItem> = class(TObjectList<T>, IInterface)
  strict private
    FItem: T;
    FOnChanged: TItemChangeEvent;
    FOnSearchStart,
    FOnSearchFinish: TNotifyEvent;
    FOnSearching: TSearchEvent;
    FOnSearchError: TSearchErrorEvent;
  protected
    FEnabledItemsCount: Integer;
    FInvalid: Boolean;
    FLock: TCriticalSection;
    procedure DoNotifyOnChanged(ANewStatus: TItemStatus);
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    /// <summary>
    ///   Constructor for creating a <c>TRootList</c> instance.
    /// </summary>
    constructor Create;

    /// <summary>
    ///   Destructor for destroying a <c>TRootList</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Adds an item to the list.
    /// </summary>
    /// <param name="AItem">
    ///   The item.
    /// </param>
    /// <returns>
    ///   The index of the item. If the item was not added <c>-1</c> is
    ///   returned.
    /// </returns>
    function Add(const AItem: TRootItem): Integer; reintroduce;

    /// <summary>
    ///   Changes the file path of the current selected item.
    /// </summary>
    procedure ChangeItemFilePath(const ANewFilePath: string); virtual;

    /// <summary>
    ///   Changes the item status of the current selected item.
    /// </summary>
    /// <param name="ANewStatus">
    ///   The new status.
    /// </param>
    procedure ChangeItemStatus(const ANewStatus: Boolean); virtual;

    /// <summary>
    ///   Removes all items from the list.
    /// </summary>
    procedure Clear();

    /// <summary>
    ///   Deletes the current selected item.
    /// </summary>
    /// <returns>
    ///   <c>>True</c> if item was successfully deleted or <c>False</c> otherwise.
    /// </returns>
    function DeleteItem(): Boolean; virtual;

    /// <summary>
    ///   Disables the current selected item.
    /// </summary>
    procedure DisableItem();

    /// <summary>
    ///   Notifies that the list needs a visual update.
    /// </summary>
    procedure DoNotifyOnFinished();

    /// <summary>
    ///   Enables the current selected item.
    /// </summary>
    procedure EnableItem();

    /// <summary>
    ///   Exports the current selected item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); virtual;

    /// <summary>
    ///   Exports the complete list as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportList(const AFileName: string); virtual; abstract;

    /// <summary>
    ///   Gets the filter for file export.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The filter formatted as <c>"File type *.ext|*.ext"</c>
    /// </returns>
    function GetExportFilter(ALanguageFile: TLanguageFile): string; virtual;

    /// <summary>
    ///   Searches for an item in the list identified by name or caption.
    /// </summary>
    /// <param name="ANameOrCaption">
    ///   The name or caption of the item.
    /// </param>
    /// <returns>
    ///   The index of the item in the list. If the item does not exist <c>-1</c>
    ///   is returned.
    /// </returns>
    function IndexOf(const ANameOrCaption: string): Integer; overload;

    /// <summary>
    ///   Searches for an item in the list identified by name or caption and
    ///   status.
    /// </summary>
    /// <param name="ANameOrCaption">
    ///   The name or caption of the item.
    /// </param>
    /// <param name="AEnabled">
    ///   The item status.
    /// </param>
    /// <returns>
    ///   The index of the item in the list. If the item does not exist <c>-1</c>
    ///   is returned.
    /// </returns>
    function IndexOf(const ANameOrCaption: string; AEnabled: Boolean): Integer; overload;

    /// <summary>
    ///   Signals that list needs an visual update.
    /// </summary>
    procedure Invalidate();

    /// <summary>
    ///   Checks if the list is currently locked.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the list is locked or <c>False</c> otherwise.
    /// </returns>
    function IsLocked(): Boolean;

    /// <summary>
    ///   Searches items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </param>
    procedure Load(AExpertMode: Boolean = False); virtual; abstract;

    /// <summary>
    ///   Renames the current selected item.
    /// </summary>
    /// <param name="ANewName">
    ///   The new name.
    /// </param>
    procedure RenameItem(const ANewName: string); virtual;

    /// <summary>
    ///   Gets the count of enabled items in the list.
    /// </summary>
    property EnabledItemsCount: Integer read FEnabledItemsCount;

    /// <summary>
    ///   Gets or sets the list visual state. If set to <c>True</c> and the list
    ///   is selected on UI the content is refreshed visually.
    /// </summary>
    property IsInvalid: Boolean read FInvalid write FInvalid;

    /// <summary>
    ///   Occurs when an item has changed.
    /// </summary>
    property OnChanged: TItemChangeEvent read FOnChanged write FOnChanged;

    /// <summary>
    ///   Occurs when item search is in progress.
    /// </summary>
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;

    /// <summary>
    ///   Occurs when item search has failed.
    /// </summary>
    property OnSearchError: TSearchErrorEvent read FOnSearchError write FOnSearchError;

    /// <summary>
    ///   Occurs when item search has finished.
    /// </summary>
    property OnSearchFinish: TNotifyEvent read FOnSearchFinish write FOnSearchFinish;

    /// <summary>
    ///   Occurs when item search has started.
    /// </summary>
    property OnSearchStart: TNotifyEvent read FOnSearchStart write FOnSearchStart;

    /// <summary>
    ///   Gets or sets the current selected item.
    /// </summary>
    property Selected: T read FItem write FItem;
  end;

const
  { Startup registry keys until Windows 7 }
  KEY_STARTUP_DISABLED        = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupreg\' deprecated;
  KEY_STARTUP_USER_DISABLED   = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupfolder\' deprecated;

  { Startup registry keys since Windows 8 }
  KEY_STARTUP_APPROVED        = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\StartupApproved\';
  KEY_STARTUP_RUN_APPROVED    = KEY_STARTUP_APPROVED +'Run';
  KEY_STARTUP_RUN32_APPROVED  = KEY_STARTUP_APPROVED +'Run32';
  KEY_STARTUP_USER_APPROVED   = KEY_STARTUP_APPROVED +'StartupFolder';

  { General startup keys }
  KEY_STARTUP_RUN             = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP_RUNONCE         = 'SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce';

  { Redirected 32-Bit Registry keys by WOW64 }
  KEY_STARTUP_RUN32           = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';
  KEY_STARTUP_RUNONCE32       = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce';

  { Extensions of backup files }
  EXT_STARTUP_COMMON          = '.CommonStartup';
  EXT_STARTUP_USER            = '.Startup';

  { Description type of startup user items }
  STARTUP_COMMON              = 'Startup Common';
  STARTUP_COMMON_XP           = 'Common Startup' deprecated;
  STARTUP_USER                = 'Startup User';
  STARTUP_USER_XP             = 'Startup' deprecated;

  /// <summary>
  ///   Used in <see cref="TStartupItemStatus"/> to signal that a startup item 
  ///   is enabled.
  /// </summary>
  ST_ENABLED                  = $2;

  /// <summary>
  ///   Used in <see cref="TStartupItemStatus"/> to signal that a startup item 
  ///   is disabled.
  /// </summary>
  ST_DISABLED                 = $3;

type
  { Exception class }
  EStartupException = class(Exception);

  /// <summary>
  ///   A <c>TStartupItemStatus</c> represents the new status of startup items
  ///   since Windows 8. The status of startup items is a binary value of 12.
  ///   digits. The first 4 bytes contain the enabled value. This can be
  ///   <c>ST_ENABLED</c> or <c>ST_DISABLED</c>. In case the value is
  ///   <c>ST_DISABLED</c> the last 8 bytes are a timestamp containing the
  ///   deactivation time as <c>TFileTime</c>.
  /// </summary>
  TStartupItemStatus = record
    /// <summary>
    ///   The status value. Can be either <see cref="ST_ENABLED"/> or
    ///   <see cref="ST_DISABLED"/>.
    /// </summary>
    Status: UINT;
    /// <summary>
    ///   The deactivation time.
    /// </summary>
    DeactivationTime: TFileTime;
  end;

  /// <summary>
  ///   A <c>TStartupListItem</c> represents a basic startup item that can be
  ///   added to a <see cref="TStartupList"/>.
  /// </summary>
  /// <remarks>
  ///   This class is intended to be only ancenstor for items.
  /// </remarks>
  TStartupListItem = class(TRegistryItem)
  private
    function GetTime(): TDateTime;
  protected
    FTime: TDateTime;
    FRootKey: TRootKey;
    procedure ChangeFilePath(const ANewFileName: string); override;
    procedure ChangeStatus(const ANewStatus: Boolean); override;
    function Disable(): Boolean; virtual; deprecated 'Since Windows 8'; abstract;
    function Enable(): Boolean; virtual; deprecated 'Since Windows 8'; abstract;
    function DateTimeToFileTime(const AFileTime: TDateTime): TFileTime;
    function DeleteValue(AKeyPath: string; AReallyWow64: Boolean = True): Boolean;
    function GetApprovedLocation(): string; virtual;
    function GetRootKey(): TRootKey; override;
    function GetWow64Key(): string; virtual;
    procedure Rename(const ANewName: string); overload; override;
    function Rename(const AKeyPath, ANewName: string;
      AReallyWow64: Boolean = True): Boolean; reintroduce; overload;
  public
    /// <summary>
    ///   Constructor for creating a <c>TStartupListItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="AFileName">
    ///   The filename to an .exe file.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="ARootKey">
    ///   The root Registry key.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AWow64">
    ///   Use the WOW64 technology.
    /// </param>
    constructor Create(const AName, AFileName, ALocation: string;
      ARootKey: TRootKey; AEnabled, AWow64: Boolean);

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Opens the item location in RegEdit.
    /// </summary>
    procedure OpenInRegEdit(); override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;

    /// <summary>
    ///   Gets the root Registry key.
    /// </summary>
    /// <returns>
    ///    The root Registry key.
    /// </returns>
    property RootKey: TRootKey read GetRootKey;

    /// <summary>
    ///   Gets or sets the deactivation timstamp.
    /// </summary>
    property Time: TDateTime read GetTime write FTime;

    /// <summary>
    ///   Gets the virtual Registry key redirected by WOW64.
    /// </summary>
    /// <returns>
    ///    The WOW64 Registry key.
    /// </returns>
    property Wow64Location: string read GetWow64Key;
  end;

  /// <summary>
  ///   A <c>TStartupItem</c> represents a default startup item that can be
  ///   added to a <see cref="TStartupList"/> and are located in the Registry.
  /// </summary>
  TStartupItem = class(TStartupListItem)
  private
    FRunOnce: Boolean;
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    function GetWow64Key(): string; override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TStartupItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="AFileName">
    ///   The filename to an .exe file.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="ARootKey">
    ///   The root Registry key.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AWow64">
    ///   Use the WOW64 technology.
    /// </param>
    /// <param name="ARunOnce">
    ///   Item is a RunOnce item and so located in a RunOnce Registry key.
    /// </param>
    constructor Create(const AName, AFileName, ALocation: string;
      ARootKey: TRootKey; AEnabled, AWow64, ARunOnce: Boolean);

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;

    /// <summary>
    ///   If <c>True</c> then item is located in a <c>RunOnce</c> Registry key.
    ///   Otherwise the item is located in a <c>Run</c> Registry key (default).
    /// </summary>
    property RunOnce: Boolean read FRunOnce;
  end;

  /// <summary>
  ///   A <c>TStartupItem</c> represents a user startup item that can be added
  ///   to a <see cref="TStartupList"/>. Those items are located in the autostart
  ///   filesystem folder and are a .lnk file.
  /// </summary>
  TStartupUserItem = class(TStartupListItem)
  private
    FLnkFile: TStartupLnkFile;
    function AddCircumflex(const AName: string): string;
    function GetStartupUser: Boolean;
  protected
    procedure ChangeFilePath(const ANewFileName: string); override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    function GetApprovedLocation(): string; override;
    function GetFullLocation(): string; override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TStartupUserItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="AFileName">
    ///   The filename to an .exe file.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="ARootKey">
    ///   The root Registry key.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="ALnkFile">
    ///   The corresponding .lnk file.
    /// </param>
    constructor Create(const AName, AFileName, ALocation: string;
      ARootKey: TRootKey; AEnabled: Boolean; ALnkFile: TStartupLnkFile);

    /// <summary>
    ///   Destructor for destroying a <c>TStartupUserItem</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Gets the filter for file export.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The filter formatted as <c>"File type *.ext|*.ext"</c>
    /// </returns>
    function GetExportFilter(ALanguageFile: TLanguageFile): string; override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;

    /// <summary>
    ///   If <c>True</c> the item is located in the startup folder of the
    ///   current user. Otherwise the item is located in startup folder of all
    ///   users.
    /// </summary>
    property StartupUser: Boolean read GetStartupUser;

    /// <summary>
    ///   Gets or sets the .lnk file.
    /// </summary>
    property LnkFile: TStartupLnkFile read FLnkFile write FLnkFile;
  end;

  /// <summary>
  ///   The possible startup locations.
  /// </summary>
  TStartupLocation = (
    slHkcuRun,
    slHkcuRunOnce,
    slHklmRun,
    slHklmRun32,
    slHklmRunOnce,
    slHklmRunOnce32,
    slStartupUser,
    slCommonStartup
  );

  TStartupLocationHelper = record helper for TStartupLocation
    /// <summary>
    ///   Gets the approved startup location.
    /// </summary>
    /// <returns>
    ///   The location.
    /// </returns>
    function GetApprovedLocation(): TPair<HKEY, string>;

    /// <summary>
    ///   Gets the startup store location.
    /// </summary>
    /// <returns>
    ///   The location.
    /// </returns>
    function GetLocation(): TPair<HKEY, string>;
  end;

  /// <summary>
  ///   A <c>TStartupList</c> is the list that contains a set of
  ///   <see cref="TStartupListItem"/>s. The list is thread-safe and supports
  ///   locking. You can use the <see cref="IsLocked"/> method to see if the
  ///   list is locked.
  /// </summary>
  TStartupList = class(TRootList<TStartupListItem>, IImportableList)
  private
    FDeleteBackup: Boolean;
    function AddNewStartupUserItem(const AName: string; AFileName: TFileName;
      AArguments: string = ''; AStartupUser: Boolean = True): Boolean;
    function AddUserItem(ALnkFile: TStartupLnkFile; AStartupUser: Boolean): Integer;
    function DeleteBackupFile(): Boolean; deprecated 'Since Windows 8';
    function LoadStatus(const AName: string;
      AStartupLocation: TStartupLocation): TPair<Boolean, TDateTime>;
  public
    /// <summary>
    ///   Constructor for creating a <c>TStartupList</c> instance.
    /// </summary>
    constructor Create;

    /// <summary>
    ///   Adds an item to the list.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute .exe filename.
    /// </param>
    /// <param name="AArguments">
    ///   Optional arguments passed to the .exe.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the item was successfully added or <c>False</c> otherwise.
    /// </returns>
    function Add(const AFileName, AArguments, ACaption: string): Boolean; overload;

    /// <summary>
    ///   Checks if a startup user backup file already exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the backup file already exists or <c>False</c> otherwise.
    /// </returns>
    function BackupExists(): Boolean; deprecated 'Since Windows 8';

    /// <summary>
    ///   Changes the item status of the current selected item.
    /// </summary>
    /// <param name="ANewStatus">
    ///   The new status.
    /// </param>
    procedure ChangeItemStatus(const ANewStatus: Boolean); override;

    /// <summary>
    ///   Deletes the current selected item.
    /// </summary>
    /// <returns>
    ///   <c>>True</c> if item was successfully deleted or <c>False</c> otherwise.
    /// </returns>
    function DeleteItem(): Boolean; override;

    /// <summary>
    ///   Exports the complete list as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportList(const AFileName: string); override;

    /// <summary>
    ///   Gets the file filter for an <c>TOpenDialogy</c> from a <c>TLanguageFile</c>.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The import file filter.
    /// </returns>
    function GetImportFilter(ALanguageFile: TLanguageFile): string;

    /// <summary>
    ///   Imports a backup file.
    /// </summary>
    /// <param name="AFileName">
    ///   The backup file.
    ///  </param>
    /// <returns>
    ///   <c>True</c> if the import was successful or <c>False</c> otherwise.
    /// </returns>
    function ImportBackup(const AFileName: TFileName): Boolean;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </param>
    /// <remarks>
    ///   Asynchronous: A thread is launched! The <see cref="OnSearchStart"/>
    ///   event occurs when the search starts. At the end the
    ///   <see cref="OnSearchFinish"/> event occurs.
    /// </remarks>
    procedure Load(AExpertMode: Boolean = False); overload; override;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AStartupLocation">
    ///   The startup location to search for items.
    /// </param>
    procedure Load(AStartupLocation: TStartupLocation); reintroduce; overload;

    /// <summary>
    ///   Searches for disabled items and adds them to the list.
    /// </summary>
    /// <param name="AStartupUser">
    ///   If set to <c>True</c> only search for startup user items. If set to
    ///   <c>False</c> only search for default startup items.
    /// </param>
    procedure LoadDisabled(AStartupUser: Boolean); deprecated 'Since Windows 8';

    /// <summary>
    ///   Gets or sets the behaviour that startup user backup files should be
    ///   automatically deleted after enabling or not.
    /// </summary>
    /// <remarks>
    ///   Deprecated since Windows 8!
    /// </remarks>
    property AutoDeleteBackup: Boolean read FDeleteBackup write FDeleteBackup;
  end;

const
  { Context menu Registry subkeys + values}
  KEY_USERCHOICE              = 'Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\%s\UserChoice';
  KEY_COMMAND_STORE           = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\CommandStore\shell';
  CM_SHELL                    = 'Shell';
  CM_SHELL_DISABLED           = 'LegacyDisable';
  CM_SHELLEX                  = 'ShellEx';
  CM_SHELLEX_HANDLERS         = CM_SHELLEX +'\ContextMenuHandlers';
  CM_SHELLEX_FILE             = 'CLSID\%s\InProcServer32';
  CM_SHELLNEW                 = 'ShellNew';
  CM_SHELLNEW_DISABLED        = '_'+ CM_SHELLNEW;
  CM_LOCATIONS_DEFAULT        = 'Directory, Folder, *, Drive';

type
  { Exception class }
  EContextMenuException = class(Exception);

  /// <summary>
  ///   A <c>TContextListItem</c> represents a basic context menu item that can
  ///   be added to a <see cref="TContextList"/>. All context menu items are
  ///   in the Registry.
  /// </summary>
  /// <remarks>
  ///   This class is intended to be only ancenstor for items.
  /// </remarks>
  TContextListItem = class(TRegistryItem)
  protected
    function GetRootKey(): TRootKey; override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Deletes a user defined program association.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the association was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function DeleteUserChoice(const AFileExtension: string): Boolean;

    /// <summary>
    ///   Checks if a user defined program association exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the association exists or <c>False</c> otherwise.
    /// </returns>
    function UserChoiceExists(const AFileExtension: string): Boolean;

    /// <summary>
    ///   Gets the root location e.g. <c>Drives</c>.
    /// </summary>
    property LocationRoot: string read FLocation;
  end;

  /// <summary>
  ///   A <c>TShellItem</c> represents a shell context menu item that can be
  ///   added to a <see cref="TContextList"/>. Those items are in plain-text.
  /// </summary>
  TShellItem = class(TContextListItem)
  private
    FExtended: Boolean;
  protected
    procedure ChangeFilePath(const ANewFileName: string); override;
    procedure ChangeStatus(const ANewStatus: Boolean); override;
    function GetIcon(): HICON; override;
    function GetLocation(): string; override;
    procedure Rename(const AValueName, ANewCaption: string); reintroduce; overload;
    procedure Rename(const ANewName: string); overload; override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TShellItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="AFileName">
    ///   The filename to an .exe file.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AExtended">
    ///   The contextmenu item is only shown when shift-key is pressed and a 
    ///   right click is performed. Otherwise the item is always shown.
    /// </param>
    constructor Create(const AName, ACaption, AFileName, ALocation: string;
      AEnabled, AExtended: Boolean);

    /// <summary>
    ///   Changes the icon of a contextmenu item.
    /// </summary>
    /// <param name="ANewIconFileName">
    ///   The new absolute .ico filename.
    ///  </param>
    /// <returns>
    ///   <c>True</c> if the icon was successfully changed or <c>False</c>
    ///   otherwise.
    /// </returns>
    function ChangeIcon(const ANewIconFileName: string): Boolean;

    /// <summary>
    ///   Deletes the icon of a contextmenu. }
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the icon was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function DeleteIcon(): Boolean;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;

    /// <summary>
    ///   Gets the behaviour that the contextmenu item is only shown when
    ///   shift-key is pressed and a right click is performed.
    /// </summary>
    property Extended: Boolean read FExtended;
  end;

  /// <summary>
  ///   A <c>TShellItem</c> represents a shell cascading context menu item that
  ///   can be added to a <see cref="TContextList"/>. Those items contain a
  ///   set of shell context menu items.
  /// </summary>
  TShellCascadingItem = class(TShellItem)
  private
    procedure GetSubCommands(var ASubCommands: TStrings);
  protected
    procedure ChangeFilePath(const ANewFileName: string); override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TShellCascadingItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AExtended">
    ///   The contextmenu item is only shown when shift-key is pressed and a 
    ///   right click is performed. Otherwise the item is always shown.
    /// </param>
    constructor Create(const AName, ACaption, ALocation: string; AEnabled, 
      AExtended: Boolean);

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;
  end;

  /// <summary>
  ///   A <c>TShellItem</c> represents a shell extension context menu item that
  ///   can be added to a <see cref="TContextList"/>. Those items are identified
  ///   by a GUID.
  /// </summary>
  TShellExItem = class(TContextListItem)
  protected
    procedure ChangeFilePath(const ANewFileName: string); override;
    procedure ChangeStatus(const ANewStatus: Boolean); override;
    function GetLocation(): string; override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;
  end;

  /// <summary>
  ///   A <c>TShellNewItem</c> represents a shell new context menu item that
  ///   can be added to a <see cref="TContextList"/>. Those items are shown
  ///   in the background context menu "new".
  /// </summary>
  TShellNewItem = class(TContextListItem)
  protected
    procedure ChangeFilePath(const ANewFileName: string); override;
    procedure ChangeStatus(const ANewStatus: Boolean); override;
    function GetLocation(): string; override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TShellNewItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    constructor Create(const AName, ACaption, ALocation: string; AEnabled: Boolean);

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;
  end;

  { Contextmenu enum }
  TShellItemType = (
    stShell, stShellEx, stShellNew
  );

  /// <summary>
  ///   A <c>TContextList</c> is the list that contains a set of
  ///   <see cref="TContextListItem"/>s. The list is thread-safe and supports
  ///   locking. You can use the <see cref="IsLocked"/> method to see if the
  ///   list is locked.
  /// </summary>
  TContextList = class(TRootList<TContextListItem>)
  public
    /// <summary>
    ///   Adds an item to the list.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute .exe filename.
    /// </param>
    /// <param name="AArguments">
    ///   Optional arguments passed to the .exe.
    /// </param>
    /// <param name="ALocationRoot">
    ///   The root Registry location e.g. <c>Drives</c>.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="AExtended">
    ///   If set to <c>True</c> the contextmenu item will only be shown when
    ///   shift-key is pressed and a right click is performed. Otherwise the
    ///   item is always shown.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the item was successfully added or <c>False</c> otherwise.
    /// </returns>
    function Add(const AFileName, AArguments, ALocationRoot, ACaption: string;
      AExtended: Boolean = False): Boolean; overload;

    /// <summary>
    ///   Exports the complete list as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportList(const AFileName: string); override;

    /// <summary>
    ///   Searches for an item in the list identified by name and location.
    /// </summary>
    /// <param name="AName">
    ///   The name of the item.
    /// </param>
    /// <param name="ALocationRoot">
    ///   The root location e.g. <c>Drives</c>.
    /// </param>
    /// <returns>
    ///   The index of the item in the list. If the item does not exist <c>-1</c>
    ///   is returned.
    /// </returns>
    function IndexOf(const AName, ALocationRoot: string): Integer; overload;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </param>
    /// <remarks>
    ///   Asynchronous: A thread is launched! The <see cref="OnSearchStart"/>
    ///   event occurs when the search starts. At the end the
    ///   <see cref="OnSearchFinish"/> event occurs.
    /// </remarks>
    procedure Load(AExpertMode: Boolean = False); override;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="ALocationRoot">
    ///   The root location e.g. <c>Drives</c>.
    /// </param>
    /// <param name="AWow64">
    ///   If set to <c>True</c> search for WOW64 items. Otherwise search for
    ///   native items.
    /// </param>
    procedure LoadContextmenu(const ALocationRoot: string; AWow64: Boolean); overload;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="ALocationRoot">
    ///   The root location e.g. <c>Drives</c>.
    /// </param>
    /// <param name="AShellItemType">
    ///   Specifies the item type to search for.
    /// </param>
    /// <param name="AWow64">
    ///   If set to <c>True</c> search for WOW64 items. Otherwise search for
    ///   native items.
    /// </param>
    procedure LoadContextmenu(const ALocationRoot: string;
      AShellItemType: TShellItemType; AWow64: Boolean); overload;
  end;

const
  { Service Registry keys }
  KEY_SERVICE_DISABLED        = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\services';
  KEY_SERVICE_ENABLED         = 'SYSTEM\CurrentControlSet\services\';

type
  { Exception class }
  EServiceException = class(Exception);

  /// <summary>
  ///   The possible service startup type.
  /// </summary>
  TServiceStart = (
    ssBoot, ssSystem, ssAutomatic, ssManual, ssDisabled
  );

  TServiceStartHelper = record helper for TServiceStart
    function ToString(ALangFile: TLanguageFile): string;
  end;

  /// <summary>
  ///   A <c>TServiceListItem</c> represents a service item that can be added
  ///   to a <see cref="TServiceList"/>.
  /// </summary>
  TServiceListItem = class(TRegistryItem)
  private
    FServiceManager: SC_HANDLE;
    FTime: TDateTime;
    FServiceStart: TServiceStart;
    function Disable(): Boolean;
    function Enable(): Boolean;
    function GetHandle(AAccess: DWORD): SC_HANDLE;
    function GetTime(): TDateTime;
  protected
    procedure ChangeFilePath(const ANewFileName: string); override;
    procedure ChangeStatus(const ANewStatus: Boolean); override;
    function GetLocation(): string; override;
    function GetRootKey(): TRootKey; override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TServiceListItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="AFileName">
    ///   The filename to an .exe file.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AServiceStart">
    ///   The service start type.
    /// </param>
    /// <param name="AServiceManager">
    ///   The handle to a service manager.
    /// </param>
    constructor Create(const AName, ACaption, AFileName: string; AEnabled: Boolean;
      AServiceStart: TServiceStart; AServiceManager: SC_HANDLE);

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;

    /// <summary>
    ///   Gets the start type.
    /// </summary>
    property Start: TServiceStart read FServiceStart;

    /// <summary>
    ///   Gets or sets the deactivation timestamp.
    /// </summary>
    property Time: TDateTime read GetTime write FTime;
  end;

  /// <summary>
  ///   A <c>TServiceList</c> is the list that contains a set of
  ///   <see cref="TServiceListItem"/>s. The list is thread-safe and supports
  ///   locking. You can use the <see cref="IsLocked"/> method to see if the
  ///   list is locked.
  /// </summary>
  TServiceList = class(TRootList<TServiceListItem>)
  private
    FManager: SC_HANDLE;
  public
    /// <summary>
    ///   Constructor for creating a <c>TServiceList</c> instance.
    /// </summary>
    constructor Create;

    /// <summary>
    ///   Destructor for destroying a <c>TStartupUserItem</c> instance.
    /// </summary>
    destructor Destroy(); override;

    /// <summary>
    ///   Adds an item to the list.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute .exe filename.
    /// </param>
    /// <param name="AArguments">
    ///   Optional arguments passed to the .exe.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the item was successfully added or <c>False</c> otherwise.
    /// </returns>
    function Add(const AFileName, AArguments, ACaption: string): Boolean; overload;

    /// <summary>
    ///   Exports the complete list as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportList(const AFileName: string); override;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </param>
    /// <remarks>
    ///   Asynchronous: A thread is launched! The <see cref="OnSearchStart"/>
    ///   event occurs when the search starts. At the end the
    ///   <see cref="OnSearchFinish"/> event occurs.
    /// </remarks>
    procedure Load(AExpertMode: Boolean = False); override;

    /// <summary>
    ///   Loads and adds a service item to the list.
    /// </summary>
    /// <param name="AName">
    ///   The name of the service.
    /// </param>
    /// <param name="AServiceHandle">
    ///   The handle to the service.
    /// </param>
    /// <param name="AIncludeDemand">
    ///   If set to <c>True</c> services that are started on demand are included.
    /// </param>
    /// <returns>
    ///   The index on which the item was added.
    /// </returns>
    function LoadService(const AName: string; AServiceHandle: SC_HANDLE;
      AIncludeDemand: Boolean = False): Integer;
  end;

  { Exception class }
  ETaskException = class(EOleError);

  /// <summary>
  ///   A <c>TTaskListItem</c> represents a scheduled task item that can be
  ///   added to a <see cref="TTaskList"/>.
  /// </summary>
  TTaskListItem = class(TRootItem)
  private
    FTask: IRegisteredTask;
    FTaskFolder: ITaskFolder;
    function GetTaskDefinition(): ITaskDefinition;
    procedure UpdateTask(const AName: string; ANewDefinition: ITaskDefinition);
  protected
    procedure ChangeFilePath(const ANewFilePath: string); override;
    procedure ChangeStatus(const ANewStatus: Boolean); override;
    function GetFullLocation(): string; override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TTaskListItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="AFileName">
    ///   The filename to an .exe file.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="ATask">
    ///   The task.
    /// </param>
    /// <param name="ATaskFolder">
    ///   A <c>ITaskFolder</c> object.
    /// </param>
    constructor Create(const AName, AFileName, ALocation: string; AEnabled: Boolean;
      ATask: IRegisteredTask; ATaskFolder: ITaskFolder);

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Gets the filter for file export.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The filter formatted as <c>"File type *.ext|*.ext"</c>
    /// </returns>
    function GetExportFilter(ALanguageFile: TLanguageFile): string; override;

    /// <summary>
    ///   Gets the item type as string.
    /// </summary>
    /// <returns>
    ///   The item type.
    /// </returns>
    function ToString(): string; override;

    /// <summary>
    ///   Gets the definition of the task.
    /// </summary>
    property Definition: ITaskDefinition read GetTaskDefinition;
  end;

  /// <summary>
  ///   A <c>TTaskList</c> is the list that contains a set of
  ///   <see cref="TTaskListItem"/>s. The list is thread-safe and supports
  ///   locking. You can use the <see cref="IsLocked"/> method to see if the
  ///   list is locked.
  /// </summary>
  TTaskList = class(TRootList<TTaskListItem>, IImportableList)
  private
    FTaskService: ITaskService;
    function AddTaskItem(ATask: IRegisteredTask; ATaskFolder: ITaskFolder): Integer;
  public
    /// <summary>
    ///   Constructor for creating a <c>TTaskList</c> instance.
    /// </summary>
    constructor Create;

    /// <summary>
    ///   Destructor for destroying a <c>TTaskList</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Exports the complete list as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportList(const AFileName: string); override;

    /// <summary>
    ///   Gets the filter for file export.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The filter formatted as <c>"File type *.ext|*.ext"</c>
    /// </returns>
    function GetExportFilter(ALanguageFile: TLanguageFile): string; override;

    /// <summary>
    ///   Gets the file filter for an <c>TOpenDialogy</c> from a <c>TLanguageFile</c>.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <c>TLanguageFile</c> that contains the translations.
    /// </param>
    /// <returns>
    ///   The import file filter.
    /// </returns>
    function GetImportFilter(ALanguageFile: TLanguageFile): string;

    /// <summary>
    ///   Imports a backup file.
    /// </summary>
    /// <param name="AFileName">
    ///   The backup file.
    ///  </param>
    /// <returns>
    ///   <c>True</c> if the import was successful or <c>False</c> otherwise.
    /// </returns>
    function ImportBackup(const AFileName: TFileName): Boolean;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </param>
    /// <remarks>
    ///   Asynchronous: A thread is launched! The <see cref="OnSearchStart"/>
    ///   event occurs when the search starts. At the end the
    ///   <see cref="OnSearchFinish"/> event occurs.
    /// </remarks>
    procedure Load(AExpertMode: Boolean = False); override;

    /// <summary>
    ///   Adds task items to the list.
    /// </summary>
    /// <param name="ATaskFolder">
    ///
    /// </param>
    /// <param name="AIncludeHidden">
    ///   If set to <c>True</c> include hidden tasks. Otherwise hidden tasks are
    ///   skipped.
    /// </param>
    procedure LoadTasks(ATaskFolder: ITaskFolder; AIncludeHidden: Boolean); overload;

    /// <summary>
    ///   Searches for task items in specific folder and adds them to the list.
    /// </summary>
    /// <param name="APath">
    ///   The relative path to the task folder. The root is <c>\</c>.
    /// </param>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> tasks in subfolders and those who are hidden are
    ///   included.
    /// </param>
    procedure LoadTasks(APath: string = '\'; AExpertMode: Boolean = False); overload;
  end;

implementation

uses StartupSearchThread, ContextSearchThread, ServiceSearchThread, TaskSearchThread;

{$I LanguageIDs.inc}

function FileTimeToDateTime(const AFileTime: TFileTime): TDateTime;
var
  ModifiedTime: TFileTime;
  SystemTime: TSystemTime;

begin
  try
    if ((AFileTime.dwLowDateTime = 0) and (AFileTime.dwHighDateTime = 0)) then
      Abort;

    FileTimeToLocalFileTime(AFileTime, ModifiedTime);
    FileTimeToSystemTime(ModifiedTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);

  except
    Result := 0;
  end;  //of try
end;

{ TLnkFile }

constructor TLnkFile.Create(AFileName: TFileName);
begin
  inherited Create;
  FFileName := AFileName;

  if Exists() then
    Read();
end;

function TLnkFile.GetFullPath(): string;
begin
  if HasArguments() then
    Result := FExeFileName +' '+ FArguments
  else
    Result := FExeFileName;
end;

function TLnkFile.GetFullPathEscaped(): string;
begin
  if HasArguments() then
    Result := '"'+ FFileName +'" '+ FArguments
  else
    Result := '"'+ FFileName +'"';
end;

procedure TLnkFile.SetArguments(const AArguments: string);
begin
  FArguments := AArguments;
  Save();
end;

procedure TLnkFile.SetExeFileName(const AExeFileName: TFileName);
begin
  FExeFileName := AExeFileName;
  Save();
end;

procedure TLnkFile.SetFileName(const AFileName: TFileName);
begin
  if (Exists() and not RenameFile(FFileName, AFileName)) then
    raise Exception.Create(SysErrorMessage(GetLastError()));

  FFileName := AFileName;
end;

function TLnkFile.Delete(): Boolean;
begin
  Result := DeleteFile(FFileName);
end;

function TLnkFile.Exists(): Boolean;
begin
  Result := FileExists(FFileName);
end;

function TLnkFile.HasArguments(): Boolean;
begin
  Result := (FArguments <> '');
end;

function TLnkFile.Read(): Boolean;
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  FileInfo: TWin32FindData;
  Path, Arguments: string;

begin
  Result := False;
  CoInitialize(nil);

  try
    if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
      IID_IShellLink, ShellLink)) then
    begin
      PersistFile := (ShellLink as IPersistFile);

      // Try to read .lnk file
      OleCheck(PersistFile.Load(PChar(FFileName), STGM_READ));
      SetLength(Path, MAX_PATH + 1);

      // Try to read path from .lnk
      OleCheck(ShellLink.GetPath(PChar(Path), MAX_PATH, FileInfo, SLR_ANY_MATCH));

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

function TLnkFile.Save(): Boolean;
begin
  Result := Save(FFileName, FExeFileName, FArguments);
end;

function TLnkFile.Save(AFileName, AExeFileName: TFileName;
  AArguments: string = ''): Boolean;
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;

begin
  Result := False;
  Assert(AFileName <> '', 'File name for .lnk file must not be empty!');
  Assert(AExeFileName <> '', 'File path to .exe must not be empty!');
  CoInitialize(nil);

  try
    if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
      IID_IShellLink, ShellLink)) then
    begin
      // Set path to .exe
      OleCheck(ShellLink.SetPath(PChar(AExeFileName)));

      // Set arguments if specified
      if (AArguments <> '') then
        OleCheck(ShellLink.SetArguments(PChar(AArguments)));

      // Set working directory
      ShellLink.SetWorkingDirectory(PChar(ExtractFilePath(AExeFileName)));

      // Save .lnk
      OleCheck(ShellLink.QueryInterface(IPersistFile, PersistFile));
      Result := Succeeded(PersistFile.Save(PChar(AFileName), True));
    end; //of begin

  finally
    CoUninitialize();
  end;  //of try
end;


{ TStartupUserLnkFile }

constructor TStartupLnkFile.Create(AFileName: TFileName);
begin
  inherited Create(AFileName);
  FStartupUser := not (ExtractFileExt(AFileName) = EXT_STARTUP_COMMON);
end;

constructor TStartupLnkFile.Create(const AName: string; AStartupUser: Boolean);
begin
  inherited Create(GetStartUpDir(AStartupUser) + ChangeFileExt(AName, '.lnk'));
  FStartupUser := AStartupUser;
end;

constructor TStartupLnkFile.Create(const AName: string; AStartupUser: Boolean;
  AExeFileName: TFileName; AArguments: string);
begin
  Create(AName, AStartupUser);
  FExeFileName := AExeFileName;
  FArguments := AArguments;
end;

function TStartupLnkFile.BackupExists(): Boolean;
begin
  // Not possible on Windows 8!
  if CheckWin32Version(6, 2) then
    Exit(False);

  Result := FileExists(GetBackupLnk());
end;

procedure TStartupLnkFile.CreateBackup();
begin
  // Deprecated since Windows 8!
  if CheckWin32Version(6, 2) then
    Exit;

  if not CopyFile(PChar(FFileName), PChar(GetBackupLnk()), False) then
    raise EStartupException.Create('Backup could not be created!');
end;

function TStartupLnkFile.DeleteBackup(): Boolean;
begin
  // Deprecated since Windows 8!
  if CheckWin32Version(6, 2) then
    Exit(False);

  Result := DeleteFile(GetBackupLnk());
end;

class function TStartupLnkFile.GetBackupDir(): string;
begin
  if GetFolderPath(CSIDL_WINDOWS, Result) then
    Result := Result +'pss\';
end;

function TStartupLnkFile.GetBackupExt(): string;
begin
  if FStartupUser then
    Result := EXT_STARTUP_USER
  else
    Result := EXT_STARTUP_COMMON;
end;

function TStartupLnkFile.GetBackupLnk(): string;
begin
  // Not possible on Windows 8!
  if CheckWin32Version(6, 2) then
    Exit;

  Result := GetBackupDir() + ExtractFileName(FFileName) + GetBackupExt();
end;

class function TStartupLnkFile.GetStartUpDir(AStartupUser: Boolean): string;
begin
  // Windows Vista?
  if CheckWin32Version(6) then
  begin
    if AStartupUser then
      GetKnownFolderPath(FOLDERID_Startup, Result)
    else
      GetKnownFolderPath(FOLDERID_CommonStartup, Result);
  end  //of begin
  else
    if AStartupUser then
      GetFolderPath(CSIDL_STARTUP, Result)
    else
      GetFolderPath(CSIDL_COMMON_STARTUP, Result);
end;


{ TRootItem }

constructor TRootItem.Create(const AName, ACaption, AFileName, ALocation: string;
  AEnabled: Boolean);
begin
  inherited Create;
  FName := AName;
  FCaption := ACaption;
  FFileName := AFileName;
  FLocation := ALocation;
  FEnabled := AEnabled;
end;

function TRootItem.GetArguments(): string;
begin
  Result := DeleteQuoteChars(ExtractArguments(FFileName));
end;

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

function TRootItem.GetFileDescription(AFileName: TFileName): string;
var
  FileInfo: TSHFileInfo;
  VersionSize, VersionHandle: DWORD;
  BufferSize: UINT;
  Buffer: PChar;
  Description: Pointer;

begin
  if (SHGetFileInfo(PChar(AFileName), 0, FileInfo, SizeOf(FileInfo), SHGFI_EXETYPE) = 0) then
    Exit;

  VersionSize := GetFileVersionInfoSize(PChar(AFileName), VersionHandle);

  if (VersionSize > 0) then
  begin
    GetMem(Buffer, VersionSize);

    try
      if not GetFileVersionInfo(PChar(AFileName), VersionHandle, VersionSize, Buffer) then
        Exit;

      if not VerQueryValue(Buffer, '\VarFileInfo\Translation', Description, BufferSize) then
        Exit;

      if VerQueryValue(Buffer, PChar(Format('\StringFileInfo\%.4x%.4x\%s',
        [LoWord(DWORD(Description^)), HiWord(DWORD(Description^)),
        'FileDescription'])), Description, BufferSize) then
        Result := PChar(Description);

    finally
      FreeMem(Buffer, VersionSize);
    end;  //of try
  end;  //of begin
end;

procedure TRootItem.ChangeFilePath(const ANewFileName: string);
begin
  FFileName := ANewFileName;
end;

function TRootItem.DeleteQuoteChars(const APath: string): string;
begin
  Result := StringReplace(APath , '"', '', [rfReplaceAll]);
end;

function TRootItem.ExtractArguments(const APath: string): string;
var
  ExtWithArguments: string;
  Ext, SpaceDelimiter: Integer;

begin
  // Cut path from extension until end
  // Note: Garbage in worst case if a folder name contains a '.'!
  Ext := AnsiPos('.', APath) - 1;

  if (Ext >= 0) and (APath.Chars[Ext] = '.') then
    ExtWithArguments := APath.SubString(Ext)
  else
    Exit;

  // Find space delimter between extension and arguments
  SpaceDelimiter := AnsiPos(' ', ExtWithArguments);

  // No space char after extension: no arguments!
  if (SpaceDelimiter = 0) then
    Exit;

  // Copy arguments without entension and space char at front and end
  Result := Trim(Copy(ExtWithArguments, SpaceDelimiter, Length(ExtWithArguments)));
end;

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

function TRootItem.GetIcon(): HICON;
begin
  Result := GetIcon(GetFileNameOnly());
end;

function TRootItem.GetIcon(AExeFileName: TFileName): HICON;
var
  FileInfo: TSHFileInfo;
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

  if Succeeded(SHGetFileInfo(PChar(AExeFileName), 0, FileInfo, SizeOf(FileInfo),
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

function TRootItem.GetLocation(): string;
begin
  Result := FLocation;
end;

procedure TRootItem.ChangeStatus(const ANewStatus: Boolean);
begin
  FEnabled := ANewStatus;
end;

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

function TRootItem.GetStatus(ALanguageFile: TLanguageFile): string;
begin
  if FEnabled then
    Result := ALanguageFile.GetString(LID_YES)
  else
    Result := ALanguageFile.GetString(LID_NO);
end;

procedure TRootItem.OpenInExplorer();
var
  PreparedFileName: string;
  ItemIDList: PItemIDList;

begin
  // Extract the file path only (without arguments and quote chars)
  PreparedFileName := GetFileNameOnly();

  if (PreparedFileName <> '') then
  begin
    ItemIDList := ILCreateFromPath(PChar(PreparedFileName));

    try
      // Open file in explorer
      if not Succeeded(SHOpenFolderAndSelectItems(ItemIDList, 0, nil, 0)) then
        raise EWarning.Create(SysErrorMessage(GetLastError()));

    finally
      ILFree(ItemIDList);
    end;  //of try
  end  //of begin
  else
    raise EWarning.Create(SysErrorMessage(ERROR_FILE_NOT_FOUND));
end;

procedure TRootItem.Rename(const ANewName: string);
begin
  FName := ANewName;
end;


{ TRegistryItem }

constructor TRegistryItem.Create(const AName, ACaption, AFileName, ALocation: string;
  AEnabled, AWow64: Boolean);
begin
  inherited Create(AName, ACaption, AFileName, ALocation, AEnabled);
  FWow64 := AWow64;
end;

function TRegistryItem.DeleteKey(AHKey: HKEY; AKeyPath, AKeyName: string;
  AFailIfNotExists: Boolean = True): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := AHKey;

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

procedure TRegistryItem.OpenInRegEdit(AWow64: Boolean);
const
  KEY_REGEDIT = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Applets\Regedit';

var
  Reg: TRegistry;
{$IFDEF WIN64}
  SystemWOW64: string;
{$ENDIF}

begin
  Reg := TRegistry.Create(KEY_WRITE);

  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(KEY_REGEDIT, True);
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

function TRegistryItem.WriteTimestamp(const AReg: TRegistry): TDateTime;
var
  Timestamp: TSystemTime;
  TimeNow: TDateTime;

begin
  TimeNow := Now();
  Result := TimeNow;
  DateTimeToSystemTime(TimeNow, Timestamp);

  try
    if not Assigned(AReg) then
      raise ERegistryException.Create('Registry instance was not initialized!');

    with AReg do
    begin
      WriteInteger('YEAR', Timestamp.wYear);
      WriteInteger('MONTH', Timestamp.wMonth);
      WriteInteger('DAY', Timestamp.wDay);
      WriteInteger('HOUR', Timestamp.wHour);
      WriteInteger('MINUTE', Timestamp.wMinute);
      WriteInteger('SECOND', Timestamp.wSecond);
    end;  //of with

  except
    on E: ERegistryException do
    begin
      E.Message := 'Error while writing deactivation timestamp: '+ E.Message;
      raise;
    end;  //of begin
  end;  //of try
end;

procedure TRegistryItem.OpenInRegEdit();
begin
  OpenInRegEdit(FWow64);
end;

function TRegistryItem.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := ALanguageFile.GetString(LID_FILTER_REGISTRY_FILE);
end;

function TRegistryItem.GetFullLocation(): string;
begin
  Result := GetRootKey().ToString() +'\'+ GetLocation();
end;

function TRegistryItem.GetTimestamp(AReg: TRegistry): TDateTime;
var
  Timestamp: TSystemTime;

begin
  Result := 0;

  // Deactivation timestamp only available for disabled items
  if (not Assigned(AReg) or FEnabled) then
    Exit;

  try
    // At least one valid date entry exists?
    if AReg.ValueExists('YEAR') then
    begin
      with Timestamp do
      begin
        wYear := AReg.ReadInteger('YEAR');
        wMonth := AReg.ReadInteger('MONTH');
        wDay := AReg.ReadInteger('DAY');
        wHour := AReg.ReadInteger('HOUR');
        wMinute := AReg.ReadInteger('MINUTE');
        wSecond := AReg.ReadInteger('SECOND');
      end;  //of with

      Result := SystemTimeToDateTime(Timestamp);
    end;  //of if

  except
    // Do not raise exception: Corrupted date is not fatal!
    Result := 0;
  end;  //of try
end;


{ TRootList }

constructor TRootList<T>.Create;
begin
  inherited Create;
  FEnabledItemsCount := 0;
  FInvalid := True;
  FLock := TCriticalSection.Create;
end;

destructor TRootList<T>.Destroy;
begin
  FLock.Free;
  Clear();
  inherited Destroy;
end;

procedure TRootList<T>.DoNotifyOnChanged(ANewStatus: TItemStatus);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self, ANewStatus);
end;

function TRootList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TRootList<T>._AddRef(): Integer;
begin
  Result := -1;
end;

function TRootList<T>._Release(): Integer;
begin
  Result := -1;
end;

procedure TRootList<T>.Clear();
begin
  inherited Clear;
  FEnabledItemsCount := 0;
  FItem := nil;
end;

function TRootList<T>.Add(const AItem: TRootItem): Integer;
begin
  Result := inherited Add(AItem);

  if ((Result <> -1) and AItem.Enabled) then
    Inc(FEnabledItemsCount);
end;

procedure TRootList<T>.ChangeItemFilePath(const ANewFilePath: string);
begin
  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    // Invalid item?
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    // Change item file path
    FItem.ChangeFilePath(ANewFilePath);

  finally
    FLock.Release();
  end;  //of try
end;

procedure TRootList<T>.ChangeItemStatus(const ANewStatus: Boolean);
var
  NewStatus: TItemStatus;

begin
  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    if ANewStatus then
    begin
      if FItem.Enabled then
        raise EWarning.Create('Item already enabled!');

      // Enable item
      FItem.ChangeStatus(ANewStatus);
      Inc(FEnabledItemsCount);
      DoNotifyOnChanged(stEnabled);
    end
    else
    begin
      if not FItem.Enabled then
        raise EWarning.Create('Item already disabled!');

      // Disable item
      FItem.ChangeStatus(ANewStatus);

      // Update active counter
      if (FEnabledItemsCount > 0) then
        Dec(FEnabledItemsCount);

      // Notify disable
      DoNotifyOnChanged(stDisabled);
    end;

  finally
    FLock.Release();
  end;  //of try
end;

function TRootList<T>.DeleteItem(): Boolean;
var
  Deleted: Boolean;

begin
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
      if (FItem.Enabled and (FEnabledItemsCount > 0)) then
        // Update active counter
        Dec(FEnabledItemsCount);

      // Remove item from list
      Remove(FItem);
      FItem := nil;

      // Notify delete
      DoNotifyOnChanged(stDeleted);
    end;  //of begin

  finally
    FLock.Release();
    Result := Deleted;
  end;  //of try
end;

procedure TRootList<T>.DisableItem();
begin
  ChangeItemStatus(False);
end;

procedure TRootList<T>.DoNotifyOnFinished();
begin
  if Assigned(FOnSearchFinish) then
    FOnSearchFinish(Self);

  DoNotifyOnChanged(stDeleted);
  FInvalid := False;
end;

procedure TRootList<T>.EnableItem();
begin
  ChangeItemStatus(True);
end;

procedure TRootList<T>.ExportItem(const AFileName: string);
begin
  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    FItem.ExportItem(AFileName);

  finally
    FLock.Release();
  end;  //of try
end;

function TRootList<T>.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := ALanguageFile.GetString(LID_FILTER_REGISTRY_FILE);
end;

function TRootList<T>.IndexOf(const ANameOrCaption: string): Integer;
var
  i: Integer;
  Item: TRootItem;

begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    Item := Items[i];

    if ((Item.Name = ANameOrCaption) or (Item.Caption = ANameOrCaption)) then
    begin
      Result := i;
      Break;
    end;  //of begin
  end;  //of for
end;

function TRootList<T>.IndexOf(const ANameOrCaption: string; AEnabled: Boolean): Integer;
var
  i: Integer;
  Item: TRootItem;

begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    Item := Items[i];

    if (((Item.Name = ANameOrCaption) or (Item.Caption = ANameOrCaption)) and
      (Item.Enabled = AEnabled)) then
    begin
      Result := i;
      Break;
    end;  //of begin
  end;  //of for
end;

procedure TRootList<T>.Invalidate();
begin
  FInvalid := True;
end;

function TRootList<T>.IsLocked(): Boolean;
var
  Entered: Boolean;

begin
  Entered := FLock.TryEnter();

  if Entered then
    FLock.Release();

  Result := not Entered;
end;

procedure TRootList<T>.RenameItem(const ANewName: string);
begin
  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    if (not Assigned(FItem) or (IndexOf(FItem) = -1)) then
      raise EInvalidItem.Create('No item selected!');

    FItem.Name := ANewName;

  finally
    FLock.Release();
  end;  //of try
end;


{ TStartupListItem }

constructor TStartupListItem.Create(const AName, AFileName, ALocation: string;
  ARootKey: TRootKey; AEnabled, AWow64: Boolean);
begin
  inherited Create(AName, AName, AFileName, ALocation, AEnabled, AWow64);
  FRootKey := ARootKey;

  try
    FCaption := GetFileDescription(FileNameOnly);

  except
    // It is not fatal if the caption could not be set! Name is used as fallback!
  end;  //of try
end;

function TStartupListItem.GetTime(): TDateTime;
begin
  if not FEnabled then
    Result := FTime
  else
    // No deactivation time for enabled items!
    Result := 0;
end;

function TStartupListItem.GetWow64Key(): string;
begin
  if ((FEnabled or CheckWin32Version(6, 2)) and Wow64) then
    Result := KEY_STARTUP_RUN32
  else
    Result := FLocation;
end;

procedure TStartupListItem.ChangeStatus(const ANewStatus: Boolean);
var
  Reg: TRegistry;
  ItemStatus: TStartupItemStatus;
  TimeNow: TDateTime;

begin
  if not CheckWin32Version(6, 2) then
  begin
    if (FEnabled and not ANewStatus) then
    begin
      if not Disable() then
        raise Exception.Create('Unknown error!');
    end  //of begin
    else
      if (not FEnabled and ANewStatus) then
      begin
        if not Enable() then
          raise Exception.Create('Unknown error!');
      end;  //of if

    inherited ChangeStatus(ANewStatus);
    Exit;
  end;  //of begin

  // Status is stored in 64-Bit registry
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := FRootKey.ToHKey();

    if not Reg.OpenKey(GetApprovedLocation(), False) then
      raise EStartupException.Create('Key '''+ GetApprovedLocation() +''' does not exist!');

    if ANewStatus then
    begin
      TimeNow := 0;
      ItemStatus.Status := ST_ENABLED;
      ItemStatus.DeactivationTime.dwLowDateTime := 0;
      ItemStatus.DeactivationTime.dwHighDateTime := 0;
    end  //of begin
    else
    begin
      TimeNow := Now();
      ItemStatus.Status := ST_DISABLED;
      ItemStatus.DeactivationTime := DateTimeToFileTime(TimeNow);
    end;  //of if

    Reg.WriteBinaryData(Name, ItemStatus, SizeOf(TStartupItemStatus));
    FTime := TimeNow;
    inherited ChangeStatus(ANewStatus);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TStartupListItem.DateTimeToFileTime(const AFileTime: TDateTime): TFileTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;

begin
  Result.dwLowDateTime := 0;
  Result.dwHighDateTime := 0;
  DateTimeToSystemTime(AFileTime, SystemTime);
  SystemTimeToFileTime(SystemTime, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, Result);
end;

function TStartupListItem.DeleteValue(AKeyPath: string;
  AReallyWow64: Boolean = True): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;

  if ((FEnabled or CheckWin32Version(6, 2)) and Wow64 and AReallyWow64) then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := FRootKey.ToHKey();

    // Key invalid?
    if not Reg.OpenKey(AKeyPath, False) then
      raise EStartupException.Create('Key does not exist!');

    // Delete value
    if (Reg.ValueExists(Name) and not Reg.DeleteValue(Name)) then
      raise EStartupException.Create('Could not delete value!');

    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TStartupListItem.GetApprovedLocation(): string;
begin
  // Only since Windows 8!
  if not CheckWin32Version(6, 2) then
    Exit;

  if Wow64 then
    Result := KEY_STARTUP_RUN32_APPROVED
  else
    Result := KEY_STARTUP_RUN_APPROVED;
end;

function TStartupListItem.GetRootKey(): TRootKey;
begin
  Result := FRootKey;
end;

function TStartupListItem.Rename(const AKeyPath, ANewName: string;
  AReallyWow64: Boolean = True): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;

  if ((FEnabled or CheckWin32Version(6, 2)) and Wow64 and AReallyWow64) then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := FRootKey.ToHKey();

    if not Reg.OpenKey(AKeyPath, False) then
      raise EStartupException.Create('Key does not exist!');

    if not Reg.ValueExists(Name) then
      ChangeStatus(True);

    Reg.RenameValue(Name, ANewName);

    if not Reg.ValueExists(ANewName) then
      raise EStartupException.Create('Value was not renamed!');

    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TStartupListItem.ChangeFilePath(const ANewFileName: string);
var
  Reg: TRegistry;
  ItemName: string;

begin
  if ((FEnabled or CheckWin32Version(6, 2)) and Wow64) then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := FRootKey.ToHKey();

    // Invalid key?
    if not Reg.OpenKey(FLocation, False) then
      raise EStartupException.Create('Key does not exist!');

    if (FEnabled or CheckWin32Version(6, 2)) then
      ItemName := Name
    else
      ItemName := 'command';

    // Value must exist!
    if not Reg.ValueExists(ItemName) then
      raise EStartupException.Create('Value '''+ ItemName +''' does not exist!');

    // Change path
    Reg.WriteString(ItemName, ANewFileName);
    inherited ChangeFilePath(ANewFileName);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TStartupListItem.Delete(): Boolean;
begin
  Result := DeleteValue(GetApprovedLocation(), False);
end;

procedure TStartupListItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    if (FEnabled or CheckWin32Version(6, 2)) then
    begin
      RegFile.ExportReg(FRootKey.ToHKey(), GetWow64Key(), Name);

      try
        // Windows 8?
        if CheckWin32Version(6, 2) then
          RegFile.ExportReg(FRootKey.ToHKey(), GetApprovedLocation(), Name);

      except
        // Approved item does not exist?
        on E: ERegistryException do
          // Just continue!
      end;
    end  //of begin
    else
      RegFile.ExportReg(FRootKey.ToHKey(), FLocation, False);

  finally
    RegFile.Free;
  end;  //of try
end;

procedure TStartupListItem.OpenInRegEdit();
begin
  if (FEnabled or CheckWin32Version(6, 2)) then
    inherited OpenInRegEdit()
  else
    // Disabled items are stored in 64-Bit Registry!
    OpenInRegEdit(False);
end;

procedure TStartupListItem.Rename(const ANewName: string);
begin
  if Rename(GetApprovedLocation(), ANewName, False) then
    inherited Rename(ANewName);
end;

function TStartupListItem.ToString(): string;
var
  Reg: TRegistry;

begin
  if (not FEnabled and not CheckWin32Version(6, 2)) then
  begin
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey(FLocation, False);
      Result := Reg.ReadString('hkey');

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try
  end  //of begin
  else
    Result := RootKey.ToString(False);

  if Wow64 then
    Result := Result +'32';
end;


{ TStartupItem }

constructor TStartupItem.Create(const AName, AFileName, ALocation: string;
  ARootKey: TRootKey; AEnabled, AWow64, ARunOnce: Boolean);
begin
  inherited Create(AName, AFileName, ALocation, ARootKey, AEnabled, AWow64);
  FRunOnce := ARunOnce;
end;

function TStartupItem.Delete(): Boolean;
begin
  if (FEnabled or CheckWin32Version(6, 2)) then
  begin
    Result := DeleteValue(GetLocation(), True);

    // Windows 8?
    if (Result and CheckWin32Version(6, 2)) then
      Result := inherited Delete();
  end  //of begin
  else
    Result := DeleteKey(HKEY_LOCAL_MACHINE, KEY_STARTUP_DISABLED, Name);
end;

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

    // Write values
    RootKey.FromHKey(RootKey.ToHKey());
    Reg.WriteString('hkey', RootKey.ToString(False));
    Reg.WriteString('key', GetWow64Key());
    Reg.WriteString('item', Name);
    Reg.WriteString('command', FileName);
    Reg.WriteString('inimapping', '0');

    // Windows >= Vista?
    if CheckWin32Version(6) then
      // Save deactivation timestamp
      FTime := WriteTimestamp(Reg);

    // Open startup location
    Reg.CloseKey();

    // Redirect to 32 Bit key?
    if Wow64 then
      Reg.Access := KEY_WOW64_32KEY or KEY_READ or KEY_WRITE;

    Reg.RootKey := FRootKey.ToHKey();
    Reg.OpenKey(FLocation, False);

    // Delete old value, but do not fail if old value does not exist!
    if (Reg.ValueExists(Name) and not Reg.DeleteValue(Name)) then
      raise EStartupException.Create('Could not delete value!');

    // Update information
    FRootKey := rkHKLM;
    FLocation := KEY_STARTUP_DISABLED + Name;
    FEnabled := False;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TStartupItem.Enable(): Boolean;
var
  Reg: TRegistry;
  NewKeyPath: string;
  NewHKey: TRootKey;
  Access64: LongWord;

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
    NewHKey.FromString(Reg.ReadString('hkey'));
    NewKeyPath := Reg.ReadString('key');

    if ((NewKeyPath = '') or (NewHKey = rkUnknown)) then
      raise EStartupException.Create('Invalid destination Registry values for '
        +'hkey or key!');

    Reg.CloseKey;

    // Redirect to 32 Bit key?
    if Wow64 then
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

    Reg.RootKey := NewHKey.ToHKey();

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
    FRootKey := NewHKey;
    FLocation := NewKeyPath;
    FEnabled := True;
    FTime := 0;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TStartupItem.GetWow64Key(): string;
begin
  if ((FEnabled or CheckWin32Version(6, 2)) and Wow64) then
  begin
    if FRunOnce then
      Result := KEY_STARTUP_RUNONCE32
    else
      Result := KEY_STARTUP_RUN32;
  end  //of begin
  else
    Result := FLocation;
end;

procedure TStartupItem.Rename(const ANewName: string);
var
  Reg: TRegistry;

begin
  if (FEnabled or CheckWin32Version(6, 2)) then
  begin
    Rename(FLocation, ANewName, True);

    // Windows 8?
    if CheckWin32Version(6, 2) then
      inherited Rename(ANewName)
    else
      FName := ANewName;

    Exit;
  end;  //of begin

  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := FRootKey.ToHKey();

    if not Reg.OpenKey(FLocation, False) then
      raise EStartupException.Create('Key does not exist!');

    if (FRootKey <> rkHKLM) then
      raise EStartupException.Create('Wrong HKEY!');

    Reg.WriteString('item', ANewName);
    Reg.CloseKey;

    if not Reg.OpenKey(ExtractFileDir(FLocation), False) then
      raise EStartupException.Create('Key does not exist!');

    if Reg.KeyExists(ANewName) then
      raise EStartupException.Create('Key already exists!');

    // Rename key and ANewName old key
    Reg.MoveKey(Name, ANewName, True);

    if not Reg.KeyExists(ANewName) then
      raise EStartupException.Create('Key was not renamed!');

    FLocation := KEY_STARTUP_DISABLED + ANewName;
    FName := ANewName;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TStartupItem.ToString(): string;
begin
  if RunOnce then
  begin
    Result := 'RunOnce';

    if Wow64 then
      Result := Result +'32';
  end  //of begin
  else
    Result := inherited ToString();
end;


{ TStartupUserItem }

constructor TStartupUserItem.Create(const AName, AFileName, ALocation: string;
  ARootKey: TRootKey; AEnabled: Boolean; ALnkFile: TStartupLnkFile);
begin
  inherited Create(AName, AFileName, ALocation, ARootKey, AEnabled, False);
  FLnkFile := ALnkFile;
end;

destructor TStartupUserItem.Destroy;
begin
  FLnkFile.Free;
  inherited Destroy;
end;

function TStartupUserItem.AddCircumflex(const AName: string): string;
begin
  Result := StringReplace(AName, '\', '^', [rfReplaceAll]);
end;

function TStartupUserItem.GetFullLocation(): string;
begin
  if FEnabled then
    // Enabled startup user items have a filesystem location! 
    Result := FLocation
  else
    // Windows 8?
    if CheckWin32Version(6, 2) then
      Result := FRootKey.ToString() +'\'+ GetApprovedLocation()
    else
      Result := inherited GetFullLocation();
end;

function TStartupUserItem.GetStartupUser(): Boolean;
begin
  Result := FLnkFile.StartupUser;
end;

function TStartupUserItem.GetApprovedLocation(): string;
begin
  // Only since Windows 8!
  if CheckWin32Version(6, 2) then
    Result := KEY_STARTUP_USER_APPROVED;
end;

function TStartupUserItem.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  if StartupUser then
    Result := ToString() +'|*'+ EXT_STARTUP_USER
  else
    Result := ToString() +'|*'+ EXT_STARTUP_COMMON;
end;

procedure TStartupUserItem.ChangeFilePath(const ANewFileName: string);
begin
  if (not Enabled and not CheckWin32Version(6, 2)) then
    inherited ChangeFilePath(ANewFileName);

  FLnkFile.ExeFileName := DeleteQuoteChars(ExtractPathToFile(ANewFileName));
  FLnkFile.Arguments := DeleteQuoteChars(ExtractArguments(ANewFileName));

  // Rewrite backup
  if (not FEnabled and FLnkFile.BackupExists()) then
    FLnkFile.CreateBackup();
end;

function TStartupUserItem.Delete(): Boolean;
var
  Win8: Boolean;

begin
  Win8 := CheckWin32Version(6, 2);

  if (FEnabled or Win8) then
  begin
    // Could not delete .lnk?
    if not FLnkFile.Delete() then
      raise EStartupException.Create('Could not delete .lnk file!');

    if Win8 then
      Result := inherited Delete()
    else
      Result := True;
  end  //of begin
  else
    Result := DeleteKey(HKEY_LOCAL_MACHINE, KEY_STARTUP_USER_DISABLED,
      AddCircumflex(FLnkFile.FileName));
end;

function TStartupUserItem.Disable(): Boolean;
var
  Reg: TRegistry;
  KeyName: string;

begin
  Result := False;

  // Create backup directory if not exist
  if not DirectoryExists(TStartupLnkFile.GetBackupDir()) then
    ForceDirectories(TStartupLnkFile.GetBackupDir());

  // .lnk does not exist
  if not FLnkFile.Exists() then
    raise EStartupException.Create('Lnk file '''+ FLnkFile.FileName +''' does not exist!');

  // Create backup by copying original .lnk
  FLnkFile.CreateBackup();

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
    if CheckWin32Version(6) then
    begin
      Reg.WriteString('backupExtension', FLnkFile.BackupExt);
      Reg.WriteString('location', ExtractFileDir(FLocation));
      FTime := WriteTimestamp(Reg);
    end  //of begin
    else
      Reg.WriteString('location', ToString());

    // Delete original .lnk
    if not FLnkFile.Delete() then
      raise EStartupException.Create('Could not delete .lnk file '''+ FLnkFile.FileName +'''!');

    // Update information
    FLocation := KEY_STARTUP_USER_DISABLED + KeyName;
    FRootKey := rkHKLM;
    FEnabled := False;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

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
      if not FLnkFile.Save() then
        raise EStartupException.Create('Could not create .lnk file!');
    end;  //of if

  // Do not abort if old key could not be deleted
  if not DeleteKey(HKEY_LOCAL_MACHINE, KEY_STARTUP_USER_DISABLED,
    AddCircumflex(FLnkFile.FileName), False) then
    raise EStartupException.Create('Could not delete key!');

  // Update information
  FLocation := FLnkFile.FileName;
  FRootKey := rkUnknown;
  FEnabled := True;
  Result := True;
end;

procedure TStartupUserItem.ExportItem(const AFileName: string);
begin
  if CheckWin32Version(6, 2) then
    CopyFile(PChar(FLnkFile.FileName), PChar(AFileName), False)
  else
    if not FEnabled then
      inherited ExportItem(AFileName)
    else
      FLnkFile.CreateBackup();
end;

procedure TStartupUserItem.Rename(const ANewName: string);
var
  OldFileName, NewFileName, NewName, OldKeyName, NewKeyName: string;
  Reg: TRegistry;
  Win8: Boolean;

begin
  OldFileName := FLnkFile.FileName;
  NewName := ChangeFileExt(ANewName, '.lnk');
  NewFileName := ReplaceText(OldFileName, ExtractFileName(OldFileName), NewName);

  // Rename .lnk file
  FLnkFile.FileName := NewFileName;
  Win8 := CheckWin32Version(6, 2);

  if (FEnabled or Win8) then
  begin
    FLocation := NewFileName;

    if Win8 then
      inherited Rename(ANewName)
    else
      FName := ANewName;
  end  //of begin
  else
  begin
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey(KEY_STARTUP_USER_DISABLED, True);

      OldKeyName := AddCircumflex(FLnkFile.FileName);
      NewKeyName := AddCircumflex(NewFileName);

      if not Reg.KeyExists(OldKeyName) then
        raise EStartupException.Create('Key does not exist!');

      if Reg.KeyExists(NewKeyName) then
        raise EStartupException.Create('Key already exists!');

      // Rename key and delete old key
      Reg.MoveKey(OldKeyName, NewKeyName, True);

      if not Reg.KeyExists(NewKeyName) then
        raise EStartupException.Create('Key was not renamed!');

      FLocation := KEY_STARTUP_USER_DISABLED + NewKeyName;

      // Update specific information
      Reg.CloseKey();
      Reg.OpenKey(FLocation, False);
      Reg.WriteString('path', NewFileName);
      Reg.WriteString('item', ChangeFileExt(NewName, ''));
      FName := ANewName;

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try
  end;  //of if
end;

function TStartupUserItem.ToString(): string;
begin
  // Windows >= Vista?
  if CheckWin32Version(6) then
  begin
    if StartupUser then
      Result := STARTUP_USER
    else
      Result := STARTUP_COMMON;
  end  //of begin
  else
    if StartupUser then
      Result := STARTUP_USER_XP
    else
      Result := STARTUP_COMMON_XP;
end;

{ TStartupLocationHelper }

function TStartupLocationHelper.GetApprovedLocation(): TPair<HKEY, string>;
begin
  case Self of
    slHkcuRun, slHkcuRunOnce:
      begin
        Result.Key := HKEY_CURRENT_USER;
        Result.Value := KEY_STARTUP_RUN_APPROVED;
      end;

    slHklmRun, slHklmRunOnce:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := KEY_STARTUP_RUN_APPROVED;
      end;

    slHklmRun32, slHklmRunOnce32:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := KEY_STARTUP_RUN32_APPROVED;
      end;

    slStartupUser:
      begin
        Result.Key := HKEY_CURRENT_USER;
        Result.Value := KEY_STARTUP_USER_APPROVED;
      end;

    slCommonStartup:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := KEY_STARTUP_USER_APPROVED;
      end;
  end;  //of case
end;

function TStartupLocationHelper.GetLocation(): TPair<HKEY, string>;
begin
  case Self of
    slHkcuRun:
      begin
        Result.Key := HKEY_CURRENT_USER;
        Result.Value := KEY_STARTUP_RUN;
      end;

    slHkcuRunOnce:
      begin
        Result.Key := HKEY_CURRENT_USER;
        Result.Value := KEY_STARTUP_RUNONCE;
      end;

    slHklmRun, slHklmRun32:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := KEY_STARTUP_RUN;
      end;

    slHklmRunOnce, slHklmRunOnce32:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := KEY_STARTUP_RUNONCE;
      end;

    slStartupUser:
      begin
        Result.Key := 0;
        Result.Value := TStartupLnkFile.GetStartUpDir(True);
      end;

    slCommonStartup:
      begin
        Result.Key := 0;
        Result.Value := TStartupLnkFile.GetStartUpDir(False);
      end;
  end;  //of case
end;


{ TStartupList }

constructor TStartupList.Create;
begin
  inherited Create;
  FDeleteBackup := True;
end;

function TStartupList.DeleteBackupFile(): Boolean;
begin
  Result := False;

  // Deprecated since Windows 8!
  if CheckWin32Version(6, 2) then
    Exit;

  if (FDeleteBackup and (Selected is TStartupUserItem)) then
    Result := (Selected as TStartupUserItem).LnkFile.DeleteBackup();
end;

function TStartupList.GetImportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := Format(ALanguageFile.GetString(LID_FILTER_STARTUP_FILES),
    [EXT_STARTUP_USER, EXT_STARTUP_USER, EXT_STARTUP_COMMON, EXT_STARTUP_COMMON]);
end;

function TStartupList.AddNewStartupUserItem(const AName: string; AFileName: TFileName;
  AArguments: string = ''; AStartupUser: Boolean = True): Boolean;
var
  i: Integer;
  LnkFile: TStartupLnkFile;

begin
  Result := False;

  // File path already exists in another item?
  for i := 0 to Count - 1 do
    if AnsiContainsStr(Items[i].FileName, AFileName) then
      Exit(False);

  LnkFile := TStartupLnkFile.Create(AName, AStartupUser, AFileName, AArguments);

  // Link file created successfully?
  if not LnkFile.Save() then
    raise EStartupException.Create('Could not create .lnk file '''+ LnkFile.FileName +'''!');

  // Add item to list
  if (AddUserItem(LnkFile, AStartupUser) <> -1) then
  begin
    Result := True;

    // Windows 8?
    if CheckWin32Version(6, 2) then
      // Write the StartupApproved value
      Last.Enabled := True;
  end;  //of begin
end;

function TStartupList.AddUserItem(ALnkFile: TStartupLnkFile;
  AStartupUser: Boolean): Integer;
var
  Location, Name, FileName: string;
  RootKey: TRootKey;

begin
  Location := ALnkFile.FileName;
  Name := ExtractFileName(Location);
  FileName := ALnkFile.FullPath;

  // Windows 8?
  if CheckWin32Version(6, 2) then
  begin
    if AStartupUser then
      RootKey := rkHKCU
    else
      RootKey := rkHKLM;
  end  //of begin
  else
    RootKey := rkUnknown;

  Result := Add(TStartupUserItem.Create(Name, FileName, Location, RootKey, True,
    ALnkFile));
end;

function TStartupList.Add(const AFileName, AArguments, ACaption: string): Boolean;
var
  Name, Ext, FullPath: string;
  Reg: TRegistry;

begin
  Name := ExtractFileName(AFileName);
  Ext := ExtractFileExt(Name);

  // Check invalid extension
  if ((Ext <> '.exe') and (Ext <> '.bat')) then
    raise EAssertionFailed.Create('Invalid program extension! Must be ''.exe'' or ''.bat''!');

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
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
      // No WOW64 redirection on HKCU!
      Reg := TRegistry.Create(KEY_READ or KEY_WRITE);

      // Try to add new startup item to Registry
      try
        Reg.RootKey := HKEY_CURRENT_USER;
        Reg.OpenKey(KEY_STARTUP_RUN, True);

        // Escape space chars using quotes
        FullPath := '"'+ AFileName +'"';

        // Append arguments if used
        if (AArguments <> '') then
          FullPath := FullPath +' '+ AArguments;

        Reg.WriteString(ACaption, FullPath);

        // Adds item to list
        Result := (Add(TStartupItem.Create(ACaption, AFileName, KEY_STARTUP_RUN,
          rkHKCU, True, False, False)) <> -1);

        // Windows 8?
        if (Result and CheckWin32Version(6, 2)) then
          // Write the StartupApproved value
          Last.Enabled := True;

      finally
        Reg.CloseKey();
        Reg.Free;
      end;  //of try
    end;  //of begin

    // Refresh TListView
    if Result then
      DoNotifyOnFinished();

  finally
    FLock.Release();
  end;  //of try
end;

function TStartupList.BackupExists(): Boolean;
begin
  if (not Assigned(Selected) or (IndexOf(Selected) = -1)) then
    raise EInvalidItem.Create('No item selected!');

  if (Selected is TStartupUserItem) then
    Result := (Selected as TStartupUserItem).LnkFile.BackupExists()
  else
    Result := False;
end;

procedure TStartupList.ChangeItemStatus(const ANewStatus: Boolean);
begin
  inherited ChangeItemStatus(ANewStatus);

  // Only delete backup if item has been enabled!
  if Selected.Enabled then
    DeleteBackupFile();
end;

function TStartupList.DeleteItem(): Boolean;
begin
  DeleteBackupFile();
  Result := inherited DeleteItem();
end;

procedure TStartupList.ExportList(const AFileName: string);
var
  i: Integer;
  RegFile: TRegistryFile;
  Item: TStartupListItem;
  Win8: Boolean;

begin
  FLock.Acquire();
  Win8 := CheckWin32Version(6, 2);

  // Init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    for i := 0 to Count - 1 do
    begin
      Item := TStartupListItem(Items[i]);

      // Skip enabled startup user items (not in Registry)!
      if ((Item is TStartupUserItem) and (Item.Enabled or Win8)) then
        Continue;

      RegFile.ExportKey(Item.RootKey.ToHKey(), Item.Wow64Location, True);
    end;  //of for

    // Windows 8?
    if Win8 then
    begin
      RegFile.ExportKey(HKEY_CURRENT_USER, KEY_STARTUP_APPROVED, True);
      RegFile.ExportKey(HKEY_LOCAL_MACHINE, KEY_STARTUP_APPROVED, True);
    end;  //of begin

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
    FLock.Release();
  end;  //of try
end;

function TStartupList.ImportBackup(const AFileName: TFileName): Boolean;
var
  Name, Ext: string;
  LnkFile: TLnkFile;

begin
  Result := False;
  Ext := ExtractFileExt(AFileName);

  // Check invalid extension
  if ((Ext <> EXT_STARTUP_COMMON) and (Ext <> EXT_STARTUP_USER)) then
    raise EAssertionFailed.Create('Invalid backup file extension! Must '+
      'be '''+ EXT_STARTUP_COMMON +''' or '''+ EXT_STARTUP_USER +'''!');

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  // Init new .lnk file
  LnkFile := TStartupLnkFile.Create(AFileName);

  // Set the name of item
  Name := ExtractFileName(ChangeFileExt(AFileName, ''));
  Name := ChangeFileExt(Name, '.lnk');

  try
    // Extract path to .exe
    if (LnkFile.ExeFileName = '') then
      raise EStartupException.Create('Could not read backup file!');

    // Create .lnk file and add it to list
    Result := AddNewStartupUserItem(Name, LnkFile.ExeFileName, LnkFile.Arguments,
      (Ext = EXT_STARTUP_USER));

    // Refresh TListView
    if Result then
      DoNotifyOnFinished();

  finally
    LnkFile.Free;
    FLock.Release();
  end;  //of try
end;

procedure TStartupList.Load(AExpertMode: Boolean = False);
var
  StartupSearchThread: TStartupSearchThread;

begin
  // Init search thread
  StartupSearchThread := TStartupSearchThread.Create(TRootList<TRootItem>(Self),
    FLock, AExpertMode);

  with StartupSearchThread do
  begin
    Win64 := (TOSVersion.Architecture = arIntelX64);
    OnError := OnSearchError;
    OnFinish := OnSearchFinish;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    Start;
  end;  // of with
end;

procedure TStartupList.LoadDisabled(AStartupUser: Boolean);
var
  Reg: TRegistry;
  Items: TStringList;
  KeyPath: string;
  i: Integer;
  Wow64: Boolean;
  Item: TStartupListItem;
  RunOnce, StartupUser: Boolean;
  Location, Name, FileName, LnkFileName, ExeFileName, ExeArguments: string;

begin
  // Deprecated since Windows 8!
  if CheckWin32Version(6, 2) then
    Exit;

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
      Location := Reg.CurrentPath;
      FileName := Reg.ReadString('command');

      if not AStartupUser then
      begin
        Wow64 := AnsiContainsText(Reg.ReadString('key'), 'Wow6432Node');
        RunOnce := (ExtractFileName(Reg.ReadString('key')) = 'RunOnce');
        Name := Items[i];

        Item := TStartupItem.Create(Name, FileName, Location, rkHKLM, False,
          Wow64, RunOnce);
      end  //of begin
      else
      begin
        Name := ExtractFileName(StringReplace(Location, '^', '\', [rfReplaceAll]));
        LnkFileName := Reg.ReadString('path');

        // Windows >= Vista?
        if CheckWin32Version(6) then
          StartupUser := not AnsiSameText(Reg.ReadString('backupExtension'), EXT_STARTUP_COMMON)
        else
          StartupUser := AnsiSameText(Reg.ReadString('location'), STARTUP_USER_XP);

        Item := TStartupUserItem.Create(Name, FileName, Location, rkHKLM, False, nil);

        // Setup .lnk file
        ExeFileName := Item.FileNameOnly;;
        ExeArguments := Item.Arguments;
        TStartupUserItem(Item).LnkFile := TStartupLnkFile.Create(LnkFileName,
          StartupUser, ExeFileName, ExeArguments);
      end;  //of if

      Item.Time := Item.GetTimestamp(Reg);
      Add(Item);
    end;  //of for

  finally
    Reg.CloseKey();
    Reg.Free;
    Items.Free;
  end;  //of try
end;

function TStartupList.LoadStatus(const AName: string;
  AStartupLocation: TStartupLocation): TPair<Boolean, TDateTime>;
var
  Reg: TRegistry;
  ItemStatus: TStartupItemStatus;

begin
  // Only on Windows 8 and later!
  if not CheckWin32Version(6, 2) then
  begin
    Result.Key := True;
    Result.Value := 0;
    Exit;
  end;  //of if

  // Status is stored in 64-Bit registry
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := AStartupLocation.GetApprovedLocation().Key;
    Reg.OpenKey(AStartupLocation.GetApprovedLocation.Value, False);

    // Value does not exist or approved value is invalid?
    if (not Reg.ValueExists(AName) or (Reg.GetDataSize(AName) <> SizeOf(TStartupItemStatus))) then
    begin
      Result.Key := True;
      Result.Value := 0;
      Exit;
    end;  //of begin

    Reg.ReadBinaryData(AName, ItemStatus, SizeOf(TStartupItemStatus));
    Result.Key := (ItemStatus.Status = ST_ENABLED);

    // Get deactivation time
    if not Result.Key then
      Result.Value := FileTimeToDateTime(ItemStatus.DeactivationTime);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TStartupList.Load(AStartupLocation: TStartupLocation);
var
  SearchResult: TSearchRec;
  LnkFile: TStartupLnkFile;
  Reg: TRegistry;
  Items: TStringList;
  i: Integer;
  StartupUser, Wow64, RunOnce: Boolean;
  RootKey: TRootKey;
  Item: TStartupListItem;
  Status: TPair<Boolean, TDateTime>;
  Location, Name, FileName: string;

begin
  if (AStartupLocation in [slStartupUser, slCommonStartup]) then
  begin
    StartupUser := (AStartupLocation = slStartupUser);

    if (FindFirst(AStartupLocation.GetLocation().Value +'*.lnk',
      faAnyFile - faDirectory, SearchResult) = 0) then
    try
      // .lnk file found
      repeat
        LnkFile := TStartupLnkFile.Create(SearchResult.Name, StartupUser);
        Location := LnkFile.FileName;
        Name := ExtractFileName(Location);
        Status := LoadStatus(Name, AStartupLocation);
        FileName := LnkFile.FullPath;

        // Windows 8?
        if CheckWin32Version(6, 2) then
        begin
          if StartupUser then
            RootKey := rkHKCU
          else
            RootKey := rkHKLM;
        end  //of begin
        else
          RootKey := rkUnknown;

        Item := TStartupUserItem.Create(Name, FileName, Location, RootKey,
          Status.Key, LnkFile);
        Item.Time := Status.Value;
        Add(Item);
      until FindNext(SearchResult) <> 0;

    finally
      FindClose(SearchResult);
    end;  //of try

    Exit;
  end;   //of begin

  Wow64 := (AStartupLocation in [slHklmRunOnce32, slHklmRun32]);

  // WOW64 only present on 64 bit Windows!
  if (Wow64 and (TOSVersion.Architecture <> arIntelX64)) then
    Exit;

  Items := TStringList.Create;
  RunOnce := (AStartupLocation in [slHkcuRunOnce, slHklmRunOnce, slHklmRunOnce32]);

  // Allow WOW64 redirection?
  if Wow64 then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := AStartupLocation.GetLocation().Key;
    Reg.OpenKey(AStartupLocation.GetLocation().Value, False);
    Reg.GetValueNames(Items);
    RootKey.FromHKey(Reg.RootKey);

    for i := 0 to Items.Count - 1 do
    begin
      Name := Items[i];
      Status := LoadStatus(Name, AStartupLocation);
      Location := Reg.CurrentPath;
      FileName := Reg.ReadString(Items[i]);

      Item := TStartupItem.Create(Name, FileName, Location, RootKey, Status.Key,
        Wow64, RunOnce);
      Item.Time := Status.Value;
      Add(Item);
    end;  //of for

  finally
    Reg.CloseKey();
    Reg.Free;
    Items.Free;
  end;  //of try
end;


{ TContextListItem }

function TContextListItem.GetRootKey(): TRootKey;
begin
  Result := rkHKCR;
end;

procedure TContextListItem.Rename(const ANewName: string);
begin
  // Important: It is only possible to rename the caption of context items!
  FCaption := ANewName;
end;

function TContextListItem.Delete(): Boolean;
begin
  if not DeleteKey(HKEY_CLASSES_ROOT, ExtractFileDir(GetLocation()), Name) then
    raise EContextMenuException.Create('Could not delete key!');

  Result := True;
end;

function TContextListItem.DeleteUserChoice(const AFileExtension: string): Boolean;
begin
  Result := (RegDeleteKey(HKEY_CURRENT_USER, PChar(Format(KEY_USERCHOICE,
    [AFileExtension]))) = ERROR_SUCCESS);
end;

function TContextListItem.UserChoiceExists(const AFileExtension: string): Boolean;
var
  UserChoice: string;

begin
  UserChoice := GetRegStringValue(Format(KEY_USERCHOICE, [AFileExtension]),
    'ProgID', HKEY_CURRENT_USER);
  Result := AnsiStartsText('Applications\', UserChoice);
end;


{ TShellItem }

constructor TShellItem.Create(const AName, ACaption, AFileName, ALocation: string;
  AEnabled, AExtended: Boolean);
begin
  inherited Create(AName, ACaption, AFileName, ALocation, AEnabled, False);
  FExtended := AExtended;
end;

function TShellItem.GetLocation(): string;
begin
  Result := inherited GetLocation() +'\'+ CM_SHELL +'\'+ Name;
end;

procedure TShellItem.Rename(const AValueName, ANewCaption: string);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Invalid data type?
    if (Reg.GetDataType(AValueName) <> rdString) then
      raise EContextMenuException.Create('Invalid data type!');

    Reg.WriteString(AValueName, ANewCaption);
    inherited Rename(ANewCaption);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TShellItem.GetIcon(): HICON;
var
  Reg: TRegistry;
  Icon: TIcon;
  IconPath: string;

begin
  Result := 0;
  Icon := TIcon.Create;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    if Reg.ValueExists('Icon') then
    begin
      IconPath := DeleteQuoteChars(Reg.ReadString('Icon'));

      if (ExtractFileExt(IconPath) = '.exe') then
        Icon.Handle := GetIcon(IconPath)
      else
        Icon.LoadFromFile(IconPath);
    end;  //of begin

    Result := Icon.Handle;

  finally
    Reg.CloseKey();
    Reg.Free;
    Icon.Free;
  end;  //of try
end;

procedure TShellItem.ChangeFilePath(const ANewFileName: string);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetLocation() +'\command', False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Change path
    case Reg.GetDataType('') of
      rdExpandString: Reg.WriteExpandString('', ANewFileName);
      rdString:       Reg.WriteString('', ANewFileName);
      else
                      raise EContextMenuException.Create('Invalid data type!');
    end;  //of case

    inherited ChangeFilePath(ANewFileName);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TShellItem.ChangeIcon(const ANewIconFileName: string): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    // Delete icon?
    if ((ANewIconFileName = '') and Reg.ValueExists('Icon')) then
      Exit(Reg.DeleteValue('Icon'));

    // Change icon
    if Reg.ValueExists('Icon') then
      case Reg.GetDataType('Icon') of
        rdExpandString: Reg.WriteExpandString('Icon', ANewIconFileName);
        rdString:       Reg.WriteString('Icon', ANewIconFileName);
        else
                        raise EContextMenuException.Create('Invalid data type!');
      end  //of case
    else
      Reg.WriteString('Icon', ANewIconFileName);

    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TShellItem.ChangeStatus(const ANewStatus: Boolean);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create('Key does not exist!');

    if ANewStatus then
    begin
      // Delete disable value, but do not fail if value does not exist!
      if (Reg.ValueExists(CM_SHELL_DISABLED) and not Reg.DeleteValue(CM_SHELL_DISABLED)) then
        raise EStartupException.Create('Could not delete value '''+ CM_SHELL_DISABLED +'''!');
    end  //of begin
    else
      Reg.WriteString(CM_SHELL_DISABLED, '');

    // Update status
    inherited ChangeStatus(ANewStatus);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TShellItem.DeleteIcon: Boolean;
begin
  Result := ChangeIcon('');
end;

procedure TShellItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    RegFile.ExportReg(HKEY_CLASSES_ROOT, GetLocation(), True);

  finally
    RegFile.Free;
  end;  //of try
end;

procedure TShellItem.Rename(const ANewName: string);
begin
  Rename('', ANewName);
end;

function TShellItem.ToString(): string;
begin
  Result := CM_SHELL;
end;


{ TShellCascadingItem }

constructor TShellCascadingItem.Create(const AName, ACaption, ALocation: string;
  AEnabled, AExtended: Boolean);
begin
  inherited Create(AName, ACaption, '', ALocation, AEnabled, AExtended);
end;

procedure TShellCascadingItem.GetSubCommands(var ASubCommands: TStrings);
const
  CM_SUBCOMMANDS = 'SubCommands';

var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if (not Reg.OpenKey(GetLocation(), False) or not Reg.ValueExists(CM_SUBCOMMANDS)) then
      Exit;

    ASubCommands.Delimiter := ';';
    ASubCommands.DelimitedText := Reg.ReadString(CM_SUBCOMMANDS);

  finally
    Reg.Free;
  end;  //of try
end;

procedure TShellCascadingItem.ChangeFilePath(const ANewFileName: string);
begin
  raise EAbstractError.Create('Impossible!');
end;

function TShellCascadingItem.Delete(): Boolean;
var
  i: Integer;
  Commands: TStrings;

begin
  Commands := TStringList.Create;

  try
    GetSubCommands(Commands);

    for i := 0 to Commands.Count - 1 do
      DeleteKey(HKEY_LOCAL_MACHINE, KEY_COMMAND_STORE, Commands[i], False);

    Result := inherited Delete();

  finally
    Commands.Free;
  end;  //of try
end;

procedure TShellCascadingItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;
  i: Integer;
  Commands: TStrings;

begin
  RegFile := TRegistryFile.Create(AFileName, True);
  Commands := TStringList.Create;

  try
    RegFile.ExportKey(HKEY_CLASSES_ROOT, GetLocation(), True);
    GetSubCommands(Commands);

    for i := 0 to Commands.Count - 1 do
      RegFile.ExportKey(HKEY_LOCAL_MACHINE, KEY_COMMAND_STORE +'\'+ Commands[i], True);

    RegFile.Save();

  finally
    Commands.Free;
    RegFile.Free;
  end;  //of try
end;

procedure TShellCascadingItem.Rename(const ANewName: string);
begin
  Rename('MUIVerb', ANewName);
end;

function TShellCascadingItem.ToString(): string;
begin
  Result := inherited ToString() +' Cascading';
end;


{ TShellExItem }

procedure TShellExItem.ChangeStatus(const ANewStatus: Boolean);
var
  Reg: TRegistry;
  OldValue, NewValue: string;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(GetLocation(), False) then
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
    inherited ChangeStatus(ANewStatus);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TShellExItem.GetLocation(): string;
begin
  Result := inherited GetLocation() +'\'+ CM_SHELLEX_HANDLERS +'\'+ Name;
end;

procedure TShellExItem.ChangeFilePath(const ANewFileName: string);
var
  Reg: TRegistry;
  ProgramKeyPath: string;

begin
  if Wow64 then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetLocation(), False) then
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
    inherited ChangeFilePath(ANewFileName);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TShellExItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;
  Reg: TRegistry;
  Key: string;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  if Wow64 then
    Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ)
  else
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if Reg.OpenKey(GetLocation(), False) then
    begin
      Key := 'CLSID\'+ Reg.ReadString('');
      Reg.CloseKey();

      // Key exists?
      if Reg.OpenKey(Key, False) then
        RegFile.ExportKey(HKEY_CLASSES_ROOT, Key, True);
    end;  //of begin

    RegFile.ExportKey(HKEY_CLASSES_ROOT, GetLocation(), True);
    RegFile.Save();

  finally
    Reg.CloseKey();
    Reg.Free;
    RegFile.Free;
  end;  //of try
end;

procedure TShellExItem.Rename(const ANewName: string);
begin
  raise EAbstractError.Create('Impossible!');
end;

function TShellExItem.ToString(): string;
begin
  Result := CM_SHELLEX;
end;


{ TShellNewItem }

constructor TShellNewItem.Create(const AName, ACaption, ALocation: string;
  AEnabled: Boolean);
begin
  inherited Create(AName, ACaption, '', ALocation, AEnabled, False);
end;

procedure TShellNewItem.ChangeStatus(const ANewStatus: Boolean);
var
  Reg: TRegistry;
  OldKeyName, NewKeyName: string;

begin
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
    inherited ChangeStatus(ANewStatus);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TShellNewItem.GetLocation(): string;
begin
  if FEnabled then
    Result := inherited GetLocation() +'\'+ CM_SHELLNEW
  else
    Result := inherited GetLocation() +'\'+ CM_SHELLNEW_DISABLED;
end;

procedure TShellNewItem.ChangeFilePath(const ANewFileName: string);
begin
  raise EAbstractError.Create('Impossible!');
end;

function TShellNewItem.Delete(): Boolean;
begin
  if not DeleteKey(HKEY_CLASSES_ROOT, ExtractFileDir(GetLocation()),
    ExtractFileName(GetLocation())) then
    raise EContextMenuException.Create('Could not delete key!');

  Result := True;
end;

procedure TShellNewItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    RegFile.ExportReg(HKEY_CLASSES_ROOT, GetLocation(), True);

  finally
    RegFile.Free;
  end;  //of try
end;

procedure TShellNewItem.Rename(const ANewName: string);
begin
  raise EAbstractError.Create('Impossible!');
end;

function TShellNewItem.ToString(): string;
begin
  Result := CM_SHELLNEW;
end;


{ TContextList }

function TContextList.Add(const AFileName, AArguments, ALocationRoot,
  ACaption: string; AExtended: Boolean = False): Boolean;
var
  Name, Ext, FullPath, LocationRoot, FileType, KeyPath: string;
  Reg: TRegistry;

begin
  Result := False;
  LocationRoot := Trim(ALocationRoot);

  // Valid location?
  if ((LocationRoot <> '*') and (Length(LocationRoot) <= 2)) then
    raise EAssertionFailed.Create('Invalid location: Expected at least two characters or ''*''!');

  Ext := ExtractFileExt(AFileName);

  // Check invalid extension
  if ((Ext <> '.exe') and (Ext <> '.bat')) then
    raise EAssertionFailed.Create('Invalid program extension! Must be ''.exe'' or ''.bat''!');

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

  try
    Name := ChangeFileExt(ExtractFileName(AFileName), '');

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
      Result := (Add(TShellItem.Create(Name, ACaption, FullPath, FileType, True, 
        AExtended)) <> -1);

      // Refresh TListView
      DoNotifyOnFinished();

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try

  finally
    FLock.Release();
  end;  //of try
end;

procedure TContextList.ExportList(const AFileName: string);
var
  i: Integer;
  RegFile: TRegistryFile;
  Item: TContextListItem;

begin
  FLock.Acquire;

  // Init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    for i := 0 to Count - 1 do
    begin
      Item := TContextListItem(Items[i]);
      RegFile.ExportKey(HKEY_CLASSES_ROOT, Item.Location, True);
    end;  //of for

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
    FLock.Release();
  end;  //of try
end;

function TContextList.IndexOf(const AName, ALocationRoot: string): Integer;
var
  i: Integer;
  Item: TContextListItem;

begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    Item := Items[i];

    if (((Item.Name = AName) or (Item.Caption = AName)) and
      (Item.LocationRoot = ALocationRoot)) then
    begin
      Result := i;
      Break;
    end;  //of begin
  end;  //of for
end;

procedure TContextList.Load(AExpertMode: Boolean = False);
var
  SearchThread: TContextSearchThread;

begin
  // Init search thread
  SearchThread := TContextSearchThread.Create(Self, FLock, AExpertMode);

  with SearchThread do
  begin
    Win64 := (TOSVersion.Architecture = arIntelX64);
    OnError := OnSearchError;
    OnFinish := OnSearchFinish;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    Start;
  end;  // of with
end;

procedure TContextList.LoadContextmenu(const ALocationRoot: string;
  AWow64: Boolean);
var
  ContextMenuItem: TShellItemType;

begin
  for ContextMenuItem := Low(TShellItemType) to High(TShellItemType) do
    LoadContextmenu(ALocationRoot, ContextMenuItem, AWow64);
end;

procedure TContextList.LoadContextmenu(const ALocationRoot: string;
  AShellItemType: TShellItemType; AWow64: Boolean);
var
  Reg: TRegistry;
  i, DelimiterPos: Integer;
  List: TStringList;
  ItemName, Key, FileName, GuID, Caption: string;
  Enabled, Wow64, Extended: Boolean;
  Access64: LongWord;
  
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

      DelimiterPos := ALocationRoot.IndexOf('\');

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
      Add(TShellNewItem.Create(Key, Caption, ALocationRoot, Enabled));
      Exit;
    end;  //of if

    // Read out all keys
    Reg.GetKeyNames(List);

    for i := 0 to List.Count - 1 do
    begin
      Reg.CloseKey();
      ItemName := List[i];
      Reg.OpenKey(Key +'\'+ ItemName, False);
      FileName := '';

      // Filter items with GUID in name
      if (ItemName[1] = '{') then
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
        Extended := Reg.ValueExists('Extended');
        
        // Cascading shell item?
        if not Reg.OpenKey('command', False) then
        begin
          if ((not Reg.ValueExists('MUIVerb') or not Reg.ValueExists('SubCommands')) and
            not Reg.KeyExists('ExtendedSubCommandsKey')) then
            Continue;

          Caption := Reg.ReadString('MUIVerb');
          Add(TShellCascadingItem.Create(ItemName, Caption, ALocationRoot, 
            Enabled, Extended));
          Continue;
        end;  //of begin

        // Filter important Shell items
        if Reg.ValueExists('DelegateExecute') then
          Continue;

        // Get file path of command
        if not (Reg.GetDataType('') in [rdString, rdExpandString]) then
          Continue;

        FileName := Reg.ReadString('');
        Add(TShellItem.Create(ItemName, Caption, FileName, ALocationRoot, 
          Enabled, Extended));
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

            FileName := Reg.ReadString('');
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

                FileName := Reg.ReadString('');
              end;  //of begin

              Reg.Access := Access64;
            end;  //of if

          // Add item to list
          Add(TShellExItem.Create(ItemName, '', FileName, ALocationRoot, Enabled, Wow64));
        end;  //of begin
    end;  //of for

  finally
    List.Free;
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;


{ TServiceStartHelper }

function TServiceStartHelper.ToString(ALangFile: TLanguageFile): string;
begin
  case Self of
    ssAutomatic: Result := ALangFile.GetString(61);
    ssManual:    Result := ALangFile.GetString(62);
    else         Result := 'Service';
  end;  //of case
end;


{ TServiceListItem }

constructor TServiceListItem.Create(const AName, ACaption, AFileName: string;
  AEnabled: Boolean; AServiceStart: TServiceStart; AServiceManager: SC_HANDLE);
begin
  inherited Create(AName, ACaption, AFileName, '', AEnabled, False);
  FServiceStart := AServiceStart;
  FServiceManager := AServiceManager;
end;

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

function TServiceListItem.GetTime(): TDateTime;
begin
  if not FEnabled then
    Result := FTime
  else
    // No deactivation time for enabled items!
    Result := 0;
end;

function TServiceListItem.GetLocation(): string;
begin
  Result := KEY_SERVICE_ENABLED + Name;
end;

function TServiceListItem.GetRootKey(): TRootKey;
begin
  Result := rkHKLM;
end;

procedure TServiceListItem.ChangeFilePath(const ANewFileName: string);
var
  Service: SC_HANDLE;

begin
  Service := GetHandle(SERVICE_CHANGE_CONFIG);

  try
    // Change path
    if not ChangeServiceConfig(Service, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE,
      SERVICE_NO_CHANGE, PChar(ANewFileName), nil, nil, nil, nil, nil, nil) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    inherited ChangeFilePath(ANewFileName);

  finally
    CloseServiceHandle(Service);
  end;  //of try
end;

procedure TServiceListItem.ChangeStatus(const ANewStatus: Boolean);
begin
  if (FEnabled and not ANewStatus) then
  begin
    if not Disable() then
      raise Exception.Create('Unknown error!');
  end  //of begin
  else
    if (not FEnabled and ANewStatus) then
    begin
      if not Enable() then
        raise Exception.Create('Unknown error!');
    end;  //of if

  inherited ChangeStatus(ANewStatus);
end;

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
      if CheckWin32Version(6) then
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
    if CheckWin32Version(6) then
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
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
    CloseServiceHandle(Service);
  end;  //of try
end;

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
    if CheckWin32Version(6) then
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
    if CheckWin32Version(6) then
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
    FTime := 0;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
    CloseServiceHandle(Service);
  end;  //of try
end;

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
      if CheckWin32Version(6) then
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

procedure TServiceListItem.Rename(const ANewName: string);
var
  Service: SC_HANDLE;

begin
  Service := GetHandle(SERVICE_CHANGE_CONFIG);

  try
    // Rename service
    if not ChangeServiceConfig(Service, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE,
      SERVICE_NO_CHANGE, nil, nil, nil, nil, nil, nil, PChar(ANewName)) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    // Important: It is only possible to rename the caption of service items!
    FCaption := ANewName;

  finally
    CloseServiceHandle(Service);
  end;  //of try
end;

function TServiceListItem.ToString(): string;
begin
  Result := 'Service';
end;


{ TServiceList }

constructor TServiceList.Create();
begin
  inherited Create;
  FManager := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE,
    SC_MANAGER_ENUMERATE_SERVICE or SC_MANAGER_CREATE_SERVICE);
end;

destructor TServiceList.Destroy();
begin
  CloseServiceHandle(FManager);
  inherited Destroy;
end;

function TServiceList.Add(const AFileName, AArguments, ACaption: string): Boolean;
var
  Name, FullPath: string;
  Service: SC_Handle;
  LastError: DWORD;

begin
  Result := False;
  Name := ExtractFileName(AFileName);

  // Check invalid extension
  Assert(ExtractFileExt(Name) = '.exe', 'Invalid program extension! Must be ''.exe''!');

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
    Result := (Add(TServiceListItem.Create(Name, ACaption, FullPath, True, 
      ssAutomatic, FManager)) <> -1);

    // Refresh TListView
    DoNotifyOnFinished();

  finally
    FLock.Release();
  end;  //of try
end;

procedure TServiceList.ExportList(const AFileName: string);
var
  i: Integer;
  RegFile: TRegistryFile;
  Item: TServiceListItem;

begin
  FLock.Acquire;

  // Init Reg file
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    // Export enabled services
    for i := 0 to Count - 1 do
    begin
      Item := TServiceListItem(Items[i]);
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

procedure TServiceList.Load(AExpertMode: Boolean = False);
var
  SearchThread: TServiceSearchThread;

begin
  SearchThread := TServiceSearchThread.Create(Self, FLock, FManager, AExpertMode);

  with SearchThread do
  begin
    OnError := OnSearchError;
    OnFinish := OnSearchFinish;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    Start;
  end;  //of with
end;

function TServiceList.LoadService(const AName: string; AServiceHandle: SC_HANDLE;
  AIncludeDemand: Boolean = False): Integer;
var
  ServiceConfig: PQueryServiceConfig;
  BytesNeeded, LastError: DWORD;
  ServiceStart: TServiceStart;
  Reg: TRegistry;
  Start: TServiceStart;
  Item: TServiceListItem;
  Caption, FileName: string;

begin
  Result := -1;
  ServiceConfig := nil;

  // Determine the required size for buffer
  QueryServiceConfig(AServiceHandle, ServiceConfig, 0, BytesNeeded);
  LastError := GetLastError();

  // ERROR_INSUFFICIENT_BUFFER will be fired normally
  if (LastError <> ERROR_INSUFFICIENT_BUFFER) then
    raise EServiceException.Create(SysErrorMessage(LastError));

  GetMem(ServiceConfig, BytesNeeded);

  try
    // Read service config
    if not QueryServiceConfig(AServiceHandle, ServiceConfig, BytesNeeded, BytesNeeded) then
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

    Caption := ServiceConfig^.lpDisplayName;
    FileName := ServiceConfig^.lpBinaryPathName;

    // Read last status of disabled service
    if (ServiceStart = ssDisabled) then
    begin
      Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;

        // Windows >= Vista?
        if CheckWin32Version(6) then
          Reg.OpenKey(KEY_SERVICE_DISABLED +'\'+ AName, False)
        else
          Reg.OpenKey(KEY_SERVICE_DISABLED, False);

        // Last status exists?
        if not Reg.ValueExists(AName) then
          Exit;

        Start := TServiceStart(Reg.ReadInteger(AName));

        // Skip demand started services
        if (not AIncludeDemand and (Start = ssManual)) then
          Exit;

        Item := TServiceListItem.Create(AName, Caption, FileName, False, Start,
          FManager);
        Item.Time := Item.GetTimestamp(Reg);

      finally
        Reg.CloseKey();
        Reg.Free;
      end;  //of try
    end  //of begin
    else
      Item := TServiceListItem.Create(AName, Caption, FileName, True,
        ServiceStart, FManager);

    // Add service to list
    Result := Add(Item);

  finally
    FreeMem(ServiceConfig, BytesNeeded);
  end;  //of try
end;


{ TTaskListItem }

constructor TTaskListItem.Create(const AName, AFileName, ALocation: string;
  AEnabled: Boolean; ATask: IRegisteredTask; ATaskFolder: ITaskFolder);
begin
  inherited Create(AName, '', AFileName, ALocation, AEnabled);
  FTask := ATask;
  FTaskFolder := ATaskFolder;
end;

procedure TTaskListItem.ChangeStatus(const ANewStatus: Boolean);
begin
  FTask.Enabled := ANewStatus;
  inherited ChangeStatus(ANewStatus);
end;

function TTaskListItem.GetTaskDefinition(): ITaskDefinition;
begin
  Result := FTask.Definition;
end;

procedure TTaskListItem.UpdateTask(const AName: string; ANewDefinition: ITaskDefinition);
var
  TempLocation: string;
  TaskFile: TStringList;
  NewTask: IRegisteredTask;

begin
  {
  // Temporary workaround:
  // On 32-Bit RegisterTaskDefinition() works fine but on 64-Bit not: WTF!
  OleCheck(FTaskFolder.RegisterTaskDefinition(FTask.Name, Definition,
    TASK_UPDATE, Null, Null, FTask.Definition.Principal.LogonType, Null, NewTask));
  }

  TempLocation := IncludeTrailingBackslash(GetTempDir()) + AName;
  TaskFile := TStringList.Create;

  try
    TaskFile.SetText(ANewDefinition.XmlText);
    TaskFile.SaveToFile(TempLocation, TEncoding.Unicode);
    Delete();

    if not ExecuteProgram('schtasks', '/create /XML "'+ TempLocation
      +'" /tn '+ AName, SW_HIDE, True, True) then
      raise Exception.Create(SysErrorMessage(GetLastError()));

    OleCheck(FTaskFolder.GetTask(PChar(AName), NewTask));
    DeleteFile(TempLocation);
    FTask := NewTask;

  finally
    TaskFile.Free;
  end;  //of try
end;

function TTaskListItem.GetFullLocation(): string;
begin
  if GetFolderPath(CSIDL_SYSTEM, Result) then
    Result := IncludeTrailingBackslash(Result +'Tasks'+ GetLocation()) + Name;
end;

procedure TTaskListItem.ChangeFilePath(const ANewFilePath: string);
var
  Actions: IEnumVariant;
  Action: IAction;
  ActionItem: OleVariant;
  ExecAction: IExecAction;
  Fetched: DWORD;
  Definition: ITaskDefinition;

begin
  Definition := FTask.Definition;
  Actions := (Definition.Actions._NewEnum as IEnumVariant);

  // Try to find executable command in task
  if (Actions.Next(1, ActionItem, Fetched) = 0) then
  begin
    Action := (IDispatch(ActionItem) as IAction);

    // Task has an executable?
    if (Action.ActionType = TASK_ACTION_EXEC) then
    begin
      ExecAction := (Action as IExecAction);

      // Change path + arguments
      ExecAction.Path := PChar(ExtractPathToFile(ANewFilePath));
      ExecAction.Arguments := PChar(ExtractArguments(ANewFilePath));

      // Update information
      UpdateTask(Name, Definition);
      inherited ChangeFilePath(ANewFilePath);
    end;  //of begin
  end;  //of while
end;

function TTaskListItem.Delete(): Boolean;
begin
  OleCheck(FTaskFolder.DeleteTask(PChar(Name), 0));
  Result := True;
end;

procedure TTaskListItem.ExportItem(const AFileName: string);
var
  Win64: Boolean;

begin
  Win64 := (TOSVersion.Architecture = arIntelX64);

  // Deny WOW64 redirection on 64 Bit Windows
  if Win64 then
    Wow64FsRedirection(True);

  try
    // Copy file
    if not CopyFile(PChar(GetFullLocation()), PChar(AFileName), False) then
      raise ETaskException.Create(SysErrorMessage(GetLastError()));

  finally
    // Allow WOW64 redirection on 64 Bit Windows again
    if Win64 then
      Wow64FsRedirection(False);
  end;  //of try
end;

function TTaskListItem.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := ALanguageFile.GetString(LID_FILTER_XML_FILES);
end;

procedure TTaskListItem.Rename(const ANewName: string);
begin
  UpdateTask(ANewName, FTask.Definition);
  inherited Rename(ANewName);
end;

function TTaskListItem.ToString(): string;
begin
  Result := 'Task';
end;


{ TTaskList }

constructor TTaskList.Create;
begin
  inherited Create;
  CoInitialize(nil);

{$IFNDEF DEBUG}
  if Failed(CoInitializeSecurity(nil, -1, nil, nil, RPC_C_AUTHN_LEVEL_PKT_PRIVACY,
    RPC_C_IMP_LEVEL_IMPERSONATE, nil, EOAC_NONE, nil)) then
    raise ETaskException.Create('Could not register security values for process!');
{$ENDIF}

  if Failed(CoCreateInstance(CLSID_TaskScheduler, nil, CLSCTX_INPROC_SERVER,
    IID_ITaskService, FTaskService)) then
    raise ETaskException.Create('Could not create task service!');

  if Failed(FTaskService.Connect(Null, Null, Null, Null)) then
    raise ETaskException.Create('Could not connect to task service!');
end;

destructor TTaskList.Destroy;
begin
  CoUninitialize();
  inherited Destroy;
end;

function TTaskList.AddTaskItem(ATask: IRegisteredTask; ATaskFolder: ITaskFolder): Integer;
var
  Item: TTaskListItem;
  Action: IAction;
  ExecAction: IExecAction;
  Actions: IEnumVariant;
  ActionItem: OleVariant;
  Fetched: DWORD;
  FileName: string;

begin
  // Try to find executable command in task
  Actions := (ATask.Definition.Actions._NewEnum as IEnumVariant);

  if (Actions.Next(1, ActionItem, Fetched) = 0) then
  begin
    Action := (IDispatch(ActionItem) as IAction);

    // Task has an executable?
    if (Action.ActionType = TASK_ACTION_EXEC) then
    begin
      ExecAction := (Action as IExecAction);
      FileName := ExecAction.Path;

      // Append arguments?
      if (ExecAction.Arguments <> '') then
        FileName := FileName +' '+ ExecAction.Arguments;
    end;  //of begin
  end;  //of while

  Item := TTaskListItem.Create(ATask.Name, FileName, ExtractFileDir(ATask.Path),
    ATask.Enabled, ATask, ATaskFolder);
  Result := Add(Item);
end;

procedure TTaskList.ExportList(const AFileName: string);
var
  i: Integer;
  ZipFile: TZipFile;
  Item: TTaskListItem;
  Path, ZipLocation: string;
{$IFDEF WIN32}
  Win64: Boolean;
{$ENDIF}

begin
  FLock.Acquire();
  ZipFile := TZipFile.Create;

{$IFDEF WIN32}
  Win64 := (TOSVersion.Architecture = arIntelX64);

  // Deny WOW64 redirection on 64 Bit Windows
  if Win64 then
    Wow64FsRedirection(True);
{$ENDIF}

  try
    ZipFile.Open(AFileName, zmWrite);

    // For validation purposes: "Clearas" is the comment
    ZipFile.Comment := 'Clearas';
    Path := IncludeTrailingPathDelimiter(Path);

    for i := 0 to Count - 1 do
    begin
      Item := Items[i];
      ZipLocation := IncludeTrailingPathDelimiter(Item.Location);
      ZipLocation := Copy(ZipLocation, 2, Length(Item.Location));
      ZipLocation := StringReplace(ZipLocation + Item.Name, '\', '/', [rfReplaceAll]);
      ZipFile.Add(Item.LocationFull, ZipLocation, zcDeflate);
    end;  //of for

    ZipFile.Close();

  finally
    ZipFile.Free;
  {$IFDEF WIN32}
    // Allow WOW64 redirection on 64 Bit Windows again
    if Win64 then
      Wow64FsRedirection(False);
  {$ENDIF}
    FLock.Release();
  end;  //of try
end;

function TTaskList.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := ALanguageFile.GetString(LID_FILTER_ZIP_FILES);
end;

function TTaskList.GetImportFilter(ALanguageFile: TLanguageFile): string;
begin
//  Result := Format('%s|%s', [ALanguageFile.GetString(LID_FILTER_XML_FILES),
//    ALanguageFile.GetString(LID_FILTER_ZIP_FILES)]);
  Result := ALanguageFile.GetString(LID_FILTER_XML_FILES);
end;

function TTaskList.ImportBackup(const AFileName: TFileName): Boolean;
var
  Ext, Path: string;
  TaskFolder: ITaskFolder;
  NewTask: IRegisteredTask;
  ErrorCode: DWORD;
  //ZipFile: TZipFile;
{$IFDEF WIN32}
  Win64: Boolean;
{$ENDIF}

begin
  Result := False;
  Ext := ExtractFileExt(AFileName);

  // Check invalid extension
  if ((Ext <> '.xml') and (Ext <> '.zip')) then
    raise EAssertionFailed.Create('Invalid backup file extension! Must be '
      +'''.xml'' or ''.zip''!');

  // List locked?
  if not FLock.TryEnter() then
    raise EListBlocked.Create('Another operation is pending. Please wait!');

{$IFDEF WIN32}
  Win64 := (TOSVersion.Architecture = arIntelX64);

  // Deny WOW64 redirection on 64 Bit Windows
  if Win64 then
    Wow64FsRedirection(True);
{$ENDIF}

  try
    if (Ext = '.xml') then
    begin
      // Open root task folder
      OleCheck(FTaskService.GetFolder('\', TaskFolder));
      Path := TaskFolder.Path + ChangeFileExt(ExtractFileName(AFileName), '');

      // Task exists?
      if Succeeded(TaskFolder.GetTask(PChar(Path), NewTask)) then
        Exit;

      {
      // Temporary workaround:
      // On 32-Bit RegisterTask() works fine but on 64-Bit not: WTF!
      OleCheck(TaskFolder.RegisterTask(PChar(Path), PChar(XmlText),
        TASK_CREATE, Null, Null, TASK_LOGON_INTERACTIVE_TOKEN, Null, NewTask));
      }
      if not ExecuteProgram('schtasks', '/create /XML "'+ AFileName +'" /tn '+
        Copy(Path, 2, Length(Path)), SW_HIDE, True, True) then
      begin
        ErrorCode := GetLastError();

        if (ErrorCode = ERROR_SUCCESS) then
          ErrorCode := SCHED_E_MALFORMEDXML;

        raise ETaskException.Create(SysErrorMessage(ErrorCode));
      end;  //of begin

      OleCheck(TaskFolder.GetTask(PChar(Path), NewTask));

      // Add new task to list
      Result := (AddTaskItem(NewTask, TaskFolder) <> -1);

      // Refresh TListView
      DoNotifyOnFinished();
    end;  //of begin
    // TODO: ZIP file import
    {else
    begin
      ZipFile := TZipFile.Create;

      try
        ZipFile.Open(AFileName, zmRead);

        // For validation purposes: "Clearas" is the comment
        if (ZipFile.Comment <> 'Clearas') then
          raise ETaskException.Create(SysErrorMessage(SCHED_E_INVALID_TASK_HASH));

        Path := GetKnownFolderPath(FOLDERID_System) +'Tasks';

        if not DirectoryExists(Path) then
          raise ETaskException.Create('Task directory does not exist!');

        ZipFile.ExtractAll(Path);
        ZipFile.Close();
        Load();
        Result := True;

      finally
        ZipFile.Free;
      end;  //of try
    end;  //of if}

  finally
  {$IFDEF WIN32}
    // Allow WOW64 redirection on 64 Bit Windows again
    if Win64 then
      Wow64FsRedirection(False);
  {$ENDIF}
    FLock.Release();
  end;  //of try
end;

procedure TTaskList.Load(AExpertMode: Boolean = False);
begin
  LoadTasks('\', AExpertMode);
end;

procedure TTaskList.LoadTasks(ATaskFolder: ITaskFolder; AIncludeHidden: Boolean);
var
  TaskCollection: IRegisteredTaskCollection;
  Task: IRegisteredTask;
  Tasks: IEnumVariant;
  TaskItem: OleVariant;
  Fetched: DWORD;
  Flags: LONG;

begin
  // Show hidden?
  if AIncludeHidden then
    Flags := LONG(TASK_ENUM_HIDDEN)
  else
    Flags := 0;

  // Add tasks in current folder
  OleCheck(ATaskFolder.GetTasks(Flags, TaskCollection));
  Tasks := (TaskCollection._NewEnum as IEnumVariant);

  // Add tasks to list
  while (Tasks.Next(1, TaskItem, Fetched) = 0) do
    try
      Task := (IDispatch(TaskItem) as IRegisteredTask);
      AddTaskItem(Task, ATaskFolder);

    except
      // Task currupted: Skip it!
      Continue;
    end;  //of try
end;

procedure TTaskList.LoadTasks(APath: string = '\'; AExpertMode: Boolean = False);
var
  SearchThread: TTaskSearchThread;

begin
  SearchThread := TTaskSearchThread.Create(Self, FLock, FTaskService, AExpertMode);

  with SearchThread do
  begin
    Path := APath;
    Win64 := (TOSVersion.Architecture = arIntelX64);
    OnError := OnSearchError;
    OnFinish := OnSearchFinish;
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    Start;
  end;  //of with
end;

end.


