{ *********************************************************************** }
{                                                                         }
{ Clearas API Interface Unit                                              }
{                                                                         }
{ Copyright (c) 2011-2017 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasAPI;

{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Winapi.Windows, Winapi.WinSvc, System.Classes, System.SysUtils, System.Win.Registry,
  Winapi.ShlObj, Winapi.ActiveX, System.Win.ComObj, System.Zip, Vcl.Graphics,
  Winapi.ShellAPI, System.SyncObjs, System.Generics.Collections, System.IOUtils,
  Winapi.KnownFolders, System.Variants, Winapi.Taskschd, Vcl.ImgList, PMCW.Registry,
  PMCW.LanguageFile, PMCW.IniFileParser, PMCW.SysUtils;

type
  /// <summary>
  ///   Executable command that contains at least a filename and optional
  ///   arguments.
  /// </summary>
  TCommandString = type string;

  TCommandStringHelper = record helper for TCommandString
    /// <summary>
    ///   Creates a new <see cref="TCommandString"/>.
    /// </summary>
    /// <param name="AFileName">
    ///   A filename.
    /// </param>
    /// <param name="AArguments">
    ///   Optional: Arguments.
    /// </param>
    /// <param name="AAddQuotes">
    ///  Optional: Add missing quotes if filename contains white spaces.
    /// </param>
    class function Create(const AFileName: string; const AArguments: string = '';
      AAddQuotes: Boolean = False): TCommandString; inline; static;

    /// <summary>
    ///   Executes the command.
    /// </summary>
    procedure Execute(); inline;

    /// <summary>
    ///   Expands the command.
    /// </summary>
    /// <remarks>
    ///   Substitution expressions like %PATH% are resolved and relative paths
    ///   are converted to absolute ones.
    /// </remarks>
    /// <returns>
    ///   The expanded command if successfully expanded or empty string otherwise.
    /// </returns>
    function Expand(): TCommandString;

    /// <summary>
    ///   Extracts the optional arguments.
    /// </summary>
    /// <returns>
    ///   The arguments.
    /// </returns>
    function ExtractArguments(): string;

    /// <summary>
    ///   Extracts the filename.
    /// </summary>
    /// <returns>
    ///   The filename.
    /// </returns>
    function ExtractFileName(): string;
  end;

  /// <summary>
  ///   A <c>TLnkFile</c> represents a symbolic link file.
  /// </summary>
  TLnkFile = class(TObject)
  private
    FFileName,
    FExeFileName: TFileName;
    FArguments: string;
    function GetCommand(): TCommandString;
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
    function Save(const AFileName, AExeFileName: TFileName;
      const AArguments: string = ''): Boolean; overload;
  public
    const
      /// <summary>
      ///   The symbolic link file extension.
      /// </summary>
      FileExtension = '.lnk';

    /// <summary>
    ///   Constructor for creating a <c>TLnkFile</c> instance.
    /// </summary>
    /// <param name="AFileName">
    ///   The filename of the .lnk file.
    /// </param>
    constructor Create(const AFileName: TFileName); reintroduce;

    /// <summary>
    ///   Checks if the .lnk file exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the .lnk file exists or <c>False</c> otherwise.
    /// </returns>
    function Exists(): Boolean; inline;

    /// <summary>
    ///   Checks if arguments are specified.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if arguments are specified or <c>False</c> otherwise.
    /// </returns>
    function HasArguments(): Boolean; inline;

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
    property Arguments: string read FArguments write FArguments;

    /// <summary>
    ///   Returns the concatenation of filename and arguments.
    /// </summary>
    property Command: TCommandString read GetCommand;

    /// <summary>
    ///   Gets or sets the filename to the .exe file.
    /// </summary>
    property ExeFileName: TFileName read FExeFileName write FExeFileName;

    /// <summary>
    ///   Gets or sets the filename to the .lnk file.
    /// </summary>
    property FileName: TFileName read FFileName write FFileName;
  end;

  /// <summary>
  ///   Raised when no item is selected.
  /// </summary>
  EInvalidItem = class(EAccessViolation);

  /// <summary>
  ///   Raised when another operation is pending on the current list and
  ///   therefore locked.
  /// </summary>
  EListBlocked = class(EAbort);

  /// <summary>
  ///   A non-critical warning.
  /// </summary>
  EWarning = class(EAbort);

  /// <summary>
  ///   Raised when trying to add an item that is already present.
  /// </summary>
  EAlreadyExists = class(EAbort);

  /// <summary>
  ///   A <c>TRootItem</c> represents an basic list item that can be added to a
  ///   <c>TRootList</c>.
  /// </summary>
  /// <remarks>
  ///   This class is intended to be only ancenstor for items.
  /// </remarks>
  TRootItem = class(TObject)
  private
    FErasable: Boolean;
    FImageIndex: Integer;
  protected
    FEnabled: Boolean;
    FCommand: TCommandString;
    FName,
    FLocation,
    FCaption: string;

    /// <summary>
    ///   Gets the complete store location.
    /// </summary>
    /// <returns>
    ///   The file name.
    /// </returns>
    function GetFullLocation(): string; virtual; abstract;

    /// <summary>
    ///   Reads the file description from a file.
    /// </summary>
    /// <param name="AFileName">
    ///   The file name.
    /// </param>
    /// <returns>
    ///   The file description.
    /// </returns>
    function GetFileDescription(const AFileName: TFileName): string;

    /// <summary>
    ///   Gets the handle to a .exe file.
    /// </summary>
    /// <param name="AExeFileName">
    ///   The .exe file.
    /// </param>
    /// <returns>
    ///   The handle to the icon.
    /// </returns>
    function GetIcon(const AExeFileName: TFileName): HICON; virtual;

    /// <summary>
    ///   Gets the icon filename.
    /// </summary>
    /// <returns>
    ///   The icon filename.
    /// </returns>
    function GetIconFileName(): string; virtual;

    /// <summary>
    ///   Gets the store location. Usually a Registry key.
    /// </summary>
    /// <returns>
    ///   The store location.
    /// </returns>
    function GetLocation(): string; virtual;

    /// <summary>
    ///   Checks if the item can be deleted.
    /// </summary>
    /// <remarks>
    ///   <see cref="UpdateErasable"/> calls this method and caches the return
    ///   value in <see cref="Erasable"/>. An item is erasable if filename is empty
    ///   or does not exist. Override this method in derived class to implement
    ///   custom behaviour.
    /// </remarks>
    function IsErasable(): Boolean; virtual;

    /// <summary>
    ///   Renames the item.
    /// </summary>
    /// <param name="ANewName">
    ///   The new name.
    /// </param>
    procedure Rename(const ANewName: string); virtual;

    /// <summary>
    ///   Changes the command.
    /// </summary>
    /// <param name="ACommand">
    ///   The new command.
    /// </param>
    procedure SetCommand(const ACommand: TCommandString); virtual;

    /// <summary>
    ///   Changes the status.
    /// </summary>
    /// <param name="AEnabled">
    ///   The new status.
    /// </param>
    procedure SetEnabled(const AEnabled: Boolean); virtual;
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
    /// <param name="ACommand">
    ///   The command.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    constructor Create(const AName, ACaption: string; const ACommand: TCommandString;
      const ALocation: string; AEnabled: Boolean); reintroduce;

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; virtual; abstract;

    /// <summary>
    ///   Executes the item.
    /// </summary>
    procedure Execute(); virtual;

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
    ///   <c>True</c> if the file in property <see cref="Command"/> exists or
    ///   <c>False</c> otherwise.
    /// </returns>
    function FileExists(): Boolean;

    /// <summary>
    ///   Gets the file extension for backup files.
    /// </summary>
    /// <returns>
    ///   The backup extension.
    /// </returns>
    function GetBackupExtension(): string; virtual; abstract;

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
    ///   Loads the icon.
    /// </summary>
    /// <param name="AIcon">
    ///   The resulting icon.
    /// </param>
    procedure LoadIcon(AIcon: TIcon);

    /// <summary>
    ///   Opens the folder that contains the .exe file in Windows Explorer and
    ///   selects this file. If the file does not exist a <see cref="EWarning"/>
    ///   is raised.
    /// </summary>
    /// <exception>
    ///   <c>EWarning</c> if file does not exist.
    /// </exception>
    procedure OpenInExplorer();

    /// <summary>
    ///   Updates the cached <see cref="Erasable"/> property.
    /// </summary>
    procedure UpdateErasable(); inline;

    /// <summary>
    ///   Gets the display name.
    /// </summary>
    property Caption: string read FCaption;

    /// <summary>
    ///   Gets or sets the filename including arguments.
    /// </summary>
    property Command: TCommandString read FCommand write SetCommand;

    /// <summary>
    ///   Gets or sets the enabled status.
    /// </summary>
    property Enabled: Boolean read FEnabled write SetEnabled;

    /// <summary>
    ///   Determines if the item can be deleted.
    /// </summary>
    property Erasable: Boolean read FErasable;

    /// <summary>
    ///   Gets the icon filename.
    /// </summary>
    property IconFileName: string read GetIconFileName;

    /// <summary>
    ///   Index of the icon inside the <see cref="TRootList.Images"/>.
    /// </summary>
    property ImageIndex: Integer read FImageIndex write FImageIndex;

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
    function GetFullLocation(): string; override;

    /// <summary>
    ///   Deletes a specified registry key and its subkeys.
    /// </summary>
    /// <param name="AHKey">
    ///   The root key.
    /// </param>
    /// <param name="AKeyPath">
    ///   The key which contains the key to be deleted.
    /// </param>
    /// <param name="AKeyName">
    ///   The key to be deleted.
    /// </param>
    /// <param name="AFailIfNotExists">
    ///   Optional: Set to <c>False</c> to avoid raising <c>EWarning</c> if the
    ///   key does not exist.
    /// </param>
    /// <returns>
    ///   <c>True</c> if key was successfully deleted or <c>False</c> otherwise.
    /// </returns>
    /// <exception>
    ///   <c>Exception</c> if key does not exist.
    ///   <c>EWarning</c> if key has already been deleted.
    /// </exception>
    function DeleteKey(AHKey: HKEY; const AKeyPath, AKeyName: string;
      AFailIfNotExists: Boolean = True): Boolean;

    /// <summary>
    ///   Gets the root Registry key.
    /// </summary>
    /// <returns>
    ///    The root Registry key.
    /// </returns>
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
    /// <exception>
    ///   <c>ERegistryException</c> when timestamp could not be written.
    /// </exception>
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
    /// <param name="ACommand">
    ///   The command.
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
    constructor Create(const AName, ACaption: string;
      const ACommand: TCommandString; const ALocation: string; AEnabled, AWow64: Boolean);

    /// <summary>
    ///   Gets the file extension for backup files.
    /// </summary>
    /// <returns>
    ///   The backup extension.
    /// </returns>
    function GetBackupExtension(): string; override;

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
    function GetTimestamp(const AReg: TRegistry): TDateTime;

    /// <summary>
    ///   Opens the item location in RegEdit.
    /// </summary>
    procedure OpenInRegEdit(); overload; virtual;

    /// <summary>
    ///   Gets the root Registry key.
    /// </summary>
    property RootKey: TRootKey read GetRootKey;

    /// <summary>
    ///   Item is redirected by WOW64.
    /// </summary>
    property Wow64: Boolean read FWow64;
  end;

  /// <summary>
  ///   Occurs when something has failed.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="AErrorMessage">
  ///   The error message.
  /// </param>
  TErrorEvent = procedure(Sender: TObject; const AErrorMessage: string) of object;

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
    ///   Gets the file filter for an <c>TOpenDialog</c> from a <c>TLanguageFile</c>.
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
  ///   You can use the <see cref="IsExporting"/> method to see if the list is
  ///   being exported.
  /// </summary>
  /// <remarks>
  ///   This class is intended to be used only as ancestor for other classes.
  /// </remarks>
  TRootList<T: TRootItem> = class(TObjectList<T>, IInterface)
  strict private
    FAllowDuplicates: Boolean;
    FOnCounterUpdate: TNotifyEvent;
    FEnabledItemsCount,
    FErasableItemsCount: Integer;
    FIcon: TIcon;
    procedure NotifyOnCounterUpdate();
  protected
    FImages: TCustomImageList;
    FSearchLock,
    FExportLock: TCriticalSection;
    procedure Notify(const Item: T; Action: TCollectionNotification); override;

    { IInterface }
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
    ///   Removes all items from the list.
    /// </summary>
    procedure Clear();

    /// <summary>
    ///   Changes the command of the specified item.
    /// </summary>
    /// <param name="AItem">
    ///   The item.
    /// </param>
    /// <param name="ACommand">
    ///   The new command.
    /// </param>
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EInvalidItem</c> if no item is selected.
    /// </exception>
    procedure ChangeCommand(AItem: T; const ACommand: TCommandString);

    /// <summary>
    ///   Changes the enabled status of an item.
    /// </summary>
    /// <param name="AItem">
    ///   The item.
    /// </param>
    /// <param name="AEnabled">
    ///   The new status.
    /// </param>
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EInvalidItem</c> if no item is selected.
    ///   <c>EWarning</c> if new status is equal to current.
    /// </exception>
    procedure ChangeItemStatus(AItem: T; const AEnabled: Boolean);

    /// <summary>
    ///   Deletes an item.
    /// </summary>
    /// <param name="AItem">
    ///   The item.
    /// </param>
    /// <returns>
    ///   <c>True</c> if item was successfully deleted or <c>False</c> otherwise.
    /// </returns>
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EInvalidItem</c> if no item is selected.
    /// </exception>
    function DeleteItem(AItem: T): Boolean;

    /// <summary>
    ///   Disables an item.
    /// </summary>
    /// <param name="AItem">
    ///   The item.
    /// </param>
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EInvalidItem</c> if no item is selected.
    ///   <c>EWarning</c> if item is already disabled.
    /// </exception>
    procedure DisableItem(AItem: T);

    /// <summary>
    ///   Enables an item.
    /// </summary>
    /// <param name="AItem">
    ///   The item.
    /// </param>
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EInvalidItem</c> if no item is selected.
    ///   <c>EWarning</c> if item is already enabled.
    /// </exception>
    procedure EnableItem(AItem: T);

    /// <summary>
    ///   Exports an item as file.
    /// </summary>
    /// <param name="AItem">
    ///   The item.
    /// </param>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    /// <exception>
    ///   <c>EInvalidItem</c> if no item is selected.
    /// </exception>
    procedure ExportItem(AItem: T; const AFileName: string);

    /// <summary>
    ///   Exports the complete list as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportList(const AFileName: string); virtual; abstract;

    /// <summary>
    ///   Gets the file extension for backup files.
    /// </summary>
    /// <returns>
    ///   The backup extension.
    /// </returns>
    function GetBackupExtension(): string; virtual;

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
    ///   Checks if the list is currently being exported.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the list is locked or <c>False</c> otherwise.
    /// </returns>
    function IsExporting(): Boolean;

    /// <summary>
    ///   Renames an item.
    /// </summary>
    /// <param name="AItem">
    ///   The item.
    /// </param>
    /// <param name="ANewName">
    ///   The new name.
    /// </param>
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EInvalidItem</c> if no item is selected.
    ///   <c>EWarning</c> if item with new name already exists.
    /// </exception>
    procedure RenameItem(AItem: T; const ANewName: string);

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   Use advanced search mode.
    /// </param>
    /// <param name="AWin64">
    ///   If set to <c>True</c> search for 64-bit items. Otherwise search for
    ///   32-bit items.
    /// </param>
    procedure Search(AExpertMode: Boolean = False; AWin64: Boolean = True); virtual; abstract;

    /// <summary>
    ///   Allow items with the the same name.
    /// </summary>
    property AllowDuplicates: Boolean read FAllowDuplicates write FAllowDuplicates;

    /// <summary>
    ///   Gets the count of enabled items in the list.
    /// </summary>
    property EnabledItemsCount: Integer read FEnabledItemsCount;

    /// <summary>
    ///   Gets the count of erasable marked items.
    /// </summary>
    property ErasableItemsCount: Integer read FErasableItemsCount;

    /// <summary>
    ///   Contains the images of the items.
    /// </summary>
    /// <remarks>
    ///   Use <see cref="TRootItem.ImageIndex"/> to get the index of the image.
    /// </remarks>
    property Images: TCustomImageList read FImages write FImages;

    /// <summary>
    ///   Occurs when counter is updated.
    /// </summary>
    property OnCounterUpdate: TNotifyEvent read FOnCounterUpdate write FOnCounterUpdate;
  end;

  /// <summary>
  ///   Abstract thread which should perform a long-lasting operation on a
  ///   <see cref="TRootList"/>.
  /// </summary>
  /// <remarks>
  ///   Implement <c>DoExecute()</c> in the derived class.
  /// </remarks>
  TRootListThread = class(TThread)
  strict private
    FOnStart,
    FOnListLocked: TNotifyEvent;
    FOnError: TErrorEvent;
    FErrorMessage: string;
    procedure DoNotifyOnError();
    procedure DoNotifyOnListLocked();
    procedure DoNotifyOnStart();
  protected
    FSelectedList: TRootList<TRootItem>;
    procedure DoExecute(); virtual; abstract;
    procedure Execute(); override; final;
  public
    /// <summary>
    ///   Contructor for creating a <c>TRootListThread</c> instance.
    /// </summary>
    /// <param name="ASelectedList">
    ///   The list on which the operation should be performed.
    /// </param>
    constructor Create(ASelectedList: TRootList<TRootItem>);

    /// <summary>
    ///   Occurs when something went wrong.
    /// </summary>
    property OnError: TErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Occurs when another operation is pending on the list and therefore locked.
    /// </summary>
    property OnListLocked: TNotifyEvent read FOnListLocked write FOnListLocked;

    /// <summary>
    ///   Occurs when thread has started.
    /// </summary>
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

  /// <summary>
  ///   Performs a search.
  /// </summary>
  TSearchThread = class(TRootListThread)
  private
    FWin64,
    FExpertMode: Boolean;
  protected
    procedure DoExecute(); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TSearchThread</c> instance.
    /// </summary>
    /// <param name="ASelectedList">
    ///   A <c>TRootList</c> to be filled.
    /// </param>
    constructor Create(ASelectedList: TRootList<TRootItem>);

    /// <summary>
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </summary>
    property ExpertMode: Boolean read FExpertMode write FExpertMode;

    /// <summary>
    ///   Search for 64-bit items.
    /// </summary>
    property Win64: Boolean read FWin64 write FWin64;
  end;

  /// <summary>
  ///   Exports a <see cref="TRootList"/> as file.
  /// </summary>
  TExportListThread = class(TRootListThread)
  private
    FFileName: string;
    FPageControlIndex: Integer;
  protected
    procedure DoExecute(); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TExportListThread</c> instance.
    /// </summary>
    /// <param name="ASelectedList">
    ///   The <c>TRootList</c> which should be exported.
    /// </param>
    /// <param name="AFileName">
    ///   The filename of the exported file.
    /// </param>
    /// <param name="APageControlIndex">
    ///   The index of the <c>TPageControl</c> on which the export was invoked.
    /// </param>
    constructor Create(ASelectedList: TRootList<TRootItem>;
      const AFileName: string; APageControlIndex: Integer);

    /// <summary>
    ///   The index of the <c>TPageControl</c> on which the export was invoked.
    /// </summary>
    property PageControlIndex: Integer read FPageControlIndex;
  end;

  /// <summary>
  ///   Raised by startup feature classes.
  /// </summary>
  EStartupException = class(Exception);

const
  /// <summary>
  ///   Used in <see cref="TStartupItemStatus"/> to signal that a startup item
  ///   is enabled.
  /// </summary>
  ST_ENABLED  = $2;

  /// <summary>
  ///   Used in <see cref="TStartupItemStatus"/> to signal that a startup item
  ///   is disabled.
  /// </summary>
  ST_DISABLED = $3;

type
  /// <summary>
  ///   A <c>TStartupItemStatus</c> represents the new status of startup items
  ///   since Windows 8. The status of startup items is a binary value of 12
  ///   bytes. The first 4 bytes contain the enabled value. If the status is
  ///   disabled the last 8 bytes are a timestamp containing the deactivation
  ///   time as <c>TFileTime</c>.
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

    /// <summary>
    ///   Converts the deactivation time to <c>TDateTime</c>.
    /// </summary>
    /// <returns>
    ///   The deactivation time.
    /// </returns>
    function GetDeactivationTime(): TDateTime;

    /// <summary>
    ///   Determines if the status is enabled.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the status is enabled or <c>False</c> otherwise.
    /// </returns>
    function GetEnabled(): Boolean;

    /// <summary>
    ///   Changes the status.
    /// </summary>
    /// <param name="AEnabled">
    ///   The new status.
    /// </param>
    procedure SetEnabled(AEnabled: Boolean);
  end;

  /// <summary>
  ///   A <c>TStartupListItem</c> represents a basic startup item that can be
  ///   added to a <see cref="TStartupList"/>.
  /// </summary>
  /// <remarks>
  ///   This class is intended to be only ancenstor for items.
  /// </remarks>
  TStartupListItem = class(TRegistryItem)
  protected
    FDeactivationTime: TDateTime;
    FRootKey: TRootKey;
    function Disable(): Boolean; virtual; deprecated 'Since Windows 8'; abstract;
    function Enable(): Boolean; virtual; deprecated 'Since Windows 8'; abstract;
    function DeleteValue(const AKeyPath: string; AReallyWow64: Boolean = True): Boolean;
    function GetApprovedLocation(): string; virtual; abstract;
    function GetRootKey(): TRootKey; override;
    function GetWow64Key(): string; virtual;
    procedure Rename(const ANewName: string); overload; override;
    function Rename(const AKeyPath, ANewName: string;
      AReallyWow64: Boolean = True): Boolean; reintroduce; overload;
    procedure SetCommand(const ACommand: TCommandString); override;
    procedure SetEnabled(const AEnabled: Boolean); override;
  public
    const
      /// <summary>
      ///   Registry status base key.
      /// </summary>
      /// <remarks>
      ///   Only since Windows 8!
      /// </remarks>
      StartupApprovedBaseKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\StartupApproved\';

    /// <summary>
    ///   Constructor for creating a <c>TStartupListItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACommand">
    ///   The command.
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
    constructor Create(const AName: string; const ACommand: TCommandString;
      const ALocation: string; ARootKey: TRootKey; AEnabled, AWow64: Boolean);

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
    property DeactivationTime: TDateTime read FDeactivationTime write FDeactivationTime;

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
    function GetApprovedLocation(): string; override;
    procedure Rename(const ANewName: string); override;
  public
    const
      /// <summary>
      ///   Registry key of disabled items.
      /// </summary>
      /// <remarks>
      ///   DEPRECATED since Windows 8!
      /// </remarks>
      DisabledKey          = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupreg\' deprecated;

      /// <summary>
      ///   Registry status key.
      /// </summary>
      /// <remarks>
      ///   Only since Windows 8!
      /// </remarks>
      StartupApprovedKey   = TStartupListItem.StartupApprovedBaseKey +'Run';

      /// <summary>
      ///   Registry status key of 32-bit items.
      /// </summary>
      /// <remarks>
      ///   Only present on 64-bit Windows and since Windows 8!
      /// </remarks>
      StartupApproved32Key = TStartupListItem.StartupApprovedBaseKey +'Run32';

      /// <summary>
      ///   Registry key of enabled items that are executed during every system
      ///   startup.
      /// </summary>
      StartupRunKey        = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';

      /// <summary>
      ///   Registry key of enabled 32-bit items that are executed during every
      ///   system startup.
      /// </summary>
      /// <remarks>
      ///   Only present on 64-bit Windows!
      /// </remarks>
      StartupRunKey32      = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';

      /// <summary>
      ///   Registry key of enabled items that are executed only during the next
      ///   system startup.
      /// </summary>
      StartupRunOnceKey    = 'SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce';

      /// <summary>
      ///   Registry key of enabled 32-bit items that are executed only during
      ///   the next system startup.
      /// </summary>
      /// <remarks>
      ///   Only present on 64-bit Windows!
      /// </remarks>
      StartupRunOnceKey32  = 'SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce';

    /// <summary>
    ///   Constructor for creating a <c>TStartupItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACommand">
    ///   The command.
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
    constructor Create(const AName: string; const ACommand: TCommandString;
      const ALocation: string; ARootKey: TRootKey; AEnabled, AWow64, ARunOnce: Boolean);

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
    FLnkFile: TLnkFile;
    FStartupUser: Boolean;
    function AddCircumflex(const AName: string): string;
    function BackupExists(): Boolean; deprecated 'Since Windows 8';
    procedure CreateBackup(); deprecated 'Since Windows 8';
    function DeleteBackup(): Boolean; deprecated 'Since Windows 8';
    function GetBackupDir(): string; deprecated 'Since Windows 8';
    function GetBackupLnk(): string; deprecated 'Since Windows 8';
  protected
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    function GetApprovedLocation(): string; override;
    function GetFullLocation(): string; override;
    procedure Rename(const ANewName: string); override;
    procedure SetCommand(const ACommand: TCommandString); override;
  public
    const
      /// <summary>
      ///   Registry key of disabled items.
      /// </summary>
      /// <remarks>
      ///   DEPRECATED since Windows 8!
      /// </remarks>
      DisabledKey = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupfolder\' deprecated;

      /// <summary>
      ///   Startup user item backup file extension.
      /// </summary>
      FileExtensionStartupUser = '.Startup';

      /// <summary>
      ///   Startup common item backup file extension.
      /// </summary>
      FileExtensionStartupCommon = '.CommonStartup';

      /// <summary>
      ///   Registry status key.
      /// </summary>
      /// <remarks>
      ///   Only since Windows 8!
      /// </remarks>
      StartupApprovedKey = TStartupListItem.StartupApprovedBaseKey +'StartupFolder';

    /// <summary>
    ///   Constructor for creating a <c>TStartupUserItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AStartupUser">
    ///   Item is located in autostart of current user or in common autostart.
    /// </param>
    /// <param name="ALnkFile">
    ///   The corresponding .lnk file.
    /// </param>
    constructor Create(const AName, ALocation: string; AEnabled, AStartupUser: Boolean;
      ALnkFile: TLnkFile);

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
    ///   Gets the file extension for backup files.
    /// </summary>
    /// <returns>
    ///   The backup extension.
    /// </returns>
    function GetBackupExtension(): string; override;

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
    ///   Gets or sets the .lnk file.
    /// </summary>
    property LnkFile: TLnkFile read FLnkFile write FLnkFile;

    /// <summary>
    ///   If <c>True</c> the item is located in the startup folder of the
    ///   current user. Otherwise the item is located in startup folder of all
    ///   users.
    /// </summary>
    property StartupUser: Boolean read FStartupUser;
  end;

  /// <summary>
  ///   The possible startup locations.
  /// </summary>
  TStartupLocation = (

    /// <summary>
    ///   HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Run
    /// </summary>
    slHkcuRun,

    /// <summary>
    ///   HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce
    /// </summary>
    slHkcuRunOnce,

    /// <summary>
    ///   HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Run
    /// </summary>
    slHklmRun,

    /// <summary>
    ///   HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run
    /// </summary>
    slHklmRun32,

    /// <summary>
    ///   HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce
    /// </summary>
    slHklmRunOnce,

    /// <summary>
    ///   HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce
    /// </summary>
    slHklmRunOnce32,

    /// <summary>
    ///   C:\Users\<Username>\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup
    /// </summary>
    slStartupUser,

    /// <summary>
    ///   C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp
    /// </summary>
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
  ///   locking. You can use the <see cref="IsExporting"/> method to see if the
  ///   list is being exported.
  /// </summary>
  TStartupList = class(TRootList<TStartupListItem>, IImportableList)
  private
    function LoadStatus(const AName: string;
      AStartupLocation: TStartupLocation): TStartupItemStatus;
  public
    /// <summary>
    ///   Adds a new item to the list.
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
    /// <param name="AStartupUser">
    ///   Item is located in autostart of current user or in common autostart.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the item was successfully added or <c>False</c> otherwise.
    /// </returns>
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EAlreadyExists</c> if item already exists.
    /// </exception>
    function Add(const AFileName, AArguments, ACaption: string;
      AStartupUser: Boolean = True): Boolean; overload;

    /// <summary>
    ///   Exports the complete list as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportList(const AFileName: string); override;

    /// <summary>
    ///   Gets the file filter for an <c>TOpenDialog</c> from a <c>TLanguageFile</c>.
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
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EAlreadyExists</c> if item already exists.
    /// </exception>
    function ImportBackup(const AFileName: TFileName): Boolean;

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
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   Use advanced search mode.
    /// </param>
    /// <param name="AWin64">
    ///   If set to <c>True</c> search for 64-bit items. Otherwise search for
    ///   32-bit items.
    /// </param>
    procedure Search(AExpertMode: Boolean = False; AWin64: Boolean = True); override;
  end;

const
  /// <summary>
  ///   Registry key that contains user custom file associations.
  /// </summary>
  KEY_USERCHOICE    = 'Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\%s\UserChoice';

  /// <summary>
  ///   Registry key that contains <see cref="TContextMenuShellCascadingItem"/>s.
  /// </summary>
  KEY_COMMAND_STORE = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\CommandStore\shell';

type
  /// <summary>
  ///   Raised by context menu feature classes.
  /// </summary>
  EContextMenuException = class(Exception);

  /// <summary>
  ///   A <c>TContextMenuListItem</c> represents a basic context menu item that can
  ///   be added to a <see cref="TContextMenuList"/>. All context menu items are
  ///   in the Registry.
  /// </summary>
  /// <remarks>
  ///   This class is intended to be only ancenstor for items.
  /// </remarks>
  TContextMenuListItem = class(TRegistryItem)
  protected
    FIcon: TMuiString;
    function GetIconFileName(): string; override;
    function GetRootKey(): TRootKey; override;
    procedure Rename(const ANewName: string); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TContextMenuListItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="ACommand">
    ///   The command.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AIcon">
    ///   The icon.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AWow64">
    ///   Use the WOW64 technology.
    /// </param>
    constructor Create(const AName, ACaption: string; const ACommand: TCommandString;
      const ALocation: string; const AIcon: TMuiString; AEnabled, AWow64: Boolean);

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Executes the item.
    /// </summary>
    /// <remarks>
    ///   Not possible for context menu items!
    /// </remarks>
    procedure Execute(); override;

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
  ///   A <c>TContextMenuShellItem</c> is a verb for a file type that can be
  ///   accessed through the shell context menu.
  /// </summary>
  TContextMenuShellItem = class(TContextMenuListItem)
  private
    FExtended: Boolean;
    procedure SetExtended(const AExtended: Boolean);
  protected
    function GetLocation(): string; override;
    procedure Rename(const AValueName, ANewCaption: string); reintroduce; overload;
    procedure Rename(const ANewName: string); overload; override;
    procedure SetCommand(const ACommand: TCommandString); override;
    procedure SetEnabled(const AEnabled: Boolean); override;
  public
    const
      /// <summary>
      ///   The registry sub key.
      /// </summary>
      CanonicalName    = 'Shell';

      /// <summary>
      ///   The registry value that exists when item is disabled.
      /// </summary>
      DisableValueName = 'LegacyDisable';

    /// <summary>
    ///   Constructor for creating a <c>TContextMenuShellItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACaption">
    ///   The display name.
    /// </param>
    /// <param name="ACommand">
    ///   The command.
    /// </param>
    /// <param name="ALocation">
    ///   The location where the item can be found.
    /// </param>
    /// <param name="AIcon">
    ///   The icon.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AExtended">
    ///   The item is hidden and only visible if "shift" is pressed.
    /// </param>
    constructor Create(const AName, ACaption: string; const ACommand: TCommandString;
      const ALocation: string; const AIcon: TMuiString; AEnabled, AExtended: Boolean);

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
    ///   Deletes the icon of a contextmenu.
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
    ///   The item is hidden and only visible if "shift" is pressed.
    /// </summary>
    property Extended: Boolean read FExtended write SetExtended;
  end;

  /// <summary>
  ///   A <c>TContextMenuShellCascadingItem</c> is a set of verbs for a file
  ///   type that can be accessed through the shell context menu.
  /// </summary>
  TContextMenuShellCascadingItem = class(TContextMenuShellItem)
  private
    procedure GetSubCommands(ASubCommands: TStrings);
  protected
    function IsErasable(): Boolean; override;
    procedure Rename(const ANewName: string); override;
    procedure SetCommand(const ACommand: TCommandString); override;
  public
    const
      /// <summary>
      ///   Name of registry value that contains comma-separated list of subcommands.
      /// </summary>
      SubCommandsValueName = 'SubCommands';

      /// <summary>
      ///   Name of registry value that contains caption of item.
      /// </summary>
      CaptionValueName     = 'MUIVerb';

    /// <summary>
    ///   Constructor for creating a <c>TContextMenuShellCascadingItem</c> instance.
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
    /// <param name="AIcon">
    ///   The icon.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    /// <param name="AExtended">
    ///   The item is hidden and only visible if "shift" is pressed.
    /// </param>
    constructor Create(const AName, ACaption, ALocation: string;
      const AIcon: TMuiString; AEnabled, AExtended: Boolean);

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
  ///   A <c>TContextMenuShellExItem</c> is a shell extension for a file type
  ///   that can be accessed through the shell context menu.
  /// </summary>
  TContextMenuShellExItem = class(TContextMenuListItem)
  protected
    function GetLocation(): string; override;
    procedure Rename(const ANewName: string); override;
    procedure SetCommand(const ACommand: TCommandString); override;
    procedure SetEnabled(const AEnabled: Boolean); override;
  public
    const
      /// <summary>
      ///   The registry sub key.
      /// </summary>
      CanonicalName = 'ShellEx';

      /// <summary>
      ///   The regitry sub key where shell extensions are located.
      /// </summary>
      HandlersKey   = CanonicalName +'\ContextMenuHandlers';

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
  ///   A <c>TContextMenuShellNewItem</c> is listed in the background context
  ///   menu under "new".
  /// </summary>
  TContextMenuShellNewItem = class(TContextMenuListItem)
  protected
    function GetLocation(): string; override;
    function IsErasable(): Boolean; override;
    procedure Rename(const ANewName: string); override;
    procedure SetCommand(const ACommand: TCommandString); override;
    procedure SetEnabled(const AEnabled: Boolean); override;
  public
    const
      /// <summary>
      ///   The enabled registry key.
      /// </summary>
      CanonicalName         = 'ShellNew';

      /// <summary>
      ///   The disabled registry key.
      /// </summary>
      CanonicalNameDisabled = '_'+ CanonicalName;

    /// <summary>
    ///   Constructor for creating a <c>TContextMenuShellNewItem</c> instance.
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
    /// <param name="AIcon">
    ///   The icon.
    /// </param>
    /// <param name="AEnabled">
    ///   The status.
    /// </param>
    constructor Create(const AName, ACaption, ALocation: string; const AIcon: TMuiString;
      AEnabled: Boolean);

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
  ///   Contextmenu item registry types.
  /// </summary>
  TContextMenuItemType = (

    /// <summary>
    ///   Contextmenu item under "Shell" registry key.
    /// </summary>
    itShell,

    /// <summary>
    ///   Contextmenu item under "ShellEx" registry key.
    /// </summary>
    itShellEx,

    /// <summary>
    ///   Contextmenu item under "ShellNew" registry key.
    /// </summary>
    itShellNew
  );

  /// <summary>
  ///   A <c>TContextMenuList</c> is the list that contains a set of
  ///   <see cref="TContextMenuListItem"/>s. The list is thread-safe and supports
  ///   locking. You can use the <see cref="IsExporting"/> method to see if the
  ///   list is being exported.
  /// </summary>
  TContextMenuList = class(TRootList<TContextMenuListItem>)
  private
    procedure Search(AExpertMode: Boolean; AWin64: Boolean;
      const ARoot: string = ''); reintroduce; overload;
  public
    const
      /// <summary>
      ///   A comma separated list that contains the default context menu locations.
      /// </summary>
      DefaultLocations = '*, AllFileSystemObjects, Folder, Directory, Directory\Background, Drive, Network, Printers';

    /// <summary>
    ///   Constructor for creating a <c>TContextMenuList</c> instance.
    /// </summary>
    constructor Create;

    /// <summary>
    ///   Adds a new item to the list.
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
    /// <returns>
    ///   <c>True</c> if the item was successfully added or <c>False</c> otherwise.
    /// </returns>
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EAlreadyExists</c> if item already exists.
    /// </exception>
    function Add(const AFileName, AArguments, ALocationRoot, ACaption: string): Boolean; overload;

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
    /// <param name="AContextMenuItemType">
    ///   Specifies the item type to search for.
    /// </param>
    /// <param name="AWow64">
    ///   If set to <c>True</c> search for WOW64 items. Otherwise search for
    ///   native items.
    /// </param>
    procedure LoadContextmenu(const ALocationRoot: string;
      AContextMenuItemType: TContextMenuItemType; AWow64: Boolean); overload;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   Use advanced search mode.
    /// </param>
    /// <param name="AWin64">
    ///   If set to <c>True</c> search for 64-bit items. Otherwise search for
    ///   32-bit items.
    /// </param>
    procedure Search(AExpertMode: Boolean = False; AWin64: Boolean = True); overload; override;
  end;

const
  /// <summary>
  ///   Registry key of disabled <see cref="TServiceListItem"/>s.
  /// </summary>
  KEY_SERVICE_DISABLED = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\services';

  /// <summary>
  ///   Registry key of enabled <see cref="TServiceListItem"/>s.
  /// </summary>
  KEY_SERVICE_ENABLED  = 'SYSTEM\CurrentControlSet\services\';

type
  /// <summary>
  ///   Raised by service feature classes.
  /// </summary>
  EServiceException = class(Exception);

  /// <summary>
  ///   The possible service startup type.
  /// </summary>
  TServiceStart = (
    ssBoot      = SERVICE_BOOT_START,
    ssSystem    = SERVICE_SYSTEM_START,
    ssAutomatic = SERVICE_AUTO_START,
    ssManual    = SERVICE_DEMAND_START,
    ssDisabled  = SERVICE_DISABLED
  );

  /// <summary>
  ///   A <c>TServiceListItem</c> represents a service item that can be added
  ///   to a <see cref="TServiceList"/>.
  /// </summary>
  TServiceListItem = class(TRegistryItem)
  private
    FServiceManager: SC_HANDLE;
    FDeactivationTime: TDateTime;
    FServiceStart: TServiceStart;
    function GetHandle(AAccess: DWORD): SC_HANDLE;
  protected
    function GetRootKey(): TRootKey; override;
    procedure Rename(const ANewName: string); override;
    procedure SetCommand(const ACommand: TCommandString); override;
    procedure SetEnabled(const AEnabled: Boolean); override;
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
    /// <param name="ACommand">
    ///   The command.
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
    constructor Create(const AName, ACaption: string; const ACommand: TCommandString;
      AEnabled: Boolean; AServiceStart: TServiceStart; AServiceManager: SC_HANDLE);

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
    property DeactivationTime: TDateTime read FDeactivationTime write FDeactivationTime;
  end;

  /// <summary>
  ///   A <c>TServiceList</c> is the list that contains a set of
  ///   <see cref="TServiceListItem"/>s. The list is thread-safe and supports
  ///   locking. You can use the <see cref="IsExporting"/> method to see if the
  ///   list is being exported.
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
    ///   Adds a new item to the list.
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
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    ///   <c>EAlreadyExists</c> if item already exists.
    /// </exception>
    function Add(const AFileName, AArguments, ACaption: string): Boolean; overload;

    /// <summary>
    ///   Exports the complete list as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportList(const AFileName: string); override;

    /// <summary>
    ///   Loads and adds a service item to the list.
    /// </summary>
    /// <param name="AName">
    ///   The name of the service.
    /// </param>
    /// <param name="AIncludeDemand">
    ///   If set to <c>True</c> services that are started on demand are included.
    /// </param>
    /// <returns>
    ///   The index on which the item was added.
    /// </returns>
    function LoadService(const AName: string; AIncludeDemand: Boolean = False): Integer;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   Use advanced search mode.
    /// </param>
    /// <param name="AWin64">
    ///   If set to <c>True</c> search for 64-bit items. Otherwise search for
    ///   32-bit items.
    /// </param>
    procedure Search(AExpertMode: Boolean = False; AWin64: Boolean = True); override;

    /// <summary>
    ///   Gets the current handle to the service manager.
    /// </summary>
    property Manager: SC_HANDLE read FManager;
  end;

  /// <summary>
  ///   Raised by scheduled tasks feature classes.
  /// </summary>
  ETaskException = class(EOleError);

  /// <summary>
  ///   A <c>TTaskListItem</c> represents a scheduled task item that can be
  ///   added to a <see cref="TTaskList"/>.
  /// </summary>
  TTaskListItem = class(TRootItem)
  private
    FTask: IRegisteredTask;
    FTaskService: ITaskService;
    FInvalid: Boolean;
    function GetTaskDefinition(): ITaskDefinition;
    function GetZipLocation(): string;
  protected
    function GetFullLocation(): string; override;
    function IsErasable(): Boolean; override;
    procedure Rename(const ANewName: string); override;
    procedure SetCommand(const ACommand: TCommandString); override;
    procedure SetEnabled(const AEnabled: Boolean); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TTaskListItem</c> instance.
    /// </summary>
    /// <param name="AName">
    ///   The internal name.
    /// </param>
    /// <param name="ACommand">
    ///   The command.
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
    /// <param name="ATaskService">
    ///   A <c>ITaskService</c> object.
    /// </param>
    /// <param name="AInvalid">
    ///   Optional: Set to <c>True</c> if task could not be read.
    /// </param>
    constructor Create(const AName: string; const ACommand: TCommandString;
      const ALocation: string; AEnabled: Boolean; const ATask: IRegisteredTask;
      const ATaskService: ITaskService; AInvalid: Boolean = False);

    /// <summary>
    ///   Deletes the item.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the item was successfully deleted or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Delete(): Boolean; override;

    /// <summary>
    ///   Executes the item.
    /// </summary>
    procedure Execute(); override;

    /// <summary>
    ///   Exports the item as file.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the file.
    /// </param>
    procedure ExportItem(const AFileName: string); override;

    /// <summary>
    ///   Gets the file extension for backup files.
    /// </summary>
    /// <returns>
    ///   The backup extension.
    /// </returns>
    function GetBackupExtension(): string; override;

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

    /// <summary>
    ///   Gets the internal ZIP archive location.
    /// </summary>
    property ZipLocation: string read GetZipLocation;
  end;

  /// <summary>
  ///   A <c>TTaskList</c> is the list that contains a set of
  ///   <see cref="TTaskListItem"/>s. The list is thread-safe and supports
  ///   locking. You can use the <see cref="IsExporting"/> method to see if the
  ///   list is being exported.
  /// </summary>
  TTaskList = class(TRootList<TTaskListItem>, IImportableList)
  private
    FTaskService: ITaskService;
    function AddTaskItem(const ATask: IRegisteredTask): Integer;
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
    ///   Gets the file extension for backup files.
    /// </summary>
    /// <returns>
    ///   The backup extension.
    /// </returns>
    function GetBackupExtension(): string; override;

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
    ///   Gets the file filter for an <c>TOpenDialog</c> from a <c>TLanguageFile</c>.
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
    /// <exception>
    ///   <c>EListBlocked</c> if another operation is pending on the list.
    /// </exception>
    function ImportBackup(const AFileName: TFileName): Boolean;

    /// <summary>
    ///   Searches for items and adds them to the list.
    /// </summary>
    /// <param name="AExpertMode">
    ///   Use advanced search mode.
    /// </param>
    /// <param name="AWin64">
    ///   If set to <c>True</c> search for 64-bit items. Otherwise search for
    ///   32-bit items.
    /// </param>
    procedure Search(AExpertMode: Boolean = False; AWin64: Boolean = True); override;

    /// <summary>
    ///   Gets the current task service.
    /// </summary>
    property TaskService: ITaskService read FTaskService;
  end;

resourcestring
  SOperationPending = 'Another operation is pending. Please wait!';
  SNoItemSelected   = 'No item selected!';

implementation

{$I LanguageIDs.inc}

/// <summary>
///   Checks if specified file exists without WOW64 redirection.
/// </summary>
/// <returns>
///   <c>True</c> if the file exists or <c>False</c> otherwise.
/// </returns>
function FileExistsWow64(const AFileName: TFileName): Boolean;
var
  OldValue: Boolean;

begin
  if (AFileName = '') then
    Exit(False);

  OldValue := DisableWow64FsRedirection();
  Result := System.SysUtils.FileExists(AFileName);
  RevertWow64FsRedirection(OldValue);
end;


{ TCommandStringHelper }

class function TCommandStringHelper.Create(const AFileName: string;
  const AArguments: string = ''; AAddQuotes: Boolean = False): TCommandString;
begin
  if (AArguments <> '') then
  begin
    // File names contains white space?
    if (AAddQuotes and not AFileName.StartsWith('"') and AFileName.Contains(' ')) then
      Result := AFileName.QuotedString('"') +' '+ AArguments.DeQuotedString('"')
    else
      Result := AFileName +' '+ AArguments;
  end  //of begin
  else
    Result := AFileName;
end;

procedure TCommandStringHelper.Execute();
begin
  ShellExecute(0, 'open', PChar(ExtractFileName), PChar(ExtractArguments), nil, SW_SHOWNORMAL);
end;

function TCommandStringHelper.Expand(): TCommandString;
const
  RUNDLL32 = 'rundll32.exe';

var
  Path: string;
  Index: Integer;

begin
  if (Self = '') then
    Exit;

  Index := string(Self).ToLower.IndexOf(RUNDLL32);

  if (Index <> -1) then
  begin
    Index := Index + Length(RUNDLL32) + 1;
    Path := string(Self).Substring(Index);
    Path := Path.Substring(0, Path.IndexOf(',')).TrimLeft;
  end  //of begin
  else
    Path := Self.ExtractFileName();

  Path := Path.DeQuotedString('"');

  // Path has to be expanded?
  if Path.StartsWith('%') then
    ExpandEnvironmentVar(Path);

  // File in system search path?
  if ((ExtractFileExt(Path) <> '') and IsRelativePath(Path)) then
    Path := FileSearch(Path, GetEnvironmentVariable('Path'));

  Result := Path;
end;

function TCommandStringHelper.ExtractArguments(): string;
var
  ExtWithArguments: string;
  Ext, SpaceDelimiter: Integer;

begin
  if (Self = '') then
    Exit;

  // Cut path from extension until end
  // Note: Garbage in worst case if a folder name contains a '.'!
  Ext := string(Self).IndexOf('.');

  if (Ext >= 0) then
    ExtWithArguments := string(Self).SubString(Ext)
  else
    ExtWithArguments := ExtractFileExt(Self);

  // Find space delimter between extension and arguments
  SpaceDelimiter := ExtWithArguments.IndexOf(' ');

  // No space char after extension: no arguments!
  if (SpaceDelimiter = -1) then
    Exit;

  // Copy arguments without entension and space char at front and end
  Result := ExtWithArguments.Substring(SpaceDelimiter).Trim;
end;

function TCommandStringHelper.ExtractFileName(): string;
var
  Parts: TArray<string>;
  i: Integer;
  Line, ExtWithArguments: string;

begin
  if (Self = '') then
    Exit;

  if string(Self).StartsWith('"') then
  begin
    Parts := string(Self).Split(['"']);

    for i := Low(Parts) to High(Parts) do
    begin
      Line := Parts[i].Trim;

      if (Line <> '') then
      begin
        Result := Line;
        Break;
      end;  //of begin
    end;  //of for
  end  //of begin
  else
  begin
    if (string(Self).CountChar('.') = 1) then
      i := string(Self).LastDelimiter(PathDelim +'.')
    else
      i := string(Self).LastDelimiter(PathDelim);

    ExtWithArguments := string(Self).SubString(i + 1);
    Parts := ExtWithArguments.Split([' ']);
    Result := string(Self).Substring(0, string(Self).IndexOf(ExtWithArguments) + Length(Parts[0]));
  end;  //of if
end;


{ TLnkFile }

constructor TLnkFile.Create(const AFileName: TFileName);
begin
  inherited Create;
  FFileName := AFileName;

  if Exists() then
    Read();
end;

function TLnkFile.GetCommand(): TCommandString;
begin
  Result := TCommandString.Create(FExeFileName, FArguments);
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

function TLnkFile.Save(const AFileName, AExeFileName: TFileName;
  const AArguments: string = ''): Boolean;
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


{ TRootItem }

constructor TRootItem.Create(const AName, ACaption: string; const ACommand: TCommandString;
  const ALocation: string; AEnabled: Boolean);
begin
  inherited Create;
  FName := AName;
  FCaption := ACaption;
  FCommand := ACommand;
  FLocation := ALocation;
  FEnabled := AEnabled;
  FImageIndex := -1;
  UpdateErasable();
end;

function TRootItem.GetFileDescription(const AFileName: TFileName): string;
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

procedure TRootItem.Execute();
begin
  FCommand.Execute();
end;

procedure TRootItem.LoadIcon(AIcon: TIcon);
var
  IconFileName: string;
  MuiString: TMuiString;
  OldValue: Boolean;

begin
  IconFileName := GetIconFileName();

  // No icon?
  if (IconFileName = '') then
  begin
    AIcon.Handle := 0;
    Exit;
  end;  //of begin

  // MUI string?
  if IconFileName.Contains(',') then
  begin
    MuiString := IconFileName;
    IconFileName := MuiString.Path;
  end  //of begin
  else
    IconFileName := IconFileName.DeQuotedString('"');

  OldValue := DisableWow64FsRedirection();

  try
    if (ExtractFileExt(IconFileName) = '.dll') then
      AIcon.Handle := ExtractIcon(HInstance, PChar(IconFileName), MuiString.Id)
    else
      if (ExtractFileExt(IconFileName) = '.ico') then
        AIcon.LoadFromFile(IconFileName)
      else
        AIcon.Handle := GetIcon(IconFileName)

  finally
    RevertWow64FsRedirection(OldValue);
  end;  //of try
end;

function TRootItem.GetIcon(const AExeFileName: TFileName): HICON;
var
  FileInfo: TSHFileInfo;

begin
  Result := 0;

  if ((AExeFileName = '') or (ExtractFileExt(AExeFileName) = '')) then
    Exit;

  if Succeeded(SHGetFileInfo(PChar(AExeFileName), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_ICON or SHGFI_SMALLICON)) then
    Result := FileInfo.hIcon;
end;

function TRootItem.GetIconFileName(): string;
begin
  Result := FCommand.Expand();
end;

function TRootItem.GetLocation(): string;
begin
  Result := FLocation;
end;

function TRootItem.FileExists(): Boolean;
begin
  Result := FileExistsWow64(FCommand.Expand());
end;

function TRootItem.IsErasable(): Boolean;
var
  ExpandedCommand: string;

begin
  if (FCommand = '') then
    Exit(True);

  ExpandedCommand := FCommand.Expand();

  // Something went wrong: Item is not erasable!
  if ((ExpandedCommand = '') or (ExtractFileExt(ExpandedCommand) = '')) then
    Exit(False);

  Result := not FileExistsWow64(ExpandedCommand);
end;

procedure TRootItem.OpenInExplorer();
var
  PreparedFileName: string;
  ItemIDList: PItemIDList;

begin
  // Extract the file path only (without arguments and quote chars)
  PreparedFileName := FCommand.Expand();

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

procedure TRootItem.SetCommand(const ACommand: TCommandString);
begin
  FCommand := ACommand;
  UpdateErasable();
end;

procedure TRootItem.SetEnabled(const AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

procedure TRootItem.UpdateErasable();
begin
  FErasable := IsErasable();
end;


{ TRegistryItem }

constructor TRegistryItem.Create(const AName, ACaption: string;
  const ACommand: TCommandString; const ALocation: string; AEnabled, AWow64: Boolean);
begin
  inherited Create(AName, ACaption, ACommand, ALocation, AEnabled);
  FWow64 := AWow64;
end;

function TRegistryItem.DeleteKey(AHKey: HKEY; const AKeyPath, AKeyName: string;
  AFailIfNotExists: Boolean = True): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := AHKey;

    if not Reg.OpenKey(AKeyPath, False) then
      raise Exception.CreateFmt('Could not open key "%s": %s', [AKeyPath, Reg.LastErrorMsg]);

    if Reg.KeyExists(AKeyName) then
      Result := Reg.DeleteKey(AKeyName)
    else
      if AFailIfNotExists then
        raise EWarning.CreateFmt('Key "%s" already deleted!', [AKeyPath])
      else
        Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TRegistryItem.OpenInRegEdit(AWow64: Boolean);
const
  REGISTRY_EDITOR = 'regedit.exe';

var
  Reg: TRegistry;
  OldValue: Boolean;

begin
  Reg := TRegistry.Create(KEY_WRITE);

  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Applets\Regedit', True);
    Reg.WriteString('LastKey', 'Computer\'+ GetFullLocation());

    // Redirected 32 bit item?
    if AWow64 then
      // Execute 32 bit RegEdit
      ShellExecute(0, 'open', REGISTRY_EDITOR, nil, {$IFDEF WIN64}PChar(GetSystemWow64Directory()){$ELSE}nil{$ENDIF}, SW_SHOWNORMAL)
    else
    begin
      // Execute 64 bit RegEdit
      OldValue := DisableWow64FsRedirection();
      ShellExecute(0, 'open', REGISTRY_EDITOR, nil, nil, SW_SHOWNORMAL);
      RevertWow64FsRedirection(OldValue);
    end;  //of if

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TRegistryItem.WriteTimestamp(const AReg: TRegistry): TDateTime;
var
  Timestamp: TSystemTime;

begin
  Assert(Assigned(AReg), 'Registry instance was not initialized!');
  Result := Now();
  DateTimeToSystemTime(Result, Timestamp);

  try
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

function TRegistryItem.GetBackupExtension(): string;
begin
  Result := '.reg';
end;

function TRegistryItem.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := ALanguageFile.GetString(LID_FILTER_REGISTRY_FILE);
end;

function TRegistryItem.GetFullLocation(): string;
begin
  Result := GetRootKey().ToString() +'\'+ GetLocation();
end;

function TRegistryItem.GetTimestamp(const AReg: TRegistry): TDateTime;
var
  Timestamp: TSystemTime;

begin
  Assert(Assigned(AReg), 'Registry instance was not initialized!');
  Result := 0;

  // Deactivation timestamp only available for disabled items
  if FEnabled then
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
  FErasableItemsCount := 0;
  FAllowDuplicates := False;
  FSearchLock := TCriticalSection.Create;
  FExportLock := TCriticalSection.Create;
  FIcon := TIcon.Create;
end;

destructor TRootList<T>.Destroy;
begin
  FreeAndNil(FIcon);
  FreeAndNil(FExportLock);
  FreeAndNil(FSearchLock);
  inherited Destroy;
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
  // No ref-counting!
  Result := -1;
end;

function TRootList<T>._Release(): Integer;
begin
  // No ref-counting!
  Result := -1;
end;

procedure TRootList<T>.ChangeCommand(AItem: T; const ACommand: TCommandString);
var
  ItemErasable: Boolean;

begin
  // Export is pending?
  if not FExportLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    // Invalid item?
    if (not Assigned(AItem) or (IndexOf(AItem) = -1)) then
      raise EInvalidItem.Create(SNoItemSelected);

    // Change item file path
    ItemErasable := AItem.Erasable;
    AItem.Command := ACommand;

    // Update icon
    if Assigned(FImages) then
    begin
      AItem.LoadIcon(FIcon);

      // NOTE: Do not remove old icon as indices will change
      // NOTE: Inserting icon at same index in image list will not update icon on UI
      AItem.ImageIndex := FImages.AddIcon(FIcon);
    end;  //of begin

    // Update erasable count
    if (ItemErasable and not AItem.Erasable) then
      Dec(FErasableItemsCount)
    else
      if (not ItemErasable and AItem.Erasable) then
        Inc(FErasableItemsCount);

  finally
    FExportLock.Release();
  end;  //of try
end;

procedure TRootList<T>.ChangeItemStatus(AItem: T; const AEnabled: Boolean);
begin
  // Export is pending?
  if not FExportLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    if (not Assigned(AItem) or (IndexOf(AItem) = -1)) then
      raise EInvalidItem.Create(SNoItemSelected);

    if AEnabled then
    begin
      if AItem.Enabled then
        raise EWarning.Create('Item already enabled!');

      // Enable item
      AItem.Enabled := AEnabled;
      Inc(FEnabledItemsCount);
      NotifyOnCounterUpdate();
    end  //of begin
    else
    begin
      if not AItem.Enabled then
        raise EWarning.Create('Item already disabled!');

      // Disable item
      AItem.Enabled := AEnabled;

      // Update active counter
      if (FEnabledItemsCount > 0) then
      begin
        Dec(FEnabledItemsCount);
        NotifyOnCounterUpdate();
      end;  //of begin
    end;  //of if

  finally
    FExportLock.Release();
  end;  //of try
end;

procedure TRootList<T>.Clear();
begin
  inherited Clear();

  if Assigned(FImages) then
    FImages.Clear();
end;

function TRootList<T>.DeleteItem(AItem: T): Boolean;
var
  Deleted: Boolean;

begin
  // Search is pending?
  if not FSearchLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    // Export is pending?
    if not FExportLock.TryEnter() then
      raise EListBlocked.Create(SOperationPending);

    try
      if (not Assigned(AItem) or (IndexOf(AItem) = -1)) then
        raise EInvalidItem.Create(SNoItemSelected);

      // Delete item
      Deleted := AItem.Delete();

      // Successful?
      if Deleted then
        // Remove item from list
        Remove(AItem);

    finally
      FExportLock.Release();
      Result := Deleted;
    end;  //of try

  finally
    FSearchLock.Release();
  end;  //of try
end;

procedure TRootList<T>.DisableItem(AItem: T);
begin
  ChangeItemStatus(AItem, False);
end;

procedure TRootList<T>.EnableItem(AItem: T);
begin
  ChangeItemStatus(AItem, True);
end;

procedure TRootList<T>.ExportItem(AItem: T; const AFileName: string);
begin
  if (not Assigned(AItem) or (IndexOf(AItem) = -1)) then
    raise EInvalidItem.Create(SNoItemSelected);

  AItem.ExportItem(AFileName);
end;

function TRootList<T>.GetBackupExtension(): string;
begin
  Result := '.reg';
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

function TRootList<T>.IsExporting(): Boolean;
var
  Entered: Boolean;

begin
  Entered := FExportLock.TryEnter();

  if Entered then
    FExportLock.Release();

  Result := not Entered;
end;

procedure TRootList<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  // Icon handling
  if (Assigned(FImages) and (Action = cnAdded)) then
  begin
    Item.LoadIcon(FIcon);
    Item.ImageIndex := FImages.AddIcon(FIcon);
  end;  //of begin

  // Must be executed in main thread!
  TThread.Synchronize(nil,
    procedure
    begin
      inherited Notify(Item, Action);

      case Action of
        cnAdded:
          begin
            if Item.Enabled then
              Inc(FEnabledItemsCount);

            if Item.Erasable then
              Inc(FErasableItemsCount);

            NotifyOnCounterUpdate();
          end;

        cnRemoved:
          begin
            // Item was enabled
            if (Item.Enabled and (FEnabledItemsCount > 0)) then
              // Update active counter
              Dec(FEnabledItemsCount);

            // Update erasable count
            if (Item.Erasable and (FErasableItemsCount > 0)) then
              Dec(FErasableItemsCount);

            NotifyOnCounterUpdate();
          end;
      end;  //of case
  end);
end;

procedure TRootList<T>.NotifyOnCounterUpdate();
begin
  if Assigned(FOnCounterUpdate) then
    FOnCounterUpdate(Self);
end;

procedure TRootList<T>.RenameItem(AItem: T; const ANewName: string);
var
  i: Integer;

begin
  // Export is pending?
  if not FExportLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    if (not Assigned(AItem) or (IndexOf(AItem) = -1)) then
      raise EInvalidItem.Create(SNoItemSelected);

    if not FAllowDuplicates then
    begin
      // Check for item with the same name
      for i := 0 to Count - 1 do
        if (Items[i].Name = ANewName) then
          raise EWarning.Create('Item named "'+ ANewName +'" already exists!');
    end;  //of begin

    AItem.Name := ANewName;

  finally
    FExportLock.Release();
  end;  //of try
end;


{ TRootListThread }

constructor TRootListThread.Create(ASelectedList: TRootList<TRootItem>);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSelectedList := ASelectedList;
end;

procedure TRootListThread.DoNotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FErrorMessage);
end;

procedure TRootListThread.DoNotifyOnListLocked();
begin
  if Assigned(FOnListLocked) then
    FOnListLocked(Self);
end;

procedure TRootListThread.DoNotifyOnStart();
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TRootListThread.Execute();
var
  SearchLock: TCriticalSection;

begin
  try
    Assert(Assigned(FSelectedList));
    SearchLock := FSelectedList.FSearchLock;

    // Search is pending?
    if not SearchLock.TryEnter() then
      raise EListBlocked.Create(SOperationPending);

    CoInitialize(nil);

    try
      Synchronize(DoNotifyOnStart);
      DoExecute();

    finally
      CoUninitialize();
      SearchLock.Release();
    end;  //of try

  except
    on E: EListBlocked do
      Synchronize(DoNotifyOnListLocked);

    on E: Exception do
    begin
      FErrorMessage := Format('%s: %s', [ClassName, E.Message]);
      Synchronize(DoNotifyOnError);
    end;
  end;  //of try
end;


{ TSearchThread }

constructor TSearchThread.Create(ASelectedList: TRootList<TRootItem>);
begin
  inherited Create(ASelectedList);
  FreeOnTerminate := True;
  FExpertMode := False;
  FWin64 := (TOSVersion.Architecture = arIntelX64);
end;

procedure TSearchThread.DoExecute();
begin
  FSelectedList.Clear();
  FSelectedList.Search(FExpertMode, FWin64);
end;


{ TExportListThread }

constructor TExportListThread.Create(ASelectedList: TRootList<TRootItem>;
  const AFileName: string; APageControlIndex: Integer);
begin
  inherited Create(ASelectedList);
  FreeOnTerminate := True;
  FFileName := AFileName;
  FPageControlIndex := APageControlIndex;
end;

procedure TExportListThread.DoExecute();
var
  ExportLock: TCriticalSection;

begin
  ExportLock := FSelectedList.FExportLock;

  // Export is pending?
  if not ExportLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    FSelectedList.ExportList(FFileName);

  finally
    ExportLock.Release();
  end;  //of try
end;


{ TStartupItemStatus }

function TStartupItemStatus.GetDeactivationTime(): TDateTime;
var
  SystemTime: TSystemTime;
  LocalFileTime: TFileTime;

begin
  // Disabled and time valid?
  if (not GetEnabled() and (DeactivationTime.dwLowDateTime <> 0) and
    (DeactivationTime.dwHighDateTime <> 0)) then
  begin
    FileTimeToLocalFileTime(DeactivationTime, LocalFileTime);
    FileTimeToSystemTime(LocalFileTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);
  end  //of begin
  else
    Result := 0;
end;

function TStartupItemStatus.GetEnabled(): Boolean;
begin
  Result := (Status <> ST_DISABLED);
end;

procedure TStartupItemStatus.SetEnabled(AEnabled: Boolean);
var
  SystemTime: TSystemTime;
  LocalFileTime: TFileTime;

begin
  ZeroMemory(@DeactivationTime, SizeOf(TFileTime));

  if not AEnabled then
  begin
    Status := ST_DISABLED;
    DateTimeToSystemTime(Now(), SystemTime);
    SystemTimeToFileTime(SystemTime, LocalFileTime);
    LocalFileTimeToFileTime(LocalFileTime, DeactivationTime);
  end  //of begin
  else
    Status := ST_ENABLED
end;


{ TStartupListItem }

constructor TStartupListItem.Create(const AName: string; const ACommand: TCommandString;
  const ALocation: string; ARootKey: TRootKey; AEnabled, AWow64: Boolean);
begin
  inherited Create(AName, AName, ACommand, ALocation, AEnabled, AWow64);
  FRootKey := ARootKey;
  FCaption := GetFileDescription(FCommand.Expand());
  FDeactivationTime := 0;
end;

function TStartupListItem.GetWow64Key(): string;
begin
  if ((FEnabled or CheckWin32Version(6, 2)) and Wow64) then
    Result := TStartupItem.StartupRunKey32
  else
    Result := FLocation;
end;

procedure TStartupListItem.SetEnabled(const AEnabled: Boolean);
var
  Reg: TRegistry;
  ItemStatus: TStartupItemStatus;

begin
  if not CheckWin32Version(6, 2) then
  begin
    if (FEnabled and not AEnabled) then
    begin
      if not Disable() then
        raise Exception.Create('Unknown error!');
    end  //of begin
    else
      if (not FEnabled and AEnabled) then
      begin
        if not Enable() then
          raise Exception.Create('Unknown error!');
      end;  //of if

    inherited SetEnabled(AEnabled);
  end  //of begin
  else
  begin
    // Status is stored in 64 bit registry
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

    try
      Reg.RootKey := FRootKey.ToHKey();
      Reg.OpenKey(GetApprovedLocation(), True);
      ItemStatus.SetEnabled(AEnabled);
      Reg.WriteBinaryData(Name, ItemStatus, SizeOf(TStartupItemStatus));
      FDeactivationTime := ItemStatus.GetDeactivationTime();
      inherited SetEnabled(AEnabled);

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try
  end;  //of if
end;

function TStartupListItem.DeleteValue(const AKeyPath: string;
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
      raise ERegistryException.CreateFmt('Could not open key "%s\%s": %s', [Reg.RootKeyName, AKeyPath, Reg.LastErrorMsg]);

    // Try to delete value
    if (Reg.ValueExists(Name) and not Reg.DeleteValue(Name)) then
      raise ERegistryException.CreateFmt('Could not delete value "%s": %s', [Name, Reg.LastErrorMsg]);

    // Deleted or does not exist
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
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
      raise EStartupException.Create(Reg.LastErrorMsg);

    if not Reg.ValueExists(Name) then
      SetEnabled(True);

    Reg.RenameValue(Name, ANewName);

    if not Reg.ValueExists(ANewName) then
      raise EStartupException.Create('Value was not renamed!');

    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TStartupListItem.SetCommand(const ACommand: TCommandString);
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
      raise EStartupException.Create(Reg.LastErrorMsg);

    if (FEnabled or CheckWin32Version(6, 2)) then
      ItemName := Name
    else
      ItemName := 'command';

    // Value must exist!
    if not Reg.ValueExists(ItemName) then
      raise EStartupException.CreateFmt('Value "%s" does not exist!', [ItemName]);

    // Change path
    Reg.WriteString(ItemName, ACommand);
    inherited SetCommand(ACommand);

    // Update caption
    FCaption := GetFileDescription(FCommand.Expand());

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
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);

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
        on E: ERegistryFileException do
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
    // Disabled items are stored in 64 bit Registry!
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

constructor TStartupItem.Create(const AName: string; const ACommand: TCommandString;
  const ALocation: string; ARootKey: TRootKey; AEnabled, AWow64, ARunOnce: Boolean);
begin
  inherited Create(AName, ACommand, ALocation, ARootKey, AEnabled, AWow64);
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
    Result := DeleteKey(HKEY_LOCAL_MACHINE, DisabledKey, Name);
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
    if not Reg.OpenKey(DisabledKey + Name, True) then
      raise EStartupException.CreateFmt('Could not create key "%s": %s', [DisabledKey + Name, Reg.LastErrorMsg]);

    // Write values
    RootKey.FromHKey(RootKey.ToHKey());
    Reg.WriteString('hkey', RootKey.ToString(False));
    Reg.WriteString('key', GetWow64Key());
    Reg.WriteString('item', Name);
    Reg.WriteString('command', Command);
    Reg.WriteString('inimapping', '0');

    // Windows >= Vista?
    if CheckWin32Version(6) then
      // Save deactivation timestamp
      FDeactivationTime := WriteTimestamp(Reg);

    // Open startup location
    Reg.CloseKey();

    // Redirect to 32 Bit key?
    if Wow64 then
      Reg.Access := KEY_WOW64_32KEY or KEY_READ or KEY_WRITE;

    Reg.RootKey := FRootKey.ToHKey();
    Reg.OpenKey(FLocation, False);

    // Delete old value, but do not fail if old value does not exist!
    if (Reg.ValueExists(Name) and not Reg.DeleteValue(Name)) then
      raise EStartupException.CreateFmt('Could not delete value "%s" in key "%s": %s', [Name, FLocation, Reg.LastErrorMsg]);

    // Update information
    FRootKey := rkHKLM;
    FLocation := DisabledKey + Name;
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
      raise EStartupException.Create(Reg.LastErrorMsg);

    if (not Reg.ValueExists('hkey') or not Reg.ValueExists('key')) then
      raise EStartupException.Create('Missing destination Registry values ''hkey'' or ''key''!');

    // Set new values
    NewHKey.FromString(Reg.ReadString('hkey'));
    NewKeyPath := Reg.ReadString('key');

    if ((NewKeyPath = '') or (NewHKey = rkUnknown)) then
      raise EStartupException.Create('Invalid destination Registry values for ''hkey'' or ''key''!');

    Reg.CloseKey;

    // Redirect to 32 Bit key?
    if Wow64 then
    begin
      // RunOnce item?
      if (ExtractFileName(NewKeyPath) = 'RunOnce') then
        NewKeyPath := StartupRunOnceKey
      else
        NewKeyPath := StartupRunKey;

      Reg.Access := KEY_WOW64_32KEY or KEY_READ or KEY_WRITE;
    end  //of begin
    else
      Reg.Access := Access64 or KEY_WRITE;

    Reg.RootKey := NewHKey.ToHKey();

    // Failed to create new key?
    if not Reg.OpenKey(NewKeyPath, True) then
      raise EStartupException.CreateFmt('Could not create key "%s": %s', [NewKeyPath, Reg.LastErrorMsg]);

    // Write startup entry
    Reg.WriteString(Name, Command);

    // Delete old key
    Reg.CloseKey();
    Reg.Access := Access64 or KEY_WRITE;
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey(DisabledKey, True) then
      raise EStartupException.CreateFmt('Could not open key "%s": %s', [DisabledKey, Reg.LastErrorMsg]);

    // Do not abort if old key does not exist!
    if (Reg.KeyExists(Name) and not Reg.DeleteKey(Name)) then
      raise EStartupException.CreateFmt('Could not delete old key "%s": %s', [Name, Reg.LastErrorMsg]);

    // Update information
    FRootKey := NewHKey;
    FLocation := NewKeyPath;
    FEnabled := True;
    FDeactivationTime := 0;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TStartupItem.GetApprovedLocation(): string;
begin
  // Only since Windows 8!
  if not CheckWin32Version(6, 2) then
    Exit;

  if Wow64 then
    Result := StartupApproved32Key
  else
    Result := StartupApprovedKey;
end;

function TStartupItem.GetWow64Key(): string;
begin
  if ((FEnabled or CheckWin32Version(6, 2)) and Wow64) then
  begin
    if FRunOnce then
      Result := StartupRunOnceKey32
    else
      Result := StartupRunKey32;
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
      raise EStartupException.Create(Reg.LastErrorMsg);

    if (FRootKey <> rkHKLM) then
      raise EStartupException.Create('Wrong HKEY!');

    Reg.WriteString('item', ANewName);
    Reg.CloseKey;

    if not Reg.OpenKey(ExtractFileDir(FLocation), False) then
      raise EStartupException.Create(Reg.LastErrorMsg);

    if Reg.KeyExists(ANewName) then
      raise EStartupException.Create('Key already exists!');

    // Rename key and ANewName old key
    Reg.MoveKey(Name, ANewName, True);

    if not Reg.KeyExists(ANewName) then
      raise EStartupException.Create('Key was not renamed!');

    FLocation := DisabledKey + ANewName;
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

constructor TStartupUserItem.Create(const AName, ALocation: string; AEnabled,
  AStartupUser: Boolean; ALnkFile: TLnkFile);
const
  cCurrentUser: array[Boolean] of TRootKey = (rkHKLM, rkHKCU);
  cEnabledWindowsVista: array[Boolean] of TRootKey = (rkHKLM, rkUnknown);

var
  RootKey: TRootKey;

begin
  // Windows 8?
  if CheckWin32Version(6, 2) then
    RootKey := cCurrentUser[AStartupUser]
  else
    RootKey := cEnabledWindowsVista[AEnabled];

  inherited Create(AName, ALnkFile.Command, ALocation, RootKey, AEnabled, False);
  FStartupUser := AStartupUser;
  FLnkFile := ALnkFile;
end;

destructor TStartupUserItem.Destroy;
begin
  FreeAndNil(FLnkFile);
  inherited Destroy;
end;

function TStartupUserItem.AddCircumflex(const AName: string): string;
begin
  Result := AName.Replace('\', '^');
end;

function TStartupUserItem.BackupExists(): Boolean;
begin
  // Deprecated since Windows 8!
  if CheckWin32Version(6, 2) then
    Exit(False);

  Result := System.SysUtils.FileExists(GetBackupLnk());
end;

procedure TStartupUserItem.CreateBackup();
begin
  // Deprecated since Windows 8!
  if CheckWin32Version(6, 2) then
    Exit;

  if not CopyFile(PChar(FLnkFile.FileName), PChar(GetBackupLnk()), False) then
    raise EStartupException.CreateFmt('Backup could not be created in "%s"!', [GetBackupLnk()]);
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

function TStartupUserItem.GetApprovedLocation(): string;
begin
  // Only since Windows 8!
  if CheckWin32Version(6, 2) then
    Result := StartupApprovedKey;
end;

function TStartupUserItem.GetBackupDir(): string;
begin
  Result := GetKnownFolderPath(FOLDERID_Windows);

  if (Result <> '') then
    Result := Result +'pss\';
end;

function TStartupUserItem.GetBackupExtension(): string;
begin
  if FStartupUser then
    Result := FileExtensionStartupUser
  else
    Result := FileExtensionStartupCommon;
end;

function TStartupUserItem.GetBackupLnk(): string;
begin
  // Not possible on Windows 8!
  if CheckWin32Version(6, 2) then
    Exit;

  Result := GetBackupDir() + Name + GetBackupExtension();
end;

function TStartupUserItem.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := ToString() +'|*'+ GetBackupExtension();
end;

procedure TStartupUserItem.SetCommand(const ACommand: TCommandString);
begin
  if (not Enabled and not CheckWin32Version(6, 2)) then
    inherited SetCommand(ACommand);

  FLnkFile.ExeFileName := ACommand.ExtractFileName();
  FLnkFile.Arguments := ACommand.ExtractArguments();

  if not FLnkFile.Save() then
    raise EStartupException.Create('Could not save .lnk!');

  FCommand := ACommand;
  UpdateErasable();

  // Update caption
  FCaption := GetFileDescription(ACommand.Expand());

  // Rewrite backup prior to Windows 7
  if not FEnabled then
    CreateBackup();
end;

function TStartupUserItem.Delete(): Boolean;
var
  Win8: Boolean;

begin
  Win8 := CheckWin32Version(6, 2);

  if (FEnabled or Win8) then
  begin
    // Could not delete .lnk?
    if not DeleteFile(FLnkFile.FileName) then
      raise EStartupException.CreateFmt('Could not delete "%s": %s!', [FLnkFile.FileName, SysErrorMessage(GetLastError())]);

    if Win8 then
      Result := inherited Delete()
    else
      Result := True;
  end  //of begin
  else
  begin
    Result := DeleteKey(HKEY_LOCAL_MACHINE, DisabledKey, AddCircumflex(FLnkFile.FileName));

    // Delete backup file prior to Windows 7
    if Result then
      DeleteBackup();
  end;  //of if
end;

function TStartupUserItem.DeleteBackup(): Boolean;
begin
  // Deprecated since Windows 8!
  if CheckWin32Version(6, 2) then
    Exit(False);

  Result := DeleteFile(GetBackupLnk());
end;

function TStartupUserItem.Disable(): Boolean;
var
  Reg: TRegistry;
  KeyName: string;

begin
  Result := False;

  // Create backup directory if not exist
  if not DirectoryExists(GetBackupDir()) then
    ForceDirectories(GetBackupDir());

  // .lnk does not exist
  if not FLnkFile.Exists() then
    raise EStartupException.CreateFmt('"%s" does not exist!', [FLnkFile.FileName]);

  // Create backup by copying original .lnk
  CreateBackup();

  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    KeyName := AddCircumflex(FLocation);

    if not Reg.OpenKey(DisabledKey + KeyName, True) then
      raise EStartupException.CreateFmt('Could not create key "%s": %s', [DisabledKey + KeyName, Reg.LastErrorMsg]);

    Reg.WriteString('path', FLocation);
    Reg.WriteString('item', ChangeFileExt(ExtractFileName(Name), ''));
    Reg.WriteString('command', Command);
    Reg.WriteString('backup', GetBackupLnk());

    // Special Registry entries only for Windows >= Vista
    if CheckWin32Version(6) then
    begin
      Reg.WriteString('backupExtension', GetBackupExtension());
      Reg.WriteString('location', ExtractFileDir(FLocation));
      FDeactivationTime := WriteTimestamp(Reg);
    end  //of begin
    else
      Reg.WriteString('location', ToString());

    // Delete original .lnk
    if not DeleteFile(FLnkFile.FileName) then
      raise EStartupException.CreateFmt('Could not delete "%s"!', [FLnkFile.FileName]);

    // Update information
    FLocation := DisabledKey + KeyName;
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
  if BackupExists() then
  begin
    // Failed to restore backup file?
    if not CopyFile(PChar(GetBackupLnk()), PChar(FLnkFile.FileName), True) then
      raise EStartupException.CreateFmt('Could not restore backup "%s"!', [GetBackupLnk()]);
  end  //of begin
  else
  begin
    // Failed to create new .lnk file?
    if not FLnkFile.Save() then
      raise EStartupException.CreateFmt('Could not create "%s"!', [FLnkFile.FileName]);
  end;  //of if

  // Do not abort if old key could not be deleted
  if not DeleteKey(HKEY_LOCAL_MACHINE, DisabledKey,
    AddCircumflex(FLnkFile.FileName), False) then
    raise EStartupException.Create('Could not delete key!');

  // Delete backup file after enabling prior to Windows 7
  DeleteBackup();

  // Update information
  FLocation := FLnkFile.FileName;
  FRootKey := rkUnknown;
  FEnabled := True;
  Result := True;
end;

procedure TStartupUserItem.ExportItem(const AFileName: string);
begin
  if not CopyFile(PChar(FLnkFile.FileName), PChar(ChangeFileExt(AFileName,
    GetBackupExtension())), False) then
    raise EStartupException.Create('Could not create backup file!');
end;

procedure TStartupUserItem.Rename(const ANewName: string);
var
  NewLnkFileName, NewName, NewKeyName, NewBackupLnkName: string;
  Reg: TRegistry;
  Win8: Boolean;

begin
  NewName := ChangeFileExt(ANewName, TLnkFile.FileExtension);
  NewLnkFileName := StringReplace(FLnkFile.FileName, ExtractFileName(FLnkFile.FileName),
    NewName, [rfReplaceAll, rfIgnoreCase]);
  Win8 := CheckWin32Version(6, 2);

  if (FEnabled or Win8) then
  begin
    // Rename .lnk file
    if not MoveFile(PChar(FLnkFile.FileName), PChar(NewLnkFileName)) then
      raise EStartupException.Create(SysErrorMessage(GetLastError()));

    FLnkFile.FileName := NewLnkFileName;
    FLocation := NewLnkFileName;

    if Win8 then
      inherited Rename(ANewName)
    else
      FName := NewName;
  end  //of begin
  else
  begin
    // Rename backup .lnk file
    NewBackupLnkName := GetBackupDir() + NewName + GetBackupExtension();
    MoveFile(PChar(GetBackupLnk()), PChar(NewBackupLnkName));

    // Rename disable key prior to Windows 8
    Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;

      if not Reg.KeyExists(FLocation) then
        raise EStartupException.CreateFmt('Key "%s" does not exist!', [FLocation]);

      NewKeyName := DisabledKey + AddCircumflex(NewLnkFileName);

      // New key must not exist
      if Reg.KeyExists(NewKeyName) then
        raise EStartupException.CreateFmt('Key "%s" already exists!', [NewKeyName]);

      // Rename key and delete old key
      Reg.MoveKey(FLocation, NewKeyName, True);

      // Update information
      if not Reg.OpenKey(NewKeyName, False) then
        raise EStartupException.CreateFmt('New key "%s" was not created!', [NewKeyName]);

      Reg.WriteString('path', NewLnkFileName);
      Reg.WriteString('item', ChangeFileExt(NewName, ''));
      Reg.WriteString('backup', NewBackupLnkName);
      FLnkFile.FileName := NewLnkFileName;
      FLocation := NewKeyName;
      FName := NewName;

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try
  end;  //of if
end;

function TStartupUserItem.ToString(): string;
begin
  if StartupUser then
    Result := 'Startup User'
  else
    Result := 'Startup Common';
end;


{ TStartupLocationHelper }

function TStartupLocationHelper.GetApprovedLocation(): TPair<HKEY, string>;
begin
  case Self of
    slHkcuRun, slHkcuRunOnce:
      begin
        Result.Key := HKEY_CURRENT_USER;
        Result.Value := TStartupItem.StartupApprovedKey;
      end;

    slHklmRun, slHklmRunOnce:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := TStartupItem.StartupApprovedKey;
      end;

    slHklmRun32, slHklmRunOnce32:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := TStartupItem.StartupApproved32Key;
      end;

    slStartupUser:
      begin
        Result.Key := HKEY_CURRENT_USER;
        Result.Value := TStartupUserItem.StartupApprovedKey;
      end;

    slCommonStartup:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := TStartupUserItem.StartupApprovedKey;
      end;
  end;  //of case
end;

function TStartupLocationHelper.GetLocation(): TPair<HKEY, string>;
begin
  case Self of
    slHkcuRun:
      begin
        Result.Key := HKEY_CURRENT_USER;
        Result.Value := TStartupItem.StartupRunKey;
      end;

    slHkcuRunOnce:
      begin
        Result.Key := HKEY_CURRENT_USER;
        Result.Value := TStartupItem.StartupRunOnceKey;
      end;

    slHklmRun, slHklmRun32:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := TStartupItem.StartupRunKey;
      end;

    slHklmRunOnce, slHklmRunOnce32:
      begin
        Result.Key := HKEY_LOCAL_MACHINE;
        Result.Value := TStartupItem.StartupRunOnceKey;
      end;

    slStartupUser:
      begin
        Result.Key := 0;
        Result.Value := GetKnownFolderPath(FOLDERID_Startup);
      end;

    slCommonStartup:
      begin
        Result.Key := 0;
        Result.Value := GetKnownFolderPath(FOLDERID_CommonStartup);
      end;
  end;  //of case
end;


{ TStartupList }

function TStartupList.GetImportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := Format(ALanguageFile.GetString(LID_FILTER_STARTUP_FILES),
    [TStartupUserItem.FileExtensionStartupUser, TStartupUserItem.FileExtensionStartupCommon]);
end;

function TStartupList.Add(const AFileName, AArguments, ACaption: string;
  AStartupUser: Boolean = True): Boolean;
const
  cStartupUserLocation: array[Boolean] of TStartupLocation = (
    slCommonStartup,
    slStartupUser
  );

var
  Name, Ext, LnkFileName: string;
  Command: TCommandString;
  Reg: TRegistry;
  LnkFile: TLnkFile;
  i :Integer;

begin
  Result := False;
  Assert(ACaption <> '', 'Caption must not be empty!');
  Name := ExtractFileName(AFileName);
  Ext := ExtractFileExt(Name);

  // Check invalid extension
  if ((Ext <> '.exe') and (Ext <> '.bat')) then
    raise EAssertionFailed.Create('Invalid program extension! Must be ''.exe'' or ''.bat''!');

  // Search is pending?
  if not FSearchLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    // Export is pending?
    if not FExportLock.TryEnter() then
      raise EListBlocked.Create(SOperationPending);

    try
      // Add new startup user item?
      if (Ext = '.exe') then
      begin
        // File path already exists in another item?
        for i := 0 to Count - 1 do
        begin
          if string(Items[i].Command).Contains(AFileName) then
            raise EAlreadyExists.CreateFmt('Item "%s" already exists!', [AFileName]);
        end;  //of for

        LnkFileName := cStartupUserLocation[AStartupUser].GetLocation().Value
          + ChangeFileExt(ACaption, TLnkFile.FileExtension);

        // Do not overwrite existing item
        if FileExists(LnkFileName) then
          raise EAlreadyExists.CreateFmt('Item "%s" already exists!', [LnkFileName]);

        // Prepare .lnk file
        LnkFile := TLnkFile.Create(LnkFileName);

        with LnkFile do
        begin
          ExeFileName := AFileName;
          Arguments := AArguments;
        end;  //of with

        // Link file created successfully?
        if not LnkFile.Save() then
        begin
          FreeAndNil(LnkFile);
          raise EStartupException.CreateFmt('Could not create "%s"!', [LnkFileName]);
        end;  //of begin

        // Add item to list
        Result := (Add(TStartupUserItem.Create(ExtractFileName(LnkFileName),
          LnkFile.FileName, True, AStartupUser, LnkFile)) <> -1);
      end  //of begin
      else
      begin
        // No WOW64 redirection on HKCU!
        Reg := TRegistry.Create(KEY_READ or KEY_WRITE);

        // Try to add new startup item to Registry
        try
          Reg.RootKey := HKEY_CURRENT_USER;
          Reg.OpenKey(TStartupItem.StartupRunKey, True);
          Command := TCommandString.Create(AFileName, AArguments, True);

          // Item already exists?
          if Reg.ValueExists(ACaption) then
            raise EAlreadyExists.CreateFmt('Item "%s" already exists!', [ACaption]);

          Reg.WriteString(ACaption, Command);

          // Adds item to list
          Result := (Add(TStartupItem.Create(ACaption, Command, TStartupItem.StartupRunKey,
            rkHKCU, True, False, False)) <> -1);

        finally
          Reg.CloseKey();
          Reg.Free;
        end;  //of try
      end;  //of begin

      // Windows 8?
      if (Result and CheckWin32Version(6, 2)) then
        // Write the StartupApproved value
        Last.Enabled := True;

    finally
      FExportLock.Release();
    end;  //of try

  finally
    FSearchLock.Release();
  end;  //of try
end;

procedure TStartupList.ExportList(const AFileName: string);
var
  i: Integer;
  RegFile: TRegistryFile;
  Item: TStartupListItem;
  Win8: Boolean;

begin
  Win8 := CheckWin32Version(6, 2);

  // Init Reg file
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);

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
      RegFile.ExportKey(HKEY_CURRENT_USER, TStartupListItem.StartupApprovedBaseKey, True);
      RegFile.ExportKey(HKEY_LOCAL_MACHINE, TStartupListItem.StartupApprovedBaseKey, True);
    end;  //of begin

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
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
  if ((Ext <> TStartupUserItem.FileExtensionStartupCommon) and
    (Ext <> TStartupUserItem.FileExtensionStartupUser)) then
    raise EAssertionFailed.CreateFmt('Invalid backup file extension! Must be "%s" or "%s"!',
      [TStartupUserItem.FileExtensionStartupCommon, TStartupUserItem.FileExtensionStartupUser]);

  // Search is pending?
  if not FSearchLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    // Export is pending?
    if not FExportLock.TryEnter() then
      raise EListBlocked.Create(SOperationPending);

    try
      LnkFile := TLnkFile.Create(AFileName);

      // Set the name of item
      Name := ExtractFileName(ChangeFileExt(AFileName, ''));
      Name := ChangeFileExt(Name, TLnkFile.FileExtension);

      try
        // Create .lnk file and add it to list
        Result := Add(LnkFile.ExeFileName, LnkFile.Arguments, Name, (Ext = TStartupUserItem.FileExtensionStartupUser));

      finally
        LnkFile.Free;
      end;  //of try

    finally
      FExportLock.Release();
    end;  //of try

  finally
    FSearchLock.Release();
  end;  //of try
end;

procedure TStartupList.LoadDisabled(AStartupUser: Boolean);
var
  Reg: TRegistry;
  Items: TStringList;
  i: Integer;
  Item: TStartupListItem;
  Wow64, RunOnce, StartupUser: Boolean;
  Location, Name, KeyPath, OriginKey: string;
  Command: TCommandString;
  LnkFile: TLnkFile;

begin
  // Deprecated since Windows 8!
  if CheckWin32Version(6, 2) then
    Exit;

  Items := TStringList.Create;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if AStartupUser then
      KeyPath := TStartupUserItem.DisabledKey
    else
      KeyPath := TStartupItem.DisabledKey;

    Reg.OpenKey(KeyPath, False);
    Reg.GetKeyNames(Items);

    for i := 0 to Items.Count - 1 do
    begin
      Reg.CloseKey();
      Reg.OpenKey(KeyPath + Items[i], False);
      Location := Reg.CurrentPath;
      Command := Reg.ReadString('command');

      if not AStartupUser then
      begin
        OriginKey := Reg.ReadString('key');
        Wow64 := OriginKey.Contains('Wow6432Node');
        RunOnce := (ExtractFileName(OriginKey) = 'RunOnce');
        Name := Items[i];
        Item := TStartupItem.Create(Name, Command, Location, rkHKLM, False, Wow64, RunOnce);
      end  //of begin
      else
      begin
        Name := ExtractFileName(Location.Replace('^', '\'));

        // Setup non-existing .lnk file
        LnkFile := TLnkFile.Create(Reg.ReadString('path'));
        LnkFile.ExeFileName := Command.ExtractFileName();
        LnkFile.Arguments := Command.ExtractArguments();

        // Windows >= Vista?
        if CheckWin32Version(6) then
          StartupUser := not AnsiSameText(Reg.ReadString('backupExtension'), TStartupUserItem.FileExtensionStartupCommon)
        else
          StartupUser := AnsiSameText(Reg.ReadString('location'), 'Startup');

        Item := TStartupUserItem.Create(Name, Location, False, StartupUser, LnkFile);
      end;  //of if

      Item.DeactivationTime := Item.GetTimestamp(Reg);
      Add(Item);
    end;  //of for

  finally
    Reg.CloseKey();
    Reg.Free;
    Items.Free;
  end;  //of try
end;

function TStartupList.LoadStatus(const AName: string;
  AStartupLocation: TStartupLocation): TStartupItemStatus;
var
  Reg: TRegistry;

begin
  ZeroMemory(@Result, SizeOf(TStartupItemStatus));
  Result.Status := ST_ENABLED;

  // Only on Windows 8 and later!
  if not CheckWin32Version(6, 2) then
    Exit;

  // Status is stored in 64 bit registry
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := AStartupLocation.GetApprovedLocation().Key;
    Reg.OpenKey(AStartupLocation.GetApprovedLocation().Value, False);

    // Approved value exists and is valid?
    if (Reg.ValueExists(AName) and (Reg.GetDataSize(AName) = SizeOf(TStartupItemStatus))) then
      Reg.ReadBinaryData(AName, Result, SizeOf(TStartupItemStatus));

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TStartupList.Load(AStartupLocation: TStartupLocation);
var
  SearchResult: TSearchRec;
  LnkFile: TLnkFile;
  Reg: TRegistry;
  Items: TStringList;
  i: Integer;
  StartupUser, Wow64, RunOnce: Boolean;
  RootKey: TRootKey;
  Item: TStartupListItem;
  Status: TStartupItemStatus;
  Name: string;

begin
  if (AStartupLocation in [slStartupUser, slCommonStartup]) then
  begin
    StartupUser := (AStartupLocation = slStartupUser);

    if (FindFirst(AStartupLocation.GetLocation().Value +'*'+ TLnkFile.FileExtension,
      faAnyFile - faDirectory, SearchResult) = 0) then
    try
      // .lnk file found
      repeat
        Name := SearchResult.Name;
        Status := LoadStatus(Name, AStartupLocation);

        // TODO: EOleSysError can be raised if .lnk is invalid
        LnkFile := TLnkFile.Create(AStartupLocation.GetLocation().Value + Name);

        Item := TStartupUserItem.Create(Name, LnkFile.FileName, Status.GetEnabled(),
          StartupUser, LnkFile);
        Item.DeactivationTime := Status.GetDeactivationTime();
        Add(Item);
      until FindNext(SearchResult) <> 0;

    finally
      FindClose(SearchResult);
    end;  //of try
  end   //of begin
  else
  begin
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
        Item := TStartupItem.Create(Name, Reg.ReadString(Name), Reg.CurrentPath,
          RootKey, Status.GetEnabled(), Wow64, RunOnce);
        Item.DeactivationTime := Status.GetDeactivationTime();
        Add(Item);
      end;  //of for

    finally
      Reg.CloseKey();
      Reg.Free;
      Items.Free;
    end;  //of try
  end;  //of if
end;

procedure TStartupList.Search(AExpertMode: Boolean = False; AWin64: Boolean = True);
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
  begin
    // Include RunOnce in expert mode only
    if ((Location in [slHkcuRunOnce, slHklmRunOnce, slHklmRunOnce32]) and not AExpertMode) then
      Continue;

    Load(Location);
  end;  //of for

  // Load disabled items from deprecated Registry keys only prior to Windows 7
  LoadDisabled(False);
  LoadDisabled(True);
end;


{ TContextMenuListItem}

constructor TContextMenuListItem.Create(const AName, ACaption: string;
  const ACommand: TCommandString; const ALocation: string;
  const AIcon: TMuiString; AEnabled, AWow64: Boolean);
begin
  inherited Create(AName, ACaption, ACommand, ALocation, AEnabled, AWow64);
  FIcon := AIcon;
end;

function TContextMenuListItem.GetIconFileName(): string;
begin
  Result := FIcon;
end;

function TContextMenuListItem.GetRootKey(): TRootKey;
begin
  Result := rkHKCR;
end;

procedure TContextMenuListItem.Rename(const ANewName: string);
begin
  // Important: It is only possible to rename the caption of context items!
  FCaption := ANewName;
end;

function TContextMenuListItem.Delete(): Boolean;
begin
  if not DeleteKey(HKEY_CLASSES_ROOT, ExtractFileDir(GetLocation()), Name) then
    raise EContextMenuException.Create('Could not delete key!');

  Result := True;
end;

function TContextMenuListItem.DeleteUserChoice(const AFileExtension: string): Boolean;
begin
  Result := (RegDeleteKey(HKEY_CURRENT_USER, PChar(Format(KEY_USERCHOICE,
    [AFileExtension]))) = ERROR_SUCCESS);
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure TContextMenuListItem.Execute();
begin
  raise EAbstractError.Create('It is impossible to execute a '+ ToString() +' item!');
end;

function TContextMenuListItem.UserChoiceExists(const AFileExtension: string): Boolean;
var
  UserChoice: string;

begin
  UserChoice := GetRegStringValue(Format(KEY_USERCHOICE, [AFileExtension]),
    'ProgID', HKEY_CURRENT_USER);
  Result := UserChoice.StartsWith('Applications\', True);
end;


{ TContextMenuShellItem }

constructor TContextMenuShellItem.Create(const AName, ACaption: string;
  const ACommand: TCommandString; const ALocation: string; const AIcon: TMuiString;
  AEnabled, AExtended: Boolean);
begin
  inherited Create(AName, ACaption, ACommand, ALocation, AIcon, AEnabled, False);
  FExtended := AExtended;
end;

function TContextMenuShellItem.GetLocation(): string;
begin
  Result := inherited GetLocation() +'\'+ CanonicalName +'\'+ Name;
end;

procedure TContextMenuShellItem.Rename(const AValueName, ANewCaption: string);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create(Reg.LastErrorMsg);

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

procedure TContextMenuShellItem.SetCommand(const ACommand: TCommandString);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetLocation() +'\command', False) then
      raise EContextMenuException.Create(Reg.LastErrorMsg);

    // Change path
    // TODO: May use SHRegSetPath()
    case Reg.GetDataType('') of
      rdExpandString: Reg.WriteExpandString('', ACommand);
      rdString:       Reg.WriteString('', ACommand);
      else            raise EContextMenuException.Create('Invalid data type!');
    end;  //of case

    inherited SetCommand(ACommand);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TContextMenuShellItem.ChangeIcon(const ANewIconFileName: string): Boolean;
const
  ICON_NAME = 'Icon';

var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Invalid key?
    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create(Reg.LastErrorMsg);

    // Delete icon?
    if ((ANewIconFileName = '') and Reg.ValueExists(ICON_NAME)) then
      Result := Reg.DeleteValue(ICON_NAME)
    else
    begin
      // Change icon
      // TODO: May use SHRegSetPath()
      if Reg.ValueExists(ICON_NAME) then
      begin
        case Reg.GetDataType(ICON_NAME) of
          rdExpandString: Reg.WriteExpandString(ICON_NAME, ANewIconFileName);
          rdString:       Reg.WriteString(ICON_NAME, ANewIconFileName);
          else            raise EContextMenuException.Create('Invalid data type!');
        end;  //of case
      end  //of begin
      else
        Reg.WriteString(ICON_NAME, ANewIconFileName);

      Result := True;
    end;  //of if

    if Result then
      FIcon := ANewIconFileName;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextMenuShellItem.SetEnabled(const AEnabled: Boolean);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create(Reg.LastErrorMsg);

    if AEnabled then
    begin
      // Delete disable value, but do not fail if value does not exist!
      if (Reg.ValueExists(DisableValueName) and not Reg.DeleteValue(DisableValueName)) then
        raise EContextMenuException.CreateFmt('Could not delete value "%s"!', [DisableValueName]);
    end  //of begin
    else
      Reg.WriteString(DisableValueName, '');

    // Update status
    inherited SetEnabled(AEnabled);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextMenuShellItem.SetExtended(const AExtended: Boolean);
const
  EXTENDED_NAME = 'Extended';

var
  Reg: TRegistry;

begin
  if (FExtended = AExtended) then
    Exit;

  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create(Reg.LastErrorMsg);

    if AExtended then
      Reg.WriteString(EXTENDED_NAME, '')
    else
      Reg.DeleteValue(EXTENDED_NAME);

    FExtended := AExtended;

  finally
    Reg.Free;
  end;  //of try
end;

function TContextMenuShellItem.DeleteIcon(): Boolean;
begin
  Result := ChangeIcon('');
end;

procedure TContextMenuShellItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);

  try
    RegFile.ExportReg(HKEY_CLASSES_ROOT, GetLocation(), True);

  finally
    RegFile.Free;
  end;  //of try
end;

procedure TContextMenuShellItem.Rename(const ANewName: string);
begin
  Rename('', ANewName);
end;

function TContextMenuShellItem.ToString(): string;
begin
  Result := CanonicalName;
end;


{ TContextMenuShellCascadingItem }

constructor TContextMenuShellCascadingItem.Create(const AName, ACaption, ALocation: string;
  const AIcon: TMuiString; AEnabled, AExtended: Boolean);
begin
  inherited Create(AName, ACaption, '', ALocation, AIcon, AEnabled, AExtended);
end;

procedure TContextMenuShellCascadingItem.GetSubCommands(ASubCommands: TStrings);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if (not Reg.OpenKey(GetLocation(), False) or not Reg.ValueExists(SubCommandsValueName)) then
      Exit;

    ASubCommands.Delimiter := ';';
    ASubCommands.DelimitedText := Reg.ReadString(SubCommandsValueName);

  finally
    Reg.Free;
  end;  //of try
end;

function TContextMenuShellCascadingItem.Delete(): Boolean;
var
  i: Integer;
  Commands: TStringList;

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

procedure TContextMenuShellCascadingItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;
  i: Integer;
  Commands: TStringList;

begin
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);
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

procedure TContextMenuShellCascadingItem.Rename(const ANewName: string);
begin
  Rename(CaptionValueName, ANewName);
end;

function TContextMenuShellCascadingItem.IsErasable(): Boolean;
begin
  // Cannot be erasable since multiple commands are used
  Result := False;
end;

procedure TContextMenuShellCascadingItem.SetCommand(const ACommand: TCommandString);
begin
  raise EAbstractError.Create('It is impossible to change the filename of a '
    + ToString() +' item!');
end;

function TContextMenuShellCascadingItem.ToString(): string;
begin
  Result := inherited ToString() +' Cascading';
end;


{ TContextMenuShellExItem }

procedure TContextMenuShellExItem.SetEnabled(const AEnabled: Boolean);
var
  Reg: TRegistry;
  OldValue: string;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(GetLocation(), False) then
      raise EContextMenuException.Create(Reg.LastErrorMsg);

    // Value does not exist?
    if not Reg.ValueExists('') then
      raise EContextMenuException.Create('Value does not exist!');

    OldValue := Reg.ReadString('');

    if (Trim(OldValue) = '') then
      raise EContextMenuException.Create('Value must not be empty!');

    // Item disabled?
    if (AEnabled and not OldValue.StartsWith('{')) then
      Reg.WriteString('', OldValue.Substring(OldValue.IndexOf('{')))
    else
      // Item enabled?
      if OldValue.StartsWith('{') then
        Reg.WriteString('', OldValue.Insert(0, '-'));

    // Update status
    inherited SetEnabled(AEnabled);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TContextMenuShellExItem.GetLocation(): string;
begin
  Result := inherited GetLocation() +'\'+ HandlersKey +'\'+ Name;
end;

procedure TContextMenuShellExItem.SetCommand(const ACommand: TCommandString);
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
      raise EContextMenuException.Create(Reg.LastErrorMsg);

    // Read GUID and setup key of program
    ProgramKeyPath := Format('CLSID\%s\InProcServer32', [Reg.ReadString('')]);
    Reg.CloseKey;

    // Invalid program key?
    if not Reg.OpenKey(ProgramKeyPath, False) then
      raise EContextMenuException.Create(Reg.LastErrorMsg);

    // Change path
    // TODO: May use SHRegSetPath()
    case Reg.GetDataType('') of
      rdExpandString: Reg.WriteExpandString('', ACommand);
      rdString:       Reg.WriteString('', ACommand);
      else            raise EContextMenuException.Create('Invalid data type!');
    end;  //of case

    // Update path
    inherited SetCommand(ACommand);
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextMenuShellExItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;
  Reg: TRegistry;
  Key: string;

begin
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);

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

procedure TContextMenuShellExItem.Rename(const ANewName: string);
begin
  raise EAbstractError.Create('It is impossible to rename a '+ ToString() +' item!');
end;

function TContextMenuShellExItem.ToString(): string;
begin
  Result := CanonicalName;
end;


{ TContextMenuShellNewItem }

constructor TContextMenuShellNewItem.Create(const AName, ACaption, ALocation: string;
  const AIcon: TMuiString; AEnabled: Boolean);
begin
  inherited Create(AName, ACaption, '', ALocation, AIcon, AEnabled, False);
end;

procedure TContextMenuShellNewItem.SetEnabled(const AEnabled: Boolean);
var
  Reg: TRegistry;
  OldKeyName, NewKeyName: string;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Key does not exist?
    if not Reg.OpenKey(FLocation, False) then
      raise EContextMenuException.Create(Reg.LastErrorMsg);

    if AEnabled then
    begin
      OldKeyName := CanonicalNameDisabled;
      NewKeyName := CanonicalName;
    end  //of begin
    else
    begin
      OldKeyName := CanonicalName;
      NewKeyName := CanonicalNameDisabled;
    end;  //of if

    // Old key does not exist?
    if not Reg.KeyExists(OldKeyName) then
      raise EContextMenuException.CreateFmt('Key "%s" does not exist!', [OldKeyName]);

    // New key already exists?
    if Reg.KeyExists(NewKeyName) and not Reg.DeleteKey(NewKeyName) then
      raise EContextMenuException.CreateFmt('Key "%s" already exists!', [NewKeyName]);

    // Rename key
    Reg.MoveKey(OldKeyName, NewKeyName, True);

    // Update status
    inherited SetEnabled(AEnabled);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TContextMenuShellNewItem.GetLocation(): string;
begin
  if FEnabled then
    Result := inherited GetLocation() +'\'+ CanonicalName
  else
    Result := inherited GetLocation() +'\'+ CanonicalNameDisabled;
end;

function TContextMenuShellNewItem.IsErasable(): Boolean;
begin
  // Command is not always used: Item is not erasable!
  if (FCommand = '') then
    Exit(False);

  Result := inherited IsErasable();
end;

procedure TContextMenuShellNewItem.SetCommand(const ACommand: TCommandString);
begin
  raise EAbstractError.Create('It is impossible to change the filename of a '
    + ToString() +' item!');
end;

function TContextMenuShellNewItem.Delete(): Boolean;
begin
  if not DeleteKey(HKEY_CLASSES_ROOT, ExtractFileDir(GetLocation()),
    ExtractFileName(GetLocation())) then
    raise EContextMenuException.Create('Could not delete key!');

  Result := True;
end;

procedure TContextMenuShellNewItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);

  try
    RegFile.ExportReg(HKEY_CLASSES_ROOT, GetLocation(), True);

  finally
    RegFile.Free;
  end;  //of try
end;

procedure TContextMenuShellNewItem.Rename(const ANewName: string);
begin
  raise EAbstractError.Create('It is impossible to rename a '+ ToString() +' item!');
end;

function TContextMenuShellNewItem.ToString(): string;
begin
  Result := CanonicalName;
end;


{ TContextMenuList }

constructor TContextMenuList.Create;
begin
  inherited Create;
  AllowDuplicates := True;
end;

function TContextMenuList.Add(const AFileName, AArguments, ALocationRoot,
  ACaption: string): Boolean;
var
  Name, Ext, LocationRoot, FileType, KeyPath: string;
  Command: TCommandString;
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

  // Search is pending?
  if not FSearchLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    // Export is pending?
    if not FExportLock.TryEnter() then
      raise EListBlocked.Create(SOperationPending);

    try
      Name := ChangeFileExt(ExtractFileName(AFileName), '');

      // File path already exists in another item?
      if (IndexOf(Name, LocationRoot) <> -1) then
        raise EAlreadyExists.CreateFmt('Item "%s" already exists in "%s"!', [Name, LocationRoot]);

      Command := TCommandString.Create(AFileName, AArguments, True);

      // Init Registry access
      Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

      try
        Reg.RootKey := HKEY_CLASSES_ROOT;

        if not Reg.OpenKey(LocationRoot, True) then
          raise EContextMenuException.CreateFmt('Could not create key "%s": %s', [LocationRoot, Reg.LastErrorMsg]);

        // Location is a file extension?
        if LocationRoot.StartsWith('.') then
        begin
          // Read default associated file type
          FileType := Reg.ReadString('');

          // Associated file type not found?
          if (FileType = '') then
          begin
            // Add new file association
            FileType := LocationRoot.Substring(1) +'file';
            Reg.WriteString('', FileType);
          end;  //of begin
        end  //of begin
        else
          FileType := LocationRoot;

        Reg.CloseKey();
        KeyPath := FileType +'\'+ TContextMenuShellItem.CanonicalName +'\'+ Name;

        // Adds new context item to Registry
        if not Reg.OpenKey(KeyPath, True) then
          raise EContextMenuException.CreateFmt('Could not open key "%s": %s', [KeyPath, Reg.LastErrorMsg]);

        // Set caption of item
        if (Trim(ACaption) <> '') then
          Reg.WriteString('', ACaption);

        Reg.CloseKey();
        KeyPath := KeyPath +'\command';

        if not Reg.OpenKey(KeyPath, True) then
          raise EContextMenuException.CreateFmt('Could not create key "%s": %s', [KeyPath, Reg.LastErrorMsg]);

        // Write command of item
        Reg.WriteString('', Command);

        // Adds item to list
        Result := (Add(TContextMenuShellItem.Create(Name, ACaption, Command, FileType, '', True, False)) <> -1);
        SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

      finally
        Reg.CloseKey();
        Reg.Free;
      end;  //of try

    finally
      FExportLock.Release();
    end;  //of try

  finally
    FSearchLock.Release();
  end;  //of try
end;

procedure TContextMenuList.ExportList(const AFileName: string);
var
  i: Integer;
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);

  try
    for i := 0 to Count - 1 do
      RegFile.ExportKey(HKEY_CLASSES_ROOT, Items[i].Location, True);

    RegFile.Save();

  finally
    RegFile.Free;
  end;  //of try
end;

function TContextMenuList.IndexOf(const AName, ALocationRoot: string): Integer;
var
  i: Integer;
  Item: TContextMenuListItem;

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

procedure TContextMenuList.LoadContextmenu(const ALocationRoot: string;
  AWow64: Boolean);
var
  ContextMenuItem: TContextMenuItemType;

begin
  for ContextMenuItem := Low(TContextMenuItemType) to High(TContextMenuItemType) do
    LoadContextmenu(ALocationRoot, ContextMenuItem, AWow64);
end;

procedure TContextMenuList.LoadContextmenu(const ALocationRoot: string;
  AContextMenuItemType: TContextMenuItemType; AWow64: Boolean);

  function GetShellExCaptionAndFileName(const AProgId: string; AWow64: Boolean;
    var ACaption, AFileName: string): Boolean;
  var
    Reg: TRegistry;

  begin
    Result := False;

    // Use 32 bit hive?
    if AWow64 then
      Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ)
    else
      Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

    try
      Reg.RootKey := HKEY_CLASSES_ROOT;

      if Reg.OpenKey('CLSID\'+ AProgId, False) then
      begin
        ACaption := Reg.ReadString('');

        if AWow64 then
          ACaption := ACaption +'32';

        if Reg.OpenKey('InProcServer32', False) then
        begin
          if not (Reg.GetDataType('') in [rdString, rdExpandString]) then
            Exit;

          AFileName := Reg.ReadString('');
          Result := True;
        end;  //of begin
      end;  //of begin

    finally
      Reg.Free;
    end;  //of try
  end;

var
  Reg: TRegistry;
  i, DelimiterPos: Integer;
  Keys: TStringList;
  ItemName, Key, FileName, ProgId, Caption: string;
  Enabled, Wow64, Extended: Boolean;
  MuiString: TMuiString;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);
  Keys := TStringList.Create;

  case AContextMenuItemType of
    itShell:    Key := ALocationRoot +'\'+ TContextMenuShellItem.CanonicalName;
    itShellEx:  Key := ALocationRoot +'\'+ TContextMenuShellExItem.HandlersKey;
    itShellNew: Key := ALocationRoot;
  end;  //of case

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Skip invalid key
    if not Reg.OpenKey(Key, False) then
      Exit;

    // Search for ShellNew items
    if (AContextMenuItemType = itShellNew) then
    begin
      // "ShellNew" and "_ShellNew" in same key are possible: prefer "ShellNew"
      // TODO: Remove _ShellNew and disable ShellNew
      if Reg.KeyExists(TContextMenuShellNewItem.CanonicalName) then
        Enabled := True
      else
        if Reg.KeyExists(TContextMenuShellNewItem.CanonicalNameDisabled) then
          Enabled := False
        else
          Exit;

      // ShellNew item inside a subkey?
      DelimiterPos := ALocationRoot.IndexOf('\');

      if (DelimiterPos > 0) then
      begin
        Reg.CloseKey;
        Reg.OpenKey(ALocationRoot.Substring(0, DelimiterPos), False);
      end;  //of begin

      // Get associated key
      Key := Reg.ReadString('');

      if (Key <> '') then
      begin
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
      end;  //of begin

      // Fallback 2: subkey
      if (Caption = '') then
        Caption := ExtractFileName(ALocationRoot);

      // Some types do not have a association: Use caption
      if (Key = '') then
        Key := Caption;

      Add(TContextMenuShellNewItem.Create(Key, Caption, ALocationRoot, '', Enabled));
    end  //of begin
    else
    begin
      // Read out all keys
      Reg.GetKeyNames(Keys);

      for i := 0 to Keys.Count - 1 do
      begin
        Reg.CloseKey();
        ItemName := Keys[i];
        Reg.OpenKey(Key +'\'+ ItemName, False);
        FileName := '';

        // Filter items with GUID in name
        if ItemName.StartsWith('{') then
          Continue;

        // Search for shell entries?
        if (AContextMenuItemType = itShell) then
        begin
          Caption := Reg.ReadString('');

          // MUI string
          if Caption.StartsWith('@') then
          begin
            MuiString := Caption;
            Caption := LoadResourceString(MuiString.Path(), MuiString.Id);
          end;  //of begin

          // Get status and caption of Shell item
          Enabled := not Reg.ValueExists(TContextMenuShellItem.DisableValueName);
          Extended := Reg.ValueExists('Extended');
          MuiString := Reg.ReadString('Icon');

          // Cascading shell item?
          if not Reg.OpenKey('command', False) then
          begin
            if ((not Reg.ValueExists(TContextMenuShellCascadingItem.CaptionValueName) or
              not Reg.ValueExists(TContextMenuShellCascadingItem.SubCommandsValueName)) and
              not Reg.KeyExists('ExtendedSubCommandsKey')) then
              Continue;

            Caption := Reg.ReadString(TContextMenuShellCascadingItem.CaptionValueName);
            Add(TContextMenuShellCascadingItem.Create(ItemName, Caption, ALocationRoot,
              MuiString, Enabled, Extended));
            Continue;
          end;  //of begin

          // Get file path of command
          if not (Reg.GetDataType('') in [rdString, rdExpandString]) then
            Continue;

          FileName := Reg.ReadString('');
          Add(TContextMenuShellItem.Create(ItemName, Caption, FileName, ALocationRoot,
            MuiString, Enabled, Extended));
        end  //of begin
        else
          // Search for shell extensions
          if (AContextMenuItemType = itShellEx) then
          begin
            ProgId := Reg.ReadString('');
            Wow64 := False;

            // Filter empty and unreadable ShellEx items
            if ((ProgId = '') or ProgId.StartsWith('@')) then
              Continue;

            // Get status and GUID of ShellEx item
            Enabled := ProgId.StartsWith('{');

            // Disabled ShellEx items got "-" before GUID!
            if not Enabled then
              ProgId := ProgId.Substring(ProgId.IndexOf('{'));

            // Try to get file path and description in native hive
            if not GetShellExCaptionAndFileName(ProgId, False, Caption, FileName) then
            begin
              // Try to get file path and description in redirected hive
              if GetShellExCaptionAndFileName(ProgId, True, Caption, FileName) then
                Wow64 := True;
            end;  //of begin

            Add(TContextMenuShellExItem.Create(ItemName, Caption, FileName, ALocationRoot, '', Enabled, Wow64));
          end;  //of begin
      end;  //of for
    end;  //of if

  finally
    Keys.Free;
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextMenuList.Search(AExpertMode: Boolean = False; AWin64: Boolean = True);
begin
  Search(AExpertMode, AWin64, '');

  if AExpertMode then
    Search(AExpertMode, AWin64, 'Directory');
end;

procedure TContextMenuList.Search(AExpertMode: Boolean; AWin64: Boolean;
  const ARoot: string = '');
var
  Reg: TRegistry;

  procedure SearchSubkey(const AKeyName: string);
  var
    Keys, Values: TStringList;
    i: Integer;

  begin
    Keys := TStringList.Create;
    Values := TStringList.Create;

    try
      if not Reg.OpenKey(AKeyName, False) then
        Exit;

      if Reg.HasSubKeys() then
      begin
        // Load subkeys
        Reg.GetKeyNames(Keys);
        Reg.CloseKey();

        for i := 0 to Keys.Count - 1 do
        begin
          // Load Shell context menu items
          if AnsiSameText(Keys[i], TContextMenuShellItem.CanonicalName) then
            LoadContextmenu(AKeyName, itShell, AWin64);

          // Load ShellEx context menu items
          if AnsiSameText(Keys[i], TContextMenuShellExItem.CanonicalName) then
            LoadContextmenu(AKeyName, itShellEx, AWin64);

          // Load ShellNew context menu items
          if Keys[i].Contains(TContextMenuShellNewItem.CanonicalName) then
          begin
            Reg.OpenKey(AKeyName +'\'+ Keys[i], False);
            Reg.GetValueNames(Values);
            Reg.CloseKey();

            // Only valid ShellNew item when there are values inside
            if (Values.Count > 0) then
            begin
              LoadContextmenu(AKeyName, itShellNew, AWin64);

              // "ShellNew" and "_ShellNew" in same key are possible: Take only one
              Exit;
            end;  //of begin
          end;  //of begin

          // File extension: Search in subkey for ShellNew items
          if AKeyName.StartsWith('.') then
            SearchSubkey(AKeyName +'\'+ Keys[i]);
        end;  //of for
      end;  //of begin

    finally
      Reg.CloseKey();
      Values.Free;
      Keys.Free;
    end;  //of try
  end;

var
  i: Integer;
  Locations: TStringList;

begin
  Locations := TStringList.Create;
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(ARoot, False);

    if AExpertMode then
    begin
      Reg.GetKeyNames(Locations);
      Reg.CloseKey();
    end  //of begin
    else
      Locations.CommaText := DefaultLocations;

    for i := 0 to Locations.Count - 1 do
    begin
      if (ARoot <> '') then
        SearchSubkey(ARoot +'\'+ Locations[i])
      else
        SearchSubkey(Locations[i]);
    end;  //of for

  finally
    Reg.CloseKey();
    Reg.Free;
    Locations.Free;
  end;  //of try
end;


{ TServiceListItem }

constructor TServiceListItem.Create(const AName, ACaption: string;
  const ACommand: TCommandString; AEnabled: Boolean; AServiceStart: TServiceStart;
  AServiceManager: SC_HANDLE);
begin
  inherited Create(AName, ACaption, ACommand, KEY_SERVICE_ENABLED + AName, AEnabled, False);
  FServiceStart := AServiceStart;
  FServiceManager := AServiceManager;
  FDeactivationTime := 0;
end;

function TServiceListItem.GetHandle(AAccess: DWORD): SC_HANDLE;
var
  Handle: SC_HANDLE;

begin
  Assert(FServiceManager <> 0, 'Service manager not initialized!');
  Handle := OpenService(FServiceManager, PChar(Name), AAccess);

  // Error occured?
  if (Handle = 0) then
    raise EServiceException.Create(SysErrorMessage(GetLastError()));

  Result := Handle;
end;

function TServiceListItem.GetRootKey(): TRootKey;
begin
  Result := rkHKLM;
end;

procedure TServiceListItem.SetCommand(const ACommand: TCommandString);
var
  Service: SC_HANDLE;

begin
  Service := GetHandle(SERVICE_CHANGE_CONFIG);

  try
    // Change path
    if not ChangeServiceConfig(Service, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE,
      SERVICE_NO_CHANGE, PChar(ACommand), nil, nil, nil, nil, nil, nil) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    inherited SetCommand(ACommand);

  finally
    CloseServiceHandle(Service);
  end;  //of try
end;

procedure TServiceListItem.SetEnabled(const AEnabled: Boolean);
var
  Service: SC_HANDLE;
  Reg: TRegistry;

begin
  Service := GetHandle(SERVICE_CHANGE_CONFIG);
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if AEnabled then
    begin
      // Windows >= Vista?
      if CheckWin32Version(6) then
      begin
        if not Reg.OpenKey(KEY_SERVICE_DISABLED +'\'+ Name, False) then
          raise EServiceException.Create(Reg.LastErrorMsg);
      end  //of begin
      else
        Reg.OpenKey(KEY_SERVICE_DISABLED, True);

      // Last status exists?
      if not Reg.ValueExists(Name) then
        raise EServiceException.Create('Last status does not exist!');

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
      begin
        // Delete last status
        if not Reg.DeleteValue(Name) then
          raise EServiceException.Create('Could not delete last status!');
      end;  //of if

      FDeactivationTime := 0;
    end  //of begin
    else
    begin
      // Disable service
      if not ChangeServiceConfig(Service, SERVICE_NO_CHANGE, SERVICE_DISABLED,
        SERVICE_NO_CHANGE, nil, nil, nil, nil, nil, nil, nil) then
        raise EServiceException.Create(SysErrorMessage(GetLastError()));

      // Windows >= Vista?
      if CheckWin32Version(6) then
      begin
        // Write disable key
        if not Reg.OpenKey(KEY_SERVICE_DISABLED +'\'+ Name, True) then
          raise EServiceException.Create('Could not create disable key: '+ Reg.LastErrorMsg);

        // Save deactivation timestamp
        FDeactivationTime := WriteTimestamp(Reg);
      end  //of begin
      else
      begin
        // Write disable key
        if not Reg.OpenKey(KEY_SERVICE_DISABLED, True) then
          raise EServiceException.Create('Could not create disable key: '+ Reg.LastErrorMsg);
      end;  //of if

      // Write last status
      Reg.WriteInteger(Name, Ord(Start));
    end;  //of if

    inherited SetEnabled(AEnabled);

  finally
    Reg.CloseKey();
    Reg.Free;
    CloseServiceHandle(Service);
  end;  //of try
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

procedure TServiceListItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);

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
  FManager := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE, SC_MANAGER_ENUMERATE_SERVICE);
end;

destructor TServiceList.Destroy();
begin
  CloseServiceHandle(FManager);
  inherited Destroy;
end;

function TServiceList.Add(const AFileName, AArguments, ACaption: string): Boolean;
var
  Name: string;
  Command: TCommandString;
  Service: SC_HANDLE;
  LastError: DWORD;
  Manager: SC_HANDLE;

begin
  Result := False;
  Name := ExtractFileName(AFileName);

  // Check invalid extension
  Assert(ExtractFileExt(Name) = '.exe', 'Invalid program extension! Must be ''.exe''!');

  // Search is pending?
  if not FSearchLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    // Export is pending?
    if not FExportLock.TryEnter() then
      raise EListBlocked.Create(SOperationPending);

    try
      // To create a new service SC_MANAGER_CREATE_SERVICE access right is required
      Manager := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE, SC_MANAGER_CREATE_SERVICE);

      // Error occured?
      if (Manager = 0) then
        raise EServiceException.Create(SysErrorMessage(GetLastError()));

      try
        Command := TCommandString.Create(AFileName, AArguments, True);
        Name := ChangeFileExt(Name, '');

        // Create a new service
        Service := CreateService(Manager, PChar(Name), PChar(ACaption),
          STANDARD_RIGHTS_READ, SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START,
          SERVICE_ERROR_NORMAL, PChar(Command), nil, nil, nil, nil, nil);

        // Error occured?
        if (Service = 0) then
        begin
          LastError := GetLastError();

          // Service already exists?
          if (LastError = ERROR_SERVICE_EXISTS) then
            raise EAlreadyExists.CreateFmt('Item "%s" already exists!', [Name]);

          raise EServiceException.Create('Could not create new service: '+ SysErrorMessage(LastError));
        end;  //of begin

        CloseServiceHandle(Service);

        // Adds service to list
        Result := (Add(TServiceListItem.Create(Name, ACaption, Command, True,
          ssAutomatic, FManager)) <> -1);

      finally
        CloseServiceHandle(Manager);
      end;  //of try

    finally
      FExportLock.Release();
    end;  //of try

  finally
    FSearchLock.Release();
  end;  //of try
end;

procedure TServiceList.ExportList(const AFileName: string);
var
  i: Integer;
  RegFile: TRegistryFile;
  Item: TServiceListItem;

begin
  // Init Reg file
  RegFile := TRegistryFile.Create(ChangeFileExt(AFileName, GetBackupExtension()), True);

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
  end;  //of try
end;

function TServiceList.LoadService(const AName: string; AIncludeDemand: Boolean = False): Integer;
var
  Service: SC_HANDLE;
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
  Service := OpenService(FManager, PChar(AName), SERVICE_QUERY_CONFIG);

  // Skip corrupted service
  if (Service = 0) then
  {$IFDEF DEBUG}
    raise EServiceException.Create(SysErrorMessage(GetLastError()));
  {$ELSE}
    Exit;
  {$ENDIF}

  try
    // Determine the required size for buffer
    QueryServiceConfig(Service, ServiceConfig, 0, BytesNeeded);
    LastError := GetLastError();

    // ERROR_INSUFFICIENT_BUFFER will be fired normally
    if (LastError <> ERROR_INSUFFICIENT_BUFFER) then
      raise EServiceException.Create(SysErrorMessage(LastError));

    GetMem(ServiceConfig, BytesNeeded);

    try
      // Read service config
      if not QueryServiceConfig(Service, ServiceConfig, BytesNeeded, BytesNeeded) then
        raise EServiceException.Create(SysErrorMessage(GetLastError()));

      ServiceStart := TServiceStart(ServiceConfig^.dwStartType);

      // Filter important system services
      if (ServiceStart in [ssBoot, ssSystem]) then
        Exit;

      // Skip services started manual when not in expert mode
      if ((ServiceStart = ssManual) and not AIncludeDemand) then
        Exit;

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
          Item.DeactivationTime := Item.GetTimestamp(Reg);

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

  finally
    CloseServiceHandle(Service);
  end;  //of try
end;

procedure TServiceList.Search(AExpertMode: Boolean = False; AWin64: Boolean = True);
var
  Services, ServicesCopy: PEnumServiceStatus;
  BytesNeeded, ServicesReturned, ResumeHandle, LastError, ServiceType: DWORD;
  i: Integer;

begin
  ServicesReturned := 0;
  ResumeHandle := 0;
  Services := nil;

  // Include services that are shared with other processes?
  if AExpertMode then
    ServiceType := SERVICE_WIN32
  else
    ServiceType := SERVICE_WIN32_OWN_PROCESS;

  // Determine the required size for buffer
  EnumServicesStatus(FManager, ServiceType, SERVICE_STATE_ALL, Services^, 0,
    BytesNeeded, ServicesReturned, ResumeHandle);

  LastError := GetLastError();

  // ERROR_MORE_DATA will be fired normally
  if (LastError <> ERROR_MORE_DATA) then
    raise EServiceException.Create(SysErrorMessage(LastError));

  GetMem(Services, BytesNeeded);

  try
    ServicesReturned := 0;
    ResumeHandle := 0;
    ServicesCopy := Services;

    // Read all services matching
    if not EnumServicesStatus(FManager, ServiceType, SERVICE_STATE_ALL,
      Services^, BytesNeeded, BytesNeeded, ServicesReturned, ResumeHandle) then
      raise EServiceException.Create(SysErrorMessage(GetLastError()));

    // Add services to list
    for i := 0 to ServicesReturned - 1 do
    begin
      LoadService(ServicesCopy^.lpServiceName, AExpertMode);
      Inc(ServicesCopy);
    end;  //of for

  finally
    FreeMem(Services, BytesNeeded);
  end;  //of try
end;


{ TTaskListItem }

constructor TTaskListItem.Create(const AName: string; const ACommand: TCommandString;
  const ALocation: string; AEnabled: Boolean; const ATask: IRegisteredTask;
  const ATaskService: ITaskService; AInvalid: Boolean = False);
begin
  inherited Create(AName, '', ACommand, ALocation, AEnabled);
  FTask := ATask;
  FTaskService := ATaskService;
  FInvalid := AInvalid;
  UpdateErasable();
end;

procedure TTaskListItem.SetEnabled(const AEnabled: Boolean);
begin
  FTask.Enabled := AEnabled;
  inherited SetEnabled(AEnabled);
end;

function TTaskListItem.GetTaskDefinition(): ITaskDefinition;
begin
  Result := FTask.Definition;
end;

function TTaskListItem.GetZipLocation(): string;
begin
  Result := IncludeTrailingPathDelimiter(Location).Substring(1) + Name;
  Result := Result.Replace('\', '/');
end;

function TTaskListItem.IsErasable(): Boolean;
begin
  // Invalid tasks can be deleted
  if FInvalid then
    Exit(True);

  // Command is not always used: Item is not erasable!
  if (FCommand = '') then
    Exit(False);

  Result := inherited IsErasable();
end;

function TTaskListItem.GetFullLocation(): string;
begin
  Result := GetKnownFolderPath(FOLDERID_System);

  if (Result <> '') then
    Result := IncludeTrailingPathDelimiter(Result +'Tasks'+ GetLocation()) + Name;
end;

procedure TTaskListItem.SetCommand(const ACommand: TCommandString);
var
  Actions: IEnumVariant;
  Action: IAction;
  ActionItem: OleVariant;
  ExecAction: IExecAction;
  Fetched: DWORD;
  NewDefinition: ITaskDefinition;
  TaskFolder: ITaskFolder;
  NewTask: IRegisteredTask;

begin
  OleCheck(FTaskService.NewTask(0, NewDefinition));
  NewDefinition := FTask.Definition;
  Actions := (NewDefinition.Actions._NewEnum as IEnumVariant);

  // Try to find executable command in task
  if (Actions.Next(1, ActionItem, Fetched) = S_OK) then
  begin
    Action := (IDispatch(ActionItem) as IAction);

    // Task has an executable?
    if (Action.ActionType = TASK_ACTION_EXEC) then
    begin
      ExecAction := (Action as IExecAction);
      ExecAction.Path := PChar(ACommand.ExtractFileName());
      ExecAction.Arguments := PChar(ACommand.ExtractArguments());

      OleCheck(FTaskService.GetFolder(PChar(Location), TaskFolder));
      OleCheck(TaskFolder.RegisterTaskDefinition(FTask.Name, NewDefinition,
        TASK_UPDATE, Null, Null, TASK_LOGON_NONE, Null, NewTask));

      FTask := NewTask;
      inherited SetCommand(ACommand);
    end;  //of begin
  end;  //of while
end;

function TTaskListItem.Delete(): Boolean;
var
  TaskFolder: ITaskFolder;

begin
  OleCheck(FTaskService.GetFolder(PChar(Location), TaskFolder));
  OleCheck(TaskFolder.DeleteTask(PChar(Name), 0));
  Result := True;
end;

procedure TTaskListItem.Execute();
var
  RunningTask: IRunningTask;

begin
  OleCheck(FTask.Run(VT_NULL, RunningTask));
end;

procedure TTaskListItem.ExportItem(const AFileName: string);
var
  ZipFile: TZipFile;
  OldValue: Boolean;

begin
  OldValue := DisableWow64FsRedirection();
  ZipFile := TZipFile.Create;

  try
    ZipFile.Open(ChangeFileExt(AFileName, GetBackupExtension()), zmWrite);

    // For validation purposes: "Clearas" is the comment
    ZipFile.Comment := 'Clearas';
    ZipFile.Add(LocationFull, ZipLocation, zcDeflate);
    ZipFile.Close();

  finally
    ZipFile.Free;
    RevertWow64FsRedirection(OldValue);
  end;  //of try
end;

function TTaskListItem.GetBackupExtension(): string;
begin
  Result := '.zip';
end;

function TTaskListItem.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := ALanguageFile.GetString(LID_FILTER_ZIP_FILES);
end;

procedure TTaskListItem.Rename(const ANewName: string);
var
  NewDefinition: ITaskDefinition;
  TaskFolder: ITaskFolder;
  NewTask: IRegisteredTask;

begin
  // Copy task definition
  OleCheck(FTaskService.NewTask(0, NewDefinition));
  NewDefinition := FTask.Definition;

  // Register definition under new task name
  OleCheck(FTaskService.GetFolder(PChar(Location), TaskFolder));
  OleCheck(TaskFolder.RegisterTaskDefinition(PChar(ANewName), NewDefinition,
    TASK_CREATE, Null, Null, TASK_LOGON_NONE, Null, NewTask));

  // Delete old task
  OleCheck(TaskFolder.DeleteTask(FTask.Name, 0));

  FTask := NewTask;
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
  OleCheck(CoCreateInstance(CLSID_TaskScheduler, nil, CLSCTX_INPROC_SERVER,
    IID_ITaskService, FTaskService));
  OleCheck(FTaskService.Connect(Null, Null, Null, Null));
end;

destructor TTaskList.Destroy;
begin
  CoUninitialize();
  inherited Destroy;
end;

function TTaskList.AddTaskItem(const ATask: IRegisteredTask): Integer;
var
  Item: TTaskListItem;
  Action: IAction;
  ExecAction: IExecAction;
  Actions: IEnumVariant;
  ActionItem: OleVariant;
  Fetched: DWORD;
  Command: TCommandString;
  Enabled, Invalid: Boolean;

begin
  Enabled := False;
  Invalid := False;

  // Try to read task definition
  try
    Actions := (ATask.Definition.Actions._NewEnum as IEnumVariant);

    if (Actions.Next(1, ActionItem, Fetched) = S_OK) then
    begin
      Action := (IDispatch(ActionItem) as IAction);

      // Task has an executable?
      if (Action.ActionType = TASK_ACTION_EXEC) then
      begin
        ExecAction := (Action as IExecAction);
        Command := TCommandString.Create(ExecAction.Path, ExecAction.Arguments);
      end;  //of begin
    end;  //of while

    Enabled := ATask.Enabled;

  except
    // Mark task as erasable if something went wrong
    on EOleSysError do
      Invalid := True;
  end;  //of try

  Item := TTaskListItem.Create(ATask.Name, Command, ExtractFileDir(ATask.Path),
    Enabled, ATask, FTaskService, Invalid);
  Result := Add(Item);
end;

procedure TTaskList.ExportList(const AFileName: string);
var
  i: Integer;
  ZipFile: TZipFile;
  OldValue: Boolean;

begin
  OldValue := DisableWow64FsRedirection();
  ZipFile := TZipFile.Create;

  try
    ZipFile.Open(ChangeFileExt(AFileName, GetBackupExtension()), zmWrite);

    // For validation purposes: "Clearas" is the comment
    ZipFile.Comment := 'Clearas';

    for i := 0 to Count - 1 do
      ZipFile.Add(Items[i].LocationFull, Items[i].ZipLocation, zcDeflate);

    ZipFile.Close();

  finally
    ZipFile.Free;
    RevertWow64FsRedirection(OldValue);
  end;  //of try
end;

function TTaskList.GetBackupExtension(): string;
begin
  Result := '.zip';
end;

function TTaskList.GetExportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := ALanguageFile.GetString(LID_FILTER_ZIP_FILES);
end;

function TTaskList.GetImportFilter(ALanguageFile: TLanguageFile): string;
begin
  Result := GetExportFilter(ALanguageFile);
end;

function TTaskList.ImportBackup(const AFileName: TFileName): Boolean;
var
  ExtractDir, FileName: string;
  TaskFolder: ITaskFolder;
  NewTask: IRegisteredTask;
  ZipFile: TZipFile;
  i: Integer;
  XmlTask: TStringList;
  OldValue: Boolean;

begin
  Result := False;

  // Check invalid extension
  if (ExtractFileExt(AFileName) <> GetBackupExtension()) then
    raise EAssertionFailed.CreateFmt('Invalid backup file extension! Must be "%s"!', [GetBackupExtension()]);

  // Search is pending?
  if not FSearchLock.TryEnter() then
    raise EListBlocked.Create(SOperationPending);

  try
    // Export is pending?
    if not FExportLock.TryEnter() then
      raise EListBlocked.Create(SOperationPending);

    OldValue := DisableWow64FsRedirection();

    try
      XmlTask := TStringList.Create;
      ZipFile := TZipFile.Create;

      try
        ZipFile.Open(AFileName, zmRead);

        // For validation purposes: "Clearas" is the comment
        if (ZipFile.Comment <> 'Clearas') then
          raise ETaskException.Create(SysErrorMessage(SCHED_E_INVALID_TASK_HASH));

        ExtractDir := GetKnownFolderPath(FOLDERID_LocalAppData) +'Temp\Clearas';

        // Extract all files inside the ZIP in a temporary directory
        for i := 0 to ZipFile.FileCount - 1 do
        begin
          NewTask := nil;
          TaskFolder := nil;
          FileName := ZipFile.FileName[i];
          ZipFile.Extract(FileName, ExtractDir);
          FileName := FileName.Replace('/', '\');
          OleCheck(FTaskService.GetFolder(PChar('\'+ ExtractFileDir(FileName)), TaskFolder));

          // Task exists?
          if Succeeded(TaskFolder.GetTask(PChar('\'+ ExtractFileName(FileName)), NewTask)) then
            Continue;

          XmlTask.LoadFromFile(ExtractDir +'\'+ FileName, TEncoding.Unicode);

          // Register the task
          OleCheck(TaskFolder.RegisterTask(PChar(ExtractFileName(FileName)),
            PChar(XmlTask.Text), TASK_CREATE, Null, Null, TASK_LOGON_INTERACTIVE_TOKEN,
            Null, NewTask));

          // Add new task to list
          if (AddTaskItem(NewTask) = -1) then
            raise ETaskException.Create('Task could not be added!');

          Result := True;
        end;  //of for

        ZipFile.Close();

        // Delete the temporary folder
        TDirectory.Delete(ExtractDir, True);

      finally
        ZipFile.Free;
        XmlTask.Free;
      end;  //of try

    finally
      FExportLock.Release();
      RevertWow64FsRedirection(OldValue);
    end;  //of try

  finally
    FSearchLock.Release();
  end;  //of try
end;

procedure TTaskList.Search(AExpertMode: Boolean = False; AWin64: Boolean = True);

  procedure LoadSubTasks(const APath: string);
  var
    FolderCollection: ITaskFolderCollection;
    Folders: IEnumVariant;
    TaskFolder: ITaskFolder;
    FolderItem: OleVariant;
    Fetched: DWORD;
    TaskCollection: IRegisteredTaskCollection;
    Task: IRegisteredTask;
    Tasks: IEnumVariant;
    TaskItem: OleVariant;
    Flags: LONG;

  begin
    // Open current folder
    OleCheck(FTaskService.GetFolder(PChar(APath), TaskFolder));

    // Show hidden?
    if AExpertMode then
      Flags := TASK_ENUM_HIDDEN
    else
      Flags := 0;

    // Add tasks in current folder
    OleCheck(TaskFolder.GetTasks(Flags, TaskCollection));
    Tasks := (TaskCollection._NewEnum as IEnumVariant);

    // Add tasks to list
    while (Tasks.Next(1, TaskItem, Fetched) = S_OK) do
    begin
      Task := (IDispatch(TaskItem) as IRegisteredTask);
      AddTaskItem(Task);
    end;  //of while

    // Include subfolders?
    if AExpertMode then
    begin
      // Read subfolders
      OleCheck(TaskFolder.GetFolders(0, FolderCollection));
      Folders := (FolderCollection._NewEnum as IEnumVariant);

      // Search for tasks in subfolders
      while (Folders.Next(1, FolderItem, Fetched) = S_OK) do
      begin
        TaskFolder := (IDispatch(FolderItem) as ITaskFolder);
        LoadSubTasks(TaskFolder.Path);
      end;  //of while
    end;  //of begin
  end;

begin
  LoadSubTasks('\');
end;

end.

