{ *********************************************************************** }
{                                                                         }
{ PM Code Works Registry Unit v1.0                                        }
{                                                                         }
{ Copyright (c) 2011-2017 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Registry;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Win.Registry;

type
  /// <summary>
  ///   The root <c>HKEY</c>s used in the Windows Registry.
  /// </summary>
  TRootKey = (

    /// <summary>
    ///   Unknown <c>HKEY</c>.
    /// </summary>
    rkUnknown,

    /// <summary>
    ///   HKEY_CURRENT_USER
    /// </summary>
    rkHKCU,

    /// <summary>
    ///   HKEY_LOCAL_MACHINE
    /// </summary>
    rkHKLM,

    /// <summary>
    ///   HKEY_CLASSES_ROOT
    /// </summary>
    rkHKCR,

    /// <summary>
    ///   HKEY_USERS
    /// </summary>
    rkHKU,

    /// <summary>
    ///   HKEY_CURRENT_CONFIG
    /// </summary>
    rkHKCC
  );

  TRootKeyHelper = record helper for TRootKey
    /// <summary>
    ///   Converts a <c>HKEY</c> into a <see cref="TRootKey"/>.
    /// </summary>
    /// <param name="AHKey">
    ///   The <c>HKEY</c>.
    /// </param>
    procedure FromHKey(AHKey: HKEY);

    /// <summary>
    ///   Converts a <c>HKEY</c> short string representation into a <see cref="TRootKey"/>.
    /// </summary>
    /// <param name="AShortHKey">
    ///   The <c>HKEY</c> short string representation.
    /// </param>
    procedure FromString(const AShortHKey: string);

    /// <summary>
    ///   Gets the string representation.
    /// </summary>
    /// <param name="ALongFormat">
    ///   If set to <c>True</c> the complete string representaion is returned.
    ///   Otherwise only the four main letters e.g. HKLM are returned.
    /// </param>
    /// <returns>
    ///   The string representation.
    /// </returns>
    function ToString(ALongFormat: Boolean = True): string;

    /// <summary>
    ///   Gets the <c>HKEY</c> representation.
    /// </summary>
    /// <returns>
    ///   The <c>HKEY</c>.
    /// </returns>
    function ToHKey(): HKEY;
  end;

  /// <summary>
  ///   Possible WOW64 registry redirections.
  /// </summary>
  TRegistryWow64Redirection = (

    /// <summary>
    ///   No redirection: 64 bit applications access 64 bit registry hive and
    ///   32 bit applications access 32 bit registry hive.
    /// </summary>
    rrNative,

    /// <summary>
    ///   Redirect registry access to 32 bit hive.
    /// </summary>
    rr32Bit,

    /// <summary>
    ///   Redirect registry access to 64 bit hive.
    /// </summary>
    rr64Bit
  );

  TRegistryWow64RedirectionHelper = record helper for TRegistryWow64Redirection
    /// <summary>
    ///   Gets the corresponding WOW64 registry redirection flag.
    /// </summary>
    /// <returns>
    ///   The WOW64 flag.
    /// </returns>
    function GetWow64Flag(): LongWord;
  end;

  /// <summary>
  ///   Indicates the changes that should be reported.
  /// </summary>
  TRegistryChangeNotificationFilter = (

    /// <summary>
    ///   Notify the caller if a subkey is added or deleted.
    /// </summary>
    nfChangeName,

    /// <summary>
    ///   Notify the caller of changes to the attributes of the key, such as the
    ///   security descriptor information.
    /// </summary>
    nfChangeAttributes,

    /// <summary>
    ///   Notify the caller of changes to a value of the key. This can include
    ///   adding or deleting a value, or changing an existing value.
    /// </summary>
    nfChangeLastSet,

    /// <summary>
    ///   Notify the caller of changes to the security descriptor of the key.
    /// </summary>
    nfChangeSecurity
  );

  /// <summary>
  ///   A set of filters.
  /// </summary>
  TRegistryChangeNotificationFilters = set of TRegistryChangeNotificationFilter;

  /// <summary>
  ///   Occurs when something goes wrong.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="AErrorMessage">
  ///   The error message.
  /// </param>
  TErrorEvent = procedure(Sender: TObject; const AErrorMessage: string) of object;

  /// <summary>
  ///   Listens for registry changes in a specified key.
  /// </summary>
  TRegistryChangeNotificationThread = class(TThread)
  private
    FRegistry: TRegistry;
    FCustomRegistryInstance,
    FReopenKey: Boolean;
    FKey,
    FErrorMessage: string;
    FOnChange: TNotifyEvent;
    FOnError: TErrorEvent;
    FFilter: TRegistryChangeNotificationFilters;
    FRecursive: Boolean;
    FTerminateEvent,
    FChangeEvent: THandle;
    FWow64Redirection: TRegistryWow64Redirection;
    function GetRootKey(): HKEY;
    procedure NotifyOnError();
    procedure NotifyOnChange();
    procedure SetFilter(const AFilter: TRegistryChangeNotificationFilters);
    procedure SetRecursive(const ARecursive: Boolean);
    procedure SetKey(const AKey: string);
    procedure SetRootKey(const ARootKey: HKEY);
    procedure SetWow64Redirection(const AWow64Redirection: TRegistryWow64Redirection);
    procedure RegisterListener();
  protected
    procedure Execute(); override; final;

    /// <summary>
    ///   Reopens the current key to apply property changes.
    /// </summary>
    procedure ReopenKey();
  public
    /// <summary>
    ///   Constructor for creating a <c>TRegistryChangeNotificationThread</c> instance.
    /// </summary>
    constructor Create; overload;

    /// <summary>
    ///   Constructor for creating a <c>TRegistryChangeNotificationThread</c> instance.
    /// </summary>
    /// <param name="ARegistry">
    ///   A fully initialized <c>TRegistry</c> instance. NOTE: Key must be opened
    ///   with <c>KEY_NOTIFY</c> access right.
    /// </param>
    constructor Create(ARegistry: TRegistry); overload;

    /// <summary>
    ///   Destructor for destroying a <c>TRegistryChangeNotificationThread</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Terminates the thread.
    /// </summary>
    procedure Terminate();

    /// <summary>
    ///   Gets or sets the notification filter.
    /// </summary>
    property Filter: TRegistryChangeNotificationFilters read FFilter write SetFilter;

    /// <summary>
    ///   Gets or sets the key where changes should be notified.
    /// </summary>
    property Key: string read FKey write SetKey;

    /// <summary>
    ///   Notifies about changes in the registry key.
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    /// <summary>
    ///   Occurs when something goes wrong.
    /// </summary>
    property OnError: TErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Determines if changes on subkeys should also be notified.
    /// </summary>
    /// <remarks>
    ///   When changing this property after thread has been started the
    ///   opened key in <see cref="CurrentKey"/> must be reopened to apply the
    ///   change. After that the new key handle must be assiged to
    ///   <see cref="CurrentKey"/>.
    /// </remarks>
    property Recursive: Boolean read FRecursive write SetRecursive;

    /// <summary>
    ///   Gets or sets the root key where changes should be notified.
    /// </summary>
    property RootKey: HKEY read GetRootKey write SetRootKey;

    /// <summary>
    ///   Gets or sets the used WOW64 registry redirection.
    /// </summary>
    property Wow64Redirection: TRegistryWow64Redirection read FWow64Redirection write SetWow64Redirection;
  end;

  /// <summary>
  ///   Notifies about registry changes in a specified key.
  /// </summary>
  TRegistryChangeNotifier = class(TObject)
  private
    FListener: TRegistryChangeNotificationThread;
    FCustomRegistry: TRegistry;
    FRecursive: Boolean;
    FFilter: TRegistryChangeNotificationFilters;
    FOnChange: TNotifyEvent;
    FKey: string;
    FRootKey: HKEY;
    FWow64Redirection: TRegistryWow64Redirection;
    function GetEnabled(): Boolean;
    procedure SetEnabled(const AEnabled: Boolean);
    procedure SetFilter(const AFilter: TRegistryChangeNotificationFilters);
    procedure SetRecursive(const ARecursive: Boolean);
    procedure SetKey(const AKey: string);
    procedure SetRootKey(const ARootKey: HKEY);
    procedure SetWow64Redirection(const AWow64Redirection: TRegistryWow64Redirection);
    function GetKey(): string;
    function GetRootKey(): HKEY;
  public
    /// <summary>
    ///   Constructor for creating a <c>TRegistryChangeNotification</c> instance.
    /// </summary>
    constructor Create; overload;

    /// <summary>
    ///   Constructor for creating a <c>TRegistryChangeNotificationThread</c> instance.
    /// </summary>
    /// <param name="ARegistry">
    ///   A fully initialized <c>TRegistry</c> instance. NOTE: Key must be opened
    ///   with <c>KEY_NOTIFY</c> access right.
    /// </param>
    constructor Create(ARegistry: TRegistry); overload;

    /// <summary>
    ///   Destructor for destroying a <c>TRegistryChangeNotification</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Enables or disables notifications.
    /// </summary>
    property Enabled: Boolean read GetEnabled write SetEnabled;

    /// <summary>
    ///   Gets or sets the notification filter.
    /// </summary>
    property Filter: TRegistryChangeNotificationFilters read FFilter write SetFilter;

    /// <summary>
    ///   Gets or sets the key where changes should be notified.
    /// </summary>
    property Key: string read GetKey write SetKey;

    /// <summary>
    ///   Notifies about changes in the key.
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    /// <summary>
    ///   Determines if changes on subkeys should also be notified.
    /// </summary>
    property Recursive: Boolean read FRecursive write SetRecursive;

    /// <summary>
    ///   Gets or sets the root key where changes should be notified.
    /// </summary>
    property RootKey: HKEY read GetRootKey write SetRootKey;

    /// <summary>
    ///   Gets or sets the used WOW64 registry redirection.
    /// </summary>
    property Wow64Redirection: TRegistryWow64Redirection read FWow64Redirection write SetWow64Redirection;
  end;

implementation

{ TRootKeyHelper }

const
  cShortHKeys: array[TRootKey] of string = (
    '',
    'HKCU',
    'HKLM',
    'HKCR',
    'HKU',
    'HKCC'
  );

  cLongHKeys: array[TRootKey] of string = (
    '',
    'HKEY_CURRENT_USER',
    'HKEY_LOCAL_MACHINE',
    'HKEY_CLASSES_ROOT',
    'HKEY_USERS',
    'HKEY_CURRENT_CONFIG'
  );

procedure TRootKeyHelper.FromHKey(AHKey: HKEY);
begin
  if (AHKey = HKEY_CURRENT_USER) then
    Self := rkHKCU
  else
  if (AHKey = HKEY_LOCAL_MACHINE) then
    Self := rkHKLM
  else
  if (AHKey = HKEY_CLASSES_ROOT) then
    Self := rkHKCR
  else
  if (AHKey = HKEY_USERS) then
    Self := rkHKU
  else
  if (AHKey = HKEY_CURRENT_CONFIG) then
    Self := rkHKCC
  else
    raise EArgumentException.Create('Unknown HKEY!');
end;

procedure TRootKeyHelper.FromString(const AShortHKey: string);
var
  RootKey, FoundKey: TRootKey;

begin
  FoundKey := rkUnknown;

  for RootKey := Low(cShortHKeys) to High(cShortHKeys) do
    if (cShortHKeys[RootKey] = AShortHKey) then
    begin
      FoundKey := RootKey;
      Break;
    end;  //of begin

  if (FoundKey = rkUnknown) then
    raise EArgumentException.Create('Unknown HKEY: "'+ AShortHKey +'"!');

  Self := FoundKey;
end;

function TRootKeyHelper.ToHKey(): HKEY;
begin
  case Self of
    rkHKCU: Result := HKEY_CURRENT_USER;
    rkHKLM: Result := HKEY_LOCAL_MACHINE;
    rkHKCR: Result := HKEY_CLASSES_ROOT;
    rkHKU:  Result := HKEY_USERS;
    rkHKCC: Result := HKEY_CURRENT_CONFIG;
    else    raise EArgumentException.Create('Unknown HKEY!');
  end;  //of case
end;

function TRootKeyHelper.ToString(ALongFormat: Boolean = True): string;
begin
  if ALongFormat then
    Result := cLongHKeys[Self]
  else
    Result := cShortHKeys[Self];
end;


{ TRegistryWow64RedirectionHelper }

function TRegistryWow64RedirectionHelper.GetWow64Flag(): LongWord;
begin
  case Self of
    rr32Bit: Result := KEY_WOW64_32KEY;
    rr64Bit: Result := KEY_WOW64_64KEY;
    else     Result := 0;
  end;  //of case
end;


{ TRegistryChangeNotificationThread }

constructor TRegistryChangeNotificationThread.Create;
begin
  Create(nil);
end;

constructor TRegistryChangeNotificationThread.Create(ARegistry: TRegistry);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  if Assigned(ARegistry) then
  begin
    FRegistry := ARegistry;
    FKey := FRegistry.CurrentPath;
    FCustomRegistryInstance := True;
  end  //of begin
  else
  begin
    FRegistry := TRegistry.Create;
    FReopenKey := True;
  end;  //of if

  FFilter := [nfChangeName, nfChangeAttributes, nfChangeLastSet, nfChangeSecurity];
  FChangeEvent := CreateEvent(nil, False, False, nil);
  FTerminateEvent := CreateEvent(nil, False, False, nil);
end;

destructor TRegistryChangeNotificationThread.Destroy;
begin
  if not FCustomRegistryInstance then
    FreeAndNil(FRegistry);

  CloseHandle(FTerminateEvent);
  CloseHandle(FChangeEvent);
  inherited Destroy;
end;

procedure TRegistryChangeNotificationThread.Execute();
var
  Events: array[0..1] of THandle;

begin
  try
    RegisterListener();
    Events[0] := FChangeEvent;
    Events[1] := FTerminateEvent;

    while not Terminated do
    begin
      if (WaitForMultipleObjects(Length(Events), @Events, False, INFINITE) = WAIT_OBJECT_0) then
        Synchronize(NotifyOnChange);

      if not Terminated then
        RegisterListener();
    end;  //of while

  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Synchronize(NotifyOnError);
    end;
  end;  //of try
end;

function TRegistryChangeNotificationThread.GetRootKey(): HKEY;
begin
  Result := FRegistry.CurrentKey;
end;

procedure TRegistryChangeNotificationThread.NotifyOnChange();
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRegistryChangeNotificationThread.NotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FErrorMessage);
end;

procedure TRegistryChangeNotificationThread.RegisterListener();
const
  cFilter: array[TRegistryChangeNotificationFilter] of DWORD = (
    REG_NOTIFY_CHANGE_NAME,
    REG_NOTIFY_CHANGE_ATTRIBUTES,
    REG_NOTIFY_CHANGE_LAST_SET,
    REG_NOTIFY_CHANGE_SECURITY
  );

var
  Filter: TRegistryChangeNotificationFilter;
  Flags: DWORD;

begin
  // Reopen key?
  if FReopenKey then
  begin
    FReopenKey := False;
    FRegistry.CloseKey();
    FRegistry.Access := KEY_READ or KEY_NOTIFY or FWow64Redirection.GetWow64Flag();

    if not FRegistry.OpenKey(FKey, False) then
      raise ERegistryException.Create(FRegistry.LastErrorMsg);
  end;  //of begin

  // Set filter
  Flags := 0;

  for Filter := Low(TRegistryChangeNotificationFilter) to High(TRegistryChangeNotificationFilter) do
  begin
    if (Filter in FFilter) then
      Flags := Flags or cFilter[Filter];
  end;  //of for

  // Start listening
  if (RegNotifyChangeKeyValue(FRegistry.CurrentKey, FRecursive, Flags,
    FChangeEvent, True) <> ERROR_SUCCESS) then
    RaiseLastOSError();
end;

procedure TRegistryChangeNotificationThread.ReopenKey();
begin
  if Started then
  begin
    FReopenKey := True;
    SetEvent(FTerminateEvent);
  end;  //of begin
end;

procedure TRegistryChangeNotificationThread.SetFilter(
  const AFilter: TRegistryChangeNotificationFilters);
begin
  if (FFilter <> AFilter) then
  begin
    FFilter := AFilter;
    ReopenKey();
  end;  //of begin
end;

procedure TRegistryChangeNotificationThread.SetKey(const AKey: string);
begin
  if (FKey <> AKey) then
  begin
    FKey := AKey;
    ReopenKey();
  end;  //of begin
end;

procedure TRegistryChangeNotificationThread.SetRecursive(const ARecursive: Boolean);
begin
  if (FRecursive <> ARecursive) then
  begin
    FRecursive := ARecursive;
    ReopenKey();
  end;  //of begin
end;

procedure TRegistryChangeNotificationThread.SetRootKey(const ARootKey: HKEY);
begin
  if (FRegistry.RootKey <> ARootKey) then
  begin
    FRegistry.RootKey := ARootKey;
    ReopenKey();
  end;  //of begin
end;

procedure TRegistryChangeNotificationThread.SetWow64Redirection(
  const AWow64Redirection: TRegistryWow64Redirection);
begin
  if (FWow64Redirection <> AWow64Redirection) then
  begin
    FWow64Redirection := AWow64Redirection;
    ReopenKey();
  end;  //of begin
end;

procedure TRegistryChangeNotificationThread.Terminate();
begin
  inherited Terminate;
  SetEvent(FTerminateEvent);
end;


{ TRegistryChangeNotification }

constructor TRegistryChangeNotifier.Create;
begin
  inherited Create;
  FFilter := [nfChangeName, nfChangeAttributes, nfChangeLastSet, nfChangeSecurity];
  FRootKey := HKEY_CURRENT_USER;
end;

constructor TRegistryChangeNotifier.Create(ARegistry: TRegistry);
begin
  Create;
  FCustomRegistry := ARegistry;
end;

destructor TRegistryChangeNotifier.Destroy;
begin
  Enabled := False;
  CheckSynchronize();
  inherited Destroy;
end;

function TRegistryChangeNotifier.GetEnabled(): Boolean;
begin
  Result := Assigned(FListener);
end;

function TRegistryChangeNotifier.GetKey(): string;
begin
  if Enabled then
    Result := FListener.Key
  else
    Result := FKey;
end;

function TRegistryChangeNotifier.GetRootKey(): HKEY;
begin
  if Enabled then
    Result := FListener.RootKey
  else
    Result := FRootKey;
end;

procedure TRegistryChangeNotifier.SetEnabled(const AEnabled: Boolean);
begin
  if (not AEnabled and Assigned(FListener)) then
  begin
    FListener.Terminate();
    FListener := nil;
  end  //of begin
  else
    if (AEnabled and not Assigned(FListener)) then
    begin
      // Start listening for changes
      FListener := TRegistryChangeNotificationThread.Create(FCustomRegistry);

      if not Assigned(FCustomRegistry) then
      begin
        FListener.Wow64Redirection := FWow64Redirection;
        FListener.RootKey := FRootKey;
        FListener.Key := FKey;
      end;  //of begin

      FListener.Filter := FFilter;
      FListener.Recursive := FRecursive;
      FListener.OnChange := OnChange;
      FListener.Start();
    end;  //of begin
end;

procedure TRegistryChangeNotifier.SetFilter(
  const AFilter: TRegistryChangeNotificationFilters);
begin
  if (FFilter <> AFilter) then
  begin
    FFilter := AFilter;

    if Enabled then
      FListener.Filter := AFilter;
  end;  //of begin
end;

procedure TRegistryChangeNotifier.SetKey(const AKey: string);
begin
  if (FKey <> AKey) then
  begin
    FKey := AKey;

    if Enabled then
      FListener.Key := AKey;
  end;  //of begin
end;

procedure TRegistryChangeNotifier.SetRecursive(const ARecursive: Boolean);
begin
  if (FRecursive <> ARecursive) then
  begin
    FRecursive := ARecursive;

    if Enabled then
      FListener.Recursive := ARecursive;
  end;  //of begin
end;

procedure TRegistryChangeNotifier.SetRootKey(const ARootKey: HKEY);
begin
  if (FRootKey <> ARootKey) then
  begin
    FRootKey := ARootKey;

    if Enabled then
      FListener.RootKey := ARootKey;
  end;  //of begin
end;

procedure TRegistryChangeNotifier.SetWow64Redirection(
  const AWow64Redirection: TRegistryWow64Redirection);
begin
  if (FWow64Redirection <> AWow64Redirection) then
  begin
    FWow64Redirection := AWow64Redirection;

    if Enabled then
      FListener.Wow64Redirection := AWow64Redirection;
  end;  //of begin
end;

end.
