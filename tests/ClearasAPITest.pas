unit ClearasAPITest;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  TestFramework, Windows, Classes, Registry, SysUtils, ShellAPI, Forms, WinSvc,
  ClearasAPI;

const
  cTestExe         = 'C:\Windows\regedit.exe';
  cTestExeErasable = 'C:\X.exe';

type
  TRootListTest = class(TTestCase)
  const
    cNewTestExe      = 'C:\Windows\notepad.exe';
    cNewTestArgument = '-o';
    cNewTestFileName = cNewTestExe +' '+ cNewTestArgument;
  strict private
    FLockingSuccessful: Boolean;
    FErrorMessage: string;
    procedure TestLocking_SearchStart(Sender: TObject);
    procedure TestLocking_SearchError(Sender: TObject; const AErrorMessage: string);
    procedure EnsureFileExportedAndDelete(const AFileName: string);
  protected
    FRootList: TRootList<TRootItem>;
    FTestItems,
    FErasableTestItems: TStringList;
    procedure LoadItems(); virtual; abstract;
    function GetItemForName(const AItemName: string): TRootItem;
    procedure TestDisable(const AItemName: string);
    procedure TestEnable(const AItemName: string);
    procedure TestDelete(const AItemName: string);
    procedure TestExport(const AItemName: string);
    procedure TestRename(const AItemName: string); virtual;
    function TestChangeFilePath(const AItemName, AExpectedFilePath,
      ANewFilePath: string): TRootItem;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CleanUp; virtual;
  published
    procedure AddEnabledTestItems; virtual; abstract;
    // TODO: Test adding new item
    //procedure TestAddItem; virtual; abstract;
    procedure TestDisableItems;
    procedure TestEnableItems;
    procedure TestRenameItems; virtual;
    procedure TestChangeItemFilePaths; virtual;
    procedure TestExportBackup;
    procedure TestExportItems;
    procedure TestDeleteItems;
    procedure TestLocking;
  end;

  TStartupListTest = class(TRootListTest)
  private
    procedure AddTestItemEnabled(ALocation: TStartupLocation;
      AErasable: Boolean = False);
    procedure DeleteTestItem(ALocation: TStartupLocation);
    function GetItemName(ALocation: TStartupLocation;
      AErasable: Boolean = False): string;
    procedure ImportUserBackup;
  protected
    procedure LoadItems(); override;
  public
    procedure SetUp; override;
    procedure CleanUp; override;
  published
    procedure TestImportBackup;
    procedure AddEnabledTestItems; override;
  end;

  TContextListTest = class(TRootListTest)
  const
    cShellFileExt             = '.789';
    cShellFileExtErasable     = '.788';
    cShellExGUID              = '{C9BD3A62-5743-4102-892C-62381FD93E3F}';
    cShellExGUIDErasable      = '{8AF5271C-9179-4703-8D88-9484739AC0C9}';
    cShellCMItem              = 'ShellTest';
    cShellCMItemErasable      = cShellCMItem +' (erasable)';
    cShellCMItemCascading     = 'ShellCascadingTest';
    cShellCMCascadingSubItem1 = 'ShellCascadingItemTest1';
    cShellCMCascadingSubItem2 = 'ShellCascadingItemTest2';
    cShellExCMItem            = 'ShellExTest';
    cShellExCMItemErasable    = cShellExCMItem +' (erasable)';
    cShellNewCMItem           = 'ShellNewTest';
  private
    procedure AddShellCMTestItem(const AFileExt, AName, ACaption, AIcon,
      AFileName: string);
    procedure AddShellCascadingCMTestItem(const AFileExt, AName, ACaption, AIcon,
      AFileName: string);
    procedure AddShellExCMTestItem(const AFileExt, AName, ACaption, AGuid,
      AFileName: string);
    procedure AddShellNewCMTestItem(const AFileExt, AName, ACaption, AIcon,
      AFileName: string);
  protected
    procedure LoadItems(); override;
    procedure TestRename(const AItemName: string); override;
  public
    procedure SetUp; override;
    procedure CleanUp; override;
  published
    procedure AddEnabledTestItems; override;
    procedure TestChangeItemFilePaths; override;
    procedure TestRenameItems; override;
  end;

  TServiceListTest = class(TRootListTest)
  const
    cService         = 'TestService';
    cServiceErasable = cService +' (erasable)';
  private
    procedure LoadService(const AName: string);
  protected
    procedure LoadItems(); override;
    procedure TestRename(const AItemName: string); override;
  public
    procedure SetUp; override;
  published
    procedure AddEnabledTestItems; override;
  end;

  TTaskListTest = class(TRootListTest)
  protected
    procedure LoadItems(); override;
  public
    procedure SetUp; override;
  published
    procedure AddEnabledTestItems; override;
  end;

implementation

type
  // Helper class needed becaue UpdateActions() is protected
  TMyCustomForm = class(TCustomForm);

function GetTickCount64(): UInt64; stdcall; external kernel32 name 'GetTickCount64' delayed;

procedure Delay(AMilliseconds: Cardinal);
var
  FirstTickCount: UInt64;

begin
  FirstTickCount := GetTickCount64();

  while (GetTickCount64() < FirstTickCount + AMilliseconds) do
  begin
    Sleep(5);

    if ((GetCurrentThreadID() = MainThreadID) and Assigned(Application)) then
    begin
      Application.ProcessMessages();

      // Also process update actions
      if Assigned(Screen.ActiveCustomForm) then
        TMyCustomForm(Screen.ActiveCustomForm).UpdateActions();

      CheckSynchronize();
    end;  //of if
  end;  //of while
end;

procedure ImportRegistryFile(const AFileName: TFileName);
begin
  ShellExecute(0, 'open', 'regedit.exe', PChar('-s '+ AFileName), PChar(ExtractFileDir(Application.ExeName)), SW_SHOWNORMAL);
end;

{ TestRootList }

procedure TRootListTest.SetUp;
begin
  inherited SetUp;
  FTestItems := TStringList.Create;
  FErasableTestItems := TStringList.Create;
end;

procedure TRootListTest.TearDown;
begin
  FreeAndNil(FRootList);
  FreeAndNil(FErasableTestItems);
  FreeAndNil(FTestItems);
  inherited TearDown;
end;

procedure TRootListTest.CleanUp;
begin
  // Nothing to clean up here!
end;

procedure TRootListTest.EnsureFileExportedAndDelete(const AFileName: string);
var
  SearchResult: TSearchRec;

begin
  CheckNotEquals('', AFileName, 'FileName of exported file must not be empty!');
  CheckEquals(0, FindFirst(AFileName, faAnyFile - faDirectory, SearchResult), 'Exported file "'+ AFileName +'" does not exist');
  Check(DeleteFile(ExtractFilePath(AFileName) + SearchResult.Name), 'Exported file "'+ ExtractFileDir(AFileName) + SearchResult.Name +'" could not be deleted!');
  FindClose(SearchResult);
end;

function TRootListTest.GetItemForName(const AItemName: string): TRootItem;
var
  Index: Integer;

begin
  CheckNotEquals(0, FRootList.Count, 'List is empty: Load() was not called');
  Index := FRootList.IndexOf(AItemName);
  CheckNotEquals(-1, Index, 'Item "'+ AItemName +'" could not be found');
  Result := FRootList[Index];
end;

function TRootListTest.TestChangeFilePath(const AItemName, AExpectedFilePath,
  ANewFilePath: string): TRootItem;
begin
  Result := GetItemForName(AItemName);
  CheckEqualsString(AExpectedFilePath, Result.FileNameOnly, 'FileName of "'+ AItemName +'" does not match before changing file path');
  FRootList.ChangeItemFilePath(Result, ANewFilePath);
  CheckEqualsString(ANewFilePath, Result.Command, 'FileName of "'+ AItemName +'" does not match after changing file path');
end;

procedure TRootListTest.TestChangeItemFilePaths;
var
  i, ErasableItems: Integer;
  SelectedItem: TRootItem;

begin
  LoadItems();

  for i := 0 to FTestItems.Count - 1 do
  begin
    SelectedItem := TestChangeFilePath(FTestItems[i], cTestExe, cNewTestFileName);
    CheckEqualsString(cNewTestArgument, SelectedItem.Command.ExtractArguments, 'Arguments of "'+ FTestItems[i] +'" does not match after changing file path');
    CheckEqualsString(cNewTestExe, SelectedItem.FileNameOnly, 'FileNameOnly of "'+ FTestItems[i] +'" does not match after changing file path');
  end;  //of for

  // Turn erasable items to normal items
  ErasableItems := FRootList.ErasableItemsCount;
  CheckEquals(FErasableTestItems.Count, ErasableItems, 'ErasableItemsCount differs from erasable items list count');

  for i := 0 to FErasableTestItems.Count - 1 do
  begin
    SelectedItem := TestChangeFilePath(FErasableTestItems[i], cTestExeErasable, cNewTestFileName);
    CheckEqualsString(cNewTestArgument, SelectedItem.Command.ExtractArguments, 'Arguments of "'+ FErasableTestItems[i] +'" does not match after changing file path');
    CheckEqualsString(cNewTestExe, SelectedItem.FileNameOnly, 'FileNameOnly of "'+ FErasableTestItems[i] +'" does not match after changing file path');
  end;  //of for

  CheckEquals(0, FRootList.ErasableItemsCount, 'After changing file paths of erasable items to a valid path ErasableItemsCount differs from expected');

  // Turn normal items to erasable items back
  for i := 0 to ErasableItems - 1 do
    TestChangeFilePath(FErasableTestItems[i], cNewTestExe, cTestExeErasable);

  CheckEquals(ErasableItems, FRootList.ErasableItemsCount, 'After changing file paths of normal items back to erasable ErasableItemsCount differs from expected');
end;

procedure TRootListTest.TestDelete(const AItemName: string);
var
  SelectedItem: TRootItem;
  Counter, EnabledCounter: Integer;

begin
  SelectedItem := GetItemForName(AItemName);
  Counter := FRootList.Count;
  EnabledCounter := FRootList.EnabledItemsCount;

  if SelectedItem.Enabled then
    Dec(EnabledCounter);

  CheckTrue(FRootList.DeleteItem(SelectedItem), 'Item "'+ AItemName +'" was not deleted!');
  Dec(Counter);
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After deleting item "'+ AItemName +'" EnabledItemsCount should be equal to EnabledCounter');
  CheckEquals(Counter, FRootList.Count, 'After deleting item "'+ AItemName +'" Count should be decreased by 1');
end;

procedure TRootListTest.TestDeleteItems;
var
  i, ErasableItems, DeletedItems: Integer;

begin
  LoadItems();
  ErasableItems := FRootList.ErasableItemsCount;
  CheckEquals(FErasableTestItems.Count, ErasableItems, 'ErasableItemsCount differs from expected');

  for i := 0 to FTestItems.Count - 1 do
    TestDelete(FTestItems[i]);

  DeletedItems := 0;

  // Delete eraseble items
  for i := FRootList.Count - 1 downto 0 do
  begin
    if (FRootList[i].Erasable and FRootList[i].Delete()) then
    begin
      FRootList.Delete(i);
      Inc(DeletedItems);
    end;  //of begin
  end;  //of for

  CheckEquals(ErasableItems, DeletedItems, 'Count of erasable marked items differs from deleted items count');
  CheckEquals(0, FRootList.ErasableItemsCount, 'After deleting all erasable items ErasableItemsCount must be 0');
  CleanUp();
end;

procedure TRootListTest.TestDisable(const AItemName: string);
var
  SelectedItem: TRootItem;
  Counter, EnabledCounter: Integer;

begin
  SelectedItem := GetItemForName(AItemName);
  Counter := FRootList.Count;
  EnabledCounter := FRootList.EnabledItemsCount;
  CheckTrue(SelectedItem.Enabled, 'Before disabling item "'+ AItemName +'" Enabled must be True');
  FRootList.DisableItem(SelectedItem);
  CheckFalse(SelectedItem.Enabled, 'After disabling item "'+ AItemName +'" Enabled should also be False');
  Dec(EnabledCounter);
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After disabling item "'+ AItemName +'" EnabledItemsCount must be decreased by 1');
  CheckEquals(Counter, FRootList.Count, 'After disabling item "'+ AItemName +'" Count must not be changed');
end;

procedure TRootListTest.TestDisableItems;
var
  i: Integer;

begin
  LoadItems();

  for i := 0 to FTestItems.Count - 1 do
    TestDisable(FTestItems[i]);
end;

procedure TRootListTest.TestEnable(const AItemName: string);
var
  SelectedItem: TRootItem;
  Counter, EnabledCounter: Integer;

begin
  SelectedItem := GetItemForName(AItemName);
  Counter := FRootList.Count;
  EnabledCounter := FRootList.EnabledItemsCount;
  CheckFalse(SelectedItem.Enabled, 'Before enabling item "'+ AItemName +'" Enabled must be False');
  FRootList.EnableItem(SelectedItem);
  CheckTrue(SelectedItem.Enabled, 'After enabling item "'+ AItemName +'" Enabled should be True');
  Inc(EnabledCounter);
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After enabling item "'+ AItemName +'" EnabledItemsCount must be increased by 1');
  CheckEquals(Counter, FRootList.Count, 'After enabling item "'+ AItemName +'" Count must not be changed');
end;

procedure TRootListTest.TestEnableItems;
var
  i: Integer;

begin
  LoadItems();

  if not CheckWin32Version(6, 2) then
  begin
    TStartupList(FRootList).LoadDisabled(False);
    TStartupList(FRootList).LoadDisabled(True);
  end;  //of begin

  for i := 0 to FTestItems.Count - 1 do
    TestEnable(FTestItems[i]);
end;

procedure TRootListTest.TestExportBackup;
begin
  LoadItems();
  FRootList.ExportList(ClassName + FRootList.GetBackupExtension());
  EnsureFileExportedAndDelete(ClassName + FRootList.GetBackupExtension());
end;

procedure TRootListTest.TestExportItems;
var
  i: Integer;

begin
  LoadItems();

  for i := 0 to FTestItems.Count - 1 do
    TestExport(FTestItems[i]);
end;

procedure TRootListTest.TestExport(const AItemName: string);
var
  SelectedItem: TRootItem;

begin
  SelectedItem := GetItemForName(AItemName);
  FRootList.ExportItem(SelectedItem, SelectedItem.Name + SelectedItem.GetBackupExtension());
  EnsureFileExportedAndDelete(SelectedItem.Name + SelectedItem.GetBackupExtension());
end;

procedure TRootListTest.TestLocking_SearchError(Sender: TObject;
  const AErrorMessage: string);
begin
  FErrorMessage := AErrorMessage;
end;

procedure TRootListTest.TestLocking_SearchStart(Sender: TObject);
begin
  try
    // This must not be possible e.g. during loading!
    FRootList.EnableItem(nil);

  except
    on E: EListBlocked do
      FLockingSuccessful := True;
  end;  //of try
end;

procedure TRootListTest.TestLocking;
var
  SearchThread: TSearchThread;

begin
  FLockingSuccessful := False;

  // Start async loading
  SearchThread := TSearchThread.Create(FRootList);

  with SearchThread do
  begin
    FreeOnTerminate := False;
    OnStart := TestLocking_SearchStart;
    OnError := TestLocking_SearchError;
    Start();
  end;

  // Wait for the thread
  SearchThread.WaitFor();
  FreeAndNil(SearchThread);
  CheckSynchronize();

  CheckEqualsString('', FErrorMessage, FErrorMessage);
  Check(FLockingSuccessful, 'List was not locked!');
end;

procedure TRootListTest.TestRename(const AItemName: string);
var
  SelectedItem: TRootItem;

begin
  SelectedItem := GetItemForName(AItemName);
  FRootList.RenameItem(SelectedItem, AItemName +'2');
  CheckEquals(AItemName +'2', SelectedItem.Name, 'Item was not renamed correctly');
  FRootList.RenameItem(SelectedItem, AItemName);
  CheckEquals(AItemName, SelectedItem.Name, 'Item was not renamed correctly twice');
end;

procedure TRootListTest.TestRenameItems;
var
  i: Integer;

begin
  LoadItems();

  for i := 0 to FTestItems.Count - 1 do
    TestRename(FTestItems[i]);
end;


{ TStartupListTest }

procedure TStartupListTest.SetUp;
var
  Location: TStartupLocation;

begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TStartupList.Create);

  for Location := Low(TStartupLocation) to High(TStartupLocation) do
  begin
  {$IFDEF DEBUG}
    if not (Location in [slHkcuRun, slHkcuRunOnce, slStartupUser]) then
      Continue;
  {$ENDIF}
    // 32 bit OS
    if (TOSVersion.Architecture = arIntelX86) and (Location in [slHklmRun32, slHklmRunOnce32]) then
      Continue;

    FTestItems.Append(GetItemName(Location));
    FErasableTestItems.Append(GetItemName(Location, True));
  end;  //of for
end;

procedure TStartupListTest.ImportUserBackup;
begin
  Check(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slStartupUser) +  TStartupUserItem.FileExtensionStartupUser), 'Startup User file already exists!');
end;

procedure TStartupListTest.TestImportBackup;
begin
  ImportUserBackup;
  CheckException(ImportUserBackup, EAlreadyExists, 'Startup User file already exists so it must not be possible to import it again!');
{$IFNDEF DEBUG}
  Check(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slCommonStartup) + TStartupUserItem.FileExtensionStartupCommon), 'Startup Common file already exists!');
  CheckEquals(2, FRootList.Count, 'After importing 2 startup backup files there should be 2 items in the list');
  TestDelete(GetItemName(slCommonStartup));
{$ELSE}
  CheckEquals(1, FRootList.Count, 'After importing 1 startup backup file there should be 1 items in the list');
{$ENDIF}
  TestDelete(GetItemName(slStartupUser));
end;

procedure TStartupListTest.AddEnabledTestItems();
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
  begin
    AddTestItemEnabled(Location);
    AddTestItemEnabled(Location, True);
  end;  //of for
end;

procedure TStartupListTest.AddTestItemEnabled(ALocation: TStartupLocation;
  AErasable: Boolean = False);
var
  Reg: TRegistry;
  LnkFile: TLnkFile;
  ItemName, ExeName: string;

begin
{$IFDEF DEBUG}
  // Skip startup locations that need admin access rights in debug configuration only
  if (ALocation in [slHklmRun..slHklmRunOnce32, slCommonStartup]) then
    Exit;
{$ENDIF}
  ItemName := GetItemName(ALocation);

  if AErasable then
  begin
    if (ALocation in [slStartupUser, slCommonStartup]) then
      ItemName := ChangeFileExt(ItemName, '') +' (erasable)'+ ExtractFileExt(ItemName)
    else
      ItemName := ItemName +' (erasable)';

    ExeName := cTestExeErasable
  end  //of begin
  else
    ExeName := cTestExe;

  if (ALocation in [slStartupUser, slCommonStartup]) then
  begin
    LnkFile := TLnkFile.Create(ALocation.GetLocation().Value + ItemName);

    try
      with LnkFile do
      begin
        ExeFileName := ExeName;
        Arguments := '-s';
      end;  //of with

      CheckTrue(LnkFile.Save(), 'Could not save .lnk file!');

    finally
      LnkFile.Free;
    end;  //of try
  end  //of begin
  else
  begin
    // 32 bit OS
    if (TOSVersion.Architecture = arIntelX86) and (ALocation in [slHklmRun32, slHklmRunOnce32]) then
      Exit;

    if (ALocation in [slHklmRun32, slHklmRunOnce32]) then
      Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
    else
      Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

    try
      Reg.RootKey := ALocation.GetLocation().Key;
      Reg.OpenKey(ALocation.GetLocation().Value, False);
      Reg.WriteString(ItemName, ExeName);
      CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try
  end;
end;

procedure TStartupListTest.LoadItems();
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);

  CheckEquals(FErasableTestItems.Count, FRootList.ErasableItemsCount, 'Count of erasable items differs from expected');
end;

procedure TStartupListTest.CleanUp;
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    DeleteTestItem(Location);
end;

procedure TStartupListTest.DeleteTestItem(ALocation: TStartupLocation);
var
  Reg: TRegistry;
  LnkFile: TLnkFile;

begin
  if (ALocation in [slStartupUser, slCommonStartup]) then
  begin
    LnkFile := TLnkFile.Create(ALocation.GetLocation().Value + GetItemName(ALocation));

    try
      with LnkFile do
      begin
        ExeFileName := cTestExe;
        Arguments := '-s';
      end;  //of with

      if LnkFile.Exists() then
        CheckTrue(LnkFile.Delete(), 'Could not delete .lnk file!');

    finally
      LnkFile.Free;
    end;  //of try
  end  //of begin
  else
  begin
    // 32 bit OS
    if (TOSVersion.Architecture = arIntelX86) and (ALocation in [slHklmRun32, slHklmRunOnce32]) then
      Exit;

    if (ALocation in [slHklmRun32, slHklmRunOnce32]) then
      Reg := TRegistry.Create(KEY_WOW64_32KEY or KEY_READ or KEY_WRITE)
    else
      Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

    try
      Reg.RootKey := ALocation.GetLocation().Key;
      Reg.OpenKey(ALocation.GetLocation().Value, False);

      if Reg.ValueExists(GetItemName(ALocation)) then
      begin
        CheckTrue(Reg.DeleteValue(GetItemName(ALocation)), 'Could not delete Registry value: '+ GetItemName(ALocation) +'!');
        CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);
      end;  //of begin

      Reg.CloseKey();

      if not CheckWin32Version(6, 2) then
      begin
        // Delete item from disabled location (prior to Windows 7)
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        Reg.OpenKey(TStartupItem.DisabledKey, False);
        Reg.DeleteKey(GetItemName(ALocation));
      end  //of begin
      else
      begin
        // Delete item from approved location (since Windows 8)
        Reg.RootKey := ALocation.GetApprovedLocation().Key;
        Reg.OpenKey(ALocation.GetApprovedLocation().Value, False);
        Reg.DeleteValue(GetItemName(ALocation));
      end;  //of if

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try
  end;
end;

function TStartupListTest.GetItemName(ALocation: TStartupLocation;
  AErasable: Boolean = False): string;

  function GetName(const AName: string; AErasable: Boolean): string;
  begin
    if AErasable then
      Result := AName +' (erasable)'
    else
      Result := AName;
  end;

begin
  case ALocation of
    slHkcuRun:       Result := GetName('HKCU', AErasable);
    slHkcuRunOnce:   Result := GetName('HKCU RunOnce', AErasable);
    slHklmRun:       Result := GetName('HKLM', AErasable);
    slHklmRun32:     Result := GetName('HKLM32', AErasable);
    slHklmRunOnce:   Result := GetName('HKLM RunOnce', AErasable);
    slHklmRunOnce32: Result := GetName('HKLM RunOnce32', AErasable);
    slStartupUser:   Result := GetName('Startup User', AErasable) + TLnkFile.FileExtension;
    slCommonStartup: Result := GetName('Startup Common', AErasable) + TLnkFile.FileExtension;
  end;  //of case
end;


{ TContextListTest }

procedure TContextListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TContextMenuList.Create);
  FRootList.Duplicates := True;

  FTestItems.Append(cShellCMItem);
  FTestItems.Append(cShellCMItemCascading);
  FTestItems.Append(cShellNewCMItem);
  FTestItems.Append(cShellExCMItem);

  FErasableTestItems.Append(cShellCMItemErasable);
  FErasableTestItems.Append(cShellExCMItemErasable);
end;

procedure TContextListTest.AddEnabledTestItems;
begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Check(False, 'Test must be run with admin access rights!');
{$ENDIF}

  AddShellCMTestItem(cShellFileExt, cShellCMItem, cShellCMItem, cTestExe, cTestExe);
  AddShellCascadingCMTestItem(cShellFileExt, cShellCMItemCascading, cShellCMItemCascading,
    cTestExe, cTestExe);
  AddShellExCMTestItem(cShellFileExt, cShellExCMItem, cShellExCMItem,
    cShellExGUID, cTestExe);
  AddShellNewCMTestItem(cShellFileExt, cShellNewCMItem, cShellNewCMItem, cTestExe, cTestExe);

  // Erasable items do not have a corresponding file
  // NOTE: Only Shell and ShellEx can be erasable!
  AddShellCMTestItem(cShellFileExtErasable, cShellCMItemErasable, cShellCMItemErasable,
    cTestExeErasable, cTestExeErasable);
  AddShellExCMTestItem(cShellFileExtErasable, cShellExCMItemErasable, cShellExCMItemErasable,
    cShellExGUIDErasable, cTestExeErasable);
end;

procedure TContextListTest.AddShellCascadingCMTestItem(const AFileExt, AName,
  ACaption, AIcon, AFileName: string);
var
  Reg: TRegistry;
  ItemName: string;

  procedure CreateMenuItem(const AName, ACaption, AFileName: string);
  begin
    Reg.CloseKey;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_COMMAND_STORE +'\'+ AName, True);
    Reg.WriteString('', ACaption);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

    Reg.CloseKey;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_COMMAND_STORE +'\'+ AName +'\command', True);
    Reg.WriteString('', AFileName);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);
  end;

begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Exit;
{$ENDIF}

  Check(AFileExt.StartsWith('.'), 'FileExt must start with a "."!');
  CheckNotEqualsString('', AName, 'Name must not be empty');
  CheckNotEqualsString('', AFileName, 'FileName must not be empty');
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AFileExt +'\shell\'+ AName, True);

    if (ACaption <> '') then
      Reg.WriteString('MUIVerb', ACaption);

    if (AIcon <> '') then
      Reg.WriteString('Icon', AIcon);

    ItemName := AName +'.Test.Item';
    Reg.WriteString('SubCommands', cShellCMCascadingSubItem1 +';'+ cShellCMCascadingSubItem2);
    CreateMenuItem(cShellCMCascadingSubItem1, cShellCMCascadingSubItem1, AFileName +' -a');
    CreateMenuItem(cShellCMCascadingSubItem2, cShellCMCascadingSubItem2, AFileName +' -b');

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextListTest.AddShellCMTestItem(const AFileExt, AName, ACaption,
  AIcon, AFileName: string);
var
  Reg: TRegistry;

begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Exit;
{$ENDIF}

  Check(AFileExt.StartsWith('.'), 'FileExt must start with a "."!');;
  CheckNotEqualsString('', AName, 'Name must not be empty!');
  CheckNotEqualsString('', AFileName, 'FileName must not be empty!');
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AFileExt +'\shell\'+ AName, True);

    if (ACaption <> '') then
      Reg.WriteString('', ACaption);

    if (AIcon <> '') then
      Reg.WriteString('Icon', AIcon);

    Reg.CloseKey;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AFileExt +'\shell\'+ AName +'\command', True);
    Reg.WriteString('', AFileName);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextListTest.AddShellExCMTestItem(const AFileExt, AName, ACaption,
  AGuid, AFileName: string);
var
  Reg: TRegistry;

begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Exit;
{$ENDIF}

  Check(AFileExt.StartsWith('.'), 'FileExt must start with a "."!');
  CheckNotEqualsString('', AName, 'Name must not be empty');
  CheckNotEqualsString('', AFileName, 'FileName must not be empty');
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AFileExt +'\'+ TContextMenuShellExItem.HandlersKey +'\'+ AName, True);
    Reg.WriteString('', AGuid);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

    Reg.CloseKey;
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if (ACaption <> '') then
    begin
      Reg.OpenKey('CLSID\'+ AGuid, True);
      Reg.WriteString('', ACaption);
      CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);
      Reg.CloseKey;
    end;

    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('CLSID\'+ AGuid +'\InprocServer32', True);
    Reg.WriteString('', AFileName);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextListTest.AddShellNewCMTestItem(const AFileExt, AName, ACaption,
  AIcon, AFileName: string);
var
  Reg: TRegistry;

begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Exit;
{$ENDIF}

  Check(AFileExt.StartsWith('.'), 'FileExt must start with a "."!');
  CheckNotEqualsString('', AFileName, 'FileName must not be empty');
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AFileExt, True);
    Reg.WriteString('', ACaption);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

    Reg.CloseKey;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AName, True);
    Reg.WriteString('', ACaption);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

    Reg.CloseKey;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AName +'\DefaultIcon', True);
    Reg.WriteString('', AIcon);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

    Reg.CloseKey;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AName +'\shell\open\command', True);
    Reg.WriteString('', AFileName);
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

    Reg.CloseKey;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(AFileExt +'\'+ TContextMenuShellNewItem.CanonicalName, True);
    Reg.WriteExpandString('ItemName', '@%systemroot%\system32\mspaint.exe,-59414');
    Reg.WriteString('NullFile', '');
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextListTest.LoadItems();
begin
  TContextMenuList(FRootList).LoadContextmenu(cShellFileExt, False);
  TContextMenuList(FRootList).LoadContextmenu(cShellFileExtErasable, False);
  CheckEquals(FErasableTestItems.Count, FRootList.ErasableItemsCount, 'Count of erasable items differs from expected');
end;

procedure TContextListTest.TestChangeItemFilePaths;
begin
  // NOTE: Changing the filename of a cascading shell and shell new items is not possible!
  FTestItems.Clear;
  FTestItems.Append(cShellCMItem);
  FTestItems.Append(cShellExCMItem);
  inherited TestChangeItemFilePaths;
end;

procedure TContextListTest.TestRename(const AItemName: string);
var
  SelectedItem: TRootItem;

begin
  SelectedItem := GetItemForName(AItemName);
  FRootList.RenameItem(SelectedItem, AItemName +'2');

  // NOTE: Renaming a contextmenu item changes the caption not the name!!!
  CheckEquals(AItemName +'2', SelectedItem.Caption, 'Item was not renamed correctly');
  FRootList.RenameItem(SelectedItem, AItemName);
  CheckEquals(AItemName, SelectedItem.Caption, 'Item was not renamed correctly twice');
end;

procedure TContextListTest.TestRenameItems;
begin
  // NOTE: Renaming shellex and shell new items is not possible
  FTestItems.Clear;
  FTestItems.Append(cShellCMItem);
  FTestItems.Append(cShellCMItemCascading);
  inherited TestRenameItems;
end;

procedure TContextListTest.CleanUp;
var
  Reg: TRegistry;

begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Check(False, 'Test must be run with admin access rights!');
{$ENDIF}

  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.DeleteKey(cShellFileExt);
    Reg.DeleteKey(cShellFileExtErasable);
    Reg.DeleteKey(cShellNewCMItem);
    Reg.DeleteKey('CLSID\'+ cShellExGUID);

    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.DeleteKey(KEY_COMMAND_STORE +'\'+ cShellCMCascadingSubItem1);
    Reg.DeleteKey(KEY_COMMAND_STORE +'\'+ cShellCMCascadingSubItem2);
    FCheckCalled := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;


{ TServiceListTest }

procedure TServiceListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TServiceList.Create);
  FTestItems.Append(cService);
  FErasableTestItems.Append(cServiceErasable);
end;

procedure TServiceListTest.AddEnabledTestItems;
begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Check(False, 'Test must be run with admin access rights!');
{$ENDIF}
  Check(TServiceList(FRootList).Add(cTestExe, '', cService), 'Service already exists!');
  Check(TServiceList(FRootList).Add(cTestExeErasable, '', cServiceErasable), 'Service already exists!');
  CheckEquals(2, FRootList.Count, 'Actual item count differs from expected count');
end;

procedure TServiceListTest.LoadItems();
begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Check(False, 'Test must be run with admin access rights!');
{$ENDIF}
  LoadService(ChangeFileExt(ExtractFileName(cTestExe), ''));
  LoadService(ChangeFileExt(ExtractFileName(cTestExeErasable), ''));
  CheckEquals(FErasableTestItems.Count, FRootList.ErasableItemsCount, 'Count of erasable items differs from expected');
end;

procedure TServiceListTest.LoadService(const AName: string);
var
  Service: SC_HANDLE;

begin
  // Add test service to list
  CheckNotEquals(0, TServiceList(FRootList).Manager, 'Invalid service manager handle');
  Service := OpenService(TServiceList(FRootList).Manager, PChar(AName), SERVICE_QUERY_CONFIG);
  CheckNotEquals(0, Service, 'Invalid service handle');
  TServiceList(FRootList).LoadService(AName, Service, False);
  CloseServiceHandle(Service);
end;

procedure TServiceListTest.TestRename(const AItemName: string);
var
  SelectedItem: TRootItem;

begin
  SelectedItem := GetItemForName(AItemName);
  FRootList.RenameItem(SelectedItem, AItemName +'2');

  // NOTE: Renaming a service item changes the caption not the name!!!
  CheckEquals(AItemName +'2', SelectedItem.Caption, 'Item was not renamed correctly');
  FRootList.RenameItem(SelectedItem, AItemName);
  CheckEquals(AItemName, SelectedItem.Caption, 'Item was not renamed correctly twice');
end;


{ TTaskListTest }

procedure TTaskListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TTaskList.Create);
  FTestItems.Append('TestTask');
end;

procedure TTaskListTest.AddEnabledTestItems;
var
  TaskFileName: string;

begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Check(False, 'Test must be run with admin access rights!');
{$ENDIF}
  TaskFileName := IncludeTrailingBackslash(ExtractFileDir(ExtractFileDir(GetCurrentDir()))) +'data\'+ FTestItems[0] +'.zip';
  Check(FileExists(TaskFileName), 'Task backup file "'+ TaskFileName +'" does not exist!');
  Check(TTaskList(FRootList).ImportBackup(TaskFileName), 'Task already exists!');
  CheckFalse(TTaskList(FRootList).ImportBackup(TaskFileName), 'Task already exists so it must not be imported again!');
  CheckEquals(FTestItems.Count, FRootList.Count, 'Actual item count differs from expected count');
end;

procedure TTaskListTest.LoadItems();
begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Check(False, 'Test must be run with admin access rights!');
{$ENDIF}
  TTaskList(FRootList).Search(False);
  CheckEquals(FErasableTestItems.Count, FRootList.ErasableItemsCount, 'Count of erasable items differs from expected');
end;

initialization
  RegisterTest(TStartupListTest.Suite);
  RegisterTest(TContextListTest.Suite);
  RegisterTest(TServiceListTest.Suite);
  RegisterTest(TTaskListTest.Suite);
end.


