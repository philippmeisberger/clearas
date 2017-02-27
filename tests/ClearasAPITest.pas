unit ClearasAPITest;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  TestFramework, Windows, Classes, Registry, SysUtils, ShellAPI, Forms, WinSvc,
  ClearasAPI;

const
  cTestExe          = 'C:\Windows\regedit.exe';
  cTestExeEraseable = 'C:\X.exe';

type
  TRootListTest = class(TTestCase)
  const
    cNewTestExe      = 'C:\Windows\notepad.exe';
    cNewTestArgument = '-o';
    cNewTestFileName = cNewTestExe +' '+ cNewTestArgument;
  strict private
    FLockingSuccessful: Boolean;
    procedure TestLocking_SearchStart(Sender: TObject);
    procedure EnsureFileExportedAndDelete(const AFileName: string);
  protected
    FRootList: TRootList<TRootItem>;
    FTestItems,
    FEraseableTestItems: TStringList;
    procedure LoadItems(); virtual; abstract;
    procedure SelectItem(const AItemName: string);
    procedure TestDisable(const AItemName: string);
    procedure TestEnable(const AItemName: string);
    procedure TestDelete(const AItemName: string);
    procedure TestExport(const AItemName: string);
    procedure TestRename(const AItemName: string); virtual;
    procedure TestChangeFilePath(const AItemName, AExpectedFilePath,
      ANewFilePath: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CleanUp; virtual;
  published
    procedure AddEnabledTestItems; virtual; abstract;
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
      AEraseable: Boolean = False);
    procedure DeleteTestItem(ALocation: TStartupLocation);
    function GetItemName(ALocation: TStartupLocation;
      AEraseable: Boolean = False): string;
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
    cShellFileExtEraseable    = '.788';
    cShellExGUID              = '{C9BD3A62-5743-4102-892C-62381FD93E3F}';
    cShellExGUIDEraseable     = '{8AF5271C-9179-4703-8D88-9484739AC0C9}';
    cShellCMItem              = 'ShellTest';
    cShellCMItemEraseable     = cShellCMItem +' (eraseable)';
    cShellCMItemCascading     = 'ShellCascadingTest';
    cShellCMCascadingSubItem1 = 'ShellCascadingItemTest1';
    cShellCMCascadingSubItem2 = 'ShellCascadingItemTest2';
    cShellExCMItem            = 'ShellExTest';
    cShellExCMItemEraseable   = cShellExCMItem +' (eraseable)';
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
    cService          = 'TestService';
    cServiceEraseable = cService +' (eraseable)';
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
  FEraseableTestItems := TStringList.Create;
end;

procedure TRootListTest.TearDown;
begin
  FreeAndNil(FRootList);
  FreeAndNil(FEraseableTestItems);
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

procedure TRootListTest.SelectItem(const AItemName: string);
var
  Index: Integer;

begin
  CheckNotEquals(0, FRootList.Count, 'List is empty: Load() was not called');
  Index := FRootList.IndexOf(AItemName);
  CheckNotEquals(-1, Index, 'Item "'+ AItemName +'" could not be found');
  FRootList.Selected := FRootList[Index];
end;

procedure TRootListTest.TestChangeFilePath(const AItemName, AExpectedFilePath,
  ANewFilePath: string);
begin
  SelectItem(AItemName);
  CheckEqualsString(AExpectedFilePath, FRootList.Selected.FileNameOnly, 'FileName of "'+ AItemName +'" does not match before changing file path');
  FRootList.ChangeItemFilePath(ANewFilePath);
  CheckEqualsString(ANewFilePath, FRootList.Selected.FileName, 'FileName of "'+ AItemName +'" does not match after changing file path');
end;

procedure TRootListTest.TestChangeItemFilePaths;
var
  i, EraseableItems: Integer;

begin
  LoadItems();

  for i := 0 to FTestItems.Count - 1 do
  begin
    TestChangeFilePath(FTestItems[i], cTestExe, cNewTestFileName);
    CheckEqualsString(cNewTestArgument, FRootList.Selected.Arguments, 'Arguments of "'+ FTestItems[i] +'" does not match after changing file path');
    CheckEqualsString(cNewTestExe, FRootList.Selected.FileNameOnly, 'FileNameOnly of "'+ FTestItems[i] +'" does not match after changing file path');
  end;  //of for

  // Turn eraseable items to normal items
  EraseableItems := FRootList.EraseableItemsCount;
  CheckEquals(FEraseableTestItems.Count, EraseableItems, 'EraseableItemsCount differs from eraseable items list count');

  for i := 0 to FEraseableTestItems.Count - 1 do
  begin
    TestChangeFilePath(FEraseableTestItems[i], cTestExeEraseable, cNewTestFileName);
    CheckEqualsString(cNewTestArgument, FRootList.Selected.Arguments, 'Arguments of "'+ FEraseableTestItems[i] +'" does not match after changing file path');
    CheckEqualsString(cNewTestExe, FRootList.Selected.FileNameOnly, 'FileNameOnly of "'+ FEraseableTestItems[i] +'" does not match after changing file path');
  end;  //of for

  CheckEquals(0, FRootList.EraseableItemsCount, 'After changing file paths of eraseable items to a valid path EraseableItemsCount differs from expected');

  // Turn normal items to eraseable items back
  for i := 0 to EraseableItems - 1 do
    TestChangeFilePath(FEraseableTestItems[i], cNewTestExe, cTestExeEraseable);

  CheckEquals(EraseableItems, FRootList.EraseableItemsCount, 'After changing file paths of normal items back to eraseable EraseableItemsCount differs from expected');
end;

procedure TRootListTest.TestDelete(const AItemName: string);
var
  Counter, EnabledCounter: Integer;

begin
  SelectItem(AItemName);
  Counter := FRootList.Count;
  EnabledCounter := FRootList.EnabledItemsCount;

  if FRootList.Selected.Enabled then
    Dec(EnabledCounter);

  CheckTrue(FRootList.DeleteItem(), 'Item "'+ AItemName +'" was not deleted!');
  Dec(Counter);
  CheckNull(FRootList.Selected, 'After deleting selected item Selected should not be assigned');
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After deleting item "'+ AItemName +'" EnabledItemsCount should be equal to EnabledCounter');
  CheckEquals(Counter, FRootList.Count, 'After deleting item "'+ AItemName +'" Count should be decreased by 1');
end;

procedure TRootListTest.TestDeleteItems;
var
  i, EraseableItems, DeletedItems: Integer;

begin
  LoadItems();
  EraseableItems := FRootList.EraseableItemsCount;
  CheckEquals(FEraseableTestItems.Count, EraseableItems, 'EraseableItemsCount differs from expected');

  for i := 0 to FTestItems.Count - 1 do
    TestDelete(FTestItems[i]);

  DeletedItems := 0;

  // Delete eraseable items
  for i := FRootList.Count - 1 downto 0 do
  begin
    if (FRootList[i].Eraseable and FRootList[i].Delete()) then
    begin
      FRootList.Delete(i);
      Inc(DeletedItems);
    end;  //of begin
  end;  //of for

  CheckEquals(EraseableItems, DeletedItems, 'Count of eraseable marked items differs from deleted items count');
  CheckEquals(0, FRootList.EraseableItemsCount, 'After deleting all eraseable items EraseableItemsCount must be 0');
  CleanUp();
end;

procedure TRootListTest.TestDisable(const AItemName: string);
var
  Counter, EnabledCounter: Integer;

begin
  SelectItem(AItemName);
  Counter := FRootList.Count;
  EnabledCounter := FRootList.EnabledItemsCount;
  CheckTrue(FRootList.Selected.Enabled, 'Before disabling item "'+ AItemName +'" Enabled must be True');
  FRootList.DisableItem();
  CheckFalse(FRootList.Selected.Enabled, 'After disabling item "'+ AItemName +'" Enabled should also be False');
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
  Counter, EnabledCounter: Integer;

begin
  SelectItem(AItemName);
  Counter := FRootList.Count;
  EnabledCounter := FRootList.EnabledItemsCount;
  CheckFalse(FRootList.Selected.Enabled, 'Before enabling item "'+ AItemName +'" Enabled must be False');
  FRootList.EnableItem();
  CheckTrue(FRootList.Selected.Enabled, 'After enabling item "'+ AItemName +'" Enabled should be True');
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
begin
  SelectItem(AItemName);
  FRootList.ExportItem(FRootList.Selected.Name + FRootList.Selected.GetBackupExtension());
  EnsureFileExportedAndDelete(FRootList.Selected.Name + FRootList.Selected.GetBackupExtension());
end;

procedure TRootListTest.TestLocking_SearchStart(Sender: TObject);
begin
  try
    // This must not be possible e.g. during loading!
    FRootList.EnableItem();

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
    OnStart := TestLocking_SearchStart;
    ExpertMode := True;
    Start();
  end;

  // Give the thread time to kill himself
  Delay(2000);
  Check(FLockingSuccessful, 'List was not locked!');
end;

procedure TRootListTest.TestRename(const AItemName: string);
begin
  SelectItem(AItemName);
  FRootList.RenameItem(AItemName +'2');
  CheckEquals(AItemName +'2', FRootList.Selected.Name, 'Item was not renamed correctly');
  FRootList.RenameItem(AItemName);
  CheckEquals(AItemName, FRootList.Selected.Name, 'Item was not renamed correctly twice');
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
    FEraseableTestItems.Append(GetItemName(Location, True));
  end;  //of for
end;

procedure TStartupListTest.TestImportBackup;
begin
  Check(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slStartupUser) +  TStartupLnkFile.StartupUserBackupFileExtension), 'Startup User file already exists!');
  CheckFalse(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slStartupUser) + TStartupLnkFile.StartupUserBackupFileExtension), 'Startup User file already exists so it must not be possible to import it again!');
{$IFNDEF DEBUG}
  Check(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slCommonStartup) + EXT_STARTUP_COMMON), STARTUP_COMMON +' file already exists!');
  CheckFalse(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slCommonStartup) + EXT_STARTUP_COMMON), STARTUP_COMMON +' file already exists so it must not be possible to import it again!');
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
  AEraseable: Boolean = False);
var
  Reg: TRegistry;
  LnkFile: TStartupLnkFile;
  ItemName, ExeFileName: string;

begin
{$IFDEF DEBUG}
  // Skip startup locations that need admin access rights in debug configuration only
  if (ALocation in [slHklmRun..slHklmRunOnce32, slCommonStartup]) then
    Exit;
{$ENDIF}
  ItemName := GetItemName(ALocation);

  if AEraseable then
  begin
    if (ALocation in [slStartupUser, slCommonStartup]) then
      ItemName := ChangeFileExt(ItemName, '') +' (eraseable)'+ ExtractFileExt(ItemName)
    else
      ItemName := ItemName +' (eraseable)';

    ExeFileName := cTestExeEraseable
  end  //of begin
  else
    ExeFileName := cTestExe;

  if (ALocation in [slStartupUser, slCommonStartup]) then
  begin
    LnkFile := TStartupLnkFile.Create(ItemName, (ALocation = slStartupUser), ExeFileName, '-s');

    try
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
      Reg.WriteString(ItemName, ExeFileName);
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

  CheckEquals(FEraseableTestItems.Count, FRootList.EraseableItemsCount, 'Count of eraseable items differs from expected');
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
  LnkFile: TStartupLnkFile;

begin
  if (ALocation in [slStartupUser, slCommonStartup]) then
  begin
    LnkFile := TStartupLnkFile.Create(GetItemName(ALocation), (ALocation = slStartupUser), cTestExe, '-s');

    try
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
  AEraseable: Boolean = False): string;

  function GetName(const AName: string; AEraseable: Boolean): string;
  begin
    if AEraseable then
      Result := AName +' (eraseable)'
    else
      Result := AName;
  end;

begin
  case ALocation of
    slHkcuRun:       Result := GetName('HKCU', AEraseable);
    slHkcuRunOnce:   Result := GetName('HKCU RunOnce', AEraseable);
    slHklmRun:       Result := GetName('HKLM', AEraseable);
    slHklmRun32:     Result := GetName('HKLM32', AEraseable);
    slHklmRunOnce:   Result := GetName('HKLM RunOnce', AEraseable);
    slHklmRunOnce32: Result := GetName('HKLM RunOnce32', AEraseable);
    slStartupUser:   Result := GetName('Startup User', AEraseable) +'.lnk';
    slCommonStartup: Result := GetName('Startup Common', AEraseable) +'.lnk';
  end;  //of case
end;


{ TContextListTest }

procedure TContextListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TContextList.Create);
  FRootList.Duplicates := True;

  FTestItems.Append(cShellCMItem);
  FTestItems.Append(cShellCMItemCascading);
  FTestItems.Append(cShellNewCMItem);
  FTestItems.Append(cShellExCMItem);

  FEraseableTestItems.Append(cShellCMItemEraseable);
  FEraseableTestItems.Append(cShellExCMItemEraseable);
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

  // Eraseable items do not have a corresponding file
  // NOTE: Only Shell and ShellEx can be eraseable!
  AddShellCMTestItem(cShellFileExtEraseable, cShellCMItemEraseable, cShellCMItemEraseable,
    cTestExeEraseable, cTestExeEraseable);
  AddShellExCMTestItem(cShellFileExtEraseable, cShellExCMItemEraseable, cShellExCMItemEraseable,
    cShellExGUIDEraseable, cTestExeEraseable);
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
    Reg.OpenKey(AFileExt +'\'+ CM_SHELLEX_HANDLERS +'\'+ AName, True);
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
    Reg.OpenKey(AFileExt +'\'+ TShellNewItem.CanonicalName, True);
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
  TContextList(FRootList).LoadContextmenu(cShellFileExt, False);
  TContextList(FRootList).LoadContextmenu(cShellFileExtEraseable, False);
  CheckEquals(FEraseableTestItems.Count, FRootList.EraseableItemsCount, 'Count of eraseable items differs from expected');
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
begin
  SelectItem(AItemName);
  FRootList.RenameItem(AItemName +'2');

  // NOTE: Renaming a contextmenu item changes the caption not the name!!!
  CheckEquals(AItemName +'2', FRootList.Selected.Caption, 'Item was not renamed correctly');
  FRootList.RenameItem(AItemName);
  CheckEquals(AItemName, FRootList.Selected.Caption, 'Item was not renamed correctly twice');
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
    Reg.DeleteKey(cShellFileExtEraseable);
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
  FEraseableTestItems.Append(cServiceEraseable);
end;

procedure TServiceListTest.AddEnabledTestItems;
begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Check(False, 'Test must be run with admin access rights!');
{$ENDIF}
  Check(TServiceList(FRootList).Add(cTestExe, '', cService), 'Service already exists!');
  Check(TServiceList(FRootList).Add(cTestExeEraseable, '', cServiceEraseable), 'Service already exists!');
  CheckEquals(2, FRootList.Count, 'Actual item count differs from expected count');
end;

procedure TServiceListTest.LoadItems();
begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  Check(False, 'Test must be run with admin access rights!');
{$ENDIF}
  LoadService(ChangeFileExt(ExtractFileName(cTestExe), ''));
  LoadService(ChangeFileExt(ExtractFileName(cTestExeEraseable), ''));
  CheckEquals(FEraseableTestItems.Count, FRootList.EraseableItemsCount, 'Count of eraseable items differs from expected');
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
begin
  SelectItem(AItemName);
  FRootList.RenameItem(AItemName +'2');

  // NOTE: Renaming a service item changes the caption not the name!!!
  CheckEquals(AItemName +'2', FRootList.Selected.Caption, 'Item was not renamed correctly');
  FRootList.RenameItem(AItemName);
  CheckEquals(AItemName, FRootList.Selected.Caption, 'Item was not renamed correctly twice');
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
  TTaskList(FRootList).Search(False);
  CheckEquals(FEraseableTestItems.Count, FRootList.EraseableItemsCount, 'Count of eraseable items differs from expected');
end;

initialization
  RegisterTest(TStartupListTest.Suite);
  RegisterTest(TContextListTest.Suite);
  RegisterTest(TServiceListTest.Suite);
  RegisterTest(TTaskListTest.Suite);
end.


