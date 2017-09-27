unit ClearasAPITest;

interface

uses
  TestFramework, Windows, Classes, Registry, SysUtils, ShellAPI, Forms,
  ClearasAPI;

const
  cTestExe         = 'C:\Windows\regedit.exe';
  cTestExeErasable = 'C:\X.exe';
  cTestFilesDir    = '..\..\data\';
  cErasableSuffix  = ' (erasable)';

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
    procedure TestLocking_TestDelete();
    procedure TestLocking_ExportStart(Sender: TObject);
    procedure TestLocking_ExportFinished(Sender: TObject);
    procedure EnsureFileExportedAndDelete(const AFileName: string);
  protected
    FRootList: TRootList<TRootItem>;
    FTestItems,
    FErasableTestItems: TStringList;
    function GetItemForName(const AItemName: string): TRootItem;
    procedure TestChangeStatus(const AItemName: string);
    procedure TestDelete(const AItemName: string);
    procedure TestExport(const AItemName: string);
    procedure TestRename(const AItemName: string); virtual;
    function TestChangeCommand(const AItemName, AExpectedFilePath,
      ANewFilePath: string): TRootItem;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestChangeItemStatus;
    procedure TestRenameItems; virtual;
    procedure TestChangeItemCommands; virtual;
    procedure TestExportBackup;
    procedure TestExportItems;
    procedure TestLocking;
  end;

  TStartupListTest = class(TRootListTest)
  const
    cStartupUserImported   = 'Startup User Backup.lnk';
    cStartupCommonImported = 'Startup Common Backup.lnk';
  private
    procedure AddTestItemEnabled(ALocation: TStartupLocation;
      AErasable: Boolean = False);
    function GetItemName(ALocation: TStartupLocation;
      AErasable: Boolean = False): string;
    procedure ImportUserBackup;
  public
    procedure SetUp; override;
  published
    procedure TestAddNewItem;
    procedure TestImportBackup;
  end;

  TContextListTest = class(TRootListTest)
  const
    cShellFileExt             = '.789';
    cShellFileExtErasable     = '.788';
    cShellNotErasableFileExt  = '.787';
    cShellExGUID              = '{C9BD3A62-5743-4102-892C-62381FD93E3F}';
    cShellExGUIDErasable      = '{8AF5271C-9179-4703-8D88-9484739AC0C9}';
    cShellCMItem              = 'ShellTest';
    cShellCMItemErasable      = cShellCMItem + cErasableSuffix;
    cShellCMItemCascading     = 'ShellCascadingTest';
    cShellCMCascadingSubItem1 = 'ShellCascadingItemTest1';
    cShellCMCascadingSubItem2 = 'ShellCascadingItemTest2';
    cShellExCMItem            = 'ShellExTest';
    cShellExCMItemErasable    = cShellExCMItem + cErasableSuffix;
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
    procedure TestRename(const AItemName: string); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddNewItem;
    procedure TestChangeItemCommands; override;
    procedure TestRenameItems; override;
    procedure TestNotErasableItems;
  end;

  TServiceListTest = class(TRootListTest)
  const
    cService         = 'TestService';
    cServiceErasable = cService + cErasableSuffix;
  protected
    procedure TestRename(const AItemName: string); override;
  public
    procedure SetUp; override;
  end;

  TTaskListTest = class(TRootListTest)
  const
    cTask         = 'TestTask';
    cTaskErasable = 'ErasableTask';
  public
    procedure SetUp; override;
  end;

implementation

type
  // Helper class needed becaue UpdateActions() is protected
  TMyCustomForm = class(TCustomForm);

function GetTickCount64(): UInt64; stdcall; external kernel32 name 'GetTickCount64';

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

function HasAdminAccessRights(): Boolean;
var
  TokenHandle: THandle;
  Token: TTokenElevation;
  Size: DWORD;

begin
  Result := False;

  if OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, TokenHandle) then
  begin
    if GetTokenInformation(TokenHandle, TokenElevation, @Token, SizeOf(TTokenElevation), Size) then
      Result := (Token.TokenIsElevated <> 0);

    CloseHandle(TokenHandle);
  end;  //of try
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
var
  i: Integer;

begin
  try
    // Delete valid test items
    for i := 0 to FTestItems.Count - 1 do
      TestDelete(FTestItems[i]);

    // Delete erasable items
    for i := 0 to FErasableTestItems.Count - 1 do
      TestDelete(FErasableTestItems[i]);

  finally
    FreeAndNil(FRootList);
    FreeAndNil(FErasableTestItems);
    FreeAndNil(FTestItems);
    inherited TearDown;
  end;
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

function TRootListTest.TestChangeCommand(const AItemName, AExpectedFilePath,
  ANewFilePath: string): TRootItem;
begin
  Result := GetItemForName(AItemName);
  CheckEqualsString(AExpectedFilePath, Result.Command.Expand(), 'FileName of "'+ AItemName +'" does not match before changing command');
  FRootList.ChangeCommand(Result, ANewFilePath);
  CheckEqualsString(ANewFilePath, Result.Command, 'FileName of "'+ AItemName +'" does not match after changing command');
end;

procedure TRootListTest.TestChangeItemCommands;
var
  i, ErasableItems: Integer;
  SelectedItem: TRootItem;

begin
  for i := 0 to FTestItems.Count - 1 do
  begin
    SelectedItem := TestChangeCommand(FTestItems[i], cTestExe, cNewTestFileName);
    CheckEqualsString(cNewTestArgument, SelectedItem.Command.ExtractArguments, 'Arguments of "'+ FTestItems[i] +'" does not match after changing command');
    CheckEqualsString(cNewTestExe, SelectedItem.Command.Expand(), 'Expanding of "'+ FTestItems[i] +'" does not match after changing command');
  end;  //of for

  // Turn erasable items to normal items
  ErasableItems := FRootList.ErasableItemsCount;
  Check(ErasableItems >= FErasableTestItems.Count, 'There must be at least '+ IntToStr(FErasableTestItems.Count) +' erasable items in the list');

  for i := 0 to FErasableTestItems.Count - 1 do
  begin
    SelectedItem := TestChangeCommand(FErasableTestItems[i], cTestExeErasable, cNewTestFileName);
    CheckEqualsString(cNewTestArgument, SelectedItem.Command.ExtractArguments, 'Arguments of "'+ FErasableTestItems[i] +'" does not match after changing command');
    CheckEqualsString(cNewTestExe, SelectedItem.Command.Expand(), 'Expanding of "'+ FErasableTestItems[i] +'" does not match after changing command');
    Dec(ErasableItems);
  end;  //of for

  CheckEquals(ErasableItems, FRootList.ErasableItemsCount, 'After changing commands of erasable items to a valid path ErasableItemsCount differs from expected');

  // Turn normal items to erasable items back
  for i := 0 to ErasableItems - 1 do
  begin
    TestChangeCommand(FErasableTestItems[i], cNewTestExe, cTestExeErasable);
    Inc(ErasableItems);
  end;

  CheckEquals(ErasableItems, FRootList.ErasableItemsCount, 'After changing commands of normal items back to erasable ErasableItemsCount differs from expected');
end;

procedure TRootListTest.TestDelete(const AItemName: string);
var
  SelectedItem: TRootItem;
  Counter, EnabledCounter, ErasableCounter: Integer;

begin
  SelectedItem := GetItemForName(AItemName);
  Counter := FRootList.Count;
  ErasableCounter := FRootList.ErasableItemsCount;
  EnabledCounter := FRootList.EnabledItemsCount;

  if SelectedItem.Enabled then
    Dec(EnabledCounter);

  if SelectedItem.Erasable then
    Dec(ErasableCounter);

  FRootList.DeleteItem(SelectedItem);
  Dec(Counter);
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After deleting item "'+ AItemName +'" EnabledItemsCount should be decreased');
  CheckEquals(ErasableCounter, FRootList.ErasableItemsCount, 'After deleting item "'+ AItemName +'" ErasableItemsCount should be decreased');
  CheckEquals(Counter, FRootList.Count, 'After deleting item "'+ AItemName +'" Count should be decreased');
end;

procedure TRootListTest.TestChangeStatus(const AItemName: string);

  procedure ChangeStatus(ASelectedItem: TRootItem; AEnabled: Boolean);
  var
    ExpectedCount, ExpectedErasableCount, ExpectedEnabledCount: Integer;

  begin
    ExpectedCount := FRootList.Count;
    ExpectedErasableCount := FRootList.ErasableItemsCount;
    ExpectedEnabledCount := FRootList.EnabledItemsCount;

    if AEnabled then
      Inc(ExpectedEnabledCount)
    else
      Dec(ExpectedEnabledCount);

    FRootList.ChangeItemStatus(ASelectedItem, AEnabled);
    CheckEquals(AEnabled, ASelectedItem.Enabled, 'After enabling item "'+ AItemName +'" again Enabled was not updated');
    CheckEquals(ExpectedEnabledCount, FRootList.EnabledItemsCount, 'After changing status of "'+ AItemName +'" EnabledItemsCount was not updated');
    CheckEquals(ExpectedErasableCount, FRootList.ErasableItemsCount, 'After changing status of "'+ AItemName +'" ErasableItemsCount must not be changed');
    CheckEquals(ExpectedCount, FRootList.Count, 'After changing status of "'+ AItemName +'" Count must not be changed');
  end;

var
  SelectedItem: TRootItem;

begin
  SelectedItem := GetItemForName(AItemName);
  ChangeStatus(SelectedItem, False);
  ChangeStatus(SelectedItem, True);

  // Disable again to test deletion of disabled item
  ChangeStatus(SelectedItem, False);
end;

procedure TRootListTest.TestChangeItemStatus;
var
  i: Integer;

begin
  for i := 0 to FTestItems.Count - 1 do
    TestChangeStatus(FTestItems[i]);
end;

procedure TRootListTest.TestExportBackup;
begin
  FRootList.ExportList(ClassName + FRootList.GetBackupExtension());
  EnsureFileExportedAndDelete(ClassName + FRootList.GetBackupExtension());
end;

procedure TRootListTest.TestExportItems;
var
  i: Integer;

begin
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
  TestChangeStatus(FTestItems[0]);
  TestRename(FTestItems[0]);
  TestExport(FTestItems[0]);
  TestChangeCommand(FTestItems[0], cTestExe, cNewTestFileName);
  CheckException(TestLocking_TestDelete, EListBlocked, 'Delete must not be possible while a search is pending');
  FLockingSuccessful := True;
end;

procedure TRootListTest.TestLocking_TestDelete;
begin
  TestDelete(FTestItems[0]);
end;

procedure TRootListTest.TestLocking_ExportFinished(Sender: TObject);
begin
  Check(DeleteFile(ClassName + FRootList.GetBackupExtension()), 'List was not exported');
end;

procedure TRootListTest.TestLocking_ExportStart(Sender: TObject);
begin
  CheckException(TestChangeItemStatus, EListBlocked, 'Status must not be changed while an export is pending');
  CheckException(TestRenameItems, EListBlocked, 'Command must not be changed while an export is pending');
  TestExport(FTestItems[0]);
  CheckException(TestChangeItemCommands, EListBlocked, 'Command must not be changed while an export is pending');
  CheckException(TestLocking_TestDelete, EListBlocked, 'Delete must not be possible while a search is pending');
  FLockingSuccessful := True;
end;

procedure TRootListTest.TestLocking;
var
  SearchThread: TSearchThread;
  ExportThread: TExportListThread;

begin
  FLockingSuccessful := False;

  // Start async export
  ExportThread := TExportListThread.Create(FRootList, ClassName + FRootList.GetBackupExtension(), -1);

  with ExportThread do
  begin
    FreeOnTerminate := False;
    OnStart := TestLocking_ExportStart;
    OnTerminate := TestLocking_ExportFinished;
    OnError := TestLocking_SearchError;
    Start();
  end;

  Delay(300);
  ExportThread.WaitFor();
  FreeAndNil(ExportThread);
  CheckSynchronize();
  CheckEqualsString('', FErrorMessage, FErrorMessage);
  Check(FLockingSuccessful, 'List was not locked during export');

  // Start async search
  FLockingSuccessful := False;
  SearchThread := TSearchThread.Create(FRootList);

  with SearchThread do
  begin
    FreeOnTerminate := False;
    ExpertMode := True;
    OnStart := TestLocking_SearchStart;
    OnError := TestLocking_SearchError;
    Start();
  end;

  Delay(300);
  SearchThread.WaitFor();
  FreeAndNil(SearchThread);
  CheckSynchronize();
  CheckEqualsString('', FErrorMessage, FErrorMessage);
  Check(FLockingSuccessful, 'List was not locked during search');
end;

procedure TRootListTest.TestRename(const AItemName: string);
var
  SelectedItem: TRootItem;
  NewName: string;

begin
  SelectedItem := GetItemForName(AItemName);
  NewName := ChangeFileExt(AItemName, '') +'2'+ ExtractFileExt(AItemName);
  FRootList.RenameItem(SelectedItem, NewName);
  CheckEquals(NewName, SelectedItem.Name, 'Item was not renamed correctly');
  FRootList.RenameItem(SelectedItem, AItemName);
  CheckEquals(AItemName, SelectedItem.Name, 'Item was not renamed correctly twice');
end;

procedure TRootListTest.TestRenameItems;
var
  i: Integer;

begin
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
    // Skip startup locations that need admin access rights
    if (not HasAdminAccessRights() and not (Location in [slHkcuRun, slHkcuRunOnce, slStartupUser])) then
      Continue;

    // 32 bit OS
    if (TOSVersion.Architecture = arIntelX86) and (Location in [slHklmRun32, slHklmRunOnce32]) then
      Continue;

    FTestItems.Append(GetItemName(Location));
    AddTestItemEnabled(Location);
    FErasableTestItems.Append(GetItemName(Location, True));
    AddTestItemEnabled(Location, True);
  end;  //of for

  FRootList.Search(True);
end;

procedure TStartupListTest.TestAddNewItem;
const
  cNewStartupUserCaption = 'New Startup User item';
  cNewStartupItemCaption = 'New Startup item';

var
  ExpectedCount: Integer;

begin
  // Add .exe to autostart
  ExpectedCount := FRootList.Count + 1;
  CheckTrue(TStartupList(FRootList).Add('C:\Windows\explorer.exe', cNewTestArgument, cNewStartupUserCaption), 'Item could not be added');
  CheckIs(TStartupList(FRootList).Last, TStartupUserItem, '.exe files must be added as TStartupUserItem');
  Check((TStartupList(FRootList).Last as TStartupUserItem).StartupUser, 'New item must be added to current user startup and not common startup');
  CheckEquals(ExpectedCount, FRootList.Count, 'Count was not increased');
  FTestItems.Add(cNewStartupUserCaption + TLnkFile.FileExtension);

  // Add .bat to autostart
  ExpectedCount := FRootList.Count + 1;
  CheckTrue(TStartupList(FRootList).Add(cTestFilesDir +'HKCU.bat', cNewTestArgument, cNewStartupItemCaption), 'Item could not be added');
  CheckIs(TStartupList(FRootList).Last, TStartupItem, '.bat files must be added as TStartupItem');
  CheckEquals(ExpectedCount, FRootList.Count, 'Count was not increased');
  FTestItems.Add(cNewStartupItemCaption);
end;

procedure TStartupListTest.ImportUserBackup;
begin
  Check(TStartupList(FRootList).ImportBackup(cTestFilesDir + cStartupUserImported + TStartupUserItem.FileExtensionStartupUser), 'Startup User file already exists!');
end;

procedure TStartupListTest.TestImportBackup;
var
  ExpectedCount: Integer;

begin
  ExpectedCount := FRootList.Count + 1;
  ImportUserBackup;
  FTestItems.Add(cStartupUserImported);
  CheckException(ImportUserBackup, EAlreadyExists, 'Startup User file already exists so it must not be possible to import it again!');

  // Admin access rights are needed when importing into all users location
  if HasAdminAccessRights() then
  begin
    Inc(ExpectedCount);
    Check(TStartupList(FRootList).ImportBackup(cTestFilesDir + cStartupCommonImported + TStartupUserItem.FileExtensionStartupCommon), 'Startup Common file already exists!');
    CheckEquals(ExpectedCount, FRootList.Count, 'After importing 2 startup backup files there should be 2 items in the list');
    FTestItems.Add(cStartupCommonImported);
  end  //of begin
  else
    CheckEquals(ExpectedCount, FRootList.Count, 'After importing 1 startup backup file there should be 1 items in the list');
end;

procedure TStartupListTest.AddTestItemEnabled(ALocation: TStartupLocation;
  AErasable: Boolean = False);
var
  Reg: TRegistry;
  LnkFile: TLnkFile;
  ItemName, ExeName: string;

begin
  // Skip startup locations that need admin access rights
  if (not HasAdminAccessRights() and (ALocation in [slHklmRun..slHklmRunOnce32, slCommonStartup])) then
    Exit;

  ItemName := GetItemName(ALocation);

  if AErasable then
  begin
    if (ALocation in [slStartupUser, slCommonStartup]) then
      ItemName := ChangeFileExt(ItemName, '') + cErasableSuffix + ExtractFileExt(ItemName)
    else
      ItemName := ItemName + cErasableSuffix;

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

function TStartupListTest.GetItemName(ALocation: TStartupLocation;
  AErasable: Boolean = False): string;

  function GetName(const AName: string; AErasable: Boolean): string;
  begin
    if AErasable then
      Result := AName + cErasableSuffix
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
    slHklmRunOnce32: Result := GetName('HKLM32 RunOnce', AErasable);
    slStartupUser:   Result := GetName('Startup User', AErasable) + TLnkFile.FileExtension;
    slCommonStartup: Result := GetName('Startup Common', AErasable) + TLnkFile.FileExtension;
  end;  //of case
end;


{ TContextListTest }

procedure TContextListTest.SetUp;
begin
  inherited SetUp;
  Check(HasAdminAccessRights(), 'Test must be run with admin access rights!');

  FRootList := TRootList<TRootItem>(TContextMenuList.Create);
  FTestItems.Append(cShellCMItem);
  AddShellCMTestItem(cShellFileExt, cShellCMItem, cShellCMItem, cTestExe, cTestExe);
  FTestItems.Append(cShellCMItemCascading);
  AddShellCascadingCMTestItem(cShellFileExt, cShellCMItemCascading, cShellCMItemCascading,
    cTestExe, cTestExe);
  FTestItems.Append(cShellNewCMItem);
  AddShellExCMTestItem(cShellFileExt, cShellExCMItem, cShellExCMItem,
    cShellExGUID, cTestExe);
  FTestItems.Append(cShellExCMItem);
  AddShellNewCMTestItem(cShellFileExt, cShellNewCMItem, cShellNewCMItem, cTestExe, cTestExe);
  TContextMenuList(FRootList).LoadContextmenu(cShellFileExt, False);
  CheckEquals(FTestItems.Count, FRootList.Count, 'Count of items differs from expected');

  // Erasable items do not have a corresponding file
  // NOTE: Only Shell and ShellEx can be erasable!
  FErasableTestItems.Append(cShellCMItemErasable);
  AddShellCMTestItem(cShellFileExtErasable, cShellCMItemErasable, cShellCMItemErasable,
    cTestExeErasable, cTestExeErasable);
  FErasableTestItems.Append(cShellExCMItemErasable);
  AddShellExCMTestItem(cShellFileExtErasable, cShellExCMItemErasable, cShellExCMItemErasable,
    cShellExGUIDErasable, cTestExeErasable);
  TContextMenuList(FRootList).LoadContextmenu(cShellFileExtErasable, False);
  CheckEquals(FErasableTestItems.Count, FRootList.ErasableItemsCount, 'Count of erasable items differs from expected');
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
  // Admin access rights are required!
  if not HasAdminAccessRights() then
    Exit;

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
  // Admin access rights are required!
  if not HasAdminAccessRights() then
    Exit;

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
  // Admin access rights are required!
  if not HasAdminAccessRights() then
    Exit;

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
  // Admin access rights are required!
  if not HasAdminAccessRights() then
    Exit;

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

procedure TContextListTest.TearDown;
var
  Reg: TRegistry;

begin
  inherited TearDown;

  if not HasAdminAccessRights() then
    Exit;

  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.DeleteKey(cShellFileExt);
    Reg.DeleteKey(cShellFileExtErasable);
    Reg.DeleteKey(cShellNewCMItem);
    Reg.DeleteKey(cShellNotErasableFileExt);
    Reg.DeleteKey('CLSID\'+ cShellExGUID);

    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.DeleteKey(KEY_COMMAND_STORE +'\'+ cShellCMCascadingSubItem1);
    Reg.DeleteKey(KEY_COMMAND_STORE +'\'+ cShellCMCascadingSubItem2);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextListTest.TestAddNewItem;
const
  cNewShellItemCaption = 'Edit with notepad';

var
  ExpectedCount: Integer;

begin
  ExpectedCount := FRootList.Count + 1;
  CheckTrue(TContextMenuList(FRootList).Add(cNewTestExe, '"%1"', '.txt', cNewShellItemCaption), 'Item could not be added');
  CheckEquals(ExpectedCount, FRootList.Count, 'Count was not increased');
  Check(TContextMenuList(FRootList).Last is TContextMenuShellItem, 'Context menu item must be added as TContextMenuShellItem');
  FTestItems.Add(cNewShellItemCaption);
end;

procedure TContextListTest.TestChangeItemCommands;
begin
  // NOTE: Changing the filename of a cascading shell and shell new items is not possible!
  FTestItems.Clear;
  FTestItems.Append(cShellCMItem);
  FTestItems.Append(cShellExCMItem);
  inherited TestChangeItemCommands;
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

procedure TContextListTest.TestNotErasableItems;
const
  cNotErasableShellFilePaths: array[0..5] of string = (
    'C:\Windows\regedit',
    'explorer "ms-windows-store://search/?query=DVD"',
    '"C:\Windows\System32\rundll32.exe" "C:\Windows\System32\dfshim.dll",ShOpenVerbShortcut %1|%2',
    '%SystemRoot%\system32\rundll32.exe cryptext.dll,CryptExtOpenSTR %1',
    '"%1" %*',
    '"colorcpl.exe" "%1"'
  );

  cNotErasableShellExFilePaths: array[0..1] of string = (
    '%SystemRoot%\system32\acppage.dll',
    'explorerframe.dll'
  );

var
  i, ErasableItemsCount: Integer;
  Name: string;

begin
  ErasableItemsCount := FRootList.ErasableItemsCount;

  // Add Shell context menu items
  for i := Low(cNotErasableShellFilePaths) to High(cNotErasableShellFilePaths) do
  begin
    Name := 'Shell not erasable ' + IntToStr(i);
    AddShellCMTestItem(cShellNotErasableFileExt, Name, '', '', cNotErasableShellFilePaths[i]);
    FTestItems.Append(Name);
  end;

  // Add ShellEx context menu items
  for i := Low(cNotErasableShellExFilePaths) to High(cNotErasableShellExFilePaths) do
  begin
    Name := 'ShellEx not erasable '+ IntToStr(i);
    AddShellExCMTestItem(cShellNotErasableFileExt, Name, '', cShellExGUID, cNotErasableShellExFilePaths[i]);
    FTestItems.Append(Name);
  end;

  TContextMenuList(FRootList).LoadContextmenu(cShellNotErasableFileExt, False);
  CheckEquals(ErasableItemsCount, FRootList.ErasableItemsCount, 'Count of erasable items differs from expected');
end;


{ TServiceListTest }

procedure TServiceListTest.SetUp;
begin
  inherited SetUp;
  Check(HasAdminAccessRights(), 'Test must be run with admin access rights!');

  FRootList := TRootList<TRootItem>(TServiceList.Create);
  FTestItems.Append(cService);
  Check(TServiceList(FRootList).Add(cTestExe, '', cService), 'Service already exists!');
  FErasableTestItems.Append(cServiceErasable);
  Check(TServiceList(FRootList).Add(cTestExeErasable, '', cServiceErasable), 'Service already exists!');
  CheckEquals(FErasableTestItems.Count, FRootList.ErasableItemsCount, 'Count of erasable items differs from expected');
  CheckEquals(2, FRootList.Count, 'Actual item count differs from expected count');

  FRootList.Clear();
  FRootList.Search(False);
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

  procedure ImportTask(const AName: string);
  var
    TaskFileName: string;

  begin
    TaskFileName := cTestFilesDir + AName + FRootList.GetBackupExtension();
    Check(FileExists(TaskFileName), 'Task backup file "'+ TaskFileName +'" does not exist!');
    Check(TTaskList(FRootList).ImportBackup(TaskFileName), 'Task already exists!');
    CheckFalse(TTaskList(FRootList).ImportBackup(TaskFileName), 'Task already exists so it must not be imported again!');
  end;

begin
  inherited SetUp;
  Check(HasAdminAccessRights(), 'Test must be run with admin access rights!');

  FRootList := TRootList<TRootItem>(TTaskList.Create);
  FTestItems.Append(cTask);
  ImportTask(cTask);
  FErasableTestItems.Append(cTaskErasable);
  ImportTask(cTaskErasable);
  CheckEquals(FErasableTestItems.Count, FRootList.ErasableItemsCount, 'Count of erasable items differs from expected');
  CheckEquals(2, FRootList.Count, 'Actual item count differs from expected count');

  FRootList.Clear();
  FRootList.Search(False);
end;

initialization
  RegisterTest(TStartupListTest.Suite);
  RegisterTest(TContextListTest.Suite);
  RegisterTest(TServiceListTest.Suite);
  RegisterTest(TTaskListTest.Suite);
end.


