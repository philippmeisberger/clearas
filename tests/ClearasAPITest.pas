unit ClearasAPITest;

interface

uses
  TestFramework, StrUtils, Registry, PMCWLanguageFile, ClearasAPI, ComObj,
  ActiveX, SysUtils, ShellAPI, Windows, PMCWOSUtils, KnownFolders, Taskschd,
  Variants, SyncObjs, ShlObj, Generics.Collections, Classes, Zip, CommCtrl,
  PMCWIniFileParser, WinSvc, Graphics, Forms;

const
  cTestExe = 'C:\Windows\regedit.exe';

type
  TRootListTest = class(TTestCase)
  strict private
    FLockingSuccessful: Boolean;
    procedure TestLocking_SearchStart(Sender: TObject);
  protected
    FRootList: TRootList<TRootItem>;
    procedure SelectItem(const AItemName: string);
    procedure TestDisable(const AItemName: string);
    procedure TestEnable(const AItemName: string);
    procedure TestDelete(const AItemName: string);
    procedure TestExport(const AItemName: string);
    procedure TestExportBackup;
    procedure TestImportBackup;
    procedure TestRename(const AItemName: string); virtual;
    procedure TestChangeFilePath(const AItemName: string);
  public
    procedure TearDown; override;
  published
    procedure TestLocking;
  end;

  TStartupListTest = class(TRootListTest)
  const
    cHKCU           = 'HKCU';
    cHKCU_RUNONCE   = 'HKCU RunOnce';
    cHKLM           = 'HKLM';
    cHKLM32         = 'HKLM32';
    cHKLM_RUNONCE   = 'HKLM RunOnce';
    cHKLM_RUNONCE32 = 'HKLM RunOnce32';
  private
    procedure AddTestItemEnabled(ALocation: TStartupLocation);
    procedure DeleteTestItem(ALocation: TStartupLocation);
    function GetItemName(ALocation: TStartupLocation): string;
  public
    procedure SetUp; override;
  published
    procedure AddEnabledTestItems;
    procedure TestDisableItems;
    procedure TestEnableItems;
    procedure TestRenameItems;
    procedure TestChangeItemFilePaths;
    procedure TestExportItems;
    procedure TestDeleteItems;
    procedure CleanUp;
  end;

  TContextListTest = class(TRootListTest)
  const
    cShellFileExt             = '.789';
    cShellExGUID              = '{C9BD3A62-5743-4102-892C-62381FD93E3F}';
    cShellCMItem              = 'ShellTest';
    cShellCMItemCascading     = 'ShellCascadingTest';
    cShellCMCascadingSubItem1 = 'ShellCascadingItemTest1';
    cShellCMCascadingSubItem2 = 'ShellCascadingItemTest2';
    cShellExCMItem            = 'ShellExTest';
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
  published
    procedure AddEnabledTestItems;
    procedure TestDisableItems;
    procedure TestEnableItems;
    procedure TestRenameItems;
    procedure TestChangeItemFilePaths;
    procedure TestExportItems;
    procedure TestDeleteItems;
    procedure CleanUp;
  end;

  TServiceListTest = class(TRootListTest)
  const
    cServiceCaption = 'TestService';
  private
    procedure LoadService();
  protected
    procedure TestRename(const AItemName: string); override;
  public
    procedure SetUp; override;
  published
    procedure AddEnabledTestItem;
    procedure TestDisableItem;
    procedure TestEnableItem;
    procedure TestRenameItem;
    procedure TestChangeItemFilePath;
    procedure TestExportItem;
    procedure TestDeleteItem;
  end;

  TTaskListTest = class(TRootListTest)
  const
    cTestTaskName = 'Task';
    cTestTaskFile = 'data\'+ cTestTaskName +'.xml';
  public
    procedure SetUp; override;
    procedure LoadTasks();
  published
    procedure AddEnabledTestItem;
    procedure TestDisableItem;
    procedure TestEnableItem;
    procedure TestRenameItem;
    procedure TestChangeItemFilePath;
    procedure TestExportItem;
    procedure TestDeleteItem;
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

procedure TRootListTest.TearDown;
begin
  FreeAndNil(FRootList);
  inherited TearDown;
end;

procedure TRootListTest.SelectItem(const AItemName: string);
var
  Index: Integer;

begin
  CheckNotEquals(0, FRootList.Count, 'List is empty: Load() was not called!');
  Index := FRootList.IndexOf(AItemName);
  CheckNotEquals(-1, Index, 'Item "'+ AItemName +'" could not be found!');
  FRootList.Selected := FRootList[Index];
end;

procedure TRootListTest.TestChangeFilePath(const AItemName: string);
const
  cNewTestExe      = 'C:\Windows\notepad.exe';
  cNewTestArgument = '-o';
  cNewTestFileName = cNewTestExe +' '+ cNewTestArgument;

begin
  SelectItem(AItemName);
  CheckEqualsString(cTestExe, FRootList.Selected.FileNameOnly, 'FileName of "'+ AItemName +'" does not match before changing file path!');
  FRootList.ChangeItemFilePath(cNewTestFileName);
  CheckEqualsString(cNewTestFileName, FRootList.Selected.FileName, 'FileName of "'+ AItemName +'" does not match after changing file path!');
  CheckEqualsString(cNewTestExe, FRootList.Selected.FileNameOnly, 'FileNameOnly of "'+ AItemName +'" does not match after changing file path!');
  CheckEqualsString(cNewTestArgument, FRootList.Selected.Arguments, 'Arguments of "'+ AItemName +'" does not match after changing file path!');
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
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After deleting item "'+ AItemName +'" EnabledItemsCount should be equal to EnabledCounter!');
  CheckEquals(Counter, FRootList.Count, 'After deleting item "'+ AItemName +'" Count should be decreased by 1!');
end;

procedure TRootListTest.TestDisable(const AItemName: string);
var
  Counter, EnabledCounter: Integer;

begin
  SelectItem(AItemName);
  Counter := FRootList.Count;
  EnabledCounter := FRootList.EnabledItemsCount;
  CheckTrue(FRootList.Selected.Enabled, 'Before disabling item "'+ AItemName +'" Enabled must be True!');
  FRootList.DisableItem();
  CheckFalse(FRootList.Selected.Enabled, 'After disabling item "'+ AItemName +'" Enabled should also be False!');
  Dec(EnabledCounter);
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After disabling item "'+ AItemName +'" EnabledItemsCount must be decreased by 1!');
  CheckEquals(Counter, FRootList.Count, 'After disabling item "'+ AItemName +'" Count must not be changed!');
end;

procedure TRootListTest.TestEnable(const AItemName: string);
var
  Counter, EnabledCounter: Integer;

begin
  SelectItem(AItemName);
  Counter := FRootList.Count;
  EnabledCounter := FRootList.EnabledItemsCount;
  CheckFalse(FRootList.Selected.Enabled, 'Before enabling item "'+ AItemName +'" Enabled must be False!');
  FRootList.EnableItem();
  CheckTrue(FRootList.Selected.Enabled, 'After enabling item "'+ AItemName +'" Enabled should be True!');
  Inc(EnabledCounter);
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After enabling item "'+ AItemName +'" EnabledItemsCount must be increased by 1!');
  CheckEquals(Counter, FRootList.Count, 'After enabling item "'+ AItemName +'" Count must not be changed!');
end;

procedure TRootListTest.TestExportBackup;
begin
  // TODO: TestExportBackup
end;

procedure TRootListTest.TestExport(const AItemName: string);
var
  SearchResult: TSearchRec;

begin
  SelectItem(AItemName);
  FRootList.ExportItem(FRootList.Selected.Name);
  CheckEquals(0, FindFirst(FRootList.Selected.Name +'.*', faAnyFile - faDirectory, SearchResult), 'Exported file does not exist!');
  CheckTrue(DeleteFile(PChar(SearchResult.Name)), 'Exported file could not be deleted!');
end;

procedure TRootListTest.TestImportBackup;
begin
{const
  TestTaskFile = 'C:\Users\Phil\PMCW\Projekte\clearas\tests\data\Task.xml';

begin
  if not Supports(FRootList, IImportableList) then
  begin
    FCheckCalled := True;
    Exit;
  end;  //of begin

  CheckTrue((FRootList as IImportableList).ImportBackup(TestTaskFile), 'Import failed');
  CheckEquals(1, FRootList.Count, 'After importing there must be 1 item in list but it is empty');
  FRootList.Selected := FRootList[0];

  if FRootList.Selected.Enabled then
  begin
    FRootList.DisableItem();
    CheckFalse(FRootList.Selected.Enabled, 'After disabling an item the status must also be disabled');
  end
  else
  begin
    FRootList.EnableItem();
    CheckTrue(FRootList.Selected.Enabled, 'After enabling an item the status must also be enabled');
  end;

  CheckFalse((FRootList as IImportableList).ImportBackup(TestTaskFile), 'Backup duplicated');
  CheckTrue(FRootList.DeleteItem(), 'Could not delete imported item');
  CheckEquals(0, FRootList.Count, 'After deleting one item the list must be empty');}
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
begin
  FLockingSuccessful := False;

  // Start async loading
  FRootList.OnSearchStart := TestLocking_SearchStart;
  FRootList.Load(True);

  // Give the thread time to kill himself
  Delay(2000);
  CheckTrue(FLockingSuccessful, 'List was not locked!!');
end;

procedure TRootListTest.TestRename(const AItemName: string);
begin
  SelectItem(AItemName);
  FRootList.RenameItem(AItemName +'2');
  CheckEquals(AItemName +'2', FRootList.Selected.Name, 'Item was not renamed correctly!');
  FRootList.RenameItem(AItemName);
  CheckEquals(AItemName, FRootList.Selected.Name, 'Item was not renamed correctly twice!');
end;


{ TStartupListTest }

procedure TStartupListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TStartupList.Create);
end;

procedure TStartupListTest.TestChangeItemFilePaths;
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);

  TestChangeFilePath(cHKCU);
  TestChangeFilePath(cHKCU_RUNONCE);
  TestChangeFilePath(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  TestChangeFilePath(cHKLM);
  TestChangeFilePath(cHKLM_RUNONCE);
  TestChangeFilePath(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    TestChangeFilePath(cHKLM32);
    TestChangeFilePath(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.TestDeleteItems;
var
  Location: TStartupLocation;
  Counter: Integer;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);

  TestDelete(cHKCU);
  TestDelete(cHKCU_RUNONCE);
  TestDelete(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  TestDelete(cHKLM);
  TestDelete(cHKLM_RUNONCE);
  TestDelete(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    TestDelete(cHKLM32);
    TestDelete(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
  Counter := FRootList.Count;
  FRootList.Clear;

  // Sanity check
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);

  CheckEquals(Counter, FRootList.Count, 'After deleting items and loading items again counter does not match!');
end;

procedure TStartupListTest.TestDisableItems;
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);

  TestDisable(cHKCU);
  TestDisable(cHKCU_RUNONCE);
  TestDisable(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  TestDisable(cHKLM);
  TestDisable(cHKLM_RUNONCE);
  TestDisable(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    TestDisable(cHKLM32);
    TestDisable(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.TestEnableItems;
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);

  TestEnable(cHKCU);
  TestEnable(cHKCU_RUNONCE);
  TestEnable(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  TestEnable(cHKLM);
  TestEnable(cHKLM_RUNONCE);
  TestEnable(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    TestEnable(cHKLM32);
    TestEnable(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.TestExportItems;
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);

  TestExport(cHKCU);
  TestExport(cHKCU_RUNONCE);
  TestExport(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  TestExport(cHKLM);
  TestExport(cHKLM_RUNONCE);
  TestExport(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    TestExport(cHKLM32);
    TestExport(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.TestRenameItems;
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);

  TestRename(cHKCU);
  TestRename(cHKCU_RUNONCE);
  TestRename(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  TestRename(cHKLM);
  TestRename(cHKLM_RUNONCE);
  TestRename(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    TestRename(cHKLM32);
    TestRename(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.AddTestItemEnabled(ALocation: TStartupLocation);
var
  Reg: TRegistry;
  LnkFile: TStartupLnkFile;

begin
{$IFDEF DEBUG}
  // Skip startup locations that need admin access rights in debug configuration only
  if (ALocation in [slHklmRun..slHklmRunOnce32, slCommonStartup]) then
    Exit;
{$ENDIF}

  if (ALocation in [slStartupUser, slCommonStartup]) then
  begin
    LnkFile := TStartupLnkFile.Create(GetItemName(ALocation), (ALocation = slStartupUser), cTestExe, '-s');

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
      Reg.WriteString(GetItemName(ALocation), cTestExe);
      CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try
  end;
end;

procedure TStartupListTest.AddEnabledTestItems();
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    AddTestItemEnabled(Location);
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
        Reg.OpenKey(KEY_STARTUP_DISABLED, False);
        Reg.DeleteKey(GetItemName(ALocation));
      end  //of begin
      else
      begin
        // Delete item from approved location (since to Windows 8)
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

function TStartupListTest.GetItemName(ALocation: TStartupLocation): string;
begin
  case ALocation of
    slHkcuRun:       Result := cHKCU;
    slHkcuRunOnce:   Result := cHKCU_RUNONCE;
    slHklmRun:       Result := cHKLM;
    slHklmRun32:     Result := cHKLM32;
    slHklmRunOnce:   Result := cHKLM_RUNONCE;
    slHklmRunOnce32: Result := cHKLM_RUNONCE32;
    slStartupUser:   Result := STARTUP_USER;
    slCommonStartup: Result := STARTUP_COMMON;
  end;  //of case
end;

{ TContextListTest }

procedure TContextListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TContextList.Create);
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

  Check(AnsiStartsStr('.', AFileExt), 'FileExt must start with a "."!');
  CheckNotEqualsString('', AName, 'Name must not be empty!');
  CheckNotEqualsString('', AFileName, 'FileName must not be empty!');
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

  Check(AnsiStartsStr('.', AFileExt), 'FileExt must start with a "."!');
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

  Check(AnsiStartsStr('.', AFileExt), 'FileExt must start with a "."!');
  CheckNotEqualsString('', AName, 'Name must not be empty!');
  CheckNotEqualsString('', AFileName, 'FileName must not be empty!');
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

  Check(AnsiStartsStr('.', AFileExt), 'FileExt must start with a "."!');
  CheckNotEqualsString('', AFileName, 'FileName must not be empty!');
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
    Reg.OpenKey(AFileExt +'\'+ CM_SHELLNEW, True);
    Reg.WriteExpandString('ItemName', '@%systemroot%\system32\mspaint.exe,-59414');
    Reg.WriteString('NullFile', '');
    CheckEqualsString('', Reg.LastErrorMsg, Reg.LastErrorMsg);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TContextListTest.AddEnabledTestItems;
begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  CheckTrue(False, 'Test must be run with admin access rights!');
{$ENDIF}

  AddShellCMTestItem(cShellFileExt, cShellCMItem, cShellCMItem, cTestExe, cTestExe);
  AddShellCascadingCMTestItem(cShellFileExt, cShellCMItemCascading, cShellCMItemCascading,
    cTestExe, cTestExe);
  AddShellExCMTestItem(cShellFileExt, cShellExCMItem, cShellExCMItem,
    cShellExGUID, cTestExe);
  AddShellNewCMTestItem(cShellFileExt, cShellNewCMItem, cShellNewCMItem, cTestExe, cTestExe);
end;

procedure TContextListTest.TestChangeItemFilePaths;
begin
  TContextList(FRootList).LoadContextmenu(cShellFileExt, False);
  TestChangeFilePath(cShellCMItem);
  TestChangeFilePath(cShellExCMItem);
  // NOTE: Changing the filename of a cascading shell and shell new items is not possible!
end;

procedure TContextListTest.TestDeleteItems;
begin
  TContextList(FRootList).LoadContextmenu(cShellFileExt, False);
  TestDelete(cShellCMItem);
  TestDelete(cShellExCMItem);
  TestDelete(cShellCMItemCascading);
  TestDelete(cShellNewCMItem);
end;

procedure TContextListTest.TestDisableItems;
begin
  TContextList(FRootList).LoadContextmenu(cShellFileExt, False);
  TestDisable(cShellCMItem);
  TestDisable(cShellExCMItem);
  TestDisable(cShellCMItemCascading);
  TestDisable(cShellNewCMItem);
end;

procedure TContextListTest.TestEnableItems;
begin
  TContextList(FRootList).LoadContextmenu(cShellFileExt, False);
  TestEnable(cShellCMItem);
  TestEnable(cShellExCMItem);
  TestEnable(cShellCMItemCascading);
  TestEnable(cShellNewCMItem);
end;

procedure TContextListTest.TestExportItems;
begin
  TContextList(FRootList).LoadContextmenu(cShellFileExt, False);
  TestExport(cShellCMItem);
  TestExport(cShellExCMItem);
  TestExport(cShellCMItemCascading);
  TestExport(cShellNewCMItem);
end;

procedure TContextListTest.TestRename(const AItemName: string);
begin
  SelectItem(AItemName);
  FRootList.RenameItem(AItemName +'2');

  // NOTE: Renaming a contextmenu item changes the caption not the name!!!
  CheckEquals(AItemName +'2', FRootList.Selected.Caption, 'Item was not renamed correctly!');
  FRootList.RenameItem(AItemName);
  CheckEquals(AItemName, FRootList.Selected.Caption, 'Item was not renamed correctly twice!');
end;

procedure TContextListTest.TestRenameItems;
begin
  TContextList(FRootList).LoadContextmenu(cShellFileExt, False);
  TestRename(cShellCMItem);
  // NOTE: Renaming shellex and shell new items is not possible
  TestRename(cShellCMItemCascading);
end;

procedure TContextListTest.CleanUp;
var
  Reg: TRegistry;

begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  CheckTrue(False, 'Test must be run with admin access rights!');
{$ENDIF}

  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.DeleteKey(cShellFileExt);
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
end;

procedure TServiceListTest.LoadService();
var
  Service: SC_HANDLE;
  ServiceName: string;

begin
{$IFDEF DEBUG}
  // Skip test in debug configuration because it needs admin access rights
  CheckTrue(False, 'Test must be run with admin access rights!');
{$ENDIF}
  // Add test service to list
  ServiceName := ChangeFileExt(ExtractFileName(cTestExe), '');
  CheckNotEquals(0, TServiceList(FRootList).Manager, 'Invalid service manager handle!');
  Service := OpenService(TServiceList(FRootList).Manager, PChar(ServiceName), SERVICE_QUERY_CONFIG);
  CheckNotEquals(0, Service, 'Invalid service handle!');
  TServiceList(FRootList).LoadService(ServiceName, Service, False);
  CloseServiceHandle(Service);
end;

procedure TServiceListTest.TestRename(const AItemName: string);
begin
  SelectItem(AItemName);
  FRootList.RenameItem(AItemName +'2');

  // NOTE: Renaming a service item changes the caption not the name!!!
  CheckEquals(AItemName +'2', FRootList.Selected.Caption, 'Item was not renamed correctly!');
  FRootList.RenameItem(AItemName);
  CheckEquals(AItemName, FRootList.Selected.Caption, 'Item was not renamed correctly twice!');
end;

procedure TServiceListTest.AddEnabledTestItem;
begin
  CheckTrue(TServiceList(FRootList).Add(cTestExe, '', cServiceCaption), 'Service already exists!');
  CheckEquals(1, FRootList.Count, 'There should be 1 service in the list!');
end;

procedure TServiceListTest.TestChangeItemFilePath;
begin
  LoadService();
  TestChangeFilePath(cServiceCaption);
end;

procedure TServiceListTest.TestDeleteItem;
begin
  LoadService();
  TestDelete(cServiceCaption);
end;

procedure TServiceListTest.TestDisableItem;
begin
  LoadService();
  TestDisable(cServiceCaption);
end;

procedure TServiceListTest.TestEnableItem;
begin
  LoadService();
  TestEnable(cServiceCaption);
end;

procedure TServiceListTest.TestExportItem;
begin
  LoadService();
  TestExport(cServiceCaption);
end;

procedure TServiceListTest.TestRenameItem;
begin
  LoadService();
  TestRename(cServiceCaption);
end;

{ TTaskListTest }

procedure TTaskListTest.LoadTasks();
var
  TaskFolder: ITaskFolder;

begin
  // Open current folder
  OleCheck(TTaskList(FRootList).TaskService.GetFolder('\', TaskFolder));

  // Add tasks in folder to list
  TTaskList(FRootList).LoadTasks(TaskFolder, False);
end;

procedure TTaskListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TTaskList.Create);
end;

procedure TTaskListTest.AddEnabledTestItem;
var
  TaskFileName: string;

begin
  TaskFileName := IncludeTrailingBackslash(ExtractFileDir(ExtractFileDir(GetCurrentDir()))) + cTestTaskFile;
  CheckTrue(TTaskList(FRootList).ImportBackup(TaskFileName), 'Task already exists!');
end;

procedure TTaskListTest.TestChangeItemFilePath;
begin
  LoadTasks();
  TestChangeFilePath(cTestTaskName);
end;

procedure TTaskListTest.TestDeleteItem;
begin
  LoadTasks();
  TestDelete(cTestTaskName);
end;

procedure TTaskListTest.TestDisableItem;
begin
  LoadTasks();
  TestDisable(cTestTaskName);
end;

procedure TTaskListTest.TestEnableItem;
begin
  LoadTasks();
  TestEnable(cTestTaskName);
end;

procedure TTaskListTest.TestExportItem;
begin
  LoadTasks();
  TestExport(cTestTaskName);
end;

procedure TTaskListTest.TestRenameItem;
begin
  LoadTasks();
  TestRename(cTestTaskName);
end;

{procedure TTaskListTest.OnTaskSearchFinished(Sender: TObject);
const
  ZipFile = 'C:\Users\Phil\PMCW\Projekte\clearas\tests\TestExportList.zip';

begin
  Check(FTaskList.Count > 0, 'List must not be empty!');
  FTaskList.ExportList(ZipFile);
  CheckTrue(FileExists(ZipFile), 'List was not exported as file!');
end;

procedure TTaskListTest.TestChangeItemFilePath;
const
  TestTaskFile = 'Task.xml';
  NewFileName = 'C:\Windows\regedit.exe';

var
  OldFileName: string;

begin
  CheckTrue(FTaskList.ImportBackup(TestTaskFile), 'Import failed');
  CheckEquals(1, FTaskList.Count, 'There must be 1 task in list but it is empty');
  FTaskList.Selected := FTaskList[0];

  OldFileName := FTaskList.Selected.FileName;
  CheckNotEquals('', OldFileName, 'FileName must not be empty!');

  // Change path
  CheckTrue(FTaskList.ChangeItemFilePath(NewFileName), 'Could not change path!');
  CheckEquals(NewFileName, FTaskList.Selected.FileName, 'FileNames are not the same!');

  CheckTrue(FTaskList.EnableItem(), 'Could not enable task');
  CheckTrue(FTaskList.Selected.Enabled, 'After enabling a task the status must be enabled');



  CheckTrue(FTaskList.DeleteItem(), 'Could not delete task');
  CheckEquals(0, FTaskList.Count, 'After deleting of one item task list must be empty');
end;

procedure TTaskListTest.TestExportBackup;
begin
  FTaskList.OnSearchFinish := OnTaskSearchFinished;
  FTaskList.Load(False);
  Delay(1000);
  FTaskList.Clear;
  CheckEquals(0, FTaskList.Count, 'After clearing the list must be empty!');
end;

procedure TTaskListTest.TestExportItem;
begin

end;

procedure TTaskListTest.TestImportBackup;
const
  TestTaskFile = 'C:\Users\Phil\PMCW\Projekte\clearas\tests\TestTask.xml';

begin
  CheckTrue(FTaskList.ImportBackup(TestTaskFile), 'Import failed');
  CheckEquals(1, FTaskList.Count, 'There must be 1 task in list but it is empty');
  FTaskList.Selected := FTaskList[0];

  if FTaskList.Selected.Enabled then
  begin
    CheckTrue(FTaskList.DisableItem(), 'Could not disable task');
    CheckFalse(FTaskList.Selected.Enabled, 'After disabling a task the status must be disabled');
  end
  else
  begin
    CheckTrue(FTaskList.EnableItem(), 'Could not enable task');
    CheckTrue(FTaskList.Selected.Enabled, 'After enabling a task the status must be enabled');
  end;

  CheckFalse(FTaskList.ImportBackup(TestTaskFile), 'Task duplicated');
  CheckTrue(FTaskList.DeleteItem(), 'Could not delete task');
  CheckEquals(0, FTaskList.Count, 'After deleting of one item task list must be empty');
end;

{procedure TestStartupList.ImportStartupFile(const AFileName: TFileName;
  ACommonStartup: Boolean = False);
var
  Destination: string;

begin
  if ACommonStartup then
    Destination := GetKnownFolderPath(FOLDERID_CommonStartup)
  else
    Destination := GetKnownFolderPath(FOLDERID_Startup);

  Destination := Destination + ChangeFileExt(ExtractFileName(AFileName), '.lnk');
  CopyFile(PChar(AFileName), PChar(Destination), False);
end;}

initialization
  RegisterTest(TStartupListTest.Suite);
  RegisterTest(TContextListTest.Suite);
  RegisterTest(TServiceListTest.Suite);
  RegisterTest(TTaskListTest.Suite);
end.

