unit ClearasAPITest;

interface

uses
  TestFramework, StrUtils, Registry, PMCWLanguageFile, ClearasAPI, ComObj,
  ActiveX, SysUtils, ShellAPI, Windows, PMCWOSUtils, KnownFolders, Taskschd,
  Variants, SyncObjs, ShlObj, Generics.Collections, Classes, Zip, CommCtrl,
  PMCWIniFileParser, WinSvc, Graphics, Forms;

type
  TStartupListTest = class(TTestCase)
  const
    cTestExe        = 'C:\Windows\regedit.exe';
    cNewTestExe     = 'C:\Windows\notepad.exe';
    cHKCU           = 'HKCU';
    cHKCU_RUNONCE   = 'HKCU RunOnce';
    cHKLM           = 'HKLM';
    cHKLM32         = 'HKLM32';
    cHKLM_RUNONCE   = 'HKLM RunOnce';
    cHKLM_RUNONCE32 = 'HKLM RunOnce32';
  strict private
    FLockingSuccessful: Boolean;
    FStartupList: TStartupList;
    procedure AddTestItemEnabled(ALocation: TStartupLocation);
    procedure AddTestItemsEnabled();
    procedure DeleteTestItem(ALocation: TStartupLocation);
    function GetItemName(ALocation: TStartupLocation): string;
    procedure TestLocking_SearchStart(Sender: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDisableItem;
    procedure TestEnableItem;
    procedure TestRenameItem;
    procedure TestChangeFilePath;
    procedure TestDeleteItem;
    procedure TestLocking;
    procedure CleanUp;
  end;

  {TestRootList = class(TTestCase)
  protected
    FRootList: TRootList<TRootItem>;
  public
    procedure TearDown; override;
  public
    procedure TestDisable;
    procedure TestEnable;
    procedure TestDelete;
    procedure TestExportItem;
    procedure TestExportBackup;
    procedure TestImportBackup;
    procedure TestRename;
  published
    procedure TestLocking;
  end;

  TestStartupList = class(TestRootList)
  private
    procedure TestItem(AName: string);
    procedure ImportStartupFile(const AFileName: TFileName;
      ACommonStartup: Boolean = False);
  public
    procedure SetUp; override;
  published
    procedure TestHKCUItem;
    procedure TestHKCU32Item;
    procedure TestHKLMItem;
    procedure TestHKLM32Item;
    procedure TestRunOnceItem;
    procedure TestRunOnce32Item;
    procedure TestStartupUserItem;
    procedure TestStartupCommonItem;
  end;

  TestTTaskList = class(TTestCase)
  strict private
    FTaskList: TTaskList;
    procedure OnTaskSearchFinished(Sender: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestChangeItemFilePath;
    procedure TestExportItem;
    procedure TestExportBackup;
    procedure TestImportBackup;
    procedure TestRenameItem;
  end;}

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
{
procedure TestRootList.TearDown;
begin
  FreeAndNil(FRootList);
  inherited TearDown;
end;

procedure TestRootList.TestDelete;
var
  CountBefore, CountEnabledBefore: Word;
  WasEnabled: Boolean;

begin
  CountBefore := FRootList.Count;
  CountEnabledBefore := FRootList.Enabled;
  WasEnabled := FRootList.Selected.Enabled;
  CheckTrue(FRootList.DeleteItem(), 'Item was not deleted!');

  if WasEnabled then
    CheckEquals(CountEnabledBefore - 1, FRootList.Enabled, 'After deleting enabled item count must be decreased by 1!')
  else
    CheckEquals(CountEnabledBefore, FRootList.Enabled, 'After deleting disabled item count must not be changed!');

  CheckEquals(CountBefore, FRootList.Count, 'After deleting item count must must be decreased by 1!');
end;

procedure TestRootList.TestDisable;
var
  CountBefore, CountEnabledBefore: Integer;

begin
  CountBefore := FRootList.Count;
  CountEnabledBefore := FRootList.Enabled;
  CheckTrue(FRootList.Selected.Enabled, 'Item must be enabled to be disabled!');
  CheckTrue(FRootList.DisableItem(), 'Item was not disabled!');
  CheckFalse(FRootList.Selected.Enabled, 'After disabling item status must also be disabled!');
  CheckEquals(0, FRootList.Enabled, 'After disabling item enabled count must be decreased by 1!');
  CheckEquals(CountBefore, FRootList.Count, 'After disabling count must not be changed!');
end;

procedure TestRootList.TestEnable;
var
  CountBefore, CountEnabledBefore: Integer;

begin
  CountBefore := FRootList.Count;
  CountEnabledBefore := FRootList.Enabled;
  CheckFalse(FRootList.Selected.Enabled, 'Item must be disabled to be enabled!');
  CheckTrue(FRootList.EnableItem(), 'Item was not enabled!');
  CheckTrue(FRootList.Selected.Enabled, 'After enabling item status must also be enabled!');
  CheckEquals(1, FRootList.Enabled, 'After enabling item enabled count must be increased by 1!');
  CheckEquals(CountBefore, FRootList.Count, 'After enabling count must not be changed!');
end;

procedure TestRootList.TestExportBackup;
begin

end;

procedure TestRootList.TestExportItem;
begin
  FRootList.ExportItem(FRootList.Selected.Name);
  FCheckCalled := True;
end;

procedure TestRootList.TestImportBackup;
const
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
    CheckTrue(FRootList.DisableItem(), 'Could not disable imported item');
    CheckFalse(FRootList.Selected.Enabled, 'After disabling an item the status must also be disabled');
  end
  else
  begin
    CheckTrue(FRootList.EnableItem(), 'Could not enable imported item');
    CheckTrue(FRootList.Selected.Enabled, 'After enabling an item the status must also be enabled');
  end;

  CheckFalse((FRootList as IImportableList).ImportBackup(TestTaskFile), 'Backup duplicated');
  CheckTrue(FRootList.DeleteItem(), 'Could not delete imported item');
  CheckEquals(0, FRootList.Count, 'After deleting one item the list must be empty');
end;

procedure TestRootList.TestLocking;
begin
  FRootList.Load();

  try
    FRootList.EnableItem();

  except
    on E: EListBlocked do
      FCheckCalled := True;
  end;

  Delay(1000);
end;

procedure TestRootList.TestRename;
begin
  CheckTrue(FRootList.RenameItem(FRootList.Selected.ClassName));
end;

{ TestTTaskList }
{
procedure TestTTaskList.SetUp;
begin
  inherited SetUp;
  FTaskList := TTaskList.Create;
end;

procedure TestTTaskList.TearDown;
begin
  FreeAndNil(FTaskList);
  inherited TearDown;
end;

procedure TestTTaskList.OnTaskSearchFinished(Sender: TObject);
const
  ZipFile = 'C:\Users\Phil\PMCW\Projekte\clearas\tests\TestExportList.zip';

begin
  Check(FTaskList.Count > 0, 'List must not be empty!');
  FTaskList.ExportList(ZipFile);
  CheckTrue(FileExists(ZipFile), 'List was not exported as file!');
end;

procedure TestTTaskList.TestChangeItemFilePath;
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

procedure TestTTaskList.TestExportBackup;
begin
  FTaskList.OnSearchFinish := OnTaskSearchFinished;
  FTaskList.Load(False);
  Delay(1000);
  FTaskList.Clear;
  CheckEquals(0, FTaskList.Count, 'After clearing the list must be empty!');
end;

procedure TestTTaskList.TestExportItem;
begin

end;

procedure TestTTaskList.TestImportBackup;
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

procedure TestTTaskList.TestRenameItem;
begin

end;


{ TestStartupList }
{
procedure TestStartupList.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TStartupList.Create);
end;

procedure TestStartupList.ImportStartupFile(const AFileName: TFileName;
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
end;

procedure TestStartupList.TestItem(AName: string);
var
  Index: Integer;

begin
  Check(FRootList.Count > 0, 'List must contain at least 1 item!');
  Index := FRootList.IndexOf(AName);
  CheckNotEquals(-1, Index, 'Item not found in list!');
  FRootList.Selected := FRootList[Index];
  TestDisable;
  TestRename;
  TestEnable;
  TestDelete;
end;

procedure TestStartupList.TestHKCU32Item;
begin
  ImportRegistryFile('..\data\HKCU32.reg');
  TStartupList(FRootList).LoadStartup(rkHKCU, False, True);
  TestItem('HKCU32');
end;

procedure TestStartupList.TestHKCUItem;
begin
  ImportRegistryFile('..\data\HKCU.reg');
  TStartupList(FRootList).LoadStartup(rkHKCU);
  TestItem('HKCU');
end;

procedure TestStartupList.TestHKLM32Item;
begin
  ImportRegistryFile('..\data\HKLM32.reg');
  TStartupList(FRootList).LoadStartup(rkHKLM, False, True);
  TestItem('HKLM32');
end;

procedure TestStartupList.TestHKLMItem;
begin
  ImportRegistryFile('..\data\HKLM32.reg');
  TStartupList(FRootList).LoadStartup(rkHKLM);
  TestItem('HKLM');
end;

procedure TestStartupList.TestRunOnce32Item;
begin
  ImportRegistryFile('..\data\RunOnce32.reg');
  TStartupList(FRootList).LoadStartup(rkHKLM, True, True);
  TestItem('RunOnce32');
end;

procedure TestStartupList.TestRunOnceItem;
begin
  ImportRegistryFile('..\data\RunOnce.reg');
  TStartupList(FRootList).LoadStartup(rkHKLM, True, False);
  TestItem('RunOnce');
end;

procedure TestStartupList.TestStartupCommonItem;
begin
  ImportStartupFile('..\data\Backup.CommonStartup', True);
  TStartupList(FRootList).LoadStartup(True);
  TestItem('Backup.CommonStartup');
end;

procedure TestStartupList.TestStartupUserItem;
begin
  ImportStartupFile('..\data\Backup.Startup', False);
  TStartupList(FRootList).LoadStartup(False);
  TestItem('Backup.Startup');
end;}

{ TStartupListTest }

procedure TStartupListTest.SetUp;
begin
  inherited SetUp;
  FStartupList := TStartupList.Create;
end;

procedure TStartupListTest.TearDown;
begin
  FStartupList.Free;
  inherited TearDown;
end;

procedure TStartupListTest.TestChangeFilePath;
var
  Location: TStartupLocation;

  procedure CheckChangeFilePath(const AItemName: string);
  var
    Index: Integer;

  begin
    Index := FStartupList.IndexOf(AItemName);
    CheckNotEquals(-1, Index, 'Item "'+ AItemName +'" could not be found before changing file path!');
    FStartupList.Selected := FStartupList[Index];
    CheckEqualsString(cTestExe, FStartupList.Selected.FileNameOnly, 'FileName of "'+ AItemName +'" does not match before renaming!');
    FStartupList.ChangeItemFilePath(cNewTestExe);
    CheckEqualsString(cNewTestExe, FStartupList.Selected.FileNameOnly, 'FileName of "'+ AItemName +'" does not match after renaming!');
  end;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    FStartupList.Load(Location);

  CheckChangeFilePath(cHKCU);
  CheckChangeFilePath(cHKCU_RUNONCE);
  //CheckChangeFilePath(cHKLM);
  //CheckChangeFilePath(cHKLM_RUNONCE);
  CheckChangeFilePath(STARTUP_USER +'.lnk');
  //CheckChangeFilePath(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    //CheckChangeFilePath(cHKLM32);
    //CheckChangeFilePath(cHKLM_RUNONCE32);
  end;  //of begin
end;

procedure TStartupListTest.TestDeleteItem;
var
  Location: TStartupLocation;
  Counter: Integer;

  procedure CheckDelete(const AItemName: string);
  var
    EnabledCounter: Integer;
    Index: Integer;

  begin
    Index := FStartupList.IndexOf(AItemName);
    CheckNotEquals(-1, Index, 'Item "'+ AItemName +'" could not be found before enabling!');
    FStartupList.Selected := FStartupList[Index];
    EnabledCounter := FStartupList.EnabledItemsCount;

    if FStartupList.Selected.Enabled then
      Dec(EnabledCounter);

    CheckTrue(FStartupList.DeleteItem(), 'Item could not be deleted!');
    Dec(Counter);
    CheckEquals(EnabledCounter, FStartupList.EnabledItemsCount, 'After deleting item EnabledItemsCount should be equal to EnabledCounter!');
  end;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    FStartupList.Load(Location);

  Counter := FStartupList.Count;
  CheckDelete(cHKCU);
  CheckDelete(cHKCU_RUNONCE);
  CheckDelete(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  CheckDelete(cHKLM);
  CheckDelete(cHKLM_RUNONCE);
  CheckDelete(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    CheckDelete(cHKLM32);
    CheckDelete(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
  CheckEquals(Counter, FStartupList.Count, 'After deleting items counter does not match!');
  FStartupList.Clear;

  // Sanity check
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    FStartupList.Load(Location);

  CheckEquals(Counter, FStartupList.Count, 'After deleting items and loading items again counter does not match!');
end;

procedure TStartupListTest.TestDisableItem;
var
  Location: TStartupLocation;

  procedure CheckDisable(const AItemName: string);
  var
    EnabledCounter: Integer;
    Index: Integer;

  begin
    Index := FStartupList.IndexOf(AItemName);
    CheckNotEquals(-1, Index, 'Item "'+ AItemName +'" could not be found before disabling!');
    FStartupList.Selected := FStartupList[Index];
    EnabledCounter := FStartupList.EnabledItemsCount;
    CheckNotEquals(0, EnabledCounter, 'Before disabling EnabledItemsCount must not be 0!');

    // TODO: Check item properties before disabling
    CheckTrue(FStartupList.Selected.Enabled, 'Before disabling item Enabled must be True!');
    Check(FStartupList.Selected.Time = 0, 'Before disabling item timestamp must be 0!');

    FStartupList.DisableItem();

    // TODO: Check item properties after disabling
    CheckFalse(FStartupList.Selected.Enabled, 'After disabling item Enabled should be False, too!');
    Check(FStartupList.Selected.Time <> 0, 'After disabling timestamp must be greater than 0!');
    Dec(EnabledCounter);
    CheckEquals(EnabledCounter, FStartupList.EnabledItemsCount, 'After disabling item EnabledItemsCount should be decreased by 1!');
  end;

begin
  AddTestItemsEnabled();

  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    FStartupList.Load(Location);

  CheckDisable(cHKCU);
  CheckDisable(cHKCU_RUNONCE);
  CheckDisable(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  CheckDisable(cHKLM);
  CheckDisable(cHKLM_RUNONCE);
  CheckDisable(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    CheckDisable(cHKLM32);
    CheckDisable(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.TestEnableItem;
var
  Location: TStartupLocation;

  procedure CheckEnable(const AItemName: string);
  var
    EnabledCounter: Integer;
    Index: Integer;

  begin
    Index := FStartupList.IndexOf(AItemName);
    CheckNotEquals(-1, Index, 'Item "'+ AItemName +'" could not be found before enabling!');
    FStartupList.Selected := FStartupList[Index];
    EnabledCounter := FStartupList.EnabledItemsCount;

    // TODO: Check item properties before enabling
    CheckFalse(FStartupList.Selected.Enabled, 'Before enabling item Enabled must be False!');
    Check(FStartupList.Selected.Time <> 0, 'Before enabling item timestamp must not be 0!');

    FStartupList.EnableItem();

    // TODO: Check item properties after enabling
    CheckTrue(FStartupList.Selected.Enabled, 'After enabling item Enabled should be True, too!');
    Check(FStartupList.Selected.Time = 0, 'After enabling timestamp must be 0!');
    Inc(EnabledCounter);
    CheckEquals(EnabledCounter, FStartupList.EnabledItemsCount, 'After enabling item EnabledItemsCount should be increased by 1!');
  end;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    FStartupList.Load(Location);

  CheckEnable(cHKCU);
  CheckEnable(cHKCU_RUNONCE);
  CheckEnable(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  CheckEnable(cHKLM);
  CheckEnable(cHKLM_RUNONCE);
  CheckEnable(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    CheckEnable(cHKLM32);
    CheckEnable(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.TestLocking_SearchStart(Sender: TObject);
begin
  try
    // This must not be possible e.g. during loading!
    FStartupList.EnableItem();

  except
    on E: EListBlocked do
      FLockingSuccessful := True;
  end;  //of try
end;

procedure TStartupListTest.TestRenameItem;
var
  Location: TStartupLocation;

  procedure CheckRename(const AItemName: string);
  var
    Index: Integer;

  begin
    Index := FStartupList.IndexOf(AItemName);
    CheckNotEquals(-1, Index, 'Item "'+ AItemName +'" could not be found before renaming!');
    FStartupList.Selected := FStartupList[Index];
    FStartupList.RenameItem(AItemName +'2');
    CheckEquals(AItemName +'2', FStartupList.Selected.Name, 'Item was not renamed correctly!');
    FStartupList.RenameItem(AItemName);
    CheckEquals(AItemName, FStartupList.Selected.Name, 'Item was not renamed correctly twice!');
  end;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    FStartupList.Load(Location);

  CheckRename(cHKCU);
  CheckRename(cHKCU_RUNONCE);
  CheckRename(STARTUP_USER +'.lnk');
{$IFNDEF DEBUG}
  CheckRename(cHKLM);
  CheckRename(cHKLM_RUNONCE);
  CheckRename(STARTUP_COMMON +'.lnk');

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    CheckRename(cHKLM32);
    CheckRename(cHKLM_RUNONCE32);
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.TestLocking;
begin
  FLockingSuccessful := False;

  // Start async loading
  FStartupList.OnSearchStart := TestLocking_SearchStart;
  FStartupList.Load(True);

  // Give the thread time to kill himself
  Delay(2000);
  CheckTrue(FLockingSuccessful, 'List was not locked!!');
end;

procedure TStartupListTest.AddTestItemEnabled(ALocation: TStartupLocation);
var
  Reg: TRegistry;
  LnkFile: TStartupLnkFile;

begin
{$IFDEF DEBUG}
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

procedure TStartupListTest.AddTestItemsEnabled();
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

initialization
  RegisterTest(TStartupListTest.Suite);
  //RegisterTest(TestStartupList.Suite);
  //RegisterTest(TestTTaskList.Suite);
end.

