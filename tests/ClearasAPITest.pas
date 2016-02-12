unit ClearasAPITest;

interface

uses
  TestFramework, StrUtils, Registry, PMCWLanguageFile, ClearasAPI, ComObj,
  ActiveX, SysUtils, ShellAPI, Windows, PMCWOSUtils, KnownFolders, Taskschd,
  Variants, SyncObjs, ShlObj, Generics.Collections, Classes, Zip, CommCtrl,
  PMCWIniFileParser, WinSvc, Graphics, Forms;

const
  cTestExe         = 'C:\Windows\regedit.exe';

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
    procedure TestExportItem(const AItemName: string);
    procedure TestExportBackup;
    procedure TestImportBackup;
    procedure TestRename(const AItemName: string);
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
    procedure AddTestItemsEnabled();
    procedure DeleteTestItem(ALocation: TStartupLocation);
    function GetItemName(ALocation: TStartupLocation): string;
  public
    procedure SetUp; override;
  published
    procedure TestDisableItems;
    procedure TestEnableItems;
    procedure TestRenameItems;
    procedure TestChangeItemFilePaths;
    procedure TestDeleteItems;
    procedure CleanUp;
  end;

  {TestTTaskList = class(TTestCase)
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

procedure TRootListTest.TearDown;
begin
  FreeAndNil(FRootList);
  inherited TearDown;
end;

procedure TRootListTest.SelectItem(const AItemName: string);
var
  Index: Integer;

begin
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
  CheckEqualsString(cTestExe, FRootList.Selected.FileNameOnly, 'FileName of "'+ AItemName +'" does not match before renaming!');
  FRootList.ChangeItemFilePath(cNewTestFileName);
  CheckEqualsString(cNewTestFileName, FRootList.Selected.FileName, 'FileName of "'+ AItemName +'" does not match after renaming!');
  CheckEqualsString(cNewTestExe, FRootList.Selected.FileNameOnly, 'FileNameOnly of "'+ AItemName +'" does not match after renaming!');
  CheckEqualsString(cNewTestArgument, FRootList.Selected.Arguments, 'Arguments of "'+ AItemName +'" does not match after renaming!');
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

procedure TRootListTest.TestExportItem(const AItemName: string);
begin
  SelectItem(AItemName);
  FRootList.ExportItem(FRootList.Selected.Name);
  // TODO: TestExportItem
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

{ TestTTaskList }
{
procedure TestTTaskList.SetUp;
begin
  inherited SetUp;
  FTaskList := TTaskList.Create;
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
  AddTestItemsEnabled();

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

