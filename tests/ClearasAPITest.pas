unit ClearasAPITest;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  TestFramework, Windows, Classes, Registry, ComObj, SysUtils, ShellAPI,
  Taskschd, WinSvc, Forms, ClearasAPI;

const
  cTestExe = 'C:\Windows\regedit.exe';

type
  TRootListTest = class(TTestCase)
  strict private
    FLockingSuccessful: Boolean;
    procedure TestLocking_SearchStart(Sender: TObject);
    procedure EnsureFileExportedAndDelete(const AFileName: string);
  protected
    FRootList: TRootList<TRootItem>;
    FTestItems: TStringList;
    procedure LoadItems(); virtual; abstract;
    procedure SelectItem(const AItemName: string);
    procedure TestDisable(const AItemName: string);
    procedure TestEnable(const AItemName: string);
    procedure TestDelete(const AItemName: string);
    procedure TestExport(const AItemName: string);
    procedure TestRename(const AItemName: string); virtual;
    procedure TestChangeFilePath(const AItemName: string);
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
    procedure AddTestItemEnabled(ALocation: TStartupLocation);
    procedure DeleteTestItem(ALocation: TStartupLocation);
    function GetItemName(ALocation: TStartupLocation): string;
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
end;

procedure TRootListTest.TearDown;
begin
  FreeAndNil(FRootList);
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
  Assert(AFileName <> '', 'FileName of exported file must not be empty!');
  CheckEquals(0, FindFirst(AFileName +'.*', faAnyFile - faDirectory, SearchResult), 'Exported file "'+ AFileName +'" does not exist!');
  Check(DeleteFile(PChar(SearchResult.Name)), 'Exported file could not be deleted!');
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

procedure TRootListTest.TestChangeItemFilePaths;
var
  i: Integer;

begin
  LoadItems();

  for i := 0 to FTestItems.Count - 1 do
    TestChangeFilePath(FTestItems[i]);
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

procedure TRootListTest.TestDeleteItems;
var
  i: Integer;

begin
  LoadItems();

  for i := 0 to FTestItems.Count - 1 do
    TestDelete(FTestItems[i]);

  CleanUp();
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
  CheckFalse(FRootList.Selected.Enabled, 'Before enabling item "'+ AItemName +'" Enabled must be False!');
  FRootList.EnableItem();
  CheckTrue(FRootList.Selected.Enabled, 'After enabling item "'+ AItemName +'" Enabled should be True!');
  Inc(EnabledCounter);
  CheckEquals(EnabledCounter, FRootList.EnabledItemsCount, 'After enabling item "'+ AItemName +'" EnabledItemsCount must be increased by 1!');
  CheckEquals(Counter, FRootList.Count, 'After enabling item "'+ AItemName +'" Count must not be changed!');
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
  FRootList.ExportList(ClassName);
  EnsureFileExportedAndDelete(ClassName);
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
  FRootList.ExportItem(FRootList.Selected.Name);
  EnsureFileExportedAndDelete(FRootList.Selected.Name);
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
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TStartupList.Create);

  FTestItems.Append(GetItemName(slHkcuRun));
  FTestItems.Append(GetItemName(slHkcuRunOnce));
  FTestItems.Append(GetItemName(slStartupUser));
{$IFNDEF DEBUG}
  FTestItems.Append(GetItemName(slHklmRun));
  FTestItems.Append(GetItemName(slHklmRunOnce));
  FTestItems.Append(GetItemName(slCommonStartup));

  if (TOSVersion.Architecture = arIntelX64) then
  begin
    FTestItems.Append(GetItemName(slHklmRun32));
    FTestItems.Append(GetItemName(slHklmRunOnce32));
  end;  //of begin
{$ENDIF}
end;

procedure TStartupListTest.TestImportBackup;
begin
  Check(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slStartupUser) + EXT_STARTUP_USER), STARTUP_USER +' file already exists!');
  CheckFalse(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slStartupUser) + EXT_STARTUP_USER), STARTUP_USER +' file already exists so it must not be possible to import it again!');
{$IFNDEF DEBUG}
  Check(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slCommonStartup) + EXT_STARTUP_COMMON), STARTUP_COMMON +' file already exists!');
  CheckFalse(TStartupList(FRootList).ImportBackup('..\..\data\'+ GetItemName(slCommonStartup) + EXT_STARTUP_COMMON), STARTUP_COMMON +' file already exists so it must not be possible to import it again!');
  CheckEquals(2, FRootList.Count, 'After importing 2 startup backup files there should be 2 items in the list!');
  TestDelete(GetItemName(slCommonStartup));
{$ELSE}
  CheckEquals(1, FRootList.Count, 'After importing 1 startup backup file there should be 1 items in the list!');
{$ENDIF}
  TestDelete(GetItemName(slStartupUser));
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

procedure TStartupListTest.LoadItems();
var
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
    TStartupList(FRootList).Load(Location);
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
    slHkcuRun:       Result := 'HKCU';
    slHkcuRunOnce:   Result := 'HKCU RunOnce';
    slHklmRun:       Result := 'HKLM';
    slHklmRun32:     Result := 'HKLM32';
    slHklmRunOnce:   Result := 'HKLM RunOnce';
    slHklmRunOnce32: Result := 'HKLM RunOnce32';
    slStartupUser:   Result := STARTUP_USER +'.lnk';
    slCommonStartup: Result := STARTUP_COMMON +'.lnk';
  end;  //of case
end;


{ TContextListTest }

procedure TContextListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TContextList.Create);

  FTestItems.Append(cShellCMItem);
  FTestItems.Append(cShellCMItemCascading);
  FTestItems.Append(cShellNewCMItem);
  FTestItems.Append(cShellExCMItem);
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

  Check(AFileExt.StartsWith('.'), 'FileExt must start with a "."!');
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

procedure TContextListTest.LoadItems();
begin
  TContextList(FRootList).LoadContextmenu(cShellFileExt, False);
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
  CheckEquals(AItemName +'2', FRootList.Selected.Caption, 'Item was not renamed correctly!');
  FRootList.RenameItem(AItemName);
  CheckEquals(AItemName, FRootList.Selected.Caption, 'Item was not renamed correctly twice!');
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
  FTestItems.Append('TestService');
end;

procedure TServiceListTest.LoadItems();
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

procedure TServiceListTest.AddEnabledTestItems;
begin
  CheckTrue(TServiceList(FRootList).Add(cTestExe, '', FTestItems[0]), 'Service already exists!');
  CheckEquals(FTestItems.Count, FRootList.Count, 'Actual item count differs from expected count!');
end;


{ TTaskListTest }

procedure TTaskListTest.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>(TTaskList.Create);
  FTestItems.Append('TestTask');
end;

procedure TTaskListTest.LoadItems();
begin
  TTaskList(FRootList).Search(False);
end;

procedure TTaskListTest.AddEnabledTestItems;
var
  TaskFileName: string;

begin
  TaskFileName := IncludeTrailingBackslash(ExtractFileDir(ExtractFileDir(GetCurrentDir()))) +'data\'+ FTestItems[0] +'.zip';
  Check(FileExists(TaskFileName), 'Task backup file "'+ TaskFileName +'" does not exist!');
  Check(TTaskList(FRootList).ImportBackup(TaskFileName), 'Task already exists!');
  CheckFalse(TTaskList(FRootList).ImportBackup(TaskFileName), 'Task already exists so it must not be imported again!');
  CheckEquals(FTestItems.Count, FRootList.Count, 'Actual item count differs from expected count!');
end;

initialization
  RegisterTest(TStartupListTest.Suite);
  RegisterTest(TContextListTest.Suite);
  RegisterTest(TServiceListTest.Suite);
  RegisterTest(TTaskListTest.Suite);
end.


