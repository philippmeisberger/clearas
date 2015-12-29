unit ClearasAPITest;

interface

uses
  TestFramework, StrUtils, Registry, PMCWLanguageFile, ClearasAPI, ComObj,
  ActiveX, SysUtils, ShellAPI, Windows, PMCWOSUtils, KnownFolders, Taskschd,
  Variants, SyncObjs, ShlObj, Generics.Collections, Classes, Zip, CommCtrl,
  PMCWIniFileParser, WinSvc, Graphics, Forms;

type
  TestRootList = class(TTestCase)
  protected
    FRootList: TRootList<TRootItem>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEnable;
    procedure TestDisable;
    procedure TestExportItem;
    procedure TestExportBackup;
    procedure TestImportBackup;
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
  ShellExecute(0, 'open', 'regedit.exe', PChar('-s '+ AFileName), nil, SW_SHOWNORMAL);
end;

{ TestRootList }

procedure TestRootList.SetUp;
begin
  inherited SetUp;
  FRootList := TRootList<TRootItem>.Create;
end;

procedure TestRootList.TearDown;
begin
  FreeAndNil(FRootList);
  inherited TearDown;
end;

procedure TestRootList.TestDisable;
begin

end;

procedure TestRootList.TestEnable;
begin

end;

procedure TestRootList.TestExportBackup;
begin

end;

procedure TestRootList.TestExportItem;
begin

end;

procedure TestRootList.TestImportBackup;
const
  TestTaskFile = 'C:\Users\Phil\PMCW\Projekte\clearas\tests\TestTask.xml';

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


{ TestTTaskList }

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
  TestTaskFile = 'TestTask.xml';
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

initialization
  RegisterTest(TestTTaskList.Suite);
end.

