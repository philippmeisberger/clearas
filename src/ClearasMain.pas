{ *********************************************************************** }
{                                                                         }
{ Clearas Main Unit                                                       }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Menus, Vcl.Graphics,
  Vcl.ClipBrd, Vcl.ImgList, Registry, StrUtils, System.ImageList, Winapi.CommCtrl,
  ClearasAPI, ExportListThread, PMCWAbout, PMCWLanguageFile, PMCWOSUtils,
  PMCWUpdater, PMCWDialogs;

const
  KEY_RECYCLEBIN = 'CLSID\{645FF040-5081-101B-9F08-00AA002F954E}\shell';

type
  { TMain }
  TMain = class(TForm, IChangeLanguageListener, IUpdateListener)
    PopupMenu: TPopupMenu;
    pmChangeStatus: TMenuItem;
    N1: TMenuItem;
    pmExport: TMenuItem;
    pmDelete: TMenuItem;
    MainMenu: TMainMenu;
    mmView: TMenuItem;
    mmRefresh: TMenuItem;
    N2: TMenuItem;
    pmCopyLocation: TMenuItem;
    mmHelp: TMenuItem;
    mmInfo: TMenuItem;
    mmEdit: TMenuItem;
    mmContext: TMenuItem;
    mmFile: TMenuItem;
    mmExportList: TMenuItem;
    N3: TMenuItem;
    mmClose: TMenuItem;
    mmExport: TMenuItem;
    mmAdd: TMenuItem;
    N4: TMenuItem;
    mmDate: TMenuItem;
    mmImport: TMenuItem;
    mmDelBackup: TMenuItem;
    mmDefault: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N6: TMenuItem;
    mmLang: TMenuItem;
    PageControl: TPageControl;
    tsStartup: TTabSheet;
    tsContext: TTabSheet;
    lwStartup: TListView;
    lStartup: TLabel;
    bCloseStartup: TButton;
    bDisableStartupItem: TButton;
    bDeleteStartupItem: TButton;
    bExportStartupItem: TButton;
    lCopy1: TLabel;
    lCopy2: TLabel;
    bExportContextItem: TButton;
    bDeleteContextItem: TButton;
    bCloseContext: TButton;
    bDisableContextItem: TButton;
    bEnableContextItem: TButton;
    lwContext: TListView;
    lContext: TLabel;
    lVersion2: TLabel;
    lVersion: TLabel;
    pbContextProgress: TProgressBar;
    cbContextExpert: TCheckBox;
    lWindows2: TLabel;
    lWindows: TLabel;
    mmUpdate: TMenuItem;
    N9: TMenuItem;
    mmInstallCertificate: TMenuItem;
    N10: TMenuItem;
    pmEdit: TMenuItem;
    mmReport: TMenuItem;
    pmOpenRegedit: TMenuItem;
    pmOpenExplorer: TMenuItem;
    IconList: TImageList;
    tsService: TTabSheet;
    lwService: TListView;
    lWindows3: TLabel;
    lService: TLabel;
    bExportServiceItem: TButton;
    bDeleteServiceItem: TButton;
    bCloseService: TButton;
    bDisableServiceItem: TButton;
    bEnableServiceItem: TButton;
    lCopy3: TLabel;
    lVersion3: TLabel;
    bEnableStartupItem: TButton;
    cbServiceExpert: TCheckBox;
    cbRunOnce: TCheckBox;
    QuickSearchIconList: TImageList;
    eContextSearch: TButtonedEdit;
    eServiceSearch: TButtonedEdit;
    pbServiceProgress: TProgressBar;
    tsTasks: TTabSheet;
    bCloseTasks: TButton;
    bDeleteTaskItem: TButton;
    bDisableTaskitem: TButton;
    bEnableTaskItem: TButton;
    bExportTaskItem: TButton;
    cbTaskExpert: TCheckBox;
    eTaskSearch: TButtonedEdit;
    lCopy4: TLabel;
    lTasks: TLabel;
    lVersion4: TLabel;
    lWindows4: TLabel;
    lwTasks: TListView;
    pbTaskProgress: TProgressBar;
    pmRename: TMenuItem;
    pmChangeIcon: TMenuItem;
    pmDeleteIcon: TMenuItem;
    N5: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bCloseStartupClick(Sender: TObject);
    procedure bEnableStartupItemClick(Sender: TObject);
    procedure bEnableContextItemClick(Sender: TObject);
    procedure bEnableServiceItemClick(Sender: TObject);
    procedure bEnableTaskItemClick(Sender: TObject);
    procedure bDeleteItemClick(Sender: TObject);
    procedure bDisableItemClick(Sender: TObject);
    procedure bExportStartupItemClick(Sender: TObject);
    procedure bExportItemClick(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure lwContextDblClick(Sender: TObject);
    procedure lwContextSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lwServiceDblClick(Sender: TObject);
    procedure lwServiceSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lwStartupDblClick(Sender: TObject);
    procedure ListViewKeyPress(Sender: TObject; var Key: Char);
    procedure lwStartupSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mmAddClick(Sender: TObject);
    procedure mmContextClick(Sender: TObject);
    procedure mmDateClick(Sender: TObject);
    procedure mmDelBackupClick(Sender: TObject);
    procedure mmExportListClick(Sender: TObject);
    procedure mmExportClick(Sender: TObject);
    procedure mmImportClick(Sender: TObject);
    procedure mmRefreshClick(Sender: TObject);
    procedure mmDefaultClick(Sender: TObject);
    procedure mmInfoClick(Sender: TObject);
    procedure mmUpdateClick(Sender: TObject);
    procedure mmInstallCertificateClick(Sender: TObject);
    procedure mmReportClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure pmChangeStatusClick(Sender: TObject);
    procedure pmCopyLocationClick(Sender: TObject);
    procedure pmDeleteClick(Sender: TObject);
    procedure pmEditClick(Sender: TObject);
    procedure pmOpenRegeditClick(Sender: TObject);
    procedure pmOpenExplorerClick(Sender: TObject);
    procedure lCopy1MouseLeave(Sender: TObject);
    procedure lCopy1MouseEnter(Sender: TObject);
    procedure lCopy1Click(Sender: TObject);
    procedure eSearchRightButtonClick(Sender: TObject);
    procedure lwTasksSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lwTasksDblClick(Sender: TObject);
    procedure pmRenameClick(Sender: TObject);
    procedure pmChangeIconClick(Sender: TObject);
    procedure pmDeleteIconClick(Sender: TObject);
  private
    FStartup: TStartupList;
    FContext: TContextList;
    FService: TServiceList;
    FTasks: TTaskList;
    FLang: TLanguageFile;
    FUpdateCheck: TUpdateCheck;
    function CreateStartupUserBackup(): Boolean;
    function DeleteItem(AConfirmMessageId: Word): Boolean;
    procedure DeleteStartupItem(Sender: TObject);
    function DisableItem(): Boolean;
    function EnableItem(): Boolean;
    function GetSelectedItem(): TRootItem;
    function GetSelectedList(): TRootList<TRootItem>;
    function GetSelectedListView(): TListView;
    procedure LoadItems(ATotalRefresh: Boolean = True);
    procedure OnContextSearchStart(Sender: TObject; const AMax: Cardinal);
    procedure OnContextSearchEnd(Sender: TObject);
    procedure OnContextItemChanged(Sender: TObject; ANewStatus: TItemStatus);
    procedure OnExportListStart(Sender: TObject; const APageControlIndex: Cardinal);
    procedure OnExportListEnd(Sender: TObject; const APageControlIndex: Cardinal);
    procedure OnExportListError(Sender: TObject; AErrorMessage: string);
    procedure OnSearchError(Sender: TObject; AErrorMessage: string);
    procedure OnStartupSearchStart(Sender: TObject; const AMax: Cardinal);
    procedure OnStartupSearchEnd(Sender: TObject);
    procedure OnStartupItemChanged(Sender: TObject; ANewStatus: TItemStatus);
    procedure OnServiceSearchStart(Sender: TObject; const AMax: Cardinal);
    procedure OnServiceSearchEnd(Sender: TObject);
    procedure OnServiceItemChanged(Sender: TObject; ANewStatus: TItemStatus);
    procedure OnTaskSearchStart(Sender: TObject; const AMax: Cardinal);
    procedure OnTaskSearchEnd(Sender: TObject);
    procedure OnTaskItemChanged(Sender: TObject; ANewStatus: TItemStatus);
    procedure OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
    procedure SetLanguage(Sender: TObject);
    function ShowExportItemDialog(): Boolean;
    procedure ShowColumnDate(AListView: TListView; AShow: Boolean = True);
    function UpdateContextPath(): Boolean;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

{ TMain }

{ TMain.FormCreate

  VCL event that is called when form is being created. }

procedure TMain.FormCreate(Sender: TObject);
begin
  // Setup languages
  FLang := TLanguageFile.Create(Self);
  FLang.BuildLanguageMenu(MainMenu, mmLang);
  FLang.Update();

  // Init update notificator
  FUpdateCheck := TUpdateCheck.Create(Self, 'Clearas', FLang);

  // Check for update on startup
  FUpdateCheck.CheckForUpdate(False);

  // Init Clearas instances
  FStartup := TStartupList.Create;

  // Link search events
  with FStartup do
  begin
    OnChanged := OnStartupItemChanged;
    OnSearchError := Self.OnSearchError;
    OnSearchFinish := OnStartupSearchEnd;
    OnSearchStart := OnStartupSearchStart;
  end;  //of with

  FContext := TContextList.Create;

  // Link search events
  with FContext do
  begin
    OnChanged := OnContextItemChanged;
    OnSearchError := Self.OnSearchError;
    OnSearchFinish := OnContextSearchEnd;
    OnSearchStart := OnContextSearchStart;
  end;  //of with

  FService := TServiceList.Create;

  // Link search events
  with FService do
  begin
    OnChanged := OnServiceItemChanged;
    OnSearchError := Self.OnSearchError;
    OnSearchFinish := OnServiceSearchEnd;
    OnSearchStart := OnServiceSearchStart;
  end;  //of with

  // Task feature only for Windows >= Vista
  if TOSVersion.Check(6) then
  begin
    FTasks := TTaskList.Create;

    // Link search events
    with FTasks do
    begin
      OnChanged := OnTaskItemChanged;
      OnSearchError := Self.OnSearchError;
      OnSearchFinish := OnTaskSearchEnd;
      OnSearchStart := OnTaskSearchStart;
    end;  //of with
  end;  //of begin

  // Set title
  Caption := Application.Title + PLATFORM_ARCH;
end;

{ TMain.FormDestroy

  VCL event that is called when form is being destroyed. }

procedure TMain.FormDestroy(Sender: TObject);
begin
  FService.Free;
  FContext.Free;
  FStartup.Free;
  FUpdateCheck.Free;
  FLang.Free;
end;

{ TMain.FormShow

  VCL event that is called when form is shown. }

procedure TMain.FormShow(Sender: TObject);
begin
  // Get version of Windows including service pack
  lWindows.Caption := TOSVersion.Name +' '+ Win32CSDVersion;
  lWindows2.Caption := lWindows.Caption;
  lWindows3.Caption := lWindows.Caption;
  lWindows4.Caption := lWindows.Caption;

  // Show "date of deactivation" only on Vista and later
  mmDate.Enabled := TOSVersion.Check(6);

  // Update Clearas recycle bin context menu entry
  mmContext.Checked := UpdateContextPath();

  // Load autostart
  FStartup.Load();
end;

{ private TMain.OnUpdate

  Event that is called by TUpdateCheck when an update is available. }

procedure TMain.OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
var
  Updater: TUpdate;

begin
  mmUpdate.Caption := FLang.GetString(24);

  // Ask user to permit download
  if (FLang.ShowMessage(FLang.Format(21, [ANewBuild]), FLang.GetString(22),
    mtConfirmation) = IDYES) then
  begin
    // init TUpdate instance
    Updater := TUpdate.Create(Self, FLang);

    try
      // Set updater options
      with Updater do
      begin
        FileNameLocal := 'Clearas.exe';

      {$IFDEF WIN64}
        FileNameRemote := 'clearas64.exe';
      {$ELSE}
        // Ask user to permit download of 64-Bit version
        if ((TOSVersion.Architecture = arIntelX64) and (FLang.ShowMessage(
          FLang.Format([34, 35], ['Clearas']), mtConfirmation) = IDYES)) then
          FileNameRemote := 'clearas64.exe'
        else
          FileNameRemote := 'clearas.exe';
      {$ENDIF}
      end;  //of begin

      // Successfully downloaded update?
      if Updater.Execute() then
      begin
        // Caption "Search for update"
        mmUpdate.Caption := FLang.GetString(15);
        mmUpdate.Enabled := False;
      end;  //of begin

    finally
      Updater.Free;
    end;  //of try
  end;  //of begin
end;

{ private TMain.CreateStartupUserBackup

  Creates a special .lnk backup for currently selected activated startup
  user entry. }

function TMain.CreateStartupUserBackup(): Boolean;
begin
  Result := False;

  try
    // Nothing selected?
    if (not Assigned(lwStartup.ItemFocused) or not Assigned(FStartup.Selected)) then
      raise EInvalidItem.Create('No item selected!');

    // Special .lnk file backup only for activated startup user entries!
    if (FStartup.Selected.Enabled and (FStartup.Selected is TStartupUserItem)) then
    begin
      FStartup.Selected.ExportItem('');
      FLang.ShowMessage(FLang.Format(42, [(FStartup.Selected as TStartupUserItem).LnkFile.BackupLnk]));

      bExportStartupItem.Enabled := False;
      pmExport.Enabled := False;
      Result := True;
    end  //of begin
    else
      // Default .reg file export
      Result := ShowExportItemDialog();

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([95, 18]), FLang.GetString(53), mtWarning);

    on E: EStartupException do
      FLang.ShowException(FLang.GetString([95, 18]), FLang.GetString(43));

    on E: Exception do
      FLang.ShowException(FLang.GetString([95, 18]), E.Message);
  end;  //of try
end;

{ private TMain.DeleteItem

  Deletes the current selected item. }

function TMain.DeleteItem(AConfirmMessageId: Word): Boolean;
var
  Answer: Integer;
  ListView: TListView;
  RootList: TRootList<TRootItem>;

begin
  Result := False;

  try
    ListView := GetSelectedListView();
    RootList := GetSelectedList();

    // Nothing selected?
    if (not Assigned(ListView.ItemFocused) or not Assigned(RootList.Selected)) then
      raise EInvalidItem.Create('No item selected!');

    // Confirm deletion of item
    if (FLang.ShowMessage(FLang.Format([AConfirmMessageId],
      [ListView.ItemFocused.SubItems[0]]), FLang.GetString([49, 50]), mtCustom) = IDYES) then
    begin
      // Ask user to export item
      Answer := FLang.ShowMessage(FLang.GetString(52), mtConfirmation);

      // Abort if user clicks cancel!
      if (((Answer = IDYES) and ShowExportItemDialog()) or (Answer = IDNO)) then
        // Successfully deleted item physically?
        if RootList.DeleteItem() then
        begin
          // Delete item from TListView
          ListView.DeleteSelected();
          ListView.ItemFocused := nil;
          Result := True;
        end  //of begin
        else
          raise Exception.Create('Unknown error!');
    end;  //of begin

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([96, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(FLang.GetString([96, 18]), E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([96, 18]), E.Message);
  end;  //of try
end;

{ private TMain.DeleteStartupItem

  Deletes currently selected startup item. }

procedure TMain.DeleteStartupItem(Sender: TObject);
var
  DelBackup, BackupExists: Boolean;
  Answer: Integer;

begin
  DelBackup := True;

  try
    // Nothing selected?
    if (not Assigned(lwStartup.ItemFocused) or not Assigned(FStartup.Selected)) then
      raise EInvalidItem.Create('No item selected!');

    // Confirm deletion of item
    if (FLang.ShowMessage(FLang.Format([48], [FStartup.Selected.Name]),
      FLang.GetString([49, 50]), mtCustom) = IDYES) then
    begin
      // Save the DeleteBackup flag
      DelBackup := FStartup.DeleteBackup;
      BackupExists := FStartup.BackupExists();

      // Skip export dialog for enabled startup user item with exising backup
      if ((FStartup.Selected is TStartupUserItem) and FStartup.Selected.Enabled
        and BackupExists) then
        Answer := IDCANCEL
      else
        Answer := FLang.ShowMessage(FLang.GetString(52), mtConfirmation);

      // Export item and only continue if this has succeeded
      if (Answer = IDYES) then
        if CreateStartupUserBackup() then
          FStartup.DeleteBackup := False
        else
          Exit;

      // Ask user to delete old existing backup
      if ((Answer = IDCANCEL) or ((FStartup.Selected is TStartupUserItem)
        and not FStartup.Selected.Enabled and BackupExists)) then
        FStartup.DeleteBackup := (FLang.ShowMessage(FLang.GetString(44),
          mtConfirmation) = IDYES);

      // Successfully deleted item physically?
      if FStartup.DeleteItem() then
      begin
        // Delete item from TListView
        lwStartup.DeleteSelected();
        lwStartup.ItemFocused := nil;
      end  //of begin
      else
        raise Exception.Create('Unknown error!');
    end;  //of begin

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([96, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(FLang.GetString([96, 18]), E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([96, 18]), E.Message);
  end;  //of try

  // Restore the DeleteBackup flag
  if (FStartup.DeleteBackup <> DelBackup) then
    FStartup.DeleteBackup := DelBackup;
end;

{ private TMain.DisableItem

  Disables the current selected item. }

function TMain.DisableItem(): Boolean;
var
  ListView: TListView;
  RootList: TRootList<TRootItem>;

begin
  Result := False;

  try
    ListView := GetSelectedListView();
    RootList := GetSelectedList();

    // Nothing selected?
    if not Assigned(ListView.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    // Successfully disabled item?
    if RootList.DisableItem() then
    begin
      // Change item visual status
      ListView.ItemFocused.Caption := RootList.Selected.GetStatus(FLang);

      // Update TListView
      ListView.OnSelectItem(Self, ListView.ItemFocused, True);
      Result := True;
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([94, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(FLang.GetString([94, 18]), E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([94, 18]), E.Message);
  end;  //of try
end;

{ private TMain.EnableItem

  Enables the current selected item. }

function TMain.EnableItem(): Boolean;
var
  ListView: TListView;
  RootList: TRootList<TRootItem>;

begin
  Result := False;

  try
    ListView := GetSelectedListView();
    RootList := GetSelectedList();

    // Nothing selected?
    if not Assigned(ListView.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    // Successfully activated item?
    if RootList.EnableItem() then
    begin
      // Change item visual status
      ListView.ItemFocused.Caption := RootList.Selected.GetStatus(FLang);

      // Update TListView
      ListView.OnSelectItem(Self, ListView.ItemFocused, True);
      Result := True;
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([93, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(FLang.GetString([93, 18]), E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([93, 18]), E.Message);
  end;  //of try
end;

{ private TMain.GetSelectedItem

  Returns the current selected TRootItem. }

function TMain.GetSelectedItem(): TRootItem;
var
  Item: TRootItem;
  List: TRootList<TRootItem>;

begin
  List := GetSelectedList();
  Item := List.Selected;

  if not Assigned(Item) then
    raise EInvalidItem.Create('No item selected!');

  Result := Item;
end;

{ private TMain.GetSelectedList

  Returns the current selected TRootList. }

function TMain.GetSelectedList(): TRootList<TRootItem>;
var
  List: TRootList<TRootItem>;

begin
  List := nil;

  case PageControl.ActivePageIndex of
    0: List := TRootList<TRootItem>(FStartup);
    1: List := TRootList<TRootItem>(FContext);
    2: List := TRootList<TRootItem>(FService);
    3: List := TRootList<TRootItem>(FTasks);
  end;  //of case

  if not Assigned(List) then
    raise EInvalidItem.Create('No list selected!');

  Result := List;
end;

{ private TMain.GetSelectedListView

  Returns the current selected TListView. }

function TMain.GetSelectedListView(): TListView;
var
  List: TListView;

begin
  List := nil;

  case PageControl.ActivePageIndex of
    0: List := lwStartup;
    1: List := lwContext;
    2: List := lwService;
    3: List := lwTasks;
  end;  //of case

  if not Assigned(List) then
    raise EInvalidItem.Create('No ListView selected!');

  Result := List;
end;

{ private TMain.LoadItems

  Loads items and brings them into a TListView. }

procedure TMain.LoadItems(ATotalRefresh: Boolean = True);
begin
  try
    // Make a total refresh or just use cached items
    if ATotalRefresh then
    case PageControl.ActivePageIndex of
      0: if Assigned(FStartup) then
           FStartup.Load(cbRunOnce.Checked);

      1: if Assigned(FContext) then
           FContext.Load(cbContextExpert.Checked);

      2: if Assigned(FService) then
           FService.Load(cbServiceExpert.Checked);

      3: if Assigned(FTasks) then
           FTasks.Load(cbTaskExpert.Checked);
    end  //of case
    else
      GetSelectedList().OnSearchFinish(Self);

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([77, 18]), FLang.GetString(53), mtWarning);
  end;  //of try
end;

{ private TMain.OnContextSearchStart

  Event that is called when search starts. }

procedure TMain.OnContextSearchStart(Sender: TObject; const AMax: Cardinal);
begin
  mmLang.Enabled := False;
  cbContextExpert.Enabled := False;
  lwContext.Cursor := crHourGlass;

  // Show progress bar
  if cbContextExpert.Checked then
  begin
    eContextSearch.Visible := False;
    pbContextProgress.Visible := True;
  end;  //of begin
end;

{ private TMain.OnContextSearchEnd

  Event that is called when search ends. }

procedure TMain.OnContextSearchEnd(Sender: TObject);
var
  i: Integer;
  Text: string;

begin
  // Clear all visual data
  lwContext.Clear;

  // Print all information about context menu entries
  for i := 0 to FContext.Count - 1 do
  begin
    // Show name or caption of item?
    if (FContext.Items[i].Caption <> '') then
      Text := FContext[i].Caption
    else
      Text := FContext[i].Name;

    // Filter items
    if ((eContextSearch.Text = '') or
      (AnsiContainsText(Text, eContextSearch.Text) or
      AnsiContainsText(FContext[i].LocationRoot, eContextSearch.Text))) then
      with lwContext.Items.Add do
      begin
        Caption := FContext[i].GetStatus(FLang);
        SubItems.Append(Text);
        SubItems.Append(FContext[i].LocationRoot);
        SubItems.Append(FContext[i].TypeOf);
      end; //of with
  end;  //of for

  // Update some VCL
  mmLang.Enabled := True;
  cbContextExpert.Enabled := True;
  lwContext.Cursor := crDefault;

  // Hide progress bar
  if cbContextExpert.Checked then
  begin
    pbContextProgress.Visible := False;
    eContextSearch.Visible := True;
  end;  //of begin

  // Sort!
  lwContext.AlphaSort();
end;

{ private TMain.OnContextItemChanged

  Event method that is called when item status has been changed. }

procedure TMain.OnContextItemChanged(Sender: TObject; ANewStatus: TItemStatus);
begin
  // Refresh counter label
  lwContext.Columns[1].Caption := FLang.Format(87, [FContext.Enabled,
    FContext.Count]);

  // Change button states
  case ANewStatus of
    stEnabled:
      begin
        bEnableContextItem.Enabled := False;
        bDisableContextItem.Enabled := True;
        pmChangeStatus.Caption := bDisableContextItem.Caption;
      end;

    stDisabled:
      begin
        bEnableContextItem.Enabled := True;
        bDisableContextItem.Enabled := False;
        pmChangeStatus.Caption := bEnableContextItem.Caption;
      end;

    stDeleted:
      begin
        bEnableContextItem.Enabled := False;
        bDisableContextItem.Enabled := False;
        bDeleteContextItem.Enabled := False;
        bExportContextItem.Enabled := False;
      end;
  end;  //of case
end;

{ private TMain.OnExportListStart

  Event that is called when export list starts. }

procedure TMain.OnExportListStart(Sender: TObject; const APageControlIndex: Cardinal);
begin
  PageControl.Pages[APageControlIndex].Cursor := crHourGlass;

  case APageControlIndex of
    0: lwStartup.Cursor := crHourGlass;

    1: begin
         eContextSearch.Visible := False;
         pbContextProgress.Visible := True;
         lwContext.Cursor := crHourGlass;
       end;

    2: begin
         eServiceSearch.Visible := False;
         pbServiceProgress.Visible := True;
         lwService.Cursor := crHourGlass;
       end;

    3: begin
         eTaskSearch.Visible := False;
         pbTaskProgress.Visible := True;
         lwTasks.Cursor := crHourGlass;
       end;
  end;  //of case
end;

{ private TMain.OnExportListEnd

  Event that is called when export list ends. }

procedure TMain.OnExportListEnd(Sender: TObject; const APageControlIndex: Cardinal);
begin
  PageControl.Pages[APageControlIndex].Cursor := crDefault;

  case APageControlIndex of
    0: lwStartup.Cursor := crDefault;

    1: begin
         pbContextProgress.Visible := False;
         eContextSearch.Visible := True;
         lwContext.Cursor := crDefault;
       end;

    2: begin
         pbServiceProgress.Visible := False;
         eServiceSearch.Visible := True;
         lwService.Cursor := crDefault;
       end;

    3: begin
         pbTaskProgress.Visible := False;
         eTaskSearch.Visible := True;
         lwTasks.Cursor := crDefault;
       end;
  end;  //of case
end;

{ private TMain.OnExportListError

  Event method that is called when export thread has failed. }

procedure TMain.OnExportListError(Sender: TObject; AErrorMessage: string);
begin
  FLang.ShowException(FLang.GetString([95, 18]), AErrorMessage);
end;

{ private TMain.OnStartupItemChanged

  Event method that is called when item status has been changed. }

procedure TMain.OnStartupItemChanged(Sender: TObject; ANewStatus: TItemStatus);
begin
  // Refresh counter label
  lwStartup.Columns[1].Caption := FLang.Format(88, [FStartup.Enabled,
    FStartup.Count]);

  // Change button states
  case ANewStatus of
    stEnabled:
      begin
        bEnableStartupItem.Enabled := False;
        bDisableStartupItem.Enabled := True;
        bExportStartupItem.Enabled := not FStartup.BackupExists;
        pmExport.Enabled := bExportStartupItem.Enabled;
        pmChangeStatus.Caption := bDisableStartupItem.Caption;
      end;

    stDisabled:
      begin
        bEnableStartupItem.Enabled := True;
        bDisableStartupItem.Enabled := False;
        pmChangeStatus.Caption := bEnableStartupItem.Caption;
      end;

    stDeleted:
      begin
        bEnableStartupItem.Enabled := False;
        bDisableStartupItem.Enabled := False;
        bDeleteStartupItem.Enabled := False;
        bExportStartupItem.Enabled := False;
      end;
  end;  //of case
end;

{ private TMain.OnStartupSearchStart

  Event that is called when search starts. }

procedure TMain.OnStartupSearchStart(Sender: TObject; const AMax: Cardinal);
begin
  mmLang.Enabled := False;
  mmImport.Enabled := False;
  cbRunOnce.Enabled := False;
  lwStartup.Cursor := crHourGlass;
end;

{ private TMain.OnStartupSearchEnd

  Event that is called when search ends. }

procedure TMain.OnStartupSearchEnd(Sender: TObject);
var
  Icon: TIcon;
  i: Integer;

begin
  // Clear all visual data
  lwStartup.Clear;
  IconList.Clear;
  Icon := TIcon.Create;

  try
    // Print all information about startup entires
    for i := 0 to FStartup.Count - 1 do
      with lwStartup.Items.Add do
      begin
        Caption := FStartup[i].GetStatus(FLang);
        SubItems.Append(FStartup[i].Name);
        SubItems.Append(FStartup[i].FileName);
        SubItems.Append(FStartup[i].TypeOf);

        // Show deactivation timestamp?
        if mmDate.Checked then
          SubItems.Append(FStartup[i].Time);

        // Get icon of program
        Icon.Handle := FStartup[i].Icon;
        ImageIndex := IconList.AddIcon(Icon);
      end;  //of with

  finally
    Icon.Free;
  end;  //of try

  // Update some VCL
  mmImport.Enabled := True;
  mmLang.Enabled := True;
  cbRunOnce.Enabled := True;
  lwStartup.Cursor := crDefault;

  // Sort!
  lwStartup.AlphaSort();
end;

{ private TMain.OnServiceItemChanged

  Event method that is called when item status has been changed. }

procedure TMain.OnServiceItemChanged(Sender: TObject; ANewStatus: TItemStatus);
begin
  // Refresh counter label
  lwService.Columns[1].Caption := FLang.Format(56, [FService.Enabled,
    FService.Count]);

  // Change button states
  case ANewStatus of
    stEnabled:
      begin
        bEnableServiceItem.Enabled := False;
        bDisableServiceItem.Enabled := True;
        pmChangeStatus.Caption := bDisableServiceItem.Caption;
      end;

    stDisabled:
      begin
        bEnableServiceItem.Enabled := True;
        bDisableServiceItem.Enabled := False;
        pmChangeStatus.Caption := bEnableServiceItem.Caption;
      end;

    stDeleted:
      begin
        bEnableServiceItem.Enabled := False;
        bDisableServiceItem.Enabled := False;
        bDeleteServiceItem.Enabled := False;
        bExportServiceItem.Enabled := False;
      end;
  end;  //of case
end;

{ private TMain.OnServiceSearchStart

  Event that is called when search starts. }

procedure TMain.OnServiceSearchStart(Sender: TObject; const AMax: Cardinal);
begin
  mmLang.Enabled := False;
  cbServiceExpert.Enabled := False;
  lwService.Cursor := crHourGlass;

  // Show progress bar
  if cbServiceExpert.Checked then
  begin
    eServiceSearch.Visible := False;
    pbServiceProgress.Visible := True;
  end;  //of begin
end;

{ private TMain.OnServiceSearchEnd

  Event that is called when search ends. }

procedure TMain.OnServiceSearchEnd(Sender: TObject);
var
  i: Integer;
  Text: string;

begin
  // Clear all visual data
  lwService.Clear;

  // Print all information about service items
  for i := 0 to FService.Count - 1 do
  begin
    // Show name or caption of item?
    if (FService[i].Caption <> '') then
      Text := FService[i].Caption
    else
      Text := FService[i].Name;

    // Filter items
    if ((eServiceSearch.Text = '') or
      (AnsiContainsText(Text, eServiceSearch.Text))) then
      with lwService.Items.Add do
      begin
        Caption := FService[i].GetStatus(FLang);
        SubItems.Append(Text);
        SubItems.Append(FService[i].FileName);
        SubItems.Append(FService[i].GetStartText(FLang));

        // Show deactivation timestamp?
        if mmDate.Checked then
          SubItems.Append(FService[i].Time);
      end;  //of with
    end;  //of for

  mmLang.Enabled := True;
  cbServiceExpert.Enabled := True;
  lwService.Cursor := crDefault;

  // Hide progress bar
  if cbServiceExpert.Checked then
  begin
    pbServiceProgress.Visible := False;
    eServiceSearch.Visible := True;
  end;  //of begin

  // Sort!
  lwService.AlphaSort();
end;

{ private TMain.OnSearchError

  Event method that is called when search thread has failed. }

procedure TMain.OnSearchError(Sender: TObject; AErrorMessage: string);
begin
  FLang.ShowException(FLang.GetString([77, 18]), AErrorMessage);
end;

{ private TMain.OnTaskItemChanged

  Event method that is called when item status has been changed. }

procedure TMain.OnTaskItemChanged(Sender: TObject; ANewStatus: TItemStatus);
begin
  // Refresh counter label
  lwTasks.Columns[1].Caption := FLang.Format(116, [FTasks.Enabled, FTasks.Count]);

  // Change button states
  case ANewStatus of
    stEnabled:
      begin
        bEnableTaskItem.Enabled := False;
        bDisableTaskitem.Enabled := True;
        pmChangeStatus.Caption := bDisableTaskitem.Caption;
      end;

    stDisabled:
      begin
        bEnableTaskItem.Enabled := True;
        bDisableTaskitem.Enabled := False;
        pmChangeStatus.Caption := bEnableTaskItem.Caption;
      end;

    stDeleted:
      begin
        bEnableTaskItem.Enabled := False;
        bDisableTaskitem.Enabled := False;
        bDeleteTaskItem.Enabled := False;
        bExportTaskItem.Enabled := False;
      end;
  end;  //of case
end;

{ private TMain.OnTaskSearchEnd

  Event that is called when export list ends. }

procedure TMain.OnTaskSearchEnd(Sender: TObject);
var
  i: Integer;
  Text: string;

begin
  // Clear all visual data
  lwTasks.Clear;

  // Print all information about task items
  for i := 0 to FTasks.Count - 1 do
  begin
    Text := FTasks[i].Name;

    // Filter items
    if ((eTaskSearch.Text = '') or (AnsiContainsText(Text, eTaskSearch.Text))) then
      with lwTasks.Items.Add do
      begin
        Caption := FTasks[i].GetStatus(FLang);
        SubItems.Append(Text);
        SubItems.Append(FTasks[i].FileName);
        SubItems.Append(FTasks[i].Location);
      end; //of with
  end;  //of for

  // Update some VCL
  mmImport.Enabled := True;
  mmLang.Enabled := True;
  cbTaskExpert.Enabled := True;
  lwTasks.Cursor := crDefault;

  // Hide progress bar
  if cbTaskExpert.Checked then
  begin
    pbTaskProgress.Visible := False;
    eTaskSearch.Visible := True;
  end;  //of begin

  // Sort!
  lwTasks.AlphaSort();
end;

{ private TMain.OnTaskSearchStart

  Event that is called when search starts. }

procedure TMain.OnTaskSearchStart(Sender: TObject; const AMax: Cardinal);
begin
  mmLang.Enabled := False;
  mmImport.Enabled := False;
  cbTaskExpert.Enabled := False;
  lwTasks.Cursor := crHourGlass;

  // Show progress bar
  if cbTaskExpert.Checked then
  begin
    eTaskSearch.Visible := False;
    pbTaskProgress.Visible := True;
  end;  //of begin
end;

{ private TMain.SetLanguage

  Updates all component captions with new language text. }

procedure TMain.SetLanguage(Sender: TObject);
begin
  with FLang do
  begin
    // File menu labels
    mmFile.Caption := GetString(33);

    case PageControl.ActivePageIndex of
      0,2: mmAdd.Caption := GetString(69);
      1:   mmAdd.Caption := GetString(105);
      3:   mmAdd.Caption := GetString(117);
    end;  //of case

    mmImport.Caption := GetString(70);
    mmExport.Caption := GetString(71);
    mmExportlist.Caption := GetString(72);
    mmClose.Caption := GetString(73);

    // Edit menu labels
    mmEdit.Caption := GetString(74);
    mmContext.Caption := GetString(75);
    mmDelBackup.Caption := GetString(76);

    // View menu labels
    mmView.Caption := GetString(10);
    mmRefresh.Caption := GetString(77);
    mmDefault.Caption := GetString(78);
    mmDate.Caption := GetString(80);
    cbRunOnce.Caption := GetString(81);
    mmLang.Caption := GetString(25);

    // Help menu labels
    mmHelp.Caption := GetString(14);
    mmUpdate.Caption := GetString(15);
    mmInstallCertificate.Caption := GetString(16);
    mmReport.Caption := GetString(26);
    mmInfo.Caption := Format(17, [Application.Title]);

    // "Startup" tab TButton labels
    tsStartup.Caption := GetString(83);
    bEnableStartupItem.Caption := GetString(93);
    bDisableStartupItem.Caption := GetString(94);
    bExportStartupItem.Caption := GetString(95);
    bDeleteStartupItem.Caption := GetString(96);
    bCloseStartup.Caption := mmClose.Caption;

    // "Startup" tab TListView labels
    lStartup.Caption := GetString(82);
    lwStartup.Columns[0].Caption := GetString(91);
    lwStartup.Columns[2].Caption := StripHotkey(mmFile.Caption);
    lwStartup.Columns[3].Caption := GetString(92);
    lCopy1.Hint := GetString(29);

    // "Context menu" tab TButton labels
    tsContext.Caption := GetString(84);
    bEnableContextItem.Caption := bEnableStartupItem.Caption;
    bDisableContextItem.Caption := bDisableStartupItem.Caption;
    bExportContextItem.Caption := bExportStartupItem.Caption;
    bDeleteContextItem.Caption := bDeleteStartupItem.Caption;
    bCloseContext.Caption := bCloseStartup.Caption;
    cbContextExpert.Caption := GetString(89);
    eContextSearch.TextHint := GetString(63);

    // "Context menu" tab TListView labels
    lContext.Caption := GetString(86);
    lwContext.Columns[0].Caption := lwStartup.Columns[0].Caption;
    lwContext.Columns[2].Caption := GetString(90);
    lwContext.Columns[3].Caption := lwStartup.Columns[3].Caption;
    lCopy2.Hint := lCopy1.Hint;

    // "Service" tab TButton labels
    tsService.Caption := GetString(60);
    lService.Caption := lStartup.Caption;
    bEnableServiceItem.Caption := bEnableStartupItem.Caption;
    bDisableServiceItem.Caption := bDisableStartupItem.Caption;
    bExportServiceItem.Caption := bExportStartupItem.Caption;
    bDeleteServiceItem.Caption := bDeleteStartupItem.Caption;
    bCloseService.Caption := bCloseStartup.Caption;
    cbServiceExpert.Caption := cbContextExpert.Caption;

    // "Service" tab TListView labels
    lwService.Columns[0].Caption := lwStartup.Columns[0].Caption;
    lwService.Columns[2].Caption := lwStartup.Columns[2].Caption;
    lwService.Columns[3].Caption := GetString(59);
    lCopy3.Hint := lCopy1.Hint;
    eServiceSearch.TextHint := eContextSearch.TextHint;

    // "Tasks" tab TButton labels
    tsTasks.Caption := GetString(114);
    lTasks.Caption := GetString(115);
    bEnableTaskItem.Caption := bEnableStartupItem.Caption;
    bDisableTaskitem.Caption := bDisableStartupItem.Caption;
    bExportTaskItem.Caption := bExportStartupItem.Caption;
    bDeleteTaskItem.Caption := bDeleteStartupItem.Caption;
    bCloseTasks.Caption := bCloseStartup.Caption;
    cbTaskExpert.Caption := cbContextExpert.Caption;

    // "Tasks" tab TListView labels
    lwTasks.Columns[0].Caption := lwStartup.Columns[0].Caption;
    lwTasks.Columns[2].Caption := lwStartup.Columns[2].Caption;
    lwTasks.Columns[3].Caption := lwContext.Columns[2].Caption;
    lCopy4.Hint := lCopy1.Hint;
    eTaskSearch.TextHint := eContextSearch.TextHint;

    // Popup menu labels
    pmChangeStatus.Caption := bDisableStartupItem.Caption;
    pmOpenRegedit.Caption := GetString(66);
    pmOpenExplorer.Caption := GetString(51);
    pmEdit.Caption := GetString(104);
    pmExport.Caption := mmExport.Caption;
    pmDelete.Caption := bDeleteStartupItem.Caption;
    pmCopyLocation.Caption := GetString(106);
    pmChangeIcon.Caption := GetString(121);
    pmDeleteIcon.Caption := GetString(122);
  end;  //of with

  // Update TListView captions
  if Assigned(FStartup) then
    FStartup.DoNotifyOnFinished();

  if Assigned(FContext) then
    FContext.DoNotifyOnFinished();

  if Assigned(FService) then
    FService.DoNotifyOnFinished();

  if Assigned(FTasks) then
    FTasks.DoNotifyOnFinished();
end;

{ private TMain.ShowColumnDate

  Adds or removes the date of deactivation column. }

procedure TMain.ShowColumnDate(AListView: TListView; AShow: Boolean = True);
begin
  // Timestamp already shown?
  if not AShow then
  begin
    if (AListView.Columns.Count = 5) then
      AListView.Columns.Delete(4);
  end  //of begin
  else
    if (AListView.Columns.Count = 4) then
      with AListView.Columns.Add do
      begin
        Caption := FLang.GetString(80);
        Width := 120;
        LoadItems(False);
      end;  //of with
end;

{ private TMain.ShowExportItemDialog

  Shows a file export dialog. }

function TMain.ShowExportItemDialog(): Boolean;
var
  FileName, Filter, DefaultExt: string;
  SelectedList: TRootList<TRootItem>;

begin
  Result := False;

  // Select file filter
  if (PageControl.ActivePageIndex = 3) then
  begin
    Filter := FLang.GetString(118);
    DefaultExt := '.xml';
  end  //of begin
  else
  begin
    Filter := FLang.GetString(36);
    DefaultExt := '.reg';
  end;  //of if

  try
    SelectedList := GetSelectedList();

    // Set a default file name
    if (PageControl.ActivePageIndex = 1) then
    begin
      if (FContext.Selected.LocationRoot = '*') then
        FileName := FContext.Selected.Name
      else
        if (FContext.Selected is TShellNewItem) then
          FileName := FContext.Selected.Name +'_'+ CM_SHELLNEW
        else
          FileName := FContext.Selected.Name +'_'+ FContext.Selected.LocationRoot;
    end  //of begin
    else
      FileName := SelectedList.Selected.Name;

    FileName := FileName + DefaultExt;

    // Show save dialog
    if PromptForFileName(FileName, Filter, DefaultExt, StripHotkey(mmExport.Caption),
      '%HOMEPATH%', True) then
    begin
      SelectedList.ExportItem(FileName);
      Result := True;
    end;  //of begin

  except
    on E: EAccessViolation do
      FLang.ShowMessage(FLang.GetString([95, 18]), FLang.GetString(53), mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([95, 18]), E.Message);
  end;  //of try
end;

{ public TMain.UpdateContextPath

  Updates "Open Clearas" in recycle bin context menu. }

function TMain.UpdateContextPath(): Boolean;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    
    // Only update if context menu entry exists
    if Reg.OpenKey(KEY_RECYCLEBIN +'\Clearas\command', False) then
    begin
      Reg.WriteString('', ParamStr(0));
      Result := True;
    end  //of begin
    else
      Result := False;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ TMain.bDeleteItemClick

  Event method that is called when user wants to delete an item. }

procedure TMain.bDeleteItemClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: DeleteStartupItem(Sender);
    1: DeleteItem(85);
    2: DeleteItem(58);
    3: DeleteItem(120);
  end;  //of case
end;

{ TMain.bDisableItemClick

  Event method that is called when user wants to disable an item. }

procedure TMain.bDisableItemClick(Sender: TObject);
begin
  DisableItem();
end;

{ TMain.bEnableStartupItemClick

  Enables currently selected startup item. }

procedure TMain.bEnableStartupItemClick(Sender: TObject);
begin
  // Successfully enabled item?
  if EnableItem() then
  begin
    // Delete deactivation timestamp if necassary
    if mmDate.Checked then
      lwStartup.ItemFocused.SubItems[3] := '';

    // Warn if file does not exist
    if not FStartup.Selected.FileExists() then
      FLang.ShowMessage(45, 46, mtWarning);
  end;  //of begin
end;

{ TMain.bEnableContextItemClick

  Enables currently selected context menu item. }

procedure TMain.bEnableContextItemClick(Sender: TObject);
begin
  // Successfully enabled item?
  if EnableItem() then
  begin
    // Warn if file does not exist
    if (not (FContext.Selected is TShellNewItem) and not FContext.Selected.FileExists()) then
      FLang.ShowMessage(45, 46, mtWarning);
  end;  //of begin
end;

{ TMain.bEnableServiceItemClick

  Enables currently selected service item. }

procedure TMain.bEnableServiceItemClick(Sender: TObject);
begin
  // Successfully enabled item?
  if EnableItem() then
  begin
    // Delete deactivation timestamp if necassary
    if mmDate.Checked then
      lwService.ItemFocused.SubItems[3] := '';

    // Warn if file does not exist
    if not FService.Selected.FileExists() then
      FLang.ShowMessage(45, 46, mtWarning);
  end;  //of begin
end;

{ TMain.bEnableTaskItemClick

  Enables currently selected task item. }

procedure TMain.bEnableTaskItemClick(Sender: TObject);
begin
  EnableItem();
end;

{ TMain.bExportStartupItemClick

  Calls the export method of current selected deactivated startup item. }

procedure TMain.bExportStartupItemClick(Sender: TObject);
begin
  CreateStartupUserBackup();
end;

{ TMain.bExportItemClick

  Calls the export method of current selected context menu item. }

procedure TMain.bExportItemClick(Sender: TObject);
begin
  try
    // Nothing selected?
    if not Assigned(GetSelectedList().Selected) then
      Abort;

    ShowExportItemDialog();

  except
    on E: Exception do
      FLang.ShowMessage(FLang.GetString([95, 18]), FLang.GetString(53), mtWarning);
  end;  //of try
end;

{ TMain.eSearchChange

  Event method that is called when user changes the search string. }

procedure TMain.eSearchChange(Sender: TObject);
begin
  if ((Sender as TButtonedEdit).Text = '') then
  begin
    (Sender as TButtonedEdit).RightButton.ImageIndex := 0;
    (Sender as TButtonedEdit).RightButton.DisabledImageIndex := 0;
    (Sender as TButtonedEdit).RightButton.HotImageIndex := 0;
    (Sender as TButtonedEdit).RightButton.PressedImageIndex := 0;
  end  //of begin
  else
  begin
    (Sender as TButtonedEdit).RightButton.ImageIndex := 1;
    (Sender as TButtonedEdit).RightButton.DisabledImageIndex := 1;
    (Sender as TButtonedEdit).RightButton.HotImageIndex := 2;
    (Sender as TButtonedEdit).RightButton.PressedImageIndex := 2;
  end;  //of if

  // Refresh TListView
  GetSelectedList().OnSearchFinish(Self);
end;

{ TMain.eSearchRightButtonClick

  Event method that is called when user clicked on clear. }

procedure TMain.eSearchRightButtonClick(Sender: TObject);
begin
  if ((Sender as TButtonedEdit).Text <> '') then
    (Sender as TButtonedEdit).Clear;
end;

{ TMain.lwContextDblClick

  Event method that is called when user double clicks on TListView item. }

procedure TMain.lwContextDblClick(Sender: TObject);
begin
  if bEnableContextItem.Enabled then
    bEnableContextItem.Click
  else
    if bDisableContextItem.Enabled then
      bDisableContextItem.Click
    else
      if bDeleteContextItem.Enabled then
        bDeleteContextItem.Click
      else
        FLang.ShowMessage(FLang.GetString(53), mtWarning);
end;

{ TMain.lwContextSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwContextSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Index: Integer;

begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    // Find index of currently selected item in backend
    Index := FContext.IndexOf(Item.SubItems[0], Item.SubItems[1]);

    // Item not found?
    if (Index = -1) then
    begin
      PopupMenu.AutoPopup := False;
      FLang.ShowMessage(FLang.GetString(53), mtError);
      Exit;
    end;  //of begin

    // Load item into cache
    FContext.Selected := FContext.Items[Index];

    // Change button states
    bEnableContextItem.Enabled := not FContext.Selected.Enabled;
    bDisableContextItem.Enabled := not bEnableContextItem.Enabled;

    // Change text of "change status" button
    if bDisableContextItem.Enabled then
      pmChangeStatus.Caption := bDisableContextItem.Caption
    else
      pmChangeStatus.Caption := bEnableContextItem.Caption;

    pmChangeStatus.Enabled := True;
    bDeleteContextItem.Enabled := True;
    pmDelete.Enabled := True;
    pmOpenRegedit.Enabled := True;
    bExportContextItem.Enabled := True;
    pmExport.Enabled := True;
    pmRename.Enabled := (FContext.Selected is TShellItem);
    pmChangeIcon.Visible := pmRename.Enabled;
    pmDeleteIcon.Visible := (pmRename.Enabled and (FContext.Selected.Icon <> 0));

    // Enable "edit path" only if file path is present
    pmEdit.Enabled := (FContext.Selected.FileName <> '');

    // Show popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
    // Nothing selected: Hide popup menu!
    PopupMenu.AutoPopup := False;
end;

{ TMain.lwServiceDblClick

  Event method that is called when user double clicks on TListView item. }

procedure TMain.lwServiceDblClick(Sender: TObject);
begin
  if bEnableServiceItem.Enabled then
    bEnableServiceItem.Click
  else
    if bDisableServiceItem.Enabled then
      bDisableServiceItem.Click
    else
      if bDeleteServiceItem.Enabled then
        bDeleteServiceItem.Click
      else
        FLang.ShowMessage(FLang.GetString(53), mtWarning);
end;

{ TMain.lwServiceSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwServiceSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Index: Integer;

begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    // Find index of currently selected item in backend
    Index := FService.IndexOf(Item.SubItems[0]);

    // Item not found?
    if (Index = -1) then
    begin
      PopupMenu.AutoPopup := False;
      FLang.ShowMessage(FLang.GetString(53), mtError);
      Exit;
    end;  //of begin

    // Load item into cache
    FService.Selected := FService.Items[Index];

    // Change button states
    bEnableServiceItem.Enabled := not FService.Selected.Enabled;
    bDisableServiceItem.Enabled := not bEnableServiceItem.Enabled;

    // Change text of "change status" button
    if bDisableServiceItem.Enabled then
      pmChangeStatus.Caption := bDisableServiceItem.Caption
    else
      pmChangeStatus.Caption := bEnableServiceItem.Caption;

    pmChangeStatus.Enabled := True;
    bDeleteServiceItem.Enabled := True;
    pmDelete.Enabled := True;
    pmOpenRegedit.Enabled := True;
    bExportServiceItem.Enabled := True;
    pmExport.Enabled := True;
    pmRename.Enabled := True;
    pmDeleteIcon.Visible := False;
    pmChangeIcon.Visible := False;

    // Enable "edit path" only if file path is present
    pmEdit.Enabled := (FService.Selected.FileName <> '');

    // Show popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
    // Nothing selected: Hide popup menu!
    PopupMenu.AutoPopup := False;
end;

{ TMain.lwTasksDblClick

  Event method that is called when user double clicks on TListView item. }

procedure TMain.lwTasksDblClick(Sender: TObject);
begin
  if bEnableTaskItem.Enabled then
    bEnableTaskItem.Click
  else
    if bDisableTaskitem.Enabled then
      bDisableTaskitem.Click
    else
      if bDeleteTaskItem.Enabled then
        bDeleteTaskItem.Click
      else
        FLang.ShowMessage(FLang.GetString(53), mtWarning);
end;

{ TMain.lwTasksSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwTasksSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Index: Integer;

begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    // Find index of currently selected item in backend
    Index := FTasks.IndexOf(Item.SubItems[0]);

    // Item not found?
    if (Index = -1) then
    begin
      PopupMenu.AutoPopup := False;
      FLang.ShowMessage(FLang.GetString(53), mtError);
      Exit;
    end;  //of begin

    // Load item into cache
    FTasks.Selected := FTasks.Items[Index];

    // Change button states
    bEnableTaskItem.Enabled := not FTasks.Selected.Enabled;
    bDisableTaskitem.Enabled := not bEnableTaskItem.Enabled;

    // Change text of "change status" button
    if bDisableTaskitem.Enabled then
      pmChangeStatus.Caption := bDisableTaskitem.Caption
    else
      pmChangeStatus.Caption := bEnableTaskItem.Caption;

    pmChangeStatus.Enabled := True;
    bDeleteTaskItem.Enabled := True;
    pmDelete.Enabled := True;
    pmOpenRegedit.Enabled := False;
    bExportTaskItem.Enabled := True;
    pmExport.Enabled := True;
    pmRename.Enabled := True;
    pmDeleteIcon.Visible := False;
    pmChangeIcon.Visible := False;

    // Enable "edit path" only if file path is present
    pmEdit.Enabled := (FTasks.Selected.FileName <> '');

    // Show popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
    // Nothing selected: Hide popup menu!
    PopupMenu.AutoPopup := False;
end;

{ TMain.ListViewColumnClick

  Event method that is called when user clicks on TListView column. }

procedure TMain.ListViewColumnClick(Sender: TObject; Column: TListColumn);
var
  List: TListView;
  Header: HWND;
  Item: THDItem;
  i: Integer;

begin
  List := (Sender as TListView);
  Header := ListView_GetHeader(List.Handle);
  ZeroMemory(@Item, SizeOf(Item));
  Item.Mask := HDI_FORMAT;

  // Remove the sort ascending flags from columns
  for i := 0 to List.Columns.Count - 1 do
  begin
    // Skip current column!
    if (i = Column.Index) then
      Continue;

    Item.Mask := HDI_FORMAT;
    Header_GetItem(Header, i, Item);
    Item.fmt := Item.fmt and not (HDF_SORTUP or HDF_SORTDOWN);
    Header_SetItem(Header, i, Item);
    List.Columns[i].Tag := 0;
  end;  //of begin

  // Current column
  Header_GetItem(Header, Column.Index, Item);

  // Sorted ascending?
  if (Item.fmt and HDF_SORTUP <> 0) then
  begin
    Item.fmt := Item.fmt and not HDF_SORTUP or HDF_SORTDOWN;
    Column.Tag := 1;
  end  //of begin
  else
  begin
    Item.fmt := Item.fmt and not HDF_SORTDOWN or HDF_SORTUP;
    Column.Tag := 0;
  end;  //of if

  // Include sort icon
  Header_SetItem(Header, Column.Index, Item);

  // Do the alphabetically sort
  List.Tag := Column.Index;
  List.AlphaSort();
end;

{ TMain.ListViewCompare

  Sorts a TListView column alphabetically. }

procedure TMain.ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  List: TListView;
  ColumnToSort: Byte;

begin
  List := (Sender as TListView);
  ColumnToSort := List.Tag;

  // Status column?
  if (ColumnToSort = 0) then
    case List.Columns[ColumnToSort].Tag of
      0: Compare := CompareText(Item1.Caption, Item2.Caption);
      1: Compare := CompareText(Item2.Caption, Item1.Caption);
    end  //of case
  else
  begin
    Data := ColumnToSort - 1;

    if (Data < 3) then
      case List.Columns[ColumnToSort].Tag of
        0: Compare := CompareText(Item1.SubItems[Data], Item2.SubItems[Data]);
        1: Compare := CompareText(Item2.SubItems[Data], Item1.SubItems[Data]);
      end;  //of case
  end;  //of if
end;

{ TMain.lwStartupDblClick

  Event method that is called when user double clicks on TListView item. }

procedure TMain.lwStartupDblClick(Sender: TObject);
begin
  if bEnableStartupItem.Enabled then
    bEnableStartupItem.Click
  else
    if bDisableStartupItem.Enabled then
      bDisableStartupItem.Click
    else
      if bDeleteStartupItem.Enabled then
        bDeleteStartupItem.Click
      else
        FLang.ShowMessage(FLang.GetString(53), mtWarning);
end;

{ TMain.ListViewKeyPress

  Event method that is called when user pushes a button inside TListView. }

procedure TMain.ListViewKeyPress(Sender: TObject; var Key: Char);
var
  i, StartIndex: Integer;
  List: TListView;

begin
  StartIndex := 0;
  List := (Sender as TListView);

  if not Assigned(List.ItemFocused) then
    Exit;

  // Current selected item already starts with key?
  if AnsiStartsText(Key, List.ItemFocused.SubItems[0]) then
    StartIndex := List.ItemFocused.Index + 1;

  // Find next item whose first char starts with key
  for i := StartIndex to List.Items.Count - 1 do
    if AnsiStartsText(Key, List.Items[i].SubItems[0]) then
    begin
      List.ItemIndex := i;
      List.ItemFocused := List.Items[i];
      Break;
    end;  //of begin
end;

{ TMain.lwStartupSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwStartupSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Index: Integer;

  function IsDouble(AName: string; AStartIndex: Integer): Integer;
  var
    i, j, k: Integer;

  begin
    Result := -1;
    j := 0;

    for i := AStartIndex +1 to lwStartup.Items.Count -1 do
      if (AName = lwStartup.Items.Item[i].SubItems[0]) then
      begin
        Inc(j);
        Result := i;
        Break;
      end;  //of begin

    if (j = 0) then
      for k := 0 to Item.Index -1 do
        if (AName = lwStartup.Items.Item[k].SubItems[0]) then
        begin
          Result := k;
          Break;
        end;  //of begin
  end;

begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    // Find index of currently selected item in backend
    Index := FStartup.IndexOf(Item.SubItems[0], (Item.Caption = FLang.GetString(102)));

    // Item not found?
    if (Index = -1) then
    begin
      PopupMenu.AutoPopup := False;
      FLang.ShowMessage(FLang.GetString(53), mtError);
      Exit;
    end;  //of begin

    // Load item into cache
    FStartup.Selected := FStartup.Items[Index];

    // Check for multiple items with the same name
    if (IsDouble(Item.SubItems[0], Item.Index) <> -1) then
    begin
      bEnableStartupItem.Enabled := False;
      bDisableStartupItem.Enabled := False;
      pmChangeStatus.Enabled := False;
      pmEdit.Enabled := False;
    end  //of begin
    else
    begin
      bEnableStartupItem.Enabled := not FStartup.Selected.Enabled;
      bDisableStartupItem.Enabled := not bEnableStartupItem.Enabled;

      // Change text of "change status" button
      if bDisableStartupItem.Enabled then
        pmChangeStatus.Caption := bDisableStartupItem.Caption
      else
        pmChangeStatus.Caption := bEnableStartupItem.Caption;

      pmChangeStatus.Enabled := True;
    end;  //of if

    // Selected item is enabled and startup user type?
    if ((FStartup.Selected is TStartupUserItem) and FStartup.Selected.Enabled) then
    begin
      // Disable "open in RegEdit" because item is on file system!
      pmOpenRegedit.Enabled := False;

      // Disable "export" if backup already exists
      bExportStartupItem.Enabled := not FStartup.BackupExists();
    end  //of begin
    else
    begin
      bExportStartupItem.Enabled := True;
      pmOpenRegedit.Enabled := True;
    end;  //of if

    pmEdit.Enabled := True;
    pmExport.Enabled := bExportStartupItem.Enabled;
    bDeleteStartupItem.Enabled := True;
    pmDelete.Enabled := True;
    pmRename.Enabled := True;
    pmDeleteIcon.Visible := False;
    pmChangeIcon.Visible := False;

    // Show popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
    // Nothing selected: Hide popup menu!
    PopupMenu.AutoPopup := False;
end;

{ TMain.pmChangeStatusClick

  Popup menu entry to change the status of the current selected item. }

procedure TMain.pmChangeStatusClick(Sender: TObject);
begin
  GetSelectedListView().OnDblClick(Sender);
end;

{ TMain.pmChangeIconClick

  Popup menu entry to change the icon of the current selected shell item. }

procedure TMain.pmChangeIconClick(Sender: TObject);
var
  FileName: string;

begin
  try
    if not (FContext.Selected is TShellItem) then
      Exit;

    if PromptForFileName(FileName, 'Application *.exe|*.exe|Icon *.ico|*.ico') then
    begin
      if not (FContext.Selected as TShellItem).ChangeIcon('"'+ FileName +'"') then
        raise Exception.Create('Unknown error!');

      pmDeleteIcon.Visible := True;
    end;  //of begin

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([121, 18]), FLang.GetString(53), mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([121, 18]), E.Message);
  end;  //of try
end;

{ TMain.pmDeleteClick

  Popup menu entry to delete the current selected item. }

procedure TMain.pmDeleteClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: bDeleteStartupItem.OnClick(Sender);
    1: bDeleteContextItem.OnClick(Sender);
    2: bDeleteServiceItem.OnClick(Sender);
    3: bDeleteTaskItem.OnClick(Sender);
  end;  //of case
end;

{ TMain.pmDeleteIconClick

  Popup menu entry to delete the icon of the current selected shell item. }

procedure TMain.pmDeleteIconClick(Sender: TObject);
begin
  try
    if not (FContext.Selected is TShellItem) then
      Exit;

    // Show confimation
    if (FLang.ShowMessage(FLang.GetString(123), mtConfirmation) = ID_YES) then
    begin
      if not (FContext.Selected as TShellItem).DeleteIcon() then
        raise Exception.Create('Unknown error!');

      pmDeleteIcon.Visible := False;
    end;  //of begin

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([122, 18]), FLang.GetString(53), mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([122, 18]), E.Message);
  end;  //of try
end;

{ TMain.pmOpenRegeditClick

  Opens the path of the current selected item in Explorer. }

procedure TMain.pmOpenExplorerClick(Sender: TObject);
begin
  try
    GetSelectedItem().OpenInExplorer();

  except
    on E: EWarning do
      FLang.ShowMessage(45, 46, mtWarning);

    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([51, 18]), FLang.GetString(53), mtWarning);
  end;  //of try
end;

{ TMain.pmOpenRegeditClick

  Opens the current selected item in RegEdit. }

procedure TMain.pmOpenRegeditClick(Sender: TObject);
var
  Item: TRootItem;

begin
  try
    Item := GetSelectedItem();

    if (Item is TRegistryItem) then
      (Item as TRegistryItem).OpenInRegEdit();

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([66, 18]), FLang.GetString(53), mtWarning);
  end;  //of try
end;

{ TMain.pmRenameClick

  Renames the current selected item. }

procedure TMain.pmRenameClick(Sender: TObject);
var
  Name: string;
  Item: TRootItem;

begin
  try
    Item := GetSelectedItem();

    if (Item.Caption <> '') then
      Name := Item.Caption
    else
      Name := Item.Name;

    if InputQuery(StripHotkey(pmRename.Caption), StripHotkey(pmRename.Caption), Name) then
    begin
      if ((Trim(Name) = '') or (Name = Item.Name) or (Name = Item.Caption)) then
        Exit;

      if GetSelectedList().RenameItem(Name) then
        GetSelectedListView().ItemFocused.SubItems[0] := Name;
    end;  //of begin

  except
    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([55, 18]), FLang.GetString(53), mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([55, 18]), E.Message);
  end;  //of try
end;

{ TMain.pmCopyLocationClick

  Popup menu entry to show some properties. }

procedure TMain.pmCopyLocationClick(Sender: TObject);
var
  Item: TRootItem;
  RootKey: string;

begin
  try
    Item := GetSelectedItem();

    if (Item is TStartupItem) then
    begin
      RootKey := HKeyToStr((Item as TStartupItem).RootKey);
      Clipboard.AsText := RootKey +'\'+ (Item as TStartupItem).Wow64Location;
    end  //of begin
    else
      Clipboard.AsText := Item.LocationFull;

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([106, 18]), FLang.GetString(53), mtWarning);
  end;  //of try
end;

{ TMain.pmEditClick

  Popup menu entry to edit the path of a program. }

procedure TMain.pmEditClick(Sender: TObject);
var
  Path, EnteredPath: string;
  Icon: TIcon;

begin
  try
    Path := GetSelectedItem().FileName;

    // Show input box for editing path
    EnteredPath := InputBox(FLang.GetString(104), FLang.GetString(54), Path);

    // Nothing entered or nothing changed
    if ((Trim(EnteredPath) = '') or (EnteredPath = Path)) then
      Exit;

    // Try to change the file path
    if not GetSelectedList().ChangeItemFilePath(EnteredPath) then
      raise Exception.Create('Error while changing path!');

    // Update icon path in TListView
    if (PageControl.ActivePageIndex = 0) then
    begin
      // Update icon
      Icon := TIcon.Create;

      try
        Icon.Handle := FStartup.Selected.Icon;
        lwStartup.ItemFocused.ImageIndex := IconList.AddIcon(Icon);

      finally
        Icon.Free;
      end;  //of try
    end;  //of begin

    // Update file path in TListView
    if (PageControl.ActivePageIndex <> 1) then
      GetSelectedListView().ItemFocused.SubItems[1] := EnteredPath;
    
  except
    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([104, 18]), FLang.GetString(53), mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([104, 18]), E.Message);
  end;  //of try
end;

{ TMain.mmAddClick

  MainMenu entry to add a new item to the current selected list. }

procedure TMain.mmAddClick(Sender: TObject);
var
  FileName, Name, Args, Location: string;
  List: TStringList;
  Extended: Boolean;

begin
  if (PageControl.ActivePageIndex = 3) then
  begin
    ExecuteProgram('control', 'schedtasks');
    Exit;
  end;  //of begin

  // Show open dialog
  if not PromptForFileName(FileName, FLang.GetString(108), '',
    StripHotKey(mmAdd.Caption), '%ProgramFiles%') then
    Exit;

  try
    // Set default name
    Name := ChangeFileExt(ExtractFileName(FileName), '');

    // Setup special default values for new contextmenu items
    if (PageControl.ActivePageIndex = 1) then
    begin
      Name := '&'+ Name;
      Args := '"%1"';
    end;  //of begin

    // User can edit the name
    if not InputQuery(StripHotKey(mmAdd.Caption), FLang.GetString(97), Name) then
      Exit;

    // Name must not be empty!
    if (Name = '') then
      raise EArgumentException.Create('Name must not be empty!');

    // Append optional parameters
    if not InputQuery(StripHotKey(mmAdd.Caption), FLang.GetString(98), Args) then
      Exit;

    // Add startup item?
    case PageControl.ActivePageIndex of
      0: begin
           // Startup item already exists?
           if not FStartup.Add(FileName, Args, Name) then
             raise EWarning.Create(FLang.Format(110, [FileName]));
         end;

      1: begin
           List := TStringList.Create;

           try
             // Init location ComboBox
             List.CommaText := CM_LOCATIONS_DEFAULT +', .txt, .zip';

             // Show dialog for location selection
             if not InputCombo(FLang.GetString(105), FLang.GetString(90) +':',
               List, Location, FLang.GetString(68), Extended, False) then
               Exit;

             // Contextmenu item already exists?
             if not FContext.Add(FileName, Args, Location, Name, Extended) then
               raise EWarning.Create(FLang.Format(41, [FileName]));

             // User choice exists for selected file extension?
             if FContext.Last.UserChoiceExists(Location) then
               // Delete user choice?
               if (FLang.ShowMessage(111, [112, 113], mtConfirmation) = ID_YES) then
                 FContext.Last.DeleteUserChoice(Location);

           finally
             List.Free;
           end;  //of try
         end;  //of if

      2: begin
           // Service item already exists?
           if not FService.Add(FileName, Args, Name) then
             raise EWarning.Create(FLang.Format(110, [FileName]));
         end;
    end;  //of case

  except
    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(StripHotKey(mmAdd.Caption) + FLang.GetString(18),
        E.Message, mtWarning);

    on E: EArgumentException do
      FLang.ShowMessage(StripHotKey(mmAdd.Caption) + FLang.GetString(18),
        E.Message, mtError);

    on E: Exception do
      FLang.ShowException(StripHotKey(mmAdd.Caption) + FLang.GetString(18),
        E.Message);
  end;  //of try
end;

{ TMain.mmDateClick

  MainMenu entry to add or remove the deactivation timestamp column. }

procedure TMain.mmDateClick(Sender: TObject);
var
  ListView: TListView;

begin
  case PageControl.ActivePageIndex of
    0: ListView := lwStartup;
    2: ListView := lwService;
    else
       Exit;
  end;  //of case

  if (WindowState = wsNormal) then
  begin
    if mmDate.Checked then
      Width := Width + 120
    else
      Width := Width - ListView.Columns[4].Width;
  end;  //of begin

  // Add or remove date column
  ShowColumnDate(ListView, mmDate.Checked);
end;

{ TMain.mmExportClick

  MainMenu entry to export a single item as .reg file. }

procedure TMain.mmExportClick(Sender: TObject);
begin
  if (PageControl.ActivePageIndex = 0) then
    bExportStartupItemClick(Sender)
  else
    bExportItemClick(Sender);
end;

{ TMain.mmExportListClick

  MainMenu entry to export the complete list as .reg (backup) file. }

procedure TMain.mmExportListClick(Sender: TObject);
var
  SelectedList: TRootList<TRootItem>;
  FileName, Filter, DefaultExt: string;

begin
  try
    SelectedList := TRootList<TRootItem>(GetSelectedList());

    // Operation pending?
    if SelectedList.IsLocked() then
      raise EListBlocked.Create('Another operation is pending. Please wait!');

    // Select file filter
    if (PageControl.ActivePageIndex = 3) then
    begin
      Filter := FLang.GetString(119);
      DefaultExt := '.zip';
    end  //of begin
    else
    begin
      Filter := FLang.GetString(36);
      DefaultExt := '.reg';
    end;  //of if

    // Sets default file name
    FileName := PageControl.ActivePage.Caption + DefaultExt;

    // Show save dialog
    if PromptForFileName(FileName, Filter, DefaultExt, StripHotkey(mmExportList.Caption),
      '%HOMEPATH%', True) then
      // Export list (threaded!)
      with TExportListThread.Create(SelectedList, FileName, PageControl.ActivePageIndex) do
      begin
        OnStart := OnExportListStart;
        OnFinish := OnExportListEnd;
        OnError := OnExportListError;
        Start;
      end;  //of with

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([72, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);
  end;  //of try
end;

{ TMain.mmImportClick

  MainMenu entry to import a startup backup file. }

procedure TMain.mmImportClick(Sender: TObject);
var
  BackupDir, Filter, FileName: string;
  ImportableList: IImportableList;

begin
  try
    case PageControl.ActivePageIndex of
      0: begin
           BackupDir := TLnkFile.GetBackupDir();

           // Create Backup directory if not exists
           if not DirectoryExists(BackupDir) then
             ForceDirectories(BackupDir);

           Filter := Format(FLang.GetString(109), [EXT_USER, EXT_USER,
             EXT_COMMON, EXT_COMMON]);
         end;

      3: Filter := FLang.GetString(118);
         //Filter := Format('%s|%s', [FLang.GetString(118), FLang.GetString(119)]);

      else
         Exit;
    end;  //of case

    // Show select file dialog
    if PromptForFileName(FileName, Filter, '', StripHotkey(mmImport.Caption), BackupDir) then
      if (Supports(GetSelectedList(), IImportableList, ImportableList) and not
        ImportableList.ImportBackup(FileName)) then
        raise EWarning.Create(FLang.GetString(41));

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([70, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(FLang.GetString([70, 18]), E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([70, 18]), E.Message);
  end;  //of try
end;

{ TMain.mmDelBackupClick

  MainMenu entry to set or resets the flag to delete backups automatically. }

procedure TMain.mmDelBackupClick(Sender: TObject);
begin
  FStartup.DeleteBackup := mmDelBackup.Checked;
end;

{ TMain.mmContextClick

  MainMenu entry to add or removes "Clearas" in the recycle bin context menu. }

procedure TMain.mmContextClick(Sender: TObject);
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Checkbox checked?
    if mmContext.Checked then
    begin
      // Remove recycle bin context menu entry
      Reg.OpenKey(KEY_RECYCLEBIN, False);

      if Reg.DeleteKey('Clearas') then
        mmContext.Checked := False;
    end  //of begin
    else
    try
      // Add recycle bin context menu entry
      Reg.OpenKey(KEY_RECYCLEBIN +'\Clearas', True);
      Reg.WriteString('', FLang.GetString(107));
      Reg.CloseKey();
      Reg.OpenKey(KEY_RECYCLEBIN +'\Clearas\command', True);
      Reg.WriteString('', ParamStr(0));
      mmContext.Checked := True;

    finally
      Reg.CloseKey();
      Reg.Free;
    end;  //of try

  except
    on E: Exception do
      FLang.ShowException('Add Clearas to recycle bin context menu failed!', E.Message);
  end;  //of try
end;

{ TMain.mmRefreshClick

  MainMenu entry to refresh the current shown TListView. }

procedure TMain.mmRefreshClick(Sender: TObject);
begin
  LoadItems();
end;

{ TMain.mmStandardClick

  MainMenu entry to resize all columns to standard size. }

procedure TMain.mmDefaultClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: begin
         lwStartup.Columns[1].Width := 125;
         lwStartup.Columns[2].Width := 122;
         lwStartup.Columns[3].Width := 75;
       end;

    1: begin
         lwContext.Columns[1].Width := 150;
         lwContext.Columns[2].Width := 107;
         lwContext.Columns[3].Width := 65;
       end;

    2: begin
         lwService.Columns[1].Width := 125;
         lwService.Columns[2].Width := 122;
         lwService.Columns[3].Width := 75;
       end;

    3: begin
         lwTasks.Columns[1].Width := 125;
         lwTasks.Columns[2].Width := 122;
         lwTasks.Columns[3].Width := 75;
       end;
  end;  //of case
end;

{ TMain.mmInstallCertificateClick

  MainMenu entry that allows to install the PM Code Works certificate. }

procedure TMain.mmInstallCertificateClick(Sender: TObject);
var
  Updater: TUpdate;

begin
  Updater := TUpdate.Create(Self, FLang);

  try
    // Certificate already installed?
    if not Updater.CertificateExists() then
      Updater.InstallCertificate()
    else
      FLang.ShowMessage(FLang.GetString(27), mtInformation);

  finally
    Updater.Free;
  end;  //of try
end;

{ TMain.mmUpdateClick

  MainMenu entry that allows users to manually search for updates. }

procedure TMain.mmUpdateClick(Sender: TObject);
begin
  FUpdateCheck.CheckForUpdate(True);
end;

{ TMain.mmReportClick

  MainMenu entry that allows users to easily report a bug by opening the web
  browser and using the "report bug" formular. }

procedure TMain.mmReportClick(Sender: TObject);
begin
  OpenUrl(URL_CONTACT);
end;

{ TMain.mmInfoClick

  MainMenu entry that shows a info page with build number and version history. }

procedure TMain.mmInfoClick(Sender: TObject);
var
  Info: TInfo;

begin
  Application.CreateForm(TInfo, Info);
  Info.ShowModal;
  Info.Free;
end;

{ TMain.lCopyClick

  Opens the homepage of PM Code Works in a web browser. }

procedure TMain.lCopy1Click(Sender: TObject);
begin
  OpenUrl(URL_BASE);
end;

{ TMain.lCopyMouseEnter

  Allows a label to have the look like a hyperlink. }

procedure TMain.lCopy1MouseEnter(Sender: TObject);
begin
  with (Sender as TLabel) do
  begin
    Font.Style := Font.Style + [fsUnderline];
    Font.Color := clBlue;
    Cursor := crHandPoint;
  end;  //of with
end;

{ TMain.lCopyMouseLeave

  Allows a label to have the look of a normal label again. }

procedure TMain.lCopy1MouseLeave(Sender: TObject);
begin
  with (Sender as TLabel) do
  begin
    Font.Style := Font.Style - [fsUnderline];
    Font.Color := clBlack;
    Cursor := crDefault;
  end;  //of with
end;

{ TMain.PageControlChange

  Event method that is called when tab is changed. }

procedure TMain.PageControlChange(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: begin
         mmAdd.Caption := FLang.GetString(69);
         mmImport.Visible := True;
         mmDate.Visible := True;
         lwStartupSelectItem(Sender, lwStartup.ItemFocused, True);
         ShowColumnDate(lwStartup, mmDate.Checked);
       end;

    1: begin
         mmAdd.Caption := FLang.GetString(105);
         mmImport.Visible := False;
         mmDate.Visible := False;
         lwContextSelectItem(Sender, lwContext.ItemFocused, True);

         // Load context menu entries dynamically
         if (Assigned(FContext) and (FContext.Count = 0)) then
           LoadItems();
       end;

    2: begin
         mmAdd.Caption := FLang.GetString(57);
         mmImport.Visible := False;
         mmDate.Visible := True;
         lwServiceSelectItem(Sender, lwService.ItemFocused, True);
         ShowColumnDate(lwService, mmDate.Checked);

         // Load context menu entries dynamically
         if (Assigned(FService) and (FService.Count = 0)) then
           LoadItems();
       end;

    3: begin
         mmAdd.Caption := FLang.GetString(117);
         mmImport.Visible := True;
         mmDate.Visible := False;
         lwTasksSelectItem(Sender, lwTasks.ItemFocused, True);

         // Load task items dynamically
         if not Assigned(FTasks) then
         begin
           mmImport.Visible := False;
           FLang.ShowMessage('Scheduled tasks feature requires at least Windows Vista!', mtWarning);
         end  //of begin
         else
           if (FTasks.Count = 0) then
             LoadItems();
       end;
  end;  //of case
end;

{ TMain.bCloseStartupClick

  Closes Clearas. }

procedure TMain.bCloseStartupClick(Sender: TObject);
begin
  Close;
end;

end.
