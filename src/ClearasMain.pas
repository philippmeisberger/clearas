{ *********************************************************************** }
{                                                                         }
{ Clearas Main Unit                                                       }
{                                                                         }
{ Copyright (c) 2011-2019 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Menus, Vcl.Graphics,
  Vcl.ClipBrd, Registry, System.ImageList, Winapi.CommCtrl, System.UITypes,
  Vcl.Forms, System.Generics.Collections, Winapi.ShellAPI, Vcl.ImgList, ClearasAPI,
  Clearas.ListColumns, Winapi.Taskschd, PMCW.Dialogs, PMCW.SysUtils, PMCW.LanguageFile,
  PMCW.Registry, PMCW.Application;

type
  { TMain }
  TMain = class(TMainForm)
    ContextMenu: TPopupMenu;
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
    mmEdit: TMenuItem;
    mmFile: TMenuItem;
    mmExport: TMenuItem;
    N3: TMenuItem;
    mmClose: TMenuItem;
    mmAdd: TMenuItem;
    N4: TMenuItem;
    mmImport: TMenuItem;
    N7: TMenuItem;
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
    bExportContextItem: TButton;
    bDeleteContextItem: TButton;
    bCloseContext: TButton;
    bDisableContextItem: TButton;
    bEnableContextItem: TButton;
    lwContext: TListView;
    lContext: TLabel;
    pbContextProgress: TProgressBar;
    cbContextExpert: TCheckBox;
    pmEditPath: TMenuItem;
    pmOpenRegedit: TMenuItem;
    pmOpenExplorer: TMenuItem;
    tsService: TTabSheet;
    lwService: TListView;
    lService: TLabel;
    bExportServiceItem: TButton;
    bDeleteServiceItem: TButton;
    bCloseService: TButton;
    bDisableServiceItem: TButton;
    bEnableServiceItem: TButton;
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
    lTasks: TLabel;
    lwTasks: TListView;
    pbTaskProgress: TProgressBar;
    pmRename: TMenuItem;
    pmChangeIcon: TMenuItem;
    pmDeleteIcon: TMenuItem;
    N5: TMenuItem;
    mmShowCaptions: TMenuItem;
    lCopy2: TLabel;
    lCopy3: TLabel;
    lCopy4: TLabel;
    mmDeleteErasable: TMenuItem;
    eStartupSearch: TButtonedEdit;
    pmExecute: TMenuItem;
    N8: TMenuItem;
    pmProperties: TMenuItem;
    StartupImages: TImageList;
    pmExtended: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bCloseStartupClick(Sender: TObject);
    procedure bEnableItemClick(Sender: TObject);
    procedure bDeleteItemClick(Sender: TObject);
    procedure bDisableItemClick(Sender: TObject);
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
    procedure ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lwStartupDblClick(Sender: TObject);
    procedure lwStartupSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mmAddClick(Sender: TObject);
    procedure mmExportClick(Sender: TObject);
    procedure mmImportClick(Sender: TObject);
    procedure mmRefreshClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure pmChangeStatusClick(Sender: TObject);
    procedure pmCopyLocationClick(Sender: TObject);
    procedure pmEditPathClick(Sender: TObject);
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
    procedure mmShowCaptionsClick(Sender: TObject);
    procedure mmDeleteErasableClick(Sender: TObject);
    procedure ContextMenuPopup(Sender: TObject);
    procedure pmExecuteClick(Sender: TObject);
    procedure pmPropertiesClick(Sender: TObject);
    procedure pmExtendedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ListViewColumnRightClick(Sender: TObject; Column: TListColumn;
      Point: TPoint);
  private
    FStartup: TStartupList;
    FContext: TContextMenuList;
    FService: TServiceList;
    FTasks: TTaskList;
    FStatusText: array[Boolean] of string;
    FColumnPopupMenu: TListViewColumnSelectionMenu;
    procedure ColumnChanged(Sender: TObject);
    function GetListForIndex(AIndex: Integer): TRootList<TRootItem>;
    function GetListViewForIndex(AIndex: Integer): TListView;
    function GetSelectedItem(): TRootItem;
    function GetSelectedList(): TRootList<TRootItem>;
    function GetSelectedListView(): TListView;
    function GetItemText(AItem: TRootItem): string;
    function GetPublisher(AItem: TRootItem): string;
    procedure Refresh(AIndex: Integer; ATotal: Boolean = True);
    procedure OnContextListNotify(Sender: TObject; const AItem: TContextMenuListItem;
      AAction: TCollectionNotification);
    procedure OnContextCounterUpdate(Sender: TObject);
    procedure OnStartupListNotify(Sender: TObject; const AItem: TStartupListItem;
      AAction: TCollectionNotification);
    procedure OnStartupCounterUpdate(Sender: TObject);
    procedure OnServiceListNotify(Sender: TObject; const AItem: TServiceListItem;
      AAction: TCollectionNotification);
    procedure OnServiceCounterUpdate(Sender: TObject);
    procedure OnTaskListNotify(Sender: TObject; const AItem: TTaskListItem;
      AAction: TCollectionNotification);
    procedure OnTaskCounterUpdate(Sender: TObject);
    procedure SortList(AListView: TListView);
    function ShowExportItemDialog(AItem: TRootItem): Boolean;
    procedure ShowOperationPendingUI(AIndex: Integer; AShow: Boolean);
    procedure OnException(Sender: TObject; E: Exception);
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure LanguageChanged(); override;
  end;

var
  Main: TMain;

implementation

{$I LanguageIDs.inc}
{$R *.dfm}

type
  TDateTimeHelper = record helper for TDateTime
    function ToString(): string; inline;
  end;

{ TDateTimeHelper }

function TDateTimeHelper.ToString(): string;
begin
  if (Self <> 0) then
    Result := DateTimeToStr(Self)
  else
    Result := '';
end;


{ TMain }

{ TMain.FormCreate

  VCL event that is called when form is being created. }

procedure TMain.FormCreate(Sender: TObject);
begin
  // Setup languages
  FLang := TLanguageFile.Create;
  FLang.AddListener(Self);

  // Build menus
  BuildLanguageMenu(mmLang);
  BuildHelpMenu(mmHelp);

  // Use custom error dialog for possible uncatched exceptions
  Application.OnException := OnException;

  // Init update notificator
{$IFDEF PORTABLE}
  CheckForUpdate('Clearas', 'clearas.exe', 'clearas64.exe', 'Clearas.exe');
{$ELSE}
  CheckForUpdate('Clearas', 'clearas_setup.exe', '', 'Clearas Setup.exe');
{$ENDIF}

  // Init lists
  FStartup := TStartupList.Create;

  // Link search events
  with FStartup do
  begin
    OnCounterUpdate := OnStartupCounterUpdate;
    OnNotify := OnStartupListNotify;
    Images := lwStartup.SmallImages;
  end;  //of with

  FContext := TContextMenuList.Create;

  // Link search events
  with FContext do
  begin
    OnCounterUpdate := OnContextCounterUpdate;
    OnNotify := OnContextListNotify;
  end;  //of with

  FService := TServiceList.Create;

  // Link search events
  with FService do
  begin
    OnCounterUpdate := OnServiceCounterUpdate;
    OnNotify := OnServiceListNotify;
  end;  //of with

  FTasks := TTaskList.Create;

  // Link search events
  with FTasks do
  begin
    OnCounterUpdate := OnTaskCounterUpdate;
    OnNotify := OnTaskListNotify;
  end;  //of with
end;

{ TMain.FormDestroy

  VCL event that is called when form is being destroyed. }

procedure TMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTasks);
  FreeAndNil(FService);
  FreeAndNil(FContext);
  FreeAndNil(FStartup);
end;

procedure TMain.OnException(Sender: TObject; E: Exception);
begin
  ExceptionDlg(FLang, 'Uncatched exception', E.Message);
end;

procedure TMain.FormShow(Sender: TObject);
begin
  // Windows Vista is required as scheduled task feature is not compatible with XP
  if not CheckWin32Version(6) then
  begin
    MessageDlg(FLang[LID_INCOMPATIBLE_OS], mtWarning, [mbClose], 0);
    Close;
  end  //of begin
  else
    // Load items
    PageControlChange(Sender);
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Integer;

begin
  CanClose := True;

  try
    for i := 0 to PageControl.PageCount - 1 do
    begin
      // Export pending?
      if GetListForIndex(i).IsExporting then
      begin
        CanClose := (MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
          mtWarning, [mbOK, mbIgnore], 0) = idIgnore);
        Break;
      end;  //of begin
    end;  //of for

  except
    on E: EInvalidItem do
      CanClose := True;
  end;  //of try
end;

function TMain.GetItemText(AItem: TRootItem): string;
begin
  if ((AItem.Caption <> '') and mmShowCaptions.Checked) then
    Result := AItem.Caption
  else
    Result := AItem.Name;
end;

function TMain.GetListForIndex(AIndex: Integer): TRootList<TRootItem>;
begin
  case AIndex of
    0:   Result := TRootList<TRootItem>(FStartup);
    1:   Result := TRootList<TRootItem>(FContext);
    2:   Result := TRootList<TRootItem>(FService);
    3:   Result := TRootList<TRootItem>(FTasks);
    else Result := nil;
  end;  //of case

  if not Assigned(Result) then
    raise EInvalidItem.CreateFmt('No list at index %d!', [AIndex]);
end;

function TMain.GetListViewForIndex(AIndex: Integer): TListView;
begin
  case AIndex of
    0:   Result := lwStartup;
    1:   Result := lwContext;
    2:   Result := lwService;
    3:   Result := lwTasks;
    else Result := nil;
  end;  //of case

  if not Assigned(Result) then
    raise EInvalidItem.CreateFmt('No ListView at index %d!', [AIndex]);
end;

function TMain.GetPublisher(AItem: TRootItem): string;
var
  ExeInformation: TExeFileInformation;

begin
  if ExeInformation.FromFile(AItem.Command.Expand.ExtractFileName()) then
    Result := ExeInformation.CompanyName
  else
    Result := '';
end;

{ private TMain.GetSelectedItem

  Returns the current selected TRootItem. }

function TMain.GetSelectedItem(): TRootItem;
var
  SelectedListView: TListView;

begin
  SelectedListView := GetSelectedListView();

  if not Assigned(SelectedListView.ItemFocused) then
    raise EInvalidItem.Create(SNoItemSelected);

  Result := TRootItem(SelectedListView.ItemFocused.Data);

  if not Assigned(Result) then
    raise EInvalidItem.Create('No object attached!');
end;

{ private TMain.GetSelectedList

  Returns the current selected TRootList. }

function TMain.GetSelectedList(): TRootList<TRootItem>;
begin
  Result := GetListForIndex(PageControl.ActivePageIndex);
end;

{ private TMain.GetSelectedListView

  Returns the current selected TListView. }

function TMain.GetSelectedListView(): TListView;
begin
  Result := GetListViewForIndex(PageControl.ActivePageIndex);
end;

{ private TMain.Refresh

  Loads items and brings them into a TListView. }

procedure TMain.Refresh(AIndex: Integer; ATotal: Boolean = True);
var
  i: Integer;
  RootList: TRootList<TRootItem>;
  ListView: TListView;

begin
  try
    RootList := GetListForIndex(AIndex);
    ListView := GetListViewForIndex(AIndex);

    // Disable buttons
    ListView.OnSelectItem(Self, nil, False);

    // Make a total refresh or just use cached items
    if ATotal then
    begin
      // Export pending?
      if RootList.IsExporting() then
        raise EListBlocked.Create(SOperationPending);

      // Search pending?
      if RootList.IsSearching() then
        raise EListBlocked.Create(SOperationPending);

      TThread.CreateAnonymousThread(
        procedure
        begin
          try
            TThread.Synchronize(nil,
              procedure
              begin
                ListView.Clear();
                ShowOperationPendingUI(AIndex, True);
              end);

            try
              case AIndex of
                0: RootList.Search(cbRunOnce.Checked);
                1: RootList.Search(cbContextExpert.Checked);
                2: RootList.Search(cbServiceExpert.Checked);
                3: RootList.Search(cbTaskExpert.Checked);
                else raise EInvalidItem.Create(SNoItemSelected);
              end;  //of case

            finally
              TThread.Synchronize(nil,
                procedure
                begin
                  SortList(ListView);
                  ShowOperationPendingUI(AIndex, False);
                end);
            end;  //of try

          except
            on E: EListBlocked do
              TThread.Synchronize(nil,
                procedure
                begin
                  MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
                    mtWarning, [mbOK], 0);
                end);

            on E: Exception do
              TThread.Synchronize(nil,
                procedure
                begin
                  ExceptionDlg(FLang, FLang.GetString([LID_REFRESH, LID_IMPOSSIBLE]), Format('%s: %s', [RootList.ClassName, E.Message]));
                end);
          end;  //of try
        end).Start();
    end  //of begin
    else
    begin
      ListView.Clear();

      for i := 0 to RootList.Count - 1 do
        RootList.OnNotify(Self, RootList[i], cnAdded);

      // Refresh counter label
      if Assigned(RootList.OnCounterUpdate) then
        RootList.OnCounterUpdate(Self);

      // Resort items
      SortList(ListView);
    end;  //of if

    // Close popup menu
    ContextMenu.CloseMenu();

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;
  end;  //of try
end;

{ private TMain.OnContextCounterUpdate

  Event method that is called when item status has been changed. }

procedure TMain.OnContextCounterUpdate(Sender: TObject);
begin
  // Refresh counter label
  if (not (csDestroying in ComponentState) and Assigned(FContext)) then
    lContext.Caption := FLang.Format(LID_CONTEXT_MENU_HEADLINE, [FContext.EnabledItemsCount, FContext.Count]);
end;

procedure TMain.OnContextListNotify(Sender: TObject; const AItem: TContextMenuListItem;
  AAction: TCollectionNotification);
var
  Text: string;
  i: Integer;

begin
  if (AAction = cnAdded) then
  begin
    Text := GetItemText(AItem);

    // Filter items
    if ((eContextSearch.Text = '') or
      (Text.ToLower().Contains(LowerCase(eContextSearch.Text)) or
      AItem.LocationRoot.ToLower().Contains(LowerCase(eContextSearch.Text)))) then
    begin
      with lwContext.Items.Add do
      begin
        Data := AItem;

        for i := 0 to lwContext.Columns.Count - 1 do
        begin
          case TClearasListColumn(lwContext.Columns[i].Tag) of
            ItemName:  Caption := Text;
            Status:    SubItems.Append(FStatusText[AItem.Enabled]);
            Command:   SubItems.Append(AItem.Command);
            ItemType:  SubItems.Append(AItem.ToString());
            Location:  SubItems.Append(AItem.LocationRoot);  // TODO: Use LocationFull constantly
            Publisher: SubItems.Append(GetPublisher(AItem));
            else       SubItems.Append('');
          end;  //of case
        end;  //of for
      end; //of with
    end;  //of begin
  end;  //of begin
end;

{ private TMain.OnStartupCounterUpdate

  Event method that is called when item status has been changed. }

procedure TMain.OnStartupCounterUpdate(Sender: TObject);
begin
  // Refresh counter label
  if (not (csDestroying in ComponentState) and Assigned(FStartup)) then
    lStartup.Caption := FLang.Format(LID_STARTUP_HEADLINE, [FStartup.EnabledItemsCount, FStartup.Count]);
end;

procedure TMain.OnStartupListNotify(Sender: TObject; const AItem: TStartupListItem;
  AAction: TCollectionNotification);

  function GetStartupType(): string;
  begin
    if (AItem is TStartupUserItem) then
      Result := FLang[LID_FILESYSTEM]
    else
      Result := FLang[LID_REGISTRY];
  end;

var
  Text: string;
  i: Integer;

begin
  if (AAction = cnAdded) then
  begin
    Text := GetItemText(AItem);

    if ((eStartupSearch.Text = '') or (Text.ToLower().Contains(LowerCase(eStartupSearch.Text)))) then
    begin
      with lwStartup.Items.Add do
      begin
        Data := AItem;
        ImageIndex := AItem.ImageIndex;

        for i := 0 to lwStartup.Columns.Count - 1 do
        begin
          case TClearasListColumn(lwStartup.Columns[i].Tag) of
            ItemName:         Caption := Text;
            Status:           SubItems.Append(FStatusText[AItem.Enabled]);
            Command:          SubItems.Append(AItem.Command);
            ItemType:         SubItems.Append(AItem.ToString());
            Location:         SubItems.Append(AItem.LocationFull);
            DeactivationDate: SubItems.Append(AItem.DeactivationTime.ToString());
            StartupType:      SubItems.Append(GetStartupType());
            Publisher:        SubItems.Append(GetPublisher(AItem));
            else              SubItems.Append('');
          end;  //of case
        end;  //of for
      end;  //of with
    end;  //of begin
  end;  //of begin
end;

{ private TMain.OnServiceCounterUpdate

  Event method that is called when item status has been changed. }

procedure TMain.OnServiceCounterUpdate(Sender: TObject);
begin
  // Refresh counter label
  if (not (csDestroying in ComponentState) and Assigned(FService)) then
    lService.Caption := FLang.Format(LID_STARTUP_HEADLINE, [FService.EnabledItemsCount, FService.Count]);
end;

procedure TMain.OnServiceListNotify(Sender: TObject;
  const AItem: TServiceListItem; AAction: TCollectionNotification);

  function GetServiceStartCaption(AServiceStart: TServiceStart): string;
  begin
    case AServiceStart of
      ssAutomatic: Result := FLang.GetString(LID_AUTOMATICALLY);
      ssManual:    Result := FLang.GetString(LID_MANUALLY);
      else         Result := 'Service';
    end;  //of case
  end;

var
  Text: string;
  i: Integer;

begin
  if (AAction = cnAdded) then
  begin
    Text := GetItemText(AItem);

    // Filter items
    if ((eServiceSearch.Text = '') or (Text.ToLower().Contains(LowerCase(eServiceSearch.Text)))) then
    begin
      with lwService.Items.Add do
      begin
        Data := AItem;

        for i := 0 to lwService.Columns.Count - 1 do
        begin
          case TClearasListColumn(lwService.Columns[i].Tag) of
            ItemName:         Caption := Text;
            Status:           SubItems.Append(FStatusText[AItem.Enabled]);
            Command:          SubItems.Append(AItem.Command);
            ItemType:         SubItems.Append(AItem.ToString());
            Location:         SubItems.Append(AItem.LocationFull);
            StartupType:      SubItems.Append(GetServiceStartCaption(AItem.Start));
            DeactivationDate: SubItems.Append(AItem.DeactivationTime.ToString());
            Publisher:        SubItems.Append(GetPublisher(AItem));
            else              SubItems.Append('');
          end;  //of case
        end;  //of for
      end;  //of with
    end;  //of begin
  end;  //of begin
end;

{ private TMain.OnTaskCounterUpdate

  Event method that is called when item status has been changed. }

procedure TMain.OnTaskCounterUpdate(Sender: TObject);
begin
  // Refresh counter label
  if (not (csDestroying in ComponentState) and Assigned(FTasks)) then
    lTasks.Caption := FLang.Format(LID_TASKS_HEADLINE, [FTasks.EnabledItemsCount, FTasks.Count]);
end;

procedure TMain.OnTaskListNotify(Sender: TObject; const AItem: TTaskListItem;
  AAction: TCollectionNotification);

  function GetStartupType(): string;
  begin
    if (AItem.Definition.Triggers.Count = 0) then
      Exit('');

    if (AItem.Definition.Triggers.Count > 1) then
      Exit('Multiple');

    case AItem.Definition.Triggers[1].TriggerType of
      TASK_TRIGGER_EVENT:                Result := 'Event';
      TASK_TRIGGER_TIME:                 Result := 'Time';
      TASK_TRIGGER_DAILY:                Result := 'Daily';
      TASK_TRIGGER_WEEKLY:               Result := 'Weekly';
      TASK_TRIGGER_MONTHLY:              Result := 'Monthly';
      TASK_TRIGGER_MONTHLYDOW:           Result := 'Monthly DOW';
      TASK_TRIGGER_IDLE:                 Result := 'Idle';
      TASK_TRIGGER_REGISTRATION:         Result := 'Registration';
      TASK_TRIGGER_BOOT:                 Result := 'Boot';
      TASK_TRIGGER_LOGON:                Result := 'Logon';
      TASK_TRIGGER_SESSION_STATE_CHANGE: Result := 'Session state change';
      else                               Result := 'Custom';
    end;  //of case
  end;

var
  Text: string;
  i: Integer;

begin
  if (AAction = cnAdded) then
  begin
    Text := AItem.Name;

    // Filter items
    if ((eTaskSearch.Text = '') or (Text.ToLower().Contains(LowerCase(eTaskSearch.Text)))) then
    begin
      with lwTasks.Items.Add do
      begin
        Data := AItem;

        for i := 0 to lwTasks.Columns.Count - 1 do
        begin
          case TClearasListColumn(lwTasks.Columns[i].Tag) of
            ItemName:    Caption := Text;
            Status:      SubItems.Append(FStatusText[AItem.Enabled]);
            Command:     SubItems.Append(AItem.Command);
            Location:    SubItems.Append(AItem.Location);  // TODO: Use LocationFull constantly
            ItemType:    SubItems.Append(AItem.ToString());
            StartupType: SubItems.Append(GetStartupType());
            Publisher:   SubItems.Append(AItem.Definition.RegistrationInfo.Author);
            else         SubItems.Append('');
          end;  //of case
        end;  //of for
      end; //of with
    end;  //of begin
  end;  //of begin
end;

{ private TMain.LanguageChanged

  Updates all component captions with new language text. }

procedure TMain.LanguageChanged();
var
  i: Integer;

begin
  inherited LanguageChanged();

  with FLang do
  begin
    // Cache status text
    FStatusText[False] := FLang.GetString(LID_NO);
    FStatusText[True] := FLang.GetString(LID_YES);

    // File menu labels
    mmFile.Caption := GetString(LID_FILE);

    case PageControl.ActivePageIndex of
      0,2: mmAdd.Caption := GetString(LID_STARTUP_ADD);
      1:   mmAdd.Caption := GetString(LID_CONTEXT_MENU_ADD);
      3:   mmAdd.Caption := GetString(LID_TASKS_ADD);
    end;  //of case

    mmImport.Caption := GetString(LID_IMPORT);
    mmExport.Caption := GetString(LID_EXPORT);
    mmClose.Caption := GetString(LID_QUIT);

    // Edit menu labels
    mmEdit.Caption := GetString(LID_EDIT);
    mmDeleteErasable.Caption := GetString(LID_DELETE_ERASABLE);

    // View menu labels
    mmView.Caption := GetString(LID_VIEW);
    mmRefresh.Caption := GetString(LID_REFRESH);
    mmShowCaptions.Caption := GetString(LID_DESCRIPTION_SHOW);
    cbRunOnce.Caption := GetString(LID_STARTUP_RUNONCE);

    // "Startup" tab TButton labels
    tsStartup.Caption := GetString(LID_STARTUP);
    bEnableStartupItem.Caption := GetString(LID_ENABLE);
    bDisableStartupItem.Caption := GetString(LID_DISABLE);
    bExportStartupItem.Caption := GetString(LID_EXPORT);
    bDeleteStartupItem.Caption := GetString(LID_DELETE);
    bCloseStartup.Caption := mmClose.Caption;

    // "Startup" tab TListView labels
    for i := 0 to lwStartup.Columns.Count - 1 do
      lwStartup.Columns[i].Caption := TClearasListColumn(lwStartup.Columns[i].Tag).ToString(FLang);

    lCopy1.Hint := GetString(LID_TO_WEBSITE);
    eStartupSearch.TextHint := GetString(LID_SEARCH);

    // "Context menu" tab TButton labels
    tsContext.Caption := GetString(LID_CONTEXT_MENU);
    bEnableContextItem.Caption := bEnableStartupItem.Caption;
    bDisableContextItem.Caption := bDisableStartupItem.Caption;
    bExportContextItem.Caption := bExportStartupItem.Caption;
    bDeleteContextItem.Caption := bDeleteStartupItem.Caption;
    bCloseContext.Caption := bCloseStartup.Caption;
    cbContextExpert.Caption := GetString(LID_EXPERT_MODE);
    eContextSearch.TextHint := GetString(LID_SEARCH);

    // "Context menu" tab TListView labels
    for i := 0 to lwContext.Columns.Count - 1 do
      lwContext.Columns[i].Caption := TClearasListColumn(lwContext.Columns[i].Tag).ToString(FLang);

    lCopy2.Hint := lCopy1.Hint;

    // "Service" tab TButton labels
    tsService.Caption := GetString(LID_SERVICES);
    bEnableServiceItem.Caption := bEnableStartupItem.Caption;
    bDisableServiceItem.Caption := bDisableStartupItem.Caption;
    bExportServiceItem.Caption := bExportStartupItem.Caption;
    bDeleteServiceItem.Caption := bDeleteStartupItem.Caption;
    bCloseService.Caption := bCloseStartup.Caption;
    cbServiceExpert.Caption := cbContextExpert.Caption;

    // "Service" tab TListView labels
    for i := 0 to lwService.Columns.Count - 1 do
      lwService.Columns[i].Caption := TClearasListColumn(lwService.Columns[i].Tag).ToString(FLang);

    lCopy3.Hint := lCopy1.Hint;
    eServiceSearch.TextHint := eContextSearch.TextHint;

    // "Tasks" tab TButton labels
    tsTasks.Caption := GetString(LID_TASKS);
    bEnableTaskItem.Caption := bEnableStartupItem.Caption;
    bDisableTaskitem.Caption := bDisableStartupItem.Caption;
    bExportTaskItem.Caption := bExportStartupItem.Caption;
    bDeleteTaskItem.Caption := bDeleteStartupItem.Caption;
    bCloseTasks.Caption := bCloseStartup.Caption;
    cbTaskExpert.Caption := cbContextExpert.Caption;

    // "Tasks" tab TListView labels
    for i := 0 to lwTasks.Columns.Count - 1 do
      lwTasks.Columns[i].Caption := TClearasListColumn(lwTasks.Columns[i].Tag).ToString(FLang);

    lCopy4.Hint := lCopy1.Hint;
    eTaskSearch.TextHint := eContextSearch.TextHint;

    // Popup menu labels
    pmChangeStatus.Caption := bDisableStartupItem.Caption;
    pmExecute.Caption := GetString(LID_EXECUTE);
    pmOpenRegedit.Caption := GetString(LID_OPEN_IN_REGEDIT);
    pmOpenExplorer.Caption := GetString(LID_OPEN_IN_EXPLORER);
    pmEditPath.Caption := GetString(LID_PATH_EDIT);
    pmExport.Caption := bExportStartupItem.Caption;
    pmDelete.Caption := bDeleteStartupItem.Caption;
    pmRename.Caption := GetString(LID_RENAME);
    pmCopyLocation.Caption := GetString(LID_LOCATION_COPY);
    pmChangeIcon.Caption := GetString(LID_CONTEXT_MENU_ICON_CHANGE);
    pmDeleteIcon.Caption := GetString(LID_CONTEXT_MENU_ICON_DELETE);
    pmExtended.Caption := GetString(LID_HIDE);
    pmProperties.Caption := GetString(LID_PROPERTIES);
  end;  //of with

  // Refresh list captions
  if Visible then
  begin
    for i := 0 to PageControl.PageCount - 1 do
      Refresh(i, False);
  end;  //of begin
end;

{ private TMain.ShowExportItemDialog

  Shows a file export dialog. }

function TMain.ShowExportItemDialog(AItem: TRootItem): Boolean;
var
  FileName, Filter, DefaultExt: string;
  SelectedList: TRootList<TRootItem>;
  ContextMenuItem: TContextMenuListItem;

begin
  Result := False;

  try
    if not Assigned(AItem) then
      raise EInvalidItem.Create(SNoItemSelected);

    SelectedList := GetSelectedList();

    // Set a default file name
    if (PageControl.ActivePageIndex = 1) then
    begin
      ContextMenuItem := (AItem as TContextMenuListItem);

      if (ContextMenuItem.LocationRoot = '*') then
        FileName := ContextMenuItem.Name
      else
        if (AItem is TContextMenuShellNewItem) then
          FileName := ContextMenuItem.Name +'_'+ TContextMenuShellNewItem.CanonicalName
        else
          FileName := ContextMenuItem.Name +'_'+ ContextMenuItem.LocationRoot;
    end  //of begin
    else
      FileName := AItem.Name;

    Filter := AItem.GetExportFilter(FLang);
    DefaultExt := AItem.GetBackupExtension();
    FileName := FileName + DefaultExt;

    // Show save dialog
    if PromptForFileName(FileName, Filter, DefaultExt, StripHotkey(pmExport.Caption),
      '', True) then
    begin
      SelectedList.ExportItem(AItem, FileName);
      Result := True;
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_EXPORT, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

procedure TMain.ShowOperationPendingUI(AIndex: Integer; AShow: Boolean);
begin
  case AIndex of
    0: cbRunOnce.Enabled := not AShow;

    1:
      begin
        pbContextProgress.Visible := AShow;
        eContextSearch.Visible := not AShow;
        cbContextExpert.Enabled := not AShow;
      end;

    2:
      begin
        pbServiceProgress.Visible := AShow;
        eServiceSearch.Visible := not AShow;
        cbServiceExpert.Enabled := not AShow;
      end;

    3:
      begin
        pbTaskProgress.Visible := AShow;
        eTaskSearch.Visible := not AShow;
        cbTaskExpert.Enabled := not AShow;
      end;
  end;  //of case
end;

procedure TMain.SortList(AListView: TListView);
begin
  if not Assigned(AListView) then
    Exit;

  // TODO: Sort icon is lost
  if (AListView.Tag <> 0) then
    AListView.AlphaSort();

  if Assigned(AListView.ItemFocused) then
    AListView.ItemFocused.MakeVisible(False);
end;

{ public TMain.mmDeleteErasableClick

  Deletes erasable marked items. }

procedure TMain.mmDeleteErasableClick(Sender: TObject);
var
  i, Answer, ItemsDeleted: Integer;
  SelectedList: TRootList<TRootItem>;
  SelectedItem: TRootItem;
  SelectedListView: TListView;

begin
  try
  {$IFDEF WIN32}
    if (TOSVersion.Architecture = arIntelX64) then
    begin
      // Bitness of Clearas should match OS bitness as environment variables are expanded differently:
      // Expanding %PROGRAMFILES% from 64-bit application on 64-bit OS results in e.g. C:\Program Files
      // Expanding %PROGRAMFILES% from 32-bit application on 64-bit OS results in e.g. C:\Program Files (x86)
      if (MessageDlg(FLang.GetString(LID_BITNESS_WARNING), mtWarning, mbAbortIgnore, 0) = idAbort) then
        Exit;
    end;  //of begin
  {$ENDIF}

    SelectedList := GetSelectedList();
    SelectedListView := GetSelectedListView();

    // No erasable items?
    if (SelectedList.ErasableItemsCount = 0) then
    begin
      MessageDlg(FLang[LID_DELETE_ERASABLE_NO_ITEMS], mtInformation, [mbOK], 0);
      Exit;
    end;  //of begin

    // Export pending?
    if SelectedList.IsExporting() then
      raise EListBlocked.Create(SOperationPending);

    ItemsDeleted := 0;

    // TListView.Items.Count is decreased when item is deleted which leads
    // to an AV if erasable items are not consecutive: Start at the end to avoid this
    for i := SelectedListView.Items.Count - 1 downto 0 do
    begin
      SelectedItem := TRootItem(SelectedListView.Items[i].Data);

      if not SelectedItem.Erasable then
        Continue;

      // Select and show item in TListView
      SelectedListView.ItemFocused := SelectedListView.Items[i];
      SelectedListView.Selected := SelectedListView.Items[i];
      SelectedListView.Items[i].MakeVisible(False);

      // Confirm deletion of every erasable item
      Answer := TaskMessageDlg(FLang.Format([LID_ITEM_DELETE],
        [SelectedListView.Items[i].Caption]), FLang.GetString([LID_FILE_DOES_NOT_EXIST,
        LID_ITEM_ERASABLE]), mtWarning, mbYesNoCancel, 0);

      case Answer of
        mrCancel:
          Break;

        mrNo:
          Continue;

        mrYes:
          begin
            // Ask user to export item
            if (MessageDlg(FLang.GetString(LID_ITEM_DELETE_STORE), mtConfirmation,
              mbYesNo, 0, mbYes) = idYes) then
            begin
              // User clicked cancel?
              if not ShowExportItemDialog(SelectedItem) then
                Continue;
            end;  //of begin

            SelectedList.DeleteItem(SelectedItem);
            SelectedListView.Items[i].Delete();
            Inc(ItemsDeleted);

            // All eraseble items deleted?
            if (SelectedList.ErasableItemsCount = 0) then
              Break;
          end;
      end;  //of case
    end;  //of for

    MessageDlg(FLang.Format(LID_DELETE_ERASABLE_SUCCESS, [ItemsDeleted]),
      mtInformation, [mbOK], 0);

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_DELETE_ERASABLE, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

procedure TMain.WMTimer(var Message: TWMTimer);
begin
  KillTimer(Handle, Message.TimerID);
  Refresh(Message.TimerID, False);
end;

{ TMain.bDeleteItemClick

  Event method that is called when user wants to delete an item. }

procedure TMain.bDeleteItemClick(Sender: TObject);
var
  SelectedListView: TListView;
  SelectedList: TRootList<TRootItem>;
  SelectedItem: TRootItem;

begin
  try
    SelectedListView := GetSelectedListView();
    SelectedList := GetSelectedList();
    SelectedItem := GetSelectedItem();

    // Confirm deletion of item
    if (TaskMessageDlg(FLang.Format([LID_ITEM_DELETE], [SelectedListView.ItemFocused.Caption]),
      FLang.GetString([LID_ITEM_DELETE_CONFIRM1, LID_ITEM_DELETE_CONFIRM2]),
      mtWarning, mbYesNo, 0, mbNo) = idYes) then
    begin
      // Ask user to export item
      if (MessageDlg(FLang.GetString(LID_ITEM_DELETE_STORE), mtConfirmation,
        mbYesNo, 0, mbYes) = idYes) then
      begin
        // User clicked cancel?
        if not ShowExportItemDialog(SelectedItem) then
          Exit;
      end;  //of begin

      SelectedList.DeleteItem(SelectedItem);
      SelectedListView.DeleteSelected();
      SelectedListView.ItemFocused := nil;

      // Disable buttons
      SelectedListView.OnSelectItem(Self, nil, False);
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EWarning do
      MessageDlg(E.Message, mtWarning, [mbOK], 0);

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_DELETE, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

{ TMain.bDisableItemClick

  Event method that is called when user wants to disable an item. }

procedure TMain.bDisableItemClick(Sender: TObject);
var
  SelectedListView: TListView;
  SelectedList: TRootList<TRootItem>;
  SelectedItem: TRootItem;

begin
  try
    SelectedListView := GetSelectedListView();
    SelectedList := GetSelectedList();
    SelectedItem := GetSelectedItem();
    SelectedList.DisableItem(SelectedItem);

    case PageControl.ActivePageIndex of
      0:
        begin
          bEnableStartupItem.Enabled := True;
          bDisableStartupItem.Enabled := False;
          pmChangeStatus.Caption := bEnableStartupItem.Caption;

          // Append deactivation timestamp if necessary
          TClearasListColumn.DeactivationDate.UpdateContent(SelectedListView,
            SelectedListView.ItemFocused, (SelectedItem as TStartupListItem).DeactivationTime.ToString());
        end;

      1:
        begin
          bEnableContextItem.Enabled := True;
          bDisableContextItem.Enabled := False;
          pmChangeStatus.Caption := bEnableContextItem.Caption;
        end;

      2:
        begin
          bEnableServiceItem.Enabled := True;
          bDisableServiceItem.Enabled := False;
          pmChangeStatus.Caption := bEnableServiceItem.Caption;

          // Append deactivation timestamp if necessary
          TClearasListColumn.DeactivationDate.UpdateContent(SelectedListView,
            SelectedListView.ItemFocused, (SelectedItem as TServiceListItem).DeactivationTime.ToString());
        end;

      3:
        begin
          bEnableTaskItem.Enabled := True;
          bDisableTaskitem.Enabled := False;
          pmChangeStatus.Caption := bEnableTaskItem.Caption;
        end;
    end;  //of case

    // Change item visual status
    TClearasListColumn.Status.UpdateContent(SelectedListView, SelectedListView.ItemFocused, FStatusText[SelectedItem.Enabled]);

    // Item is erasable?
    if SelectedItem.Erasable then
    begin
      TaskMessageDlg(FLang.GetString(LID_FILE_DOES_NOT_EXIST),
        FLang.GetString(LID_ITEM_ERASABLE), mtWarning, [mbOK], 0);
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EWarning do
      MessageDlg(E.Message, mtWarning, [mbOK], 0);

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_DISABLE, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

{ TMain.bEnableItemClick

  Enables currently selected item. }

procedure TMain.bEnableItemClick(Sender: TObject);
var
  SelectedListView: TListView;
  SelectedList: TRootList<TRootItem>;
  SelectedItem: TRootItem;

begin
  try
    SelectedListView := GetSelectedListView();
    SelectedList := GetSelectedList();
    SelectedItem := GetSelectedItem();
    SelectedList.EnableItem(SelectedItem);

    case PageControl.ActivePageIndex of
      0:
        begin
          bEnableStartupItem.Enabled := False;
          bDisableStartupItem.Enabled := True;
          pmChangeStatus.Caption := bDisableStartupItem.Caption;

          // Delete deactivation timestamp if necessary
          TClearasListColumn.DeactivationDate.UpdateContent(SelectedListView,
            SelectedListView.ItemFocused, '');
        end;

      1:
        begin
          bEnableContextItem.Enabled := False;
          bDisableContextItem.Enabled := True;
          pmChangeStatus.Caption := bDisableContextItem.Caption;
        end;

      2:
        begin
          bEnableServiceItem.Enabled := False;
          bDisableServiceItem.Enabled := True;
          pmChangeStatus.Caption := bDisableServiceItem.Caption;

          // Delete deactivation timestamp if necessary
          TClearasListColumn.DeactivationDate.UpdateContent(SelectedListView,
            SelectedListView.ItemFocused, '');
        end;

      3:
        begin
          bEnableTaskItem.Enabled := False;
          bDisableTaskitem.Enabled := True;
          pmChangeStatus.Caption := bDisableTaskitem.Caption;
        end;
    end;  //of case

    // Change item visual status
    TClearasListColumn.Status.UpdateContent(SelectedListView, SelectedListView.ItemFocused, FStatusText[SelectedItem.Enabled]);

    // Item is erasable?
    if SelectedItem.Erasable then
    begin
      TaskMessageDlg(FLang.GetString(LID_FILE_DOES_NOT_EXIST),
        FLang.GetString(LID_ITEM_ERASABLE), mtWarning, [mbOK], 0);
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EWarning do
      MessageDlg(E.Message, mtWarning, [mbOK], 0);

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_ENABLE, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

{ TMain.bExportItemClick

  Event method that is called when user wants to export an item. }

procedure TMain.bExportItemClick(Sender: TObject);
begin
  try
    ShowExportItemDialog(GetSelectedItem());

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
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

  // Refresh TListView delayed
  SetTimer(Handle, PageControl.ActivePageIndex, 250, nil);
end;

{ TMain.eSearchRightButtonClick

  Event method that is called when user clicked on clear. }

procedure TMain.eSearchRightButtonClick(Sender: TObject);
begin
  if ((Sender as TButtonedEdit).Text <> '') then
    (Sender as TButtonedEdit).Clear();
end;

{ TMain.CustomDrawItem

  Custom drawing of erasable items. }

procedure TMain.ListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  // Mark erasable items
  if TRootItem(Item.Data).Erasable then
    Sender.Canvas.Font.Color := clGray
  else
    Sender.Canvas.Font.Color := clBlack;
end;

{ TMain.lwContextDblClick

  Event method that is called when user double clicks on TListView item. }

procedure TMain.lwContextDblClick(Sender: TObject);
begin
  if bEnableContextItem.Enabled then
    bEnableContextItem.Click
  else
    if bDisableContextItem.Enabled then
      bDisableContextItem.Click;
end;

{ TMain.lwContextSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwContextSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    // Change button states
    bEnableContextItem.Enabled := not TRootItem(Item.Data).Enabled;
    bDisableContextItem.Enabled := not bEnableContextItem.Enabled;
    bDeleteContextItem.Enabled := True;
    bExportContextItem.Enabled := True;

    // Allow popup menu
    ContextMenu.AutoPopup := True;
  end  //of begin
  else
  begin
    // Nothing selected
    lwContext.ItemFocused := nil;
    bEnableContextItem.Enabled := False;
    bDisableContextItem.Enabled := False;
    bDeleteContextItem.Enabled := False;
    bExportContextItem.Enabled := False;

    // Disallow popup menu
    ContextMenu.AutoPopup := False;
  end;
end;

{ TMain.lwServiceDblClick

  Event method that is called when user double clicks on TListView item. }

procedure TMain.lwServiceDblClick(Sender: TObject);
begin
  if bEnableServiceItem.Enabled then
    bEnableServiceItem.Click
  else
    if bDisableServiceItem.Enabled then
      bDisableServiceItem.Click;
end;

{ TMain.lwServiceSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwServiceSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    // Change button states
    bEnableServiceItem.Enabled := not TRootItem(Item.Data).Enabled;
    bDisableServiceItem.Enabled := not bEnableServiceItem.Enabled;
    bDeleteServiceItem.Enabled := True;
    bExportServiceItem.Enabled := True;

    // Allow popup menu
    ContextMenu.AutoPopup := True;
  end  //of begin
  else
  begin
    // Nothing selected
    lwService.ItemFocused := nil;
    bEnableServiceItem.Enabled := False;
    bDisableServiceItem.Enabled := False;
    bDeleteServiceItem.Enabled := False;
    bExportServiceItem.Enabled := False;

    // Disallow popup menu
    ContextMenu.AutoPopup := False;
  end;  //of if
end;

{ TMain.lwTasksDblClick

  Event method that is called when user double clicks on TListView item. }

procedure TMain.lwTasksDblClick(Sender: TObject);
begin
  if bEnableTaskItem.Enabled then
    bEnableTaskItem.Click
  else
    if bDisableTaskitem.Enabled then
      bDisableTaskitem.Click;
end;

{ TMain.lwTasksSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwTasksSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    // Change button states
    bEnableTaskItem.Enabled := not TRootItem(Item.Data).Enabled;
    bDisableTaskitem.Enabled := not bEnableTaskItem.Enabled;
    bDeleteTaskItem.Enabled := True;
    bExportTaskItem.Enabled := True;

    // Allow popup menu
    ContextMenu.AutoPopup := True;
  end  //of begin
  else
  begin
    // Nothing selected
    lwTasks.ItemFocused := nil;
    bEnableTaskItem.Enabled := False;
    bDisableTaskitem.Enabled := False;
    bDeleteTaskItem.Enabled := False;
    bExportTaskItem.Enabled := False;

    // Disallow popup menu
    ContextMenu.AutoPopup := False;
  end;  //of if
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

  for i := 0 to List.Columns.Count - 1 do
  begin
    Header_GetItem(Header, i, Item);

    // Sort current column
    if (i = Column.Index) then
    begin
      // Descending
      if (Item.fmt and HDF_SORTUP <> 0) then
      begin
        Item.fmt := Item.fmt and not HDF_SORTUP or HDF_SORTDOWN;
        List.Tag := -Column.Tag;
      end  //of begin
      else
      begin
        Item.fmt := Item.fmt and not HDF_SORTDOWN or HDF_SORTUP;
        List.Tag := Column.Tag;
      end;  //of if
    end  //of begin
    else
      // Remove the sort icon from other column
      Item.fmt := Item.fmt and not (HDF_SORTUP or HDF_SORTDOWN);

    Header_SetItem(Header, i, Item);
  end;  //of begin

  // Do the alphabetically sort
  List.AlphaSort();
end;

{ TMain.ListViewCompare

  Sorts a TListView column alphabetically. }

procedure TMain.ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  List: TListView;
  ColumnIndex: Integer;

begin
  List := (Sender as TListView);

  // Nothing to sort
  if (List.Tag = 0) then
    Exit;

  // First column?
  if (Abs(List.Tag) = 1) then
  begin
    // Ascending
    if (List.Tag > 0) then
      Compare := CompareText(Item1.Caption, Item2.Caption)
    else
      Compare := CompareText(Item2.Caption, Item1.Caption);
  end  //of begin
  else
  begin
    ColumnIndex := TClearasListColumn(Abs(List.Tag)).GetColumnIndex(List);

    if (ColumnIndex = -1) then
      Exit;

    if (ColumnIndex > 0) then
      Dec(ColumnIndex);

    // Ascending
    if (List.Tag > 0) then
      Compare := CompareText(Item1.SubItems[ColumnIndex], Item2.SubItems[ColumnIndex])
    else
      Compare := CompareText(Item2.SubItems[ColumnIndex], Item1.SubItems[ColumnIndex]);
  end;  //of begin
end;

procedure TMain.ColumnChanged(Sender: TObject);
begin
  Refresh(PageControl.ActivePageIndex, False);
end;

procedure TMain.ListViewColumnRightClick(Sender: TObject; Column: TListColumn;
  Point: TPoint);
begin
  if not Assigned(FColumnPopupMenu) then
  begin
    FColumnPopupMenu := TListViewColumnSelectionMenu.Create(Sender as TListView, FLang);
    FColumnPopupMenu.OnColumnChanged := ColumnChanged;
  end  //of begin
  else
    FColumnPopupMenu.ListView := (Sender as TListView);

  // Show column selection popup menu
  FColumnPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y, Column);
end;

procedure TMain.lwStartupDblClick(Sender: TObject);
begin
  if bEnableStartupItem.Enabled then
    bEnableStartupItem.Click
  else
    if bDisableStartupItem.Enabled then
      bDisableStartupItem.Click;
end;

{ TMain.lwStartupSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwStartupSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    // Change button states
    bEnableStartupItem.Enabled := not TRootItem(Item.Data).Enabled;
    bDisableStartupItem.Enabled := not bEnableStartupItem.Enabled;
    bDeleteStartupItem.Enabled := True;
    bExportStartupItem.Enabled := True;

    // Allow popup menu
    ContextMenu.AutoPopup := True;
  end  //of begin
  else
  begin
    // Nothing selected
    lwStartup.ItemFocused := nil;
    bEnableStartupItem.Enabled := False;
    bDisableStartupItem.Enabled := False;
    bDeleteStartupItem.Enabled := False;
    bExportStartupItem.Enabled := False;

    // Disallow popup menu
    ContextMenu.AutoPopup := False;
  end;  //of if
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
  SelectedItem: TRootItem;

begin
  try
    // Export pending?
    if FContext.IsExporting() then
      raise EListBlocked.Create(SOperationPending);

    SelectedItem := GetSelectedItem();

    // Only icon of shell item can be changed
    if not (SelectedItem is TContextMenuShellItem) then
      Exit;

    if PromptForFileName(FileName, 'Application *.exe|*.exe|Icon *.ico|*.ico',
      '', StripHotkey(pmChangeIcon.Caption)) then
    begin
      FContext.ChangeIcon(SelectedItem as TContextMenuShellItem, '"'+ FileName +'"');
      pmDeleteIcon.Visible := True;
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
    begin
      ExceptionDlg(FLang, FLang.GetString([LID_CONTEXT_MENU_ICON_CHANGE,
        LID_IMPOSSIBLE]), E.Message);
    end;
  end;  //of try
end;

{ TMain.pmDeleteIconClick

  Popup menu entry to delete the icon of the current selected shell item. }

procedure TMain.pmDeleteIconClick(Sender: TObject);
var
  SelectedItem: TRootItem;

begin
  try
    // Export pending?
    if FContext.IsExporting() then
      raise EListBlocked.Create(SOperationPending);

    SelectedItem := GetSelectedItem();

    // Only icon of shell item can be deleted
    if not (SelectedItem is TContextMenuShellItem) then
      Exit;

    // Show confimation
    if (MessageDlg(FLang.GetString(LID_CONTEXT_MENU_ICON_DELETE_CONFIRM),
      mtConfirmation, mbYesNo, 0) = idYes) then
    begin
      FContext.DeleteIcon(SelectedItem as TContextMenuShellItem);
      pmDeleteIcon.Visible := False;
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
    begin
      ExceptionDlg(FLang, FLang.GetString([LID_CONTEXT_MENU_ICON_DELETE,
        LID_IMPOSSIBLE]), E.Message);
    end;
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
    begin
      TaskMessageDlg(FLang.GetString(LID_FILE_DOES_NOT_EXIST),
        FLang.GetString(LID_ITEM_ERASABLE), mtWarning, [mbOK], 0);
    end;

    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
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
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
  end;  //of try
end;

procedure TMain.pmPropertiesClick(Sender: TObject);
var
  ShellExecuteInfo: TShellExecuteInfo;
  SelectedItem: TRootItem;

begin
  try
    SelectedItem := GetSelectedItem();

    if (SelectedItem.Command <> '') then
    begin
      ZeroMemory(@ShellExecuteInfo, SizeOf(TShellExecuteInfo));

      with ShellExecuteInfo do
      begin
        cbSize := SizeOf(TShellExecuteInfo);
        lpVerb := 'properties';
        lpFile := PChar(SelectedItem.Command.Expand());
        nShow := SW_SHOWNORMAL;
        fMask := SEE_MASK_INVOKEIDLIST;
      end;  //of with

      if not ShellExecuteEx(@ShellExecuteInfo) then
        MessageDlg(SysErrorMessage(GetLastError()), mtError, [mbOK], 0);
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
  end;  //of try
end;

{ TMain.pmRenameClick

  Renames the current selected item. }

procedure TMain.pmRenameClick(Sender: TObject);
var
  Name, OriginalName: string;
  Item: TRootItem;

begin
  try
    Item := GetSelectedItem();

    // The caption of startup and task items can not be renamed
    if (PageControl.ActivePageIndex in [0, 3]) then
      Name := Item.Name
    else
      Name := Item.Caption;

    OriginalName := Name;

    if InputQuery(StripHotkey(pmRename.Caption), StripHotkey(pmRename.Caption), Name) then
    begin
      // Nothing entered or nothing changed
      if ((Trim(Name) = '') or AnsiSameText(Name, OriginalName)) then
        Exit;

      GetSelectedList().RenameItem(Item, Name);

      // Names are visible instead of captions?
      if (not mmShowCaptions.Checked or (PageControl.ActivePageIndex in [1, 3])) then
        GetSelectedListView().ItemFocused.Caption := Name;
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EAlreadyExists do
      MessageDlg(FLang.GetString(LID_ITEM_ALREADY_EXISTS), mtError, [mbOK], 0);

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_RENAME, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

procedure TMain.ContextMenuPopup(Sender: TObject);
var
  SelectedItem: TRootItem;

begin
  // OnPopup also occurs when hotkeys are pressed
  if not ContextMenu.AutoPopup then
    Exit;

  // Since this point cannot be reached when no item is selected EInvalidItem is
  // assumed not to be raised
  SelectedItem := GetSelectedItem();

  // Change text
  if SelectedItem.Enabled then
    pmChangeStatus.Caption := bDisableStartupItem.Caption
  else
    pmChangeStatus.Caption := bEnableStartupItem.Caption;

  // Disable properties if item is erasable or command is empty
  pmProperties.Enabled := (not SelectedItem.Erasable and (SelectedItem.Command <> ''));

  // Update popup menu items
  case PageControl.ActivePageIndex of
    0:
      begin
        // Startup user items are located in filesystem not in registry!
        pmOpenRegedit.Enabled := not ((SelectedItem is TStartupUserItem) and
          SelectedItem.Enabled);
        pmOpenExplorer.Enabled := True;
        pmRename.Enabled := True;
        pmDeleteIcon.Visible := False;
        pmChangeIcon.Visible := False;
        pmExtended.Visible := False;
        pmEditPath.Enabled := True;
        pmExecute.Enabled := not SelectedItem.Erasable;
      end;

    1:
      begin
        pmOpenRegedit.Enabled := True;

        // ShellNew and cascading Shell items cannot be opened in Explorer
        pmOpenExplorer.Enabled := not (SelectedItem is TContextMenuShellNewItem) and not
          (SelectedItem is TContextMenuShellCascadingItem);

        // Only Shell contextmenu items can be renamed
        pmRename.Enabled := (SelectedItem is TContextMenuShellItem);

        // Currently only icon of Shell contextmenu items can be changed
        pmChangeIcon.Visible := pmRename.Enabled;
        pmDeleteIcon.Visible := (pmRename.Enabled and (SelectedItem.IconFileName <> ''));
        pmExtended.Visible := pmRename.Enabled;
        pmExtended.Checked := (pmRename.Enabled and (SelectedItem as TContextMenuShellItem).Extended);
        pmEditPath.Enabled := (SelectedItem.Command <> '');

        // Context menu items cannot be executed
        pmExecute.Enabled := False;
      end;

    2:
      begin
        pmOpenRegedit.Enabled := True;
        pmOpenExplorer.Enabled := True;
        pmRename.Enabled := True;
        pmDeleteIcon.Visible := False;
        pmChangeIcon.Visible := False;
        pmExtended.Visible := False;
        pmEditPath.Enabled := (SelectedItem.Command <> '');
        pmExecute.Enabled := not SelectedItem.Erasable;
      end;

    3:
      begin
        pmOpenRegedit.Enabled := False;
        pmOpenExplorer.Enabled := True;
        pmRename.Enabled := True;
        pmDeleteIcon.Visible := False;
        pmChangeIcon.Visible := False;
        pmExtended.Visible := False;
        pmEditPath.Enabled := (SelectedItem.Command <> '');

        // Tasks cannot be executed when they are disabled
        pmExecute.Enabled := SelectedItem.Enabled;
      end;
  end;  //of case
end;

{ TMain.pmCopyLocationClick

  Popup menu entry to show some properties. }

procedure TMain.pmCopyLocationClick(Sender: TObject);
var
  SelectedItem: TRootItem;

begin
  try
    SelectedItem := GetSelectedItem();

    if (SelectedItem is TStartupItem) then
    begin
      with SelectedItem as TStartupItem do
        Clipboard.AsText := RootKey.ToString() +'\'+ Wow64Location
    end  //of begin
    else
      Clipboard.AsText := SelectedItem.LocationFull;

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
  end;  //of try
end;

{ TMain.pmEditClick

  Popup menu entry to edit the path of a program. }

procedure TMain.pmEditPathClick(Sender: TObject);
var
  Path, EnteredPath: string;
  SelectedListView: TListView;
  SelectedItem: TRootItem;

begin
  try
    SelectedListView := GetSelectedListView();
    SelectedItem := GetSelectedItem();
    Path := SelectedItem.Command;

    // Show input box for editing path
    EnteredPath := InputBox(FLang.GetString(LID_PATH_EDIT),
      FLang.GetString(LID_ITEM_CHANGE_PATH), Path);

    // Nothing entered or nothing changed
    if ((Trim(EnteredPath) = '') or AnsiSameText(EnteredPath, Path)) then
      Exit;

    // Try to change the file path
    GetSelectedList().ChangeCommand(SelectedItem, EnteredPath);

    // Update icon
    if Assigned(SelectedListView.SmallImages) then
      SelectedListView.ItemFocused.ImageIndex := SelectedItem.ImageIndex;

    // Update command in TListView
    TClearasListColumn.Command.UpdateContent(SelectedListView, SelectedListView.ItemFocused, EnteredPath);

    // Update caption
    if ((SelectedItem.Caption <> '') and mmShowCaptions.Checked) then
      SelectedListView.ItemFocused.Caption := SelectedItem.Caption;

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_PATH_EDIT, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

procedure TMain.pmExecuteClick(Sender: TObject);
begin
  try
    GetSelectedItem().Execute();

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_EXECUTE, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

procedure TMain.pmExtendedClick(Sender: TObject);
var
  SelectedItem: TRootItem;

begin
  try
    // Export pending?
    if FContext.IsExporting() then
      raise EListBlocked.Create(SOperationPending);

    SelectedItem := GetSelectedItem();

    if (SelectedItem is TContextMenuShellItem) then
      FContext.ChangeExtended(SelectedItem as TContextMenuShellItem, pmExtended.Checked);

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString([LID_HIDE, LID_IMPOSSIBLE]), E.Message);
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
    ShellExecute(0, 'open', 'control', 'schedtasks', nil, SW_SHOWNORMAL);
    Exit;
  end;  //of begin

  // Show open dialog
  if not PromptForFileName(FileName, FLang.GetString(LID_FILTER_EXE_BAT_FILES),
    '', StripHotKey(mmAdd.Caption)) then
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
    if not InputQuery(StripHotKey(mmAdd.Caption), FLang.GetString(LID_RENAME_PROMPT), Name) then
      Exit;

    if (Name = '') then
      raise EArgumentException.Create('Name must not be empty!');

    // Append optional parameters
    if not InputQuery(StripHotKey(mmAdd.Caption), FLang.GetString(LID_PARAMETERS_PROMPT), Args) then
      Exit;

    // Add startup item?
    case PageControl.ActivePageIndex of
      0: begin
           if not FStartup.Add(FileName, Args, Name) then
             raise Exception.Create('Item was not added!');
         end;

      1: begin
           List := TStringList.Create;

           try
             // Init location ComboBox
             List.CommaText := TContextMenuList.DefaultLocations +', .txt, .zip';

             // Show dialog for location selection
             if not InputCombo(FLang.GetString(LID_CONTEXT_MENU_ADD),
               FLang.GetString(LID_LOCATION) +':', List, Location,
               FLang.GetString(LID_HIDE), Extended, False) then
               Exit;

             if not FContext.Add(FileName, Args, Location, Name) then
               raise Exception.Create('Item was not added!');

             (FContext.Last as TContextMenuShellItem).Extended := Extended;

             // User choice exists for selected file extension?
             if FContext.Last.UserChoiceExists(Location) then
             begin
               // Delete user choice?
               if (TaskMessageDlg(FLang[LID_CONTEXT_MENU_USER_CHOICE_WARNING1],
                 FLang.GetString([LID_CONTEXT_MENU_USER_CHOICE_WARNING2, LID_CONTEXT_MENU_USER_CHOICE_RESET]),
                 mtConfirmation, mbYesNo, 0) = idYes) then
                 FContext.Last.DeleteUserChoice(Location);
             end;  //of begin

           finally
             List.Free;
           end;  //of try
         end;  //of if

      2: begin
           if not FService.Add(FileName, Args, Name) then
             raise Exception.Create('Item was not added!');
         end;
    end;  //of case

  except
    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EAlreadyExists do
      MessageDlg(FLang.GetString(LID_ITEM_ALREADY_EXISTS), mtError, [mbOK], 0);

    on E: EArgumentException do
      MessageDlg(E.Message, mtError, [mbOK], 0);

    on E: Exception do
    begin
      ExceptionDlg(FLang, StripHotKey(mmAdd.Caption) + FLang.GetString(LID_IMPOSSIBLE),
        E.Message);
    end;
  end;  //of try
end;

{ TMain.mmExportListClick

  MainMenu entry to export the complete list as .reg (backup) file. }

procedure TMain.mmExportClick(Sender: TObject);
var
  SelectedList: TRootList<TRootItem>;
  FileName, Filter, DefaultExt: string;
  PageIndex: Integer;

begin
  try
    SelectedList := GetSelectedList();

    // Export already pending?
    if SelectedList.IsExporting() then
      raise EListBlocked.Create(SOperationPending);

    Filter := SelectedList.GetExportFilter(FLang);
    DefaultExt := SelectedList.GetBackupExtension();
    FileName := PageControl.ActivePage.Caption + DefaultExt;

    // Show save dialog
    if PromptForFileName(FileName, Filter, DefaultExt, StripHotkey(mmExport.Caption),
      '', True) then
    begin
      PageIndex := PageControl.ActivePageIndex;

      TThread.CreateAnonymousThread(
        procedure
        begin
          try
            TThread.Synchronize(nil,
              procedure
              begin
                PageControl.Pages[PageIndex].Cursor := crHourGlass;
                GetListViewForIndex(PageIndex).Cursor := crHourGlass;
                ShowOperationPendingUI(PageIndex, True);
              end);

            try
              SelectedList.ExportList(FileName);

            finally
              TThread.Synchronize(nil,
                procedure
                begin
                  PageControl.Pages[PageIndex].Cursor := crDefault;
                  GetListViewForIndex(PageIndex).Cursor := crDefault;
                  ShowOperationPendingUI(PageIndex, False);
                end);
            end;  //of try

          except
            on E: EListBlocked do
              TThread.Synchronize(nil,
                procedure
                begin
                  MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
                    mtWarning, [mbOK], 0);
                end);

            on E: Exception do
              TThread.Synchronize(nil,
                procedure
                begin
                   ExceptionDlg(FLang, FLang.GetString([LID_EXPORT, LID_IMPOSSIBLE]), E.Message);
                end);
          end;
        end).Start();
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;
  end;  //of try
end;

{ TMain.mmImportClick

  MainMenu entry to import a startup backup file. }

procedure TMain.mmImportClick(Sender: TObject);
var
  Filter, FileName: string;
  ImportableList: IImportableList;

begin
  try
    // Selected list does not support importing backups
    if not Supports(GetSelectedList(), IImportableList, ImportableList) then
      Exit;

    Filter := ImportableList.GetImportFilter(FLang);

    // Show select file dialog
    if PromptForFileName(FileName, Filter, '', StripHotkey(mmImport.Caption)) then
    begin
      if not ImportableList.ImportBackup(FileName) then
        raise EWarning.Create(FLang.GetString(LID_ITEM_ALREADY_EXISTS));
    end;  //of begin

  except
    on E: EInvalidItem do
      MessageDlg(FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EArgumentException do
      MessageDlg(E.Message, mtError, [mbOK], 0);

    on E: EAlreadyExists do
      MessageDlg(FLang.GetString(LID_ITEM_ALREADY_EXISTS), mtError, [mbOK], 0);

    on E: Exception do
    begin
      ExceptionDlg(FLang, FLang.GetString([LID_IMPORT, LID_IMPOSSIBLE]),
        E.Message);
    end;
  end;  //of try
end;

{ TMain.mmRefreshClick

  MainMenu entry to refresh the current shown TListView. }

procedure TMain.mmRefreshClick(Sender: TObject);
begin
  Refresh(PageControl.ActivePageIndex);
end;

{ TMain.mmShowCaptionsClick

  MainMenu entry to show captions instead of names. }

procedure TMain.mmShowCaptionsClick(Sender: TObject);
var
  i: Integer;

begin
  for i := 0 to PageControl.PageCount - 1 do
    Refresh(i, False);
end;

// TODO: Move hyperlink code to TMainForm
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
var
  SelectedList: TRootList<TRootItem>;

begin
  case PageControl.ActivePageIndex of
    0: begin
         mmAdd.Caption := FLang.GetString(LID_STARTUP_ADD);
         mmShowCaptions.Enabled := True;
       end;

    1: begin
         mmAdd.Caption := FLang.GetString(LID_CONTEXT_MENU_ADD);
         mmImport.Enabled := False;
         mmShowCaptions.Enabled := True;
       end;

    2: begin
         mmAdd.Caption := FLang.GetString(LID_SERVICE_ADD);
         mmShowCaptions.Enabled := True;
       end;

    3: begin
         mmAdd.Caption := FLang.GetString(LID_TASKS_ADD);
         mmShowCaptions.Enabled := False;
       end;
  end;  //of case

  SelectedList := GetSelectedList();

  // Only enable "Import" if list supports it
  mmImport.Enabled := Supports(SelectedList, IImportableList);

  // Load items dynamically
  if (SelectedList.Count = 0) then
    Refresh(PageControl.ActivePageIndex);

  // Only allow popup menu if item is focused
  ContextMenu.AutoPopup := Assigned(GetSelectedListView().ItemFocused);
end;

{ TMain.bCloseStartupClick

  Closes Clearas. }

procedure TMain.bCloseStartupClick(Sender: TObject);
begin
  Close;
end;

end.
