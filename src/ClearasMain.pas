{ *********************************************************************** }
{                                                                         }
{ Clearas Main Unit                                                       }
{                                                                         }
{ Copyright (c) 2011-2017 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Menus, Vcl.Graphics,
  Vcl.ClipBrd, Registry, System.ImageList, Winapi.CommCtrl, System.UITypes,
  ClearasAPI, PMCW.Dialogs.About, PMCW.LanguageFile, PMCW.FileSystem, PMCW.CA,
  PMCW.Dialogs.Updater, ClearasDialogs, Vcl.ImgList, Winapi.Messages, PMCW.Registry;

type
  { TMain }
  TMain = class(TForm, IChangeLanguageListener)
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
    mmAbout: TMenuItem;
    mmEdit: TMenuItem;
    mmContext: TMenuItem;
    mmFile: TMenuItem;
    mmExport: TMenuItem;
    N3: TMenuItem;
    mmClose: TMenuItem;
    mmAdd: TMenuItem;
    N4: TMenuItem;
    mmDate: TMenuItem;
    mmImport: TMenuItem;
    mmDefault: TMenuItem;
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
    lVersion2: TLabel;
    lVersion1: TLabel;
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
    lTasks: TLabel;
    lVersion4: TLabel;
    lWindows4: TLabel;
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
    N11: TMenuItem;
    mmDeleteErasable: TMenuItem;
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
    procedure ListViewKeyPress(Sender: TObject; var Key: Char);
    procedure lwStartupSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mmAddClick(Sender: TObject);
    procedure mmContextClick(Sender: TObject);
    procedure mmDateClick(Sender: TObject);
    procedure mmExportClick(Sender: TObject);
    procedure mmImportClick(Sender: TObject);
    procedure mmRefreshClick(Sender: TObject);
    procedure mmDefaultClick(Sender: TObject);
    procedure mmAboutClick(Sender: TObject);
    procedure mmUpdateClick(Sender: TObject);
    procedure mmInstallCertificateClick(Sender: TObject);
    procedure mmReportClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure pmChangeStatusClick(Sender: TObject);
    procedure pmCopyLocationClick(Sender: TObject);
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
    procedure mmShowCaptionsClick(Sender: TObject);
    procedure mmDeleteErasableClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    FStartup: TStartupList;
    FContext: TContextMenuList;
    FService: TServiceList;
    FTasks: TTaskList;
    FLang: TLanguageFile;
    FUpdateCheck: TUpdateCheck;
    function GetSelectedItem(): TRootItem;
    function GetSelectedList(): TRootList<TRootItem>;
    function GetSelectedListView(): TListView;
    procedure LoadItems(ATotalRefresh: Boolean = True);
    procedure OnContextSearchStart(Sender: TObject);
    procedure OnContextSearchEnd(Sender: TObject);
    procedure OnContextItemChanged(Sender: TObject; AItemChange: TItemChange);
    procedure OnExportListStart(Sender: TObject);
    procedure OnExportListEnd(Sender: TObject);
    procedure OnExportListError(Sender: TObject; const AErrorMessage: string);
    procedure OnSearchError(Sender: TObject; const AErrorMessage: string);
    procedure OnStartupSearchStart(Sender: TObject);
    procedure OnStartupSearchEnd(Sender: TObject);
    procedure OnStartupItemChanged(Sender: TObject; AItemChange: TItemChange);
    procedure OnServiceSearchStart(Sender: TObject);
    procedure OnServiceSearchEnd(Sender: TObject);
    procedure OnServiceItemChanged(Sender: TObject; AItemChange: TItemChange);
    procedure OnTaskSearchStart(Sender: TObject);
    procedure OnTaskSearchEnd(Sender: TObject);
    procedure OnTaskItemChanged(Sender: TObject; AItemChange: TItemChange);
    function ShowExportItemDialog(): Boolean;
    procedure ShowColumnDate(AListView: TListView; AShow: Boolean = True);
    function UpdateContextPath(): Boolean;
    procedure OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
    const
      DELAY_TIMER = 1;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    { IChangeLanguageListener }
    procedure LanguageChanged();
  end;

var
  Main: TMain;

implementation

{$I LanguageIDs.inc}
{$R *.dfm}

const
  KEY_RECYCLEBIN  = 'CLSID\{645FF040-5081-101B-9F08-00AA002F954E}\shell';
  SORT_ASCENDING  = 0;
  SORT_DESCENDING = 1;

{ TMain }

{ TMain.FormCreate

  VCL event that is called when form is being created. }

procedure TMain.FormCreate(Sender: TObject);
var
  FileVersion: TFileVersion;

begin
  // Setup languages
  FLang := TLanguageFile.Create;

  with FLang do
  begin
    AddListener(Self);
    BuildLanguageMenu(mmLang);
  end;  //of with

  // Init update notificator
  FUpdateCheck := TUpdateCheck.Create('Clearas', FLang);

  with FUpdateCheck do
  begin
    OnUpdate := Self.OnUpdate;
  {$IFNDEF DEBUG}
    FUpdateCheck.CheckForUpdate();
  {$ENDIF}
  end;  //of with

  // Init lists
  FStartup := TStartupList.Create;

  // Link search events
  with FStartup do
  begin
    OnChanged := OnStartupItemChanged;
    OnRefresh := OnStartupSearchEnd;
  end;  //of with

  FContext := TContextMenuList.Create;

  // Link search events
  with FContext do
  begin
    Duplicates := True;
    OnChanged := OnContextItemChanged;
    OnRefresh := OnContextSearchEnd;
  end;  //of with

  FService := TServiceList.Create;

  // Link search events
  with FService do
  begin
    OnChanged := OnServiceItemChanged;
    OnRefresh := OnServiceSearchEnd;
  end;  //of with

  FTasks := TTaskList.Create;

  // Link search events
  with FTasks do
  begin
    OnChanged := OnTaskItemChanged;
    OnRefresh := OnTaskSearchEnd;
  end;  //of with

  // Get version information
  if FileVersion.FromFile(Application.ExeName) then
  begin
    lVersion1.Caption := FileVersion.ToString('v%d.%d');
    lVersion2.Caption := lVersion1.Caption;
    lVersion3.Caption := lVersion1.Caption;
    lVersion4.Caption := lVersion1.Caption;
  end;  //of begin
end;

{ TMain.FormDestroy

  VCL event that is called when form is being destroyed. }

procedure TMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTasks);
  FreeAndNil(FService);
  FreeAndNil(FContext);
  FreeAndNil(FStartup);
  FreeAndNil(FUpdateCheck);
  FreeAndNil(FLang);
end;

{ TMain.FormShow

  VCL event that is called when form is shown. }

procedure TMain.FormShow(Sender: TObject);
begin
  // Get version of Windows including service pack
  if CheckWin32Version(10) then
    lWindows.Caption := Format('Windows %d %s', [Win32MajorVersion, Win32CSDVersion])
  else
    lWindows.Caption := TOSVersion.Name +' '+ Win32CSDVersion;

  lWindows2.Caption := lWindows.Caption;
  lWindows3.Caption := lWindows.Caption;
  lWindows4.Caption := lWindows.Caption;

  // Update Clearas recycle bin context menu entry
  mmContext.Checked := UpdateContextPath();

  // Load items
  PageControlChange(Sender);
end;

{ private TMain.OnUpdate

  Event that is called by TUpdateCheck when an update is available. }

procedure TMain.OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
var
  Updater: TUpdateDialog;

begin
  mmUpdate.Caption := FLang.GetString(LID_UPDATE_DOWNLOAD);

  // Ask user to permit download
  if (TaskMessageDlg(FLang.Format(LID_UPDATE_AVAILABLE, [ANewBuild]),
    FLang[LID_UPDATE_CONFIRM_DOWNLOAD], mtConfirmation, mbYesNo, 0, mbYes) = idYes) then
  begin
    Updater := TUpdateDialog.Create(Self, FLang);

    try
      with Updater do
      begin
        FileNameLocal := 'Clearas.exe';

        // Download 64-Bit version?
        if (TOSVersion.Architecture = arIntelX64) then
          FileNameRemote := 'clearas64.exe'
        else
          FileNameRemote := 'clearas.exe';
      end;  //of begin

      // Successfully downloaded update?
      if Updater.Execute() then
      begin
        mmUpdate.Caption := FLang.GetString(LID_UPDATE_SEARCH);
        mmUpdate.Enabled := False;
      end;  //of begin

    finally
      Updater.Free;
    end;  //of try
  end;  //of begin
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
var
  SearchThread: TSearchThread;
  SelectedList: TRootList<TRootItem>;

begin
  try
    SelectedList := GetSelectedList();

    // Make a total refresh or just use cached items
    if ATotalRefresh then
    begin
      SearchThread := TSearchThread.Create(SelectedList);

      with SearchThread do
      begin
        OnError := OnSearchError;

        case PageControl.ActivePageIndex of
          0:
            begin
              OnStart := OnStartupSearchStart;
              OnTerminate := OnStartupSearchEnd;
              ExpertMode := cbRunOnce.Checked;
              Start();
            end;

          1:
            begin
              OnStart := OnContextSearchStart;
              OnTerminate := OnContextSearchEnd;
              ExpertMode := cbContextExpert.Checked;
              Start();
            end;

          2:
            begin
              OnStart := OnServiceSearchStart;
              OnTerminate := OnServiceSearchEnd;
              ExpertMode := cbServiceExpert.Checked;
              Start();
            end;

          3:
            begin
              OnStart := OnTaskSearchStart;
              OnTerminate := OnTaskSearchEnd;
              ExpertMode := cbTaskExpert.Checked;
              Start();
            end;
        end;
      end;  //of with
    end  //of begin
    else
      SelectedList.Refresh();

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_REFRESH, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;
  end;  //of try
end;

{ private TMain.OnContextSearchStart

  Event that is called when search starts. }

procedure TMain.OnContextSearchStart(Sender: TObject);
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
    if ((FContext.Items[i].Caption <> '') and mmShowCaptions.Checked) then
      Text := FContext[i].Caption
    else
      Text := FContext[i].Name;

    // Filter items
    if ((eContextSearch.Text = '') or
      (Text.ToLower().Contains(LowerCase(eContextSearch.Text)) or
      FContext[i].LocationRoot.ToLower().Contains(LowerCase(eContextSearch.Text)))) then
      with lwContext.Items.Add do
      begin
        Caption := FContext[i].GetStatusText(FLang);
        SubItems.AddObject(Text, FContext[i]);
        SubItems.Append(FContext[i].LocationRoot);
        SubItems.Append(FContext[i].ToString());
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

  // Refresh counter
  OnContextItemChanged(Sender, icDeleted);
end;

{ private TMain.OnContextItemChanged

  Event method that is called when item status has been changed. }

procedure TMain.OnContextItemChanged(Sender: TObject; AItemChange: TItemChange);
begin
  // Refresh counter label
  lwContext.Columns[1].Caption := FLang.Format(LID_CONTEXT_MENU_COUNTER,
    [FContext.EnabledItemsCount, FContext.Count]);

  // Change button states
  case AItemChange of
    icEnabled:
      begin
        bEnableContextItem.Enabled := False;
        bDisableContextItem.Enabled := True;
        pmChangeStatus.Caption := bDisableContextItem.Caption;
      end;

    icDisabled:
      begin
        bEnableContextItem.Enabled := True;
        bDisableContextItem.Enabled := False;
        pmChangeStatus.Caption := bEnableContextItem.Caption;
      end;

    icDeleted:
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

procedure TMain.OnExportListStart(Sender: TObject);
var
  Index: Integer;

begin
  Index := (Sender as TExportListThread).PageControlIndex;
  PageControl.Pages[Index].Cursor := crHourGlass;

  case Index of
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

procedure TMain.OnExportListEnd(Sender: TObject);
var
  Index: Integer;

begin
  Index := (Sender as TExportListThread).PageControlIndex;
  PageControl.Pages[Index].Cursor := crDefault;

  case Index of
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

procedure TMain.OnExportListError(Sender: TObject; const AErrorMessage: string);
begin
  FLang.ShowException(FLang.GetString([LID_EXPORT, LID_IMPOSSIBLE]), AErrorMessage);
end;

{ private TMain.OnStartupItemChanged

  Event method that is called when item status has been changed. }

procedure TMain.OnStartupItemChanged(Sender: TObject; AItemChange: TItemChange);
var
  Icon: TIcon;

begin
  // Refresh counter label
  if Assigned(FStartup) then
    lwStartup.Columns[1].Caption := FLang.Format(LID_PROGRAM_COUNTER,
      [FStartup.EnabledItemsCount, FStartup.Count]);

  // Change button states
  case AItemChange of
    icEnabled:
      begin
        bEnableStartupItem.Enabled := False;
        bDisableStartupItem.Enabled := True;
        pmExport.Enabled := True;
        pmChangeStatus.Caption := bDisableStartupItem.Caption;

        // Delete deactivation timestamp if necassary
        if (mmDate.Enabled and mmDate.Checked) then
          lwStartup.ItemFocused.SubItems[3] := '';
      end;

    icDisabled:
      begin
        bEnableStartupItem.Enabled := True;
        bDisableStartupItem.Enabled := False;
        pmChangeStatus.Caption := bEnableStartupItem.Caption;

        // Append deactivation timestamp if necassary
        if (mmDate.Enabled and mmDate.Checked and (FStartup.Selected.Time <> 0)) then
          lwStartup.ItemFocused.SubItems[3] := DateTimeToStr(FStartup.Selected.Time);
      end;

    icDeleted:
      begin
        bEnableStartupItem.Enabled := False;
        bDisableStartupItem.Enabled := False;
        bDeleteStartupItem.Enabled := False;
        bExportStartupItem.Enabled := False;
      end;

    icPathChanged:
      begin
        Icon := TIcon.Create;

        try
          // Update icon
          Icon.Handle := FStartup.Selected.Icon;
          lwStartup.ItemFocused.ImageIndex := IconList.AddIcon(Icon);

        finally
          FreeAndNil(Icon);
        end;  //of try
      end;
  end;  //of case
end;

{ private TMain.OnStartupSearchStart

  Event that is called when search starts. }

procedure TMain.OnStartupSearchStart(Sender: TObject);
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
  Text: string;

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
        Caption := FStartup[i].GetStatusText(FLang);

        if ((FStartup[i].Caption <> '') and mmShowCaptions.Checked) then
          Text := FStartup[i].Caption
        else
          Text := FStartup[i].Name;

        SubItems.AddObject(Text, FStartup[i]);
        SubItems.Append(FStartup[i].FileName);
        SubItems.Append(FStartup[i].ToString());

        // Show deactivation timestamp?
        if mmDate.Checked then
        begin
          if (FStartup[i].Time <> 0) then
            SubItems.Append(DateTimeToStr(FStartup[i].Time))
          else
            SubItems.Append('');
        end;  //of begin

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

  // Refresh counter
  OnStartupItemChanged(Sender, icDeleted);
end;

{ private TMain.OnServiceItemChanged

  Event method that is called when item status has been changed. }

procedure TMain.OnServiceItemChanged(Sender: TObject; AItemChange: TItemChange);
begin
  // Refresh counter label
  lwService.Columns[1].Caption := FLang.Format(LID_SERVICE_COUNTER,
    [FService.EnabledItemsCount, FService.Count]);

  // Change button states
  case AItemChange of
    icEnabled:
      begin
        bEnableServiceItem.Enabled := False;
        bDisableServiceItem.Enabled := True;
        pmChangeStatus.Caption := bDisableServiceItem.Caption;

        // Delete deactivation timestamp if necassary
        if (mmDate.Enabled and mmDate.Checked) then
          lwService.ItemFocused.SubItems[3] := '';
      end;

    icDisabled:
      begin
        bEnableServiceItem.Enabled := True;
        bDisableServiceItem.Enabled := False;
        pmChangeStatus.Caption := bEnableServiceItem.Caption;

        // Append deactivation timestamp if necassary
        if (mmDate.Enabled and mmDate.Checked and (FService.Selected.Time <> 0)) then
          lwService.ItemFocused.SubItems[3] := DateTimeToStr(FService.Selected.Time);
      end;

    icDeleted:
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

procedure TMain.OnServiceSearchStart(Sender: TObject);
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
    if ((FService[i].Caption <> '') and mmShowCaptions.Checked) then
      Text := FService[i].Caption
    else
      Text := FService[i].Name;

    // Filter items
    if ((eServiceSearch.Text = '') or (Text.ToLower().Contains(LowerCase(eServiceSearch.Text)))) then
      with lwService.Items.Add do
      begin
        Caption := FService[i].GetStatusText(FLang);
        SubItems.AddObject(Text, FService[i]);
        SubItems.Append(FService[i].FileName);
        SubItems.Append(FService[i].Start.ToString(FLang));

        // Show deactivation timestamp?
        if (mmDate.Checked and (FService[i].Time <> 0)) then
          SubItems.Append(DateTimeToStr(FService[i].Time));
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

  // Refresh counter
  OnServiceItemChanged(Sender, icDeleted);
end;

{ private TMain.OnSearchError

  Event method that is called when search thread has failed. }

procedure TMain.OnSearchError(Sender: TObject; const AErrorMessage: string);
begin
  FLang.ShowException(FLang.GetString([LID_REFRESH, LID_IMPOSSIBLE]), AErrorMessage);
end;

{ private TMain.OnTaskItemChanged

  Event method that is called when item status has been changed. }

procedure TMain.OnTaskItemChanged(Sender: TObject; AItemChange: TItemChange);
begin
  // Refresh counter label
  lwTasks.Columns[1].Caption := FLang.Format(LID_TASKS_COUNTER, [FTasks.EnabledItemsCount,
    FTasks.Count]);

  // Change button states
  case AItemChange of
    icEnabled:
      begin
        bEnableTaskItem.Enabled := False;
        bDisableTaskitem.Enabled := True;
        pmChangeStatus.Caption := bDisableTaskitem.Caption;
      end;

    icDisabled:
      begin
        bEnableTaskItem.Enabled := True;
        bDisableTaskitem.Enabled := False;
        pmChangeStatus.Caption := bEnableTaskItem.Caption;
      end;

    icDeleted:
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
    if ((eTaskSearch.Text = '') or (Text.ToLower().Contains(LowerCase(eTaskSearch.Text)))) then
      with lwTasks.Items.Add do
      begin
        Caption := FTasks[i].GetStatusText(FLang);
        SubItems.AddObject(Text, FTasks[i]);
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

  // Refresh counter
  OnTaskItemChanged(Sender, icDeleted);
end;

{ private TMain.OnTaskSearchStart

  Event that is called when search starts. }

procedure TMain.OnTaskSearchStart(Sender: TObject);
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

procedure TMain.LanguageChanged();
begin
  with FLang do
  begin
    // File menu labels
    mmFile.Caption := GetString(33);

    case PageControl.ActivePageIndex of
      0,2: mmAdd.Caption := GetString(LID_STARTUP_ADD);
      1:   mmAdd.Caption := GetString(LID_CONTEXT_MENU_ADD);
      3:   mmAdd.Caption := GetString(LID_TASKS_ADD);
    end;  //of case

    mmImport.Caption := GetString(LID_BACKUP_IMPORT);
    mmExport.Caption := GetString(LID_ITEMS_EXPORT);
    mmClose.Caption := GetString(LID_QUIT);

    // Edit menu labels
    mmEdit.Caption := GetString(LID_EDIT);
    mmContext.Caption := GetString(LID_RECYCLEBIN_ENTRY);
    mmDeleteErasable.Caption := GetString(LID_DELETE_ERASABLE);

    // View menu labels
    mmView.Caption := GetString(LID_VIEW);
    mmRefresh.Caption := GetString(LID_REFRESH);
    mmDefault.Caption := GetString(LID_COLUMN_DEFAULT_SIZE);
    mmShowCaptions.Caption := GetString(LID_DESCRIPTION_SHOW);
    mmDate.Caption := GetString(LID_DATE_OF_DEACTIVATION);
    cbRunOnce.Caption := GetString(LID_STARTUP_RUNONCE);
    mmLang.Caption := GetString(LID_SELECT_LANGUAGE);

    // Help menu labels
    mmHelp.Caption := GetString(LID_HELP);
    mmUpdate.Caption := GetString(LID_UPDATE_SEARCH);
    mmInstallCertificate.Caption := GetString(LID_CERTIFICATE_INSTALL);
    mmReport.Caption := GetString(LID_REPORT_BUG);
    mmAbout.Caption := Format(LID_ABOUT, [Application.Title]);

    // "Startup" tab TButton labels
    tsStartup.Caption := GetString(LID_STARTUP);
    bEnableStartupItem.Caption := GetString(LID_ENABLE);
    bDisableStartupItem.Caption := GetString(LID_DISABLE);
    bExportStartupItem.Caption := GetString(LID_EXPORT);
    bDeleteStartupItem.Caption := GetString(LID_DELETE);
    bCloseStartup.Caption := mmClose.Caption;

    // "Startup" tab TListView labels
    lStartup.Caption := GetString(LID_STARTUP_HEADLINE);
    lwStartup.Columns[0].Caption := GetString(LID_ENABLED);
    lwStartup.Columns[2].Caption := StripHotkey(mmFile.Caption);
    lwStartup.Columns[3].Caption := GetString(LID_KEY);

    // Date column visible?
    if (lwStartup.Columns.Count > 4) then
      lwStartup.Columns[4].Caption := GetString(LID_DATE_OF_DEACTIVATION);

    lCopy1.Hint := GetString(LID_TO_WEBSITE);

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
    lContext.Caption := GetString(LID_CONTEXT_MENU_HEADLINE);
    lwContext.Columns[0].Caption := lwStartup.Columns[0].Caption;
    lwContext.Columns[2].Caption := GetString(LID_LOCATION);
    lwContext.Columns[3].Caption := lwStartup.Columns[3].Caption;
    lCopy2.Hint := lCopy1.Hint;

    // "Service" tab TButton labels
    tsService.Caption := GetString(LID_SERVICES);
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
    lwService.Columns[3].Caption := GetString(LID_SERVICE_START);

    // Date column visible?
    if (lwService.Columns.Count > 4) then
      lwService.Columns[4].Caption := GetString(LID_DATE_OF_DEACTIVATION);

    lCopy3.Hint := lCopy1.Hint;
    eServiceSearch.TextHint := eContextSearch.TextHint;

    // "Tasks" tab TButton labels
    tsTasks.Caption := GetString(LID_TASKS);
    lTasks.Caption := GetString(LID_TASKS_HEADLINE);
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
    pmOpenRegedit.Caption := GetString(LID_OPEN_IN_REGEDIT);
    pmOpenExplorer.Caption := GetString(LID_OPEN_IN_EXPLORER);
    pmEdit.Caption := GetString(LID_PATH_EDIT);
    pmExport.Caption := bExportStartupItem.Caption;
    pmDelete.Caption := bDeleteStartupItem.Caption;
    pmRename.Caption := GetString(LID_RENAME);
    pmCopyLocation.Caption := GetString(LID_LOCATION_COPY);
    pmChangeIcon.Caption := GetString(LID_CONTEXT_MENU_ICON_CHANGE);
    pmDeleteIcon.Caption := GetString(LID_CONTEXT_MENU_ICON_DELETE);
  end;  //of with

  // Refresh list captions
  if Assigned(FStartup) then
    FStartup.Refresh();

  if Assigned(FContext) then
    FContext.Refresh();

  if Assigned(FService) then
    FService.Refresh();

  if Assigned(FTasks) then
    FTasks.Refresh();
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
        Caption := FLang.GetString(LID_DATE_OF_DEACTIVATION);
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

  try
    SelectedList := GetSelectedList();

    // No item selected?
    if not Assigned(SelectedList.Selected) then
      raise EInvalidItem.Create('No item selected!');

    // Set a default file name
    if (PageControl.ActivePageIndex = 1) then
    begin
      if (FContext.Selected.LocationRoot = '*') then
        FileName := FContext.Selected.Name
      else
        if (FContext.Selected is TContextMenuShellNewItem) then
          FileName := FContext.Selected.Name +'_'+ TContextMenuShellNewItem.CanonicalName
        else
          FileName := FContext.Selected.Name +'_'+ FContext.Selected.LocationRoot;
    end  //of begin
    else
      FileName := SelectedList.Selected.Name;

    Filter := SelectedList.Selected.GetExportFilter(FLang);
    DefaultExt := SelectedList.Selected.GetBackupExtension();
    FileName := FileName + DefaultExt;

    // Show save dialog
    if PromptForFileName(FileName, Filter, DefaultExt, StripHotkey(pmExport.Caption),
      '', True) then
    begin
      SelectedList.ExportItem(FileName);
      Result := True;
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_EXPORT, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      FLang.ShowException(FLang.GetString([LID_EXPORT, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

{ public TMain.mmDeleteErasableClick

  Deletes erasable marked items. }

procedure TMain.mmDeleteErasableClick(Sender: TObject);
var
  i, Answer, ItemsDeleted: Integer;
  RootList: TRootList<TRootItem>;
  ListView: TListView;

begin
  try
    RootList := GetSelectedList();
    ListView := GetSelectedListView();

    // No erasable items?
    if (RootList.ErasableItemsCount = 0) then
    begin
      MessageDlg(FLang[LID_DELETE_ERASABLE_NO_ITEMS], mtInformation, [mbOK], 0);
      Exit;
    end;  //of begin

    if RootList.IsLocked() then
      raise EListBlocked.Create('Another operation is pending. Please wait!');

    ItemsDeleted := 0;

    for i := 0 to ListView.Items.Count - 1 do
    begin
      RootList.Selected := TRootItem(ListView.Items[i].SubItems.Objects[0]);

      if not RootList.Selected.Erasable then
        Continue;

      // Confirm deletion of every erasable item
      Answer := TaskMessageDlg(FLang.Format([LID_DELETE_ERASABLE_CONFIRM],
        [ListView.Items[i].SubItems[0]]), FLang.GetString([LID_ITEM_DELETE_CONFIRM1,
        LID_ITEM_DELETE_CONFIRM2]), mtConfirmation, mbYesNoCancel, 0);

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
              if not ShowExportItemDialog() then
                Continue;
            end;  //of begin

            if RootList.DeleteItem() then
            begin
              ListView.Items[i].Delete();
              Inc(ItemsDeleted);
            end;  //of begin

            // All eraseble items deleted?
            if (RootList.ErasableItemsCount = 0) then
              Break;
          end;
      end;  //of case
    end;  //of for

    MessageDlg(FLang.Format(LID_DELETE_ERASABLE_SUCCESS, [ItemsDeleted]),
      mtInformation, [mbOK], 0);

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_DELETE_ERASABLE, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      FLang.ShowException(FLang.GetString([LID_DELETE_ERASABLE, LID_IMPOSSIBLE]), E.Message);
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

procedure TMain.WMTimer(var Message: TWMTimer);
begin
  KillTimer(Handle, DELAY_TIMER);
  GetSelectedList().Refresh();
end;

{ TMain.bDeleteItemClick

  Event method that is called when user wants to delete an item. }

procedure TMain.bDeleteItemClick(Sender: TObject);
var
  ListView: TListView;
  RootList: TRootList<TRootItem>;
  ConfirmMessage: TLanguageId;

begin
  try
    ListView := GetSelectedListView();
    RootList := GetSelectedList();

    // Nothing selected?
    if (not Assigned(ListView.ItemFocused) or not Assigned(RootList.Selected)) then
      raise EInvalidItem.Create('No item selected!');

    // Different message per tab
    case PageControl.ActivePageIndex of
      0:   ConfirmMessage := LID_STARTUP_DELETE_CONFIRM;
      1:   ConfirmMessage := LID_CONTEXT_MENU_DELETE_CONFIRM;
      2:   ConfirmMessage := LID_SERVICE_DELETE_CONFIRM;
      3:   ConfirmMessage := LID_TASKS_DELETE_CONFIRM;
      else ConfirmMessage := LID_STARTUP_DELETE_CONFIRM;
    end;  //of case

    // Confirm deletion of item
    if (TaskMessageDlg(FLang.Format([ConfirmMessage], [ListView.ItemFocused.SubItems[0]]),
      FLang.GetString([LID_ITEM_DELETE_CONFIRM1, LID_ITEM_DELETE_CONFIRM2]),
      mtWarning, mbYesNo, 0, mbNo) = idYes) then
    begin
      // Ask user to export item
      if (MessageDlg(FLang.GetString(LID_ITEM_DELETE_STORE), mtConfirmation,
        mbYesNo, 0, mbYes) = idYes) then
      begin
        // User clicked cancel?
        if not ShowExportItemDialog() then
          Exit;
      end;  //of begin

      // Successfully deleted?
      if RootList.DeleteItem() then
      begin
        // Delete item from TListView
        ListView.DeleteSelected();
        ListView.ItemFocused := nil;
      end  //of begin
      else
        raise Exception.Create('Unknown error!');
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_DELETE, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EWarning do
    begin
      TaskMessageDlg(FLang.GetString([LID_DELETE, LID_IMPOSSIBLE]), E.Message,
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      FLang.ShowException(FLang.GetString([LID_DELETE, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

{ TMain.bDisableItemClick

  Event method that is called when user wants to disable an item. }

procedure TMain.bDisableItemClick(Sender: TObject);
var
  ListView: TListView;
  RootList: TRootList<TRootItem>;

begin
  try
    ListView := GetSelectedListView();
    RootList := GetSelectedList();

    // Nothing selected?
    if not Assigned(ListView.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    RootList.DisableItem();

    // Change item visual status
    ListView.ItemFocused.Caption := RootList.Selected.GetStatusText(FLang);

    // Item is erasable?
    if RootList.Selected.Erasable then
    begin
      MessageDlg(FLang.GetString([LID_FILE_DOES_NOT_EXIST, LID_ENTRY_CAN_DE_DELETED]),
        mtWarning, [mbOK], 0);
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_DISABLE, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EWarning do
    begin
      TaskMessageDlg(FLang.GetString([LID_DISABLE, LID_IMPOSSIBLE]), E.Message,
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      FLang.ShowException(FLang.GetString([LID_DISABLE, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

{ TMain.bEnableItemClick

  Enables currently selected item. }

procedure TMain.bEnableItemClick(Sender: TObject);
var
  ListView: TListView;
  RootList: TRootList<TRootItem>;

begin
  try
    ListView := GetSelectedListView();
    RootList := GetSelectedList();

    // Nothing selected?
    if not Assigned(ListView.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    RootList.EnableItem();

    // Change item visual status
    ListView.ItemFocused.Caption := RootList.Selected.GetStatusText(FLang);

    // Item is erasable?
    if RootList.Selected.Erasable then
    begin
      MessageDlg(FLang.GetString([LID_FILE_DOES_NOT_EXIST, LID_ENTRY_CAN_DE_DELETED]),
        mtWarning, [mbOK], 0);
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_ENABLE, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EWarning do
    begin
      TaskMessageDlg(FLang.GetString([LID_ENABLE, LID_IMPOSSIBLE]),
        E.Message, mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      FLang.ShowException(FLang.GetString([LID_ENABLE, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

{ TMain.bExportItemClick

  Event method that is called when user wants to export an item. }

procedure TMain.bExportItemClick(Sender: TObject);
begin
  ShowExportItemDialog();
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
  SetTimer(Handle, DELAY_TIMER, 250, nil);
end;

{ TMain.eSearchRightButtonClick

  Event method that is called when user clicked on clear. }

procedure TMain.eSearchRightButtonClick(Sender: TObject);
begin
  if ((Sender as TButtonedEdit).Text <> '') then
    (Sender as TButtonedEdit).Clear;
end;

{ TMain.CustomDrawItem

  Custom drawing of erasable items. }

procedure TMain.ListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  // Mark erasable items
  if TRootItem(Item.SubItems.Objects[0]).Erasable then
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
    FContext.Selected := TContextMenuListItem(Item.SubItems.Objects[0]);

    // Change button states
    bEnableContextItem.Enabled := not FContext.Selected.Enabled;
    bDisableContextItem.Enabled := not bEnableContextItem.Enabled;
    bDeleteContextItem.Enabled := True;
    bExportContextItem.Enabled := True;

    // Allow popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
  begin
    // Nothing selected
    lwContext.ItemFocused := nil;
    FContext.Selected := nil;
    bEnableContextItem.Enabled := False;
    bDisableContextItem.Enabled := False;
    bDeleteContextItem.Enabled := False;
    bExportContextItem.Enabled := False;

    // Disallow popup menu
    PopupMenu.AutoPopup := False;
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
    FService.Selected := TServiceListItem(Item.SubItems.Objects[0]);

    // Change button states
    bEnableServiceItem.Enabled := not FService.Selected.Enabled;
    bDisableServiceItem.Enabled := not bEnableServiceItem.Enabled;
    bDeleteServiceItem.Enabled := True;
    bExportServiceItem.Enabled := True;

    // Allow popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
  begin
    // Nothing selected
    lwService.ItemFocused := nil;
    FService.Selected := nil;
    bEnableServiceItem.Enabled := False;
    bDisableServiceItem.Enabled := False;
    bDeleteServiceItem.Enabled := False;
    bExportServiceItem.Enabled := False;

    // Disallow popup menu
    PopupMenu.AutoPopup := False;
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
    FTasks.Selected := TTaskListItem(Item.SubItems.Objects[0]);

    // Change button states
    bEnableTaskItem.Enabled := not FTasks.Selected.Enabled;
    bDisableTaskitem.Enabled := not bEnableTaskItem.Enabled;
    bDeleteTaskItem.Enabled := True;
    bExportTaskItem.Enabled := True;

    // Allow popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
  begin
    // Nothing selected
    lwTasks.ItemFocused := nil;
    FTasks.Selected := nil;
    bEnableTaskItem.Enabled := False;
    bDisableTaskitem.Enabled := False;
    bDeleteTaskItem.Enabled := False;
    bExportTaskItem.Enabled := False;

    // Disallow popup menu
    PopupMenu.AutoPopup := False;
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
    List.Columns[i].Tag := SORT_ASCENDING;
  end;  //of begin

  // Current column
  Header_GetItem(Header, Column.Index, Item);

  // Sorted ascending?
  if (Item.fmt and HDF_SORTUP <> 0) then
  begin
    Item.fmt := Item.fmt and not HDF_SORTUP or HDF_SORTDOWN;
    Column.Tag := SORT_DESCENDING;
  end  //of begin
  else
  begin
    Item.fmt := Item.fmt and not HDF_SORTDOWN or HDF_SORTUP;
    Column.Tag := SORT_ASCENDING;
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
  ColumnToSort: NativeInt;

begin
  List := (Sender as TListView);
  ColumnToSort := List.Tag;

  // Status column?
  if (ColumnToSort = 0) then
    case List.Columns[ColumnToSort].Tag of
      SORT_ASCENDING:  Compare := CompareText(Item1.Caption, Item2.Caption);
      SORT_DESCENDING: Compare := CompareText(Item2.Caption, Item1.Caption);
    end  //of case
  else
  begin
    Data := ColumnToSort - 1;

    if (Data < 3) then
      case List.Columns[ColumnToSort].Tag of
        SORT_ASCENDING:  Compare := CompareText(Item1.SubItems[Data], Item2.SubItems[Data]);
        SORT_DESCENDING: Compare := CompareText(Item2.SubItems[Data], Item1.SubItems[Data]);
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
      bDisableStartupItem.Click;
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
  if List.ItemFocused.SubItems[0].StartsWith(Key, True) then
    StartIndex := List.ItemFocused.Index + 1;

  // Find next item whose first char starts with key
  for i := StartIndex to List.Items.Count - 1 do
    if List.Items[i].SubItems[0].StartsWith(Key, True) then
    begin
      List.ItemIndex := i;
      List.ItemFocused := List.Items[i];
      List.ItemFocused.MakeVisible(True);
      Break;
    end;  //of begin
end;

{ TMain.lwStartupSelectItem

  Event method that is called when user selects an item in list. }

procedure TMain.lwStartupSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  // Item selected?
  if (Selected and Assigned(Item)) then
  begin
    FStartup.Selected := TStartupListItem(Item.SubItems.Objects[0]);

    // Change button states
    bEnableStartupItem.Enabled := not FStartup.Selected.Enabled;
    bDisableStartupItem.Enabled := not bEnableStartupItem.Enabled;
    bDeleteStartupItem.Enabled := True;
    bExportStartupItem.Enabled := True;

    // Allow popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
  begin
    // Nothing selected
    lwStartup.ItemFocused := nil;
    FStartup.Selected := nil;
    bEnableStartupItem.Enabled := False;
    bDisableStartupItem.Enabled := False;
    bDeleteStartupItem.Enabled := False;
    bExportStartupItem.Enabled := False;

    // Disallow popup menu
    PopupMenu.AutoPopup := False;
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

begin
  try
    // Only icon of shell item can be changed
    if not (FContext.Selected is TContextMenuShellItem) then
      Exit;

    if PromptForFileName(FileName, 'Application *.exe|*.exe|Icon *.ico|*.ico',
      '', StripHotkey(pmChangeIcon.Caption)) then
    begin
      if not (FContext.Selected as TContextMenuShellItem).ChangeIcon('"'+ FileName +'"') then
        raise Exception.Create('Unknown error!');

      pmDeleteIcon.Visible := True;
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_CONTEXT_MENU_ICON_CHANGE, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: Exception do
    begin
      FLang.ShowException(FLang.GetString([LID_CONTEXT_MENU_ICON_CHANGE,
        LID_IMPOSSIBLE]), E.Message);
    end;
  end;  //of try
end;

{ TMain.pmDeleteIconClick

  Popup menu entry to delete the icon of the current selected shell item. }

procedure TMain.pmDeleteIconClick(Sender: TObject);
begin
  try
    // Only icon of shell item can be deleted
    if not (FContext.Selected is TContextMenuShellItem) then
      Exit;

    // Show confimation
    if (MessageDlg(FLang.GetString(LID_CONTEXT_MENU_ICON_DELETE_CONFIRM),
      mtConfirmation, mbYesNo, 0) = idYes) then
    begin
      if not (FContext.Selected as TContextMenuShellItem).DeleteIcon() then
        raise Exception.Create('Unknown error!');

      pmDeleteIcon.Visible := False;
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_CONTEXT_MENU_ICON_DELETE, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: Exception do
    begin
      FLang.ShowException(FLang.GetString([LID_CONTEXT_MENU_ICON_DELETE,
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
      MessageDlg(FLang.GetString([LID_FILE_DOES_NOT_EXIST, LID_ENTRY_CAN_DE_DELETED]),
        mtWarning, [mbOK], 0);
    end;

    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_OPEN_IN_EXPLORER, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;
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
    begin
      TaskMessageDlg(FLang.GetString([LID_OPEN_IN_REGEDIT, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;
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

    // The caption of startup and task items can not be renamed
    if (PageControl.ActivePageIndex in [0, 3]) then
      Name := Item.Name
    else
      Name := Item.Caption;

    if InputQuery(StripHotkey(pmRename.Caption), StripHotkey(pmRename.Caption), Name) then
    begin
      if ((Trim(Name) = '') or (Name = Item.Name) or (Name = Item.Caption)) then
        Exit;

      GetSelectedList().RenameItem(Name);

      // Names are visible instead of captions?
      if (not mmShowCaptions.Checked or (PageControl.ActivePageIndex in [1, 3])) then
        GetSelectedListView().ItemFocused.SubItems[0] := Name;
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_RENAME, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EWarning do
    begin
      TaskMessageDlg(FLang.GetString([LID_RENAME, LID_IMPOSSIBLE]),
        FLang.GetString(LID_ENTRY_ALREADY_EXISTS), mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      FLang.ShowException(FLang.GetString([LID_RENAME, LID_IMPOSSIBLE]), E.Message);
  end;  //of try
end;

procedure TMain.PopupMenuPopup(Sender: TObject);
var
  SelectedItem: TRootItem;

begin
  // OnPopup also occurs when hotkeys are pressed
  if not PopupMenu.AutoPopup then
    Exit;

  // Since this point cannot be reached when no item is selected EInvalidItem is
  // assumed not to be raised
  SelectedItem := GetSelectedItem();

  // Change text
  if SelectedItem.Enabled then
    pmChangeStatus.Caption := bDisableStartupItem.Caption
  else
    pmChangeStatus.Caption := bEnableStartupItem.Caption;

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
        pmEdit.Enabled := True;
      end;

    1:
      begin
        pmOpenRegedit.Enabled := True;

        // ShellNew and cascading Shell items cannot be opened in Explorer
        pmOpenExplorer.Enabled := not (SelectedItem is TContextMenuShellNewItem) and not
          (SelectedItem is TContextMenuShellCascadingItem);

        // Only Shell contextmenu items can be renamed
        pmRename.Enabled := (SelectedItem is TContextMenuShellItem);

        // Only icon of Shell contextmenu items can be changed
        pmChangeIcon.Visible := pmRename.Enabled;
        pmDeleteIcon.Visible := (pmRename.Enabled and (SelectedItem.Icon <> 0));
        pmEdit.Enabled := (SelectedItem.FileName <> '');
      end;

    2:
      begin
        pmOpenRegedit.Enabled := True;
        pmOpenExplorer.Enabled := True;
        pmRename.Enabled := True;
        pmDeleteIcon.Visible := False;
        pmChangeIcon.Visible := False;
        pmEdit.Enabled := (SelectedItem.FileName <> '');
      end;

    3:
      begin
        pmOpenRegedit.Enabled := False;
        pmOpenExplorer.Enabled := True;
        pmRename.Enabled := True;
        pmDeleteIcon.Visible := False;
        pmChangeIcon.Visible := False;
        pmEdit.Enabled := (SelectedItem.FileName <> '');
      end;
  end;  //of case
end;

{ TMain.pmCopyLocationClick

  Popup menu entry to show some properties. }

procedure TMain.pmCopyLocationClick(Sender: TObject);
var
  Item: TRootItem;

begin
  try
    Item := GetSelectedItem();

    if (Item is TStartupItem) then
      Clipboard.AsText := TStartupItem(Item).RootKey.ToString() +'\'+ TStartupItem(Item).Wow64Location
    else
      Clipboard.AsText := Item.LocationFull;

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_LOCATION_COPY, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;
  end;  //of try
end;

{ TMain.pmEditClick

  Popup menu entry to edit the path of a program. }

procedure TMain.pmEditClick(Sender: TObject);
var
  Path, EnteredPath: string;

begin
  try
    Path := GetSelectedItem().FileName;

    // Show input box for editing path
    EnteredPath := InputBox(FLang.GetString(LID_PATH_EDIT),
      FLang.GetString(LID_ITEM_CHANGE_PATH), Path);

    // Nothing entered or nothing changed
    if ((Trim(EnteredPath) = '') or (EnteredPath = Path)) then
      Exit;

    // Try to change the file path
    GetSelectedList().ChangeItemFilePath(EnteredPath);

    // Update file path in TListView
    if (PageControl.ActivePageIndex <> 1) then
      GetSelectedListView().ItemFocused.SubItems[1] := EnteredPath;

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_PATH_EDIT, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: Exception do
      FLang.ShowException(FLang.GetString([LID_PATH_EDIT, LID_IMPOSSIBLE]), E.Message);
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

    Assert(Name <> '', 'Name must not be empty!');

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

             if not FContext.Add(FileName, Args, Location, Name, Extended) then
               raise Exception.Create('Item was not added!');

             // User choice exists for selected file extension?
             if FContext.Last.UserChoiceExists(Location) then
             begin
               // Delete user choice?
               if (MessageDlg(FLang.Format(LID_CONTEXT_MENU_USER_CHOICE_WARNING1,
                 [LID_CONTEXT_MENU_USER_CHOICE_WARNING2, LID_CONTEXT_MENU_USER_CHOICE_RESET]),
                 mtConfirmation, mbYesNo, 0) = idYes) then
                 FContext.Last.DeleteUserChoice(Location);
             end;

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
    begin
      TaskMessageDlg(StripHotKey(mmAdd.Caption) + FLang.GetString(LID_IMPOSSIBLE),
        FLang.GetString(LID_ENTRY_ALREADY_EXISTS), mtWarning, [mbOK], 0);
    end;

    on E: EAssertionFailed do
    begin
      TaskMessageDlg(StripHotKey(mmAdd.Caption) + FLang.GetString(LID_IMPOSSIBLE),
        E.Message, mtWarning, [mbOK], 0);
    end;

    on E: Exception do
    begin
      FLang.ShowException(StripHotKey(mmAdd.Caption) + FLang.GetString(LID_IMPOSSIBLE),
        E.Message);
    end;
  end;  //of try
end;

{ TMain.mmDateClick

  MainMenu entry to add or remove the deactivation timestamp column. }

procedure TMain.mmDateClick(Sender: TObject);
var
  ListView: TListView;

begin
  case PageControl.ActivePageIndex of
    0:   ListView := lwStartup;
    2:   ListView := lwService;
    else Exit;
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

{ TMain.mmExportListClick

  MainMenu entry to export the complete list as .reg (backup) file. }

procedure TMain.mmExportClick(Sender: TObject);
var
  SelectedList: TRootList<TRootItem>;
  FileName, Filter, DefaultExt: string;

begin
  try
    SelectedList := TRootList<TRootItem>(GetSelectedList());

    // Operation pending?
    if SelectedList.IsLocked() then
      raise EListBlocked.Create('Another operation is pending. Please wait!');

    Filter := SelectedList.GetExportFilter(FLang);
    DefaultExt := SelectedList.GetBackupExtension();
    FileName := PageControl.ActivePage.Caption + DefaultExt;

    // Show save dialog
    if PromptForFileName(FileName, Filter, DefaultExt, StripHotkey(mmExport.Caption),
      '', True) then
    begin
      // Export list (threaded!)
      with TExportListThread.Create(SelectedList, FileName, PageControl.ActivePageIndex) do
      begin
        OnStart := OnExportListStart;
        OnTerminate := OnExportListEnd;
        OnError := OnExportListError;
        Start;
      end;  //of with
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_ITEMS_EXPORT, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

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
        raise EWarning.Create(FLang.GetString(LID_ENTRY_ALREADY_EXISTS));
    end;  //of begin

  except
    on E: EInvalidItem do
    begin
      TaskMessageDlg(FLang.GetString([LID_BACKUP_IMPORT, LID_IMPOSSIBLE]),
        FLang.GetString(LID_NOTHING_SELECTED), mtWarning, [mbOK], 0);
    end;

    on E: EListBlocked do
    begin
      MessageDlg(FLang.GetString([LID_OPERATION_PENDING1, LID_OPERATION_PENDING2]),
        mtWarning, [mbOK], 0);
    end;

    on E: EWarning do
    begin
      TaskMessageDlg(FLang.GetString([LID_BACKUP_IMPORT, LID_IMPOSSIBLE]),
        E.Message, mtWarning, [mbOK], 0);
    end;

    on E: Exception do
    begin
      FLang.ShowException(FLang.GetString([LID_BACKUP_IMPORT, LID_IMPOSSIBLE]),
        E.Message);
    end;
  end;  //of try
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
      Reg.WriteString('', FLang.GetString(LID_OPEN_CLEARAS));
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
      FLang.ShowException('Adding Clearas to recycle bin context menu failed!', E.Message);
  end;  //of try
end;

{ TMain.mmRefreshClick

  MainMenu entry to refresh the current shown TListView. }

procedure TMain.mmRefreshClick(Sender: TObject);
begin
  LoadItems();
end;

{ TMain.mmShowCaptionsClick

  MainMenu entry to show captions instead of names. }

procedure TMain.mmShowCaptionsClick(Sender: TObject);
begin
  FStartup.Refresh();
  FContext.Refresh();
  FService.Refresh();
  FTasks.Refresh();
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
begin
  try
    // Certificate already installed?
    if CertificateExists() then
    begin
      MessageDlg(FLang.GetString(LID_CERTIFICATE_ALREADY_INSTALLED),
        mtInformation, [mbOK], 0);
    end  //of begin
    else
      InstallCertificate();

  except
    on E: EOSError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

{ TMain.mmUpdateClick

  MainMenu entry that allows users to manually search for updates. }

procedure TMain.mmUpdateClick(Sender: TObject);
begin
  FUpdateCheck.NotifyNoUpdate := True;
  FUpdateCheck.CheckForUpdate();
end;

{ TMain.mmReportClick

  MainMenu entry that allows users to easily report a bug by opening the web
  browser and using the "report bug" formular. }

procedure TMain.mmReportClick(Sender: TObject);
begin
  OpenUrl(URL_CONTACT);
end;

{ TMain.mmAboutClick

  MainMenu entry that shows a info page with build number and version history. }

procedure TMain.mmAboutClick(Sender: TObject);
var
  AboutDialog: TAboutDialog;
  Description, Changelog: TResourceStream;

begin
  AboutDialog := TAboutDialog.Create(Self);
  Description := TResourceStream.Create(HInstance, RESOURCE_DESCRIPTION, RT_RCDATA);
  Changelog := TResourceStream.Create(HInstance, RESOURCE_CHANGELOG, RT_RCDATA);

  try
    AboutDialog.Title := StripHotkey(mmAbout.Caption);
    AboutDialog.Description.LoadFromStream(Description);
    AboutDialog.Changelog.LoadFromStream(Changelog);
    AboutDialog.Execute();

  finally
    Changelog.Free;
    Description.Free;
    AboutDialog.Free;
  end;  //of begin
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
         mmAdd.Caption := FLang.GetString(LID_STARTUP_ADD);
         mmImport.Enabled := True;
         mmDate.Enabled := True;
         mmShowCaptions.Enabled := True;
         ShowColumnDate(lwStartup, (mmDate.Enabled and mmDate.Checked));
       end;

    1: begin
         mmAdd.Caption := FLang.GetString(LID_CONTEXT_MENU_ADD);
         mmImport.Enabled := False;
         mmDate.Enabled := False;
         mmShowCaptions.Enabled := True;
       end;

    2: begin
         mmAdd.Caption := FLang.GetString(LID_SERVICE_ADD);
         mmImport.Enabled := False;
         mmDate.Enabled := True;
         mmShowCaptions.Enabled := True;
         ShowColumnDate(lwService, mmDate.Checked);
       end;

    3: begin
         mmAdd.Caption := FLang.GetString(LID_TASKS_ADD);
         mmImport.Enabled := True;
         mmDate.Enabled := False;
         mmShowCaptions.Enabled := False;
       end;
  end;  //of case

  // Load items dynamically
  if (GetSelectedList().Count = 0) then
    LoadItems();

  // Only allow popup menu if item is focused
  PopupMenu.AutoPopup := Assigned(GetSelectedListView().ItemFocused);
end;

{ TMain.bCloseStartupClick

  Closes Clearas. }

procedure TMain.bCloseStartupClick(Sender: TObject);
begin
  Close;
end;

end.
