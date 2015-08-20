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
  Vcl.ClipBrd, Vcl.ImgList, Registry, StrUtils, ClearasAPI, ExportListThread,
  PMCWAbout, PMCWLanguageFile, PMCWOSUtils, PMCWUpdater, PMCWDialogs,
  System.ImageList, Winapi.CommCtrl;

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
    pbContextLoad: TProgressBar;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bCloseStartupClick(Sender: TObject);
    procedure bDeleteStartupItemClick(Sender: TObject);
    procedure bDeleteContextItemClick(Sender: TObject);
    procedure bDeleteServiceItemClick(Sender: TObject);
    procedure bEnableStartupItemClick(Sender: TObject);
    procedure bEnableContextItemClick(Sender: TObject);
    procedure bEnableServiceItemClick(Sender: TObject);
    procedure bDisableStartupItemClick(Sender: TObject);
    procedure bDisableContextItemClick(Sender: TObject);
    procedure bDisableServiceItemClick(Sender: TObject);
    procedure bExportStartupItemClick(Sender: TObject);
    procedure bExportContextItemClick(Sender: TObject);
    procedure bExportServiceItemClick(Sender: TObject);
    procedure eContextSearchChange(Sender: TObject);
    procedure lwContextDblClick(Sender: TObject);
    procedure lwContextSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lwServiceDblClick(Sender: TObject);
    procedure lwServiceSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lwStartupColumnClick(Sender: TObject; Column: TListColumn);
    procedure lwStartupCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lwStartupDblClick(Sender: TObject);
    procedure lwStartupKeyPress(Sender: TObject; var Key: Char);
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
    procedure mmShowIconsClick(Sender: TObject);
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
    procedure eContextSearchRightButtonClick(Sender: TObject);
  private
    FColumnToSort: Word;
    FStartup: TStartupList;
    FContext: TContextList;
    FService: TServiceList;
    FLang: TLanguageFile;
    FUpdateCheck: TUpdateCheck;
    function CreateStartupUserBackup(): Boolean;
    function GetSelectedItem(): TRootItem;
    function GetSelectedList(): TRootList;
    procedure LoadContextMenuItems(ATotalRefresh: Boolean = True);
    procedure LoadStartupItems(ATotalRefresh: Boolean = True);
    procedure LoadServiceItems(ATotalRefresh: Boolean = True);
    procedure OnContextSearchStart(Sender: TObject; const AWorkCountMax: Cardinal);
    procedure OnContextSearching(Sender: TObject; const AProgress: Cardinal);
    procedure OnContextSearchEnd(Sender: TObject);
    procedure OnContextItemChanged(Sender: TObject);
    procedure OnExportListStart(Sender: TObject; const APageControlIndex: Cardinal);
    procedure OnExportListEnd(Sender: TObject; const APageControlIndex: Cardinal);
    procedure OnStartupSearchStart(Sender: TObject; const AWorkCountMax: Cardinal);
    procedure OnStartupSearchEnd(Sender: TObject);
    procedure OnStartupItemChanged(Sender: TObject);
    procedure OnServiceSearchStart(Sender: TObject; const AWorkCountMax: Cardinal);
    procedure OnServiceSearchEnd(Sender: TObject);
    procedure OnServiceItemChanged(Sender: TObject);
    procedure OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
    procedure SetLanguage(Sender: TObject);
    function ShowRegistryExportDialog(): Boolean;
    procedure ShowColumnDate(AListView: TListView; AShow: Boolean = True);
    function UpdateContextPath(): Boolean; overload;
    function UpdateContextPath(ALangFile: TLanguageFile): Boolean; overload; deprecated;
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
    OnSearchStart := OnStartupSearchStart;
    OnSearchFinish := OnStartupSearchEnd;
  end;  //of with

  FContext := TContextList.Create;

  // Link search events
  with FContext do
  begin
    OnChanged := OnContextItemChanged;
    OnSearchStart := OnContextSearchStart;
    OnSearching := OnContextSearching;
    OnSearchFinish := OnContextSearchEnd;
  end;  //of with

  FService := TServiceList.Create;

  // Link search events
  with FService do
  begin
    OnSearchStart := OnServiceSearchStart;
    OnSearchFinish := OnServiceSearchEnd;
    OnChanged := OnServiceItemChanged;
  end;  //of with

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

  // At least Windows 2000!
  if not TOSVersion.Check(5) then
  begin
    FLang.ShowMessage(FLang.Format([64, 65], [TOSVersion.Name]), mtError);
    mmExportList.Enabled := False;
    mmRefresh.Enabled := False;
    mmContext.Enabled := False;
    mmDate.Enabled := False;
    lwStartup.Enabled := False;
    cbRunOnce.Enabled := False;
    lwContext.Enabled := False;
    cbContextExpert.Enabled := False;
    eContextSearch.Enabled := False;
    lwService.Enabled := False;
    cbServiceExpert.Enabled := False;
    eServiceSearch.Enabled := False;
    Exit;
  end;  //of if

  // Show "date of deactivation" only on Vista and later
  mmDate.Enabled := TOSVersion.Check(6);

  // Update Clearas recycle bin context menu entry
  mmContext.Checked := UpdateContextPath();

  // Load autostart
  LoadStartupItems();
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
    if not Assigned(FStartup.Selected) then
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
      Result := ShowRegistryExportDialog();

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([95, 18]), FLang.GetString(53), mtWarning);

    on E: EStartupException do
      FLang.ShowException(FLang.GetString([95, 18]), FLang.GetString(43));

    on E: Exception do
      FLang.ShowException(FLang.GetString([95, 18]), E.Message);
  end;  //of try
end;

{ private TMain.GetSelectedItem

  Returns the current selected TRootItem. }

function TMain.GetSelectedItem(): TRootItem;
var
  Item: TRootItem;
  List: TRootList;

begin
  List := GetSelectedList();
  Item := List.Selected;

  if not Assigned(Item) then
    raise EInvalidItem.Create('No item selected!');

  Result := Item;
end;

{ private TMain.GetSelectedList

  Returns the current selected TRootList. }

function TMain.GetSelectedList(): TRootList;
var
  List: TRootList;

begin
  List := nil;

  case PageControl.ActivePageIndex of
    0: List := FStartup;
    1: List := FContext;
    2: List := FService;
  end;  //of case

  if not Assigned(List) then
    raise EInvalidItem.Create('No list selected!');

  Result := List;
end;

{ private TMain.LoadContextMenuItems

  Loads context menu entries and brings them into a TListView. }

procedure TMain.LoadContextMenuItems(ATotalRefresh: Boolean = True);
begin
  // Clear all visual data
  lwContext.Clear;

  // Disable VCL
  bDisableContextItem.Enabled := False;
  bEnableContextItem.Enabled := False;
  bDeleteContextItem.Enabled := False;
  bExportContextItem.Enabled := False;

  // Make a total refresh or just use cached items
  if ATotalRefresh then
  begin
    // Use expert search mode?
    if cbContextExpert.Checked then
      // Start the expert search (threaded!)
      FContext.LoadContextmenus()
    else
      // Use default search mode (threaded!)
      FContext.LoadContextMenus(CM_LOCATIONS_DEFAULT);
  end  //of begin
  else
    OnContextSearchEnd(Self);
end;

{ private TMain.LoadStartupItems

  Loads startup entries and brings them into a TListView. }

procedure TMain.LoadStartupItems(ATotalRefresh: Boolean = True);
begin
  // Clear all visual data
  lwStartup.Clear;

  // Disable VCL
  bDisableStartupItem.Enabled := False;
  bEnableStartupItem.Enabled := False;
  bDeleteStartupItem.Enabled := False;
  bExportStartupItem.Enabled := False;

  // Make a total refresh or just use cached items
  if ATotalRefresh then
    // Load autostart with or without special RunOnce entries (threaded!)
    FStartup.LoadStartup(cbRunOnce.Checked)
  else
    OnStartupSearchEnd(Self);
end;

{ private TMain.LoadServiceItems

  Loads service entries and brings them into a TListView. }

procedure TMain.LoadServiceItems(ATotalRefresh: Boolean = True);
begin
  // Clear all visual data
  lwService.Clear;

  // Disable VCL
  bDisableServiceItem.Enabled := False;
  bEnableServiceItem.Enabled := False;
  bDeleteServiceItem.Enabled := False;
  bExportServiceItem.Enabled := False;

  // Make a total refresh or just use cached items
  if ATotalRefresh then
    // Load service items (threaded!)
    FService.LoadServices(cbServiceExpert.Checked)
  else
    OnServiceSearchEnd(Self);
end;

{ private TMain.OnContextSearchStart

  Event that is called when search starts. }

procedure TMain.OnContextSearchStart(Sender: TObject;
  const AWorkCountMax: Cardinal);
begin
  mmLang.Enabled := False;
  cbContextExpert.Enabled := False;
  lwContext.Cursor := crHourGlass;

  if (AWorkCountMax > 0) then
  begin
    eContextSearch.Visible := False;
    pbContextLoad.Visible := True;
    pbContextLoad.Max := AWorkCountMax;
  end;  //of begin
end;

{ private TMain.OnContextSearching

  Event that is called when search is in progress. }

procedure TMain.OnContextSearching(Sender: TObject; const AProgress: Cardinal);
begin
  pbContextLoad.Position := AProgress;
end;

{ private TMain.OnContextSearchEnd

  Event that is called when search ends. }

procedure TMain.OnContextSearchEnd(Sender: TObject);
var
  i: Integer;
  Text: string;

begin
  // Print all information about context menu entries
  for i := 0 to FContext.Count - 1 do
  begin
    // Show name or caption of item?
    if (FContext[i].Caption <> '') then
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

  // Refresh counter label
  OnContextItemChanged(Sender);

  // Update some VCL
  pbContextLoad.Visible := False;
  pbContextLoad.Position := 0;
  eContextSearch.Visible := True;
  mmLang.Enabled := True;
  cbContextExpert.Enabled := True;
  lwContext.Cursor := crDefault;
end;

{ private TMain.OnContextItemChanged

  Refreshs the counter label of context menu items. }

procedure TMain.OnContextItemChanged(Sender: TObject);
begin
  lwContext.Columns[1].Caption := FLang.Format(87, [FContext.Enabled,
    FContext.Count]);
end;

{ private TMain.OnExportListStart

  Event that is called when export list starts. }

procedure TMain.OnExportListStart(Sender: TObject; const APageControlIndex: Cardinal);
begin
  eContextSearch.Visible := False;
  pbContextLoad.Visible := True;
  pbContextLoad.Style := pbstMarquee;
  PageControl.Pages[APageControlIndex].Cursor := crHourGlass;

  case APageControlIndex of
    0: lwStartup.Cursor := crHourGlass;
    1: lwContext.Cursor := crHourGlass;
    2: lwService.Cursor := crHourGlass;
  end;  //of case
end;

{ private TMain.OnExportListEnd

  Event that is called when export list ends. }

procedure TMain.OnExportListEnd(Sender: TObject; const APageControlIndex: Cardinal);
begin
  eContextSearch.Visible := True;
  pbContextLoad.Visible := False;
  pbContextLoad.Style := pbstNormal;
  PageControl.Pages[APageControlIndex].Cursor := crDefault;

  case APageControlIndex of
    0: lwStartup.Cursor := crDefault;
    1: lwContext.Cursor := crDefault;
    2: lwService.Cursor := crDefault;
  end;  //of case
end;

{ private TMain.OnStartupSearchStart

  Event that is called when search starts. }

procedure TMain.OnStartupSearchStart(Sender: TObject;
  const AWorkCountMax: Cardinal);
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

  // Refresh counter label
  OnStartupItemChanged(Sender);
  mmImport.Enabled := True;
  mmLang.Enabled := True;
  cbRunOnce.Enabled := True;
  lwStartup.Cursor := crDefault;
end;

{ private TMain.OnStartupItemChanged

  Refreshs the counter label of startup items. }

procedure TMain.OnStartupItemChanged(Sender: TObject);
begin
  lwStartup.Columns[1].Caption := FLang.Format(88, [FStartup.Enabled,
    FStartup.Count]);
end;

{ private TMain.OnServiceSearchStart

  Event that is called when search starts. }

procedure TMain.OnServiceSearchStart(Sender: TObject;
  const AWorkCountMax: Cardinal);
begin
  mmLang.Enabled := False;
  cbServiceExpert.Enabled := False;
  lwService.Cursor := crHourGlass;
end;

{ private TMain.OnServiceSearchEnd

  Event that is called when search ends. }

procedure TMain.OnServiceSearchEnd(Sender: TObject);
var
  i: Integer;
  Text: string;

begin
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

  // Refresh counter label
  OnServiceItemChanged(Sender);
  mmLang.Enabled := True;
  cbServiceExpert.Enabled := True;
  lwService.Cursor := crDefault;
end;

{ private TMain.OnServiceItemChanged

  Refreshs the counter label of service items. }

procedure TMain.OnServiceItemChanged(Sender: TObject);
begin
  lwService.Columns[1].Caption := FLang.Format(56, [FService.Enabled,
    FService.Count]);
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

    // Set placeholder text for search
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

    // Set placeholder text for search
    eServiceSearch.TextHint := eContextSearch.TextHint;

    // Popup menu labels
    pmChangeStatus.Caption := bDisableStartupItem.Caption;
    pmOpenRegedit.Caption := GetString(66);
    pmOpenExplorer.Caption := GetString(51);
    pmEdit.Caption := GetString(104);
    pmExport.Caption := mmExport.Caption;
    pmDelete.Caption := bDeleteStartupItem.Caption;
    pmCopyLocation.Caption := GetString(106);
  end;  //of with

  // Update TListView captions
  if Assigned(FStartup) and Assigned(FContext) and Assigned(FService) then
  begin
    LoadStartupItems(False);
    LoadContextMenuItems(False);
    LoadServiceItems(False);
  end;  //of begin
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
        mmShowIconsClick(Self);
      end;  //of with
end;

{ private TMain.ShowRegistryExportDialog

  Shows a .reg file export dialog. }

function TMain.ShowRegistryExportDialog(): Boolean;
var
  SaveDialog: TSaveDialog;

begin
  Result := False;
  SaveDialog := TSaveDialog.Create(Self);

  try
    // Set TSaveDialog options
    with SaveDialog do
    begin
      Title := StripHotkey(bExportStartupItem.Caption);
      InitialDir := '%HOMEPATH%';

      // Confirm overwrite
      Options := Options + [ofOverwritePrompt];

      // Filter .reg files only
      Filter := FLang.GetString(36);
      DefaultExt := '.reg';

      // Set a default file name
      case PageControl.ActivePageIndex of
        0: FileName := FStartup.Selected.Name + DefaultExt;
        1: FileName := FContext.Selected.Name +'_'+ FContext.Selected.Location + DefaultExt;
        2: FileName := FService.Selected.Name + DefaultExt;
      end;  //of case
    end;  //of with

    try
      // User clicked "save"?
      if SaveDialog.Execute then
      begin
        GetSelectedList().ExportItem(SaveDialog.FileName);
        Result := True;
      end;  //of begin

    finally
      SaveDialog.Free;
    end;  //of try

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

{ public TMain.UpdateContextPath

  Updates "Open Clearas" in recycle bin context menu. }

function TMain.UpdateContextPath(ALangFile: TLanguageFile): Boolean;
var
  Reg: TRegistry;
  ClearasKey: string;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(KEY_RECYCLEBIN, False);
    ClearasKey := ALangFile.GetString(107);

    // Only update if context menu entry exists
    if Reg.KeyExists(ClearasKey) then
    begin
      // Delete old context menu key
      if not Reg.DeleteKey(ClearasKey) then
        raise Exception.Create('Could not delete key: '+ ClearasKey);
    end;  //of begin

  finally
    Reg.CloseKey();
    Reg.Free;
    Result := UpdateContextPath();
  end;  //of try
end;

{ TMain.bDeleteServiceItemClick

  Deletes currently selected service item. }

procedure TMain.bDeleteServiceItemClick(Sender: TObject);
var
  Answer: Integer;

begin
  try
    // Nothing selected?
    if (not Assigned(lwService.ItemFocused) or not Assigned(FService.Selected)) then
      raise EInvalidItem.Create('No item selected!');

    // Confirm deletion of item
    if (FLang.ShowMessage(FLang.Format([58], [lwService.ItemFocused.SubItems[0]]),
      FLang.GetString([49, 50]), mtCustom) = IDYES) then
    begin
      // Ask user to export item
      Answer := FLang.ShowMessage(FLang.GetString(52), mtConfirmation);

      // Abort if user clicks cancel!
      if (((Answer = IDYES) and ShowRegistryExportDialog()) or (Answer = IDNO)) then
        // Successfully deleted item physically?
        if FService.DeleteItem() then
        begin
          // Delete item from TListView
          lwService.DeleteSelected();
          lwService.ItemFocused := nil;

          // Disable VCL buttons
          bEnableServiceItem.Enabled := False;
          bDisableServiceItem.Enabled := False;
          bDeleteServiceItem.Enabled := False;
          bExportServiceItem.Enabled := False;
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

{ TMain.bDeleteStartupItemClick

  Deletes currently selected service item. }

procedure TMain.bDeleteStartupItemClick(Sender: TObject);
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

        // Disable VCL buttons
        bEnableStartupItem.Enabled := False;
        bDisableStartupItem.Enabled := False;
        bDeleteStartupItem.Enabled := False;
        bExportStartupItem.Enabled := False;
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

{ TMain.bDeleteContextItemClick

  Deletes currently selected context menu item. }

procedure TMain.bDeleteContextItemClick(Sender: TObject);
var
  Answer: Integer;

begin
  try
    // Nothing selected?
    if (not Assigned(lwContext.ItemFocused) or not Assigned(FContext.Selected)) then
      raise EInvalidItem.Create('No item selected!');

    // Confirm deletion of item
    if (FLang.ShowMessage(FLang.Format([85], [lwContext.ItemFocused.SubItems[0]]),
      FLang.GetString([49, 50]), mtCustom) = IDYES) then
    begin
      // Ask user to export item
      Answer := FLang.ShowMessage(FLang.GetString(52), mtConfirmation);

      // Abort if user clicks cancel!
      if (((Answer = IDYES) and ShowRegistryExportDialog()) or (Answer = IDNO)) then
        // Successfully deleted item physically?
        if FContext.DeleteItem() then
        begin
          // Delete item from TListView
          lwContext.DeleteSelected();
          lwContext.ItemFocused := nil;

          // Disable VCL buttons
          bEnableContextItem.Enabled := False;
          bDisableContextItem.Enabled := False;
          bDeleteContextItem.Enabled := False;
          bExportContextItem.Enabled := False;
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

{ TMain.bDisableStartupItemClick

  Disables currently selected startup item. }

procedure TMain.bDisableStartupItemClick(Sender: TObject);
begin
  try
    // Nothing selected?
    if not Assigned(lwStartup.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    // Successfully deactivated item?
    if FStartup.DisableItem() then
    begin
      // Change item visual status
      lwStartup.ItemFocused.Caption := FStartup.Selected.GetStatus(FLang);

      // Change button states
      bDisableStartupItem.Enabled := False;
      bEnableStartupItem.Enabled := True;
      pmChangeStatus.Caption := bEnableStartupItem.Caption;

      // Show deactivation timestamp?
      if mmDate.Checked then
        lwStartup.ItemFocused.SubItems[3] := FStartup.Item.Time;

      // Update TListView
      lwStartupSelectItem(Self, lwStartup.ItemFocused, True);
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

{ TMain.bDisableContextItemClick

  Disables currently selected context menu item. }

procedure TMain.bDisableContextItemClick(Sender: TObject);
begin
  try
    // Nothing selected?
    if not Assigned(lwContext.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    // Successfully deactivated item?
    if FContext.DisableItem() then
    begin
      // Change item visual status
      lwContext.ItemFocused.Caption := FContext.Selected.GetStatus(FLang);

      // Change button states
      bDisableStartupItem.Enabled := False;
      bEnableContextItem.Enabled := True;
      pmChangeStatus.Caption := bEnableContextItem.Caption;

      // Update TListView
      lwContextSelectItem(Self, lwContext.ItemFocused, True);
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

{ TMain.bDisableServiceItemClick

  Disables currently selected service item. }

procedure TMain.bDisableServiceItemClick(Sender: TObject);
begin
  try
    // Nothing selected?
    if not Assigned(lwService.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    // Successfully deactivated item?
    if FService.DisableItem() then
    begin
      // Change item visual status
      lwService.ItemFocused.Caption := FService.Selected.GetStatus(FLang);

      // Change button states
      bDisableServiceItem.Enabled := False;
      bEnableServiceItem.Enabled := True;
      pmChangeStatus.Caption := bEnableServiceItem.Caption;

      // Show deactivation timestamp?
      if mmDate.Checked then
        lwService.ItemFocused.SubItems[3] := FService.Item.Time;

      // Update TListView
      lwServiceSelectItem(Self, lwService.ItemFocused, True);
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

{ TMain.bEnableStartupItemClick

  Enables currently selected startup item. }

procedure TMain.bEnableStartupItemClick(Sender: TObject);
begin
  try
    // Nothing selected?
    if not Assigned(lwStartup.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    // Successfully activated item?
    if FStartup.EnableItem() then
    begin
      // Change item visual status
      lwStartup.ItemFocused.Caption := FStartup.Selected.GetStatus(FLang);

      // Change button states
      bEnableStartupItem.Enabled := False;
      bDisableStartupItem.Enabled := True;
      bExportStartupItem.Enabled := not FStartup.BackupExists;
      pmExport.Enabled := bExportStartupItem.Enabled;
      pmChangeStatus.Caption := bDisableStartupItem.Caption;

      // Delete deactivation timestamp if necassary
      if mmDate.Checked then
        lwStartup.ItemFocused.SubItems[3] := '';

      // Update TListView
      lwStartupSelectItem(Self, lwStartup.ItemFocused, True);

      // Warn if file does not exist
      if not FStartup.Selected.FileExists() then
        raise EWarning.Create(FLang.GetString([45, NEW_LINE, 46]));
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([93, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([93, 18]), E.Message);
  end;  //of try
end;

{ TMain.bEnableContextItemClick

  Enables currently selected context menu item. }

procedure TMain.bEnableContextItemClick(Sender: TObject);
begin
  try
    // Nothing selected?
    if not Assigned(lwContext.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    // Successfully activated item?
    if FContext.EnableItem() then
    begin
      // Change item visual status
      lwContext.ItemFocused.Caption := FContext.Selected.GetStatus(FLang);

      // Change button states
      bEnableContextItem.Enabled := False;
      bDisableContextItem.Enabled := True;
      pmChangeStatus.Caption := bDisableStartupItem.Caption;

      // Update TListView
      lwContextSelectItem(Self, lwContext.ItemFocused, True);

      // Warn if file does not exist
      if (not (FContext.Selected is TShellNewItem) and not FContext.Selected.FileExists()) then
        raise EWarning.Create(FLang.GetString([45, NEW_LINE, 46]));
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([93, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([93, 18]), E.Message);
  end;  //of try
end;

{ TMain.bEnableServiceItemClick

  Enables currently selected service item. }

procedure TMain.bEnableServiceItemClick(Sender: TObject);
begin
  try
    // Nothing selected?
    if not Assigned(lwService.ItemFocused) then
      raise EInvalidItem.Create('No item selected!');

    // Successfully activated item?
    if FService.EnableItem() then
    begin
      // Change item visual status
      lwService.ItemFocused.Caption := FService.Selected.GetStatus(FLang);

      // Change button states
      bEnableServiceItem.Enabled := False;
      bDisableServiceItem.Enabled := True;
      pmChangeStatus.Caption := bDisableServiceItem.Caption;

      // Delete deactivation timestamp if necassary
      if mmDate.Checked then
        lwService.ItemFocused.SubItems[3] := '';

      // Update TListView
      lwServiceSelectItem(Self, lwService.ItemFocused, True);

      // Warn if file does not exist
      if not FService.Selected.FileExists() then
        raise EWarning.Create(FLang.GetString([45, NEW_LINE, 46]));
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.ShowMessage(FLang.GetString([93, 18]), FLang.GetString(53), mtWarning);

    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString([93, 18]), E.Message);
  end;  //of try
end;

{ TMain.bExportStartupItemClick

  Calls the export method of current selected deactivated startup item. }

procedure TMain.bExportStartupItemClick(Sender: TObject);
begin
  CreateStartupUserBackup();
end;

{ TMain.bExportContextItemClick

  Calls the export method of current selected context menu item. }

procedure TMain.bExportContextItemClick(Sender: TObject);
begin
  // Nothing selected?
  if not Assigned(FContext.Selected) then
  begin
    FLang.ShowMessage(FLang.GetString([95, 18]), FLang.GetString(53), mtWarning);
    Exit;
  end;  //of begin

  ShowRegistryExportDialog();
end;

{ TMain.bExportServiceItemClick

  Calls the export method of current selected service item. }

procedure TMain.bExportServiceItemClick(Sender: TObject);
begin
  // Nothing selected?
  if not Assigned(FService.Selected) then
  begin
    FLang.ShowMessage(FLang.GetString([95, 18]), FLang.GetString(53), mtWarning);
    Exit;
  end;  //of begin

  ShowRegistryExportDialog();
end;

{ TMain.eContextSearchChange

  Event method that is called when user changes the search string. }

procedure TMain.eContextSearchChange(Sender: TObject);
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

  case PageControl.ActivePageIndex of
    1: LoadContextMenuItems(False);
    2: LoadServiceItems(False);
  end;  //of case
end;

{ TMain.eContextSearchRightButtonClick

  Event method that is called when user clicked on clear. }

procedure TMain.eContextSearchRightButtonClick(Sender: TObject);
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
      FLang.ShowMessage(FLang.GetString(53), mtWarning);
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
      FLang.ShowMessage(FLang.GetString(53), mtWarning);
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

    // Enable "edit path" only if file path is present
    pmEdit.Enabled := (FService.Selected.FileName <> '');

    // Show popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
    // Nothing selected: Hide popup menu!
    PopupMenu.AutoPopup := False;
end;

{ TMain.lwStartupColumnClick

  Event method that is called when user clicks on TListView column. }

procedure TMain.lwStartupColumnClick(Sender: TObject; Column: TListColumn);
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
  FColumnToSort := Column.Index;
  List.AlphaSort;
end;

{ TMain.lwStartupCompare

  Sorts a TListView column alphabetically. }

procedure TMain.lwStartupCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  // Status column?
  if (FColumnToSort = 0) then
    case (Sender as TListView).Columns[FColumnToSort].Tag of
      0: Compare := CompareText(Item1.Caption, Item2.Caption);
      1: Compare := CompareText(Item2.Caption, Item1.Caption);
    end  //of case
  else
  begin
    Data := FColumnToSort - 1;

    if (Data < 3) then
      case (Sender as TListView).Columns[FColumnToSort].Tag of
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

{ TMain.lwStartupKeyPress

  Event method that is called when user pushes a button inside TListView. }

procedure TMain.lwStartupKeyPress(Sender: TObject; var Key: Char);
var
  i, StartIndex: Integer;
  SelectedList: TListView;

begin
  StartIndex := 0;
  SelectedList := (Sender as TListView);

  if not Assigned(SelectedList.ItemFocused) then
    Exit;

  // Current selected item already starts with key?
  if AnsiStartsText(Key, SelectedList.ItemFocused.SubItems[0]) then
    StartIndex := SelectedList.ItemFocused.Index + 1;

  // Find next item whose first char starts with key
  for i := StartIndex to SelectedList.Items.Count - 1 do
    if AnsiStartsText(Key, SelectedList.Items[i].SubItems[0]) then
    begin
      SelectedList.ItemIndex := i;
      SelectedList.ItemFocused := SelectedList.Items[i];
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
      FLang.ShowMessage(FLang.GetString(53), mtWarning);
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

    // Show popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
    // Nothing selected: Hide popup menu!
    PopupMenu.AutoPopup := False;
end;

{ TMain.pmChangeStatusClick

  Popup menu entry to deactivate an item. }

procedure TMain.pmChangeStatusClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: lwStartupDblClick(Sender);
    1: lwContextDblClick(Sender);
    2: lwServiceDblClick(Sender);
  end;  //of case
end;

{ TMain.pmDeleteClick

  Popup menu entry to delete an item. }

procedure TMain.pmDeleteClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: bDeleteStartupItem.Click;
    1: bDeleteContextItem.Click;
    2: bDeleteServiceItem.Click;
  end;  //of case
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
      FLang.ShowMessage(StripHotkey(pmOpenExplorer.Caption) + FLang.GetString(18),
        FLang.GetString(53), mtWarning);
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

    if (Item is TRootRegItem) then
      (Item as TRootRegItem).OpenInRegEdit();

  except
    on E: EInvalidItem do
      FLang.ShowMessage(StripHotkey(pmOpenRegedit.Caption) + FLang.GetString(18),
        FLang.GetString(53), mtWarning);
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
      FLang.ShowMessage(StripHotkey(pmCopyLocation.Caption) + FLang.GetString(18),
        FLang.GetString(53), mtWarning);
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

    // Update file path in TListView
    case PageControl.ActivePageIndex of
      0: begin
           lwStartup.ItemFocused.SubItems[1] := EnteredPath;

           // Update icon
           Icon := TIcon.Create;

           try
             Icon.Handle := FStartup.Selected.Icon;
             lwStartup.ItemFocused.ImageIndex := IconList.AddIcon(Icon);

           finally
             Icon.Free;
           end;  //of try
         end;

      2: lwService.ItemFocused.SubItems[1] := EnteredPath;
    end;  //of case
    
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

  MainMenu entry to add a application to autostart. }

procedure TMain.mmAddClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Name, Args, Location: string;
  List: TStringList;

begin
  OpenDialog := TOpenDialog.Create(Self);

  // Set TOpenDialog options
  with OpenDialog do
  begin
    Title := StripHotKey(mmAdd.Caption);
    InitialDir := '%ProgramFiles%';

    // Filter .exe and .bat files
    Filter := FLang.GetString(108);
  end;  //of with

  try
    try
      // User clicked "open"?
      if not OpenDialog.Execute then
        Exit;

      // Set default name
      Name := ChangeFileExt(ExtractFileName(OpenDialog.FileName), '');

      // User can edit the name
      if not InputQuery(OpenDialog.Title, FLang.GetString(97), Name) then
        Exit;

      // Name must not be empty!
      if (Name = '') then
        // TODO: Show abort message
        Exit;

      // Append optional parameters
      if not InputQuery(OpenDialog.Title, FLang.GetString(98), Args) then
        Exit;

      // Add startup item?
      case PageControl.ActivePageIndex of
        0: begin
             // Startup item already exists?
             if not FStartup.Add(OpenDialog.FileName, Args, Name) then
               raise EWarning.Create(FLang.Format(110, [OpenDialog.FileName]));

             // Update TListView
             LoadStartupItems(False);
           end;

        1: begin
             List := TStringList.Create;

             try
               // Init location ComboBox
               List.CommaText := CM_LOCATIONS_DEFAULT;

               // Show dialog for location selection
               if not InputCombo(Self, FLang.GetString(105), FLang.GetString(90) +':',
                 List, Location, False) then
                 Exit;

               // Contextmenu item already exists?
               if not FContext.Add(OpenDialog.FileName, Args, Location, Name) then
                 raise EWarning.Create(FLang.Format(41, [OpenDialog.FileName]));

               // Update TListView
               LoadContextMenuItems(False);

             finally
               List.Free;
             end;  //of try
           end;  //of if

        2: begin
             // Service item already exists?
             if not FService.Add(OpenDialog.FileName, Args, Name) then
               raise EWarning.Create(FLang.Format(110, [OpenDialog.FileName]));

             // Update TListView
             LoadServiceItems(False);
           end;
      end;  //of case

    finally
      OpenDialog.Free;
    end;  //of try

  except
    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(StripHotKey(mmAdd.Caption) + FLang.GetString(18),
        E.Message, mtWarning);

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
  case PageControl.ActivePageIndex of
    0: bExportStartupItem.Click();
    1: bExportContextItem.Click();
    2: bExportServiceItem.Click();
  end;  //of case
end;

{ TMain.mmExportListClick

  MainMenu entry to export the complete autostart as .reg (backup) file. }

procedure TMain.mmExportListClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  SelectedList: TRootRegList;

begin
  SelectedList := (GetSelectedList() as TRootRegList);

  // Operation pending?
  if SelectedList.IsLocked() then
  begin
    FLang.ShowMessage(100, 101, mtWarning);
    Exit;
  end;  //of begin

  SaveDialog := TSaveDialog.Create(Self);

  try
    // Set TSaveDialog options
    with SaveDialog do
    begin
      Title := StripHotkey(mmExportList.Caption);
      InitialDir := '%HOMEPATH%';
      
      // Confirm overwrite
      Options := Options + [ofOverwritePrompt];

      // Filter .reg files only
      Filter := FLang.GetString(36);
      DefaultExt := '.reg';

      // Sets default file name
      FileName := PageControl.ActivePage.Caption + DefaultExt;
    end;  //of with

    // User clicked "save"?
    if SaveDialog.Execute then
    begin
      // Export list (threaded!)
      with TExportListThread.Create(SelectedList, SaveDialog.FileName,
        PageControl.ActivePageIndex) do
      begin
        OnStart := OnExportListStart;
        OnFinish := OnExportListEnd;
        Start;
      end;  //of with
    end;  //of begin

  finally
    SaveDialog.Free;
  end;  //of try
end;

{ TMain.mmImportClick

  MainMenu entry to import a startup backup file. }

procedure TMain.mmImportClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  BackupDir: string;

begin
  BackupDir := TLnkFile.GetBackupDir();
  OpenDialog := TOpenDialog.Create(Self);

  try
    // Create Backup directory if not exists
    if not DirectoryExists(BackupDir) then
      ForceDirectories(BackupDir);

    // Set TOpenDialog options
    with OpenDialog do
    begin
      Title := StripHotkey(mmImport.Caption);
      InitialDir := BackupDir;
      Filter := Format(FLang.GetString(109), [EXT_USER, EXT_USER, EXT_COMMON, EXT_COMMON]);
    end;  //of with

    try
      // User clicked "open"?
      if OpenDialog.Execute then
      begin
        // Item already exists?
        if not FStartup.ImportBackup(OpenDialog.FileName) then
          raise EWarning.Create(FLang.GetString(41));

        // Update TListView
        LoadStartupItems(False);
      end;  //of begin

    finally
      OpenDialog.Free;
    end;  //of try

  except
    on E: EListBlocked do
      FLang.ShowMessage(100, 101, mtWarning);

    on E: EWarning do
      FLang.ShowMessage(FLang.GetString(55), E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString(55), E.Message);
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
    begin
      // Add recycle bin context menu entry
      Reg.OpenKey(KEY_RECYCLEBIN +'\Clearas', True);
      Reg.WriteString('', FLang.GetString(107));
      Reg.CloseKey();
      Reg.OpenKey(KEY_RECYCLEBIN +'\Clearas\command', True);
      Reg.WriteString('', ParamStr(0));
      mmContext.Checked := True;
    end;  //of if

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ TMain.mmRefreshClick

  MainMenu entry to refresh the current shown TListView. }

procedure TMain.mmRefreshClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: LoadStartupItems();
    1: LoadContextMenuItems();
    2: LoadServiceItems();
  end;  //of case
end;

{ TMain.mmShowIconsClick

  MainMenu entry to show/hide program icons in TListView. }

procedure TMain.mmShowIconsClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: LoadStartupItems(False);
    1: LoadContextMenuItems(False);
    2: LoadServiceItems(False);
  end;  //of case
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
         if (FContext.Count = 0) then
           LoadContextMenuItems();
       end;

    2: begin
         mmAdd.Caption := FLang.GetString(57);
         mmImport.Visible := False;
         mmDate.Visible := True;
         lwServiceSelectItem(Sender, lwService.ItemFocused, True);
         ShowColumnDate(lwService, mmDate.Checked);

         // Load context menu entries dynamically
         if (FService.Count = 0) then
           LoadServiceItems();
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
