{ *********************************************************************** }
{                                                                         }
{ Clearas Main Unit                                                       }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit ClearasMain;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ComCtrls, StdCtrls, ExtCtrls,
  Dialogs, Menus, Graphics, ClearasAPI, ClearasInfo, LanguageFile, OSUtils,
  Updater, AddDialogs, ClipBrd, ImgList;

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
    N5: TMenuItem;
    mmDate: TMenuItem;
    mmImport: TMenuItem;
    mmDelBackup: TMenuItem;
    mmDefault: TMenuItem;
    N7: TMenuItem;
    mmOptimate: TMenuItem;
    N8: TMenuItem;
    N6: TMenuItem;
    mmLang: TMenuItem;
    mmGer: TMenuItem;
    mmEng: TMenuItem;
    PageControl: TPageControl;
    tsStartup: TTabSheet;
    tsContext: TTabSheet;
    lwStartup: TListView;
    lStartup: TLabel;
    bEnableStartupItem: TButton;
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
    pbLoad: TProgressBar;
    cbExpert: TCheckBox;
    lWindows2: TLabel;
    lWindows: TLabel;
    mmFra: TMenuItem;
    mmUpdate: TMenuItem;
    N9: TMenuItem;
    mmDownloadCert: TMenuItem;
    N10: TMenuItem;
    pmEdit: TMenuItem;
    mmReport: TMenuItem;
    mmRunOnce: TMenuItem;
    pmOpenRegedit: TMenuItem;
    pmOpenExplorer: TMenuItem;
    mmShowIcons: TMenuItem;
    IconList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bCloseStartupClick(Sender: TObject);
    procedure bDeleteStartupItemClick(Sender: TObject);
    procedure bDeleteContextItemClick(Sender: TObject);
    procedure bEnableStartupItemClick(Sender: TObject);
    procedure bEnableContextItemClick(Sender: TObject);
    procedure bDisableStartupItemClick(Sender: TObject);
    procedure bDisableContextItemClick(Sender: TObject);
    procedure bExportStartupItemClick(Sender: TObject);
    procedure bExportContextItemClick(Sender: TObject);
    procedure cbExpertClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lwStartupSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lwContextSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lwContextDblClick(Sender: TObject);
    procedure lwStartupDblClick(Sender: TObject);
    procedure lwStartupColumnClick(Sender: TObject; Column: TListColumn);
    procedure lwStartupCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure mmAddClick(Sender: TObject);
    procedure mmContextClick(Sender: TObject);
    procedure mmDateClick(Sender: TObject);
    procedure mmDelBackupClick(Sender: TObject);
    procedure mmExportListClick(Sender: TObject);
    procedure mmExportClick(Sender: TObject);
    procedure mmImportClick(Sender: TObject);
    procedure mmRefreshClick(Sender: TObject);
    procedure mmShowIconsClick(Sender: TObject);
    procedure mmGerClick(Sender: TObject);
    procedure mmEngClick(Sender: TObject);
    procedure mmFraClick(Sender: TObject);
    procedure mmDefaultClick(Sender: TObject);
    procedure mmOptimateClick(Sender: TObject);
    procedure mmInfoClick(Sender: TObject);
    procedure mmUpdateClick(Sender: TObject);
    procedure mmDownloadCertClick(Sender: TObject);
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
  private
    FColumnToSort: Word;
    Startup: TStartupList;
    Context: TContextList;
    FLang: TLanguageFile;
    FUpdateCheck: TUpdateCheck;
    procedure AfterUpdate(Sender: TObject; ADownloadedFileName: string);
    procedure BeforeUpdate(Sender: TObject; const ANewBuild: Cardinal);
    function CreateStartupUserBackup(): Boolean;
    procedure OnContextSearchProgress(Sender: TObject; AWorkCount: Cardinal);
    procedure OnContextSearchStart(Sender: TObject; AWorkCountMax: Cardinal);
    procedure OnContextSearchEnd(Sender: TObject);
    procedure OnStartupSearchStart(Sender: TObject; AWorkCountMax: Cardinal);
    procedure OnStartupSearchEnd(Sender: TObject);
    procedure RefreshContextCounter();
    procedure RefreshStartupCounter();
    procedure SetLanguage(Sender: TObject);
    function ShowRegistryExportDialog(): Boolean;
    procedure LoadContextMenuEntries(ATotalRefresh: Boolean = True);
    procedure LoadStartupEntries(ATotalRefresh: Boolean = True);
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
  // German language default
  FLang := TLanguageFile.Create(100, Application);
  FLang.AddListener(Self);
  SetLanguage(Self);

  // Init update notificator
  FUpdateCheck := TUpdateCheck.Create(Self, 'Clearas', FLang);

  // Check for update on startup
  FUpdateCheck.CheckForUpdate(False);

  // Init Clearas instances
  Startup := TStartupList.Create;

  // Link search events
  with Startup do
  begin
    Startup.OnSearchStart := OnStartupSearchStart;
    Startup.OnSearchFinish := OnStartupSearchEnd;
  end;  //of with

  Context := TContextList.Create;

  // Link search events
  with Context do
  begin
    Context.OnSearchStart := OnContextSearchStart;
    Context.OnSearching := OnContextSearchProgress;
    Context.OnSearchFinish := OnContextSearchEnd;
  end;  //of with

  // Set title
  Caption := Application.Title + TOSUtils.GetArchitecture();
end;

{ TMain.FormDestroy

  VCL event that is called when form is being destroyed. }

procedure TMain.FormDestroy(Sender: TObject);
begin
  Context.Free;
  Startup.Free;
  FUpdateCheck.Free;
  FLang.Free;
end;

{ TMain.FormShow

  VCL event that is called when form is shown. }

procedure TMain.FormShow(Sender: TObject);
var
  windows: string;
  newWindows: Boolean;

begin
  // Get version of Windows including service pack
  windows := TOSUtils.GetWinVersion(True);
  newWindows := TOSUtils.WindowsVistaOrLater();
  lWindows.Caption := lWindows.Caption +' '+ windows;
  lWindows2.Caption := lWindows.Caption;

  // Check for incompatibility
  if not (newWindows or (windows <> '')) then
  begin
    Flang.MessageBox(FLang.Format([64, 65], [windows]), mtError);
    mmExportList.Enabled := False;
    mmRefresh.Enabled := False;
    mmContext.Enabled := False;
    mmRunOnce.Enabled := False;
    mmDate.Enabled := False;
    lwStartup.Enabled := False;
    lwContext.Enabled := False;
    cbExpert.Enabled := False;
    Exit;
  end;  //of if

  // Show "date of deactivation" only on Vista and later
  mmDate.Enabled := newWindows;

  // Update Clearas recycle bin context menu entry
  mmContext.Checked := TRegUtils.UpdateContextPath(FLang);

  // Load autostart
  LoadStartupEntries();
end;

{ private TMain.AfterUpdate

  Event method that is called by TUpdate when download is finished. }

procedure TMain.AfterUpdate(Sender: TObject; ADownloadedFileName: string);
begin
  if (ExtractFileExt(ADownloadedFileName) <> '.reg') then
  begin
    // Caption "Search for update"
    mmUpdate.Caption := FLang.GetString(15);
    mmUpdate.Enabled := False;
  end  //of begin
  else
    mmDownloadCert.Enabled := False;
end;

{ private TMain.BeforeUpdate

  Event that is called by TUpdateCheck when TUpdateCheckThread finds an update. }

procedure TMain.BeforeUpdate(Sender: TObject; const ANewBuild: Cardinal);
var
  Updater: TUpdate;

begin
  // Ask user to permit download
  if (FLang.MessageBox([21, NEW_LINE, 22], [ANewBuild], mtQuestion, True) = IDYES) then
  begin
    // init TUpdate instance
    Updater := TUpdate.Create(Self, FLang);

    try
      with Updater do
      begin
        Title := FLang.GetString(24);
        Download('clearas.exe', 'Clearas.exe');
      end;  //of begin

    finally
      Updater.Free;
    end;  //of try
  end  //of begin
  else
    mmUpdate.Caption := FLang.GetString(24);
end;

{ private TMain.CreateStartupUserBackup

  Creates a special .lnk backup for currently selected activated startup
  user entry. }

function TMain.CreateStartupUserBackup(): Boolean;
begin
  Result := False;

  try
    // Special .lnk file backup only for activated startup user entries!
    if (Startup.Selected.Enabled and (Startup.Selected is TStartupUserItem)) then
    begin
      Startup.Selected.ExportItem('');
      FLang.MessageBox(Flang.Format(42, [(Startup.Selected as TStartupUserItem).LnkFile.BackupLnk]));
      bExportStartupItem.Enabled := False;
      pmExport.Enabled := False;
      Result := False;
    end  //of begin
    else
      // Default .reg file export
      Result := ShowRegistryExportDialog();

  except
    on E: EAccessViolation do
      FLang.MessageBox([95, 18, NEW_LINE, 53], mtWarning);

    on E: EStartupException do
      Flang.MessageBox(43, mtError);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([95, 18, NEW_LINE]) + E.Message, mtError);
  end;  //of try
end;

{ private TMain.OnContextSearchProgress

  Event that is called when search is in progress. }

procedure TMain.OnContextSearchProgress(Sender: TObject; AWorkCount: Cardinal);
begin
  pbLoad.Position := AWorkCount;
end;

{ private TMain.OnContextSearchStart

  Event that is called when search starts. }

procedure TMain.OnContextSearchStart(Sender: TObject; AWorkCountMax: Cardinal);
begin
  mmView.Enabled := False;
  mmFile.Enabled := False;
  pbLoad.Visible := True;
  pbLoad.Max := AWorkCountMax;
  lwContext.Cursor := crHourGlass;
end;

{ private TMain.OnContextSearchEnd

  Event that is called when search ends. }

procedure TMain.OnContextSearchEnd(Sender: TObject);
var
  i: Integer;

begin
  // Print all information about context menu entires
  for i := 0 to Context.Count - 1 do
    with lwContext.Items.Add do
    begin
      Caption := Context[i].GetStatus(FLang);

      // Show name or caption of item?
      if (Context[i].Caption <> '') then
        SubItems.Append(Context[i].Caption)
      else
        SubItems.Append(Context[i].Name);

      SubItems.Append(Context[i].Location);
      SubItems.Append(Context[i].TypeOf);
    end; //of with

  // Refresh counter label
  RefreshContextCounter();
  pbLoad.Visible := False;
  pbLoad.Position := 0;
  lwContext.Cursor := crDefault;
  mmView.Enabled := True;
  mmFile.Enabled := True;
end;

{ private TMain.OnStartupSearchStart

  Event that is called when search starts. }

procedure TMain.OnStartupSearchStart(Sender: TObject; AWorkCountMax: Cardinal);
begin
  mmView.Enabled := False;
  mmFile.Enabled := False;
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

  if mmShowIcons.Checked then
    lwStartup.SmallImages := IconList
  else
    lwStartup.SmallImages := nil;

  Icon := TIcon.Create;

  try
    // Print all information about startup entires
    for i := 0 to Startup.Count - 1 do
      with lwStartup.Items.Add do
      begin
        Caption := Startup[i].GetStatus(FLang);
        SubItems.Append(Startup[i].Name);
        SubItems.Append(Startup[i].FilePath);
        SubItems.Append(Startup[i].TypeOf);

        // Show deactivation timestamp?
        if mmDate.Checked then
          SubItems.Append(Startup[i].Time);

        // Show icon of program?
        if mmShowIcons.Checked then
        begin
          // Get icon of program
          Icon.Handle := Startup[i].Icon;
          ImageIndex := IconList.AddIcon(Icon);
        end;  //of begin
      end;  //of with

  finally
    Icon.Free;
  end;  //of try

  // Refresh counter label
  RefreshStartupCounter();
  lwStartup.Cursor := crDefault;
  mmView.Enabled := True;
  mmFile.Enabled := True;
end;

{ private TMain.RefreshContextCounter

  Refreshs the counter label of context menu items. }

procedure TMain.RefreshContextCounter();
begin
  lwContext.Columns[1].Caption := FLang.Format(87, [Context.ActCount, Context.Count]);
end;

{ private TMain.RefreshStartupCounter

  Refreshs the counter label of startup items. }

procedure TMain.RefreshStartupCounter();
begin
  lwStartup.Columns[1].Caption := FLang.Format(88, [Startup.ActCount, Startup.Count]);
end;

{ private TMain.SetLanguage

  Updates all component captions with new language text. }

procedure TMain.SetLanguage(Sender: TObject);
var
  i: Integer;

begin
  with FLang do
  begin
    // File menu labels
    mmFile.Caption := GetString(68);

    if (PageControl.ActivePage = tsStartup) then
      mmAdd.Caption := GetString(69)
    else
      mmAdd.Caption := GetString(34);

    mmImport.Caption := GetString(70);
    mmExport.Caption := GetString(71);
    mmExportlist.Caption := GetString(72);
    mmClose.Caption := GetString(73);

    // Edit menu labels
    mmEdit.Caption := GetString(74);
    mmContext.Caption := GetString(75);
    mmDelBackup.Caption := GetString(76);

    // View menu labels
    mmView.Caption := GetString(20);
    mmRefresh.Caption := GetString(77);
    mmDefault.Caption := GetString(78);
    mmOptimate.Caption := GetString(79);
    mmDate.Caption := GetString(80);
    mmRunOnce.Caption := GetString(81);
    mmShowIcons.Caption := GetString(47);
    mmLang.Caption := GetString(25);

    // Help menu labels
    mmHelp.Caption := GetString(14);
    mmUpdate.Caption := GetString(15);
    mmDownloadCert.Caption := GetString(16);
    mmReport.Caption := GetString(26);
    mmInfo.Caption := GetString(17);

    // Startup tab TButton labels
    tsContext.Caption := GetString(84);
    bEnableStartupItem.Caption := GetString(93);
    bDisableStartupItem.Caption := GetString(94);
    bExportStartupItem.Caption := GetString(95);
    bDeleteStartupItem.Caption := GetString(96);
    bCloseStartup.Caption := mmClose.Caption;

    // Startup tab TListView labels
    lStartup.Caption := GetString(82);
    lwStartup.Columns[0].Caption := GetString(91);
    lwStartup.Columns[2].Caption := GetString(68);
    lwStartup.Columns[3].Caption := GetString(92);
    lCopy1.Hint := GetString(29);

    // Context menu tab TButton labels
    bEnableContextItem.Caption := bEnableStartupItem.Caption;
    bDisableContextItem.Caption := bDisableStartupItem.Caption;
    bExportContextItem.Caption := bExportStartupItem.Caption;
    bDeleteContextItem.Caption := bDeleteStartupItem.Caption;
    bCloseContext.Caption := bCloseStartup.Caption;
    cbExpert.Caption := GetString(89);
    tsStartup.Caption := GetString(83);

    // Context menu tab TListView labels
    lContext.Caption := GetString(86);
    lwContext.Columns[0].Caption := lwStartup.Columns[0].Caption;
    lwContext.Columns[2].Caption := GetString(90);
    lwContext.Columns[3].Caption := lwStartup.Columns[3].Caption;
    lCopy2.Hint := lCopy1.Hint;

    // Popup menu labels
    pmChangeStatus.Caption := bDisableStartupItem.Caption;
    pmOpenRegedit.Caption := GetString(66);
    pmOpenExplorer.Caption := GetString(51);
    pmEdit.Caption := GetString(33);
    pmExport.Caption := mmExport.Caption;
    pmDelete.Caption := bDeleteStartupItem.Caption;
    pmCopyLocation.Caption := GetString(35);
  end;  //of with

  // Update list labels
  if Assigned(Startup) and Assigned(Context) then
  begin
    for i := 0 to Startup.Count - 1 do
      lwStartup.Items[i].Caption := Startup[i].GetStatus(FLang);

    for i := 0 to Context.Count - 1 do
      lwContext.Items[i].Caption := Context[i].GetStatus(FLang);
  end;  //of begin
end;

{ private TMain.LoadContextMenuEntries

  Loads context menu entries and brings them into a TListView. }

procedure TMain.LoadContextMenuEntries(ATotalRefresh: Boolean = True);
begin
  // Clear all visual data
  lwContext.Clear;

  // Make a total refresh or just use cached items
  if ATotalRefresh then
  begin
    // Clear selected item
    Context.Selected := nil;

    // Clear data in backend
    Context.Clear;

    // Disable VCL buttons
    bDisableContextItem.Enabled := False;
    bEnableContextItem.Enabled := False;
    bDeleteContextItem.Enabled := False;
    bExportContextItem.Enabled := False;

    // Use expert search mode?
    if cbExpert.Checked then
      // Start the expert search in a thread
      Context.LoadContextmenu()
    else
      // Use default search mode
      Context.LoadContextMenus();
  end  //of begin
  else
    OnContextSearchEnd(Self);
end;

{ private TMain.LoadStartupEntries

  Loads startup entries and brings them into a TListView. }

procedure TMain.LoadStartupEntries(ATotalRefresh: Boolean = True);
begin
  // Clear all visual data
  lwStartup.Clear;

  // Make a total refresh or just use cached items
  if ATotalRefresh then
  begin
    // Clear selected item
    Startup.Selected := nil;

    // Clear data in backend
    Startup.Clear;

    // Disable VCL buttons
    bDisableStartupItem.Enabled := False;
    bEnableStartupItem.Enabled := False;
    bDeleteStartupItem.Enabled := False;
    bExportStartupItem.Enabled := False;

    // Load autostart with or without special RunOnce entries
    Startup.LoadAutostart(mmRunOnce.Checked);
  end  //of begin
  else
    OnStartupSearchEnd(Self);
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

      // Confirm overwrite
      Options := Options + [ofOverwritePrompt];

      // Filter .reg files only
      Filter := FLang.GetString(36);
      DefaultExt := '.reg';

      // Set a default file name
      if (PageControl.ActivePage = tsStartup) then
        FileName := Startup.Selected.Name + DefaultExt
      else
        FileName := Context.Selected.Name +'_'+ Context.Selected.Location + DefaultExt;
    end;  //of with

    try
      // User clicked "save"?
      if SaveDialog.Execute then
      begin
        if (PageControl.ActivePage = tsStartup) then
          Startup.ExportItem(SaveDialog.FileName)
        else
          Context.ExportItem(SaveDialog.FileName);

        Result := True;
      end;  //of begin

    finally
      SaveDialog.Free;
    end;  //of try

  except
    on E: EAccessViolation do
      FLang.MessageBox([95, 18, NEW_LINE, 53], mtWarning);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([95, 18, NEW_LINE]) + E.Message, mtError);
  end;  //of try
end;

{ TMain.bExportContextClick

  Calls the export method of current selected deactivated startup item. }

procedure TMain.bExportStartupItemClick(Sender: TObject);
begin
  CreateStartupUserBackup();
end;

{ TMain.bExportContextClick

  Calls the export method of current selected context menu item. }

procedure TMain.bExportContextItemClick(Sender: TObject);
begin
  // Nothing selected?
  if not Assigned(Context.Selected) then
  begin
    FLang.MessageBox([95, 18, NEW_LINE, 53], mtWarning);
    Exit;
  end;  //of begin

  ShowRegistryExportDialog();
end;

{ TMain.bDeleteClick

  Deletes currently selected startup item. }

procedure TMain.bDeleteStartupItemClick(Sender: TObject);
var
  DelBackup, BackupExists: Boolean;
  Answer: Integer;

begin
  DelBackup := True;

  try
    // Confirm deletion of item
    if (FLang.MessageBox(FLang.Format([48, NEW_LINE, 49, 50],
      [Startup.Selected.Name]), mtConfirm) = IDYES) then
    begin
      // Save the DeleteBackup flag
      DelBackup := Startup.DeleteBackup;
      BackupExists := Startup.BackupExists();

      // Skip export dialog for enabled startup user item with exising backup
      if ((Startup.Selected is TStartupUserItem) and Startup.Selected.Enabled
        and BackupExists) then
        Answer := IDCANCEL
      else
        Answer := FLang.MessageBox(52, mtQuestion);

      // Export item and only continue if this has succeeded
      if (Answer = IDYES) then
        if CreateStartupUserBackup() then
          Startup.DeleteBackup := False
        else
          Exit;

      // Ask user to delete old existing backup
      if ((Answer = IDCANCEL) or ((Startup.Selected is TStartupUserItem)
        and not Startup.Selected.Enabled and BackupExists)) then
        Startup.DeleteBackup := (FLang.MessageBox(44, mtQuestion) = IDYES);

      // Successfully deleted item physically?
      if Startup.DeleteItem() then
      begin
        // Delete item from TListView
        lwStartup.DeleteSelected();

        // Refresh counter label
        RefreshStartupCounter();

        // Change button states
        bEnableStartupItem.Enabled := False;
        bDisableStartupItem.Enabled := False;
        bDeleteStartupItem.Enabled := False;
        bExportStartupItem.Enabled := False;
        bCloseStartup.Default := True;
      end  //of begin
      else
        raise Exception.Create('Unknown error!');
    end;  //of begin

  except
    on E: EAccessViolation do
      FLang.MessageBox([96, 18, NEW_LINE, 53], mtWarning);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([96, 18, NEW_LINE]) + E.Message, mtError);
  end;  //of try

  // Restore the DeleteBackup flag
  if (Startup.DeleteBackup <> DelBackup) then
    Startup.DeleteBackup := DelBackup;
end;

{ TMain.bDeleteContextClick

  Deletes currently selected context menu item. }

procedure TMain.bDeleteContextItemClick(Sender: TObject);
var
  Answer: Integer;

begin
  try
    // Confirm deletion of item
    if (FLang.MessageBox(FLang.Format([85, NEW_LINE, 49, 50],
      [Context.Selected.Name]), mtConfirm) = IDYES) then
    begin
      // Ask user to export item
      Answer := FLang.MessageBox(52, mtQuestion);

      // Abort if user clicks cancel!
      if (((Answer = IDYES) and ShowRegistryExportDialog()) or (Answer = IDNO)) then
        // Successfully deleted item physically?
        if Context.DeleteItem() then
        begin
          // Delete item from TListView
          lwContext.DeleteSelected();

          // Refresh counter label
          RefreshContextCounter();

          // Change button states
          bEnableContextItem.Enabled := False;
          bDisableContextItem.Enabled := False;
          bDeleteContextItem.Enabled := False;
          bExportContextItem.Enabled := False;
          bCloseContext.Default := True;
        end  //of begin
        else
          raise Exception.Create('Unknown error!');
    end;  //of begin

  except
    on E: EAccessViolation do
      FLang.MessageBox([96, 18, NEW_LINE, 53], mtWarning);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([96, 18, NEW_LINE]) + E.Message, mtError);
  end;  //of try
end;

{ TMain.bActivateClick

  Activates currently selected startup item. }

procedure TMain.bEnableStartupItemClick(Sender: TObject);
begin
  try
    // Successfully activated item?
    if Startup.EnableItem() then
    begin
      // Change item visual status
      lwStartup.ItemFocused.Caption := Startup.Selected.GetStatus(FLang);

      // Change button states
      bEnableStartupItem.Enabled := False;
      bDisableStartupItem.Enabled := True;
      bDisableStartupItem.Default := True;
      bExportStartupItem.Enabled := not Startup.BackupExists;
      pmExport.Enabled := bExportStartupItem.Enabled;
      pmChangeStatus.Caption := bDisableStartupItem.Caption;

      // Delete deactivation timestamp if necassary
      if mmDate.Checked then
        lwStartup.ItemFocused.SubItems[3] := '';

      // Refresh counter label
      RefreshStartupCounter();

      // Update TListView
      lwStartupSelectItem(Self, lwStartup.ItemFocused, True);
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.MessageBox([93, 18, NEW_LINE, 53], mtWarning);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([93, 18, NEW_LINE]) + E.Message, mtError);
  end;  //of try
end;

{ TMain.bActContextClick

  Activates currently selected context menu item. }

procedure TMain.bEnableContextItemClick(Sender: TObject);
begin
  try
    // Successfully activated item?
    if Context.EnableItem() then
    begin
      // Change item visual status
      lwContext.ItemFocused.Caption := Context.Selected.GetStatus(FLang);

      // Change button states
      bEnableContextItem.Enabled := False;
      bDisableContextItem.Enabled := True;
      bDisableContextItem.Default := True;
      pmChangeStatus.Caption := bDisableStartupItem.Caption;

      // Refresh counter label
      RefreshContextCounter();

      // Update TListView
      lwContextSelectItem(Self, lwContext.ItemFocused, True);
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.MessageBox([93, 18, NEW_LINE, 53], mtWarning);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([93, 18, NEW_LINE]) + E.Message, mtError);
  end;  //of try
end;

{ TMain.bDeactivateClick

  Deactivates currently selected startup item. }

procedure TMain.bDisableStartupItemClick(Sender: TObject);
begin
  try
    // Successfully deactivated item?
    if Startup.DisableItem() then
    begin
      // Change item visual status
      lwStartup.ItemFocused.Caption := Startup.Selected.GetStatus(FLang);

      // Change button states
      bDisableStartupItem.Enabled := False;
      bEnableStartupItem.Enabled := True;
      bEnableStartupItem.Default := True;
      pmChangeStatus.Caption := bEnableStartupItem.Caption;

      // Show deactivation timestamp?
      if mmDate.Checked then
        lwStartup.ItemFocused.SubItems[3] := Startup.Selected.Time;

      // Refresh counter label
      RefreshStartupCounter();

      // Update TListView
      lwStartupSelectItem(Self, lwStartup.ItemFocused, True);
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.MessageBox([94, 18, NEW_LINE, 53], mtWarning);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([94, 18, NEW_LINE]) + E.Message, mtError);
  end;  //of try
end;

{ TMain.bDeactContextClick

  Deactivates currently selected context menu item. }

procedure TMain.bDisableContextItemClick(Sender: TObject);
begin
  try
    // Successfully deactivated item?
    if Context.DisableItem() then
    begin
      // Change item visual status
      lwContext.ItemFocused.Caption := Context.Selected.GetStatus(FLang);

      // Change button states
      bDisableStartupItem.Enabled := False;
      bEnableContextItem.Enabled := True;
      bEnableContextItem.Default := True;
      pmChangeStatus.Caption := bEnableContextItem.Caption;

      // Refresh counter label
      RefreshContextCounter();

      // Update TListView
      lwContextSelectItem(Self, lwContext.ItemFocused, True);
    end  //of begin
    else
      raise Exception.Create('Unknown error!');

  except
    on E: EInvalidItem do
      FLang.MessageBox([94, 18, NEW_LINE, 53], mtWarning);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([94, 18, NEW_LINE]) + E.Message, mtError);
  end;  //of try
end;

{ TMain.FormKeyDown

  Event method that is called when user presses a key. }

procedure TMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Bind the "delete" key to delete methods
  if (Key = VK_DELETE) then
    if (PageControl.ActivePage = tsStartup) then
      bDeleteStartupItem.Click
    else
      if (PageControl.ActivePage = tsContext) then
        bDeleteContextItem.Click;
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
    Index := Startup.IndexOf(Item.SubItems[0], (Item.Caption = FLang.GetString(31)));

    // Item not found?
    if (Index = -1) then
      Exit;

    // Load item into cache
    Startup.Selected := Startup.Items[Index];

    bCloseStartup.Default := False;

    // Check for multiple items with the same name
    if (IsDouble(Item.SubItems[0], Item.Index) <> -1) then
    begin
      bEnableStartupItem.Enabled := False;
      bDisableStartupItem.Enabled := False;
      pmChangeStatus.Enabled := False;
      bDeleteStartupItem.Default := True;
      pmEdit.Enabled := False;
    end  //of begin
    else
      begin
        bDeleteStartupItem.Default := False;

        bEnableStartupItem.Enabled := not Startup.Selected.Enabled;
        bEnableStartupItem.Default := bEnableStartupItem.Enabled;

        bDisableStartupItem.Enabled := not bEnableStartupItem.Enabled;
        bDisableStartupItem.Default := bDisableStartupItem.Enabled;

        // Change text of "change status" button
        if bDisableStartupItem.Enabled then
          pmChangeStatus.Caption := bDisableStartupItem.Caption
        else
          pmChangeStatus.Caption := bEnableStartupItem.Caption;

        pmChangeStatus.Enabled := True;
      end;  //of if

    // Selected item is enabled and startup user type?
    if ((Startup.Selected is TStartupUserItem) and Startup.Selected.Enabled) then
    begin
      // Disable "open in RegEdit" because item is on file system!
      pmOpenRegedit.Enabled := False;

      // Disable "export" if backup already exists
      bExportStartupItem.Enabled := not Startup.BackupExists();
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
    Index := Context.IndexOf(Item.SubItems[0], Item.SubItems[1]);

    // Item not found?
    if (Index = -1) then
      Exit;

    // Load item into cache
    Context.Selected := Context.Items[Index];

    // Change button states
    bCloseContext.Default := False;
    bDeleteContextItem.Default := False;

    bEnableContextItem.Enabled := not Context.Selected.Enabled;
    bEnableContextItem.Default := bEnableContextItem.Enabled;

    bDisableContextItem.Enabled := not bEnableContextItem.Enabled;
    bDisableContextItem.Default := bDisableContextItem.Enabled;

    // Change text of "change status" button
    if bDisableContextItem.Enabled then
      pmChangeStatus.Caption := bDisableContextItem.Caption
    else
      pmChangeStatus.Caption := bEnableContextItem.Caption;

    pmChangeStatus.Enabled := True;

    // Enable "edit path" only if file path is present
    pmEdit.Enabled := (Context.Selected.FilePath <> '');

    // Enable "open in Explorer" only if file path is present
    pmOpenExplorer.Enabled := pmEdit.Enabled;

    bDeleteContextItem.Enabled := True;
    pmDelete.Enabled := True;
    pmOpenRegedit.Enabled := True;
    bExportContextItem.Enabled := True;
    pmExport.Enabled := True;

    // Show popup menu
    PopupMenu.AutoPopup := True;
  end  //of begin
  else
    // Nothing selected: Hide popup menu!
    PopupMenu.AutoPopup := False;
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
        FLang.MessageBox(53, mtWarning);
end;

{ TMain.lwContextDblClick

  Event method that is called when user double clicks on TListView item. }

procedure TMain.lwContextDblClick(Sender: TObject);
begin
  if bEnableContextItem.Default then
    bEnableContextItem.Click
  else
    if bDisableContextItem.Default then
      bDisableContextItem.Click
    else
      if bDeleteContextItem.Default then
        bDeleteContextItem.Click
      else
        FLang.MessageBox(53, mtWarning);
end;

{ TMain.lwStartupColumnClick

  Event method that is called when user clicks on TListView column. }

procedure TMain.lwStartupColumnClick(Sender: TObject; Column: TListColumn);
begin
  FColumnToSort := Column.Index;
  (Sender as TCustomListView).AlphaSort;
end;

{ TMain.lwStartupCompare

  Sorts a TListView column alphabetically. }

procedure TMain.lwStartupCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  // Status column?
  if (FColumnToSort = 0) then
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else
    begin
      Data := FColumnToSort -1;

      if (Data < 3) then
        Compare := CompareText(Item1.SubItems[Data], Item2.SubItems[Data]);
    end;  //of if
end;

{ TMain.pmChangeStatusClick

  Popup menu entry to deactivate an item. }

procedure TMain.pmChangeStatusClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tsStartup) then
    lwStartupDblClick(Sender)
  else
    lwContextDblClick(Sender);
end;

{ TMain.pmDeleteClick

  Popup menu entry to delete an item. }

procedure TMain.pmDeleteClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tsStartup) then
    bDeleteStartupItem.Click
  else
    bDeleteContextItem.Click;
end;

{ TMain.pmOpenRegeditClick

  Opens the path of the current selected item in Explorer. }

procedure TMain.pmOpenExplorerClick(Sender: TObject);
begin
  try
    if (PageControl.ActivePage = tsStartup) then
      Startup.Selected.OpenInExplorer()
    else
      Context.Selected.OpenInExplorer();

  except
    on E: EAbort do
      FLang.MessageBox([45, NEW_LINE, 46], mtWarning);
  end;  //of try
end;

{ TMain.pmOpenRegeditClick

  Opens the current selected item in RegEdit. }

procedure TMain.pmOpenRegeditClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tsStartup) then
    Startup.Selected.OpenInRegEdit()
  else
    Context.Selected.OpenInRegEdit();
end;

{ TMain.pmPropertiesClick

  Popup menu entry to show some properties. }

procedure TMain.pmCopyLocationClick(Sender: TObject);
begin
  try
    if (PageControl.ActivePage = tsStartup) then
      Clipboard.AsText := Startup.Selected.GetFullKeyPath()
    else
      Clipboard.AsText := Context.Selected.GetFullKeyPath();

  except
    FLang.MessageBox(53, mtWarning);
  end;  //of begin
end;

{ TMain.pmEditClick

  Popup menu entry to edit the path of a program. }

procedure TMain.pmEditClick(Sender: TObject);
var
  Path, EnteredPath: string;
  SelectedList: TRootList;
  Icon: TIcon;

begin
  try
    if (PageControl.ActivePage = tsStartup) then
    begin
      SelectedList := Startup;
      Path := Startup.Selected.FilePath;
    end  //of begin
    else
      begin
        SelectedList := Context;
        Path := Context.Selected.FilePath;
      end;  //of if

    // Show input box for editing path
    EnteredPath := InputBox(FLang.GetString(33), FLang.GetString(54), Path);

    // Nothing entered or nothing changed
    if ((Trim(EnteredPath) = '') or (EnteredPath = Path)) then
      Exit;

    // Try to change the file path
    if not SelectedList.ChangeItemFilePath(EnteredPath) then
      raise Exception.Create('Could not change path!');

    // Update file path in TListView
    if (PageControl.ActivePage = tsStartup) then
    begin
      lwStartup.ItemFocused.SubItems[1] := EnteredPath;

      // Update icon
      if mmShowIcons.Checked then
      begin
        Icon := TIcon.Create;

        try
          Icon.Handle := Startup.Selected.Icon;
          lwStartup.ItemFocused.ImageIndex := IconList.AddIcon(Icon);

        finally
          Icon.Free;
        end;  //of try
      end;  //of begin
    end;  //of begin

  except
    on E: EAccessViolation do
      FLang.MessageBox(53, mtWarning);

    on E: Exception do
      FLang.MessageBox(FLang.GetString([33, 18, NEW_LINE]) + E.Message, mtError);
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

    // Filter .exe files
    Filter := FLang.GetString(38);
  end;  //of with

  try
    try
      // User clicked "open"?
      if OpenDialog.Execute then
      begin
        // Set default name
        Name := TRegUtils.DeleteExt(ExtractFileName(OpenDialog.FileName));

        // User can edit the name
        Name := InputBox(FLang.GetString(74), FLang.GetString(97), Name);

        // Name must not be empty!
        if (Name = '') then
          Exit;

        // optional parameters
        if not InputQuery(FLang.GetString(99), FLang.GetString(98), Args) then
          Exit;

        // Add startup item?
        if (PageControl.ActivePage = tsStartup) then
        begin
          // Startup item already exists?
          if not Startup.AddProgram(OpenDialog.FileName, Args, Name) then
            FLang.MessageBox(FLang.Format(40, [OpenDialog.FileName]), mtWarning)
          else
            // Update TListView
            LoadStartupEntries(False);
        end  //of begin
        else
          begin
            List := TStringList.Create;

            try
              // Init location ComboBox
              List.Add('Directory');
              List.Add('Folder');
              List.Add('*');     
              List.Add('Drive');

              // Show dialog for location selection
              if not InputCombo(FLang.GetString(34), FLang.GetString(90) +':',
                List, Location) then
                Exit;

              // Contextmenu item already exists?
              if not Context.AddEntry(OpenDialog.FileName, Args, Location, Name) then
                FLang.MessageBox(FLang.Format(41, [OpenDialog.FileName]), mtWarning)
              else
                // Update TListView
                LoadContextMenuEntries(False);

            finally
              List.Free;
            end;  //of try
          end;  //of if
      end;  //of begin

    finally
      OpenDialog.Free;
    end;  //of try

  except
    FLang.MessageBox(55, mtError);
  end;  //of try
end;

{ TMain.mmDateClick

  MainMenu entry to add or removes the deactivation timestamp column with
  an animation. }

procedure TMain.mmDateClick(Sender: TObject);
var
  endOf: integer;

begin
  if not (WindowState = wsMaximized) then
    lwStartup.Clear;

  // Timestamp already shown?
  if mmDate.Checked then
  begin
    mmDate.Checked := False;

    if (lwStartup.Columns.Count > 4) then
    begin
      Constraints.MinWidth := Constraints.MinWidth - lwStartup.Column[4].Width;
      lwStartup.Column[4].MinWidth := 0;
      endOf := Width - 120;

      // Animation to remove column smoothly
      repeat
        Width := Width - 1;

        if (WindowState = wsMaximized) then
          endOf := Width;

        if (lwStartup.Column[4].Width <= 120) then
          lwStartup.Column[4].Width := lwStartup.Column[4].Width - 1;

        Update;
        Sleep(0);
      until ((lwStartup.Column[4].Width <= 0) and (Width = endOf));

      // Delete column
      lwStartup.Columns.Delete(4);

      // Refresh counter label
      LoadStartupEntries(False);
    end;  //of if
  end  //of begin
  else
    begin
      mmDate.Checked := True;
      endOf := Width + 120;

      // Add column
      lwStartup.Columns.Add.Caption := FLang.GetString(80);

      if (WindowState = wsMaximized) then
        LoadStartupEntries(False);

      // Animation to add column smoothly
      repeat
        Width := Width + 1;

        if (WindowState = wsMaximized) then
          endOf := Width;

        if (lwStartup.Column[4].Width < 120) then
          lwStartup.Column[4].Width := lwStartup.Column[4].Width + 1;

        Update;
        Sleep(0);
      until ((lwStartup.Column[4].Width >= 120) and (Width = endOf));

      lwStartup.Column[4].MinWidth := lwStartup.Column[4].Width;
      lwStartup.Column[4].MaxWidth := lwStartup.Column[4].Width;
      Constraints.MinWidth := Constraints.MinWidth + lwStartup.Column[4].Width;

      if not (WindowState = wsMaximized) then
        LoadStartupEntries(False);
    end;  //of if
end;

{ TMain.mmExportClick

  MainMenu entry to export a single item as .reg file. }

procedure TMain.mmExportClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tsStartup) then
    bExportStartupItem.Click()
  else
    if (PageControl.ActivePage = tsContext) then
      bExportContextItem.Click()
    else
      FLang.MessageBox([95, 18, NEW_LINE, 53], mtWarning);
end;

{ TMain.mmExportListClick

  MainMenu entry to export the complete autostart as .reg file (for backup). }

procedure TMain.mmExportListClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;

begin
  SaveDialog := TSaveDialog.Create(Self);

  try
    // Set TSaveDialog options
    with SaveDialog do
    begin
      Title := StripHotkey(mmExportList.Caption);

      // Confirm overwrite
      Options := Options + [ofOverwritePrompt];

      // Filter .reg files only
      Filter := FLang.GetString(36);
      DefaultExt := '.reg';

      // Sets a default file name
      FileName := FLang.GetString(68) + DefaultExt;
    end;  //of with

    // User clicked "save"?
    if SaveDialog.Execute then
      if (PageControl.ActivePage = tsStartup) then
        Startup.ExportList(SaveDialog.FileName)
      else
        Context.ExportList(SaveDialog.FileName);

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
      Filter := Format(FLang.GetString(39), [EXT_USER, EXT_USER, EXT_COMMON, EXT_COMMON]);
    end;  //of with

    try
      // User clicked "open"?
      if OpenDialog.Execute then
        // Item already exists?
        if not Startup.ImportBackup(OpenDialog.FileName) then
          FLang.MessageBox(41, mtWarning)
        else
          // Update TListView
          LoadStartupEntries(False);

    finally
      OpenDialog.Free;
    end;  //of try

  except
    FLang.MessageBox(55, mtError);
  end;  //of try
end;

{ TMain.mmDelBackupClick

  MainMenu entry to set or resets the flag to delete backups automatically. }

procedure TMain.mmDelBackupClick(Sender: TObject);
begin
  Startup.DeleteBackup := mmDelBackup.Checked;
end;

{ TMain.mmContextClick

  MainMenu entry to add or removes "Clearas" in the recycle bin context menu. }

procedure TMain.mmContextClick(Sender: TObject);
begin
  TRegUtils.RegisterInContextMenu(not mmContext.Checked);
end;

{ TMain.mmRefreshClick

  MainMenu entry to refresh the current shown TListView. }

procedure TMain.mmRefreshClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tsStartup) then
    LoadStartupEntries()
  else
    LoadContextMenuEntries();
end;

{ TMain.mmShowIconsClick

  MainMenu entry to show/hide program icons in TListView. }

procedure TMain.mmShowIconsClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tsStartup) then
    LoadStartupEntries(False)
  else
    LoadContextMenuEntries(False);
end;

{ TMain.mmStandardClick

  MainMenu entry to resize all columns to standard size. }

procedure TMain.mmDefaultClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tsStartup) then
  begin
    lwStartup.Columns[1].Width := 125;
    lwStartup.Columns[2].Width := 122;
    lwStartup.Columns[3].Width := 75;
  end  //of begin
  else
    begin
      lwContext.Columns[1].Width := 150;
      lwContext.Columns[2].Width := 107;
      lwContext.Columns[3].Width := 65;
    end;  //of if
end;

{ TMain.mmOptimateClick

  MainMenu entry to resize all columns to fit the shown text optimal. }

procedure TMain.mmOptimateClick(Sender: TObject);
begin
  if (PageControl.ActivePage = tsStartup) then
  begin
    lwStartup.Columns[1].Width := ColumnTextWidth;
    lwStartup.Columns[2].Width := ColumnTextWidth;
    lwStartup.Columns[3].Width := ColumnTextWidth;
  end  //of begin
  else
    begin
      lwContext.Columns[1].Width := ColumnTextWidth;
      lwContext.Columns[2].Width := ColumnTextWidth;
    end;  //of if
end;

{ TMain.mmGerClick

  MainMenu entry that allows to change the current language to german. }

procedure TMain.mmGerClick(Sender: TObject);
begin
  FLang.ChangeLanguage(Sender, 100);
end;

{ TMain.mmEngClick

  MainMenu entry that allows to change the current language to english. }

procedure TMain.mmEngClick(Sender: TObject);
begin
  FLang.ChangeLanguage(Sender, 300);
end;

{ TMain.mmFraClick

  MainMenu entry that allows to change the current language to french. }

procedure TMain.mmFraClick(Sender: TObject);
begin
  FLang.ChangeLanguage(Sender, 500);
end;

{ TMain.mmDownloadCertClick

  MainMenu entry that allows to download the PM Code Works certificate. }

procedure TMain.mmDownloadCertClick(Sender: TObject);
var
  Updater: TUpdate;

begin
  // Certificate already installed?
  if (TOSUtils.PMCertExists() and (FLang.MessageBox([27, NEW_LINE, 28],
    mtQuestion) = IDNO)) then
    Exit;

  // Init downloader
  Updater := TUpdate.Create(Self, FLang);

  // Download certificate
  try
    with Updater do
    begin
      Title := FLang.GetString(16);
      DownloadCertificate();
    end;  //of begin

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
  TOSUtils.OpenUrl(URL_CONTACT);
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
  TOSUtils.OpenUrl(URL_BASE);
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
  // Disable some menu items not used on context menu page
  if (PageControl.ActivePage = tsContext) then
  begin
    mmImport.Visible := False;
    mmDate.Visible := False;
    mmRunOnce.Visible := False;
    mmShowIcons.Visible := False;
    mmAdd.Caption := FLang.GetString(34);
    lwContextSelectItem(Sender, lwContext.ItemFocused, True);

    // Load context menu entries dynamically
    if (Context.Count = 0) then
      LoadContextMenuEntries();
  end  //of begin
  else
    begin
      mmImport.Visible := True;
      mmDate.Visible := True;
      mmRunOnce.Visible := True;
      mmShowIcons.Visible := True;
      mmAdd.Caption := FLang.GetString(69);
      lwStartupSelectItem(Sender, lwStartup.ItemFocused, True);
    end;  //of if
end;

{ TMain.cbExpertClick

  Event method that is called when user clicked on "expert mode". }

procedure TMain.cbExpertClick(Sender: TObject);
begin
  LoadContextMenuEntries();
end;

{ TMain.bCloseClick

  Closes Clearas. }

procedure TMain.bCloseStartupClick(Sender: TObject);
begin
  Close;
end;

end.
