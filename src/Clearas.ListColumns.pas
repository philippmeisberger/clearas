{ *********************************************************************** }
{                                                                         }
{ Clearas ListView Columns Unit                                           }
{                                                                         }
{ Copyright (c) 2011-2019 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit Clearas.ListColumns;

interface

uses
  System.Classes, Vcl.Menus, Vcl.ComCtrls, Winapi.CommCtrl, PMCW.LanguageFile;

type
  /// <summary>
  ///   Available list columns.
  /// </summary>
  TClearasListColumn = (

    /// <summary>
    ///   Shows the item name/caption.
    /// </summary>
    ItemName = 1,

    /// <summary>
    ///   Shows the command.
    /// </summary>
    Command = 2,

    /// <summary>
    ///   Shows the item type.
    /// </summary>
    ItemType = 3,

    /// <summary>
    ///   Shows the status.
    /// </summary>
    Status = 4,

    /// <summary>
    ///   Shows the deactivation date.
    /// </summary>
    DeactivationDate = 5,

    /// <summary>
    ///   Shows the item location.
    /// </summary>
    Location = 6,

    /// <summary>
    ///   Shows the item startup type.
    /// </summary>
    StartupType = 7,

    /// <summary>
    ///   Shows the publisher.
    /// </summary>
    Publisher = 8
  );

  TClearasListColumnHelper = record helper for TClearasListColumn
    /// <summary>
    ///   Checks if the column is currently shown in a specified <see cref="TListView"/>.
    /// </summary>
    /// <param name="AListView">
    ///   The <see cref="TListView"/> to check.
    /// </param>
    /// <returns>
    ///   <c>True</c> if column is visible or <c>False</c> otherwise.
    /// </returns>
    function IsVisible(AListView: TListView): Boolean; inline;

    /// <summary>
    ///   Gets the index of the column inside a specified <see cref="TListView"/>.
    /// </summary>
    /// <param name="AListView">
    ///   The <see cref="TListView"/>.
    /// </param>
    /// <returns>
    ///   The column index or <c>-1</c> if not found.
    /// </returns>
    function GetColumnIndex(AListView: TListView): Integer;

    /// <summary>
    ///   Updates the displayed information inside a specified <see cref="TListView"/>.
    /// </summary>
    /// <param name="AListView">
    ///   The <see cref="TListView"/>.
    /// </param>
    /// <param name="AItem">
    ///   The item to update.
    /// </param>
    /// <param name="ANewContent">
    ///   The new content.
    /// </param>
    procedure UpdateContent(AListView: TListView; AItem: TListItem; const ANewContent: string);

    /// <summary>
    ///   Gets the column caption.
    /// </summary>
    /// <param name="ALanguageFile">
    ///   A <see cref="TLanguageFile"/> that contains the translations.
    /// </param>
    /// <returns>
    ///   The caption.
    /// </returns>
    function ToString(ALanguageFile: TLanguageFile): string;
  end;

  /// <summary>
  ///   A popup menu used to show/hide columns inside a <see cref="TListView"/>.
  /// </summary>
  TListViewColumnSelectionMenu = class(TPopupMenu, IChangeLanguageListener)
  private
    FListView: TListView;
    FAutoSizeColumn,
    FAutoSizeAllColumns: TMenuItem;
    FColumn: TListColumn;
    FLanguageFile: TLanguageFile;
    FOnColumnChanged: TNotifyEvent;
    procedure ColumnMenuItemClick(Sender: TObject);
    procedure AutoSizeColumnClick(Sender: TObject);
    procedure AutoSizeColumnsClick(Sender: TObject);
  protected
    { IChangeLanguageListener }
    procedure LanguageChanged();
  public
    /// <summary>
    ///   Constructor for creating a <c>TListViewColumnSelectionMenu</c> instance.
    /// </summary>
    /// <param name="AListView">
    ///   The <see cref="TListView"/> which columns should be changed.
    /// </param>
    /// <param name="ALanguageFile">
    ///   A <see cref="TLanguageFile"/> that contains the translations.
    /// </param>
    constructor Create(AListView: TListView; ALanguageFile: TLanguageFile); reintroduce;

    /// <summary>
    ///   Shows the popup menu at the specified location.
    /// </summary>
    /// <param name="x,y">
    ///   The coordinates.
    /// </param>
    procedure Popup(X, Y: Integer); overload; override;

    /// <summary>
    ///   Shows the popup menu at the specified location and allows column auto-size.
    /// </summary>
    /// <param name="x,y">
    ///   The coordinates.
    /// </param>
    /// <param name="AColumn">
    ///   The clicked column.
    /// </param>
    procedure Popup(X, Y: Integer; AColumn: TListColumn); reintroduce; overload;

    /// <summary>
    ///   Occurs if column should be shown or hidden.
    /// </summary>
    property OnColumnChanged: TNotifyEvent read FOnColumnChanged write FOnColumnChanged;

    /// <summary>
    ///  Gets or sets the <see cref="TListView"/> which columns should be changed.
    /// </summary>
    property ListView: TListView read FListView write FListView;
  end;

implementation

{$I LanguageIDs.inc}

{ TClearasListColumnHelper }

function TClearasListColumnHelper.GetColumnIndex(AListView: TListView): Integer;
var
  i: Integer;

begin
  Result := -1;

  for i := 0 to AListView.Columns.Count - 1 do
  begin
    if (Self = TClearasListColumn(AListView.Columns[i].Tag)) then
      Exit(i);
  end;  //of for
end;

function TClearasListColumnHelper.IsVisible(AListView: TListView): Boolean;
begin
  Result := (GetColumnIndex(AListView) <> -1);
end;

function TClearasListColumnHelper.ToString(ALanguageFile: TLanguageFile): string;
begin
  case Self of
    ItemName:         Result := ALanguageFile[LID_NAME];
    Command:          Result := ALanguageFile[LID_FILE];
    ItemType:         Result := ALanguageFile[LID_TYPE];
    Status:           Result := ALanguageFile[LID_ENABLED];
    DeactivationDate: Result := ALanguageFile[LID_DATE_OF_DEACTIVATION];
    Location:         Result := ALanguageFile[LID_LOCATION];
    StartupType:      Result := ALanguageFile[LID_STARTUP_TYPE];
    Publisher:        Result := ALanguageFile[LID_PUBLISHER];
    else              Result := '';
  end;  //of case
end;

procedure TClearasListColumnHelper.UpdateContent(AListView: TListView;
  AItem: TListItem; const ANewContent: string);
var
  ColumnIndex: Integer;

begin
  if not Assigned(AItem) then
    Exit;

  ColumnIndex := GetColumnIndex(AListView);

  if (ColumnIndex = 0) then
    AItem.Caption := ANewContent
  else
    if (ColumnIndex > 0) then
      AItem.SubItems[ColumnIndex - 1] := ANewContent;
end;


{ TListColumnSelectionMenu }

constructor TListViewColumnSelectionMenu.Create(AListView: TListView;
  ALanguageFile: TLanguageFile);
var
  MenuItem: TMenuItem;
  ListColumn: TClearasListColumn;

begin
  inherited Create(AListView);
  FListView := AListView;
  FLanguageFile := ALanguageFile;
  ALanguageFile.AddListener(Self);

  // Auto-size this colummn
  FAutoSizeColumn := TMenuItem.Create(Self);

  with FAutoSizeColumn do
  begin
    Caption := FLanguageFile[LID_ADJUST_COLUMN];
    OnClick := AutoSizeColumnClick;
  end;  //of with

  Items.Add(FAutoSizeColumn);

  // Auto-size all columns
  FAutoSizeAllColumns := TMenuItem.Create(Self);

  with FAutoSizeAllColumns do
  begin
    Caption := FLanguageFile[LID_ADJUST_COLUMNS];
    OnClick := AutoSizeColumnsClick;
  end;  //of with

  Items.Add(FAutoSizeAllColumns);

  // Separator
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := '-';
  Items.Add(MenuItem);

  // Create a menu with all available columns
  for ListColumn := Low(TClearasListColumn) to High(TClearasListColumn) do
  begin
    MenuItem := TMenuItem.Create(Self);

    with MenuItem do
    begin
      Caption := ListColumn.ToString(FLanguageFile);
      AutoCheck := True;
      OnClick := ColumnMenuItemClick;
      Tag := NativeInt(ListColumn);
      Enabled := (ListColumn <> TClearasListColumn.ItemName);
    end;  //of with

    Items.Add(MenuItem);
  end;  //of for
end;

procedure TListViewColumnSelectionMenu.LanguageChanged;
var
  MenuItem: TMenuItem;

begin
  if Assigned(FAutoSizeColumn) then
    FAutoSizeColumn.Caption := FLanguageFile[LID_ADJUST_COLUMN];

  if Assigned(FAutoSizeAllColumns) then
    FAutoSizeAllColumns.Caption := FLanguageFile[LID_ADJUST_COLUMNS];

  for MenuItem in Items do
  begin
    if (MenuItem.Tag <> 0) then
      MenuItem.Caption := TClearasListColumn(MenuItem.Tag).ToString(FLanguageFile);
  end;  //of for
end;

procedure TListViewColumnSelectionMenu.AutoSizeColumnClick(Sender: TObject);
begin
  // Auto-size selected column
  if Assigned(FColumn) then
    FColumn.Width := LVSCW_AUTOSIZE;
end;

procedure TListViewColumnSelectionMenu.AutoSizeColumnsClick(Sender: TObject);
var
  i: Integer;

begin
  // Auto-size all columns
  for i := 0 to FListView.Columns.Count - 1 do
    FListView.Column[i].Width := LVSCW_AUTOSIZE;
end;

procedure TListViewColumnSelectionMenu.ColumnMenuItemClick(Sender: TObject);
var
  ColumnIndex: Integer;

begin
  if (Sender as TMenuItem).Checked then
  begin
    // Add column
    with FListView.Columns.Add do
    begin
      Tag := (Sender as TMenuItem).Tag;
      Caption := TClearasListColumn(Tag).ToString(FLanguageFile);
    end;  //of with
  end  //of begin
  else
  begin
    // Delete column
    ColumnIndex := TClearasListColumn((Sender as TMenuItem).Tag).GetColumnIndex(FListView);

    if (ColumnIndex <> -1) then
      FListView.Columns.Delete(ColumnIndex);
  end;  //of if

  // Notify columns have changed to refresh UI
  if Assigned(OnColumnChanged) then
    OnColumnChanged(Self);
end;

procedure TListViewColumnSelectionMenu.Popup(X, Y: Integer);
var
  i: Integer;

begin
  // Refresh check marks
  for i := 0 to Items.Count - 1 do
    Items[i].Checked := TClearasListColumn(Items[i].Tag).IsVisible(FListView);

  FAutoSizeColumn.Visible := Assigned(FColumn);
  inherited Popup(X, Y);
end;

procedure TListViewColumnSelectionMenu.Popup(X, Y: Integer; AColumn: TListColumn);
begin
  FColumn := AColumn;
  Popup(X, Y);
end;

end.
