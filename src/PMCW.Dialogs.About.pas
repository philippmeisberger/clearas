{ *********************************************************************** }
{                                                                         }
{ PM Code Works About Form v2.0                                           }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs.About;

{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  VersionTypes, Resource,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls,
  Dialogs, PMCW.Dialogs.Updater;

const
  /// <summary>
  ///   The default name of the copyright/description resource.
  /// </summary>
  RESOURCE_DESCRIPTION = 'DESCRIPTION';

  /// <summary>
  ///   The default name of the changelog resource.
  /// </summary>
  RESOURCE_CHANGELOG   = 'CHANGELOG';

type
  /// <summary>
  ///   A <c>TAboutDialog</c> displays information about the current application
  ///   like version, copyright and changelog.
  /// </summary>
  /// <remarks>
  ///   The copyright and changelog must be stored in resources named per
  ///   default <c>DESCRIPTION</c> and <c>CHANGELOG</c>. The properties
  ///   <see cref="DescriptionResourceName"/> and <see cref="ChangelogResourceName"/>
  ///   allow to change the resource name. The main application icon is
  ///   retrieved automatically on Windows and shown on the left side. On Linux
  ///   this icon must be manually set through <see cref="ImageFile"/> property.
  /// </remarks>
  TAboutDialog = class(TCommonDialog)
  private
    FForm: TForm;
    FPageControl: TPageControl;
    FDescriptionTab,
    FChangelogTab: TTabSheet;
    FImage: TImage;
    FVersionLabel: TLabel;
    FOkButton1,
    FOkButton2: TButton;
    FCopying,
    FChangelog: TMemo;
    FDescriptionResourceName,
    FChangelogResourceName: string;
  {$IFDEF LINUX}
    FImageFile: string;
  {$ENDIF}
    procedure LoadResourceIntoMemo(const AResourceName: string; var AMemo: TMemo);
    procedure SetTitle(const ATitle: string);
    function GetTitle(): string;
    function GetChangelogCaption: TCaption;
    function GetDescriptionCaption: TCaption;
    procedure SetChangelogCaption(const AChangelogCaption: TCaption);
    procedure SetDescriptionCaption(const ADescriptionCaption: TCaption);
  public
    /// <summary>
    ///   Constructor for creating a <c>TAboutDialog</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   The owner.
    /// </param>
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Executes the dialog.
    /// </summary>
    /// <param name="AParentHwnd">
    ///   The handle to the parent window.
    /// </param>
    /// <returns>
    ///   Always <c>True</c>.
    /// </returns>
    function Execute({$IFNDEF FPC}AParentHwnd: HWND{$ENDIF}): Boolean; override;

    /// <summary>
    ///   Gets or set the caption of the changelog tab.
    /// </summary>
    property ChangelogCaption: TCaption read GetChangelogCaption write SetChangelogCaption;

    /// <summary>
    ///   Gets or sets the name of the changelog resource.
    /// </summary>
    property ChangelogResourceName: string read FChangelogResourceName write FChangelogResourceName;

    /// <summary>
    ///   Gets or set the caption of the description tab.
    /// </summary>
    property DescriptionCaption: TCaption read GetDescriptionCaption write SetDescriptionCaption;

    /// <summary>
    ///   Gets or sets the name of the copyright/description resource.
    /// </summary>
    property DescriptionResourceName: string read FDescriptionResourceName write FDescriptionResourceName;

  {$IFDEF LINUX}
    /// <summary>
    ///   Gets or sets the absolute path to the application icon that will be
    ///   displayed on the left side.
    /// </summary>
    /// <remarks>
    ///   Icon dimensions must be 48x48!
    /// </remarks>
    property ImageFile: string read FImageFile write FImageFile;
  {$ENDIF}

    /// <summary>
    ///   Gets or sets the dialog title.
    /// </summary>
    property Title: string read GetTitle write SetTitle;
  end;

implementation

{ TAboutDialog }

constructor TAboutDialog.Create(AOwner: TComponent);
const
  Margin = 15;

begin
  inherited Create(AOwner);
  FChangelogResourceName := RESOURCE_CHANGELOG;
  FDescriptionResourceName := RESOURCE_DESCRIPTION;
  FForm := TForm.Create(Self);

  with FForm do
  begin
    Font.Size := 8;
    BorderStyle := bsDialog;
    Caption := #220'ber '+ Application.Title;
    ClientHeight := 260;
    ClientWidth := 460;
    Position := poScreenCenter;
    PixelsPerInch := 96;
  end;  //of with

  FPageControl := TPageControl.Create(FForm);

  with FPageControl do
  begin
    Parent := FForm;
    Align := alClient;
    TabOrder := 0;
  end;  //of with

  // Description/copying tab
  FDescriptionTab := TTabSheet.Create(FForm);

  with FDescriptionTab do
  begin
    Parent := FPageControl;
    PageControl := FPageControl;
    Caption := 'Infos';
  {$IFDEF FPC}
    Width := FForm.Width - 6;
    Height := FForm.Height - 25;
  {$ENDIF}
  end;  //of with

  FImage := TImage.Create(FForm);

  with FImage do
  begin
    Parent := FDescriptionTab;
    Left := Margin;
    Top := Margin;
    Width := 48;
    Height := Width;
  end;  //of with

  FVersionLabel := TLabel.Create(FForm);

  with FVersionLabel do
  begin
    Parent := FDescriptionTab;
    AutoSize := False;
    Top := FImage.Top + FImage.Height + 5;
    Width := (2 * FImage.Left) + FImage.Width;
    Height := 2 * Height;
    Alignment := taCenter;
    WordWrap := True;
  end;  //of with

  FOkButton1 := TButton.Create(FForm);

  with FOkButton1 do
  begin
    Parent := FDescriptionTab;
    Width := 75;
    Height := 25;
    Left := FDescriptionTab.Width - Width - Margin;
    Top := FDescriptionTab.Height - Height - (Margin - 5);
  {$IFNDEF FPC}
    Anchors := [akBottom];
  {$ENDIF}
    Cancel := True;
    Caption := 'OK';
    Default := True;
    ModalResult := mrOk;
    TabOrder := 0;
  end;  //of with

  FCopying := TMemo.Create(FForm);

  with FCopying do
  begin
    Parent := FDescriptionTab;
    Left := FImage.Left + FImage.Width + Margin;
    Top := FImage.Top;
    Width := FDescriptionTab.Width - Left - Margin;
    Height := FOkButton1.Top - Margin - 5;
  {$IFNDEF FPC}
    Anchors := [akLeft, akTop, akRight, akBottom];
  {$ENDIF}
    ReadOnly := True;
    TabOrder := 1;
  end;  //of with

  // Changelog tab
  FChangelogTab := TTabSheet.Create(FForm);

  with FChangelogTab do
  begin
    Parent := FPageControl;
    PageControl := FPageControl;
    Caption := 'Changelog';
  end;  //of with

  FOkButton2 := TButton.Create(FForm);

  with FOkButton2 do
  begin
    Parent := FChangelogTab;
    Left := FOkButton1.Left;
    Top := FOkButton1.Top;
    Width := FOkButton1.Width;
    Height := FOkButton1.Height;
    Anchors := FOkButton1.Anchors;
    Cancel := FOkButton1.Cancel;
    Caption := FOkButton1.Caption;
    Default := FOkButton1.Default;
    ModalResult := FOkButton1.ModalResult;
    TabOrder := 0;
  end;  //of with

  FChangelog := TMemo.Create(FForm);

  with FChangelog do
  begin
    Parent := FChangelogTab;
    Left := Margin;
    Top := FCopying.Top;
    Width := FDescriptionTab.Width - Left - Margin;
    Height := FCopying.Height;
  {$IFNDEF FPC}
    Anchors := [akLeft, akTop, akRight, akBottom];
  {$ENDIF}
    ReadOnly := True;
    ScrollBars := ssVertical;
    TabOrder := 1;
  end;  //of with
end;

function TAboutDialog.Execute({$IFNDEF FPC}AParentHwnd: HWND{$ENDIF}): Boolean;
var
  VersionInfo: TFileProductVersion;

begin
  // Show version information
  if TUpdateCheck.GetFileVersion(Application.ExeName, VersionInfo) then
  begin
    // Long or short format?
    if (VersionInfo[VERSION_SERVICE] <> 0) then
      FVersionLabel.Caption := Format('v%d.%d.%d'+ sLineBreak +'(Build: %d)',
        [VersionInfo[VERSION_MAJOR], VersionInfo[VERSION_MINOR],
        VersionInfo[VERSION_SERVICE], VersionInfo[VERSION_BUILD]])
    else
      FVersionLabel.Caption := Format('v%d.%d '+ sLineBreak +'(Build: %d)',
        [VersionInfo[VERSION_MAJOR], VersionInfo[VERSION_MINOR],
        VersionInfo[VERSION_BUILD]])
  end;  //of begin

  // Load application icon into TImage
{$IFDEF MSWINDOWS}
  FImage.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
    FImage.Width, FImage.Height, LR_DEFAULTCOLOR);
{$ELSE}
  FImage.Picture.Icon.LoadFromFile(FImageFile);
{$ENDIF}

  // Load resources
  LoadResourceIntoMemo(FDescriptionResourceName, FCopying);
  LoadResourceIntoMemo(FChangelogResourceName, FChangelog);
  FForm.ShowModal();
  Result := True;
end;

function TAboutDialog.GetChangelogCaption(): TCaption;
begin
  Result := FChangelogTab.Caption;
end;

function TAboutDialog.GetDescriptionCaption(): TCaption;
begin
  Result := FDescriptionTab.Caption;
end;

function TAboutDialog.GetTitle(): string;
begin
  Result := FForm.Caption;
end;

procedure TAboutDialog.LoadResourceIntoMemo(const AResourceName: string;
  var AMemo: TMemo);
var
  ResourceStream: TResourceStream;

begin
  ResourceStream := TResourceStream.Create(HInstance, AResourceName, PChar(RT_RCDATA));

  try
    AMemo.Lines.LoadFromStream(ResourceStream);

  finally
    ResourceStream.Free;
  end;  //of try
end;

procedure TAboutDialog.SetChangelogCaption(const AChangelogCaption: TCaption);
begin
  FChangelogTab.Caption := AChangelogCaption;
end;

procedure TAboutDialog.SetDescriptionCaption(const ADescriptionCaption: TCaption);
begin
  FDescriptionTab.Caption := ADescriptionCaption;
end;

procedure TAboutDialog.SetTitle(const ATitle: string);
begin
  FForm.Caption := ATitle;
end;

end.
