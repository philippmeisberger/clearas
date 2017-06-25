{ *********************************************************************** }
{                                                                         }
{ PM Code Works About Form v2.1                                           }
{                                                                         }
{ Copyright (c) 2011-2017 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs.About;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Dialogs,
  PMCW.SysUtils;

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
  ///   The copyright and changelog information must be set by using
  ///   <see cref="Changelog"/> and <see cref="Description"/> properties. The
  ///   version information are retrieved automatically. The main application
  ///   icon is retrieved automatically only on Windows. On Linux this icon must
  ///   be manually set through <see cref="ImageFile"/> property.
  /// </remarks>
  TAboutDialog = class(TCommonDialog)
  private
    FForm: TForm;
    FPageControl: TPageControl;
    FDescriptionTab,
    FChangelogTab: TTabSheet;
    FImage: TImage;
    FVersionLabel: TLabel;
    FCloseButton1,
    FCloseButton2: TButton;
    FCopying,
    FChangelog: TMemo;
  {$IFDEF LINUX}
    FImageFile: string;
  {$ENDIF}
    function GetChangelog(): TStrings;
    function GetDescription(): TStrings;
    function GetTitle(): string;
    function GetChangelogCaption(): TCaption;
    function GetDescriptionCaption(): TCaption;
    procedure SetChangelogCaption(const AChangelogCaption: TCaption);
    procedure SetDescriptionCaption(const ADescriptionCaption: TCaption);
    procedure SetTitle(const ATitle: string);
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
    ///   Gets or sets the changelog.
    /// </summary>
    property Changelog: TStrings read GetChangelog;

    /// <summary>
    ///   Gets or set the caption of the changelog tab.
    /// </summary>
    property ChangelogCaption: TCaption read GetChangelogCaption write SetChangelogCaption;

    /// <summary>
    ///   Gets or sets the copyright/description.
    /// </summary>
    property Description: TStrings read GetDescription;

    /// <summary>
    ///   Gets or set the caption of the description tab.
    /// </summary>
    property DescriptionCaption: TCaption read GetDescriptionCaption write SetDescriptionCaption;
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
  FForm := TForm.Create(Self);

  with FForm do
  begin
    Font.Size := 8;
    BorderStyle := bsDialog;
    Caption := 'About '+ Application.Title;
    ClientHeight := 260;
    ClientWidth := 460;
    Position := poScreenCenter;
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
    Caption := 'Information';
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
    Height := 3 * Height;
    Alignment := taCenter;
    WordWrap := True;
  end;  //of with

  FCloseButton1 := TButton.Create(FForm);

  with FCloseButton1 do
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
    Height := FCloseButton1.Top - Margin - 5;
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

  FCloseButton2 := TButton.Create(FForm);

  with FCloseButton2 do
  begin
    Parent := FChangelogTab;
    Left := FCloseButton1.Left;
    Top := FCloseButton1.Top;
    Width := FCloseButton1.Width;
    Height := FCloseButton1.Height;
    Anchors := FCloseButton1.Anchors;
    Cancel := FCloseButton1.Cancel;
    Caption := FCloseButton1.Caption;
    Default := FCloseButton1.Default;
    ModalResult := FCloseButton1.ModalResult;
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
const
  Architecture = {$IFDEF CPUX64}'64-bit'{$ELSE}'32-bit'{$ENDIF};

var
  FileVersion: TFileVersion;

begin
  // Show version information
  if FileVersion.FromFile(Application.ExeName) then
  begin
    if (FileVersion.Service <> 0) then
    begin
      FVersionLabel.Caption := FileVersion.ToString('v%d.%d.%d'+ sLineBreak
        +'(Build: %d)'+ sLineBreak + Architecture);
    end  //of begin
    else
    begin
      FVersionLabel.Caption := Format('v%d.%d'+ sLineBreak +'(Build: %d)'
        + sLineBreak + Architecture, [FileVersion.Major, FileVersion.Minor,
        FileVersion.Build]);
    end;  //of if
  end;  //of begin

  // Load application icon into TImage
{$IFDEF MSWINDOWS}
  FImage.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
    FImage.Width, FImage.Height, LR_DEFAULTCOLOR);
{$ELSE}
  FImage.Picture.Icon.LoadFromFile(FImageFile);
{$ENDIF}
  FForm.ShowModal();
  Result := True;
end;

function TAboutDialog.GetChangelog(): TStrings;
begin
  Result := FChangelog.Lines;
end;

function TAboutDialog.GetChangelogCaption(): TCaption;
begin
  Result := FChangelogTab.Caption;
end;

function TAboutDialog.GetDescription(): TStrings;
begin
  Result := FCopying.Lines;
end;

function TAboutDialog.GetDescriptionCaption(): TCaption;
begin
  Result := FDescriptionTab.Caption;
end;

function TAboutDialog.GetTitle(): string;
begin
  Result := FForm.Caption;
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
