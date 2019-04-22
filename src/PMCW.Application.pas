{ *********************************************************************** }
{                                                                         }
{ PM Code Works Application Unit v1.0                                     }
{                                                                         }
{ Copyright (c) 2011-2018 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Application;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, PMCW.CA,
{$ENDIF}
{$IFDEF FPC}
  PMCW.SysUtils,
{$ELSE}
  System.UITypes, PMCW.Dialogs.Updater,
{$ENDIF}
  SysUtils, Classes, Forms, Menus, Dialogs, PMCW.LanguageFile, PMCW.Dialogs,
  PMCW.Dialogs.About;

type
  /// <summary>
  ///   Base class for main forms.
  /// </summary>
  TMainForm = class(TForm, IChangeLanguageListener)
  private
  {$IFNDEF FPC}
    FUpdateCheck: TUpdateCheck;
    FFileNameLocal,
    FFileNameRemote,
    FFileNameRemote64: string;
  {$ENDIF}
    FMenuLanguages,
    FMenuHelp,
    FMenuUpdate,
    FMenuBreak1,
  {$IFDEF MSWINDOWS}
    FMenuInstallCert,
  {$ENDIF}
    FMenuReportBug,
    FMenuBreak2,
    FMenuAbout: TMenuItem;
    procedure AboutClick(Sender: TObject);
    procedure LanguageSelected(Sender: TObject);
    procedure UpdateSelectedLanguage(Sender: TObject);
  {$IFNDEF FPC}
    procedure OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    procedure InstallCertificateClick(Sender: TObject);
  {$ENDIF}
    procedure ReportBugClick(Sender: TObject);
    procedure UpdateClick(Sender: TObject);
  protected
    FLang: TLanguageFile;

    /// <summary>
    ///   Builds the help menu.
    /// </summary>
    /// <param name="AMainMenu">
    ///   The menu item to create the submenu.
    /// </param>
    procedure BuildHelpMenu(AMenuItem: TMenuItem);

    /// <summary>
    ///   Builds the choose language menu.
    /// </summary>
    /// <param name="AMainMenu">
    ///   The menu item to create the submenu.
    /// </param>
    procedure BuildLanguageMenu(AMenuItem: TMenuItem);
  {$IFNDEF FPC}
    /// <summary>
    ///   Checks for update.
    /// </summary>
    /// <param name="ARemoteDirName">
    ///   The remote directory which contains version.txt.
    /// </param>
    /// <param name="AFileNameRemote">
    ///   The name of the remote file.
    /// </param>
    /// <param name="AFileNameRemote64">
    ///   The name of the remote 64-bit file.
    /// </param>
    /// <param name="AFileNameLocal">
    ///   The name to store the remote file.
    /// </param>
    procedure CheckForUpdate(const ARemoteDirName, AFileNameRemote,
      AFileNameRemote64, AFileNameLocal: string);
  {$ENDIF}
    { IChangeLanguageListener }
    procedure LanguageChanged(); virtual;
  public
    /// <summary>
    ///   Destructor for destroying a <c>TMainForm</c> instance.
    /// </summary>
    destructor Destroy; override;
  end;

implementation

{ TMainForm }

destructor TMainForm.Destroy;
begin
  FreeAndNil(FMenuUpdate);
  FreeAndNil(FMenuBreak1);
{$IFDEF MSWINDOWS}
  FreeAndNil(FMenuInstallCert);
{$ENDIF}
  FreeAndNil(FMenuReportBug);
  FreeAndNil(FMenuBreak2);
  FreeAndNil(FMenuAbout);
{$IFNDEF FPC}
  FreeAndNil(FUpdateCheck);
{$ENDIF}
  FreeAndNil(FLang);
  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
procedure TMainForm.InstallCertificateClick(Sender: TObject);
begin
  try
    // Certificate already installed?
    if CertificateExists() then
      MessageDlg(FLang[LID_CERTIFICATE_ALREADY_INSTALLED], mtInformation, [mbOK], 0)
    else
      InstallCertificate();

  except
    on E: EOSError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;  //of try
end;
{$ENDIF}

procedure TMainForm.AboutClick(Sender: TObject);
var
  AboutDialog: TAboutDialog;
  Description, Changelog: TResourceStream;

begin
  AboutDialog := TAboutDialog.Create(nil);
  Description := TResourceStream.Create(HInstance, RESOURCE_DESCRIPTION, RT_RCDATA);
  Changelog := TResourceStream.Create(HInstance, RESOURCE_CHANGELOG, RT_RCDATA);

  try
    AboutDialog.Title := {$IFDEF FPC}FMenuAbout.Caption{$ELSE}StripHotkey(FMenuAbout.Caption){$ENDIF};
  {$IFDEF LINUX}
    AboutDialog.Icon.LoadFromResourceName(HINSTANCE, 'MAINICON');
  {$ENDIF}
    AboutDialog.Description.LoadFromStream(Description);
    AboutDialog.Changelog.LoadFromStream(Changelog);
    AboutDialog.Execute();

  finally
    FreeAndNil(Changelog);
    FreeAndNil(Description);
    FreeAndNil(AboutDialog);
  end;  //of begin
end;

procedure TMainForm.BuildHelpMenu(AMenuItem: TMenuItem);
begin
  FMenuHelp := AMenuItem;
  FMenuHelp.Caption := FLang[LID_HELP];

  // "Search for update"
  FMenuUpdate := TMenuItem.Create(AMenuItem);
  FMenuUpdate.Caption := FLang[{$IFDEF FPC}LID_TO_WEBSITE{$ELSE}LID_UPDATE_SEARCH{$ENDIF}];
  FMenuUpdate.OnClick := UpdateClick;
  AMenuItem.Add(FMenuUpdate);

  // Separator
  FMenuBreak1 := TMenuItem.Create(AMenuItem);
  FMenuBreak1.Caption := '-';
  AMenuItem.Add(FMenuBreak1);

{$IFDEF MSWINDOWS}
  // "Install certificate"
  FMenuInstallCert := TMenuItem.Create(AMenuItem);
  FMenuInstallCert.Caption := FLang[LID_CERTIFICATE_INSTALL];
  FMenuInstallCert.OnClick := InstallCertificateClick;
  AMenuItem.Add(FMenuInstallCert);
{$ENDIF}

  // "Report bug"
  FMenuReportBug := TMenuItem.Create(AMenuItem);
  FMenuReportBug.Caption := FLang[LID_REPORT_BUG];
  FMenuReportBug.OnClick := ReportBugClick;
  AMenuItem.Add(FMenuReportBug);

  // Separator
  FMenuBreak2 := TMenuItem.Create(AMenuItem);
  FMenuBreak2.Caption := '-';
  AMenuItem.Add(FMenuBreak2);

  // "About ..."
  FMenuAbout := TMenuItem.Create(AMenuItem);
  FMenuAbout.Caption := FLang.Format(LID_ABOUT, [Application.Title]);
  FMenuAbout.OnClick := AboutClick;
  AMenuItem.Add(FMenuAbout);
end;

procedure TMainForm.BuildLanguageMenu(AMenuItem: TMenuItem);
var
  MenuItem: TMenuItem;
  i: Integer;
{$IFDEF MSWINDOWS}
  Locale: TLocale;
{$ENDIF}

begin
  FMenuLanguages := AMenuItem;
  FMenuLanguages.Caption := FLang[LID_SELECT_LANGUAGE];
  FMenuLanguages.OnClick := UpdateSelectedLanguage;

  // Create submenu
  for i := 0 to FLang.Languages.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(AMenuItem.Owner);

    with MenuItem do
    begin
      RadioItem := True;
      AutoCheck := True;
    {$IFDEF MSWINDOWS}
      Locale := StrToInt(FLang.Languages.Names[i]);
      Tag := Locale;
      Caption := Locale.DisplayName();
    {$ELSE}
      Hint := FLang.Languages.Names[i];
      Caption := FLang.Languages.ValueFromIndex[i];
    {$ENDIF}
      OnClick := LanguageSelected;
    end;  //of with

    AMenuItem.Add(MenuItem);
  end;  //of for
end;

procedure TMainForm.LanguageChanged();
begin
  if not Assigned(FMenuLanguages) then
    Exit;

  FMenuLanguages.Caption := FLang[LID_SELECT_LANGUAGE];
  FMenuHelp.Caption := FLang[LID_HELP];
  FMenuUpdate.Caption := FLang[{$IFDEF FPC}LID_TO_WEBSITE{$ELSE}LID_UPDATE_SEARCH{$ENDIF}];
{$IFDEF MSWINDOWS}
  FMenuInstallCert.Caption := FLang[LID_CERTIFICATE_INSTALL];
{$ENDIF}
  FMenuReportBug.Caption := FLang[LID_REPORT_BUG];
  FMenuAbout.Caption := FLang.Format(LID_ABOUT, [Application.Title]);
end;

procedure TMainForm.LanguageSelected(Sender: TObject);
begin
  FLang.Locale := (Sender as TMenuItem).{$IFDEF MSWINDOWS}Tag{$ELSE}Hint{$ENDIF};
end;

{$IFNDEF FPC}
procedure TMainForm.CheckForUpdate(const ARemoteDirName, AFileNameRemote,
  AFileNameRemote64, AFileNameLocal: string);
begin
  FFileNameRemote := AFileNameRemote;
  FFileNameRemote64 := AFileNameRemote64;
  FFileNameLocal := AFileNameLocal;

  if not Assigned(FUpdateCheck) then
  begin
    FUpdateCheck := TUpdateCheck.Create(ARemoteDirName, FLang);
    FUpdateCheck.OnUpdate := OnUpdate;
  {$IFNDEF DEBUG}
    FUpdateCheck.CheckForUpdate();
  {$ENDIF}
  end  //of begin
  else
    UpdateClick(Self);
end;
{$ENDIF}

procedure TMainForm.ReportBugClick(Sender: TObject);
begin
  ReportBugDlg(FLang, '');
end;

procedure TMainForm.UpdateClick(Sender: TObject);
begin
{$IFDEF FPC}
  OpenUrl(URL_BASE);
{$ELSE}
  FUpdateCheck.NotifyNoUpdate := True;
  FUpdateCheck.CheckForUpdate();
{$ENDIF}
end;

procedure TMainForm.UpdateSelectedLanguage(Sender: TObject);
var
  i: Integer;

begin
  for i := 0 to FMenuLanguages.Count - 1 do
    FMenuLanguages[i].Checked := (FMenuLanguages[i].{$IFDEF MSWINDOWS}Tag{$ELSE}Hint{$ENDIF} = FLang.Locale);
end;

{$IFNDEF FPC}
procedure TMainForm.OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
var
  Updater: TUpdateDialog;

begin
  FMenuUpdate.Caption := FLang[LID_UPDATE_DOWNLOAD];

  // Ask user to permit download
  if (TaskMessageDlg(FLang.Format(LID_UPDATE_AVAILABLE, [ANewBuild]),
    FLang[LID_UPDATE_CONFIRM_DOWNLOAD], mtConfirmation, mbYesNo, 0) = idYes) then
  begin
    Updater := TUpdateDialog.Create(Self, FLang);

    try
      with Updater do
      begin
        FileNameLocal := FFileNameLocal;

        // Download 64-Bit version?
        if ((FFileNameRemote64 <> '') and (TOSVersion.Architecture = arIntelX64)) then
          FileNameRemote := FFileNameRemote64
        else
          FileNameRemote := FFileNameRemote;
      end;  //of with

      // Successfully downloaded update?
      if Updater.Execute() then
      begin
        FMenuUpdate.Caption := FLang[LID_UPDATE_SEARCH];
        FMenuUpdate.Enabled := False;
      end;  //of begin

    finally
      Updater.Free;
    end;  //of try
  end;  //of begin
end;
{$ENDIF}

end.
