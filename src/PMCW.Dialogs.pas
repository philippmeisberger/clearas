{ *********************************************************************** }
{                                                                         }
{ PM Code Works Dialogs Unit v1.0                                         }
{                                                                         }
{ Copyright (c) 2011-2018 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  SysUtils, Classes, Dialogs, PMCW.LanguageFile, PMCW.Dialogs.About,
{$IFDEF FPC}
  PMCW.SysUtils,
{$ELSE}
  System.UITypes, PMCW.Dialogs.ReportBug,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Winapi.Windows, Vcl.Forms, Vcl.StdCtrls, PMCW.CA;
{$ENDIF}

/// <summary>
///   Shows the about dialog.
/// </summary>
/// <param name="ATitle">
///   The title.
/// </param>
procedure AboutDlg(const ATitle: string);

/// <summary>
///   Shows an exception message with additional information.
/// </summary>
/// <param name="ALanguageFile">
///   UI translation file.
/// </param>
/// <param name="AMessage">
///   An error message.
/// </param>
/// <param name="ADetails">
///   Technical error details.
/// </param>
procedure ExceptionDlg(ALanguageFile: TLanguageFile; const AMessage, ADetails: string);

{$IFDEF MSWINDOWS}
/// <summary>
///   Shows a dialog to install the code-signing certificate.
/// </summary>
/// <param name="ALanguageFile">
///   UI translation file.
/// </param>
procedure InstallCertificateDlg(ALanguageFile: TLanguageFile);
{$ENDIF}

/// <summary>
///   Shows a report bug dialog.
/// </summary>
/// <param name="ALanguageFile">
///   UI translation file.
/// </param>
/// <param name="AMessage">
///   The error message.
/// </param>
procedure ReportBugDlg(ALanguageFile: TLanguageFile; const AMessage: string);

implementation

procedure AboutDlg(const ATitle: string);
var
  AboutDialog: TAboutDialog;
  Description, Changelog: TResourceStream;

begin
  AboutDialog := TAboutDialog.Create(nil);
  Description := TResourceStream.Create(HInstance, RESOURCE_DESCRIPTION, RT_RCDATA);
  Changelog := TResourceStream.Create(HInstance, RESOURCE_CHANGELOG, RT_RCDATA);

  try
    AboutDialog.Title := ATitle;
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

procedure ExceptionDlg(ALanguageFile: TLanguageFile; const AMessage, ADetails: string);
{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
var
  TaskDialog: TTaskDialog;

begin
  TaskDialog := TTaskDialog.Create(nil);

  try
    with TaskDialog do
    begin
      MainIcon := tdiError;
      Title := ALanguageFile[LID_FATAL_ERROR];
      Text := AMessage;
      ExpandedText := ADetails;
      ExpandButtonCaption := ALanguageFile[LID_TECHNICAL_DETAILS];
      Flags := [tfExpandFooterArea];
      CommonButtons := [tcbClose];

      with Buttons.Add do
      begin
        Caption := ALanguageFile[LID_REPORT_BUG];
        ModalResult := mrOk;
      end;  //of with
    end;  //of with

    TaskDialog.Execute();

    // Report bug?
    if (TaskDialog.ModalResult = mrOk) then
      ReportBugDlg(ALanguageFile, ALanguageFile.Format(LID_REPORT_BUG_BODY, [AMessage, ADetails]));

  finally
    FreeAndNil(TaskDialog);
  end;  //of try
{$WARN SYMBOL_PLATFORM ON}
{$ELSE}
begin
  MessageDlg(ALanguageFile[LID_FATAL_ERROR] +': '+ AMessage + sLineBreak
    + ADetails, mtError, [mbClose], 0);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure InstallCertificateDlg(ALanguageFile: TLanguageFile);
begin
  try
    // Certificate already installed?
    if CertificateExists() then
      MessageDlg(ALanguageFile[LID_CERTIFICATE_ALREADY_INSTALLED], mtInformation, [mbOK], 0)
    else
      InstallCertificate();

  except
    on E: EOSError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;  //of try
end;
{$ENDIF}

procedure ReportBugDlg(ALanguageFile: TLanguageFile; const AMessage: string);
{$IFDEF FPC}
begin
  OpenUrl(URL_CONTACT);
{$ELSE}
var
  ReportDialog: TReportBugDialog;

begin
  ReportDialog := TReportBugDialog.Create(nil, ALanguageFile);

  try
    ReportDialog.Report.Text := AMessage;
    ReportDialog.Execute();

  finally
    FreeAndNil(ReportDialog);
  end;  //of try
{$ENDIF}
end;

end.
