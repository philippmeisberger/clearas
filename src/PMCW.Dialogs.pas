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
{$IFDEF FPC}
  PMCW.SysUtils,
{$ELSE}
  System.UITypes, PMCW.Dialogs.ReportBug,
{$ENDIF}
  SysUtils, Dialogs, PMCW.LanguageFile;

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
