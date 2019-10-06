{ *********************************************************************** }
{                                                                         }
{ PM Code Works Dialogs Unit v1.0                                         }
{                                                                         }
{ Copyright (c) 2011-2019 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
{$IFDEF FPC}
  PMCW.SysUtils, Controls,
{$ELSE}
  System.UITypes, PMCW.Dialogs.ReportBug,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, Classes, StdCtrls, Forms,
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

{$IFDEF MSWINDOWS}
/// <summary>
///   Shows a dialog with a pre defined <c>TComboBox</c> list item selection.
///   Similar to the <c>InputQuery</c> dialog.
/// </summary>
/// <param name="ACaption">
///   The window caption to use.
/// </param>
/// <param name="APrompt">
///   The prompt to show.
/// </param>
/// <param name="AList">
///   The available items in the <c>TComboBox</c>.
/// </param>
/// <param name="AValue">
///   The value from the <c>TComboBox</c> which was selected by the user. This
///   can also be used as the initial value.
/// </param>
/// <param name="AReadOnly">
///   If set to <c>True</c> the user cannot set a custom value and is forced to
///   choose one of the items in the <c>TComboBox</c>. Otherwise custom values
///   are possible.
/// </param>
/// <returns>
///   <c>True</c> if the user clicks "OK" or <c>False</c> otherwise.
/// </returns>
function InputCombo(const ACaption, APrompt: string; AList: TStrings;
  var AValue: string; AReadOnly: Boolean = True): Boolean; overload;

/// <summary>
///   Shows a dialog with a pre defined <c>TComboBox</c> list item selection.
///   Similar to the <c>InputQuery</c> dialog with optional verification
///   <c>TCheckBox</c>.
/// </summary>
/// <param name="ACaption">
///   The window caption to use.
/// </param>
/// <param name="APrompt">
///   The prompt to show.
/// </param>
/// <param name="AList">
///   The available items in the <c>TComboBox</c>.
/// </param>
/// <param name="AValue">
///   The value from the <c>TComboBox</c> which was selected by the user. This
///   can also be used as the initial value.
/// </param>
/// <param name="ACheckBoxCaption">
///   The caption of the <c>TCheckBox</c>.
/// </param>
/// <param name="ACheckBoxChecked">
///   The value from the <c>TCheckBox</c> which can be selected by the user.
///   This can also be used as the initial value.
/// </param>
/// <param name="AReadOnly">
///   If set to <c>True</c> the user cannot set a custom value and is forced to
///   choose one of the items in the <c>TComboBox</c>. Otherwise custom values
///   are possible.
/// </param>
/// <returns>
///   <c>True</c> if the user clicks "OK" or <c>False</c> otherwise.
/// </returns>
function InputCombo(const ACaption, APrompt: string; AList: TStrings;
  var AValue: string; const ACheckBoxCaption: string; var ACheckBoxChecked: Boolean;
  AReadOnly: Boolean = True): Boolean; overload;
{$ENDIF}

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
  if (MessageDlg(ALanguageFile[LID_FATAL_ERROR], AMessage +': '+ ADetails + sLineBreak +
    ALanguageFile[LID_REPORT_BUG]+'?', mtError, [mbYes, mbClose], 0) = mrYes) then
    ReportBugDlg(ALanguageFile, AMessage);
{$ENDIF}
end;

procedure ReportBugDlg(ALanguageFile: TLanguageFile; const AMessage: string);
{$IFDEF FPC}
begin
  OpenUrl('mailto:'+ MAIL_CONTACT);
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

{$IFDEF MSWINDOWS}
function InputCombo(const ACaption, APrompt: string; AList: TStrings;
  var AValue: string; AReadOnly: Boolean = True): Boolean;
var
  Checked: Boolean;

begin
  Result := InputCombo(ACaption, APrompt, AList, AValue, '', Checked, AReadOnly);
end;

function InputCombo(const ACaption, APrompt: string; AList: TStrings;
  var AValue: string; const ACheckBoxCaption: string; var ACheckBoxChecked: Boolean;
  AReadOnly: Boolean = True): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Combo: TComboBox;
  CheckBox: TCheckBox;
  DialogUnits, PromptSize: TSize;
  CurPrompt, MaxPromptWidth, ButtonTop, ButtonWidth, ButtonHeight: Integer;

begin
  Result := False;

  // Init TForm
  Form := TForm.CreateNew(Application);

  try
    with Form do
    begin
      Canvas.Font := Font;
      GetTextExtentPoint(Canvas.Handle, 'Üy', 2, DialogUnits);
      DialogUnits.cx := DialogUnits.cx div 2;
      GetTextExtentPoint(Canvas.Handle, PChar(APrompt), Length(APrompt), PromptSize);
      MaxPromptWidth := Abs(DialogUnits.cx + PromptSize.cx);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180 + MaxPromptWidth, DialogUnits.cx, 4);
      PopupMode := pmAuto;
      Position := poScreenCenter;
      CurPrompt := MulDiv(8, DialogUnits.cy, 8);

      // Init TLabel
      Prompt := TLabel.Create(Form);

      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.cx, 4);
        Top := CurPrompt;
        Constraints.MaxWidth := MaxPromptWidth;
        WordWrap := True;
      end;

      // Init TComboBox
      Combo := TComboBox.Create(Form);

      with Combo do
      begin
        Parent := Form;

        if AReadOnly then
          Style := csDropDownList
        else
          Style := csDropDown;

        Items.AddStrings(AList);
        ItemIndex := 0;
        Left := Prompt.Left + MaxPromptWidth;
        Top := Prompt.Top;
        Width := Form.ClientWidth - Left - MulDiv(8, DialogUnits.cx, 4);
        MaxLength := 255;
        SelectAll;
        Prompt.FocusControl := Combo;
      end;  //of with

      ButtonTop := Combo.Top + Combo.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.cx, 4);
      ButtonHeight := MulDiv(14, DialogUnits.cy, 8);
      CheckBox := nil;

      // Init TCheckbox
      if (ACheckBoxCaption <> '') then
      begin
        CheckBox := TCheckBox.Create(Form);

        with CheckBox do
        begin
          Parent := Form;
          Caption := ACheckBoxCaption;
          Checked := ACheckBoxChecked;
          Left := Combo.Left;
          Top := Combo.Top + Combo.Height + 5;
          Width := Combo.Width;
        end;  //of with

        ButtonTop := ButtonTop + 15;
      end;  //of begin

      // Init "OK" button
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.cx, 4)) * 2, ButtonTop, ButtonWidth, ButtonHeight);
      end;  //of with

      // Init "Cancel" button
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.cx, 4)), ButtonTop, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;  //of with

      if (ShowModal = mrOk) then
      begin
        AValue := Combo.Text;
        Result := True;

        if Assigned(CheckBox) then
          ACheckBoxChecked := CheckBox.Checked;
      end;  //of begin
    end;  //of with

  finally
    FreeAndNil(Form);
  end;  //of try
end;
{$ENDIF}

end.
