{ *********************************************************************** }
{                                                                         }
{ Clearas dialogs                                                         }
{                                                                         }
{ Copyright (c) 2011-2018 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasDialogs;

interface

uses
  Winapi.Windows, System.Classes, System.UITypes, Vcl.Forms, Vcl.StdCtrls,
  Vcl.Consts;

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

implementation

function InputCombo(const ACaption, APrompt: string; AList: TStrings; var AValue: string;
  AReadOnly: Boolean = True): Boolean;
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

  with Form do
    try
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
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.cx, 4)) * 2, ButtonTop, ButtonWidth, ButtonHeight);
      end;  //of with

      // Init "Cancel" button
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
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

    finally
      Form.Free;
    end;  //of try
end;

end.
