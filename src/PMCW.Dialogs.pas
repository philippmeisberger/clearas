{ *********************************************************************** }
{                                                                         }
{ PM Code Works Additional Dialogs Unit                                   }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, System.UITypes, Vcl.Dialogs, Vcl.Consts;

const
  tdiQuestion            = 99;
  tdiShieldBanner        = 65531;
  tdiShieldWarning       = 107;
  tdiShieldWarningBanner = 65530;
  tdiShieldQuestion      = 104;
  tdiShieldError         = 105;
  tdiShieldErrorBanner   = 65529;
  tdiShieldOk            = 106;
  tdiShieldOkBanner      = 65528;

/// <summary>
///   Shows a dialog with a pre defined TComboBox list item selection. Similar
///   to the InputQuery dialog.
/// </summary>
/// <param name="ACaption">
///   The window caption to use.
/// </param>
/// <param name="APrompt">
///   The prompt to show.
/// </param>
/// <param name="AList">
///   The available items in the TComboBox.
/// </param>
/// <param name="AValue">
///   The value from the TComboBox which was selected by the user. This can also
///   be used as the initial value.
/// </param>
/// <param name="AReadOnly">
///   If set to <c>True</c> the user cannot set a custom value and is forced to
///   choose one of the items in the TComboBox. Otherwise custom values are possible.
/// </param>
/// <returns>
///   <c>True</c> if the user clicks "OK" or <c>False</c> otherwise.
/// </returns>
function InputCombo(const ACaption, APrompt: string; AList: TStrings;
  var AValue: string; AReadOnly: Boolean = True): Boolean; overload;

/// <summary>
///   Shows a dialog with a pre defined TComboBox list item selection. Similar
///   to the InputQuery dialog with optional verification TCheckBox.
/// </summary>
/// <param name="ACaption">
///   The window caption to use.
/// </param>
/// <param name="APrompt">
///   The prompt to show.
/// </param>
/// <param name="AList">
///   The available items in the TComboBox.
/// </param>
/// <param name="AValue">
///   The value from the TComboBox which was selected by the user. This can also
///   be used as the initial value.
/// </param>
/// <param name="ACheckBoxCaption">
///   The caption of the TCheckBox.
/// </param>
/// <param name="ACheckBoxChecked">
///   The value from the TCheckBox which can be selected by the user. This can
///   also be used as the initial value.
/// </param>
/// <param name="AReadOnly">
///   If set to <c>True</c> the user cannot set a custom value and is forced to
///   choose one of the items in the TComboBox. Otherwise custom values are possible.
/// </param>
/// <returns>
///   <c>True</c> if the user clicks "OK" or <c>False</c> otherwise.
/// </returns>
function InputCombo(const ACaption, APrompt: string; AList: TStrings;
  var AValue: string; ACheckBoxCaption: string; var ACheckBoxChecked: Boolean;
  AReadOnly: Boolean = True): Boolean; overload;

/// <summary>
///   Shows the new task dialog of Windows Vista.
/// </summary>
/// <param name="AOwner">
///   The owner of the window.
/// </param>
/// <param name="ACaption">
///   The window caption to use.
/// </param>
/// <param name="ATitle">
///   The title.
/// </param>
/// <param name="AText">
///   The main text.
/// </param>
/// <param name="ACommonButtons">
///   A set of buttons to use.
/// </param>
/// <param name="AIcon">
///   The icon to use.
/// </param>
/// <param name="AFlags">
///   <c>TTaskDialogFlags</c> to use.
/// </param>
/// <returns>
///   The user choice.
/// </returns>
function ShowTaskDialog(AOwner: TComponent; ACaption, ATitle, AText: WideString;
  ACommonButtons: TTaskDialogCommonButtons; AIcon: TTaskDialogIcon;
  AFlags: TTaskDialogFlags = []): Integer;

/// <summary>
///   Shows an exception with additional information.
/// </summary>
/// <param name="AOwner">
///   The owner of the window.
/// </param>
/// <param name="ACaption">
///   The window caption to use.
/// </param>
/// <param name="AText">
///   The main text.
/// </param>
/// <param name="AInformation">
///   Debug information about the error.
/// </param>
/// <param name="AFlags">
///   <c>TTaskDialogFlags</c> to use.
/// </param>
procedure ShowException(AOwner: TComponent; ACaption, AText, AInformation: WideString;
  AFlags: TTaskDialogFlags = [tfExpandFooterArea]);

implementation

uses Math;

function InputCombo(const ACaption, APrompt: string; AList: TStrings; var AValue: string;
  AReadOnly: Boolean = True): Boolean;
var
  Checked: Boolean;

begin
  Result := InputCombo(ACaption, APrompt, AList, AValue, '', Checked, AReadOnly);
end;

function InputCombo(const ACaption, APrompt: string; AList: TStrings;
  var AValue: string; ACheckBoxCaption: string; var ACheckBoxChecked: Boolean;
  AReadOnly: Boolean = True): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Combo: TComboBox;
  CheckBox: TCheckBox;
  DialogUnits: TPoint;
  CurPrompt: Integer;
  MaxPromptWidth: Integer;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;

  function GetPromptCaption(const ACaption: string): string;
  begin
    if (Length(ACaption) > 1) and (ACaption[1] < #32) then
      Result := Copy(ACaption, 2, MaxInt)
    else
      Result := ACaption;
  end;

  function GetMaxPromptWidth(Canvas: TCanvas): Integer;
  var
    LLabel: TLabel;

  begin
    Result := 0;
    // Use a TLabel rather than an API such as GetTextExtentPoint32 to
    // avoid differences in handling characters such as line breaks.
    LLabel := TLabel.Create(nil);
    try
      LLabel.Caption := GetPromptCaption(APrompt);
      Result := Max(Result, LLabel.Width + DialogUnits.X);

    finally
      LLabel.Free;
    end;
  end;

  function GetPasswordChar(const ACaption: string): Char;
  begin
    if (Length(ACaption) > 1) and (ACaption[1] < #32) then
      Result := '*'
    else
      Result := #0;
  end;

  function GetTextBaseline(AControl: TControl; ACanvas: TCanvas): Integer;
  var
    tm: TTextMetric;
    ClientRect: TRect;
    Ascent: Integer;
  begin
    ClientRect := AControl.ClientRect;
    GetTextMetrics(ACanvas.Handle, tm);
    Ascent := tm.tmAscent + 1;
    Result := ClientRect.Top + Ascent;
    Result := AControl.Parent.ScreenToClient(AControl.ClientToScreen(TPoint.Create(0, Result))).Y - AControl.Top;
  end;

  function GetAveCharSize(Canvas: TCanvas): TPoint;
  {$IF DEFINED(CLR)}
  var
    I: Integer;
    Buffer: string;
    Size: TSize;
  begin
    SetLength(Buffer, 52);
    for I := 0 to 25 do Buffer[I + 1] := Chr(I + Ord('A'));
    for I := 0 to 25 do Buffer[I + 27] := Chr(I + Ord('a'));
    GetTextExtentPoint(Canvas.Handle, Buffer, 52, Size);
    Result.X := Size.cx div 52;
    Result.Y := Size.cy;
  end;
  {$ELSE}
  var
    I: Integer;
    Buffer: array[0..51] of Char;
  begin
    for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
    for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
    GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
    Result.X := Result.X div 52;
  end;
  {$ENDIF}

begin
  Result := False;

  // Init TForm
  Form := TForm.CreateNew(Application);

  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      MaxPromptWidth := GetMaxPromptWidth(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180 + MaxPromptWidth, DialogUnits.X, 4);
      PopupMode := pmAuto;
      Position := poScreenCenter;
      CurPrompt := MulDiv(8, DialogUnits.Y, 8);

      // Init TLabel
      Prompt := TLabel.Create(Form);

      with Prompt do
      begin
        Parent := Form;
        Caption := GetPromptCaption(APrompt);
        Left := MulDiv(8, DialogUnits.X, 4);
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
        Top := Prompt.Top + Prompt.Height - DialogUnits.Y -
          (GetTextBaseline(Combo, Canvas) - GetTextBaseline(Prompt, Canvas));
        Width := Form.ClientWidth - Left - MulDiv(8, DialogUnits.X, 4);
        MaxLength := 255;
        SelectAll;
        Prompt.FocusControl := Combo;
      end;  //of with

      ButtonTop := Combo.Top + Combo.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
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
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.X, 4)) * 2, ButtonTop, ButtonWidth, ButtonHeight);
      end;  //of with

      // Init "Cancel" button
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.X, 4)), ButtonTop, ButtonWidth, ButtonHeight);
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

function ShowTaskDialog(AOwner: TComponent; ACaption, ATitle, AText: WideString;
  ACommonButtons: TTaskDialogCommonButtons; AIcon: TTaskDialogIcon;
  AFlags: TTaskDialogFlags = []): Integer;
var
  TaskDialog: TTaskDialog;

begin
  Result := -1;
  TaskDialog := TTaskDialog.Create(AOwner);

  try
    with TaskDialog do
    begin
      Title := ATitle;
      Caption := ACaption;
      Text := AText;
      CommonButtons := ACommonButtons;
      MainIcon := AIcon;
      Flags := AFlags;

      if ((AIcon = tdiWarning) and (tcbNo in ACommonButtons)) then
        DefaultButton := tcbNo;
    end;  //of with

    if not TaskDialog.Execute() then
      raise Exception.Create(SysErrorMessage(GetLastError()));

    Result := TaskDialog.ModalResult;

  finally
    TaskDialog.Free;
  end;  //of try
end;

procedure ShowException(AOwner: TComponent; ACaption, AText, AInformation: WideString;
  AFlags: TTaskDialogFlags = [tfExpandFooterArea]);
var
  TaskDialog: TTaskDialog;

begin
  TaskDialog := TTaskDialog.Create(AOwner);

  try
    with TaskDialog do
    begin
      Caption := ACaption;
      Text := AText;
      CommonButtons := [tcbClose];
      MainIcon := tdiError;
      ExpandedText := AInformation;
      Flags := AFlags;
    end;  //of with

    if not TaskDialog.Execute() then
      raise Exception.Create(ACaption +': '+ AText + sLinebreak + AInformation);

  finally
    TaskDialog.Free;
  end;  //of try
end;

end.
