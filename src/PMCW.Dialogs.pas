{ *********************************************************************** }
{                                                                         }
{ PM Code Works Additional Dialogs Unit                                   }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs;

interface

uses
  Classes, SysUtils, Types, Windows, Forms, StdCtrls, Graphics, Dialogs,
  ShellAPI;

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

function InputCombo(AOwner: TComponent; ACaption, APrompt: string; AList: TStrings;
  var AValue: string): Boolean;

function ShowAddRegistryDialog(ARegFilePath: string): Boolean;

function ShowTaskDialog(AOwner: TComponent; ACaption, ATitle, AText: WideString;
  ACommonButtons: TTaskDialogCommonButtons; AIcon: TTaskDialogIcon;
  AFlags: TTaskDialogFlags = []): Integer;

procedure ShowException(AOwner: TComponent; ACaption, AText, AInformation: WideString;
  AFlags: TTaskDialogFlags = [tfExpandFooterArea]);


implementation

{ InputCombo

  Shows a dialog with a pre defined TComboBox list item selection. Similar to
  the InputQuery dialog. }

function InputCombo(AOwner: TComponent; ACaption, APrompt: string;
  AList: TStrings; var AValue: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Combo: TComboBox;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;

  function GetCharSize(Canvas: TCanvas): TPoint;
  var
    i: Integer;
    Buffer: array[0..51] of Char;

  begin
    for i := 0 to 25 do
      Buffer[i] := Chr(i + Ord('A'));

    for i := 0 to 25 do
      Buffer[i + 26] := Chr(i + Ord('a'));

    GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(result));
    result.X := result.X div 52;
  end;

begin
  Result := False;

  // Init TForm
  Form := TForm.Create(AOwner);

  try
    with Form do
    begin
      Canvas.Font := Font;
      DialogUnits := GetCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;
    end;  //of with

    // Init TLabel
    Prompt := TLabel.Create(Form);

    with Prompt do
    begin
      Parent := Form;
      Caption := APrompt;
      Left := MulDiv(8, DialogUnits.X, 4);
      Top := MulDiv(8, DialogUnits.Y, 8);
      Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
      WordWrap := True;
    end;  //of with

    // Init TComboBox
    Combo := TComboBox.Create(Form);

    with Combo do
    begin
      Parent := Form;
      Style := csDropDownList;
      Items.AddStrings(AList);
      ItemIndex := 0;
      Left := Prompt.Left;
      Top := Prompt.Top + Prompt.Height + 5;
      Width := MulDiv(164, DialogUnits.X, 4);
    end;  //of with

    ButtonTop := Combo.Top + Combo.Height + 15;
    ButtonWidth := MulDiv(50, DialogUnits.X, 4);
    ButtonHeight := MulDiv(14, DialogUnits.Y, 8);

    // Init "OK" TButton
    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := 'OK';
      ModalResult := IDOK;
      Default := True;
      SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;  //of with

    // Init "Cancel" TButton
    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := 'Cancel';
      ModalResult := IDCANCEL;
      Cancel := True;
      SetBounds(MulDiv(92, DialogUnits.X, 4), Combo.Top + Combo.Height + 15,
        ButtonWidth, ButtonHeight);
      Form.ClientHeight := Top + Height + 13;
    end;  //of with

    // "OK" clicked?
    if (Form.ShowModal = IDOK) then
    begin
      AValue := Combo.Text;
      Result := True;
    end;  //of begin

  finally
    Form.Free;
  end;  // of try
end;

{ ShowAddRegistryDialog

  Shows an dialog where user has the choice to add a *.reg file.  }

function ShowAddRegistryDialog(ARegFilePath: string): Boolean;
var
  RegFilePath: string;

begin
  if (ARegFilePath = '') then
    raise Exception.Create('Missing parameter with a .reg file!');

  if (ARegFilePath[1] <> '"') then
    RegFilePath := '"'+ ARegFilePath +'"'
  else
    RegFilePath := ARegFilePath;

  Result := BOOL(ShellExecute(0, 'open', PChar('regedit.exe'), PChar(RegFilePath),
    nil, SW_SHOWNORMAL));
end;

{ ShowTaskDialog

  Shows the new task dialog of Windows Vista. }

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

{ ShowException

  Shows an exception with additional information. }

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
