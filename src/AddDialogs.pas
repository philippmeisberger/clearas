{ *********************************************************************** }
{                                                                         }
{ PM Code Works Additional Dialogs Unit                                   }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit AddDialogs;

interface

uses
  Windows, Classes, Forms, SysUtils, StdCtrls, Controls, Graphics, CommCtrl;

const
  { Control messages }
  ICC_STANDARD_CLASSES  = $00004000;
  ICC_LINK_CLASS        = $00008000;

  CCM_SETWINDOWTHEME    = CCM_FIRST + $0b;
  CCM_SETVERSION        = CCM_FIRST + $07;
  CCM_GETVERSION        = CCM_FIRST + $08;

  ECM_FIRST             = $1500;  // Edit control messages
  BCM_FIRST             = $1600;  // Button control messages
  CBM_FIRST             = $1700;  // Combobox control messages

type
  _tagEDITBALLOONTIP = packed record
    cbStruct: DWORD;
    pszTitle,
    pszText : PWideChar;
    ttiIcon : integer;
  end;
  EDITBALLOONTIP  = _tagEDITBALLOONTIP;
  TEditBalloonTip = _tagEDITBALLOONTIP;
  PEditBalloonTip = ^TEditBalloonTip;

const
  EM_SETCUEBANNER   = ECM_FIRST + 1;
  EM_GETCUEBANNER   = ECM_FIRST + 2;
  EM_SHOWBALLOONTIP = ECM_FIRST + 3;
  EM_HIDEBALLOONTIP = ECM_FIRST + 4;

function Edit_ShowBalloonTip(hEdit: HWND; pEditBalloonTip: PEditBalloonTip): BOOL;
function Edit_HideBalloonTip(hEdit: HWND): BOOL;
function Edit_GetCueBannerText(hEdit: HWND; lpcwText: WideString; cchText: LongInt): BOOL;
function Edit_SetCueBannerText(hEdit: HWND; lpcwText: WideString): BOOL;

const
  BCM_SETSHIELD = BCM_FIRST + $000C;

function Button_SetElevationRequiredState(hButton: HWND; fRequired: BOOL = True): BOOL;

function InputCombo(const ACaption, APrompt: string; const AList: TStrings;
  var AValue: string): Boolean;

implementation

{ InputCombo

  Shows a dialog with a pre defined TComboBox list item selection. Similar to
  the InputQuery dialog. }

function InputCombo(const ACaption, APrompt: string; const AList: TStrings;
  var AValue: string): Boolean;
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
  Form := TForm.Create(Application);

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
      ModalResult := mrOk;
      Default := True;
      SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;  //of with

    // Init "Cancel" TButton
    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := 'Cancel';
      ModalResult := mrCancel;
      Cancel := True;
      SetBounds(MulDiv(92, DialogUnits.X, 4), Combo.Top + Combo.Height + 15,
        ButtonWidth, ButtonHeight);
      Form.ClientHeight := Top + Height + 13;
    end;  //of with

    // "OK" clicked?
    if (Form.ShowModal = mrOk) then
    begin
      AValue := Combo.Text;
      Result := True;
    end;  //of begin

  finally
    Form.Free;
  end;  // of try
end;

{ Edit_ShowBalloonTip

  Shows a balloon tip inside an edit field. }

function Edit_ShowBalloonTip(hEdit: HWND; pEditBalloonTip: PEditBalloonTip): BOOL;
begin
  Result := BOOL(SendMessage(hEdit, EM_SHOWBALLOONTIP, 0, LParam(pEditBalloonTip)));
end;

{ Edit_HideBalloonTip

  Hides the balloon tip inside an edit field. }

function Edit_HideBalloonTip(hEdit: HWND): BOOL;
begin
  Result := BOOL(SendMessage(hEdit, EM_HIDEBALLOONTIP, 0, 0));
end;

{ Edit_SetCueBannerText

  Sets a cue text for an edit field. }

function Edit_SetCueBannerText(hEdit: HWND; lpcwText: WideString): BOOL;
begin
  Result := BOOL(SendMessage(hEdit, EM_SETCUEBANNER, 0, LParam(lpcwText)));
end;

{ Edit_SetCueBannerText

  Gets the cue text from an edit field. }

function Edit_GetCueBannerText(hEdit: HWND; lpcwText: WideString; cchText: LongInt): BOOL;
begin
  Result := BOOL(SendMessage(hEdit, EM_GETCUEBANNER, WParam(lpcwText), LParam(cchText)));
end;

{ Button_SetElevationRequiredState

  Adds the Windows UAC shield to a button. }

function Button_SetElevationRequiredState(hButton: HWND; fRequired: BOOL = True): BOOL;
begin
  Result := BOOL(SendMessage(hButton, BCM_SETSHIELD, 0, Ord(fRequired)));
end;

end.
