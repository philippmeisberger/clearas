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
  Windows, Classes, Forms, SysUtils, StdCtrls, Controls, Graphics, CommCtrl, 
  ShellAPI;

const
  { Balloon tip icons }
  TTI_NONE          = 0;
  TTI_INFO          = 1;
  TTI_WARNING       = 2;
  TTI_ERROR         = 3;
  TTI_INFO_LARGE    = 4;
  TTI_WARNING_LARGE = 5;
  TTI_ERROR_LARGE   = 6;

  { Edit control messages }
  ECM_FIRST         = $1500;
  EM_SETCUEBANNER   = ECM_FIRST + 1;
  EM_GETCUEBANNER   = ECM_FIRST + 2;

function Edit_GetCueBannerText(hEdit: HWND; lpcwText: WideString; cchText: LongInt): BOOL;
function Edit_SetCueBannerText(hEdit: HWND; lpcwText: WideString): BOOL;

type
  _tagEDITBALLOONTIP = packed record
    cbStruct: DWORD;
    pszTitle,
    pszText: PWideChar;
    ttiIcon: Integer;
  end;
  EDITBALLOONTIP  = _tagEDITBALLOONTIP;
  TEditBalloonTip = _tagEDITBALLOONTIP;
  PEditBalloonTip = ^TEditBalloonTip;

  { Balloon tip icon }
  TBalloonIcon = (biNone, biInfo, biWarning, biError, biInfoLarge,
    biWarningLarge, biErrorLarge);

const
  EM_SHOWBALLOONTIP = ECM_FIRST + 3;
  EM_HIDEBALLOONTIP = ECM_FIRST + 4;

function Edit_ShowBalloonTip(hEdit: HWND; pEditBalloonTip: PEditBalloonTip): BOOL; overload;
function Edit_ShowBalloonTip(AEditHandle: THandle; ATitle, AText: WideString;
  AIcon: TBalloonIcon = biInfo): BOOL; overload;
function Edit_HideBalloonTip(hEdit: HWND): BOOL;

const
  { Button control messages }
  BCM_FIRST         = $1600;
  BCM_SETSHIELD     = BCM_FIRST + $000C;

function Button_SetElevationRequiredState(hButton: HWND; fRequired: BOOL = True): BOOL;

const
  { Combobox control messages }
  CBM_FIRST             = $1700;

function InputCombo(const ACaption, APrompt: string; const AList: TStrings;
  var AValue: string): Boolean;
function ShowAddRegistryDialog(const ARegFilePath: string): Boolean;


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

{ ShowAddRegistryDialog

  Shows an dialog where user has the choice to add a *.reg file.  }

function ShowAddRegistryDialog(const ARegFilePath: string): Boolean;
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

{ Edit_ShowBalloonTip

  Shows a balloon tip inside an edit field. }

function Edit_ShowBalloonTip(hEdit: HWND; pEditBalloonTip: PEditBalloonTip): BOOL;
begin
  Result := BOOL(SendMessage(hEdit, EM_SHOWBALLOONTIP, 0, LParam(pEditBalloonTip)));
end;

{ Edit_ShowBalloonTip

  Shows a balloon tip inside an edit field with more comfortable usage. }

function Edit_ShowBalloonTip(AEditHandle: THandle; ATitle, AText: WideString;
  AIcon: TBalloonIcon = biInfo): BOOL;
var
  BalloonTip: TEditBalloonTip;

begin
  ZeroMemory(@BalloonTip, SizeOf(BalloonTip));

  with BalloonTip do
  begin
    cbStruct := SizeOf(BalloonTip);
    pszTitle := PWideChar(ATitle);
    pszText := PWideChar(AText);

    case AIcon of
      biNone: ttiIcon := TTI_NONE;
      biInfo: ttiIcon := TTI_INFO;
      biInfoLarge: ttiIcon := TTI_INFO_LARGE;
      biWarning: ttiIcon := TTI_WARNING;
      biWarningLarge: ttiIcon := TTI_WARNING_LARGE;
      biError: ttiIcon := TTI_ERROR;
      biErrorLarge: ttiIcon := TTI_ERROR_LARGE;
    end;
  end;  //of with

  Result := Edit_ShowBalloonTip(AEditHandle, @BalloonTip);
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
