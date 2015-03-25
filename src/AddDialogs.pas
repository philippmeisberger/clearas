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
  Windows, Classes, Forms, SysUtils, StdCtrls, Controls, Graphics;

const
  TD_ICON_BLANK = 17;
  TD_ICON_WARNING = 84;
  TD_ICON_QUESTION = 99;
  TD_ICON_ERROR = 98;
  TD_ICON_INFORMATION = 81;
  TD_ICON_BLANK_AGAIN = 0;
  TD_ICON_SHIELD = 78;

  // Well, Microsoft did it again, incorrect TD_ICON_xxx values in the SDK
  // and changing values just between last beta2 & RTM... Gotta love them.
  // These values were obtained emperically by the lack of proper documentation
{
  TD_ICON_BLANK       = 100;
  TD_ICON_WARNING     = 101;
  TD_ICON_QUESTION    = 102;
  TD_ICON_ERROR       = 103;
  TD_ICON_INFORMATION = 104;
  TD_ICON_BLANK_AGAIN = 105;
  TD_ICON_SHIELD      = 106;
}
  TDCBF_OK_BUTTON     = $0001;
  TDCBF_YES_BUTTON    = $0002;
  TDCBF_NO_BUTTON     = $0004;
  TDCBF_CANCEL_BUTTON = $0008;
  TDCBF_RETRY_BUTTON  = $0010;
  TDCBF_CLOSE_BUTTON  = $0020;

  TDF_ENABLE_HYPERLINKS               = $0001;
  TDF_USE_HICON_MAIN                  = $0002;
  TDF_USE_HICON_FOOTER                = $0004;
  TDF_ALLOW_DIALOG_CANCELLATION       = $0008;
  TDF_USE_COMMAND_LINKS               = $0010;
  TDF_USE_COMMAND_LINKS_NO_ICON       = $0020;
  TDF_EXPAND_FOOTER_AREA              = $0040;
  TDF_EXPANDED_BY_DEFAULT             = $0080;
  TDF_VERIFICATION_FLAG_CHECKED       = $0100;
  TDF_SHOW_PROGRESS_BAR               = $0200;
  TDF_SHOW_MARQUEE_PROGRESS_BAR       = $0400;
  TDF_CALLBACK_TIMER                  = $0800;
  TDF_POSITION_RELATIVE_TO_WINDOW     = $1000;
  TDF_RTL_LAYOUT                      = $2000;
  TDF_NO_DEFAULT_RADIO_BUTTON         = $4000;
  TDF_CAN_BE_MINIMIZED                = $8000;

type
  PTASKDIALOG_BUTTON = ^TTASKDIALOG_BUTTON; 
  TTASKDIALOG_BUTTON = record
    nButtonID: Integer;
    pszButtonText: PWideChar;
  end;

  TASKDIALOGCONFIG = packed record
    cbSize : uint;
    hwndParent : HWND;
    hInstance : longword;
    dwFlags : dword;
    dwCommonButtons : dword;
    pszWindowTitle : PWideChar;
    case integer of
      0 : (hMainIcon : HICON);
      1 : (pszMainIcon : PWideChar;
           pszMainInstruction : PWideChar;
           pszContent : PWideChar;
           cButtons : uint;
           pButtons : pointer;
           iDefaultButton : integer;
           cRadioButtons : uint;
           pRadioButtons : pointer;
           iDefaultRadioButton : integer;
           pszVerificationText,
           pszExpandedInformation,
           pszExpandedControlText,
           pszCollapsedControlText : PWideChar;
           case integer of
             0 : (hFooterIcon : HICON);
             1 : (pszFooterIcon : PWideChar;
                  pszFooterText : PWideChar;
                  pfCallback : pointer;
                  lpCallbackData : pointer;
                  cxWidth : uint;));
  end;

  PTaskDialogConfig = ^TASKDIALOGCONFIG;
  TTaskDialogConfig = TASKDIALOGCONFIG;

function InputCombo(const ACaption, APrompt: string; const AList: TStrings;
  var AValue: string): Boolean;

function ShowTaskDialog(AHandle: HWND; ATitle, ADescription, AContent: string;
  AButtons, AIcon: Integer): Integer;

function ShowTaskDialogIndirect(AHandle: HWND; ATitle, ADescription,
  AContent: string; AButtons, AIcon, AFlags: Integer): Integer;

function ShowException(AHandle: HWND; ATitle, ADescription, AContent,
  AInformation, AExpandText: string; AFlags: Integer = 0): Integer;

function TaskDialog(hwndParent: HWND; hInstance: LongWord; pszWindowTitle,
  pszMainInstruction, pszContent: PWideChar; dwCommonButtons: DWORD;
  Icon: PWideChar; var pnButton: Integer): HRESULT; stdcall;

function TaskDialogIndirect(ptc: PTaskDialogConfig; pnButton: PInteger;
  pnRadioButton: PInteger; pfVerificationFlagChecked: PBool): HRESULT; stdcall;

function TDICallbackProc(hwndDlg: HWND; uNotification: UINT; wp: WPARAM;
  lp: LPARAM; dwRefData: PDWORD): HRESULT; stdcall;

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

{ ShowTaskDialog

  Shows the new task dialog of Windows Vista. }

function ShowTaskDialog(AHandle: HWND; ATitle, ADescription, AContent: string;
  AButtons, AIcon: Integer): Integer;
begin
  if Failed(TaskDialog(AHandle, 0, StringToOleStr(ATitle),
    StringToOleStr(ADescription), StringToOleStr(AContent), AButtons,
    MAKEINTRESOURCEW(AIcon), Result)) then
    raise Exception.Create(SysErrorMessage(GetLastError()));
end;

{ ShowTaskDialogIndirect

  Shows the new task dialog indirect of Windows Vista. }

function ShowTaskDialogIndirect(AHandle: HWND; ATitle, ADescription,
  AContent: string; AButtons, AIcon, AFlags: Integer): Integer;
var
  tc: TASKDIALOGCONFIG;

begin
  ZeroMemory(@tc, SizeOf(tc));
  tc.cbSize := SizeOf(tc);
  tc.hwndParent := AHandle;
  tc.hInstance := 0;
  tc.dwFlags := AFlags;
  tc.dwCommonButtons := AButtons;
  tc.pszWindowTitle := StringToOleStr(ATitle);
  tc.pszMainIcon := MAKEINTRESOURCEW(AIcon);
  tc.pszMainInstruction := StringToOleStr(ADescription);
  tc.pszContent := StringToOleStr(AContent);
  TaskDialogIndirect(@tc, @Result, nil, nil);
end;

{ ShowException

  Shows an exception with additional information. }

function ShowException(AHandle: HWND; ATitle, ADescription, AContent,
  AInformation, AExpandText: string; AFlags: Integer = 0): Integer;
var
  tc: TASKDIALOGCONFIG;

begin
  ZeroMemory(@tc, SizeOf(tc));
  tc.cbSize := SizeOf(tc);
  tc.hwndParent := AHandle;
  tc.hInstance := 0;
  tc.dwFlags := TDF_EXPAND_FOOTER_AREA or AFlags;
  tc.dwCommonButtons := TDCBF_CLOSE_BUTTON;
  tc.pszWindowTitle := StringToOleStr(ATitle);
  tc.pszMainIcon := MAKEINTRESOURCEW(TD_ICON_ERROR);
  tc.pszMainInstruction := StringToOleStr(ADescription);
  tc.pszContent := StringToOleStr(AContent);
  tc.pszExpandedInformation := StringToOleStr(AInformation);
  tc.pszExpandedControlText := StringToOleStr(AExpandText);
  tc.pszCollapsedControlText := StringToOleStr(AExpandText);
  TaskDialogIndirect(@tc, @Result, nil, nil);
end;

function TaskDialog; external comctl32 name 'TaskDialog';
function TaskDialogIndirect; external comctl32 name 'TaskDialogIndirect';
function TDICallbackProc; external comctl32 name 'TDICallbackProc';

function TaskDialogCallbackProc(hWnd: THandle; msg, wParam, lparam: integer; refData: pointer): integer; stdcall; 
var 
  td: TAdvTaskDialog; 
  Pos: integer; 
  State: TTaskDialogProgressState; 
  Res: integer; 
  CanClose: boolean; 
   
begin 
  td := nil; 
  if Assigned(refdata) then 
    td := TAdvTaskDialog(refdata); 
 
  Res := 0; 
 
  if Assigned(td) then 
    td.hWnd := hWnd; 
 
  case msg of 
  TDN_CREATED: 
    begin 
      if Assigned(td) and Assigned(td.OnDialogCreated) then 
      begin 
        td.OnDialogCreated(td); 
 
        if (doProgressBar in td.Options) then 
        begin 
          SendMessage(hWnd, TDM_SET_PROGRESS_BAR_RANGE, 0, MakeLParam(td.ProgressBarMin,td.ProgressBarMax)); 
        end; 
      end; 
    end; 
  TDN_BUTTON_CLICKED: 
    begin 
      if Assigned(td) and Assigned(td.OnDialogButtonClick) then 
      begin 
        td.OnDialogButtonClick(td, wParam); 
      end; 
 
      if Assigned(td) and Assigned(td.OnDialogClose) then 
      begin 
        CanClose := true; 
        td.OnDialogClose(td, CanClose); 
        if not CanClose then 
          Res := 1; 
      end; 
    end; 
  TDN_RADIO_BUTTON_CLICKED: 
    begin 
      if Assigned(td) and Assigned(td.OnDialogRadioClick) then 
      begin 
        td.OnDialogRadioClick(td, wParam); 
      end; 
    end; 
  TDN_HYPERLINK_CLICKED: 
    begin 
      if Assigned(td) and Assigned(td.OnDialogHyperlinkClick) then 
      begin 
        td.OnDialogHyperlinkClick(td, WideCharToString(PWideChar(lparam))); 
      end; 
    end; 
  TDN_VERIFICATION_CLICKED: 
    begin 
      if Assigned(td) and Assigned(td.OnDialogVerifyClick) then 
      begin 
        td.OnDialogVerifyClick(td, bool(wparam)); 
      end; 
    end; 
  TDN_TIMER: 
    begin 
      if Assigned(td) and Assigned(td.OnDialogTimer) then 
      begin 
        td.OnDialogTimer(td); 
      end; 
 
      if Assigned(td) and Assigned(td.OnDialogProgress) then 
      begin 
        td.OnDialogProgress(td, Pos, State); 
        SendMessage(hWnd,TDM_SET_PROGRESS_BAR_POS,Pos,0); 
        case State of 
        psNormal: SendMessage(hWnd,TDM_SET_PROGRESS_BAR_STATE, PBST_NORMAL, 0); 
        psError: SendMessage(hWnd,TDM_SET_PROGRESS_BAR_STATE, PBST_ERROR, 0); 
        psPaused: SendMessage(hWnd,TDM_SET_PROGRESS_BAR_STATE, PBST_PAUSED, 0); 
        end; 
      end; 
    end; 
  end; 
 
  Result := Res; 
end;

end.
