{ *********************************************************************** }
{                                                                         }
{ PM Code Works Task Dialog Unit                                          }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit TaskDlg;

interface

uses
  Windows, Classes, SysUtils, Graphics, Messages, ShellAPI;

const
  { TASKDIALOG_ICONS }
  TD_ICON_BLANK                    = 0;
  TD_ICON_WARNING                  = 84;
  TD_ICON_QUESTION                 = 99;
  TD_ICON_ERROR                    = 98;
  TD_ICON_INFORMATION              = 81;
  TD_ICON_SHIELD                   = 78;
  TD_ICON_SHIELD_QUESTION          = 104;
  TD_ICON_SHIELD_ERROR             = 105;
  TD_ICON_SHIELD_OK                = 106;
  TD_ICON_SHIELD_WARNING           = 107;

  { Common buttons }
  TDCBF_OK_BUTTON                  = $0001;
  TDCBF_YES_BUTTON                 = $0002;
  TDCBF_NO_BUTTON                  = $0004;
  TDCBF_CANCEL_BUTTON              = $0008;
  TDCBF_RETRY_BUTTON               = $0010;
  TDCBF_CLOSE_BUTTON               = $0020;

  { TASKDIALOG_FLAGS }
  TDF_ENABLE_HYPERLINKS            = $0001;
  TDF_USE_HICON_MAIN               = $0002;
  TDF_USE_HICON_FOOTER             = $0004;
  TDF_ALLOW_DIALOG_CANCELLATION    = $0008;
  TDF_USE_COMMAND_LINKS            = $0010;
  TDF_USE_COMMAND_LINKS_NO_ICON    = $0020;
  TDF_EXPAND_FOOTER_AREA           = $0040;
  TDF_EXPANDED_BY_DEFAULT          = $0080;
  TDF_VERIFICATION_FLAG_CHECKED    = $0100;
  TDF_SHOW_PROGRESS_BAR            = $0200;
  TDF_SHOW_MARQUEE_PROGRESS_BAR    = $0400;
  TDF_CALLBACK_TIMER               = $0800;
  TDF_POSITION_RELATIVE_TO_WINDOW  = $1000;
  TDF_RTL_LAYOUT                   = $2000;
  TDF_NO_DEFAULT_RADIO_BUTTON      = $4000;
  TDF_CAN_BE_MINIMIZED             = $8000;

  { TASKDIALOG_NOTIFICATIONS }
  TDN_CREATED                      = 0;
  TDN_NAVIGATED                    = 1;
  TDN_BUTTON_CLICKED               = 2;  // wParam = Button ID
  TDN_HYPERLINK_CLICKED            = 3;  // lParam = (LPCWSTR)pszHREF
  TDN_TIMER                        = 4;  // wParam = Milliseconds since dialog created or timer reset
  TDN_DESTROYED                    = 5;
  TDN_RADIO_BUTTON_CLICKED         = 6;  // wParam = Radio Button ID
  TDN_DIALOG_CONSTRUCTED           = 7;
  TDN_VERIFICATION_CLICKED         = 8;  // wParam = 1 if checkbox checked; 0 if not; lParam is unused and always 0
  TDN_HELP                          = 9;
  TDN_EXPANDO_BUTTON_CLICKED       = 10; // wParam = 0 (dialog is now collapsed); wParam != 0 (dialog is now expanded)

  { TASKDIALOG_MESSAGES }
  TDM_NAVIGATE_PAGE                = WM_USER+101;
  TDM_CLICK_BUTTON                 = WM_USER+102; // wParam = Button ID
  TDM_SET_MARQUEE_PROGRESS_BAR     = WM_USER+103; // wParam = 0 (nonMarque) wParam != 0 (Marquee)
  TDM_SET_PROGRESS_BAR_STATE       = WM_USER+104; // wParam = new progress state
  TDM_SET_PROGRESS_BAR_RANGE       = WM_USER+105; // lParam = MAKELPARAM(nMinRange; nMaxRange)
  TDM_SET_PROGRESS_BAR_POS         = WM_USER+106; // wParam = new position
  TDM_SET_PROGRESS_BAR_MARQUEE     = WM_USER+107; // wParam = 0 (stop marquee); wParam != 0 (start marquee); lparam = speed (milliseconds between repaints)
  TDM_SET_ELEMENT_TEXT             = WM_USER+108; // wParam = element (TASKDIALOG_ELEMENTS); lParam = new element text (LPCWSTR)
  TDM_CLICK_RADIO_BUTTON           = WM_USER+110; // wParam = Radio Button ID
  TDM_ENABLE_BUTTON                = WM_USER+111; // lParam = 0 (disable); lParam != 0 (enable); wParam = Button ID
  TDM_ENABLE_RADIO_BUTTON          = WM_USER+112; // lParam = 0 (disable); lParam != 0 (enable); wParam = Radio Button ID
  TDM_CLICK_VERIFICATION           = WM_USER+113; // wParam = 0 (unchecked); 1 (checked); lParam = 1 (set key focus)
  TDM_UPDATE_ELEMENT_TEXT          = WM_USER+114; // wParam = element (TASKDIALOG_ELEMENTS); lParam = new element text (LPCWSTR)
  TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE = WM_USER+115; // wParam = Button ID; lParam = 0 (elevation not required); lParam != 0 (elevation required)
  TDM_UPDATE_ICON                  = WM_USER+116;  // wParam = icon element (TASKDIALOG_ICON_ELEMENTS); lParam = new icon (hIcon if TDF_USE_HICON_* was set; PCWSTR otherwise)

type
  PTASKDIALOG_BUTTON = ^TTASKDIALOG_BUTTON; 
  TTASKDIALOG_BUTTON = record
    nButtonID: Integer;
    pszButtonText: PWideChar;
  end;

  TASKDIALOGCONFIG = packed record
    cbSize: UINT;
    hwndParent: HWND;
    hInstance: LongWord;
    dwFlags: DWORD;
    dwCommonButtons: DWORD;
    pszWindowTitle: PWideChar;
    case Integer of
      0: (hMainIcon: HICON);
      1: (pszMainIcon: PWideChar;
          pszMainInstruction: PWideChar;
          pszContent: PWideChar;
          cButtons: UINT;
          pButtons: Pointer;
          iDefaultButton: Integer;
          cRadioButtons: UINT;
          pRadioButtons: Pointer;
          iDefaultRadioButton: Integer;
          pszVerificationText,
          pszExpandedInformation,
          pszExpandedControlText,
          pszCollapsedControlText: PWideChar;
          case Integer of
            0: (hFooterIcon: HICON);
            1: (pszFooterIcon: PWideChar;
                pszFooterText: PWideChar;
                pfCallback: Pointer;
                lpCallbackData: Pointer;
                cxWidth: UINT;));
  end;

  PTaskDialogConfig = ^TASKDIALOGCONFIG;
  TTaskDialogConfig = TASKDIALOGCONFIG;

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

function TaskDialogCallback(hwndDlg: HWND; uNotification: UINT; wp: WPARAM;
  lp: LPARAM; dwRefData: PDWORD): HRESULT; stdcall;

implementation

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

{ TaskDialogCallback

  Receives notification events from TaskDialog. }

function TaskDialogCallback(hwndDlg: HWND; uNotification: UINT; wp: WPARAM;
  lp: LPARAM; dwRefData: PDWORD): HRESULT;
begin
  Result := S_OK;

  case uNotification of
    TDN_HYPERLINK_CLICKED:
      ShellExecuteW(0, 'open', PWideChar(lp), nil, nil, SW_SHOWNORMAL);
  end;  //of case
end;

end.
