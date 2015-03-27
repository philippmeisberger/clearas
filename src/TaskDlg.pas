{ *********************************************************************** }
{                                                                         }
{ PM Code Works Task Dialog Unit v1.0                                     }
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
  TDN_HELP                         = 9;
  TDN_EXPANDO_BUTTON_CLICKED       = 10; // wParam = 0 (dialog is now collapsed); wParam != 0 (dialog is now expanded)

  { TASKDIALOG_MESSAGES }
  TDM_NAVIGATE_PAGE                = WM_USER + 101;
  TDM_CLICK_BUTTON                 = WM_USER + 102; // wParam = Button ID
  TDM_SET_MARQUEE_PROGRESS_BAR     = WM_USER + 103; // wParam = 0 (nonMarque) wParam != 0 (Marquee)
  TDM_SET_PROGRESS_BAR_STATE       = WM_USER + 104; // wParam = new progress state
  TDM_SET_PROGRESS_BAR_RANGE       = WM_USER + 105; // lParam = MAKELPARAM(nMinRange; nMaxRange)
  TDM_SET_PROGRESS_BAR_POS         = WM_USER + 106; // wParam = new position
  TDM_SET_PROGRESS_BAR_MARQUEE     = WM_USER + 107; // wParam = 0 (stop marquee); wParam != 0 (start marquee); lparam = speed (milliseconds between repaints)
  TDM_SET_ELEMENT_TEXT             = WM_USER + 108; // wParam = element (TASKDIALOG_ELEMENTS); lParam = new element text (LPCWSTR)
  TDM_CLICK_RADIO_BUTTON           = WM_USER + 110; // wParam = Radio Button ID
  TDM_ENABLE_BUTTON                = WM_USER + 111; // lParam = 0 (disable); lParam != 0 (enable); wParam = Button ID
  TDM_ENABLE_RADIO_BUTTON          = WM_USER + 112; // lParam = 0 (disable); lParam != 0 (enable); wParam = Radio Button ID
  TDM_CLICK_VERIFICATION           = WM_USER + 113; // wParam = 0 (unchecked); 1 (checked); lParam = 1 (set key focus)
  TDM_UPDATE_ELEMENT_TEXT          = WM_USER + 114; // wParam = element (TASKDIALOG_ELEMENTS); lParam = new element text (LPCWSTR)
  TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE = WM_USER + 115; // wParam = Button ID; lParam = 0 (elevation not required); lParam != 0 (elevation required)
  TDM_UPDATE_ICON                  = WM_USER+116;  // wParam = icon element (TASKDIALOG_ICON_ELEMENTS); lParam = new icon (hIcon if TDF_USE_HICON_* was set; PCWSTR otherwise)

type
  { TASKDIALOG_BUTTON }
  TASKDIALOG_BUTTON = packed record
    nButtonId: Integer;
    pszButtonText: PWideChar;
  end;
  PTaskDialogButton = ^TASKDIALOG_BUTTON;
  TTaskDialogButton = TASKDIALOG_BUTTON;

  { TASKDIALOGCONFIG }
  TASKDIALOGCONFIG = packed record
    cbSize: UINT;
    hwndParent,
    hInstance: HWND;
    dwFlags,
    dwCommonButtons: DWORD;
    pszWindowTitle: PWideChar;
    case Integer of
      0: (hMainIcon: HICON);
      1: (pszMainIcon: PWideChar;
          pszMainInstruction: PWideChar;
          pszContent: PWideChar;
          cButtons: UINT;
          pButtons: PTaskDialogButton;
          iDefaultButton: Integer;
          cRadioButtons: UINT;
          pRadioButtons: PTaskDialogButton;
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

function TaskDialog(hwndParent: HWND; hInstance: LongWord; pszWindowTitle,
  pszMainInstruction, pszContent: PWideChar; dwCommonButtons: DWORD;
  Icon: PWideChar; var pnButton: Integer): HRESULT; stdcall;

function TaskDialogIndirect(ptc: PTaskDialogConfig; pnButton: PInteger;
  pnRadioButton: PInteger; pfVerificationFlagChecked: PBool): HRESULT; stdcall;

function TaskDialogCallback(hwndDlg: HWND; uNotification: UINT; wp: WPARAM;
  lp: LPARAM; dwRefData: PDWORD): HRESULT; stdcall;

const
  { Custom button default values }
  CUSTOM_BUTTON1 = 100;
  CUSTOM_BUTTON2 = CUSTOM_BUTTON1 + 1;
  CUSTOM_BUTTON3 = CUSTOM_BUTTON1 + 2;
  CUSTOM_BUTTON4 = CUSTOM_BUTTON1 + 3;
  CUSTOM_BUTTON5 = CUSTOM_BUTTON1 + 4;
  CUSTOM_BUTTON6 = CUSTOM_BUTTON1 + 5;

  { Radio button default values }
  RADIO_BUTTON1 = 200;
  RADIO_BUTTON2 = RADIO_BUTTON1 + 1;
  RADIO_BUTTON3 = RADIO_BUTTON1 + 2;
  RADIO_BUTTON4 = RADIO_BUTTON1 + 3;
  RADIO_BUTTON5 = RADIO_BUTTON1 + 4;
  RADIO_BUTTON6 = RADIO_BUTTON1 + 5;

  { Common buttons default selection }
  DEFAULT_BUTTON1 = CUSTOM_BUTTON1;
  DEFAULT_BUTTON2 = CUSTOM_BUTTON2;
  DEFAULT_BUTTON3 = CUSTOM_BUTTON3;
  DEFAULT_BUTTON4 = CUSTOM_BUTTON4;
  DEFAULT_BUTTON5 = CUSTOM_BUTTON5;
  DEFAULT_BUTTON6 = CUSTOM_BUTTON6;

type
  { TCommonButton }
  TCommonButton = (cbOk, cbYes, cbNo, cbCancel, cbRetry, cbClose);
  TCommonButtons = set of TCommonButton;

  { TTaskDialogIcon }
  TTaskDialogIcon = (tiBlank, tiWarning, tiQuestion, tiError, tiInformation,
    tiShield, tiShieldQuestion, tiShieldError, tiShieldOk, tiShieldWarning
  );

  { TTaskDialogOption }
  TTaskDialogOption = (doHyperlinks, doUseMainIcon, doUseFooterIcon,
    doCancel, doCommandLinks, doCommandLinksNoIcon, doExpandFooter,
    doExpandDefault, doVerify, doProgressBar, doProgressBarMarquee, doCallBackTimer,
    doRelativeToWindow, doRtlLayout, doRadioButtonNoDefault, doMinimize);
  TTaskDialogOptions = set of TTaskDialogOption;

  { TTaskDialog }
  TTaskDialog = class(TObject)
  private
    FTitle, FInstruction, FContent, FFooter, FVerificationText: WideString;
    FExpandedInformation, FExpandedControlText, FCollapsedControlText: WideString;
    FButtons: TCommonButtons;
    FIcon, FFooterIcon: TTaskDialogIcon;
    FOptions: TTaskDialogOptions;
    FOwner: THandle;
    FCustomButtons, FRadioButtons: TStringList;
    FModalResult, FRadioButtonResult, FDefaultButton, FDefaultRadioButton: Integer;
    FVerifyResult: Boolean;
  public
    constructor Create(AOwner: THandle);
    destructor Destroy; override;
    function Execute(): Boolean;
    { external }
    property CollapsedControlText: WideString read FCollapsedControlText write FCollapsedControlText;
    property CommonButtons: TCommonButtons read FButtons write FButtons;
    property Content: WideString read FContent write FContent;
    property CustomButtons: TStringList read FCustomButtons write FCustomButtons;
    property DefaultButton: Integer read FDefaultButton write FDefaultButton;
    property DefaultRadioButton: Integer read FDefaultRadioButton write FDefaultRadioButton;
    property ExpandedControlText: WideString read FExpandedControlText write FExpandedControlText;
    property ExpandedInformation: WideString read FExpandedInformation write FExpandedInformation;
    property Footer: WideString read FFooter write FFooter;
    property FooterIcon: TTaskDialogIcon read FFooterIcon write FFooterIcon;
    property Icon: TTaskDialogIcon read FIcon write FIcon;
    property Instruction: WideString read FInstruction write FInstruction;
    property ModalResult: Integer read FModalResult;
    property Options: TTaskDialogOptions read FOptions write FOptions;
    property Owner: THandle read FOwner;
    property RadioButtons: TStringList read FRadioButtons write FRadioButtons;
    property RadioButtonResult: Integer read FRadioButtonResult;
    property Title: WideString read FTitle write FTitle;
    property VerifyResult: Boolean read FVerifyResult;
    property VerificationText: WideString read FVerificationText write FVerificationText;
  end;

const
  { Enumeration to button translator }
  TDCommonButton: array[TCommonButton] of Integer = (
    TDCBF_OK_BUTTON,
    TDCBF_YES_BUTTON,
    TDCBF_NO_BUTTON,
    TDCBF_CANCEL_BUTTON,
    TDCBF_RETRY_BUTTON,
    TDCBF_CLOSE_BUTTON
  );

  { Enumeration to icon translator }
  TDIcon: array[TTaskDialogIcon] of Integer = (
    TD_ICON_BLANK,
    TD_ICON_WARNING,
    TD_ICON_QUESTION,
    TD_ICON_ERROR,
    TD_ICON_INFORMATION,
    TD_ICON_SHIELD,
    TD_ICON_SHIELD_QUESTION,
    TD_ICON_SHIELD_ERROR,
    TD_ICON_SHIELD_OK,
    TD_ICON_SHIELD_WARNING
  );

  { Enumeration to option translator }
  TDOption: array[TTaskDialogOption] of Integer = (
    TDF_ENABLE_HYPERLINKS,
    TDF_USE_HICON_MAIN,
    TDF_USE_HICON_FOOTER,
    TDF_ALLOW_DIALOG_CANCELLATION,
    TDF_USE_COMMAND_LINKS,
    TDF_USE_COMMAND_LINKS_NO_ICON,
    TDF_EXPAND_FOOTER_AREA,
    TDF_EXPANDED_BY_DEFAULT,
    TDF_VERIFICATION_FLAG_CHECKED,
    TDF_SHOW_PROGRESS_BAR,
    TDF_SHOW_MARQUEE_PROGRESS_BAR,
    TDF_CALLBACK_TIMER,
    TDF_POSITION_RELATIVE_TO_WINDOW,
    TDF_RTL_LAYOUT,
    TDF_NO_DEFAULT_RADIO_BUTTON,
    TDF_CAN_BE_MINIMIZED
  );

function ShowTaskDialog(AHandle: THandle; ATitle, AInstruction, AContent: WideString;
  ACommonButtons: TCommonButtons; AIcon: TTaskDialogIcon): Integer;

procedure ShowException(AHandle: THandle; AInstruction, AContent,
  AInformation: WideString; AOptions: TTaskDialogOptions = [doExpandFooter]);

implementation

function TaskDialog;         external comctl32 name 'TaskDialog';
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

{ ShowTaskDialog

  Shows the new task dialog of Windows Vista. }

function ShowTaskDialog(AHandle: THandle; ATitle, AInstruction, AContent: WideString;
  ACommonButtons: TCommonButtons; AIcon: TTaskDialogIcon): Integer;
var
  TaskDialog: TTaskDialog;

begin
  Result := -1;
  TaskDialog := TTaskDialog.Create(AHandle);

  try
    with TaskDialog do
    begin
      Title := ATitle;
      Instruction := AInstruction;
      Content := AContent;
      CommonButtons := ACommonButtons;
      Icon := AIcon;
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

procedure ShowException(AHandle: THandle; AInstruction, AContent,
  AInformation: WideString; AOptions: TTaskDialogOptions = [doExpandFooter]);
var
  TaskDialog: TTaskDialog;

begin
  TaskDialog := TTaskDialog.Create(AHandle);

  try
    with TaskDialog do
    begin
      Instruction := AInstruction;
      Content := AContent;
      CommonButtons := [cbClose];
      Icon := tiError;
      ExpandedInformation := AInformation;
      Options := AOptions;
    end;  //of with

    if not TaskDialog.Execute() then
      raise Exception.Create(AInstruction +': '+ AContent + sLinebreak + AInformation);

  finally
    TaskDialog.Free;
  end;  //of try
end;


{ TTaskDialog }

{ public TTaskDialog.Create

  Constructor for creating a TTaskDialog instance. }

constructor TTaskDialog.Create(AOwner: THandle);
begin
  inherited Create;
  FOwner := AOwner;
  FCustomButtons := TStringList.Create;
  FRadioButtons := TStringList.Create;
end;

{ public TTaskDialog.Destroy

  Destructor for destroying a TTaskDialog instance. }

destructor TTaskDialog.Destroy;
begin
  FRadioButtons.Free;
  FCustomButtons.Free;
  inherited Destroy;
end;

{ public TTaskDialog.Execute

  Executes a configured TaskDialog. }

function TTaskDialog.Execute(): Boolean;
var
  Dialog: TTaskDialogConfig;
  Option: TTaskDialogOption;
  Button: TCommonButton;
  i: Integer;
  CustomButtons, RadioButtons: array of TTaskDialogButton;

begin
  ZeroMemory(@Dialog, SizeOf(Dialog));

  with Dialog do
  begin
    cbSize := SizeOf(Dialog);
    hwndParent := FOwner;
    hInstance := 0;

    // Setup icons
    pszMainIcon := MAKEINTRESOURCEW(TDIcon[FIcon]);
    pszFooterIcon := MAKEINTRESOURCEW(TDIcon[FFooterIcon]);

    // Setup options
    for Option := doHyperlinks to doMinimize do
      if (Option in FOptions) then
        dwFlags := dwFlags + Cardinal(TDOption[Option]);

    // Setup custom buttons
    if (FCustomButtons.Count > 0) then
    begin
      SetLength(CustomButtons, FCustomButtons.Count);

      for i := 0 to FCustomButtons.Count - 1 do
        with CustomButtons[i] do
        begin
          nButtonId := 100 + i;
          pszButtonText := StringToOleStr(FCustomButtons[i]);
        end;  //of with

      cButtons := Length(CustomButtons);
      pButtons := @CustomButtons[0];
    end  //of begin
    else
      // Setup common buttons
      for Button := cbOk to cbClose do
        if (Button in FButtons) then
          dwCommonButtons := dwCommonButtons + Cardinal(TDCommonButton[Button]);

    iDefaultButton := FDefaultButton;

    // Setup radio buttons
    if (FRadioButtons.Count > 0) then
    begin
      SetLength(RadioButtons, FRadioButtons.Count);

      for i := 0 to FRadioButtons.Count - 1 do
        with RadioButtons[i] do
        begin
          nButtonId := 200 + i;
          pszButtonText := StringToOleStr(FRadioButtons[i]);
        end;  //of with

      cRadioButtons := Length(RadioButtons);
      pRadioButtons := @RadioButtons[0];
      iDefaultRadioButton := FDefaultRadioButton;
    end;  //of begin

    // Enable clickable hyperlink
    if (doHyperLinks in FOptions) then
      pfCallback := @TaskDialogCallback;

    // Setup text
    pszWindowTitle := PWideChar(FTitle);
    pszMainInstruction := PWideChar(FInstruction);
    pszContent := PWideChar(FContent);
    pszExpandedInformation := PWideChar(FExpandedInformation);
    pszExpandedControlText := PWideChar(FExpandedControlText);
    pszCollapsedControlText := PWideChar(FCollapsedControlText);
    pszVerificationText := PWideChar(FVerificationText);
    pszFooterText := PWideChar(FFooter);
  end;  //of with

  Result := Succeeded(TaskDialogIndirect(@Dialog, @FModalResult,
    @FRadioButtonResult, @FVerifyResult));
end;

end.
