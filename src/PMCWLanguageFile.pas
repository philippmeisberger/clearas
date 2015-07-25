{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Language Handler Unit v2.0                }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWLanguageFile;

{$IFDEF LINUX} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Dialogs, IdURI,
{$IFDEF MSWINDOWS}
  Windows, CommCtrl, System.Generics.Collections, ShellAPI;
{$ELSE}
  LCLType, StringHashList, PMCWIniFileParser;
{$ENDIF}

const
  { Flag indicating line feed }
  NEW_LINE     = 1023;

{$IFDEF MSWINDOWS}
  { Flag to load user language }
  LANG_USER    = 0;
{$ELSE}
  LANG_USER    = '';
  LANG_GERMAN  = 'de_DE';
  LANG_ENGLISH = 'en_EN';
  LANG_FRENCH  = 'fr_FR';
{$ENDIF}

type
  { Exception class }
  ELanguageException = class(Exception);

  { IChangeLanguageListener }
  IChangeLanguageListener = interface
  ['{FF4AAD19-49DC-403B-8EA0-3E24D984B603}']
    procedure SetLanguage(Sender: TObject);
  end;

{$IFDEF MSWINDOWS}
  { Balloon tip icon }
  TBalloonIcon = (biNone, biInfo, biWarning, biError, biInfoLarge,
    biWarningLarge, biErrorLarge);
{$ENDIF}

  { TLanguageFile }
  TLanguageFile = class(TObject)
  private
    FOwner: TComponent;
  {$IFDEF LINUX}
    FLocale, FLangId: WideString;
    FIni: TIniFile;
    FLanguages: TStringHashList;
  {$ELSE}
    FLocale, FLangId: Word;
    FLanguages: TDictionary<Word, Word>;
    function GetWindowsLanguage(ALCType: Word = LOCALE_ILANGUAGE): string;
    procedure HyperlinkClicked(Sender: TObject);
  {$ENDIF}
    procedure SetLangId(ALangId: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF});
  protected
    FListeners: TInterfaceList;
    procedure DoNotify();
  public
    constructor Create(AOwner: TComponent{$IFDEF LINUX}; AIniFile: string = ''{$ENDIF});
    destructor Destroy; override;
    procedure AddListener(AListener: IChangeLanguageListener);
    procedure AddLanguage(ALanguage, ALanguageId: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF});
    procedure ChangeLanguage(ALanguage: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF});
    function Format(const AIndex: Word; const AArgs: array of
      {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF}): string; overload;
    function Format(const AIndexes: array of Word; const AArgs: array of
      {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF}): string; overload;
  {$IFDEF MSWINDOWS}
    function EditBalloonTip(AEditHandle: THandle; ATitle, AText: WideString;
      AIcon: TBalloonIcon = biInfo): Boolean; overload;
    function EditBalloonTip(AEditHandle: THandle; ATitle, AText: Word;
      AIcon: TBalloonIcon = biInfo): Boolean; overload;
  {$ELSE}
    procedure GetLanguages(ASections: TStrings);
  {$ENDIF}
    function GetString(const AIndex: Word): string; overload;
    function GetString(const AIndexes: array of Word): string; overload;
  {$IFDEF LINUX}
    function GetUserDefaultLCID(): string;
  {$ENDIF}
    procedure RemoveLanguage(ALocale: {$IFDEF MSWINDOWS}Word{$ELSE}string{$ENDIF});
    procedure RemoveListener(AListener: IChangeLanguageListener);
    function ShowMessage(AText: string;
      AMessageType: TMsgDlgType = mtInformation): Integer; overload;
    function ShowMessage(ATitle, AText: string;
      AMessageType: TMsgDlgType = mtInformation): Integer; overload;
    function ShowMessage(ATitle, AText: Word;
      AMessageType: TMsgDlgType = mtInformation): Integer; overload;
    function ShowMessage(ATitle: Word; AIndexes: array of Word;
      AMessageType: TMsgDlgType = mtInformation): Integer; overload;
    function ShowMessage(ATitle: Word; AIndexes: array of Word;
      AArgs: array of {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF};
      AMessageType: TMsgDlgType = mtInformation): Integer; overload;
    procedure ShowException(AText, AInformation: string{$IFDEF MSWINDOWS};
      AOptions: TTaskDialogFlags = []{$ENDIF});
    { external }
    property Id: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF} read FLangId write SetLangId;
    property Locale: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF} read FLocale;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R 'lang.res' 'lang.rc'}
{$ENDIF}

{ TLanguageFile }

{ public TLanguageFile.Create

  Constructor for creating a TLanguageFile instance. }

constructor TLanguageFile.Create(AOwner: TComponent{$IFDEF LINUX}; AIniFile: string = ''{$ENDIF});
begin
  inherited Create;
  FOwner := AOwner;
  FListeners := TInterfaceList.Create;
  FListeners.Add(AOwner);
{$IFDEF MSWINDOWS}
  FLanguages := TDictionary<Word, Word>.Create;
{$ELSE}
  FLanguages := TStringHashList.Create(True);

  if (AIniFile = '') then
    AIniFile := ExtractFilePath(ParamStr(0)) +'lang';

  FIni := TIniFile.Create(AIniFile);
{$ENDIF}
  FLocale := LANG_USER;
end;

{ public TLanguageFile.Destroy

  Destructor for destroying a TLanguageFile instance. }

destructor TLanguageFile.Destroy;
begin
{$IFDEF LINUX}
  if Assigned(FIni) then
    FIni.Free;
{$ENDIF}
  FLanguages.Free;
  FreeAndNil(FListeners);
  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
{ private TLanguageFile.GetWindowsLanguage

  Returns language information on Windows. }

function TLanguageFile.GetWindowsLanguage(ALCType: Word = LOCALE_ILANGUAGE): string;
var
  Buffer: PChar;
  Size: Integer;

begin
  Size := GetLocaleInfo(LOCALE_USER_DEFAULT, ALCType, nil, 0);
  GetMem(Buffer, Size);

  try
    GetLocaleInfo(LOCALE_USER_DEFAULT, ALCType, Buffer, Size);
    Result := string(Buffer);

  finally
    FreeMem(Buffer);
  end;  //of try
end;

{ private TLanguageFile.HyperlinkClicked

  Event that is called when user clicked on hyperlink. }

procedure TLanguageFile.HyperlinkClicked(Sender: TObject);
begin
  if (Sender is TTaskDialog) then
    ShellExecute(0, 'open', PChar((Sender as TTaskDialog).URL), nil, nil, SW_SHOWNORMAL);
end;

{$ELSE}
{ public TLanguageFile.GetString

  Loads a string from a *.ini file based language file. }

function TLanguageFile.GetString(const AIndex: Word): string;
begin
  Result := FIni.ReadString(FLangId, IntToStr(AIndex + 100));
end;

{ public TLanguageFile.GetLanguages

  Returns a list containing all available languages. }

procedure TLanguageFile.GetLanguages(ASections: TStrings);
begin
  FIni.GetSections(ASections);
end;
{$ENDIF}

{ private TLanguageFile.SetLangId

  Setter for the language identifier property }

procedure TLanguageFile.SetLangId(ALangId: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF});
begin
  FLangId := ALangId;
  DoNotify();
end;

{ private TLanguageFile.DoNotify

  Notifies all registered listeners. }

procedure TLanguageFile.DoNotify();
var
  i: Word;
  Listener: IChangeLanguageListener;

begin
  for i := 0 to FListeners.Count - 1 do
    if Supports(FListeners[i], IChangeLanguageListener, Listener) then
      Listener.SetLanguage(Self);
end;

{$IFDEF MSWINDOWS}
{ public TLanguageFile.EditBalloonTip

  Shows a balloon tip inside an edit field with more comfortable usage. }

function TLanguageFile.EditBalloonTip(AEditHandle: THandle; ATitle, AText: WideString;
  AIcon: TBalloonIcon = biInfo): Boolean;
var
  BalloonTip: TEditBalloonTip;

begin
  FillChar(BalloonTip, SizeOf(BalloonTip), 0);

  with BalloonTip do
  begin
    cbStruct := SizeOf(BalloonTip);
    pszTitle := PChar(ATitle);
    pszText := PChar(AText);
    ttiIcon := Ord(AIcon);
  end;  //of with

  Result := Edit_ShowBalloonTip(AEditHandle, BalloonTip);
end;

function TLanguageFile.EditBalloonTip(AEditHandle: THandle; ATitle, AText: Word;
  AIcon: TBalloonIcon): Boolean;
begin
  Result := EditBalloonTip(AEditHandle, GetString(ATitle), GetString(AText), AIcon);
end;

{ public TLanguageFile.GetString

  Loads a single string from a StringTable file based language file. }

function TLanguageFile.GetString(const AIndex: Word): string;
var
  Buffer: array[0..80] of Char;

begin
  if (LoadString(HInstance, FLangId + AIndex, Buffer, SizeOf(Buffer)) = 0) then
    if (GetLastError() <> 0) then
      raise ELanguageException.Create(SysUtils.Format(SysErrorMessage(
        ERROR_RESOURCE_LANG_NOT_FOUND) +'. ID %d', [AIndex]));

  Result := Buffer;
end;
{$ENDIF}

{ public TLanguageFile.GetString

  Loads multiple strings from a StringTable file based language file. }

function TLanguageFile.GetString(const AIndexes: array of Word): string;
var
  i: Word;
  Text: string;

begin
  for i := 0 to Length(AIndexes) -1 do
    if (AIndexes[i] = NEW_LINE) then
      Text := Text + sLineBreak
    else
      Text := Text + GetString(AIndexes[i]);

  Result := Text;
end;

{ public TLanguageFile.AddListener

  Adds a listener to the notification list. }

procedure TLanguageFile.AddListener(AListener: IChangeLanguageListener);
begin
  FListeners.Add(AListener);
end;

{ public TLanguageFile.AddLanguage

  Adds a language to the list. }

procedure TLanguageFile.AddLanguage(ALanguage, ALanguageId: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF});
begin
{$IFDEF MSWINDOWS}
  FLanguages.Add(MAKELANGID(ALanguage, SUBLANG_DEFAULT), ALanguageId);
{$ELSE}
  FLanguages.Add(ALanguage, Pointer(ALanguageId));
{$ENDIF}
end;

{ public TLanguageFile.ChangeLanguage

  Allows users to change the language. }

procedure TLanguageFile.ChangeLanguage(ALanguage: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF});
var
  LocaleId: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF};

begin
  // Get user language
  if (ALanguage = LANG_USER) then
    LocaleId := GetUserDefaultLCID()
  else
  {$IFDEF MSWINDOWS}
    LocaleId := MAKELANGID(ALanguage, SUBLANG_DEFAULT);
  {$ELSE}
    LocaleId := ALanguage;
  {$ENDIF}

  // Load default language
{$IFDEF MSWINDOWS}
  if not FLanguages.ContainsKey(LocaleId) then
  begin
    LocaleId := MAKELANGID(LANG_GERMAN, SUBLANG_DEFAULT);

    // Language file contains no default language?
    if not FLanguages.ContainsKey(LocaleId) then
      raise ELanguageException.Create('No languages not found in language file!');
  end;  //of begin

  FLangId := FLanguages[LocaleId];
{$ELSE}
  if (FLanguages.Find(LocaleId) = -1) then
  begin
    LocaleId := LANG_GERMAN;

    // Language file contains no default language?
    if (FLanguages.Find(LocaleId) = -1) then
      raise ELanguageException.Create('No languages not found in language file!');
  end;  //of begin

  FLangId := WideString(FLanguages[LocaleId]);
{$ENDIF}
  FLocale := LocaleId;

  // Notify all listeners
  DoNotify();
end;

{ public TLanguageFile.Format

  Embeds data into a single string by replacing a special flag starting with %. }

function TLanguageFile.Format(const AIndex: Word; const AArgs: array of
  {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF}): string;
begin
  Result := SysUtils.Format(GetString(AIndex), AArgs);
end;

{ public TLanguageFile.Format

  Embeds data into a multiple strings by replacing a special flag starting with %. }

function TLanguageFile.Format(const AIndexes: array of Word;
  const AArgs: array of {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF}): string;
var
  i: Word;
  Text: string;

begin
  for i := 0 to Length(AIndexes) -1 do
    if (AIndexes[i] = NEW_LINE) then
      Text := Text + sLineBreak
    else
      Text := Text + Format(AIndexes[i], AArgs);

  Result := Text;
end;

{$IFDEF LINUX}
{ public TLanguageFile.GetUserDefaultLCID

  Returns a the current used system language. }

function TLanguageFile.GetUserDefaultLCID(): string;
begin
  Result := Copy(SysUtils.GetEnvironmentVariable('LANG'), 1, 5);
end;
{$ENDIF}

{ public TLanguageFile.RemoveLanguage

  Removes a language from the list. }

procedure TLanguageFile.RemoveLanguage(ALocale: {$IFDEF MSWINDOWS}Word{$ELSE}string{$ENDIF});
begin
  FLanguages.Remove(ALocale);
end;

{ public TLanguageFile.RemoveListener

  Removes a listener from the notification list. }

procedure TLanguageFile.RemoveListener(AListener: IChangeLanguageListener);
begin
  FListeners.Remove(AListener);
end;

{ public TLanguageFile.ShowMessage

  Shows a message with text and specific look. }

function TLanguageFile.ShowMessage(AText: string;
  AMessageType: TMsgDlgType = mtInformation): Integer;
begin
  Result := ShowMessage('', AText, AMessageType);
end;

{ public TLanguageFile.ShowMessage

  Shows a message with text and specific look. }

function TLanguageFile.ShowMessage(ATitle, AText: string;
  AMessageType: TMsgDlgType = mtInformation): Integer;
var
  Buttons: TMsgDlgButtons;
  DefaultButton: TMsgDlgBtn;

begin
  DefaultButton := mbOK;

  case AMessageType of
    mtInformation:
      begin
        Buttons := [mbOK];
        DefaultButton := mbOK;
      {$IFDEF MSWINDOWS}
        MessageBeep(MB_ICONINFORMATION);
      {$ENDIF}
      end;

    mtWarning:
      begin
        Buttons := [mbOK];
      {$IFDEF MSWINDOWS}
        MessageBeep(MB_ICONWARNING);
      {$ENDIF}
      end;

    mtConfirmation:
      begin
        Buttons := mbYesNo;
        DefaultButton := mbYes;
      {$IFDEF MSWINDOWS}
        MessageBeep(MB_ICONWARNING);
      {$ENDIF}
      end;

    mtCustom:
      begin
        Buttons := mbYesNo;
        DefaultButton := mbNo;
        AMessageType := mtWarning;
      end;

    mtError:
      begin
        Buttons := [mbClose];
        DefaultButton := mbClose;
      {$IFDEF MSWINDOWS}
        MessageBeep(MB_ICONERROR);
      {$ENDIF}
      end;
  end;  //of case

{$IFDEF MSWINDOWS}
  Result := TaskMessageDlg(ATitle, AText, AMessageType, Buttons, 0, DefaultButton);
{$ELSE}
  if (ATitle <> '') then
    Result := MessageDlg(ATitle + sLineBreak + AText, AMessageType, Buttons, 0)
  else
    Result := MessageDlg(AText, AMessageType, Buttons, 0);
{$ENDIF}
end;

{ public TLanguageFile.ShowMessage

  Shows a message with text and specific look. }

function TLanguageFile.ShowMessage(ATitle, AText: Word;
  AMessageType: TMsgDlgType = mtInformation): Integer;
begin
  Result := ShowMessage(GetString(ATitle), GetString(AText), AMessageType);
end;

{ public TLanguageFile.ShowMessage

  Shows a message with multiple string text and specific look. }

function TLanguageFile.ShowMessage(ATitle: Word; AIndexes: array of Word;
  AMessageType: TMsgDlgType = mtInformation): Integer;
begin
  Result := ShowMessage(GetString(ATitle), GetString(AIndexes), AMessageType);
end;

{ public TLanguageFile.ShowMessage

  Shows a message with multiple formatted string text and specific look. }

function TLanguageFile.ShowMessage(ATitle: Word; AIndexes: array of Word;
  AArgs: array of {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF};
  AMessageType: TMsgDlgType = mtInformation): Integer;
begin
  Result := ShowMessage(GetString(ATitle), Format(AIndexes, AArgs), AMessageType);
end;

{ public TLanguageFile.ShowException

  Shows an exception message with additional information. }

procedure TLanguageFile.ShowException(AText, AInformation: string{$IFDEF MSWINDOWS};
  AOptions: TTaskDialogFlags = []{$ENDIF});
{$IFDEF MSWINDOWS}
var
  TaskDialog: TTaskDialog;
  MailSubject, MailBody: string;

begin
  // TaskDialogIndirect only possible for Windows >= Vista!
  if (Win32MajorVersion < 6) then
  begin
    ShowMessage(GetString(31) +': '+ AText + sLineBreak + AInformation, mtError);
    Exit;
  end;  //of begin

  TaskDialog := TTaskDialog.Create(FOwner);

  try
    with TaskDialog do
    begin
      Caption := Application.Title;
      MainIcon := tdiError;
      Title := GetString(31);
      Text := AText;
      ExpandedText := AInformation;
      ExpandButtonCaption := GetString(32);
      MailSubject := TIdURI.ParamsEncode('Bug Report "'+ Application.Title +'"');
      MailBody := TIdURI.ParamsEncode('Dear PM Code Works,'+ sLineBreak + sLineBreak +
        'I found a possible bug:'+ sLineBreak + AText +' '+ AInformation);
      FooterText := '<a href="mailto:team@pm-codeworks.de?subject='+ MailSubject +
        '&body='+ MailBody +'">'+ GetString(26) +'</a>';
      Flags := [tfExpandFooterArea, tfEnableHyperlinks] + AOptions;
      CommonButtons := [tcbClose];
      OnHyperlinkClicked := HyperlinkClicked;
    end;  //of with

    MessageBeep(MB_ICONERROR);

    if not TaskDialog.Execute() then
      ShowMessage(GetString(31) +': '+ AText + sLineBreak + AInformation, mtError);

  finally
    TaskDialog.Free;
  end;  //of try
{$ELSE}
begin
  ShowMessage(GetString(31) +': '+ AText + sLineBreak + AInformation, mtError);
{$ENDIF}
end;

end.
