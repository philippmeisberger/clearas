{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Language Handler Unit v2.0                }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWLanguageFile;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Dialogs, IdURI, Menus,
{$IFDEF MSWINDOWS}
  Windows, CommCtrl, System.Generics.Collections, ShellAPI;
{$ELSE}
  LCLType, PMCWIniFileParser;
{$ENDIF}

const
  { Flag indicating line feed }
  NEW_LINE = 1023;

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
    FMenu: TMenuItem;
  {$IFDEF LINUX}
    FLocale, FLangId: WideString;
    FIni: TIniFile;
    FLanguages: TStringList;
  {$ELSE}
    FLocale, FLangId, FInterval: Word;
    FLanguages: TDictionary<Word, Word>;
    procedure OnHyperlinkClicked(Sender: TObject);
  {$ENDIF}
    procedure OnSelectLanguage(Sender: TObject);
  protected
    FListeners: TInterfaceList;
    procedure DoNotify();
  public
    constructor Create(AOwner: TComponent{$IFDEF LINUX}; AIniFile: string = ''{$ENDIF});
    destructor Destroy; override;
    procedure AddListener(AListener: IChangeLanguageListener);
    procedure BuildLanguageMenu(AMainMenu: TMainMenu; AMenuItem: TMenuItem);
    procedure ChangeLanguage(ALocale: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF});
    function Format(const AIndex: Word; const AArgs: array of
      {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF}): string; overload;
    function Format(const AIndexes: array of Word; const AArgs: array of
      {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF}): string; overload;
  {$IFDEF MSWINDOWS}
    function EditBalloonTip(AEditHandle: THandle; ATitle, AText: WideString;
      AIcon: TBalloonIcon = biInfo): Boolean; overload;
    function EditBalloonTip(AEditHandle: THandle; ATitle, AText: Word;
      AIcon: TBalloonIcon = biInfo): Boolean; overload;
    function GetLanguageName(ALanguage: Word): string;
  {$ENDIF}
    function GetString(const AIndex: Word): string; overload;
    function GetString(const AIndexes: array of Word): string; overload;
  {$IFDEF LINUX}
    function GetUserDefaultUILanguage(): string;
  {$ENDIF}
    procedure Load();
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
    procedure Update();
    { external }
    property Id: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF} read FLangId;
  {$IFDEF MSWINDOWS}
    property Interval: Word read FInterval write FInterval;
  {$ENDIF}
    property Locale: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF} read FLocale write ChangeLanguage;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R lang.res}
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
  FInterval := 200;
  FLangId := 0;
{$ELSE}
  FLanguages := TStringList.Create;
  FLanguages.Duplicates := dupIgnore;

  if (AIniFile = '') then
    AIniFile := ExtractFilePath(ParamStr(0)) +'lang';

  FIni := TIniFile.Create(AIniFile);
{$ENDIF}
  FLocale := GetUserDefaultUILanguage();
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
{ private TLanguageFile.OnHyperlinkClicked

  Event that is called when user clicked on hyperlink. }

procedure TLanguageFile.OnHyperlinkClicked(Sender: TObject);
begin
  if (Sender is TTaskDialog) then
    ShellExecute(0, 'open', PChar((Sender as TTaskDialog).URL), nil, nil, SW_SHOWNORMAL);
end;
{$ENDIF}

{ private TLanguageFile.OnSelectLanguage

  Event that is called when user selected a language. }

procedure TLanguageFile.OnSelectLanguage(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ChangeLanguage((Sender as TMenuItem).Tag);
{$ELSE}
  ChangeLanguage((Sender as TMenuItem).Hint);
{$ENDIF}
end;

{ protected TLanguageFile.DoNotify

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

{ public TLanguageFile.AddListener

  Adds a listener to the notification list. }

procedure TLanguageFile.AddListener(AListener: IChangeLanguageListener);
begin
  FListeners.Add(AListener);
end;

{ public TLanguageFile.BuildLanguageMenu

  Builds a select language menu based on available languages. }

procedure TLanguageFile.BuildLanguageMenu(AMainMenu: TMainMenu; AMenuItem: TMenuItem);
var
  MenuItem: TMenuItem;
{$IFDEF MSWINDOWS}
  Language: Word;
{$ELSE}
  i: Integer;
{$ENDIF}

begin
  if (FLangId = {$IFDEF MSWINDOWS}0{$ELSE}''{$ENDIF}) then
    Load();

  FMenu := AMenuItem;

  // Create submenu
{$IFDEF MSWINDOWS}
  for Language in FLanguages.Keys do
{$ELSE}
  for i := 0 to FLanguages.Count - 1 do
{$ENDIF}
  begin
    MenuItem := TMenuItem.Create(AMainMenu);

    with MenuItem do
    begin
      RadioItem := True;
    {$IFDEF MSWINDOWS}
      Tag := Language;
      Caption := GetLanguageName(Language);
      Checked := (FLocale = Language);
    {$ELSE}
      Hint := FLanguages.Names[i];
      Caption := FLanguages.ValueFromIndex[i];
      Checked := (FLocale = Hint);
    {$ENDIF}
      OnClick := OnSelectLanguage;
    end;  //of with

    AMenuItem.Add(MenuItem);
  end;  //of for
end;

{ public TLanguageFile.ChangeLanguage

  Allows users to change the language. }

procedure TLanguageFile.ChangeLanguage(ALocale: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF});
var
  LocaleId: {$IFDEF MSWINDOWS}Word{$ELSE}WideString{$ENDIF};
  i: Integer;

begin
  LocaleId := ALocale;

  // Load default language
{$IFDEF MSWINDOWS}
  if not FLanguages.ContainsKey(LocaleId) then
  begin
    LocaleId := MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT);

    // Language file contains no default language?
    if not FLanguages.ContainsKey(LocaleId) then
      raise ELanguageException.Create('No default language found in language file!');
  end;  //of begin

  FLangId := FLanguages[LocaleId];
{$ELSE}
  if (FLanguages.Values[LocaleId] = '') then
  begin
    LocaleId := 'en_US';

    // Language file contains no default language?
    if (FLanguages.Values[LocaleId] = '') then
      raise ELanguageException.Create('No default language found in language file!');
  end;  //of begin

  FLangId := FLanguages.Values[LocaleId];
{$ENDIF}
  FLocale := LocaleId;

  // Select language visual
  for i := 0 to FMenu.Count - 1 do
  {$IFDEF MSWINDOWS}
    if (FMenu[i].Tag = LocaleId) then
  {$ELSE}
    if (FMenu[i].Hint = LocaleId) then
  {$ENDIF}
    begin
      FMenu[i].Checked := True;
      Break;
    end;  //of begin

  // Notify all listeners
  DoNotify();
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
{$ENDIF}

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

{$IFDEF MSWINDOWS}
{ public TLanguageFile.GetLanguageName

  Returns the name of a language ID. }

function TLanguageFile.GetLanguageName(ALanguage: Word): string;
begin
  SetLength(Result, 255);
  SetLength(Result, VerLanguageName(MAKELANGID(ALanguage, SUBLANG_DEFAULT),
    @Result[1], Length(Result)));
end;

{ public TLanguageFile.GetString

  Loads a single string from a StringTable resource based language file. }

function TLanguageFile.GetString(const AIndex: Word): string;
var
  Buffer: array[0..80] of Char;
  Error: Cardinal;

begin
  if (LoadString(HInstance, FLangId + AIndex, Buffer, SizeOf(Buffer)) = 0) then
  begin
    Error := GetLastError();

    // ERROR_INVALID_WINDOW_HANDLE will be raised on Windows XP
    if ((Error <> ERROR_SUCCESS) and (Error <> ERROR_INVALID_WINDOW_HANDLE)) then
      raise ELanguageException.Create(SysUtils.Format(SysErrorMessage(
        ERROR_RESOURCE_LANG_NOT_FOUND) +'. ID %d (Error %d)', [AIndex, Error]));
  end;  //of begin

  Result := Buffer;
end;
{$ELSE}
{ public TLanguageFile.GetString

  Loads a string from a *.ini file based language file. }

function TLanguageFile.GetString(const AIndex: Word): string;
begin
  Result := FIni.ReadString(FLangId, IntToStr(AIndex + 100));
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

{$IFDEF LINUX}
{ public TLanguageFile.GetUserDefaultUILanguage

  Returns a the current used system language. }

function TLanguageFile.GetUserDefaultUILanguage(): string;
begin
  Result := Copy(SysUtils.GetEnvironmentVariable('LANG'), 1, 5);
end;
{$ENDIF}

{ public TLanguageFile.Load

  Loads available languages from language file. }

procedure TLanguageFile.Load();
{$IFDEF MSWINDOWS}
var
  Language: Word;
  Buffer: array[0..4] of Char;

begin
  Language := 100;

  while (LoadString(HInstance, Language, Buffer, SizeOf(Buffer)) <> 0) do
  begin
    FLanguages.Add(StrToInt(Buffer), Language);
    Inc(Language, FInterval);
  end;  //of while

  // Set language ID
  FLangId := FLanguages[FLocale];
{$ELSE}
var
  Languages: TStringList;
  i: Integer;

begin
  Languages := TStringList.Create;

  try
    FIni.GetSections(Languages);

    for i := 0 to Languages.Count - 1 do
      FLanguages.CommaText := FLanguages.CommaText + FIni.ReadString(Languages[i], '100') +'='+ Languages[i]+',';

    FLanguages.CommaText := Copy(FLanguages.CommaText, 0, Length(FLanguages.CommaText) - 1);

  finally
    Languages.Free;
  end;

  // Set language ID
  FLangId := FLanguages.Values[FLocale];
{$ENDIF}
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
{$IFDEF MSWINDOWS}
  DefaultButton: TMsgDlgBtn;
{$ENDIF}

begin
{$IFDEF MSWINDOWS}
  DefaultButton := mbOK;
{$ENDIF}

  case AMessageType of
    mtInformation:
      begin
        Buttons := [mbOK];
      {$IFDEF MSWINDOWS}
        DefaultButton := mbOK;
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
      {$IFDEF MSWINDOWS}
        DefaultButton := mbYes;
        MessageBeep(MB_ICONWARNING);
      {$ENDIF}
      end;

    mtCustom:
      begin
        Buttons := mbYesNo;
      {$IFDEF MSWINDOWS}
        DefaultButton := mbNo;
      {$ENDIF}
        AMessageType := mtWarning;
      end;

    mtError:
      begin
        Buttons := [mbClose];
      {$IFDEF MSWINDOWS}
        DefaultButton := mbClose;
        MessageBeep(MB_ICONERROR);
      {$ENDIF}
      end;
  end;  //of case

{$IFDEF MSWINDOWS}
  if (Win32MajorVersion >= 6) then
    Result := TaskMessageDlg(ATitle, AText, AMessageType, Buttons, 0, DefaultButton)
  else
    Result := MessageDlg(ATitle + sLineBreak + AText, AMessageType, Buttons, 0);
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
      OnHyperlinkClicked := Self.OnHyperlinkClicked;
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

{ public TLanguageFile.Update

  Uses the current selected language to notify all listeners. }

procedure TLanguageFile.Update;
begin
  ChangeLanguage(FLocale);
end;

end.
