{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Language Handler Unit v1.5                }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.LanguageFile;

{$IFDEF LINUX} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  Classes, SysUtils, Forms,
{$IFDEF MSWINDOWS}
  Windows, Dialogs, System.Generics.Collections;
{$ELSE}
  IniFileParser, LCLType;
{$ENDIF}

const
  { Flag indicating line feed }
  NEW_LINE     = 1023;

  { Flag to load user language }
  LANG_USER    = 0;

{$IFDEF LINUX}
  LANG_GERMAN  = $07;
  LANG_ENGLISH = $09;
  LANG_FRENCH  = $0c;
{$ENDIF}

type
  { Exception class }
  ELanguageException = class(Exception);

  { IChangeLanguageListener }
  IChangeLanguageListener = interface
  ['{FF4AAD19-49DC-403B-8EA0-3E24D984B603}']
    procedure SetLanguage(Sender: TObject);
  end;

  { TLanguageFile }
  TLanguageFile = class(TObject)
  private
  {$IFDEF LINUX}
    FLang: string;
    FIni: TIniFile;
    FHashmap: TDictionary<Word, string>;
  {$ELSE}
    FLang: Word;
    FHashmap: TDictionary<Word, Word>;
  {$ENDIF}
    FApplication: TApplication;
  protected
    FListeners: TInterfaceList;
  public
  {$IFDEF MSWINDOWS}
    constructor Create(AOwner: TComponent; AApplication: TApplication);
  {$ELSE}
    constructor Create(ALanguage: string; AConfig: string = '';
      AApplication: TApplication = nil);
  {$ENDIF}
    destructor Destroy; override;
    procedure AddListener(AListener: IChangeLanguageListener);
    procedure AddLanguage(APrimaryLanguage: Word;
      ALanguageIndex: {$IFDEF MSWINDOWS}Word{$ELSE}string{$ENDIF});
    procedure ChangeLanguage(APrimaryLanguage: Word);
    function Format(const AIndex: Word; const AArgs: array of
      {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF}): string; overload;
    function Format(const AIndexes: array of Word; const AArgs: array of
      {$IFDEF MSWINDOWS}TVarRec{$ELSE}const{$ENDIF}): string; overload;
  {$IFDEF LINUX}
    procedure GetLanguages(ASections: TStrings);
  {$ENDIF}
    function GetString(const AIndex: Word): string; overload;
    function GetString(const AIndexes: array of Word): string; overload;
    procedure RemoveLanguage(ALocale: Word);
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
    procedure ShowException(AText, AInformation: string;
      AOptions: TTaskDialogFlags = []);
    { external }
    property Lang: {$IFDEF LINUX}string{$ELSE}Word{$ENDIF} read FLang write FLang;
  end;

implementation

{ TLanguageFile }

{ public TLanguageFile.Create

  Constructor for creating a TLanguageFile instance. }

{$IFDEF MSWINDOWS}
{$R 'lang.res' 'lang.rc'}

constructor TLanguageFile.Create(AOwner: TComponent; AApplication: TApplication);
begin
  inherited Create;
  FApplication := AApplication;
  FListeners := TInterfaceList.Create;
  FListeners.Add(AOwner);
  FHashmap := TDictionary<Word, Word>.Create;
end;

{ public TLanguageFile.Destroy

  Destructor for destroying a TLanguageFile instance. }

destructor TLanguageFile.Destroy;
begin
  FHashmap.Free;
  FreeAndNil(FListeners);
  inherited Destroy;
end;
{$ENDIF}

{$IFDEF LINUX}
constructor TLanguageFile.Create(ALanguage: string; AConfig: string = '';
  AApplication: TApplication = nil);
begin
  if (AConfig = '') then
    AConfig := ExtractFilePath(ParamStr(0)) +'lang';

  if not FileExists(AConfig) then
    raise ELanguageException.Create('"'+ AConfig +'" not found!');

  FLang := ALanguage;
  FIni := TIniFile.Create(AConfig);
  FApplication := AApplication;
  FListeners := TInterfaceList.Create;
  FHashmap := TDictionary<Word, string>.Create;
end;

{ public TLanguageFile.Destroy

  Destructor for destroying a TLanguageFile instance. }

destructor TLanguageFile.Destroy;
begin
  FIni.Free;
  FHashmap.Free;
  FreeAndNil(FListeners);
  inherited Destroy;
end;

{ public TLanguageFile.GetString

  Loads a string from a *.ini file based language file. }

function TLanguageFile.GetString(const AIndex: Word): string;
begin
  Result := FIni.ReadString(FLang, IntToStr(AIndex + LANGUAGE_INTERVAL));
end;

{ public TLanguageFile.GetLanguages

  Returns a list containing all available languages. }

procedure TLanguageFile.GetLanguages(ASections: TStrings);
begin
  FIni.GetSections(ASections);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

{ public TLanguageFile.GetString

  Loads a single string from a StringTable file based language file. }

function TLanguageFile.GetString(const AIndex: Word): string;
var
  Buffer: array[0..80] of Char;

begin
  if (LoadString(HInstance, FLang + AIndex, Buffer, SizeOf(Buffer)) = 0) then
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

{ public TLanguageFile.AddListener

  Adds a language to the list. }

procedure TLanguageFile.AddLanguage(APrimaryLanguage: Word;
  ALanguageIndex: {$IFDEF MSWINDOWS}Word{$ELSE}string{$ENDIF});
begin
  FHashmap.Add(MAKELANGID(APrimaryLanguage, SUBLANG_DEFAULT), ALanguageIndex);
end;

{ public TLanguageFile.ChangeLanguage

  Allows users to change the language. }

procedure TLanguageFile.ChangeLanguage(APrimaryLanguage: Word);
var
  Locale, i: Word;
  Listener: IChangeLanguageListener;

begin
  // Get user language
  if (APrimaryLanguage = LANG_USER) then
    Locale := GetSystemDefaultLCID()
  else
    Locale := MAKELANGID(APrimaryLanguage, SUBLANG_DEFAULT);

  // Load default language
  if not FHashmap.ContainsKey(Locale) then
  begin
    Locale := MAKELANGID(LANG_GERMAN, SUBLANG_DEFAULT);

    // Language file contains no default language?
    if not FHashmap.ContainsKey(Locale) then
      raise ELanguageException.Create('No languages not found in language file!');
  end;

  FLang := FHashmap[Locale];

  // Notify all listeners
  for i := 0 to FListeners.Count - 1 do
    if Supports(FListeners[i], IChangeLanguageListener, Listener) then
      Listener.SetLanguage(Self);
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
        MessageBeep(MB_ICONINFORMATION);
      end;

    mtWarning:
      begin
        Buttons := [mbOK];
        MessageBeep(MB_ICONWARNING);
      end;

    mtConfirmation:
      begin
        Buttons := mbYesNo;
        DefaultButton := mbYes;
        MessageBeep(MB_ICONWARNING);
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
        MessageBeep(MB_ICONERROR);
      end;
  end;  //of case

  Result := TaskMessageDlg(ATitle, AText, AMessageType, Buttons, 0, DefaultButton);
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

procedure TLanguageFile.ShowException(AText, AInformation: string;
  AOptions: TTaskDialogFlags = []);
{$IFDEF MSWINDOWS}

  function EncodeUri(const AText: string): string;
  begin
    Result := StringReplace(AText, '!', '%21', [rfReplaceAll]);
    Result := StringReplace(Result, ' ', '%20', [rfReplaceAll]);
  end;

var
  TaskDialog: TTaskDialog;
  MailSubject, MailBody: string;

{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // TaskDialogIndirect only possible for Windows >= Vista!
  if (Win32MajorVersion < 6) then
  begin
    ShowMessage(GetString(31) +': '+ AText + sLineBreak + AInformation, mtError);
    Exit;
  end;  //of begin

  TaskDialog := TTaskDialog.Create(FApplication.MainForm);

  try
    with TaskDialog do
    begin
      Title := FApplication.Name;
      MainIcon := tdiError;
      Caption := GetString(31);
      Text := AText;
      ExpandedText := AInformation;
      ExpandButtonCaption := GetString(33);
      //CollapsedControlText := GetString(32);
      MailSubject := EncodeUri('Bug Report '+ FApplication.Title);
      MailBody := EncodeUri('Dear PM Code Works,%0A%0AI found a possible '+
        'bug:%0A'+ AInformation);
      FooterText := '<a href="mailto:team@pm-codeworks.de?subject='+ MailSubject +
        '&body='+ MailBody +'">'+ GetString(26) +'</a>';
      Flags := [tfExpandFooterArea, tfEnableHyperlinks] + AOptions;
      CommonButtons := [tcbClose];
    end;  //of with

    MessageBeep(MB_ICONERROR);

    if not TaskDialog.Execute() then
      ShowMessage(GetString(31) +': '+ AText + sLineBreak + AInformation, mtError);
    
  finally
    TaskDialog.Free;
  end;  //of try
{$ELSE}
  Result := ShowMessage(GetString(31) +': '+ AContent + sLineBreak + AInformation,
    mtError);
{$ENDIF}
end;

{ public TLanguageFile.RemoveLanguage

  Removes a language from the list. }

procedure TLanguageFile.RemoveLanguage(ALocale: Word);
begin
  FHashmap.Remove(ALocale);
end;

{ public TLanguageFile.RemoveListener

  Removes a listener from the notification list. }

procedure TLanguageFile.RemoveListener(AListener: IChangeLanguageListener);
begin
  FListeners.Remove(AListener);
end;

end.
