{ *********************************************************************** }
{                                                                         }
{ PM Code Works Language Resource Unit v2.2                               }
{                                                                         }
{ Copyright (c) 2011-2017 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.LanguageFile;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes, SysUtils, Menus, Dialogs,
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellAPI, System.NetEncoding, System.UITypes, Vcl.Forms;
{$ELSE}
  IniFiles;
{$ENDIF}

const
  /// <summary>
  ///   Flag indicating line feed.
  /// </summary>
  NEW_LINE                          = 1023;

  /// <summary>
  ///   The absolute start index of the first language.
  /// </summary>
  FIRST_LANGUAGE_START_INDEX        = 100;

  { General language IDs }
  LID_WARNING                       = 1;
  LID_ERROR                         = 2;
  LID_QUESTION                      = 3;
  LID_INFORMATION                   = 4;
  LID_CANCEL                        = 6;
  LID_CANCELED                      = 7;
  LID_FINISHED                      = 8;
  LID_VIEW                          = 10;
  LID_HELP                          = 14;
  LID_ABOUT                         = 17;
  LID_FILE                          = 33;
  LID_IMPOSSIBLE                    = 18;
  LID_SELECT_LANGUAGE               = 25;
  LID_TO_WEBSITE                    = 29;
  LID_REPORT_BUG                    = 26;
  LID_REPORT_BUG_SUBJECT            = 19;
  LID_REPORT_BUG_BODY               = 20;
  LID_FATAL_ERROR                   = 31;
  LID_TECHNICAL_DETAILS             = 32;
  LID_FILTER_REGISTRY_FILE          = 36;

  { Update language IDs }
  LID_UPDATE                        = 5;
  LID_UPDATE_SELECT_DIR             = 9;
  LID_UPDATE_INSTALL                = 11;
  LID_UPDATE_NO_CONNECTION          = 12;
  LID_UPDATE_CHECK_CONNECTION       = 13;
  LID_UPDATE_SEARCH                 = 15;
  LID_UPDATE_AVAILABLE              = 21;
  LID_UPDATE_CONFIRM_DOWNLOAD       = 22;
  LID_UPDATE_DOWNLOAD               = 24;
  LID_UPDATE_NOT_AVAILABLE          = 23;
  LID_UPDATE_CANCELED               = 30;
  LID_UPDATE_64BIT                  = 34;
  LID_UPDATE_64BIT_CONFIRM          = 35;
  LID_UPDATE_SECURE                 = 37;
  LID_UPDATE_SECURE_DESCRIPTION1    = 38;
  LID_UPDATE_SECURE_DESCRIPTION2    = 39;

  { Certificate language IDs }
  LID_CERTIFICATE_INSTALL           = 16;
  LID_CERTIFICATE_INSTALL_CONFIRM   = 40;
  LID_CERTIFICATE_ALREADY_INSTALLED = 27;
  LID_CERTIFICATE_NO_CERTUTIL       = 28;

  /// <summary>
  ///   End mark of the predefined range.
  /// </summary>
  LID_LAST                          = 40;

type
  /// <summary>
  ///   The language exception class.
  /// </summary>
  ELanguageException = class(Exception);

  /// <summary>
  ///   The identifier of a translation string.
  /// </summary>
  TLanguageId = Word;

  /// <summary>
  ///   The language code.
  /// </summary>
  TLocale = {$IFDEF MSWINDOWS}TLanguageId{$ELSE}string{$ENDIF};

{$IFDEF MSWINDOWS}
  TLocaleHelper = record helper for TLocale
    /// <summary>
    ///   Gets the name of the locale.
    /// </summary>
    /// <returns>
    ///   The name.
    /// </returns>
    function DisplayName(): string;
  end;
{$ENDIF}

  /// <summary>
  ///   Receive notfication when user changes the language of the current
  ///   application. Must be implemented by classes that use the
  ///   <see cref="TLanguageFile"/>.
  /// </summary>
  IChangeLanguageListener = interface
  ['{FF4AAD19-49DC-403B-8EA0-3E24D984B603}']
    /// <summary>
    ///   Translates the UI to the requested language.
    /// </summary>
    procedure LanguageChanged();
  end;

  /// <summary>
  ///   The <c>TLanguageFile</c> is a platform independent UI translater. It
  ///   uses language IDs to unique identify strings inside a StringTable resource
  ///   on Windows or INI file on other platforms. It scans such a resource an
  ///   identifies available languages. A menu for language selection can be
  ///   created using <see cref="BuildLanguageMenu"/>.
  /// </summary>
  TLanguageFile = class(TObject)
  private
    FListeners: TInterfaceList;
    FLocale,
    FLangId: TLocale;
    FLanguages: TStringList;
  {$IFDEF LINUX}
    FIni: TIniFile;
  {$ENDIF}
    procedure SetLocale(const ALocale: TLocale);
    procedure LanguageSelected(Sender: TObject);
  {$IFDEF MSWINDOWS}
    procedure HyperlinkClicked(Sender: TObject);
  {$ENDIF}
  protected
  {$IFDEF MSWINDOWS}
    /// <summary>
    ///   Loads available languages from language resource.
    /// </summary>
    /// <param name="AInterval">
    ///   Optional: The application defined interval between languages.
    /// </param>
    procedure Load(const AInterval: Word = 200);
  {$ELSE}
    /// <summary>
    ///   Loads available languages from language resource.
    /// </summary>
    /// <exceptions>
    ///   <c>ELanguageException</c> if no language was found.
    /// </exceptions>
    procedure Load();
  {$ENDIF}
  public
  {$IFDEF MSWINDOWS}
    /// <summary>
    ///   Constructor for creating a <c>TLanguageFile</c> instance.
    /// </summary>
    /// <param name="AInterval">
    ///   Optional: The application defined interval between languages.
    /// </param>
    constructor Create(const AInterval: Word = 200);
  {$ELSE}
    /// <summary>
    ///   Constructor for creating a <c>TLanguageFile</c> instance.
    /// </summary>
    /// <param name="AIniFile">
    ///   The absolute filename of the language file.
    /// </param>
    /// <exception>
    ///   <c>EArgumentException</c> if file could not be found.
    ///   <c>ELanguageException</c> if no language was found.
    /// </exception>
    constructor Create(const AIniFile: TFileName);
  {$ENDIF}
    /// <summary>
    ///   Destructor for destroying a <c>TLanguageFile</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Adds a listener to the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IChangeLanguageListener"/> interface.
    /// </param>
    /// <remarks>
    ///   Listener gets notified after registering.
    /// </remarks>
    procedure AddListener(AListener: IChangeLanguageListener);

    /// <summary>
    ///   Builds a select language menu based on available languages.
    /// </summary>
    /// <param name="AMainMenu">
    ///   The menu item to create the submenu.
    /// </param>
    procedure BuildLanguageMenu(AMenuItem: TMenuItem);

    /// <summary>
    ///   Embeds data into a single string by replacing a special flag starting
    ///   with <c>%</c> using a language ID.
    /// </summary>
    /// <param name="AIndex">
    ///   The language ID.
    /// </param>
    /// <param name="AArgs">
    ///   Arguments passed to <see cref="Format"/>.
    /// </param>
    /// <returns>
    ///   The formatted string.
    /// </returns>
    function Format(const AIndex: TLanguageId; const AArgs: array of
      {$IFDEF FPC}const{$ELSE}TVarRec{$ENDIF}): string; overload;

    /// <summary>
    ///   Embeds data into a multiple strings by replacing a special flag
    ///   starting with <c>%</c> using language IDs.
    /// </summary>
    /// <param name="AIndices">
    ///   An array containing multiple language IDs.
    /// </param>
    /// <param name="AArgs">
    ///   Arguments passed to <see cref="Format"/>.
    /// </param>
    /// <returns>
    ///   The formatted string.
    /// </returns>
    function Format(const AIndices: array of TLanguageId; const AArgs: array of
      {$IFDEF FPC}const{$ELSE}TVarRec{$ENDIF}): string; overload;

    /// <summary>
    ///   Loads a single string from a StringTable resource based language file.
    /// </summary>
    /// <param name="AIndex">
    ///   The language ID.
    /// </param>
    /// <returns>
    ///   The string.
    /// </returns>
    function GetString(AIndex: TLanguageId): string; overload;

    /// <summary>
    ///   Loads multiple strings from a StringTable file based language file.
    /// </summary>
    /// <param name="AIndices">
    ///   An array containing multiple language IDs.
    /// </param>
    /// <returns>
    ///   The string.
    /// </returns>
    function GetString(const AIndices: array of TLanguageId): string; overload;

    /// <summary>
    ///   Removes a listener from the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IChangeLanguageListener"/> interface.
    /// </param>
    procedure RemoveListener(AListener: IChangeLanguageListener);

    /// <summary>
    ///   Shows an exception message with additional information.
    /// </summary>
    /// <param name="AMessage">
    ///   Text containing an error message.
    /// </param>
    /// <param name="ATechnicalDetails">
    ///   Technical error details.
    /// </param>
    procedure ShowException(const AMessage, ATechnicalDetails: string);

    /// <summary>
    ///   Gets or sets the current used locale for UI translation.
    /// </summary>
    property Locale: TLocale read FLocale write SetLocale;

    /// <summary>
    ///   Loads a single string from a StringTable resource based language file.
    /// </summary>
    property Strings[AIndex: TLanguageId]: string read GetString; default;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R languages.res}

{ TLocaleHelper }

function TLocaleHelper.DisplayName(): string;
var
  CopiedChars: DWORD;

begin
  SetLength(Result, 80);
  CopiedChars := VerLanguageName(MAKELANGID(Self, SUBLANG_DEFAULT), PChar(Result),
    Length(Result));
  SetLength(Result, CopiedChars);
end;
{$ELSE}
function GetUserDefaultUILanguage(): string;
begin
  Result := Copy(SysUtils.GetEnvironmentVariable('LANG'), 1, 5);
end;
{$ENDIF}

{ TLanguageFile }

constructor TLanguageFile.Create({$IFDEF LINUX}const AIniFile: TFileName{$ELSE}
  const AInterval: Word = 200{$ENDIF});
begin
  inherited Create;
{$IFDEF MSWINDOWS}
  FLangId := 0;
{$ELSE}
  if not FileExists(AIniFile) then
    raise EArgumentException.Create(SysUtils.Format('Language file "%s" could not be found!', [AIniFile]));

  FIni := TIniFile.Create(AIniFile);
{$ENDIF}
  FListeners := TInterfaceList.Create;
  FLanguages := TStringList.Create;
  FLanguages.Duplicates := dupIgnore;
  Load({$IFDEF MSWINDOWS}AInterval{$ENDIF});
end;

destructor TLanguageFile.Destroy;
begin
  FreeAndNil(FLanguages);
  FreeAndNil(FListeners);
{$IFDEF LINUX}
  FreeAndNil(FIni);
{$ENDIF}
  inherited Destroy;
end;

procedure TLanguageFile.LanguageSelected(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  SetLocale((Sender as TMenuItem).Tag);
{$ELSE}
  SetLocale((Sender as TMenuItem).Hint);
{$ENDIF}
end;

procedure TLanguageFile.AddListener(AListener: IChangeLanguageListener);
begin
  if Assigned(AListener) then
  begin
    FListeners.Add(AListener);
    AListener.LanguageChanged();
  end;  //of begin
end;

procedure TLanguageFile.BuildLanguageMenu(AMenuItem: TMenuItem);
var
  MenuItem: TMenuItem;
  i: Integer;
{$IFDEF MSWINDOWS}
  Locale: TLocale;
{$ENDIF}

begin
  // Create submenu
  for i := 0 to FLanguages.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(AMenuItem.Owner);

    with MenuItem do
    begin
      RadioItem := True;
      AutoCheck := True;
    {$IFDEF MSWINDOWS}
      Locale := StrToInt(FLanguages.Names[i]);
      Tag := Locale;
      Caption := Locale.DisplayName();
      Checked := (FLocale = Tag);
    {$ELSE}
      Caption := FLanguages.ValueFromIndex[i];
      Hint := FLanguages.Names[i];
      Checked := (FLocale = Hint);
    {$ENDIF}
      OnClick := LanguageSelected;
    end;  //of with

    AMenuItem.Add(MenuItem);
  end;  //of for
end;

function TLanguageFile.Format(const AIndex: TLanguageId; const AArgs: array of
  {$IFDEF FPC}const{$ELSE}TVarRec{$ENDIF}): string;
begin
  Result := SysUtils.Format(GetString(AIndex), AArgs);
end;

function TLanguageFile.Format(const AIndices: array of TLanguageId;
  const AArgs: array of {$IFDEF FPC}const{$ELSE}TVarRec{$ENDIF}): string;
var
  i: Integer;
  Text: string;

begin
  Text := '';

  for i := 0 to Length(AIndices) -1 do
  begin
    if (AIndices[i] = NEW_LINE) then
      Text := Text + sLineBreak
    else
      Text := Text + Format(AIndices[i], AArgs);
  end;  //of for

  Result := Text;
end;

function TLanguageFile.GetString(AIndex: TLanguageId): string;
{$IFDEF MSWINDOWS}
var
  Buffer: array[0..79] of Char;
  Error: DWORD;

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
{$ELSE}
begin
  Result := FIni.ReadString(FLangId, IntToStr(AIndex + FIRST_LANGUAGE_START_INDEX), '');
{$ENDIF}
end;

function TLanguageFile.GetString(const AIndices: array of TLanguageId): string;
var
  i: Integer;
  Text: string;

begin
  Text := '';

  for i := 0 to Length(AIndices) - 1 do
  begin
    if (AIndices[i] = NEW_LINE) then
      Text := Text + sLineBreak
    else
      Text := Text + GetString(AIndices[i]);
  end;  //of for

  Result := Text;
end;

procedure TLanguageFile.Load({$IFDEF MSWINDOWS}const AInterval: Word = 200{$ENDIF});
{$IFDEF MSWINDOWS}
var
  Language: TLanguageId;
  Buffer: array[0..4] of Char;

begin
  FLanguages.CommaText := '';
  Language := FIRST_LANGUAGE_START_INDEX;

  // Load available languages
  while (LoadString(HInstance, Language, Buffer, SizeOf(Buffer)) <> 0) do
  begin
    FLanguages.CommaText := FLanguages.CommaText + Buffer +'='+ IntToStr(Language) +',';
    Inc(Language, AInterval);
  end;  //of while

  // Remove last separator
  FLanguages.CommaText := FLanguages.CommaText.Remove(Length(FLanguages.CommaText) - 1);

  // Try to set user prefered language
  SetLocale(GetUserDefaultUILanguage());
{$ELSE}
var
  Languages: TStrings;
  i: Integer;

begin
  FLanguages.CommaText := '';
  Languages := TStringList.Create;

  try
    FIni.ReadSections(Languages);

    // Load available languages
    for i := 0 to Languages.Count - 1 do
    begin
      FLanguages.CommaText := FLanguages.CommaText + FIni.ReadString(Languages[i],
        IntToStr(FIRST_LANGUAGE_START_INDEX), '') +'='+ Languages[i]+',';
    end;  //of for

    // Remove last separator
    FLanguages.CommaText := Copy(FLanguages.CommaText, 0, Length(FLanguages.CommaText) - 1);

  finally
    Languages.Free;
  end;  //of try

  // Try to set user prefered language
  SetLocale(GetUserDefaultUILanguage());
{$ENDIF}
end;

procedure TLanguageFile.RemoveListener(AListener: IChangeLanguageListener);
begin
  if Assigned(AListener) then
    FListeners.Remove(AListener);
end;

procedure TLanguageFile.SetLocale(const ALocale: TLocale);
var
  i: Integer;
  Listener: IChangeLanguageListener;

begin
  if (FLocale <> ALocale) then
  begin
  {$IFDEF MSWINDOWS}
    // Requested language not found?
    if (FLanguages.Values[IntToStr(ALocale)] = '') then
    begin
      // English as fallback
      FLocale := MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT);

      // Default language not found?
      if (FLanguages.Values[IntToStr(FLocale)] = '') then
        raise ELanguageException.Create('No default language found in language file!');
    end  //of begin
    else
      FLocale := ALocale;

    FLangId := StrToInt(FLanguages.Values[IntToStr(FLocale)]);
  {$ELSE}
    // Requested language not found?
    if (FLanguages.Values[ALocale] = '') then
    begin
      // English as fallback
      FLocale := 'en_US';

      // Default language not found?
      if (FLanguages.Values[FLocale] = '') then
        raise ELanguageException.Create('No default language found in language file!');
    end  //of begin
    else
      FLocale := ALocale;

    FLangId := FLanguages.Values[FLocale];
  {$ENDIF}

    // Notify all listeners
    for i := 0 to FListeners.Count - 1 do
    begin
      if Supports(FListeners[i], IChangeLanguageListener, Listener) then
        Listener.LanguageChanged();
    end;  //of for
  end;  //of begin
end;

{$IFDEF MSWINDOWS}
procedure TLanguageFile.HyperlinkClicked(Sender: TObject);
begin
{$WARN SYMBOL_PLATFORM OFF}
  // Try to send the report by mail client
  if (Sender is TTaskDialog) and (ShellExecute(0, 'open',
    PChar((Sender as TTaskDialog).URL), nil, nil, SW_SHOWNORMAL) <= 32) then
    // No mail client installed: Send it by report bug formular on website
    ShellExecute(0, 'open', 'http://www.pm-codeworks.de/kontakt.html', nil, nil, SW_SHOWNORMAL);
{$WARN SYMBOL_PLATFORM ON}
end;
{$ENDIF}

procedure TLanguageFile.ShowException(const AMessage, ATechnicalDetails: string);
{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
const
  URL_MAILTO = '<a href="mailto:%s?subject=%s&body=%s">%s</a>';

var
  TaskDialog: TTaskDialog;
  MailSubject, MailBody: string;

  function URLEncode(const AString: string): string;
  begin
    Result := TNetEncoding.URL.Encode(AString);

    // Embarcadero encodes spaces as '+' which is generally correct but not in
    // mailto hyperlinks: there it must be '%20' to generate a correct mail!
    Result := Result.Replace('+', '%20');
  end;

begin
  // TaskDialog only since Windows Vista
  if (Win32MajorVersion < 6) then
  begin
    MessageDlg(GetString(LID_FATAL_ERROR) +': '+ AMessage + sLineBreak
      + ATechnicalDetails, mtError, [mbClose], 0);
    Exit;
  end;  //of begin

  TaskDialog := TTaskDialog.Create(nil);

  try
    with TaskDialog do
    begin
      Caption := Application.Title;
      MainIcon := tdiError;
      Title := GetString(LID_FATAL_ERROR);
      Text := AMessage;
      ExpandedText := ATechnicalDetails;
      ExpandButtonCaption := GetString(LID_TECHNICAL_DETAILS);
      MailSubject := URLEncode(Format(LID_REPORT_BUG_SUBJECT, [Application.Title]));
      MailBody := URLEncode(Format(LID_REPORT_BUG_BODY, [AMessage, ATechnicalDetails]));
      FooterText := SysUtils.Format(URL_MAILTO, ['team@pm-codeworks.de',
        MailSubject, MailBody, GetString(LID_REPORT_BUG)]);
      Flags := [tfExpandFooterArea, tfEnableHyperlinks];
      CommonButtons := [tcbClose];
      OnHyperlinkClicked := HyperlinkClicked;
    end;  //of with

    MessageBeep(MB_ICONERROR);
    TaskDialog.Execute();

  finally
    TaskDialog.Free;
  end;  //of try
{$WARN SYMBOL_PLATFORM ON}
{$ELSE}
begin
  MessageDlg(GetString(LID_FATAL_ERROR) +': '+ AMessage + sLineBreak
    + ATechnicalDetails, mtError, [mbClose], 0);
{$ENDIF}
end;

end.
