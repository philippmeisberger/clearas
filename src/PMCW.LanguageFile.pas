{ *********************************************************************** }
{                                                                         }
{ PM Code Works Language File Unit v2.3                                   }
{                                                                         }
{ Copyright (c) 2011-2019 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.LanguageFile;

{$IFDEF FPC}{$mode Delphi}{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  Windows, PMCW.SysUtils;
{$ELSE}
  StrUtils, IniFiles;
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
  LID_FILTER_REGISTRY_FILE          = 36;
  LID_DONATE                        = 38;

  { Report bug IDs }
  LID_REPORT_BUG                    = 26;
  LID_REPORT_BUG_BODY               = 20;
  LID_REPORT_SUBMIT                 = 19;
  LID_FATAL_ERROR                   = 31;
  LID_TECHNICAL_DETAILS             = 32;

  { Translate IDs }
  LID_TRANSLATE                     = 34;
  LID_TRANSLATE_SELECT              = 35;
  LID_TRANSLATE_FINISHED            = 39;
  LID_TRANSLATE_SEND                = 40;

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
  LID_UPDATE_SECURE                 = 37;

  { Certificate language IDs }
  LID_CERTIFICATE_INSTALL           = 16;
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
  ///   <c>TLanguageFile</c> is a platform independent UI translator. It uses
  ///   language IDs to unique identify strings inside a <c>StringTable</c>
  ///   resource on Windows or INI file on other platforms. It scans such a
  ///   resource and identifies available languages. A menu for language
  ///   selection can be created using <see cref="BuildLanguageMenu"/>.
  /// </summary>
  TLanguageFile = class(TObject)
  private
    FListeners: TInterfaceList;
    FLocale,
    FSection: TLocale;
    FLanguages: TStringList;
  {$IFDEF MSWINDOWS}
    FInterval: Integer;
  {$ENDIF}
  {$IFDEF LINUX}
    FIni: TIniFile;
  {$ENDIF}
    function GetCount(): Integer;
    procedure SetLocale(const ALocale: TLocale);
    function GetLocale(AIndex: Integer): TLocale;
    function GetName(AIndex: Integer): string;
  protected
  {$IFDEF MSWINDOWS}
    /// <summary>
    ///   Loads available languages from language resource.
    /// </summary>
    /// <param name="AInterval">
    ///   Optional: The application defined interval between languages.
    /// </param>
    /// <exception href="ELanguageException">
    ///   if no language was found.
    /// </exception>
    /// <exception href="EArgumentException">
    ///   if <c>AInterval</c> is <c>0</c>.
    /// </exception>
    procedure Load(const AInterval: Word = 200);
  {$ELSE}
    /// <summary>
    ///   Loads available languages from language resource.
    /// </summary>
    /// <exception href="ELanguageException">
    ///   if no language was found.
    /// </exception>
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
    /// <exception href="ELanguageException">
    ///   if no language was found.
    /// </exception>
    constructor Create(const AInterval: Word = 200);
  {$ELSE}
    /// <summary>
    ///   Constructor for creating a <c>TLanguageFile</c> instance.
    /// </summary>
    /// <param name="AIniFile">
    ///   The absolute filename of the language file.
    /// </param>
    /// <exception href="EArgumentException">
    ///   if file could not be found.
    /// </exception>
    /// <exception href="ELanguageException">
    ///   if no language was found.
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
    ///   Loads a single string from the language file.
    /// </summary>
    /// <param name="AIndex">
    ///   The language ID.
    /// </param>
    /// <returns>
    ///   The string.
    /// </returns>
    function GetString(AIndex: TLanguageId): string; overload;

    /// <summary>
    ///   Loads multiple strings from the language file.
    /// </summary>
    /// <param name="AIndices">
    ///   An array containing multiple language IDs.
    /// </param>
    /// <returns>
    ///   The string.
    /// </returns>
    function GetString(const AIndices: array of TLanguageId): string; overload;

    /// <summary>
    ///   Gets the translation for a specified locale.
    /// </summary>
    /// <param name="ALocale">
    ///   The locale.
    /// </param>
    /// <returns>
    ///   The translated locale.
    /// </returns>
    /// <exception href="ELanguageException">
    ///   if no translation was found.
    /// </exception>
    function GetTranslation(const ALocale: TLocale): TLocale;

    /// <summary>
    ///   Checks if a translation is available for a specified locale.
    /// </summary>
    /// <param name="ALocale">
    ///   The locale to check.
    /// </param>
    /// <returns>
    ///   <c>True</c> if translation is available or <c>False</c> otherwise.
    /// </returns>
    function IsTranslated(const ALocale: TLocale): Boolean; inline;

    /// <summary>
    ///   Removes a listener from the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IChangeLanguageListener"/> interface.
    /// </param>
    procedure RemoveListener(AListener: IChangeLanguageListener);

    /// <summary>
    ///   Gets the number of supported languages.
    /// </summary>
    property Count: Integer read GetCount;
  {$IFDEF MSWINDOWS}
    /// <summary>
    ///   Gets the interval between languages.
    /// </summary>
    property Interval: Integer read FInterval;
  {$ENDIF}
    /// <summary>
    ///   Gets or sets the current used locale for UI translation.
    /// </summary>
    property Locale: TLocale read FLocale write SetLocale;

    /// <summary>
    ///   Gets the locale ID of a translation.
    /// </summary>
    property Locales[AIndex: Integer]: TLocale read GetLocale;

    /// <summary>
    ///   Gets the display name of a translation.
    /// </summary>
    property Names[AIndex: Integer]: string read GetName;

    /// <summary>
    ///   Loads a single string from the language file.
    /// </summary>
    property Strings[AIndex: TLanguageId]: string read GetString; default;
  end;

implementation

{$IFDEF MSWINDOWS}
{$IFDEF FPC}
{$R languages.rc}
function GetUserDefaultUILanguage(): LANGID; stdcall; external kernel32 name 'GetUserDefaultUILanguage';
{$ELSE}
{$R languages.res}
{$ENDIF}
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
  FSection := 0;
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

procedure TLanguageFile.AddListener(AListener: IChangeLanguageListener);
begin
  if Assigned(AListener) then
  begin
    FListeners.Add(AListener);
    AListener.LanguageChanged();
  end;  //of begin
end;

function TLanguageFile.Format(const AIndex: TLanguageId; const AArgs: array of
  {$IFDEF FPC}const{$ELSE}TVarRec{$ENDIF}): string;
begin
  Result := SysUtils.Format(GetString(AIndex), AArgs);
end;

function TLanguageFile.Format(const AIndices: array of TLanguageId;
  const AArgs: array of {$IFDEF FPC}const{$ELSE}TVarRec{$ENDIF}): string;
var
  LanguageId: TLanguageId;

begin
  Result := '';

  for LanguageId in AIndices do
  begin
    if (LanguageId = NEW_LINE) then
      Result := Result + sLineBreak
    else
      Result := Result + Format(LanguageId, AArgs) +' ';
  end;  //of for
end;

function TLanguageFile.GetString(AIndex: TLanguageId): string;
begin
{$IFDEF MSWINDOWS}
  Result := LoadResourceString(FSection + AIndex);
{$ELSE}
  Result := FIni.ReadString(FSection, IntToStr(AIndex + FIRST_LANGUAGE_START_INDEX), '');
{$ENDIF}
end;

function TLanguageFile.GetCount(): Integer;
begin
  Result := FLanguages.Count;
end;

function TLanguageFile.GetLocale(AIndex: Integer): TLocale;
begin
{$IFDEF MSWINDOWS}
  Result := StrToInt(FLanguages.Names[AIndex]);
{$ELSE}
  Result := FLanguages.Names[AIndex];
{$ENDIF}
end;

function TLanguageFile.GetName(AIndex: Integer): string;
{$IFDEF MSWINDOWS}
var
  CopiedChars: DWORD;
{$ENDIF}

begin
{$IFDEF MSWINDOWS}
  SetLength(Result, 80);
  CopiedChars := VerLanguageName(MAKELANGID(Locales[AIndex], SUBLANG_DEFAULT),
    PChar(Result), Length(Result));
  SetLength(Result, CopiedChars);
{$ELSE}
  Result := FLanguages.ValueFromIndex[AIndex];
{$ENDIF}
end;

function TLanguageFile.GetString(const AIndices: array of TLanguageId): string;
var
  LanguageId: TLanguageId;

begin
  Result := '';

  for LanguageId in AIndices do
  begin
    if (LanguageId = NEW_LINE) then
      Result := Result + sLineBreak
    else
      Result := Result + GetString(LanguageId) +' ';
  end;  //of for
end;

function TLanguageFile.GetTranslation(const ALocale: TLocale): TLocale;
{$IFNDEF MSWINDOWS}
var
  MatchFound: Boolean;
  i: Integer;
{$ENDIF}

begin
  Result := ALocale;

  if not IsTranslated(Result) then
  begin
  {$IFDEF MSWINDOWS}
    // Try primary language
    Result := MAKELANGID(PRIMARYLANGID(ALocale), SUBLANG_DEFAULT);

    if not IsTranslated(Result) then
  {$ELSE}
    MatchFound := False;

    // Try primary language
    for i := 0 to FLanguages.Count - 1 do
    begin
      Result := Locale[i];

      if AnsiStartsText(Copy(ALocale, 1, 3), Result) then
      begin
        MatchFound := True;
        Break;
      end;  //of begin
    end;  //of for

    if not MatchFound then
  {$ENDIF}
    begin
      // English as fallback
      Result := {$IFDEF MSWINDOWS}MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT){$ELSE}'en_US'{$ENDIF};

      // Default language not found?
      if not IsTranslated(Result) then
        raise ELanguageException.Create('No default language found in language file!');
    end;  //of begin
  end;  //of begin
end;

function TLanguageFile.IsTranslated(const ALocale: TLocale): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (FLanguages.Values[IntToStr(ALocale)] <> '');
{$ELSE}
  Result := (FLanguages.Values[ALocale] <> '');
{$ENDIF}
end;

procedure TLanguageFile.Load({$IFDEF MSWINDOWS}const AInterval: Word = 200{$ENDIF});
{$IFDEF MSWINDOWS}
var
  Language: TLanguageId;
  Buffer: array[0..4] of Char;

begin
  if (AInterval = 0) then
    raise EArgumentException.Create('Invalid interval!');

  FInterval := AInterval;
  FLanguages.Clear();
  Language := FIRST_LANGUAGE_START_INDEX;

  // Load available languages
  while (LoadString(HInstance, Language, Buffer, Length(Buffer)) <> 0) do
  begin
    FLanguages.Append(string(Buffer) + FLanguages.NameValueSeparator + IntToStr(Language));
    Inc(Language, AInterval);
  end;  //of while
{$ELSE}
var
  i: Integer;

begin
  FLanguages.Clear();
  FIni.ReadSections(FLanguages);

  // Load available languages
  for i := 0 to FLanguages.Count - 1 do
    FLanguages[i] := FIni.ReadString(FLanguages[i], IntToStr(FIRST_LANGUAGE_START_INDEX), '') +'='+ FLanguages[i];
{$ENDIF}

  // Try to set user prefered language
  SetLocale(GetUserDefaultUILanguage());
end;

procedure TLanguageFile.RemoveListener(AListener: IChangeLanguageListener);
begin
  if Assigned(AListener) then
    FListeners.Remove(AListener);
end;

procedure TLanguageFile.SetLocale(const ALocale: TLocale);
var
  Listener: IChangeLanguageListener;
  i: Integer;

begin
  if (FLocale <> ALocale) then
  begin
    FLocale := GetTranslation(ALocale);
  {$IFDEF MSWINDOWS}
    FSection := StrToInt(FLanguages.Values[IntToStr(FLocale)]);
  {$ELSE}
    FSection := FLanguages.Values[FLocale];
  {$ENDIF}

    // Notify all listeners
    for i := 0 to FListeners.Count - 1 do
    begin
      if Supports(FListeners[i], IChangeLanguageListener, Listener) then
        Listener.LanguageChanged();
    end;  //of for
  end;  //of begin
end;

end.
