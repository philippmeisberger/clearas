﻿{ *********************************************************************** }
{                                                                         }
{ PM Code Works Language File Unit v2.3                                   }
{                                                                         }
{ Copyright (c) 2011-2018 Philipp Meisberger (PM Code Works)              }
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

  { Report bug IDs }
  LID_REPORT_BUG                    = 26;
  LID_REPORT_BUG_BODY               = 20;
  LID_REPORT_SUBMIT                 = 19;
  LID_REPORT_SENDING_FAILED         = 38;
  LID_REPORT_MANUAL                 = 39;
  LID_FATAL_ERROR                   = 31;
  LID_TECHNICAL_DETAILS             = 32;

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
  {$IFDEF LINUX}
    FIni: TIniFile;
  {$ENDIF}
    procedure SetLocale(const ALocale: TLocale);
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
    ///   Removes a listener from the notification list.
    /// </summary>
    /// <param name="AListener">
    ///   A listener which implements the <see cref="IChangeLanguageListener"/> interface.
    /// </param>
    procedure RemoveListener(AListener: IChangeLanguageListener);

    /// <summary>
    ///   Gets the available translations.
    /// </summary>
    property Languages: TStringList read FLanguages;

    /// <summary>
    ///   Gets or sets the current used locale for UI translation.
    /// </summary>
    property Locale: TLocale read FLocale write SetLocale;

    /// <summary>
    ///   Loads a single string from the language file.
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

procedure TLanguageFile.Load({$IFDEF MSWINDOWS}const AInterval: Word = 200{$ENDIF});
{$IFDEF MSWINDOWS}
var
  Language: TLanguageId;
  Buffer: array[0..4] of Char;

begin
  if (AInterval = 0) then
    raise EArgumentException.Create('Invalid interval!');

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
  Languages: TStrings;
  i: Integer;

begin
  FLanguages.Clear();
  Languages := TStringList.Create;

  try
    FIni.ReadSections(Languages);

    // Load available languages
    for i := 0 to Languages.Count - 1 do
      FLanguages.Append(FIni.ReadString(Languages[i], IntToStr(FIRST_LANGUAGE_START_INDEX), '') +'='+ Languages[i]);

  finally
    Languages.Free;
  end;  //of try
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
  i: Integer;
  Listener: IChangeLanguageListener;
  Locale: TLocale;
{$IFNDEF MSWINDOWS}
  MatchFound: Boolean;
{$ENDIF}

begin
  if (FLocale <> ALocale) then
  begin
  {$IFDEF MSWINDOWS}
    // Requested language not found?
    if (FLanguages.Values[IntToStr(ALocale)] = '') then
    begin
      // Try primary language
      Locale := MAKELANGID(PRIMARYLANGID(ALocale), SUBLANG_DEFAULT);

      if (FLanguages.Values[IntToStr(Locale)] = '') then
      begin
        // English as fallback
        Locale := MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT);

        // Default language not found?
        if (FLanguages.Values[IntToStr(Locale)] = '') then
          raise ELanguageException.Create('No default language found in language file!');

        FLocale := Locale;
      end;  //of begin
    end  //of begin
    else
      FLocale := ALocale;

    FSection := StrToInt(FLanguages.Values[IntToStr(FLocale)]);
  {$ELSE}
    // Requested language not found?
    if (FLanguages.Values[ALocale] = '') then
    begin
      MatchFound := False;

      // Try primary language
      for i := 0 to FLanguages.Count - 1 do
      begin
        Locale := FIni.ReadString(FLanguages.ValueFromIndex[i], IntToStr(FIRST_LANGUAGE_START_INDEX), '');

        if AnsiStartsText(Copy(ALocale, 1, 3), Locale) then
        begin
          MatchFound := True;
          Break;
        end;  //of begin
      end;  //of for

      if not MatchFound then
      begin
        // English as fallback
        Locale := 'en_US';

        // Default language not found?
        if (FLanguages.Values[Locale] = '') then
          raise ELanguageException.Create('No default language found in language file!');
      end;  //of begin

      FLocale := Locale;
    end  //of begin
    else
      FLocale := ALocale;

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
