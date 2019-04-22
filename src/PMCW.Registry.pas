{ *********************************************************************** }
{                                                                         }
{ PM Code Works Registry Unit v1.0                                        }
{                                                                         }
{ Copyright (c) 2011-2019 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Registry;

interface

uses
  Winapi.Windows, System.SysUtils, System.Win.Registry, PMCW.SysUtils;

type
  /// <summary>
  ///   The root <c>HKEY</c>s used in the Windows Registry.
  /// </summary>
  TRootKey = (

    /// <summary>
    ///   Unknown <c>HKEY</c>.
    /// </summary>
    rkUnknown,

    /// <summary>
    ///   HKEY_CURRENT_USER
    /// </summary>
    rkHKCU,

    /// <summary>
    ///   HKEY_LOCAL_MACHINE
    /// </summary>
    rkHKLM,

    /// <summary>
    ///   HKEY_CLASSES_ROOT
    /// </summary>
    rkHKCR,

    /// <summary>
    ///   HKEY_USERS
    /// </summary>
    rkHKU,

    /// <summary>
    ///   HKEY_CURRENT_CONFIG
    /// </summary>
    rkHKCC
  );

  TRootKeyHelper = record helper for TRootKey
    /// <summary>
    ///   Converts a <c>HKEY</c> into a <see cref="TRootKey"/>.
    /// </summary>
    /// <param name="AHKey">
    ///   The <c>HKEY</c>.
    /// </param>
    procedure FromHKey(AHKey: HKEY);

    /// <summary>
    ///   Converts a <c>HKEY</c> short string representation into a <see cref="TRootKey"/>.
    /// </summary>
    /// <param name="AShortHKey">
    ///   The <c>HKEY</c> short string representation.
    /// </param>
    procedure FromString(const AShortHKey: string);

    /// <summary>
    ///   Gets the string representation.
    /// </summary>
    /// <param name="ALongFormat">
    ///   If set to <c>True</c> the complete string representaion is returned.
    ///   Otherwise only the four main letters e.g. HKLM are returned.
    /// </param>
    /// <returns>
    ///   The string representation.
    /// </returns>
    function ToString(ALongFormat: Boolean = True): string;

    /// <summary>
    ///   Gets the <c>HKEY</c> representation.
    /// </summary>
    /// <returns>
    ///   The <c>HKEY</c>.
    /// </returns>
    function ToHKey(): HKEY;
  end;

  /// <summary>
  ///   Extends the capabilities of <c>TRegistry</c>.
  /// </summary>
  TRegistryEx = class(TRegistry)
  public
    /// <summary>
    ///   Reads a MUI string and loads its resource string.
    /// </summary>
    /// <param name="AName">
    ///   The value name.
    /// </param>
    /// <returns>
    ///   The resource string.
    /// </returns>
    function ReadMuiString(const AName: string): string;
  end;

  /// <summary>
  ///   Multi-language user interface string.
  /// </summary>
  /// <remarks>
  ///   Has the format <c>@path,-ID;comment</c> where comment is optional e.g.
  ///   <c>@shell32.dll,-8773</c>.
  /// </remarks>
  TMuiString = type string;

  TMuiStringHelper = record helper for TMuiString
    /// <summary>
    ///   Gets the path.
    /// </summary>
    /// <returns>
    ///   The path.
    /// </returns>
    function Path(): string; inline;

    /// <summary>
    ///   Gets the ID.
    /// </summary>
    /// <returns>
    ///   The ID if extracting succeeded or <c>0</c> otherwise.
    /// </returns>
    function Id(): Word; inline;

    /// <summary>
    ///   Gets the comment.
    /// </summary>
    /// <returns>
    ///   The comment.
    /// </returns>
    function Comment(): string; inline;

    /// <summary>
    ///   Decodes the MUI string.
    /// </summary>
    /// <param name="APath">
    ///   The path to a PE.
    /// </param>
    /// <param name="AId">
    ///   The ID of the resource inside the PE.
    /// </param>
    /// <returns>
    ///   <c>True</c> if successful or <c>False</c> otherwise.
    /// </returns>
    function Decode(var APath: string; var AId: Word): Boolean; overload; inline;

    /// <summary>
    ///   Decodes the MUI string.
    /// </summary>
    /// <param name="APath">
    ///   The path to a PE.
    /// </param>
    /// <param name="AId">
    ///   The ID of the resource inside the PE.
    /// </param>
    /// <param name="AComment">
    ///   The comment.
    /// </param>
    /// <returns>
    ///   <c>True</c> if successful or <c>False</c> otherwise.
    /// </returns>
    function Decode(var APath: string; var AId: Word; var AComment: string): Boolean; overload;

    /// <summary>
    ///   Creates a <c>TMuiString</c>.
    /// </summary>
    /// <param name="APath">
    ///   The path to a PE.
    /// </param>
    /// <param name="AId">
    ///   The ID of the resource.
    /// </param>
    /// <param name="AComment">
    ///   Optional: A comment.
    /// </param>
    /// <returns>
    ///   The MUI string.
    /// </returns>
    function Encode(const APath: string; const AId: Word; const AComment: string = ''): TMuiString; inline;
  end;

implementation

{$WARN SYMBOL_PLATFORM OFF}
function RegLoadMUIString(hKey: HKEY; pszValue, pszOutBuf: PWideChar;
  cbOutBuf: DWORD; out pcbData: DWORD; Flags: DWORD; pszDirectory: PWideChar): LONG; stdcall; external advapi32 name 'RegLoadMUIStringW' delayed;
{$WARN SYMBOL_PLATFORM ON}

{ TRootKeyHelper }

const
  cShortHKeys: array[TRootKey] of string = (
    '',
    'HKCU',
    'HKLM',
    'HKCR',
    'HKU',
    'HKCC'
  );

  cLongHKeys: array[TRootKey] of string = (
    '',
    'HKEY_CURRENT_USER',
    'HKEY_LOCAL_MACHINE',
    'HKEY_CLASSES_ROOT',
    'HKEY_USERS',
    'HKEY_CURRENT_CONFIG'
  );

procedure TRootKeyHelper.FromHKey(AHKey: HKEY);
begin
  if (AHKey = HKEY_CURRENT_USER) then
    Self := rkHKCU
  else
  if (AHKey = HKEY_LOCAL_MACHINE) then
    Self := rkHKLM
  else
  if (AHKey = HKEY_CLASSES_ROOT) then
    Self := rkHKCR
  else
  if (AHKey = HKEY_USERS) then
    Self := rkHKU
  else
  if (AHKey = HKEY_CURRENT_CONFIG) then
    Self := rkHKCC
  else
    raise EArgumentException.Create('Unknown HKEY!');
end;

procedure TRootKeyHelper.FromString(const AShortHKey: string);
var
  RootKey, FoundKey: TRootKey;

begin
  FoundKey := rkUnknown;

  for RootKey := Low(cShortHKeys) to High(cShortHKeys) do
    if (cShortHKeys[RootKey] = AShortHKey) then
    begin
      FoundKey := RootKey;
      Break;
    end;  //of begin

  if (FoundKey = rkUnknown) then
    raise EArgumentException.Create('Unknown HKEY: "'+ AShortHKey +'"!');

  Self := FoundKey;
end;

function TRootKeyHelper.ToHKey(): HKEY;
begin
  case Self of
    rkHKCU: Result := HKEY_CURRENT_USER;
    rkHKLM: Result := HKEY_LOCAL_MACHINE;
    rkHKCR: Result := HKEY_CLASSES_ROOT;
    rkHKU:  Result := HKEY_USERS;
    rkHKCC: Result := HKEY_CURRENT_CONFIG;
    else    raise EArgumentException.Create('Unknown HKEY!');
  end;  //of case
end;

function TRootKeyHelper.ToString(ALongFormat: Boolean = True): string;
begin
  if ALongFormat then
    Result := cLongHKeys[Self]
  else
    Result := cShortHKeys[Self];
end;


{ TRegistryEx }

function TRegistryEx.ReadMuiString(const AName: string): string;
var
  Size: DWORD;

begin
  // Get required buffer size
  RegLoadMUIString(CurrentKey, PChar(AName), nil, 0, Size, 0, nil);

  if (Size > 0) then
  begin
    SetString(Result, nil, Size div SizeOf(Char));

    if not CheckResult(RegLoadMUIString(CurrentKey, PChar(AName), PChar(Result), Size, Size, 0, nil)) then
      raise ERegistryException.Create(LastErrorMsg);

    // Remove terminating null
    SetLength(Result, (Size div SizeOf(Char)) - 1);
  end  //of begin
  else
    Result := '';
end;


{ TMuiStringHelper }

function TMuiStringHelper.Comment(): string;
var
  Path: string;
  Id: Word;

begin
  Decode(Path, Id, Result);
end;

function TMuiStringHelper.Id(): Word;
var
  Path: string;

begin
  Decode(Path, Result);
end;

function TMuiStringHelper.Path(): string;
var
  Id: Word;

begin
  Decode(Result, Id);
end;

function TMuiStringHelper.Decode(var APath: string; var AId: Word): Boolean;
var
  Comment: string;

begin
  Result := Decode(APath, AId, Comment);
end;

function TMuiStringHelper.Decode(var APath: string; var AId: Word;
  var AComment: string): Boolean;
var
  Parts: TArray<string>;
  Id: Integer;

begin
  APath := '';
  AId := 0;
  AComment := '';
  Parts := string(Self).Split([',', ';'], 3);

  // Path and ID are required
  if (Length(Parts) < 2)  then
    Exit(False);

  // Extract path
  if Parts[0].StartsWith('@') then
    APath := Parts[0].SubString(1)
  else
    APath := Parts[0];

  // Extract ID
  if not TryStrToInt(Parts[1], Id) then
    Exit(False);

  ExpandEnvironmentVar(APath);
  AId := Abs(Id);

  // Extract optional comment
  if (Length(Parts) = 3) then
    AComment := Parts[2];

  Result := True;
end;

function TMuiStringHelper.Encode(const APath: string; const AId: Word;
  const AComment: string = ''): TMuiString;
const
  FormatWithoutComment = '@%s,-%d';
  FormatWithComment    = FormatWithoutComment +';%s';

begin
  if (AComment <> '') then
    Result := Format(FormatWithComment, [APath, AId, AComment])
  else
    Result := Format(FormatWithoutComment, [APath, AId])
end;

end.
