{ *********************************************************************** }
{                                                                         }
{ PM Code Works Initialization file parser Unit v1.2.2                    }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWIniFileParser;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  Classes, SysUtils, StrUtils;

{$IFDEF MSWINDOWS}
const
  REG_BINARY = 'hex:';
  REG_INTEGER = 'dword:';
  REG_EXPANDSTRING = 'hex(2):';
{$ENDIF}

type
  { Exception classes }
  EInvalidIniFormat = class(Exception);

  { TIniFile }
  TIniFile = class(TObject)
  private
    FFile: TStringList;
    FFileName: string;
    FSaveOnDestroy: Boolean;
    function Add(AIndex: Integer; AString: string): Integer;
    function FindNextItem(AStartIndex: Integer = 0; AEndIndex: Integer = -1): Integer;
    function FindNextSection(AStartIndex: Integer = 0): Integer;
    function FindNextSectionItem(AStartIndex: Integer = 0): Integer;
    function GetEndOfItem(AIndex: Integer): Integer; overload;
    function GetEndOfItem(ASectionName, AKey: string): Integer; overload;
    function GetEndOfSection(AIndex: Integer): Integer; overload;
    function GetEndOfSection(ASectionName: string): Integer; overload;
    function GetKey(AIndex: Integer): string;
    function GetLength(): Integer;
    function GetValue(AIndex: Integer): string;
    function Remove(AStartIndex, AEndIndex: Integer): Boolean; overload;
  protected
    function AddRaw(ALine: string): Integer;
    function ExtractKey(const AKeyValuePair: string): string;
    function ExtractValue(const AKeyValuePair: string): string;
  public
    constructor Create(const AFileName: string; AOverwriteIfExists: Boolean = False;
      ASaveOnDestroy: Boolean = False);
    destructor Destroy; override;
    procedure AddRemove(ASectionName, AKey, AValue: string);
    function AddSection(ASectionName: string): Boolean; overload;
    function AddSection(ASectionName: string; AHashMap: TStrings): Boolean; overload;
    procedure Clear();
    procedure GetKeys(ASectionName: string; AKeys: TStrings);
    procedure GetSections(ASections: TStrings);
    function IndexOfKey(ASectionName, AKey: string): Integer;
    function IndexOfSection(ASectionName: string): Integer;
    function IsSection(AIndex: Integer): Boolean;
    function KeyExists(ASectionName, AKey: string): Boolean;
    function ReadBoolean(ASectionName, AKey: string): Boolean;
    function ReadInteger(ASectionName, AKey: string; ADefault: Integer = -1): Integer;
    function ReadString(ASectionName, AKey: string; ADefault: string = ''): string;
    function Remove(ASectionName, AKey: string): Boolean; overload;
    function RemoveSection(ASectionName: string): Boolean;
    procedure Save(); overload; virtual;
{$IFDEF MSWINDOWS}
    procedure Save(AEncoding: TEncoding); overload;
{$ENDIF}
    function SectionExists(ASectionName: string): Boolean;
    function WriteBoolean(ASectionName, AKey: string; AValue: Boolean): Integer;
    function WriteInteger(ASectionName, AKey: string; AValue: Integer): Integer;
    function WriteString(ASectionName, AKey, AValue: string): Integer;
    function WriteStrings(ASectionName, AKey: string; AValues: TStrings): Integer;
    { external }
    property FileName: string read FFileName;
    property Lines: Integer read GetLength;
    property Keys[AIndex: Integer]: string read GetKey; default;
    property SaveOnDestroy: Boolean read FSaveOnDestroy write FSaveOnDestroy;
    property Values[AIndex: Integer]: string read GetValue;
  end;

{$IFDEF MSWINDOWS}
  { Filter set }
  TFilterDataTypes = set of TRegDataType;

  { TRegistryFile }
  TRegistryFile = class(TIniFile)
  private
    FOnExportBegin, FOnExportEnd: TNotifyEvent;
    FReg: TRegistry;
    FAccess64: Boolean;
    function EscapeIdentifier(const AIdent: string): string;
    function GetKey(AIndex: Integer): string;
    function GetValue(AIndex: Integer): string;
    procedure SetAccess(AAccess64: Boolean);
    procedure WriteBinary(ASection, AIdent: string; ARegBinary: Boolean;
      ABytes: array of Byte); overload;
  public
    constructor Create(const AFileName: string; AOverwriteIfExists: Boolean = False;
      ASaveOnDestroy: Boolean = False);
    destructor Destroy; override;
    procedure AddRemove(ASection, AIdent, AValue: string);
    function AddSection(AHKey: HKEY; AKeyPath: string): Boolean; reintroduce;
    procedure Clear();
    function DeleteQuoteChars(const AText: string): string;
    function EscapePathDelimiter(const APath: string): string;
    procedure ExportKey(AHKey: HKEY; AKeyPath: string; ARecursive: Boolean;
      AFilterValues: TStrings = nil; AFilterTypes: TFilterDataTypes = []);
    procedure ExportReg(AHKey: HKEY; AKeyPath: string; ARecursive: Boolean = True;
      AFilterValues: TStrings = nil; AFilterTypes: TFilterDataTypes = []); overload;
    procedure ExportReg(AHKey: HKEY; AKeyPath, AValueName: string); overload;
    function GetSection(AHKey: HKEY; AKeyPath: string): string;
    procedure MakeHeadline();
    function ReadBinary(ASection, AIdent: string): TBytes;
    function ReadBoolean(ASection, AIdent: string): Boolean;
    function ReadExpandString(ASection, AIdent: string): string;
    function ReadInteger(ASection, AIdent: string): Integer;
    function ReadString(ASection, AIdent: string): string;
    function Remove(ASection, AIdent: string): Boolean;
    procedure Save(); override;
    function UnescapePathDelimiter(const APath: string): string;
    procedure WriteBinary(ASection, AIdent: string; AValue: TBytes); overload;
    procedure WriteBoolean(ASection, AIdent: string; AValue: Boolean);
    procedure WriteExpandString(ASection, AIdent, AValue: string);
    procedure WriteInteger(ASection, AIdent: string; AValue: Integer);
    procedure WriteString(ASection, AIdent, AValue: string);
    { external }
    property Access64: Boolean read FAccess64 write SetAccess;
    property Keys[AIndex: Integer]: string read GetKey; default;
    property OnExportBegin: TNotifyEvent read FOnExportBegin write FOnExportBegin;
    property OnExportEnd: TNotifyEvent read FOnExportEnd write FOnExportEnd;
    property Values[AIndex: Integer]: string read GetValue;
  end;
{$ENDIF}

implementation

{ TIniFile }

{ public TIniFile.Create

  General constructor for creating a TIniFile instance. }

constructor TIniFile.Create(const AFileName: string;
  AOverwriteIfExists: Boolean = False; ASaveOnDestroy: Boolean = False);
begin
  inherited Create;

  if (AFileName = '') then
    raise EArgumentException.Create('Missing parameter file name!');

  FFileName := AFileName;
  FSaveOnDestroy := ASaveOnDestroy;
  FFile := TStringList.Create;

  if ((not AOverwriteIfExists) and FileExists(AFileName)) then
    FFile.LoadFromFile(AFileName);
end;

{ public TIniFile.Destroy

  General destructor for destroying a TIniFile instance. }

destructor TIniFile.Destroy;
begin
  // Automatically save on destroy?
  if FSaveOnDestroy then
    Save();

  FFile.Free;
  inherited Destroy;
end;

{ private TIniFile.Add

  Inserts or adds a key/value pair to file. }

function TIniFile.Add(AIndex: Integer; AString: string): Integer;
begin
  if (AIndex + 1 < FFile.Count) then
  begin
    // Insert inside file
    FFile.Insert(AIndex, AString);
    Result := AIndex;
  end  //of if
  else
    // Append at the end
    Result := FFile.Add(AString);
end;

{ private TIniFile.FindNextItem

  Returns the next index of an item beginning the search from AStartIndex. }

function TIniFile.FindNextItem(AStartIndex: Integer = 0; AEndIndex: Integer = -1): Integer;
var
  i: Integer;
  Line: string;

begin
  Result := -1;

  // Search until file ends?
  if (AEndIndex = -1) then
    AEndIndex := FFile.Count;

  // Index valid?
  if (AStartIndex >= 0) and (AEndIndex <= FFile.Count) then
    for i := AStartIndex + 1 to AEndIndex - 1 do
    begin
      // Remove spaces from line
      Line := Trim(FFile[i]);

      // Current line is neither empty nor a comment or a section and contains "="
      if ((Line <> '') and not (Line[1] in ['#', ';', '[']) and AnsiContainsStr(Line, '=')) then
      begin
        Result := i;
        Break;
      end;  //of begin
    end;  //of for
end;

{ private TIniFile.FindNextSection

  Returns the next index of a section beginning the search from AStartIndex. }

function TIniFile.FindNextSection(AStartIndex: Integer = 0): Integer;
var
  i: Integer;

begin
  Result := -1;

  // Index valid and file not empty?
  if ((AStartIndex >= 0) and (FFile.Count > 0)) then
    for i := AStartIndex + 1 to FFile.Count - 1 do
      if IsSection(i) then
      begin
        Result := i;
        Break;
      end;  //of begin
end;

{ private TIniFile.FindNextSectionItem

  Returns the next index of an item in section beginning the search from
  AStartIndex until section ends. }

function TIniFile.FindNextSectionItem(AStartIndex: Integer = 0): Integer;
begin
  Result := FindNextItem(AStartIndex, FindNextSection(AStartIndex));
end;

{ private TIniFile.GetEndOfItem

  Returns the last line index of an item for appending text. }

function TIniFile.GetEndOfItem(AIndex: Integer): Integer;
var
  NextItemIndex: Integer;

begin
  // Item index valid?
  if (AIndex > 0) then
  begin
    // Find next item
    NextItemIndex := FindNextSectionItem(AIndex);

    // Maybe only one item or last item
    if (NextItemIndex = -1) then
      NextItemIndex := GetEndOfSection(AIndex) - 1
    else
      // Return end of current item (not of next)
      Dec(NextItemIndex);

    Result := NextItemIndex;
  end  //of begin
  else
    Result := -1;
end;

{ private TIniFile.GetEndOfItem

  Returns the last line index of an item for appending text. }

function TIniFile.GetEndOfItem(ASectionName, AKey: string): Integer;
begin
  Result := GetEndOfItem(IndexOfKey(ASectionName, AKey));
end;

{ private TIniFile.GetEndOfSection

  Returns the last line index of a section for appending text. }

function TIniFile.GetEndOfSection(AIndex: Integer): Integer;
var
  NextSectionIndex: Integer;

begin
  // Index valid?
  if (AIndex > 0) then
  begin
    // Find next section
    NextSectionIndex := FindNextSection(AIndex);

    // Maybe only one section or last section
    if (NextSectionIndex = -1) then
      NextSectionIndex := FFile.Count
    else
      // Return end of current section (not of next)
      Dec(NextSectionIndex);

    Result := NextSectionIndex;
  end  //of begin
  else
    Result := -1;
end;

{ private TIniFile.GetEndOfSection

  Returns the last line index of a section for appending text. }

function TIniFile.GetEndOfSection(ASectionName: string): Integer;
begin
  Result := GetEndOfSection(IndexOfSection(ASectionName));
end;

{ private TIniFile.GetKey

  Returns the name of an item at index. }

function TIniFile.GetKey(AIndex: Integer): string;
begin
  Result := ExtractKey(FFile[AIndex]);
end;

{ private TIniFile.GetLength

  Returns the number of lines. }

function TIniFile.GetLength(): Integer;
begin
  Result := FFile.Count;
end;

{ private TIniFile.GetValue

  Returns the value of an item at index. }

function TIniFile.GetValue(AIndex: Integer): string;
var
  Lines: TStringList;
  EndIndex, i: Integer;
  Line: string;

begin
  Lines := TStringList.Create;

  try
    EndIndex := GetEndOfItem(AIndex);

    // Multi-line value?
    if (EndIndex > AIndex) then
    begin
      // Concatenate lines without backslash at the end
      for i := AIndex to EndIndex do
        Line := Line + TrimLeft(Copy(FFile[i], 0, Length(FFile[i]) - 1));
    end  //of begin
    else
      // Single line value
      Line := FFile[AIndex];

    Result := ExtractValue(Line);

  finally
    Lines.Free;
  end;  //of try
end;

{ private TIniFile.Remove

  Removes lines from start to end index. }

function TIniFile.Remove(AStartIndex, AEndIndex: Integer): Boolean;
var
  i: Integer;

begin
  // Section found?
  if ((AStartIndex <> -1) and (AEndIndex <> -1)) then
  begin
    for i := AEndIndex downto AStartIndex do
      FFile.Delete(i);

    Result := True;
  end  //of begin
  else
    Result := False;
end;

{ protected TIniFile.AddRaw

  Adds a raw string. Can be used to add comments. }

function TIniFile.AddRaw(ALine: string): Integer;
begin
  Result := FFile.Add(ALine);
end;

{ protected TIniFile.ExtractKey

  Extracts the key property from a key-value-pair. }

function TIniFile.ExtractKey(const AKeyValuePair: string): string;
var
  Line: string;
  DelimiterPos: Integer;

begin
  Line := Trim(AKeyValuePair);
  DelimiterPos := AnsiPos('=', Line);

  if (DelimiterPos <> -1) then
    Result := TrimRight(Copy(Line, 0, DelimiterPos - 1))
  else
    Result := '';
end;

{ protected TIniFile.ExtractValue

  Extracts the item value from a key-value-pair. }

function TIniFile.ExtractValue(const AKeyValuePair: string): string;
var
  Line: string;
  DelimiterPos: Integer;

begin
  Line := Trim(AKeyValuePair);
  DelimiterPos := AnsiPos('=', Line);

  if (DelimiterPos <> -1) then
    Result := TrimLeft(Copy(Line, DelimiterPos + 1, Length(Line) - DelimiterPos))
  else
    Result := '';
end;

{ public TIniFile.AddRemove

  Adds a new or changes an existing value. If the new value is empty the item
  will be removed. }

procedure TIniFile.AddRemove(ASectionName, AKey, AValue: string);
begin
  if (AValue <> '') then
    WriteString(ASectionName, AKey, AValue)
  else
    Remove(ASectionName, AKey);
end;

{ public TIniFile.AddSection

  Adds a new section if not exist. }

function TIniFile.AddSection(ASectionName: string): Boolean;
var
  Exists: Boolean;

begin
  // Check for invalid section name
  if (ASectionName = '') then
    raise EInvalidIniFormat.Create('Section name must not be empty!');

  Exists := not SectionExists(ASectionName);

  if Exists then
  begin
    // No new line in first line!
    if (FFile.Count <> 0) then
      FFile.Append('');

    FFile.Append('['+ ASectionName +']');
  end;  //of begin

  Result := Exists;
end;

{ public TIniFile.AddSection

  Adds a new section with content. A line in AHashMap must be key=value }

function TIniFile.AddSection(ASectionName: string; AHashMap: TStrings): Boolean;
var
  InsertPos, i: Integer;

begin
  // Add section if not exists
  if AddSection(ASectionName) then
    // Find insert position of section
    InsertPos := GetEndOfSection(ASectionName)
  else
    InsertPos := -1;

  if ((InsertPos > 0) and (AHashMap.Count > 0)) then
  begin
    // Write section content
    for i := 0 to AHashMap.Count -1 do
      Add(InsertPos + i, AHashMap[i]);

    Result := True;
  end  //of begin
  else
    Result := False;
end;

{ public TIniFile.Clear

  Empties the current file. }

procedure TIniFile.Clear();
begin
  FFile.Clear;
end;

{ public TIniFile.GetKeys

  Reads the keys of a section. }

procedure TIniFile.GetKeys(ASectionName: string; AKeys: TStrings);
var
  Index, EndIndex, i: Integer;
  Key: string;

begin
  // Find section
  Index := IndexOfSection(ASectionName);

  // Find first item in section
  EndIndex := GetEndOfSection(Index);

  if ((Index <> -1) and (EndIndex <> -1)) then
    // Collect keys of section
    for i := Index + 1 to EndIndex do
    begin
      Key := GetKey(i);

      // Multi-line item?
      if (Key <> '') then
        AKeys.Append(Key);
    end;  //of for
end;

{ public TIniFile.GetSections

  Reads and collects all section names. }

procedure TIniFile.GetSections(ASections: TStrings);
var
  Index: Integer;
  Line: string;

begin
  Index := FindNextSection(0);

  while (Index <> -1) do
  begin
    // Remove spaces from line
    Line := Trim(FFile[Index]);

    // Save name of section (without start + end tags) in list
    ASections.Append(Copy(Line, 2, Length(Line) - 2));

    // Find next section beginning from next line
    Index := FindNextSection(Index + 1);
  end;  //of while
end;

{ public TIniFile.IndexOfSection

  Returns the index of a section. }

function TIniFile.IndexOfSection(ASectionName: string): Integer;
var
  Index: Integer;

begin
  Index := FindNextSection(0);

  while (Index <> -1) do
  begin
    if (FFile[Index] = '['+ ASectionName +']') then
      Break;

    Index := FindNextSection(Index);
  end;  //of while

  Result := Index;
end;

{ public TIniFile.IndexOfKey

  Returns the index of a key. }

function TIniFile.IndexOfKey(ASectionName, AKey: string): Integer;
var
  Index: Integer;

begin
  // Find section
  Index := IndexOfSection(ASectionName);

  // Find first item in section
  Index := FindNextSectionItem(Index);

  // Section got items?
  while (Index <> -1) do
  begin
    // Item matches search?
    if (ExtractKey(FFile[Index]) = AKey) then
      Break;

    // Find next item in section
    Index := FindNextSectionItem(Index);
  end;  //of while

  Result := Index;
end;

{ public TIniFile.IsSection

  Checks if a line contains a section. }

function TIniFile.IsSection(AIndex: Integer): Boolean;
var
  Line: string;

begin
  if ((AIndex > 0) and (FFile.Count > AIndex)) then
    // Remove spaces from line
    Line := Trim(FFile[AIndex]);

  // Section starts with "[" and ends with "]"
  result := ((Line <> '') and (Line[1] = '[') and (Line[Length(Line)] = ']'));
end;

{ public TIniFile.KeyExists

  Checks if a value name exists. }

function TIniFile.KeyExists(ASectionName, AKey: string): Boolean;
begin
  Result := (IndexOfKey(ASectionName, AKey) <> -1);
end;

{ public TIniFile.ReadBoolean

  Returns a boolean value of a key in section. }

function TIniFile.ReadBoolean(ASectionName, AKey: string): Boolean;
begin
  Result := (ReadInteger(ASectionName, AKey) = 1);
end;

{ public TIniFile.ReadInteger

  Returns an integer value of a key in section. }

function TIniFile.ReadInteger(ASectionName, AKey: string; ADefault: Integer = -1): Integer;
begin
  if not TryStrToInt(ReadString(ASectionName, AKey), Result) then
    Result := ADefault;
end;

{ public TIniFile.ReadString

  Returns a string value of a key in section. }

function TIniFile.ReadString(ASectionName, AKey: string; ADefault: string = ''): string;
var
  Index: Integer;

begin
  // Search for key
  Index := IndexOfKey(ASectionName, AKey);

  // Key found?
  if (Index > 0) then
    Result := GetValue(Index)
  else
    Result := ADefault;
end;

{ public TIniFile.Remove

  Removes a key inside a section. }

function TIniFile.Remove(ASectionName, AKey: string): Boolean;
var
  StartIndex, EndIndex: Integer;

begin
  // Search for key
  StartIndex := IndexOfKey(ASectionName, AKey);
  EndIndex := GetEndOfItem(StartIndex);

  // Item is last element in file?
  if (EndIndex = -1) then
    EndIndex := FFile.Count;

  Result := Remove(StartIndex, EndIndex);
end;

{ public TIniFile.RemoveSection

  Removes an entire section with all items. }

function TIniFile.RemoveSection(ASectionName: string): Boolean;
begin
  Result := Remove(IndexOfSection(ASectionName), GetEndOfSection(ASectionName));
end;

{ public TIniFile.Save

  Writes current file to disk. }

procedure TIniFile.Save();
begin
{$IFDEF MSWINDOWS}
  Save(TEncoding.UTF8);
{$ELSE}
  FFile.SaveToFile(FFileName);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
{ public TIniFile.Save

  Writes current file with explicit encoding to disk. }

procedure TIniFile.Save(AEncoding: TEncoding);
begin
  FFile.SaveToFile(FFileName, AEncoding);
end;
{$ENDIF}

{ public TIniFile.SectionExists

  Checks if a section exists. }

function TIniFile.SectionExists(ASectionName: string): Boolean;
begin
  Result := (IndexOfSection(ASectionName) <> -1);
end;

{ public TIniFile.WriteBoolean

  Writes an boolean value to a a key in section. }

function TIniFile.WriteBoolean(ASectionName, AKey: string; AValue: Boolean): Integer;
begin
  Result := WriteInteger(ASectionName, AKey, Ord(AValue));
end;

{ public TIniFile.WriteInteger

  Writes an integer value to a key in section. }

function TIniFile.WriteInteger(ASectionName, AKey: string; AValue: Integer): Integer;
begin
  Result := WriteString(ASectionName, AKey, IntToStr(AValue));
end;

{ public TIniFile.WriteString

  Writes a string value to a key in section. }

function TIniFile.WriteString(ASectionName, AKey, AValue: string): Integer;
var
  Index: Integer;

begin
  // Check for invalid key
  if (AKey = '') then
    raise EInvalidIniFormat.Create('Key must not be empty!');

  // Search for key
  Index := IndexOfKey(ASectionName, AKey);

  // Key does not exist?
  if (Index = -1) then
  begin
    // Append item at the end of section
    Index := GetEndOfSection(ASectionName);

    // Add section if not exists
    if (Index = -1) then
    begin
      AddSection(ASectionName);
      Index := FFile.Count;
    end;  //of begin
  end  //of begin
  else
    // Delete current item
    Remove(Index, GetEndOfItem(Index));

  // Add item
  Result := Add(Index, AKey +'='+ AValue);
end;

{ public TIniFile.WriteStrings

  Writes a multi-line string value to a key in section. }

function TIniFile.WriteStrings(ASectionName, AKey: string; AValues: TStrings): Integer;
var
  Index, i: Integer;

begin
  // Insert key with first line
  Index := WriteString(ASectionName, AKey, AValues[0]);

  // Add other lines
  for i := 1 to AValues.Count -1 do
  begin
    Inc(Index);
    Add(Index, AValues[i]);
  end;  //of for

  // Insert empty line before another section
  if IsSection(Index + 1) then
    Add(Index + 1, '');

  Result := Index;
end;

{$IFDEF MSWINDOWS}
{ TRegistryFile }

{ public TRegistryFile.Create

  General constructor for creating a TRegistryFile instance. }

constructor TRegistryFile.Create(const AFileName: string;
  AOverwriteIfExists: Boolean = False; ASaveOnDestroy: Boolean = False);
begin
  if (ExtractFileExt(AFileName) <> '.reg') then
    raise EArgumentException.Create('The specified file is no .reg file!');

  inherited Create(AFileName, AOverwriteIfExists, ASaveOnDestroy);
  MakeHeadline();
  FAccess64 := True;
  FReg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);
end;

{ public TRegistryFile.Destroy

  Destructor for destroying a TRegistryFile instance. }

destructor TRegistryFile.Destroy;
begin
  FReg.Free;
  inherited Destroy;
end;

{ private TRegistryFile.EscapeIdentifier

  Returns the identifier quoted. }

function TRegistryFile.EscapeIdentifier(const AIdent: string): string;
begin
  if ((AIdent = '') or (AIdent = '@')) then
    Result := '@'
  else
    Result := '"'+ AIdent +'"';
end;

{ private TRegistryFile.GetKey

  Returns the name of an item at index. }

function TRegistryFile.GetKey(AIndex: Integer): string;
begin
  Result := DeleteQuoteChars(inherited GetKey(AIndex));
end;

{ private TRegistryFile.GetValue

  Returns the item value at index. }

function TRegistryFile.GetValue(AIndex: Integer): string;
begin
  Result := DeleteQuoteChars(inherited GetValue(AIndex));
end;

{ private TRegistryFile.SetAccess

  Sets the current registry access rights. }

procedure TRegistryFile.SetAccess(AAccess64: Boolean);
begin
  if AAccess64 then
    FReg.Access := KEY_WOW64_64KEY or KEY_READ
  else
    FReg.Access := KEY_READ;

  FAccess64 := AAccess64;
end;

{ private TRegistryFile.WriteBinary

  Writes a little endian encoded binary value to a .reg file. }

procedure TRegistryFile.WriteBinary(ASection, AIdent: string; ARegBinary: Boolean;
  ABytes: array of Byte);
var
  i: Integer;
  Line: string;
  Lines: TStringList;

begin
  // rdBinary or rdExpandString?
  if ARegBinary then
    Line := REG_BINARY
  else
    Line := REG_EXPANDSTRING;

  // Init line cache
  Lines := TStringList.Create;

  try
    for i := 0 to Length(ABytes) - 1 do
    begin
      // Linebreaks after 80 characters
      if (Length(Line) + 6 > 80) then
      begin
        Lines.Append(Line + '\');
        Line := '  ';
      end;  //of begin

      // Convert Byte to Hex and separate 2 digits by comma
      Line := Line + IntToHex(ABytes[i], 2) +',';
    end;  //of while

    // Append missing zero Byte without comma (only for rdExpandString)
    if not ARegBinary then
      Line := Line +'00'
    else
      // Delete last comma (only for rdBinary)
      Delete(Line, Length(Line), 1);

    // Append last line
    Lines.Append(Line);

    // Write lines
    WriteStrings(ASection, EscapeIdentifier(AIdent), Lines);

  finally
    Lines.Free;
  end;  //of try
end;

{ public TRegistryFile.AddRemove

  Adds a new or changes an existing value. If the new value is empty the item
  will be removed. }

procedure TRegistryFile.AddRemove(ASection, AIdent, AValue: string);
begin
  if (AValue <> '') then
    WriteString(ASection, AIdent, AValue)
  else
    Remove(ASection, AIdent);
end;

{ public TRegistryFile.AddSection

  Adds a new section. }

function TRegistryFile.AddSection(AHKey: HKEY; AKeyPath: string): Boolean;
begin
  Result := inherited AddSection(GetSection(AHKey, AKeyPath));
end;

{ public TRegistryFile.Clear

  Empties the current file. }

procedure TRegistryFile.Clear();
begin
  inherited Clear();
  MakeHeadline();
end;

{ public TRegistryFile.DeleteQuoteChars

 Deletes all quote chars " from a string. }

function TRegistryFile.DeleteQuoteChars(const AText: string): string;
begin
  Result := StringReplace(AText, '"', '', [rfReplaceAll]);
end;

{ public TRegistryFile.EscapePathDelimiter

 Escapes all baskslashes in a string. }

function TRegistryFile.EscapePathDelimiter(const APath: string): string;
begin
  // Escape path delimiter
  Result := StringReplace(APath, '\', '\\', [rfReplaceAll]);

  // Escape quote chars
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

{ public TRegistryFile.ExportKey

  Collects data from a key path and writes it to .reg file. }

procedure TRegistryFile.ExportKey(AHKey: HKEY; AKeyPath: string;
  ARecursive: Boolean; AFilterValues: TStrings = nil; AFilterTypes: TFilterDataTypes = []);
var
  Values, Keys: TStringList;
  i: Cardinal;
  Section: string;
  Buffer: TBytes;

begin
  // Default: Filter nothing
  if (AFilterTypes = []) then
    AFilterTypes := [rdString, rdInteger, rdExpandString, rdBinary];

  try
    // Init Registry access
    FReg.RootKey := AHKey;

    // Key invalid?
    if not FReg.OpenKey(AKeyPath, False) then
      Exit;

    // Read all values from current key
    Values := TStringList.Create;
    FReg.GetValueNames(Values);

    // Build and append section header
    Section := GetSection(AHKey, AKeyPath);
    inherited AddSection(Section);

    if (Values.Count > 0) then
      // Append key-value pairs
      for i := 0 to Values.Count - 1 do
      begin
        // Filter values
        if (Assigned(AFilterValues) and (AFilterValues.Count > 0)) then
          if (AFilterValues.IndexOf(Values[i]) = -1) then
            Continue;

        case FReg.GetDataType(Values[i]) of
          rdString:
            if (rdString in AFilterTypes) then
              WriteString(Section, Values[i], FReg.ReadString(Values[i]));

          rdInteger:
            if (rdInteger in AFilterTypes) then
              WriteInteger(Section, Values[i], FReg.ReadInteger(Values[i]));

          rdExpandString:
            if (rdExpandString in AFilterTypes) then
              WriteExpandString(Section, Values[i], FReg.ReadString(Values[i]));

          rdBinary:
            if (rdBinary in AFilterTypes) then
            begin
              SetLength(Buffer, FReg.GetDataSize(Values[i]));
              FReg.ReadBinaryData(Values[i], Buffer[0], Length(Buffer));
              WriteBinary(Section, Values[i], Buffer);
            end;  //of begin
        end;  //of case
      end;  //of begin

    // Include subkeys?
    if (ARecursive and FReg.HasSubKeys()) then
    begin
      Keys := TStringList.Create;
      FReg.GetKeyNames(Keys);

      // Start recursion of subkeys
      for i := 0 to Keys.Count -1 do
      begin
        FReg.CloseKey();
        ExportKey(AHKey, IncludeTrailingPathDelimiter(AKeyPath) + Keys[i], True,
          AFilterValues, AFilterTypes);
      end;  //of for
    end;  //of begin

  finally
    FReg.CloseKey();
    Buffer := nil;
  end;  //of try
end;

{ public TRegistryFile.ExportReg

  Exports an entire Registry key (opt. recursive) and saves it as .reg file. }

procedure TRegistryFile.ExportReg(AHKey: HKEY; AKeyPath: string;
  ARecursive: Boolean = True; AFilterValues: TStrings = nil;
  AFilterTypes: TFilterDataTypes = []);
begin
  if Assigned(FOnExportBegin) then
    FOnExportBegin(Self);

  MakeHeadline();
  ExportKey(AHKey, AKeyPath, ARecursive, AFilterValues, AFilterTypes);
  Save();

  if Assigned(FOnExportEnd) then
    FOnExportEnd(Self);
end;

{ public TRegistryFile.ExportReg

  Exports a single Registry value and saves it as .reg file. }

procedure TRegistryFile.ExportReg(AHKey: HKEY; AKeyPath, AValueName: string);
var
  Section: string;
  Buffer: TBytes;

begin
  try
    // Init Registry access
    FReg.RootKey := AHKey;

    // Invalid key?
    if not FReg.OpenKey(AKeyPath, False) then
      raise ERegistryException.Create('Error while exporting value: Key does not exist!');

    // Invalid value?
    if not FReg.ValueExists(AValueName) then
      raise ERegistryException.Create('Error while exporting value: Value does not exist!');

    MakeHeadline();

    // Build and append section header
    Section := GetSection(AHKey, AKeyPath);
    inherited AddSection(Section);

    // Append key-value pair
    case FReg.GetDataType(AValueName) of
      rdString:       WriteString(Section, AValueName, FReg.ReadString(AValueName));
      rdInteger:      WriteInteger(Section, AValueName, FReg.ReadInteger(AValueName));
      rdExpandString: WriteExpandString(Section, AValueName, FReg.ReadString(AValueName));
      rdBinary:       begin
                        SetLength(Buffer, FReg.GetDataSize(AValueName));
                        FReg.ReadBinaryData(AValueName, Buffer[0], Length(Buffer));
                        WriteBinary(Section, AValueName, Buffer);
                      end;
    end;  //of case

    // Save .reg file
    Save();

  finally
    FReg.CloseKey();
    Buffer := nil;
  end;  //of try
end;

{ public TRegistryFile.GetSection

  Returns the concat of AHKEY and AKeyPath. }

function TRegistryFile.GetSection(AHKey: HKEY; AKeyPath: string): string;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create;

  try
    Reg.RootKey := AHKey;
    Result := ExcludeTrailingPathDelimiter(Reg.RootKeyName +'\'+ AKeyPath);

  finally
    Reg.Free;
  end;  //of try
end;

{ public TRegistryFile.MakeHeadline

  Adds the first line of an .reg file. Must be used only once. }

procedure TRegistryFile.MakeHeadline();
begin
  if (Lines = 0) then
    AddRaw('Windows Registry Editor Version 5.00');
end;

{ public TRegistryFile.ReadBinary

  Reads a little endian encoded binary value from a .reg file. }

function TRegistryFile.ReadBinary(ASection, AIdent: string): TBytes;
var
  StringValue: string;
  StartIndex, EndIndex, i, j: Integer;

begin
  // Read full value
  StringValue := ReadString(ASection, AIdent);

  // No binary data?
  if (not AnsiStartsStr(REG_BINARY, StringValue) and
    not AnsiStartsStr(REG_EXPANDSTRING, StringValue)) then
    Exit;

  StartIndex := AnsiPos(':', StringValue);
  EndIndex := Length(StringValue);

  // Invalid indices?
  if ((StartIndex = 0) or (EndIndex = 0)) then
    Exit;

  // 2 hex chars + 1 comma delimiter (rounded up)
  SetLength(Result, ((EndIndex - StartIndex) div 3) + 1);

  i := StartIndex + 1;
  j := 0;

  // Parse binary value
  while (i <= EndIndex) do
  begin
    // Search for hex chars (skip comma delimiter)
    if (StringValue[i] <> ',') then
    begin
      // Convert 2 hex chars to 1 Byte
      Result[j] := StrToInt('$'+ StringValue[i] + StringValue[i+1]);

      // Already read 2 hex chars!
      i := i + 2;
      Inc(j);
    end //of begin
    else
      Inc(i);
  end;  //of while
end;

{ public TRegistryFile.ReadBoolean

  Returns a boolean value of a key in section. }

function TRegistryFile.ReadBoolean(ASection, AIdent: string): Boolean;
begin
  Result := (ReadInteger(ASection, AIdent) = 1);
end;

{ public TRegistryFile.ReadExpandString

  Returns a expand string value of a key in section. }

function TRegistryFile.ReadExpandString(ASection, AIdent: string): string;
var
  i: Integer;
  Bytes: TBytes;

begin
  // Convert little endian encoded hex value to Byte array
  Bytes := ReadBinary(ASection, AIdent);

  // Convert Byte to Char
  for i := 0 to Length(Bytes) - 1 do
    // Skip zero Bytes
    if (Bytes[i] <> 0) then
      Result := Result + Chr(Bytes[i]);

  // Mark array as removable
  Bytes := nil;
end;

{ public TRegistryFile.ReadInteger

  Reads an integer from a .reg file. }

function TRegistryFile.ReadInteger(ASection, AIdent: string): Integer;
var
  StringVal: string;

begin
  Result := -1;
  StringVal := inherited ReadString(ASection, EscapeIdentifier(AIdent));

  if AnsiContainsStr(StringVal, REG_INTEGER) then
  begin
    StringVal := Copy(StringVal, 7, Length(StringVal));
    Result := StrToInt('$'+ StringVal);
  end;  //of begin
end;

{ public TRegistryFile.ReadString

  Reads a string from a .reg file. }

function TRegistryFile.ReadString(ASection, AIdent: string): string;
var
  Value: string;

begin
  Value := inherited ReadString(ASection, EscapeIdentifier(AIdent));
  Value := UnescapePathDelimiter(Value);
  Result := DeleteQuoteChars(Value);
end;

{ public TRegistryFile.Remove

  Removes a key inside a section. }

function TRegistryFile.Remove(ASection, AIdent: string): Boolean;
begin
  Result := inherited Remove(ASection, EscapeIdentifier(AIdent));
end;

{ public TRegistryFile.Save

  Writes current .reg file to disk. }

procedure TRegistryFile.Save();
begin
  inherited Save(TEncoding.Unicode);
end;

{ public TRegistryFile.UnescapePathDelimiter

 Deletes escape chars from a string. }

function TRegistryFile.UnescapePathDelimiter(const APath: string): string;
begin
  // Remove escape of path delimiter
  Result := StringReplace(APath, '\\', '\', [rfReplaceAll]);

  // Remove escape of quote chars
  Result := StringReplace(Result, '\"', '"', [rfReplaceAll]);
end;

{ public TRegistryFile.WriteBinary

  Writes a little endian encoded binary value to a .reg file. }

procedure TRegistryFile.WriteBinary(ASection, AIdent: string; AValue: TBytes);
begin
  WriteBinary(ASection, AIdent, True, AValue);
end;

{ public TRegistryFile.WriteBoolean

  Writes an boolean value to a a key in section. }

procedure TRegistryFile.WriteBoolean(ASection, AIdent: string; AValue: Boolean);
begin
  WriteInteger(ASection, AIdent, Ord(AValue));
end;

{ public TRegistryFile.WriteExpandString

  Writes an expand string in little endian encoding to a .reg file. }

procedure TRegistryFile.WriteExpandString(ASection, AIdent, AValue: string);
var
  i, j: Integer;
  Bytes: array of Byte;

begin
  // Expand 1 Byte character to 2 Byte Hex characters
  SetLength(Bytes, (Length(AValue) * 2) + 1);
  i := 0;

  // Convert char to Byte
  for j := 1 to Length(AValue) do
  begin
    Bytes[i] := Ord(AValue[j]);
    i := i + 2;
  end;  //of while

  // Write binary data with linebreaks to .reg file
  WriteBinary(ASection, AIdent, False, Bytes);

  // Mark array as removable
  Bytes := nil;
end;

{ public TRegistryFile.WriteInteger

  Writes an escaped integer to a .reg file. }

procedure TRegistryFile.WriteInteger(ASection, AIdent: string; AValue: Integer);
begin
  inherited WriteString(ASection, EscapeIdentifier(AIdent), REG_INTEGER + IntToHex(AValue, 8));
end;

{ public TRegistryFile.WriteString

  Writes an escaped string to a .reg file. }

procedure TRegistryFile.WriteString(ASection, AIdent, AValue: string);
begin
  inherited WriteString(ASection, EscapeIdentifier(AIdent), '"'+ EscapePathDelimiter(AValue) +'"');
end;
{$ENDIF}

end.
