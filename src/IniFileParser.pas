{ *********************************************************************** }
{                                                                         }
{ Initialization file parser Unit v1.1                                    }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit IniFileParser;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  Classes, SysUtils, StrUtils, OSUtils;

type
  { Exception classes }
  EInvalidIniFormat = class(Exception);
  EParserException = class(Exception);

  { TIniFile }
  TIniFile = class(TObject)
  private
    FFile: TStringList;
    FFileName: string;
    FSaveOnDestroy: Boolean;
    function FindNextItem(AStartIndex: Integer = 0): Integer;
    function FindNextSection(AStartIndex: Integer = 0): Integer;
    function GetEndOfSection(ASectionName: string): Integer;
    function GetKey(AIndex: Integer): string;
    function GetLength(): Integer;
    function GetValue(AIndex: Integer): string;
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
    procedure GetSections(ASections: TStrings);
    function IndexOfKey(ASectionName, AKey: string): Integer;
    function IndexOfSection(ASectionName: string): Integer;
    function KeyExists(ASectionName, AKey: string): Boolean;
    procedure ReadSection(ASectionName: string; ASection: TStrings);
    function ReadBoolean(ASectionName, AKey: string): Boolean;
    function ReadInteger(ASectionName, AKey: string): Integer;
    function ReadString(ASectionName, AKey: string): string;
    function Remove(ASectionName, AKey: string): Boolean;
    function RemoveSection(ASectionName: string): Boolean;
    procedure Save();
    function SectionExists(ASectionName: string): Boolean;
    procedure WriteBoolean(ASectionName, AKey: string; AValue: Boolean);
    procedure WriteInteger(ASectionName, AKey: string; AValue: Integer);
    procedure WriteString(ASectionName, AKey, AValue: string);
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
    function GetKey(AIndex: Integer): string;
    function GetValue(AIndex: Integer): string;
    procedure Write(ASection, AIdent, AValue: string);
    procedure WriteByte(ASection, AIdent: string; ARegBinary: Boolean;
      var AValue: array of Byte);
  public
    constructor Create(const AFileName: string; AOverwriteIfExists: Boolean = False;
      ASaveOnDestroy: Boolean = False);
    destructor Destroy; override;
    procedure AddRemove(ASectionName, AKey, AValue: string);
    function AddSection(AHKey: HKEY; AKeyPath: string): Boolean; reintroduce;
    procedure Clear();
    function DeleteQuoteChars(const AText: string): string;
    function EscapePathDelimiter(const APath: string): string;
    procedure ExportKey(AHKey: HKEY; AKeyPath: string; ARecursive: Boolean;
      AFilter: TFilterDataTypes = []);
    procedure ExportReg(AHKey: HKEY; AKeyPath: string; ARecursive: Boolean = True;
      AFilter: TFilterDataTypes = []); overload;
    procedure ExportReg(AHKey: HKEY; AKeyPath, AValueName: string); overload;
    function GetSection(AHKey: HKEY; AKeyPath: string): string;
    procedure MakeHeadline();
    function ReadBoolean(ASection, AIdent: string): Boolean;
    function ReadInteger(ASection, AIdent: string): Integer;
    function ReadString(ASection, AIdent: string): string;
    function Remove(ASection, AIdent: string): Boolean;
    function UnescapePathDelimiter(const APath: string): string;
    procedure WriteBinary(ASection, AIdent: string; var AValue: array of Byte);
    procedure WriteBoolean(ASection, AIdent: string; AValue: Boolean);
    procedure WriteInteger(ASection, AIdent: string; AValue: Integer);
    procedure WriteExpandString(ASection, AIdent, AValue: string);
    procedure WriteString(ASection, AIdent, AValue: string);
    { external }
    property Keys[AIndex: Integer]: string read GetKey; default;
    property Values[AIndex: Integer]: string read GetValue;
    property OnExportBegin: TNotifyEvent read FOnExportBegin write FOnExportBegin;
    property OnExportEnd: TNotifyEvent read FOnExportEnd write FOnExportEnd;
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
    raise EInvalidArgument.Create('Missing parameter file name!');

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

{ private TIniFile.FindNextItem

  Returns the next index of an item beginning the search from AStartIndex. }

function TIniFile.FindNextItem(AStartIndex: Integer = 0): Integer;
var
  i: Integer;
  Line: string;

begin
  Result := -1;

  // Index valid?
  if (AStartIndex >= 0) then
    for i := AStartIndex to FFile.Count -1 do
    begin
      // Remove spaces from line
      Line := Trim(FFile[i]);

      // Current line is neither empty, a comment nor a section
      if ((Line <> '') and not (Line[1] in ['#', ';', '['])) then
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
  Line: string;

begin
  Result := -1;

  // Index valid and file not empty?
  if ((AStartIndex >= 0) and (FFile.Count > 0)) then
    for i := AStartIndex to FFile.Count -1 do
    begin
      // Remove spaces from line
      Line := Trim(FFile[i]);

      // "[" is the start tag of a section
      if ((Line <> '') and (Line[1] = '[')) then
      begin
        // Find end tag "]" of a section
        if (AnsiPos(']', Line) > 0) then
        begin
          Result := i;
          Break;
        end;  //of begin
      end;  //of begin
    end;  //of for
end;

{ private TIniFile.GetEndOfSection

  Returns the last line index of a section for appending text. }

function TIniFile.GetEndOfSection(ASectionName: string): Integer;
var
  SectionIndex, NextSectionIndex: Integer;

begin
  SectionIndex := IndexOfSection(ASectionName);

  // Section found?
  if (SectionIndex <> -1) then
  begin
    // Find next section
    NextSectionIndex := FindNextSection(SectionIndex);

    // Maybe only one section
    if (NextSectionIndex = -1) then
      NextSectionIndex := FFile.Count;

    Result := NextSectionIndex;
  end  //of begin
  else
    Result := -1;
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
begin
  Result := ExtractValue(FFile[AIndex]);
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
      FFile.Insert(InsertPos + i, AHashMap[i]);

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

    Index := FindNextSection(Index + 1);
  end;  //of while

  // File index starts with 1
  if (Index <> -1) then
    Inc(Index);

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
  Index := FindNextItem(Index);

  // Section got items?
  while (Index <> -1) do
  begin
    // Item matches search?
    if (ExtractKey(FFile[Index]) = AKey) then
      Break;

    Index := FindNextItem(Index + 1);
  end;  //of while

  // File index starts with 1
  if (Index <> -1) then
    Inc(Index);

  Result := Index;
end;

{ public TIniFile.KeyExists

  Checks if a value name exists. }

function TIniFile.KeyExists(ASectionName, AKey: string): Boolean;
begin
  Result := (IndexOfKey(ASectionName, AKey) <> -1);
end;

{ public TIniFile.ReadSection

  Reads the content of a section. }

procedure TIniFile.ReadSection(ASectionName: string; ASection: TStrings);
var
  Index: Integer;
  Line: string;

begin
  // Find section
  Index := IndexOfSection(ASectionName);

  // Find first item in section
  Index := FindNextItem(Index);

  // Section got items?
  while (Index <> -1) do
  begin
    Line := FFile[Index];

    // Line contains =
    if (AnsiPos('=', Line) > 0) then
      ASection.Append(Line);

    Index := FindNextItem(Index + 1);
  end;  //of while
end;

{ public TIniFile.ReadBoolean

  Returns a boolean value of a key in section. }

function TIniFile.ReadBoolean(ASectionName, AKey: string): Boolean;
begin
  Result := (ReadInteger(ASectionName, AKey) = 1);
end;

{ public TIniFile.ReadInteger

  Returns an integer value of a key in section. }

function TIniFile.ReadInteger(ASectionName, AKey: string): Integer;
var
  Value: string;

begin
  Value := ReadString(ASectionName, AKey);

  if (Value = '') then
    Result := -1
  else
    Result := StrToInt(Value);
end;

{ public TIniFile.ReadString

  Returns a string value of a key in section. }

function TIniFile.ReadString(ASectionName, AKey: string): string;
var
  Index: Integer;

begin
  // Search for key
  Index := IndexOfKey(ASectionName, AKey);

  // Key found?
  if (Index > 0) then
    Result := GetValue(Index - 1)
  else
    Result := '';
end;

{ public TIniFile.Remove

  Removes a key inside a section. }

function TIniFile.Remove(ASectionName, AKey: string): Boolean;
var
  Index: Integer;

begin
  // Search for key
  Index := IndexOfKey(ASectionName, AKey);

  // Key found?
  if (Index > 0) then
  begin
    FFile.Delete(Index - 1);
    Result := True;
  end  //of begin
  else
    Result := False;
end;

{ public TIniFile.RemoveSection

  Removes an entire section with all items. }

function TIniFile.RemoveSection(ASectionName: string): Boolean;
var
  StartIndex, EndIndex, i: Integer;

begin
  StartIndex := IndexOfSection(ASectionName);
  EndIndex := GetEndOfSection(ASectionName);

  if ((StartIndex <> -1) and (EndIndex <> -1)) then
  begin
    for i := EndIndex - 1 downto StartIndex -1 do
      FFile.Delete(i);

    Result := True;
  end  //of begin
  else
    Result := False;
end;

{ public TIniFile.Save

  Writes current file to disk. }

procedure TIniFile.Save();
begin
{var
  IniFile: TextFile;
  i: Cardinal;

begin
  Assign(IniFile, FFileName);
  Rewrite(IniFile);

  for i := 0 to FFile.Count - 1 do
    Writeln(IniFile, FFile[i]);

  CloseFile(IniFile);}
  FFile.SaveToFile(FFileName);
end;

{ public TIniFile.SectionExists

  Checks if a section exists. }

function TIniFile.SectionExists(ASectionName: string): Boolean;
begin
  Result := (IndexOfSection(ASectionName) <> -1);
end;

{ public TIniFile.WriteBoolean

  Writes an boolean value to a a key in section. }

procedure TIniFile.WriteBoolean(ASectionName, AKey: string; AValue: Boolean);
begin
  WriteInteger(ASectionName, AKey, Ord(AValue));
end;

{ public TIniFile.WriteInteger

  Writes an integer value to a key in section. }

procedure TIniFile.WriteInteger(ASectionName, AKey: string; AValue: Integer);
begin
  WriteString(ASectionName, AKey, IntToStr(AValue));
end;

{ public TIniFile.WriteString

  Writes a string value to a key in section. }

procedure TIniFile.WriteString(ASectionName, AKey, AValue: string);
var
  Index, EndIndex: Integer;

begin
  // Check for invalid key
  if (AKey = '') then
    raise EInvalidIniFormat.Create('Key must not be empty!');

  // Search for key
  Index := IndexOfKey(ASectionName, AKey);

  // Key already exists?
  if (Index <> -1) then
  begin
    // Delete current item
    FFile.Delete(Index - 1);

    // Replace with new item
    FFile.Insert(Index - 1, AKey +'='+ AValue);
  end  //of begin
  else
    begin
      // Append item at the end of section
      EndIndex := GetEndOfSection(ASectionName);

      // Add section if not exists
      if (EndIndex = -1) then
      begin
        AddSection(ASectionName);
        EndIndex := FFile.Count;
      end;  //of begin

      FFile.Insert(EndIndex, AKey +'='+ AValue)
    end;  //of if
end;

{$IFDEF MSWINDOWS}
{ TRegistryFile }

{ public TRegistryFile.Create

  General constructor for creating a TRegistryFile instance. }

constructor TRegistryFile.Create(const AFileName: string;
  AOverwriteIfExists: Boolean = False; ASaveOnDestroy: Boolean = False);
begin
  if (ExtractFileExt(AFileName) <> '.reg') then
    raise EInvalidArgument.Create('The specified file is no .reg file!');

  inherited Create(AFileName, AOverwriteIfExists, ASaveOnDestroy);
  MakeHeadline();
  FReg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));
end;

{ public TRegistryFile.Destroy

  Destructor for destroying a TRegistryFile instance. }

destructor TRegistryFile.Destroy;
begin
  FReg.Free;
  inherited Destroy;
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

{ private TRegistryFile.Write

  Adds a string with possible empty identifier. }

procedure TRegistryFile.Write(ASection, AIdent, AValue: string);
begin
  if (AIdent = '') then
    inherited WriteString(ASection, '@', AValue)
  else
    inherited WriteString(ASection, '"'+ AIdent +'"', AValue);
end;

{ private TRegistryFile.WriteHex

  Writes a hex value to a .reg file. }

procedure TRegistryFile.WriteByte(ASection, AIdent: string; ARegBinary: Boolean;
  var AValue: array of Byte);
var
  i: Integer;
  Line: string;
  Lines: TStringList;

begin
  // rdBinary or rdExpandString?
  if ARegBinary then
    Line := 'hex:'
  else
    Line := 'hex(2):';

  // Init line cache
  Lines := TStringList.Create;

  try
    for i := 0 to Length(AValue) - 1 do
    begin
      // Linebreaks after 80 characters
      if (Length(Line) + 6 > 80) then
      begin
        Lines.Append(Line + '\');
        Line := '  ';
      end;  //of begin

      // Convert Byte to Hex and separate 2 Bytes by comma
      Line := Line + IntToHex(AValue[i], 2) +',';
    end;  //of while

    // Append missing zero Byte without comma (only for rdExpandString)
    if not ARegBinary then
      Line := Line +'00'
    else
      // Delete last comma (only for rdBinary)
      Delete(Line, Length(Line), 1);

    // Append last line
    Lines.Append(Line);

    // Write first line separated
    Write(ASection, AIdent, Lines[0]);

    // Append other lines
    for i := 1 to Lines.Count - 1 do
      AddRaw(Lines[i]);

  finally
    Lines.Free;
  end;  //of try
end;

{ public TRegistryFile.AddRemove

  Adds a new or changes an existing value. If the new value is empty the item
  will be removed. }

procedure TRegistryFile.AddRemove(ASectionName, AKey, AValue: string);
begin
  if (AValue <> '') then
    WriteString(ASectionName, AKey, AValue)
  else
    Remove(ASectionName, AKey);
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
  ARecursive: Boolean; AFilter: TFilterDataTypes = []);
var
  Values, Keys: TStringList;
  i: Cardinal;
  Section: string;
  Buffer: array of Byte;

begin
  // Default: Filter nothing
  if (AFilter = []) then
    AFilter := [rdString, rdInteger, rdExpandString, rdBinary];

  try
    // Init Registry access
    FReg.RootKey := AHKey;

    // Invalid key?
    if not FReg.OpenKey(AKeyPath, False) then
      raise EParserException.Create('Error while exporting key: Key does not exist!');

    // Read all values from current key
    Values := TStringList.Create;
    FReg.GetValueNames(Values);

    // Build and append section header
    Section := GetSection(AHKey, AKeyPath);
    inherited AddSection(Section);

    if (Values.Count > 0) then
      // Append key-value pairs
      for i := 0 to Values.Count -1 do
        case FReg.GetDataType(Values[i]) of
          rdString:
            if (rdString in AFilter) then
             WriteString(Section, Values[i], FReg.ReadString(Values[i]));

          rdInteger:
            if (rdInteger in AFilter) then
              WriteInteger(Section, Values[i], FReg.ReadInteger(Values[i]));

          rdExpandString:
            if (rdExpandString in AFilter) then
              WriteExpandString(Section, Values[i], FReg.ReadString(Values[i]));

          rdBinary:
            if (rdBinary in AFilter) then
            begin
              SetLength(Buffer, FReg.GetDataSize(Values[i]));
              FReg.ReadBinaryData(Values[i], Buffer[0], Length(Buffer));
              WriteBinary(Section, Values[i], Buffer);
            end;  //of begin
        end;  //of case

    // Include subkeys?
    if (ARecursive and FReg.HasSubKeys()) then
    begin
      Keys := TStringList.Create;
      FReg.GetKeyNames(Keys);

      // Start recursion of subkeys
      for i := 0 to Keys.Count -1 do
      begin
        FReg.CloseKey();
        ExportKey(AHKey, AKeyPath +'\'+ Keys[i], True);
      end;  //of for
    end;  //of begin

  finally
    FReg.CloseKey();
  end;  //of try
end;

{ public TRegistryFile.ExportReg

  Exports an entire Registry key (opt. recursive) and saves it as .reg file. }

procedure TRegistryFile.ExportReg(AHKey: HKEY; AKeyPath: string;
  ARecursive: Boolean = True; AFilter: TFilterDataTypes = []);
begin
  if Assigned(FOnExportBegin) then
    FOnExportBegin(Self);

  MakeHeadline();
  ExportKey(AHKey, AKeyPath, ARecursive, AFilter);
  Save();

  if Assigned(FOnExportEnd) then
    FOnExportEnd(Self);
end;

{ public TRegistryFile.ExportReg

  Exports a single Registry value and saves it as .reg file. }

procedure TRegistryFile.ExportReg(AHKey: HKEY; AKeyPath, AValueName: string);
var
  Section: string;
  Buffer: array of Byte;

begin
  try
    // Init Registry access
    FReg.RootKey := AHKey;

    // Invalid key?
    if not FReg.OpenKey(AKeyPath, False) then
      raise EParserException.Create('Error while exporting value: Key does not exist!');

    // Invalid value?
    if not FReg.ValueExists(AValueName) then
      raise EParserException.Create('Error while exporting value: Value does not exist!');

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
  end;  //of try
end;

{ public TRegistryFile.GetSection

  Returns the concat of AHKEY and AKeyPath. }

function TRegistryFile.GetSection(AHKey: HKEY; AKeyPath: string): string;
begin
  Result := TOSUtils.HKeyToStr(AHKey) +'\'+ AKeyPath;
end;

{ public TRegistryFile.MakeHeadline

  Adds the first line of an .reg file. Must be used only once. }

procedure TRegistryFile.MakeHeadline();
begin
  if (Lines = 0) then
    AddRaw('Windows Registry Editor Version 5.00');
end;

{ public TRegistryFile.ReadBoolean

  Returns a boolean value of a key in section. }

function TRegistryFile.ReadBoolean(ASection, AIdent: string): Boolean;
begin
  Result := (ReadInteger(ASection, AIdent) = 1);
end;

{ public TRegistryFile.ReadInteger

  Reads an integer from a .reg file. }

function TRegistryFile.ReadInteger(ASection, AIdent: string): Integer;
var
  StringVal: string;

begin
  Result := -1;

  if (AIdent <> '') then
  begin
    StringVal := inherited ReadString(ASection, '"'+ AIdent +'"');

    if ((StringVal <> '') and AnsiContainsStr(StringVal, 'dword:')) then
    begin
      StringVal := Copy(StringVal, 7, Length(StringVal));
      Result := TOSUtils.HexToInt(StringVal);
    end;  //of begin
  end;  //of begin
end;

{ public TRegistryFile.ReadString

  Reads a string from a .reg file. }

function TRegistryFile.ReadString(ASection, AIdent: string): string;
var
  Value: string;

begin
  if (AIdent <> '') then
  begin
    Value := inherited ReadString(ASection, '"'+ AIdent +'"');
    Value := UnescapePathDelimiter(Value);
    Result := DeleteQuoteChars(Value);
  end  //of begin
  else
    Result := '';
end;

{ public TRegistryFile.Remove

  Removes a key inside a section. }

function TRegistryFile.Remove(ASection, AIdent: string): Boolean;
begin
  Result := inherited Remove(ASection, '"'+ AIdent +'"');
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

{ public TRegistryFile.WriteExpandString

  Writes a string that has to be expanded (in hex format) to a .reg file. }

procedure TRegistryFile.WriteBinary(ASection, AIdent: string;
  var AValue: array of Byte);
begin
  WriteByte(ASection, AIdent, True, AValue);
end;

{ public TRegistryFile.WriteBoolean

  Writes an boolean value to a a key in section. }

procedure TRegistryFile.WriteBoolean(ASection, AIdent: string; AValue: Boolean);
begin
  WriteInteger(ASection, '"'+ AIdent +'"', Ord(AValue));
end;

{ public TRegistryFile.WriteInteger

  Writes an escaped integer to a .reg file. }

procedure TRegistryFile.WriteInteger(ASection, AIdent: string; AValue: Integer);
begin
  Write(ASection, AIdent, 'dword:'+ IntToHex(AValue, 8));
end;

{ public TRegistryFile.WriteExpandString

  Writes a string that has to be expanded (in hex format) to a .reg file. }

procedure TRegistryFile.WriteExpandString(ASection, AIdent, AValue: string);
var
  i, j: Integer;
  Hex: array of Byte;

begin
  // Expand 1 Byte char to 2 Byte Hex chars
  SetLength(Hex, (Length(AValue) * 2) + 1);
  i := 0;

  // Convert char to Byte
  for j := 1 to Length(AValue) do
  begin
    Hex[i] := Ord(AValue[j]);
    i := i + 2;
  end;  //of while

  // Write Hex data with linebreaks to .reg file
  WriteByte(ASection, AIdent, False, Hex);
end;

{ public TRegistryFile.WriteString

  Writes an escaped string to a .reg file. }

procedure TRegistryFile.WriteString(ASection, AIdent, AValue: string);
begin
  Write(ASection, AIdent, '"'+ EscapePathDelimiter(AValue) +'"');
end;
{$ENDIF}

end.