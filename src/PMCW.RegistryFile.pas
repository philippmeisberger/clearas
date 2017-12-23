{ *********************************************************************** }
{                                                                         }
{ Registry File Unit v1.0                                                 }
{                                                                         }
{ Copyright (c) 2011-2017 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.RegistryFile;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.IniFiles,
  System.Win.Registry;

type
  /// <summary>
  ///   Filter to export only specified Registry values.
  /// </summary>
  TRegistryFilter = set of TRegDataType;

  /// <summary>
  ///   Occurs while key is exported.
  /// </summary>
  /// <param name="Sender">
  ///   The sender.
  /// </param>
  /// <param name="AKey">
  ///   The key that is currently being exported.
  /// </param>
  /// <param name="AValue">
  ///   The value that is currently being exported.
  /// </param>
  /// <param name="AValueIndex">
  ///   Number of value that is currently being exported.
  /// </param>
  /// <param name="AValues">
  ///   Number of all values in the current key (non-recursive).
  /// </param>
  /// <param name="ACancel">
  ///   Set to <c>True</c> to abort export.
  /// </param>
  TRegistryKeyExportEvent = procedure(Sender: TObject; const AKey, AValue: string;
    AValueIndex, AValues: Integer; var ACancel: Boolean) of object;

  /// <summary>
  ///   <c>TRegistryFile</c> provides methods for reading and manipulating a
  ///   Registry file.
  /// </summary>
  /// <remarks>
  ///  <example>
  ///     The following example shows how to write a value to the file:
  ///     <code>
  ///       RegistryFile.WriteString('HKEY_CURRENT_USER\Software\Company', 'Identifier', 'Value');
  ///
  ///       // Do not forget to save the file
  ///       RegistryFile.UpdateFile();
  ///     </code>
  ///   </example>
  ///   <example>
  ///     The following example shows how to export a Registry key recursive:
  ///     <code>
  ///       RegistryFile.ExportKey(HKEY_CURRENT_USER, 'Software\Company', True);
  ///
  ///       // Do not forget to save the file
  ///       RegistryFile.UpdateFile();
  ///     </code>
  ///  </example>
  ///  <example>
  ///     The following example shows how to export a single Registry value:
  ///     <code>
  ///       RegistryFile.ExportValue(HKEY_CURRENT_USER, 'Software\Company', 'Value');
  ///
  ///       // Do not forget to save the file
  ///       RegistryFile.UpdateFile();
  ///     </code>
  ///   </example>
  /// </remarks>
  TRegistryFile = class(TMemIniFile)
  private
    FAccess64: Boolean;
    FOnExportBegin,
    FOnExportEnd: TNotifyEvent;
    FOnExporting: TRegistryKeyExportEvent;
  protected
    /// <summary>
    ///   Encodes a binary value as string.
    /// </summary>
    /// <param name="ABytes">
    ///   The binary value to encode.
    /// </param>
    /// <param name="AType">
    ///   The data type.
    /// </param>
    /// <returns>
    ///   The encoded string.
    /// </returns>
    function EncodeBinary(const ABytes: TBytes; AType: TRegDataType): string;

    /// <summary>
    ///   Decodes a string as binary value.
    /// </summary>
    /// <param name="AString">
    ///   The string to decode.
    /// </param>
    /// <param name="AType">
    ///   The data type.
    /// </param>
    /// <returns>
    ///   The decoded bytes.
    /// </returns>
    function DecodeBinary(const AString: string; AType: TRegDataType): TBytes;

    /// <summary>
    ///   Escapes the identifier.
    /// </summary>
    /// <param name="AIdent">
    ///   The identifier.
    /// </param>
    /// <returns>
    ///   The escaped identifier.
    /// </returns>
    function EscapeIdentifier(const AIdent: string): string;

    /// <summary>
    ///   Writes a little endian encoded binary value to a .reg file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    /// <param name="AType">
    ///   The data type.
    /// </param>
    procedure WriteBytes(const ASection, AIdent: string; const AValue: TBytes;
      AType: TRegDataType); overload;
  public
    const
      /// <summary>
      ///   The file extension.
      /// </summary>
      FileExtension = '.reg';

      /// <summary>
      ///   File filter for an open or save dialog.
      /// </summary>
      FileFilter    = 'Registry file *.reg|*.reg';

    /// <summary>
    ///   Constructor for creating a <c>TRegistryFile</c> instance.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the .reg file.
    /// </param>
    /// <param name="AWriteOnly">
    ///   Optional: Open file only for writing. If set to <c>True</c> file content
    ///   is not read.
    /// </param>
    constructor Create(const AFileName: string; AWriteOnly: Boolean = False);

    /// <summary>
    ///   Deletes an identifier.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    procedure DeleteKey(const ASection, AIdent: string); override;

    /// <summary>
    ///   Exports an entire Registry key.
    /// </summary>
    /// <param name="AHKey">
    ///   The root Registry key.
    /// </param>
    /// <param name="AKeyPath">
    ///   The key path relative to <c>AHKey</c>.
    /// </param>
    /// <param name="ARecursive">
    ///   Export sub-keys.
    /// </param>
    /// <param name="AFilterValues">
    ///   Optional: Export only specified Registry values.
    /// </param>
    /// <param name="AFilterTypes">
    ///   Optional: Export only specified Registry value types.
    /// </param>
    /// <returns>
    ///   Number of exported values.
    /// </returns>
    /// <exception cref="ERegistryException">
    ///    if key could not be opened.
    /// </exception>
    function ExportKey(AHKey: HKEY; const AKeyPath: string; ARecursive: Boolean;
      const AFilterValues: TStrings = nil;
      const AFilterTypes: TRegistryFilter = [rdString, rdInteger, rdExpandString, rdBinary]): Integer;

    /// <summary>
    ///   Exports a single Registry value and saves it as .reg file.
    /// </summary>
    /// <param name="AHKey">
    ///   The root Registry key.
    /// </param>
    /// <param name="AKeyPath">
    ///   The key path relative to <c>AHKey</c>.
    /// </param>
    /// <param name="AValueName">
    ///   The name of the Registry value that should be exported.
    /// </param>
    /// <exception cref="ERegistryException">
    ///    if value could not be found.
    /// </exception>
    procedure ExportValue(AHKey: HKEY; const AKeyPath, AValueName: string);

    /// <summary>
    ///   Reads a binary value from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ABuffer">
    ///   The value.
    /// </param>
    /// <param name="ASize">
    ///   The size of the value in bytes.
    /// </param>
    /// <returns>
    ///   The number of bytes read.
    /// </returns>
    function ReadBinaryData(const ASection, AIdent: string; var ABuffer; ASize: Integer): Integer;

    /// <summary>
    ///   Reads a binary value from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadBytes(const ASection, AIdent: string): TBytes;

    /// <summary>
    ///   Reads a boolean value from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ADefault">
    ///   The default value.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadBool(const ASection, AIdent: string; ADefault: Boolean): Boolean; override;

    /// <summary>
    ///   Reads a date from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ADefault">
    ///   The default value.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadDate(const ASection, AIdent: string; ADefault: TDateTime): TDateTime; override;

    /// <summary>
    ///   Reads time and date from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ADefault">
    ///   The default value.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadDateTime(const ASection, AIdent: string; ADefault: TDateTime): TDateTime; override;

    /// <summary>
    ///   Reads an expanded string value from the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <param name="ADefault">
    ///   The default value.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadExpandString(const ASection, AIdent, ADefault: string): string;

    /// <summary>
    ///   Reads a float from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ADefault">
    ///   The default value.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadFloat(const ASection, AIdent: string; ADefault: Double): Double; override;

    /// <summary>
    ///   Reads an integer value from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ADefault">
    ///   The default value.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadInteger(const ASection, AIdent: string; ADefault: Integer): Integer; override;

    /// <summary>
    ///   Reads a string value from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ADefault">
    ///   The default value.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadString(const ASection, AIdent, ADefault: string): string; override;

    /// <summary>
    ///   Reads a time from the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ADefault">
    ///   The default value.
    /// </param>
    /// <returns>
    ///   The value.
    /// </returns>
    function ReadTime(const ASection, AIdent: string; ADefault: TDateTime): TDateTime; override;

    /// <summary>
    ///   Stores the file.
    /// </summary>
    procedure UpdateFile(); override;

    /// <summary>
    ///   Writes a binary value to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="ABuffer">
    ///   The value to be written.
    /// </param>
    /// <param name="ASize">
    ///   The size of the value in bytes.
    /// </param>
    procedure WriteBinaryData(const ASection, AIdent: string; const ABuffer; ASize: Integer);

    /// <summary>
    ///   Writes a binary value to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteBytes(const ASection, AIdent: string; const AValue: TBytes); overload;

    /// <summary>
    ///   Writes a boolean value to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteBool(const ASection, AIdent: string; AValue: Boolean); override;

    /// <summary>
    ///   Writes a date to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteDate(const ASection, AIdent: string; AValue: TDateTime); override;

    /// <summary>
    ///   Writes time and date to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteDateTime(const ASection, AIdent: string; AValue: TDateTime); override;

    /// <summary>
    ///   Writes an expand string in little endian encoding to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteExpandString(const ASection, AIdent, AValue: string);

    /// <summary>
    ///   Writes a float value to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteFloat(const ASection, AIdent: string; AValue: Double); override;

    /// <summary>
    ///    Writes an integer value to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteInteger(const ASection, AIdent: string; AValue: Integer); override;

    /// <summary>
    ///   Writes a string value to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteString(const ASection, AIdent, AValue: string); override;

    /// <summary>
    ///   Writes a time to the file.
    /// </summary>
    /// <param name="ASection">
    ///   The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///   The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteTime(const ASection, AIdent: string; AValue: TDateTime); override;

    /// <summary>
    ///   Gets or sets if the 64-bit Registry should be used.
    /// </summary>
    /// <remarks>
    ///   Set to <c>True</c> to use 64-bit Registry view. Otherwise use 32-bit view.
    /// </remarks>
    property Access64: Boolean read FAccess64 write FAccess64;

    /// <summary>
    ///   Occurs while key is exported.
    /// </summary>
    property OnExporting: TRegistryKeyExportEvent read FOnExporting write FOnExporting;

    /// <summary>
    ///   Occurs when export has started.
    /// </summary>
    property OnExportBegin: TNotifyEvent read FOnExportBegin write FOnExportBegin;

    /// <summary>
    ///   Occurs when export has finished.
    /// </summary>
    property OnExportEnd: TNotifyEvent read FOnExportEnd write FOnExportEnd;
  end;

implementation

type
  TRegistryPrefix = record helper for TRegDataType
    function GetPrefix(): string;
  end;

{ TRegistryPrefix }

function TRegistryPrefix.GetPrefix(): string;
begin
  case Self of
    rdExpandString: Result := 'hex(2):';
    rdInteger:      Result := 'dword:';
    rdBinary:       Result := 'hex:';
    else            Result := '';
  end;  //of case
end;


{ TRegistryFile }

constructor TRegistryFile.Create(const AFileName: string; AWriteOnly: Boolean = False);
var
  List: TStringList;

begin
  inherited Create(AFileName, TEncoding.Unicode);
  FAccess64 := True;
  Clear();

  // Load file content
  if (not AWriteOnly and (FileName <> '') and FileExists(FileName)) then
  begin
    List := TStringList.Create;

    try
      List.LoadFromFile(FileName, Encoding);

      // Remove escaped linebreaks
      List.Text := List.Text.Replace('\'+ sLineBreak +'  ', '');
      SetStrings(List);

    finally
      List.Free;
    end;  //of try
  end;  //of begin
end;

function TRegistryFile.DecodeBinary(const AString: string; AType: TRegDataType): TBytes;
var
  i, j: Integer;
  Value: string;

begin
  if AString.StartsWith(AType.GetPrefix()) then
  begin
    Value := AString.Substring(Length(AType.GetPrefix()));
    SetLength(Result, Length(Value) div 2);
    j := 0;

    for i := 0 to Length(Result) - 1 do
    begin
      if (i + j + 2 > Length(Value)) then
        Break;

      Result[i] := StrToInt('$'+ Value.Substring(i + j, 2));
      Inc(j, 2);
    end;  //of for

    SetLength(Result, j div 2);
  end;  //of begin
end;

procedure TRegistryFile.DeleteKey(const ASection, AIdent: string);
begin
  inherited DeleteKey(ASection, EscapeIdentifier(AIdent));
end;

function TRegistryFile.EncodeBinary(const ABytes: TBytes; AType: TRegDataType): string;
var
  i: Integer;

begin
  Result := AType.GetPrefix();

  if (Length(ABytes) > 0) then
  begin
    // Convert Byte to Hex and separate 2 digits by comma
    for i := 0 to Length(ABytes) - 1 do
      Result := Result + LowerCase(IntToHex(ABytes[i], 2) +',');

    // Delete last comma
    Delete(Result, Length(Result), 1);
  end;  //of begin
end;

function TRegistryFile.EscapeIdentifier(const AIdent: string): string;
begin
  if ((AIdent = '') or (AIdent = '@')) then
    Result := '@'
  else
    Result := AIdent.QuotedString('"');
end;

function TRegistryFile.ExportKey(AHKey: HKEY; const AKeyPath: string;
  ARecursive: Boolean; const AFilterValues: TStrings = nil;
  const AFilterTypes: TRegistryFilter = [rdString, rdInteger, rdExpandString, rdBinary]): Integer;

  function ExportKeyValues(const ARegistry: TRegistry): Integer;
  var
    Values, Keys: TStringList;
    Section: string;
    i: Integer;
    Buffer: TBytes;
    Cancel: Boolean;

  begin
    Result := 0;
    Section := ExcludeTrailingPathDelimiter(ARegistry.RootKeyName +'\'+ ARegistry.CurrentPath);
    Values := TStringList.Create;

    try
      ARegistry.GetValueNames(Values);

      for i := 0 to Values.Count - 1 do
      begin
        if Assigned(FOnExporting) then
        begin
          FOnExporting(Self, Section, Values[i], i, Values.Count, Cancel);

          if Cancel then
            Break;
        end;  //of begin

        // Filter values
        if (Assigned(AFilterValues) and (AFilterValues.Count > 0) and
          (AFilterValues.IndexOf(Values[i]) = -1)) then
          Continue;

        case ARegistry.GetDataType(Values[i]) of
          rdString:
            begin
              if (rdString in AFilterTypes) then
              begin
                WriteString(Section, Values[i], ARegistry.ReadString(Values[i]));
                Inc(Result);
              end;  //of begin
            end;

          rdInteger:
            begin
              if (rdInteger in AFilterTypes) then
              begin
                WriteInteger(Section, Values[i], ARegistry.ReadInteger(Values[i]));
                Inc(Result);
              end;  //of begin
            end;

          rdExpandString:
            begin
              if (rdExpandString in AFilterTypes) then
              begin
                WriteExpandString(Section, Values[i], ARegistry.ReadString(Values[i]));
                Inc(Result);
              end;  //of begin
            end;

          rdBinary:
            begin
              if (rdBinary in AFilterTypes) then
              begin
                SetLength(Buffer, ARegistry.GetDataSize(Values[i]));
                ARegistry.ReadBinaryData(Values[i], Buffer[0], Length(Buffer));
                WriteBytes(Section, Values[i], Buffer);
                Inc(Result);
              end;  //of begin
            end;
        end;  //of case
      end;  //of begin

    finally
      FreeAndNil(Values);
    end;  //of try

    // Include subkeys?
    if (not Cancel and ARecursive and ARegistry.HasSubKeys()) then
    begin
      Keys := TStringList.Create;

      try
        ARegistry.GetKeyNames(Keys);

        // Export subkeys
        for i := 0 to Keys.Count - 1 do
        begin
          ARegistry.CloseKey();

          if ARegistry.OpenKeyReadOnly(IncludeTrailingPathDelimiter(AKeyPath) + Keys[i]) then
            Result := Result + ExportKeyValues(ARegistry);
        end;  //of for

      finally
        FreeAndNil(Keys);
      end;  //of try
    end;  //of begin
  end;

var
  Reg: TRegistry;

begin
  Result := 0;

  if FAccess64 then
    Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY)
  else
    Reg := TRegistry.Create(KEY_READ or KEY_WOW64_32KEY);

  // Notify start
  if Assigned(FOnExportBegin) then
    FOnExportBegin(Self);

  try
    Reg.RootKey := AHKey;

    // Start export
    if not Reg.OpenKeyReadOnly(AKeyPath) then
      raise ERegistryException.CreateFmt('Could not open key "%s\%s": %s', [Reg.RootKeyName, AKeyPath, Reg.LastErrorMsg]);

    Result := ExportKeyValues(Reg);

    // Notify finish
    if Assigned(FOnExportEnd) then
      FOnExportEnd(Self);

  finally
    Reg.Free;
  end;  //of try
end;

procedure TRegistryFile.ExportValue(AHKey: HKEY; const AKeyPath, AValueName: string);
var
  Filter: TStringList;

begin
  Filter := TStringList.Create;

  try
    Filter.Append(AValueName);

    if (ExportKey(AHKey, AKeyPath, False, Filter) = 0) then
      raise ERegistryException.CreateFmt('Value "%s" could not be found in "%s"!', [AValueName, AKeyPath]);

  finally
    Filter.Free;
  end;  //of try
end;

function TRegistryFile.ReadBytes(const ASection, AIdent: string): TBytes;
begin
  Result := DecodeBinary(inherited ReadString(ASection, EscapeIdentifier(AIdent), ''), rdBinary);
end;

function TRegistryFile.ReadBinaryData(const ASection, AIdent: string;
  var ABuffer; ASize: Integer): Integer;
var
  Buffer: TBytes;

begin
  Buffer := ReadBytes(ASection, AIdent);

  if (ASize > Length(Buffer)) then
    Result := Length(Buffer)
  else
    Result := ASize;

  if (Result > 0) then
    Move(Buffer[0], ABuffer, Result);
end;

function TRegistryFile.ReadBool(const ASection, AIdent: string; ADefault: Boolean): Boolean;
begin
  Result := ReadInteger(ASection, AIdent, Ord(ADefault)).ToBoolean();
end;

function TRegistryFile.ReadDate(const ASection, AIdent: string;
  ADefault: TDateTime): TDateTime;
begin
  Result := ReadDateTime(ASection, AIdent, ADefault);
end;

function TRegistryFile.ReadDateTime(const ASection, AIdent: string;
  ADefault: TDateTime): TDateTime;
begin
  if (ReadBinaryData(ASection, AIdent, Result, SizeOf(TDateTime)) <> SizeOf(TDateTime)) then
    Result := ADefault;
end;

function TRegistryFile.ReadExpandString(const ASection, AIdent, ADefault: string): string;
var
  Bytes: TBytes;

begin
  Bytes := DecodeBinary(inherited ReadString(ASection, EscapeIdentifier(AIdent), ''), rdExpandString);

  if (Length(Bytes) > 0) then
    // Remove null terminator
    Result := PChar(Encoding.GetString(Bytes))
  else
    Result := ADefault;
end;

function TRegistryFile.ReadFloat(const ASection, AIdent: string;
  ADefault: Double): Double;
begin
  if (ReadBinaryData(ASection, AIdent, Result, SizeOf(Double)) <> SizeOf(Double)) then
    Result := ADefault;
end;

function TRegistryFile.ReadInteger(const ASection, AIdent: string; ADefault: Integer): Integer;
var
  Value: string;

begin
  Result := ADefault;
  Value := inherited ReadString(ASection, EscapeIdentifier(AIdent), IntToStr(ADefault));

  if Value.StartsWith(rdInteger.GetPrefix()) then
    Result := StrToInt('$'+ Value.Substring(Length(rdInteger.GetPrefix())));
end;

function TRegistryFile.ReadString(const ASection, AIdent, ADefault: string): string;
begin
  Result := inherited ReadString(ASection, EscapeIdentifier(AIdent), ADefault);
  Result := Result.Replace('\\', '\');
  Result := Result.Replace('\"', '');
  Result := Result.DeQuotedString('"');
end;

function TRegistryFile.ReadTime(const ASection, AIdent: string;
  ADefault: TDateTime): TDateTime;
begin
  Result := ReadDateTime(ASection, AIdent, ADefault);
end;

procedure TRegistryFile.UpdateFile();
var
  List: TStringList;
  i: Integer;

begin
  List := TStringList.Create;

  try
    List.Add('Windows Registry Editor Version 5.00');
    List.Add('');
    GetStrings(List);

    // Add escaped linebreaks
    for i := 2 to List.Count - 1 do
      List[i] := WrapText(List[i], '\'+ sLineBreak +'  ', [','], 77);

    List.SaveToFile(FileName, Encoding);

  finally
    List.Free;
  end;  //of try
end;

procedure TRegistryFile.WriteBytes(const ASection, AIdent: string;
  const AValue: TBytes; AType: TRegDataType);
begin
  inherited WriteString(ASection, EscapeIdentifier(AIdent), EncodeBinary(AValue, AType));
end;

procedure TRegistryFile.WriteBinaryData(const ASection, AIdent: string;
  const ABuffer; ASize: Integer);
var
  Buffer: TBytes;

begin
  SetLength(Buffer, ASize);
  Move(ABuffer, Buffer[0], ASize);
  WriteBytes(ASection, AIdent, Buffer, rdBinary);
end;

procedure TRegistryFile.WriteBytes(const ASection, AIdent: string; const AValue: TBytes);
begin
  WriteBytes(ASection, AIdent, AValue, rdBinary);
end;

procedure TRegistryFile.WriteBool(const ASection, AIdent: string;
  AValue: Boolean);
begin
  WriteInteger(ASection, AIdent, Ord(AValue));
end;

procedure TRegistryFile.WriteDate(const ASection, AIdent: string;
  AValue: TDateTime);
begin
  WriteDateTime(ASection, AIdent, AValue);
end;

procedure TRegistryFile.WriteDateTime(const ASection, AIdent: string;
  AValue: TDateTime);
begin
  WriteBinaryData(ASection, AIdent, AValue, SizeOf(TDateTime));
end;

procedure TRegistryFile.WriteExpandString(const ASection, AIdent, AValue: string);
begin
  WriteBytes(ASection, AIdent, Encoding.GetBytes(AValue + #0), rdExpandString);
end;

procedure TRegistryFile.WriteFloat(const ASection, AIdent: string; AValue: Double);
begin
  WriteBinaryData(ASection, AIdent, AValue, SizeOf(Double));
end;

procedure TRegistryFile.WriteInteger(const ASection, AIdent: string; AValue: Integer);
begin
  inherited WriteString(ASection, EscapeIdentifier(AIdent), rdInteger.GetPrefix() + LowerCase(IntToHex(AValue, 8)));
end;

procedure TRegistryFile.WriteString(const ASection, AIdent, AValue: string);
var
  EscapedPath: string;

begin
  EscapedPath := AValue.Replace('\', '\\');
  EscapedPath := EscapedPath.Replace('"', '\"');
  inherited WriteString(ASection, EscapeIdentifier(AIdent), '"'+ EscapedPath +'"');
end;

procedure TRegistryFile.WriteTime(const ASection, AIdent: string;
  AValue: TDateTime);
begin
  WriteDateTime(ASection, AIdent, AValue);
end;

end.
