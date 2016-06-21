{ *********************************************************************** }
{                                                                         }
{ Initialization file parser Unit v1.3.1                                  }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
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

type
  EInvalidIniFormat = class(Exception);

  /// <summary>
  ///   A <c>TIniFile</c> provides methods for reading and manipulating an INI
  ///   file.
  /// </summary>
  TIniFile = class(TObject)
  private
    FFile: TStringList;
    FFileName: string;
    FSaveOnDestroy: Boolean;
    function Insert(AIndex: Integer; const AString: string): Integer;
    function FindNextItem(AStartIndex: Integer = 0; AEndIndex: Integer = -1): Integer; inline;
    function FindNextSection(AStartIndex: Integer = 0): Integer;
    function FindNextSectionItem(AStartIndex: Integer = 0): Integer;
    function GetEndOfItem(AIndex: Integer): Integer; overload;
    //function GetEndOfItem(const ASectionName, AKey: string): Integer; overload;
    function GetEndOfSection(AIndex: Integer): Integer; overload;
    function GetEndOfSection(const ASectionName: string): Integer; overload;
    function GetKey(AIndex: Integer): string;
    function GetLength(): Integer;
    function GetValue(AIndex: Integer): string;
    function Remove(AStartIndex, AEndIndex: Integer): Boolean; overload;
  protected
    /// <summary>
    ///   Adds a raw string.
    /// </summary>
    /// <remarks>
    ///   Can be used to add comments.
    /// </remarks>
    /// <returns>
    ///   The index of the line where the data was added.
    /// </returns>
    function AddRaw(const ALine: string): Integer;

    /// <summary>
    ///   Extracts the key property from a key-value-pair.
    /// </summary>
    /// <param name="AKeyValuePair">
    ///   The key-value-pair that is formatted like: <c>key=value</c>
    /// </param>
    /// <returns>
    ///   The extracted key.
    /// </returns>
    function ExtractKey(const AKeyValuePair: string): string;

    /// <summary>
    ///   Extracts the item value from a key-value-pair.
    /// </summary>
    /// <param name="AKeyValuePair">
    ///   The key-value-pair that is formatted like: <c>key=value</c>
    /// </param>
    /// <returns>
    ///   The extracted value.
    /// </returns>
    function ExtractValue(const AKeyValuePair: string): string;
  public
    /// <summary>
    ///   Constructor for creating a <c>TIniFile</c> instance.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the .ini file.
    /// </param>
    /// <param name="AOverwriteIfExists">
    ///   Optional: Allow overwrite if a file with the same name already exists.
    /// </param>
    /// <param name="ASaveOnDestroy">
    ///   Optional: Automatically save the file when <c>Free()</c> is called.
    /// </param>
    constructor Create(const AFileName: TFileName; AOverwriteIfExists: Boolean = False;
      ASaveOnDestroy: Boolean = False);

    /// <summary>
    ///   Destructor for destroying a <c>TIniFile</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Adds a new or changes an existing value.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The key of the item.
    /// </param>
    /// <param name="AValue">
    ///   The value of the item. If this value is empty the item will be removed.
    /// </param>
    procedure AddRemove(const ASectionName, AKey, AValue: string);

    /// <summary>
    ///   Adds a new section if not exist.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the section was added successfully or <c>False</c>
    ///   otherwise.
    /// </returns>
    function AddSection(const ASectionName: string): Boolean; overload;

    /// <summary>
    ///   Adds a new section with content.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AHashMap">
    ///   A line must be formatted like <c>key=value</c>.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the section was added successfully or <c>False</c>
    ///   otherwise.
    /// </returns>
    function AddSection(const ASectionName: string; AHashMap: TStrings): Boolean; overload;

    /// <summary>
    ///   Deletes all contents from the file.
    /// </summary>
    procedure Clear(); virtual;

    /// <summary>
    ///   Reads the keys of a section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKeys">
    ///    The list where the keys will be appended.
    /// </param>
    procedure GetKeys(const ASectionName: string; var AKeys: TStrings);

    /// <summary>
    ///   Reads and collects all section names.
    /// </summary>
    /// <param name="ASections">
    ///   The list where the sections will be appended.
    /// </param>
    procedure GetSections(var ASections: TStrings);

    /// <summary>
    ///   Returns the index of a key.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key to search for.
    /// </param>
    /// <returns>
    ///   The index of the found item. If the item was not found <c>-1</c> is
    ///   returned.
    /// </returns>
    function IndexOfKey(const ASectionName, AKey: string): Integer;

    /// <summary>
    ///   Returns the index of a section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section to search for.
    /// </param>
    /// <returns>
    ///   The index of the found item. If the item was not found <c>-1</c> is
    ///   returned.
    /// </returns>
    function IndexOfSection(const ASectionName: string): Integer;

    /// <summary>
    ///   Checks if a line at a specified index is a section.
    /// </summary>
    /// <param name="AIndex">
    ///   The index of the line to check.
    /// </param>
    /// <remarks>
    ///   Sections are formatted like: <c>[section]</c>
    /// </remarks>
    /// <returns>
    ///   <c>True</c> if the line is a section or <c>False</c> otherwise.
    /// </returns>
    function IsSection(AIndex: Integer): Boolean;

    /// <summary>
    ///   Checks if a key exists.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key to search for.
    /// </param>
    /// <remarks>
    ///   Keys and values are formatted like: <c>key=value</c>
    /// </remarks>
    /// <returns>
    ///   <c>True</c> if the key exists or <c>False</c> otherwise.
    /// </returns>
    function KeyExists(const ASectionName, AKey: string): Boolean;

    /// <summary>
    ///   Reads a boolean value of a key in section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key.
    /// </param>
    /// <returns>
    ///   The read value.
    /// </returns>
    function ReadBoolean(const ASectionName, AKey: string): Boolean;

    /// <summary>
    ///   Reads an integer value of a key in section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key.
    /// </param>
    /// <param name="ADefault">
    ///   Optional: A default value that is returned when item was not found.
    /// </param>
    /// <returns>
    ///   The read value.
    /// </returns>
    function ReadInteger(const ASectionName, AKey: string; ADefault: Integer = -1): Integer;

    /// <summary>
    ///   Reads a string value of a key in section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key.
    /// </param>
    /// <param name="ADefault">
    ///   Optional: A default value that is returned when item was not found.
    /// </param>
    /// <returns>
    ///   The read value.
    /// </returns>
    function ReadString(const ASectionName, AKey: string; const ADefault: string = ''): string;

    /// <summary>
    ///   Removes a key inside a section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key to be deleted.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the key was removed successfully or <c>>False</c>
    ///   otherwise.
    /// </returns>
    function Remove(const ASectionName, AKey: string): Boolean; overload;

    /// <summary>
    ///   Removes an entire section with all items.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section to be deleted.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the section was removed successfully or <c>>False</c>
    ///   otherwise.
    /// </returns>
    function RemoveSection(const ASectionName: string): Boolean;

    /// <summary>
    ///   Writes current file to disk.
    /// </summary>
    procedure Save(); overload; virtual;
{$IFDEF MSWINDOWS}
    /// <summary>
    ///    Writes current file with explicit encoding to disk.
    /// </summary>
    /// <param name="AEncoding">
    ///   The used file encoding. Recommended is <c>TEncoding.UTF8</c>.
    /// </param>
    procedure Save(AEncoding: TEncoding); overload;
{$ENDIF}
    /// <summary>
    ///   Checks if a section exists.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section to search for.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the section exists or <c>>False</c> otherwise.
    /// </returns>
    function SectionExists(const ASectionName: string): Boolean;

    /// <summary>
    ///   Writes an boolean value to a key in section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    /// <returns>
    ///   The index of the line where the item was written.
    /// </returns>
    function WriteBoolean(const ASectionName, AKey: string; AValue: Boolean): Integer;

    /// <summary>
    ///   Writes an integer value to a key in section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    /// <returns>
    ///   The index of the line where the item was written.
    /// </returns>
    function WriteInteger(const ASectionName, AKey: string; AValue: Integer): Integer;

    /// <summary>
    ///   Writes a string value to a key in section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    /// <returns>
    ///   The index of the line where the item was written.
    /// </returns>
    function WriteString(const ASectionName, AKey, AValue: string): Integer;

    /// <summary>
    ///   Writes a multi-line string value to a key in section.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The name of the key.
    /// </param>
    /// <param name="AValues">
    ///   The values to be written.
    /// </param>
    /// <returns>
    ///   The index of the line where last item was written.
    /// </returns>
    function WriteStrings(const ASectionName, AKey: string; AValues: TStrings): Integer;

    /// <summary>
    ///   Gets the filename of the current used file.
    /// </summary>
    property FileName: string read FFileName;

    /// <summary>
    ///   Gets the count of lines in the current file.
    /// </summary>
    property Lines: Integer read GetLength;

    /// <summary>
    ///   Gets the key at specified index.
    /// </summary>
    property Keys[AIndex: Integer]: string read GetKey; default;

    /// <summary>
    ///   Gets or sets if the file is saved automatically before calling
    ///   <c>Free()</c>.
    /// </summary>
    property SaveOnDestroy: Boolean read FSaveOnDestroy write FSaveOnDestroy;

    /// <summary>
    ///   Gets the value at specified index.
    /// </summary>
    property Values[AIndex: Integer]: string read GetValue;
  end;

{$IFDEF MSWINDOWS}
type
  ERegistryFileException = class(Exception);

  /// <summary>
  ///   Filter to export only specified Registry values.
  /// </summary>
  TRegistryFilter = set of TRegDataType;

  TRegistryPrefix = record helper for TRegDataType
    function GetPrefix(): string;
  end;

  /// <summary>
  ///   A <c>TRegistryFile</c> provides methods for reading and manipulating a
  ///   Registry file used by Windows. This class is especially designed for
  ///   exporting Registry keys to a .reg file that can later be imported again.
  /// </summary>
  TRegistryFile = class(TIniFile)
  private
    FOnExportBegin,
    FOnExportEnd: TNotifyEvent;
    FReg: TRegistry;
    FAccess64: Boolean;
    function EscapeIdentifier(const AIdent: string): string;
    function GetKey(AIndex: Integer): string;
    function GetValue(AIndex: Integer): string;
    procedure SetAccess(const AAccess64: Boolean);
    procedure WriteBinary(const ASection, AIdent: string; ARegBinary: Boolean;
      ABytes: array of Byte); overload;
  public
    /// <summary>
    ///   Constructor for creating a <c>TRegistryFile</c> instance.
    /// </summary>
    /// <param name="AFileName">
    ///   The absolute filename to the .reg file.
    /// </param>
    /// <param name="AOverwriteIfExists">
    ///   Optional: Allow overwrite if a file with the same name already exists.
    /// </param>
    /// <param name="ASaveOnDestroy">
    ///   Optional: Automatically save the file when <c>Free()</c> is called.
    /// </param>
    constructor Create(const AFileName: string; AOverwriteIfExists: Boolean = False;
      ASaveOnDestroy: Boolean = False);

    /// <summary>
    ///   Destructor for destroying a <c>TRegistryFile</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Adds a new or changes an existing value.
    /// </summary>
    /// <param name="ASectionName">
    ///    The name of the section.
    /// </param>
    /// <param name="AKey">
    ///    The key of the item.
    /// </param>
    /// <param name="AValue">
    ///   The value of the item. If this value is empty the item will be removed.
    /// </param>
    procedure AddRemove(const ASection, AIdent, AValue: string);

    /// <summary>
    ///   Adds a new section.
    /// </summary>
    function AddSection(AHKey: HKEY; const AKeyPath: string): Boolean; reintroduce;

    /// <summary>
    ///   Deletes all contents from the file.
    /// </summary>
    procedure Clear(); override;

    /// <summary>
    ///   Escapes all path delimiters in a path.
    /// </summary>
    /// <param name="APath">
    ///   The path.
    /// </param>
    /// <returns>
    ///   The path with escaped path delimiters.
    /// </returns>
    function EscapePathDelimiter(const APath: string): string;

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
    procedure ExportKey(AHKey: HKEY; const AKeyPath: string; ARecursive: Boolean;
      AFilterValues: TStrings = nil; AFilterTypes: TRegistryFilter = []);

    /// <summary>
    ///   Exports an entire Registry key and saves it as .reg file.
    /// </summary>
    /// <param name="AHKey">
    ///   The root Registry key.
    /// </param>
    /// <param name="AKeyPath">
    ///   The key path relative to <c>AHKey</c>.
    /// </param>
    /// <param name="ARecursive">
    ///   Optional: Export sub-keys.
    /// </param>
    /// <param name="AFilterValues">
    ///   Optional: Export only specified Registry values.
    /// </param>
    /// <param name="AFilterTypes">
    ///   Optional: Export only specified Registry value types.
    /// </param>
    /// <remarks>
    ///   The <c>OnExportBegin</c> event is issued when the export starts.
    ///   When finished <c>OnExportEnd</c> is issued.
    /// </remarks>
    procedure ExportReg(AHKey: HKEY; const AKeyPath: string; ARecursive: Boolean = True;
      AFilterValues: TStrings = nil; AFilterTypes: TRegistryFilter = []); overload;

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
    /// <exception>
    ///   <c>ERegistryFileException</c> when key or value does not exist.
    /// </exception>
    procedure ExportReg(AHKey: HKEY; const AKeyPath, AValueName: string); overload;

    /// <summary>
    ///   Builds the concatination of a <c>HKEY</c> and a key path.
    /// </summary>
    /// <param name="AHKey">
    ///   The root Registry key.
    /// </param>
    /// <param name="AKeyPath">
    ///   The key path relative to <c>AHKey</c>.
    /// </param>
    /// <remarks>
    ///   A section is formatted like: <c>[HKEY_CURRENT_USER\Software]</c>
    /// </remarks>
    /// <returns>
    ///   The formatted section.
    /// </returns>
    function GetSection(AHKey: HKEY; const AKeyPath: string): string;

    /// <summary>
    ///   Adds the first line of a Registry file.
    /// </summary>
    /// <remarks>
    ///   IMPORTANT: Must be used only once!
    /// </remarks>
    procedure MakeHeadline();

    /// <summary>
    ///   Reads a little endian encoded binary value from the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <returns>
    ///   The read value.
    /// </returns>
    function ReadBinary(const ASection, AIdent: string): TBytes;

    /// <summary>
    ///   Reads a boolean value from the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <returns>
    ///   The read value.
    /// </returns>
    function ReadBoolean(const ASection, AIdent: string): Boolean;

    /// <summary>
    ///   Reads an expanded string value from the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <returns>
    ///   The read value.
    /// </returns>
    function ReadExpandString(const ASection, AIdent: string): string;

    /// <summary>
    ///   Reads an integer value from the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <returns>
    ///   The read value.
    /// </returns>
    function ReadInteger(const ASection, AIdent: string): Integer;

    /// <summary>
    ///   Reads a string value from the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <returns>
    ///   The read value.
    /// </returns>
    function ReadString(const ASection, AIdent: string): string;

    /// <summary>
    ///   Removes an identifier inside a section.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the identifier was removed successfully or <c>False</c>
    ///   otherwise.
    /// </returns>
    function Remove(const ASection, AIdent: string): Boolean;

    /// <summary>
    ///   Writes current file to disk.
    /// </summary>
    procedure Save(); override;

    /// <summary>
    ///   Deletes the path escape chars from a path
    /// </summary>
    /// <param name="APath">
    ///   The path.
    /// </param>
    /// <remarks>
    ///   An escaped path is formatted like: <c>C:\\Windows\\explorer.exe</c>
    /// </remarks>
    /// <returns>
    ///   The string without escaped path delimiters.
    /// </returns>
    function UnescapePathDelimiter(const APath: string): string;

    /// <summary>
    ///   Writes a little endian encoded binary value to a .reg file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteBinary(const ASection, AIdent: string; AValue: TBytes); overload;

    /// <summary>
    ///   Writes an boolean value to a a key in section.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    /// <returns>
    ///   The index of the line where the item was written.
    /// </returns>
    function WriteBoolean(const ASection, AIdent: string; AValue: Boolean): Integer;

    /// <summary>
    ///   Writes an expand string in little endian encoding to the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    procedure WriteExpandString(const ASection, AIdent, AValue: string);

    /// <summary>
    ///    Writes an escaped integer to the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    /// <returns>
    ///   The index of the line where the item was written.
    /// </returns>
    function WriteInteger(const ASection, AIdent: string; AValue: Integer): Integer;

    /// <summary>
    ///   Writes an escaped string to the file.
    /// </summary>
    /// <param name="ASection">
    ///    The name of the section.
    /// </param>
    /// <param name="AIdent">
    ///    The name of the identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value to be written.
    /// </param>
    /// <returns>
    ///   The index of the line where the item was written.
    /// </returns>
    function WriteString(const ASection, AIdent, AValue: string): Integer; reintroduce;

    /// <summary>
    ///   Gets or sets if the 64-bit Registry should be used.
    /// </summary>
    property Access64: Boolean read FAccess64 write SetAccess;

    /// <summary>
    ///   Gets the key at specified index.
    /// </summary>
    property Keys[AIndex: Integer]: string read GetKey; default;

    /// <summary>
    ///   Issued by <see cref="ExportReg"/> when export has started.
    /// </summary>
    property OnExportBegin: TNotifyEvent read FOnExportBegin write FOnExportBegin;

    /// <summary>
    ///   Issued by <see cref="ExportReg"/> when export has finished.
    /// </summary>
    property OnExportEnd: TNotifyEvent read FOnExportEnd write FOnExportEnd;

    /// <summary>
    ///   Gets the value at specified index.
    /// </summary>
    property Values[AIndex: Integer]: string read GetValue;
  end;
{$ENDIF}

implementation

{ TIniFile }

constructor TIniFile.Create(const AFileName: TFileName;
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

destructor TIniFile.Destroy;
begin
  // Automatically save on destroy?
  if FSaveOnDestroy then
    Save();

  FreeAndNil(FFile);
  inherited Destroy;
end;

function TIniFile.Insert(AIndex: Integer; const AString: string): Integer;
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
      if ((Line <> '') and (Line[1] <> '#') and (Line[1] <> ';') and
        (Line[1] <> '[') and AnsiContainsStr(Line, '=')) then
      begin
        Result := i;
        Break;
      end;  //of begin
    end;  //of for
end;

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

function TIniFile.FindNextSectionItem(AStartIndex: Integer = 0): Integer;
begin
  Result := FindNextItem(AStartIndex, FindNextSection(AStartIndex));
end;

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

{function TIniFile.GetEndOfItem(const ASectionName, AKey: string): Integer;
begin
  Result := GetEndOfItem(IndexOfKey(ASectionName, AKey));
end; }

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

function TIniFile.GetEndOfSection(const ASectionName: string): Integer;
begin
  Result := GetEndOfSection(IndexOfSection(ASectionName));
end;

function TIniFile.GetKey(AIndex: Integer): string;
begin
  Result := ExtractKey(FFile[AIndex]);
end;

function TIniFile.GetLength(): Integer;
begin
  Result := FFile.Count;
end;

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

function TIniFile.AddRaw(const ALine: string): Integer;
begin
  Result := FFile.Add(ALine);
end;

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

procedure TIniFile.AddRemove(const ASectionName, AKey, AValue: string);
begin
  if (AValue <> '') then
    WriteString(ASectionName, AKey, AValue)
  else
    Remove(ASectionName, AKey);
end;

function TIniFile.AddSection(const ASectionName: string): Boolean;
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

function TIniFile.AddSection(const ASectionName: string; AHashMap: TStrings): Boolean;
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
      Insert(InsertPos + i, AHashMap[i]);

    Result := True;
  end  //of begin
  else
    Result := False;
end;

procedure TIniFile.Clear();
begin
  FFile.Clear;
end;

procedure TIniFile.GetKeys(const ASectionName: string; var AKeys: TStrings);
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

procedure TIniFile.GetSections(var ASections: TStrings);
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

function TIniFile.IndexOfKey(const ASectionName, AKey: string): Integer;
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

function TIniFile.IndexOfSection(const ASectionName: string): Integer;
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

function TIniFile.KeyExists(const ASectionName, AKey: string): Boolean;
begin
  Result := (IndexOfKey(ASectionName, AKey) <> -1);
end;

function TIniFile.ReadBoolean(const ASectionName, AKey: string): Boolean;
begin
  Result := (ReadInteger(ASectionName, AKey) = 1);
end;

function TIniFile.ReadInteger(const ASectionName, AKey: string;
  ADefault: Integer = -1): Integer;
begin
  if not TryStrToInt(ReadString(ASectionName, AKey), Result) then
    Result := ADefault;
end;

function TIniFile.ReadString(const ASectionName, AKey: string;
  const ADefault: string = ''): string;
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

function TIniFile.Remove(const ASectionName, AKey: string): Boolean;
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

function TIniFile.RemoveSection(const ASectionName: string): Boolean;
begin
  Result := Remove(IndexOfSection(ASectionName), GetEndOfSection(ASectionName));
end;

procedure TIniFile.Save();
begin
{$IFDEF MSWINDOWS}
  Save(TEncoding.UTF8);
{$ELSE}
  FFile.SaveToFile(FFileName);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TIniFile.Save(AEncoding: TEncoding);
begin
  FFile.SaveToFile(FFileName, AEncoding);
end;
{$ENDIF}

function TIniFile.SectionExists(const ASectionName: string): Boolean;
begin
  Result := (IndexOfSection(ASectionName) <> -1);
end;

function TIniFile.WriteBoolean(const ASectionName, AKey: string;
  AValue: Boolean): Integer;
begin
  Result := WriteInteger(ASectionName, AKey, Ord(AValue));
end;

function TIniFile.WriteInteger(const ASectionName, AKey: string;
  AValue: Integer): Integer;
begin
  Result := WriteString(ASectionName, AKey, IntToStr(AValue));
end;

function TIniFile.WriteString(const ASectionName, AKey, AValue: string): Integer;
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
  Result := Insert(Index, AKey +'='+ AValue);
end;

function TIniFile.WriteStrings(const ASectionName, AKey: string;
  AValues: TStrings): Integer;
var
  Index, i: Integer;

begin
  // Insert key with first line
  Index := WriteString(ASectionName, AKey, AValues[0]);

  // Add other lines
  for i := 1 to AValues.Count -1 do
  begin
    Inc(Index);
    Insert(Index, AValues[i]);
  end;  //of for

  // Insert empty line before another section
  if IsSection(Index + 1) then
    Insert(Index + 1, '');

  Result := Index;
end;

{$IFDEF MSWINDOWS}

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

destructor TRegistryFile.Destroy;
begin
  FreeAndNil(FReg);
  inherited Destroy;
end;

function TRegistryFile.EscapeIdentifier(const AIdent: string): string;
begin
  if ((AIdent = '') or (AIdent = '@')) then
    Result := '@'
  else
    Result := AIdent.QuotedString('"');
end;

function TRegistryFile.GetKey(AIndex: Integer): string;
begin
  Result := inherited GetKey(AIndex).DeQuotedString('"');
end;

function TRegistryFile.GetValue(AIndex: Integer): string;
begin
  Result := inherited GetValue(AIndex).DeQuotedString('"');
end;

procedure TRegistryFile.SetAccess(const AAccess64: Boolean);
begin
  if AAccess64 then
    FReg.Access := KEY_WOW64_64KEY or KEY_READ
  else
    FReg.Access := KEY_READ;

  FAccess64 := AAccess64;
end;

procedure TRegistryFile.WriteBinary(const ASection, AIdent: string;
  ARegBinary: Boolean; ABytes: array of Byte);
var
  i: Integer;
  Line: string;
  Lines: TStringList;

begin
  // rdBinary or rdExpandString?
  if ARegBinary then
    Line := rdBinary.GetPrefix()
  else
    Line := rdExpandString.GetPrefix();

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

procedure TRegistryFile.AddRemove(const ASection, AIdent, AValue: string);
begin
  if (AValue <> '') then
    WriteString(ASection, AIdent, AValue)
  else
    Remove(ASection, AIdent);
end;

function TRegistryFile.AddSection(AHKey: HKEY; const AKeyPath: string): Boolean;
begin
  Result := inherited AddSection(GetSection(AHKey, AKeyPath));
end;

procedure TRegistryFile.Clear();
begin
  inherited Clear();
  MakeHeadline();
end;

function TRegistryFile.EscapePathDelimiter(const APath: string): string;
begin
  // Escape path delimiter
  Result := StringReplace(APath, '\', '\\', [rfReplaceAll]);

  // Escape quote chars
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

procedure TRegistryFile.ExportKey(AHKey: HKEY; const AKeyPath: string;
  ARecursive: Boolean; AFilterValues: TStrings = nil; AFilterTypes: TRegistryFilter = []);
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

procedure TRegistryFile.ExportReg(AHKey: HKEY; const AKeyPath: string;
  ARecursive: Boolean = True; AFilterValues: TStrings = nil;
  AFilterTypes: TRegistryFilter = []);
begin
  if Assigned(FOnExportBegin) then
    FOnExportBegin(Self);

  MakeHeadline();
  ExportKey(AHKey, AKeyPath, ARecursive, AFilterValues, AFilterTypes);
  Save();

  if Assigned(FOnExportEnd) then
    FOnExportEnd(Self);
end;

procedure TRegistryFile.ExportReg(AHKey: HKEY; const AKeyPath, AValueName: string);
var
  Section: string;
  Buffer: TBytes;

begin
  try
    // Init Registry access
    FReg.RootKey := AHKey;

    // Invalid key?
    if not FReg.OpenKey(AKeyPath, False) then
      raise ERegistryFileException.Create('Error while exporting value: Key does not exist!');

    // Invalid value?
    if not FReg.ValueExists(AValueName) then
      raise ERegistryFileException.Create('Error while exporting value: Value does not exist!');

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

function TRegistryFile.GetSection(AHKey: HKEY; const AKeyPath: string): string;
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

procedure TRegistryFile.MakeHeadline();
begin
  if (Lines = 0) then
    AddRaw('Windows Registry Editor Version 5.00');
end;

function TRegistryFile.ReadBinary(const ASection, AIdent: string): TBytes;
var
  StringValue: string;
  StartIndex, EndIndex, i, j: Integer;

begin
  // Read full value
  StringValue := ReadString(ASection, AIdent);

  // No binary data?
  if (not AnsiStartsStr(rdBinary.GetPrefix(), StringValue) and
    not AnsiStartsStr(rdExpandString.GetPrefix(), StringValue)) then
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

function TRegistryFile.ReadBoolean(const ASection, AIdent: string): Boolean;
begin
  Result := (ReadInteger(ASection, AIdent) = 1);
end;

function TRegistryFile.ReadExpandString(const ASection, AIdent: string): string;
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

function TRegistryFile.ReadInteger(const ASection, AIdent: string): Integer;
var
  StringVal: string;

begin
  Result := -1;
  StringVal := inherited ReadString(ASection, EscapeIdentifier(AIdent));

  if AnsiContainsStr(StringVal, rdInteger.GetPrefix()) then
  begin
    StringVal := Copy(StringVal, 7, Length(StringVal));
    Result := StrToInt('$'+ StringVal);
  end;  //of begin
end;

function TRegistryFile.ReadString(const ASection, AIdent: string): string;
var
  Value: string;

begin
  Value := inherited ReadString(ASection, EscapeIdentifier(AIdent));
  Value := UnescapePathDelimiter(Value);
  Result := Value.DeQuotedString('"');
end;

function TRegistryFile.Remove(const ASection, AIdent: string): Boolean;
begin
  Result := inherited Remove(ASection, EscapeIdentifier(AIdent));
end;

procedure TRegistryFile.Save();
begin
  inherited Save(TEncoding.Unicode);
end;

function TRegistryFile.UnescapePathDelimiter(const APath: string): string;
begin
  // Remove escape of path delimiter
  Result := StringReplace(APath, '\\', '\', [rfReplaceAll]);

  // Remove escape of quote chars
  Result := StringReplace(Result, '\"', '"', [rfReplaceAll]);
end;

procedure TRegistryFile.WriteBinary(const ASection, AIdent: string; AValue: TBytes);
begin
  WriteBinary(ASection, AIdent, True, AValue);
end;

function TRegistryFile.WriteBoolean(const ASection, AIdent: string;
  AValue: Boolean): Integer;
begin
  Result := WriteInteger(ASection, AIdent, Ord(AValue));
end;

procedure TRegistryFile.WriteExpandString(const ASection, AIdent, AValue: string);
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
end;

function TRegistryFile.WriteInteger(const ASection, AIdent: string; AValue: Integer): Integer;
begin
  Result := inherited WriteString(ASection, EscapeIdentifier(AIdent), rdInteger.GetPrefix() + IntToHex(AValue, 8));
end;

function TRegistryFile.WriteString(const ASection, AIdent, AValue: string): Integer;
begin
  Result := inherited WriteString(ASection, EscapeIdentifier(AIdent), '"'+ EscapePathDelimiter(AValue) +'"');
end;
{$ENDIF}

end.
