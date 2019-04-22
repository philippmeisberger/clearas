{ *********************************************************************** }
{                                                                         }
{ PM Code Works System Utilities Unit v1.0                                }
{                                                                         }
{ Copyright (c) 2011-2019 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.SysUtils;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows, ShellAPI, ShlObj, ActiveX;
{$ELSE}
  Process, Resource, ElfReader, VersionResource;
{$ENDIF}

const
  /// <summary>
  ///   URL to the website.
  /// </summary>
  URL_BASE     = 'https://www.pm-codeworks.de/';

  /// <summary>
  ///   Contact mail.
  /// </summary>
  MAIL_CONTACT = 'team@pm-codeworks.de';

type
  /// <summary>
  ///   Contains file version information.
  /// </summary>
  TFileVersion = record

    /// <summary>
    ///   The major version.
    /// </summary>
    Major: Cardinal;

    /// <summary>
    ///   The minor version.
    /// </summary>
    Minor: Cardinal;

    /// <summary>
    ///   The service version.
    /// </summary>
    Service: Cardinal;

    /// <summary>
    ///   The build number.
    /// </summary>
    Build: Cardinal;

    /// <summary>
    ///   Gets the complete version information of a file.
    /// </summary>
    /// <param name="AFileName">
    ///   The file.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the version information of the file could be retrieved
    ///   successfully or <c>False</c> otherwise.
    /// </returns>
    function FromFile(const AFileName: string): Boolean;

    /// <summary>
    ///   Formats the file version as string.
    /// </summary>
    /// <param name="AFormat">
    ///   Optional: The format string. NOTE: Since every version information is
    ///   decimal use <c>%d</c>.
    /// </param>
    function ToString(const AFormat: string = '%d.%d.%d.%d'): string;
  end;

{$IFDEF MSWINDOWS}
  /// <summary>
  ///   Contains information stored in a .exe file.
  /// </summary>
  TExeFileInformation = record
    /// <summary>
    ///   The comments field.
    /// </summary>
    Comments: string;

    /// <summary>
    ///   The internal name.
    /// </summary>
    InternalName: string;

    /// <summary>
    ///   The product name.
    /// </summary>
    ProductName: string;

    /// <summary>
    ///   The company name.
    /// </summary>
    CompanyName: string;

    /// <summary>
    ///   Copyright owner.
    /// </summary>
    LegalCopyright: string;

    /// <summary>
    ///   The product version.
    /// </summary>
    ProductVersion: string;

    /// <summary>
    ///   The file description.
    /// </summary>
    FileDescription: string;

    /// <summary>
    ///   Trademark information.
    /// </summary>
    LegalTrademarks: string;

    /// <summary>
    ///   Private build identifier.
    /// </summary>
    PrivateBuild: string;

    /// <summary>
    ///   The file version.
    /// </summary>
    FileVersion: string;

    /// <summary>
    ///   The original filename.
    /// </summary>
    OriginalFilename: string;

    /// <summary>
    ///   Special build identifier.
    /// </summary>
    SpecialBuild: string;

    /// <summary>
    ///   Reads the <see cref="TExeFileInformation"/> from a .exe file.
    /// </summary>
    /// <param name="AFileName">
    ///   The .exe file.
    /// </param>
    /// <returns>
    ///   <c>True</c> if information could be read or <c>False</c> otherwise.
    /// </returns>
    function FromFile(const AFileName: string): Boolean;
  end;
{$ENDIF}

/// <summary>
///   Opens a given URL.
/// </summary>
/// <param name="AUrl">
///    The URL that should be opened.
/// </param>
/// <returns>
///   <c>True</c> if the URL was successfully opened or <c>False</c> otherwise.
/// </returns>
function OpenUrl(const AUrl: string): Boolean;

{$IFDEF MSWINDOWS}
/// <summary>
///   Loads a string from a <c>STRINGTABLE</c> resource.
/// </summary>
/// <param name="AResource">
///   Path to the resource.
/// </param>
/// <param name="AIdent">
///   ID of the string.
/// </param>
/// <param name="ADefault">
///   Optional: The default string to use if loading failed.
/// </param>
/// <returns>
///   The string.
/// </returns>
function LoadResourceString(const AIdent: Word; const ADefault: string = ''): string; overload; inline;

/// <summary>
///   Loads a string from a resource.
/// </summary>
/// <param name="AResource">
///   Path to the resource.
/// </param>
/// <param name="AIdent">
///   ID of the string.
/// </param>
/// <param name="ADefault">
///   Optional: The default string to use if loading failed.
/// </param>
/// <returns>
///   The string.
/// </returns>
function LoadResourceString(const AResource: string; const AIdent: Word;
  const ADefault: string = ''): string; overload; inline;

/// <summary>
///   Loads a string from a resource.
/// </summary>
/// <param name="AInstance">
///   The instance.
/// </param>
/// <param name="AIdent">
///   ID of the string.
/// </param>
/// <param name="ADefault">
///   Optional: The default string to use if loading failed.
/// </param>
/// <returns>
///   The string.
/// </returns>
function LoadResourceString(AInstance: HINST; const AIdent: Word;
  const ADefault: string = ''): string; overload;

/// <summary>
///   Expands an environment variable.
/// </summary>
/// <param name="AVariable">
///   The variable that has to be expanded. If the function succeeds the
///   original content will be overwritten.
/// </param>
/// <returns>
///   <c>True</c> if the variable was successfully expanded or <c>False</c>
///   otherwise.
/// </returns>
function ExpandEnvironmentVar(var AVariable: string): Boolean;

/// <summary>
///   Retrieves the path of default folders identified by a CSIDL.
/// </summary>
/// <param name="ACSIDL">
///   A <c>CSIDL</c> value that is used as identifier.
/// </param>
/// <returns>
///   The folder path.
/// </returns>
/// <remarks>
///   DEPRECATED since Windows Vista! Use <c>GetKnownFolderPath()</c> instead.
/// </remarks>
function GetFolderPath(ACSIDL: Integer): string; deprecated 'Use GetKnownFolderPath()';

/// <summary>
///   Retrieves the path of known folders identified by a GUID (Windows >= Vista!).
/// </summary>
/// <param name="AFolderId">
///   A GUID that is used as identifier.
/// </param>
/// <returns>
///   The folder path.
/// </returns>
/// <remarks>
///   GUIDs are defined in <c>Winapi.KnownFolders</c> unit.
/// </remarks>
function GetKnownFolderPath(AFolderId: TGUID): string;

/// <summary>
///   Retrieves the path of the system directory used by WOW64. Note that this
///   directory is not present on a 32-bit Windows!
/// </summary>
/// <returns>
///   The path.
/// </returns>
function GetSystemWow64Directory(): string; overload;

/// <summary>
///   Disables the WOW64 filesystem redirection on 64-bit Windows for a 32-bit
///   application.
/// </summary>
/// <returns>
///   The current WOW64 filesystem redirection status. NOTE: Pass this value to
///   <see cref="RevertWow64FsRedirection"/>.
/// </returns>
/// <remarks>
///   IMPORTANT: Always revert the redirection to its original value by calling
///   <see cref="RevertWow64FsRedirection"/>!!! Otherwise strange behavior
///   can be the consequence.
/// </remarks>
function DisableWow64FsRedirection(): Boolean; inline;

/// <summary>
///   Reverts the WOW64 filesystem redirection to its original value.
/// </summary>
/// <param name="AOldValue">
///   The current WOW64 filesystem redirection status. NOTE: Return value from
///   <see cref="DisableWow64FsRedirection"/>.
/// </param>
procedure RevertWow64FsRedirection(AOldValue: Boolean); inline;

// Must be declared here to be used inline by DisableWow64FsRedirection() and RevertWow64FsRedirection()
function Wow64DisableWow64FsRedirection(out OldValue: BOOL): BOOL; stdcall;
function Wow64RevertWow64FsRedirection(OldValue: BOOL): BOOL; stdcall;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
function GetSystemWow64Directory(lpBuffer: LPTSTR; uSize: UINT): UINT; stdcall;
  external kernel32 name 'GetSystemWow64Directory'+{$IFDEF UNICODE}'W'{$ELSE}'A'{$ENDIF}; overload;
function Wow64DisableWow64FsRedirection; external kernel32 name 'Wow64DisableWow64FsRedirection';
function Wow64RevertWow64FsRedirection; external kernel32 name 'Wow64RevertWow64FsRedirection';
{$IFDEF FPC}
function SHGetKnownFolderPath(const rfid: TIID; dwFlags: DWORD; hToken: THandle;
  var ppszPath: LPWSTR): HRESULT; external shell32 name 'SHGetKnownFolderPath';
{$ENDIF}

function DisableWow64FsRedirection(): Boolean;
{$IFDEF WIN32}
var
  OldValue: BOOL;
{$ENDIF}

begin
  // WOW64 enabled (default)
  Result := True;

{$IFDEF WIN32}
  if Wow64DisableWow64FsRedirection(OldValue) then
    Result := OldValue;
{$ENDIF}
end;

procedure RevertWow64FsRedirection(AOldValue: Boolean);
begin
{$IFDEF WIN32}
  // WOW64 only present on 64-bit Windows
  Wow64RevertWow64FsRedirection(AOldValue);
{$ENDIF}
end;

function ExpandEnvironmentVar(var AVariable: string): Boolean;
var
  BufferSize: Integer;
  Buffer: string;

begin
  Result := False;

  // Get required buffer size
  BufferSize := ExpandEnvironmentStrings(PChar(AVariable), nil, 0);

  if (BufferSize > 0) then
  begin
    SetLength(Buffer, BufferSize);

    if (ExpandEnvironmentStrings(PChar(AVariable), PChar(Buffer), BufferSize) <> 0) then
    begin
      AVariable := PChar(Buffer);
      Result := True;
    end;  //of begin
  end;  //of begin
end;

function GetSystemWow64Directory(): string;
var
  Size: UINT;

begin
  SetLength(Result, MAX_PATH);
  Size := GetSystemWow64Directory(PChar(Result), Length(Result));
  SetLength(Result, Size);

  if (Size > 0) then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function GetFolderPath(ACSIDL: Integer): string;
var
  Path: array[0..MAX_PATH] of Char;

begin
  if Succeeded(SHGetFolderPath(0, ACSIDL, 0, 0, @Path)) then
    Result := IncludeTrailingPathDelimiter(Path);
end;

function GetKnownFolderPath(AFolderId: TGUID): string;
var
  Path: PWideChar;

begin
  if Succeeded(SHGetKnownFolderPath(AFolderId, 0, 0, Path)) then
  begin
    Result := IncludeTrailingPathDelimiter(string(Path));
    CoTaskMemFree(Path);
  end;  //of begin
end;

function LoadResourceString(const AIdent: Word; const ADefault: string = ''): string;
begin
  Result := LoadResourceString(HInstance, AIdent, ADefault);
end;

function LoadResourceString(const AResource: string; const AIdent: Word;
  const ADefault: string = ''): string;
begin
  Result := LoadResourceString(GetModuleHandle(PChar(AResource)), AIdent, ADefault);
end;

function LoadResourceString(AInstance: HINST; const AIdent: Word;
  const ADefault: string = ''): string;
var
  CharsCopied: Integer;

begin
  SetLength(Result, 255);
  CharsCopied := LoadString(AInstance, AIdent, PChar(Result), Length(Result));

  if (CharsCopied > 0) then
    SetLength(Result, CharsCopied)
  else
    Result := ADefault;
end;
{$ENDIF}

function OpenUrl(const AUrl: string): Boolean;
{$IFNDEF MSWINDOWS}
var
  Process: TProcess;

begin
  Process := TProcess.Create(nil);

  try
    Process.Executable := '/usr/bin/xdg-open';
    Process.Parameters.Append(AUrl);
    Process.Options := Process.Options + [poWaitOnExit];
    Process.Execute();
    Result := (Process.ExitStatus = 0);

  finally
    Process.Free;
  end;  //of try
end;
{$ELSE}
var
  ShellExecuteInfo: TShellExecuteInfo;

begin
  ZeroMemory(@ShellExecuteInfo, SizeOf(TShellExecuteInfo));

  with ShellExecuteInfo do
  begin
    cbSize := SizeOf(TShellExecuteInfo);
    Wnd := 0;
    lpVerb := 'open';
    lpFile := PChar(AUrl);
    lpDirectory := nil;
    lpParameters := nil;
    nShow := SW_SHOWDEFAULT;
  end;  //of with

  Result := {$IFDEF UNICODE}ShellExecuteExW{$ELSE}ShellExecuteExA{$ENDIF}(@ShellExecuteInfo);
end;


{ TExeFileInformation }

function TExeFileInformation.FromFile(const AFileName: string): Boolean;
var
  FileInfo: TSHFileInfo;
  VersionSize, VersionHandle: DWORD;
  BufferSize: UINT;
  VersionInfo: PChar;
  Buffer: Pointer;
  ExeFileInfo: string;

begin
  // File is .exe file
  if (SHGetFileInfo(PChar(AFileName), 0, FileInfo, SizeOf(FileInfo), SHGFI_EXETYPE) = 0) then
    Exit(False);

  VersionSize := GetFileVersionInfoSize(PChar(AFileName), VersionHandle);

  if (VersionSize = 0) then
    Exit(False);

  GetMem(VersionInfo, VersionSize);

  try
    if not GetFileVersionInfo(PChar(AFileName), VersionHandle, VersionSize, VersionInfo) then
      Exit(False);

    if not VerQueryValue(VersionInfo, '\VarFileInfo\Translation', Buffer, BufferSize) then
      Exit(False);

    ExeFileInfo := Format('\StringFileInfo\%.4x%.4x\', [LoWord(DWORD(Buffer^)), HiWord(DWORD(Buffer^))]);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'Comments'), Buffer, BufferSize) then
      Comments := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'InternalName'), Buffer, BufferSize) then
      InternalName := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'ProductName'), Buffer, BufferSize) then
      ProductName := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'CompanyName'), Buffer, BufferSize) then
      CompanyName := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'LegalCopyright'), Buffer, BufferSize) then
      LegalCopyright := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'ProductVersion'), Buffer, BufferSize) then
      ProductVersion := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'FileDescription'), Buffer, BufferSize) then
      FileDescription := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'LegalTrademarks'), Buffer, BufferSize) then
      LegalTrademarks := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'PrivateBuild'), Buffer, BufferSize) then
      PrivateBuild := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'FileVersion'), Buffer, BufferSize) then
      FileVersion := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'OriginalFilename'), Buffer, BufferSize) then
      OriginalFilename := PChar(Buffer);

    if VerQueryValue(VersionInfo, PChar(ExeFileInfo +'SpecialBuild'), Buffer, BufferSize) then
      SpecialBuild := PChar(Buffer);

    Result := True;

  finally
    FreeMem(VersionInfo, VersionSize);
  end;  //of try
end;
{$ENDIF}

{ TFileVersion }

function TFileVersion.FromFile(const AFileName: string): Boolean;
{$IFDEF MSWINDOWS}
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;

begin
  Result := False;
  VerInfoSize := GetFileVersionInfoSize(PChar(AFileName), Dummy);

  if (VerInfoSize <> 0) then
  begin
    GetMem(VerInfo, VerInfoSize);

    try
      GetFileVersionInfo(PChar(AFileName), 0, VerInfoSize, VerInfo);

      if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
      begin
        Major := LongRec(VerValue.dwFileVersionMS).Hi;
        Minor := LongRec(VerValue.dwFileVersionMS).Lo;
        Service := LongRec(VerValue.dwFileVersionLS).Hi;
        Build := LongRec(VerValue.dwFileVersionLS).Lo;
        Result := True;
      end;  //of begin

    finally
      FreeMem(VerInfo, VerInfoSize);
    end;  //of try
  end;  //of begin
end;
{$ELSE}
var
  Resources: TResources;
  ResourceReader: TElfResourceReader;
  VersionResource: TVersionResource;
  i: Integer;

begin
  Result := False;
  VersionResource := nil;
  Resources := TResources.Create;
  ResourceReader := TElfResourceReader.Create;
  i := 0;

  try
    Resources.LoadFromFile(AFileName, ResourceReader);

    while (not Assigned(VersionResource) and (i < Resources.Count)) do
    begin
      if (Resources.Items[i] is TVersionResource) then
        VersionResource := TVersionResource(Resources.Items[i]);

      Inc(i);
    end;  //of while

    if Assigned(VersionResource) then
    begin
      Major := VersionResource.FixedInfo.FileVersion[0];
      Minor := VersionResource.FixedInfo.FileVersion[1];
      Service := VersionResource.FixedInfo.FileVersion[2];
      Build := VersionResource.FixedInfo.FileVersion[3];
      Result := True;
    end;  //of begin

  finally
    ResourceReader.Free;
    Resources.FRee;
  end;  //of try
end;
{$ENDIF}

function TFileVersion.ToString(const AFormat: string = '%d.%d.%d.%d'): string;
begin
  Result := Format(AFormat, [Major, Minor, Service, Build]);
end;

end.
