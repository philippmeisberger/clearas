{ *********************************************************************** }
{                                                                         }
{ PM Code Works System Utilities Unit v1.0                                }
{                                                                         }
{ Copyright (c) 2011-2017 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.SysUtils;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellAPI, Winapi.ShlObj, Winapi.ActiveX, Vcl.Forms;
{$ELSE}
  Process, StrUtils, Resource, ElfReader, VersionResource;
{$ENDIF}

const
  /// <summary>
  ///   URL to the website.
  /// </summary>
  URL_BASE    = 'http://www.pm-codeworks.de/';

  /// <summary>
  ///   URL to the report bug formular on the website.
  /// </summary>
  URL_CONTACT = URL_BASE +'kontakt.html';

type
  /// <summary>
  ///   Contains product version information.
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
  const ADefault: string = ''): string;

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
function GetSystemWow64Directory(): string;

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
{$WARN SYMBOL_PLATFORM OFF}
function GetSystemWow64DirectoryW(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall; external kernel32 name 'GetSystemWow64DirectoryW' delayed;
function Wow64DisableWow64FsRedirection; external kernel32 name 'Wow64DisableWow64FsRedirection' delayed;
function Wow64RevertWow64FsRedirection; external kernel32 name 'Wow64RevertWow64FsRedirection' delayed;
{$WARN SYMBOL_PLATFORM ON}

function DisableWow64FsRedirection(): Boolean;
{$IFDEF WIN32}
var
  OldValue: BOOL;
{$ENDIF}

begin
  // WOW64 enabled (default)
  Result := True;

{$IFDEF WIN32}
  // WOW64 only present on 64-bit Windows
  if (TOSVersion.Architecture = arIntelX64) then
  begin
    if Wow64DisableWow64FsRedirection(OldValue) then
      Result := OldValue;
  end;  //of begin
{$ENDIF}
end;

procedure RevertWow64FsRedirection(AOldValue: Boolean);
begin
{$IFDEF WIN32}
  // WOW64 only present on 64-bit Windows
  if (not AOldValue and (TOSVersion.Architecture = arIntelX64)) then
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
  Length: UINT;

begin
  // Not present on 32-bit Windows
  if (TOSVersion.Architecture <> arIntelX64) then
    Exit;

  SetLength(Result, MAX_PATH);
  Length := GetSystemWow64DirectoryW(PWideChar(Result), MAX_PATH);

  if (Length > 0) then
  begin
    SetLength(Result, Length);
    Result := IncludeTrailingPathDelimiter(Result);
  end;  //of begin
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
  // Only since Windows Vista
  if (Win32MajorVersion < 6) then
    Exit;

  if Succeeded(SHGetKnownFolderPath(AFolderId, 0, 0, Path)) then
  begin
    Result := IncludeTrailingPathDelimiter(string(Path));
    CoTaskMemFree(Path);
  end;  //of begin
end;

function LoadResourceString(const AResource: string; const AIdent: Word;
  const ADefault: string = ''): string;
var
  Module: HMODULE;
  Buffer: array[0..255] of Char;

begin
  Module := GetModuleHandle(PChar(AResource));

  if ((Module <> 0) and (LoadString(Module, AIdent, @Buffer[0], SizeOf(Buffer)) <> 0)) then
    Result := Buffer
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

  Result := ShellExecuteEx(@ShellExecuteInfo);
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
