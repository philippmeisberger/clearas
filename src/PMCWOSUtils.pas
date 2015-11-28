{ *********************************************************************** }
{                                                                         }
{ PM Code Works Operating System Utilities Unit v2.2.5                    }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWOSUtils;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Classes, Registry, ShellAPI, ShlObj, Forms, SHFolder, Knownfolders,
{$ELSE}
  Process,
{$ENDIF}
  SysUtils, StrUtils;

const
  { PMCW Website URLs }
  URL_BASE = 'http://www.pm-codeworks.de/';
  URL_CONTACT = URL_BASE +'kontakt.html';
{$IFDEF WIN64}
  PLATFORM_ARCH = ' [64-Bit]';
{$ELSE}
  PLATFORM_ARCH = ' [32-Bit]';
{$ENDIF}

type
{$IFDEF LINUX}
  EArgumentException = class(Exception);
{$ELSE}
  /// <summary>
  ///   The four letter short spelling of a <c>HKEY</c> string representation.
  /// </summary>
  TRootKey = string[4];

  /// <summary>
  ///   Creates an new folder in the temporay directory.
  /// </summary>
  /// <param name="AFolderName">
  ///   The folder name.
  /// </param>
  /// <returns>
  ///   <c>True</c> if the folder was successfully created or <c>False</c>
  ///   otherwise.
  /// </returns>
  function CreateTempDir(const AFolderName: string): Boolean;

  /// <summary>
  ///   Executes a program using ShellExecuteEx.
  /// </summary>
  /// <param name="AProgram">
  ///   The program to execute.
  /// </param>
  /// <param name="AArguments">
  ///   Optional arguments passed to the program.
  /// </param>
  /// <param name="AShow">
  ///   The show mode. Default is <c>SW_SHOWNORMAL</c>.
  /// </param>
  /// <param name="ARunAsAdmin">
  ///   If set to <c>True</c> execute the program with admin access rights
  ///   otherwise with normal user access rights.
  /// </param>
  /// <param name="AWait">
  ///   If set to <c>True</c> wait until the program has finished. Otherwise
  ///   continue directly. NOTE: Can freeze the main program if this function
  ///   is called by the main thread.
  /// </param>
  /// <returns>
  ///   <c>True</c> if the program was successfully launched or <c>False</c>
  ///   otherwise.
  /// </returns>
  function ExecuteProgram(const AProgram: string; AArguments: string = '';
    AShow: Integer = SW_SHOWNORMAL; ARunAsAdmin: Boolean = False;
    AWait: Boolean = False): Boolean;

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
  ///    A <c>CSIDL</c> value that is used as identifier.
  /// </param>
  /// <returns>
  ///   The folder path.
  /// </returns>
  function GetFolderPath(ACSIDL: Integer): string; overload; deprecated 'Use GetKnownFolderPath()';

  /// <summary>
  ///   Retrieves the path of default folders identified by a CSIDL.
  /// </summary>
  /// <param name="ACSIDL">
  ///    A <c>CSIDL</c> value that is used as identifier.
  /// </param>
  /// <param name="AFolderPath">
  ///   The folder path.
  /// </param>
  /// <returns>
  ///   <c>True</c> if the folder could be retrieved successfully or <c>False</c>
  ///   otherwise.
  /// </returns>
  function GetFolderPath(ACSIDL: Integer; out AFolderPath: string): Boolean; overload; deprecated 'Use GetKnownFolderPath()';

  /// <summary>
  ///   Retrieves the path of known folders identified by a GUID (Windows >= Vista!).
  /// </summary>
  /// <param name="AFolderId">
  ///    A GUID that is used as identifier.
  /// </param>
  /// <returns>
  ///   The folder path.
  /// </returns>
  function GetKnownFolderPath(AFolderId: TGUID): string; overload;

  /// <summary>
  ///   Retrieves the path of known folders identified by a GUID (Windows >= Vista!).
  /// </summary>
  /// <param name="AFolderId">
  ///    A GUID that is used as identifier.
  /// </param>
  /// <param name="AFolderPath">
  ///   The folder path.
  /// </param>
  /// <returns>
  ///   <c>True</c> if the folder could be retrieved successfully or <c>False</c>
  ///   otherwise.
  /// </returns>
  function GetKnownFolderPath(AFolderId: TGUID; out AFolderPath: string): Boolean; overload;

  /// <summary>
  ///   Retrieves the path of the system directory used by WOW64. Note that this
  ///   directory is not present on 32-bit Windows!
  /// </summary>
  /// <param name="ASystemWow64Directory">
  ///   A string which should receive the system directory used by WOW64.
  /// </param>
  /// <returns>
  ///   <c>True</c> if the folder could be retrieved successfully or <c>False</c>
  ///   otherwise.
  /// </returns>
  function GetSystemWow64Directory(out ASystemWow64Directory: string): Boolean;

  /// <summary>
  ///   Retrieves the path to the temporary directory of Windows.
  /// </summary>
  /// <returns>
  ///   The path.
  /// </returns>
  function GetTempDir(): string;

  /// <summary>
  ///   Converts a <c>HKEY</c> into its string representation.
  /// </summary>
  /// <param name="AHKey">
  ///   The <c>HKEY</c>.
  /// </param>
  /// <param name="ALongFormat">
  ///   If set to <c>True</c> the complete string representaion is returned.
  ///   Otherwise only the four main letters e.g. HKLM are returned.
  /// </param>
  /// <returns>
  ///   The string representation.
  /// </returns>
  /// <remarks>
  ///   Raises a <see cref="EArgumentException"/> if the <c>HKEY</c> is invalid.
  /// </remarks>
  function HKeyToStr(AHKey: HKey; ALongFormat: Boolean = True): string;
{$ENDIF}

  /// <summary>
  ///   Opens a given URL in the default web browser.
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
  ///   Converts short <c>HKEY</c> string representation into real <c>HKEY</c> type.
  /// </summary>
  /// <param name="ARootKey">
  ///    A <see cref="TRootKey"/> short string.
  /// </param>
  /// <returns>
  ///   The <c>HKEY</c>.
  /// </returns>
  /// <remarks>
  ///   Raises a <see cref="EArgumentException"/> if the root key is invalid.
  /// </remarks>
  function StrToHKey(const ARootKey: TRootKey): HKEY;

  /// <summary>
  ///   Disables or reverts the WOW64 filesystem redirection on 64-bit Windows.
  /// </summary>
  /// <param name="A64Bit">
  ///    If set to <c>True</c> use the 64-bit filesystem. Otherwise use the
  ///    32-bit.
  /// </param>
  /// <returns>
  ///   <c>True</c> if the filesystem redirection was successfully or <c>False</c>
  ///   otherwise.
  /// </returns>
  /// <remarks>
  ///
  /// </remarks>
  function Wow64FsRedirection(A64Bit: Boolean = True): Boolean;

  /// <summary>
  ///   Gets an access rights mask to disable or revert the WOW64 registry
  ///   redirection on 64-bit Windows.
  /// </summary>
  /// <param name="AAccessRight">
  ///   The access right to use, e.g. <c>KEY_READ</c>.
  /// </param>
  /// <param name="A64Bit">
  ///    If set to <c>True</c> use the 64-bit registry. Otherwise use the 32-bit.
  /// </param>
  /// <returns>
  ///    The access mask.
  /// </returns>
  function Wow64RegistryRedirection(AAccessRight: LongWord;
    A64Bit: Boolean = True): LongWord;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
function CreateTempDir(const AFolderName: string): Boolean;
begin
  Result := ForceDirectories(GetTempDir() + AFolderName);
end;

function ExecuteProgram(const AProgram: string; AArguments: string = '';
  AShow: Integer = SW_SHOWNORMAL; ARunAsAdmin: Boolean = False;
  AWait: Boolean = False): Boolean;
var
  Info: TShellExecuteInfo;
  ExitCode: Cardinal;

begin
  FillChar(Info, SizeOf(Info), #0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_NOCLOSEPROCESS;

  // Run as administrator?
  if ARunAsAdmin then
    Info.lpVerb := 'runas'
  else
    Info.lpVerb := 'open';

  Info.lpFile := PChar(AProgram);
  Info.lpParameters := PChar(AArguments);
  Info.nShow := AShow;
  Result := ShellExecuteEx(@Info);

  if (Result and AWait) then
  begin
    while (WaitForSingleObject(Info.hProcess, 100) = WAIT_TIMEOUT) do
      Application.ProcessMessages;

    if GetExitCodeProcess(Info.hProcess, ExitCode) then
      Result := (ExitCode = 0)
    else
      Result := False;
  end;  //of begin
end;

function ExpandEnvironmentVar(var AVariable: string): Boolean;
var
  BufferSize: Integer;
  Buffer: array of Char;

begin
  Result := False;

  // Get required buffer size
  BufferSize := ExpandEnvironmentStrings(PChar(AVariable), nil, 0);

  if (BufferSize > 0) then
  begin
    SetLength(Buffer, BufferSize);

    if (ExpandEnvironmentStrings(PChar(AVariable), PChar(Buffer), BufferSize) <> 0) then
    begin
      AVariable := StrPas(PChar(Buffer));
      Result := True;
    end;  //of begin
  end;  //of begin
end;

function GetFolderPath(ACSIDL: Integer): string;
begin
  GetFolderPath(ACSIDL, Result);
end;

function GetFolderPath(ACSIDL: Integer; out AFolderPath: string): Boolean;
var
  Path: array[0..MAX_PATH] of Char;

begin
  Result := False;

  if Succeeded(SHGetFolderPath(0, ACSIDL, 0, 0, @Path)) then
  begin
    AFolderPath := IncludeTrailingBackslash(Path);
    Result := True;
  end;  //of begin
end;

function GetKnownFolderPath(AFolderId: TGUID): string;
begin
  GetKnownFolderPath(AFolderId, Result);
end;

function GetKnownFolderPath(AFolderId: TGUID; out AFolderPath: string): Boolean;
var
  Path: PChar;

begin
  Result := False;

  if (Win32MajorVersion < 6) then
    Exit;

  if Succeeded(SHGetKnownFolderPath(AFolderId, 0, 0, Path)) then
  begin
    AFolderPath := IncludeTrailingBackslash(string(Path));
    Result := True;
  end;  //of begin
end;

function GetSystemWow64Directory(out ASystemWow64Directory: string): Boolean;
{$IFDEF WIN64}
type
  TGetSystemWow64Directory = function(lpBuffer: LPTSTR; uSize: UINT): UINT; stdcall;

var
  LibraryHandle: HMODULE;
  GetSystemWow64Directory: TGetSystemWow64Directory;
  Directory: string;
  Length: Cardinal;

begin
  Result := False;

  // Init handle
  LibraryHandle := GetModuleHandle(kernel32);

  if (LibraryHandle <> 0) then
  begin
  {$IFDEF UNICODE}
    GetSystemWow64Directory := GetProcAddress(LibraryHandle, 'GetSystemWow64DirectoryW');
  {$ELSE}
    GetSystemWow64Directory := GetProcAddress(LibraryHandle, 'GetSystemWow64DirectoryA');
  {$ENDIF}

    // Loading of GetSystemWow64Directory successful?
    if Assigned(GetSystemWow64Directory) then
    begin
      SetLength(Directory, MAX_PATH);
      Length := GetSystemWow64Directory(PChar(Directory), MAX_PATH);

      if (Length > 0) then
      begin
        SetLength(Directory, Length);
        ASystemWow64Directory := IncludeTrailingBackslash(Directory);
        Result := True;
      end;  //of begin
    end;  //of begin
  end;  //of begin
{$ELSE}
begin
  // Not present on 32-bit Windows
  Result := False;
{$ENDIF}
end;

function GetTempDir(): string;
var
  Path: string;

begin
  Path := '%TEMP%';

  if ExpandEnvironmentVar(Path) then
    Result := IncludeTrailingBackslash(Path);
end;

function HKeyToStr(AHKey: HKey; ALongFormat: Boolean = True): string;
begin
  // DONE: Using a case leads to warning W1012
  if (AHKey = HKEY_CLASSES_ROOT) then
  begin
    if ALongFormat then
      Result := 'HKEY_CLASSES_ROOT'
    else
      Result := 'HKCR';
  end  //of begin
  else
  if (AHKey = HKEY_CURRENT_USER) then
  begin
    if ALongFormat then
      Result := 'HKEY_CURRENT_USER'
    else
      Result := 'HKCU';
  end  //of begin
  else
  if (AHKey = HKEY_LOCAL_MACHINE) then
  begin
    if ALongFormat then
      Result := 'HKEY_LOCAL_MACHINE'
    else
      Result := 'HKLM';
  end  //of begin
  else
  if (AHKey = HKEY_USERS) then
  begin
    if ALongFormat then
      Result := 'HKEY_USERS'
    else
      Result := 'HKU';
  end  //of begin
  else
  if (AHKey = HKEY_PERFORMANCE_DATA) then
  begin
    if ALongFormat then
      Result := 'HKEY_PERFORMANCE_DATA'
    else
      Result := 'HKPD';
  end  //of begin
  else
  if (AHKey = HKEY_CURRENT_CONFIG) then
  begin
    if ALongFormat then
      Result := 'HKEY_CURRENT_CONFIG'
    else
      Result := 'HKCC';
  end  //of begin
  else
    raise EArgumentException.Create('Unknown HKEY!');
end;
{$ENDIF}

function OpenUrl(const AUrl: string): Boolean;
{$IFNDEF MSWINDOWS}
var
  Process : TProcess;
{$ENDIF}
begin
  if not (AnsiStartsText('http://', AUrl) or AnsiStartsText('https://', AUrl)) then
  begin
    Result := False;
    Exit;
  end;  //of begin

{$IFNDEF MSWINDOWS}
  if FileExists('/usr/bin/xdg-open') then
    try
      Process := TProcess.Create(nil);

      try
        Process.Executable := '/usr/bin/xdg-open';
        Process.Parameters.Append(AUrl);
        Process.Execute;
        Result := True;

      finally
        Process.Free;
      end;  //of try

    except
      Result := False;
    end  //of try
  else
    Result := False;
{$ELSE}
  Result := ExecuteProgram(AUrl);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
function StrToHKey(const ARootKey: TRootKey): HKEY;
begin
  if (ARootKey = 'HKCR') then
    Result := HKEY_CLASSES_ROOT
  else
    if (ARootKey = 'HKCU') then
      Result := HKEY_CURRENT_USER
    else
      if (ARootKey = 'HKLM') then
        Result := HKEY_LOCAL_MACHINE
      else
        if (ARootKey = 'HKU') then
          Result := HKEY_USERS
        else
          if (ARootKey = 'HKPD') then
            Result := HKEY_PERFORMANCE_DATA
          else
            if (ARootKey = 'HKCC') then
              Result := HKEY_CURRENT_CONFIG
            else
              raise EArgumentException.Create('Unknown HKEY: "'+ string(ARootKey) +'"!');
end;

function Wow64FsRedirection(A64Bit: Boolean = True): Boolean;
{$IFDEF WIN32}
type
  TWow64DisableWow64FsRedirection = function(OldValue: Pointer): BOOL; stdcall;
  TWow64RevertWow64FsRedirection = function(OldValue: Pointer): BOOL; stdcall;

{$IFNDEF DEBUG}
var
  LibraryHandle: HMODULE;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
  Wow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection;
{$ENDIF}
begin
  Result := False;

{$IFNDEF DEBUG}
  // Init handle
  LibraryHandle := GetModuleHandle(kernel32);

  if (LibraryHandle <> 0) then
  begin
    if A64Bit then
    begin
      Wow64DisableWow64FsRedirection := GetProcAddress(LibraryHandle,
        'Wow64DisableWow64FsRedirection');

      // Loading of Wow64DisableWow64FsRedirection successful?
      if Assigned(Wow64DisableWow64FsRedirection) then
        Result := Wow64DisableWow64FsRedirection(nil);
    end  //of begin
    else
       begin
         Wow64RevertWow64FsRedirection := GetProcAddress(LibraryHandle,
           'Wow64RevertWow64FsRedirection');

         // Loading of Wow64RevertWow64FsRedirection successful?
         if Assigned(Wow64RevertWow64FsRedirection) then
           Result := Wow64RevertWow64FsRedirection(nil);
       end;  //of begin
  end;  //of begin
{$ENDIF}
{$ELSE}
begin
  // Nothing redirected on 64 bit Windows!
  Result := True;
{$ENDIF}
end;

function Wow64RegistryRedirection(AAccessRight: LongWord;
  A64Bit: Boolean = True): LongWord;
begin
  Result := AAccessRight;

{$IFDEF WIN64}
   if not A64Bit then
     // Enable redirection to 32 Bit registry hive
     Result := Result or KEY_WOW64_32KEY;
{$ELSE}
  if (A64Bit and (TOSVersion.Architecture = arIntelX64)) then
    // Enable redirection to 64 Bit registry hive
    Result := Result or KEY_WOW64_64KEY;
{$ENDIF}
end;
{$ENDIF}

end.
