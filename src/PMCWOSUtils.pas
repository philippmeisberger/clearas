{ *********************************************************************** }
{                                                                         }
{ PM Code Works Operating System Utilities Unit v2.2                      }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWOSUtils;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Classes, Registry, ShellAPI,
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
  { Exception class }
  EArgumentException = class(Exception);
{$ELSE}
  { TRootKey }
  TRootKey = string[4];

  function CreateTempDir(const AFolderName: string): Boolean;
  function ExecuteProgram(const AProgram: string;
    AArguments: string = ''; ARunAsAdmin: Boolean = False): Boolean;
  function ExpandEnvironmentVar(var AVariable: string): Boolean;
  function GetTempDir(): string;
  function GetUserAppDataDir(): string;
  function GetWinDir(): string;
  function HKeyToStr(AHKey: HKey; ALongFormat: Boolean = True): string;
{$ENDIF}
  function OpenUrl(const AUrl: string): Boolean;
{$IFDEF MSWINDOWS}
  function StrToHKey(ARootKey: TRootKey): HKEY;
  function Wow64FsRedirection(A64Bit: Boolean = True): Boolean;
  function Wow64RegistryRedirection(AAccessRight: Cardinal;
    A64Bit: Boolean = True): Cardinal;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{ CreateTempDir

  Creates an new folder in the temporay directory. }

function CreateTempDir(const AFolderName: string): Boolean;
begin
  Result := ForceDirectories(GetTempDir() + AFolderName);
end;

{ ExecuteProgram

  Executes a program (optional as admin) using ShellExecute. }

function ExecuteProgram(const AProgram: string;
  AArguments: string = ''; ARunAsAdmin: Boolean = False): Boolean;
var
  Operation: PChar;

begin
  // Run as administrator?
  if ARunAsAdmin then
    Operation := 'runas'
  else
    Operation := 'open';

  Result := (ShellExecute(0, Operation, PChar(AProgram), PChar(AArguments), nil,
    SW_SHOWNORMAL) > 32);
end;

{ ExpandEnvironmentVar

  Expands an environment variable. }

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

{ GetTempDir

  Returns path to the temporary directory of Windows. }

function GetTempDir(): string;
begin
  Result := SysUtils.GetEnvironmentVariable('temp');
end;

{ GetUserAppDataDir

  Returns the path to users application data directory. }

function GetUserAppDataDir(): string;
var
  Path: string;

begin
  Path := '%APPDATA%';

  if ExpandEnvironmentVar(Path) then
    Result := IncludeTrailingBackslash(Path);
end;

{ GetWinDir

  Returns path to install directory of Windows. }

function GetWinDir(): string;
begin
  Result := SysUtils.GetEnvironmentVariable('windir');
end;

{ HKeyToStr

  Converts a HKEY into its string representation. }

function HKeyToStr(AHKey: HKey; ALongFormat: Boolean = True): string;
begin
  case AHKey of
    HKEY_CLASSES_ROOT:     if ALongFormat then
                             Result := 'HKEY_CLASSES_ROOT'
                           else
                             Result := 'HKCR';

    HKEY_CURRENT_USER:     if ALongFormat then
                             Result := 'HKEY_CURRENT_USER'
                           else
                             Result := 'HKCU';

    HKEY_LOCAL_MACHINE:    if ALongFormat then
                             Result := 'HKEY_LOCAL_MACHINE'
                           else
                             Result := 'HKLM';

    HKEY_USERS:            if ALongFormat then
                             Result := 'HKEY_USERS'
                           else
                             Result := 'HKU';

    HKEY_PERFORMANCE_DATA: if ALongFormat then
                             Result := 'HKEY_PERFORMANCE_DATA'
                           else
                             Result := 'HKPD';

    HKEY_CURRENT_CONFIG:   if ALongFormat then
                             Result := 'HKEY_CURRENT_CONFIG'
                           else
                             Result := 'HKCC';

    else                   raise EArgumentException.Create('Unknown HKEY!');
  end;  //of case
end;
{$ENDIF}

{ OpenUrl

  Opens a given URL in the default web browser. }

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
{ StrToHKey

  Converts short HKEY string into real HKEY type. }

function StrToHKey(ARootKey: TRootKey): HKEY;
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
              raise EArgumentException.Create('Unknown HKEY: "'+ ARootKey +'"!');
end;

{ Wow64FsRedirection

  Disables or reverts the WOW64 file system redirection on 64 Bit Windows. }

function Wow64FsRedirection(A64Bit: Boolean = True): Boolean;
{$IFDEF WIN32}
type
  TWow64DisableWow64FsRedirection = function(OldValue: Pointer): BOOL; stdcall;
  TWow64RevertWow64FsRedirection = function(OldValue: Pointer): BOOL; stdcall;

var
  LibraryHandle: HMODULE;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
  Wow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection;

begin
  Result := False;

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
{$ELSE}
begin
  // Nothing redirected on 64 bit Windows!
  Result := True;
{$ENDIF}
end;

{ Wow64RegistryRedirection

  Disables or reverts the WOW64 registry redirection on 64 Bit Windows. }

function Wow64RegistryRedirection(AAccessRight: Cardinal;
  A64Bit: Boolean = True): Cardinal;
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
