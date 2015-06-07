{ *********************************************************************** }
{                                                                         }
{ PM Code Works Operating System Utilities Unit v2.1                      }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.OSUtils;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
 SysUtils,
{$IFDEF MSWINDOWS}
  Windows, Classes, TLHelp32, Registry, ShellAPI, MMSystem;
{$ELSE}
  Process, Resource, ElfReader, VersionResource, LResources;
{$ENDIF}

const
  { PMCW Website URLs }
  URL_BASE = 'http://www.pm-codeworks.de/';
  URL_CONTACT = URL_BASE +'kontakt.html';

type
  { Exception class }
  EInvalidArgument = class(Exception);

{$IFDEF MSWINDOWS}  
  { TWinWOW64 }
  TWinWOW64 = class(TObject)
  public
    class function DenyWOW64Redirection(AAccessRight: Cardinal): Cardinal; deprecated;
    class function Wow64FsRedirection(A64Bit: Boolean = True): Boolean;
    class function Wow64RegistryRedirection(AAccessRight: Cardinal;
      A64Bit: Boolean = True): Cardinal;
    class function GetArchitecture(): string;
    class function IsWindows64(): Boolean;
  end;

  { TRootKey }
  TRootKey = string[4];

  { TOSUtils }
  TOSUtils = class(TWinWOW64)
  public
    class function CreateTempDir(const AFolderName: string): Boolean;
    class function ExecuteProgram(const AProgram: string;
      AArguments: string = ''; ARunAsAdmin: Boolean = False): Boolean;
    class function ExitWindows(AAction: UINT): Boolean;
    class function ExplorerReboot(): Boolean;
    class function ExpandEnvironmentVar(var AVariable: string): Boolean;
    class function GetBuildNumber(): Cardinal;
    class function GetTempDir(): string;
    class function GetWinDir(): string;
    class function GetWinVersion(AShowServicePack: Boolean = False): string;
    class function HexToInt(AHexValue: string): Integer;
    class function HKeyToStr(AHKey: HKey; ALongFormat: Boolean = True): string;
    class function KillProcess(AExeName: string): Boolean;
    class function OpenUrl(const AUrl: string): Boolean;
    class function PlaySound(AFileName: string; ASynchronized: Boolean = False): Boolean;
    class function PMCertExists(): Boolean;
    class function Shutdown(): Boolean;
    class function StrToHKey(ARootKey: TRootKey): HKEY;
    class function WindowsVistaOrLater(): Boolean; deprecated;
  end;
{$ELSE}
  { TOSUtils }
  TOSUtils = class(TObject)
  public
    class function GetBuildNumber(): Cardinal;
    class function HexToInt(AHexValue: string): Integer;
    class function OpenUrl(const AUrl: string): Boolean;
    class function PlaySound(AFileName: string; ASynchronized: Boolean = False): Boolean;
    class function Shutdown(): Boolean;
  end;
{$ENDIF}

implementation

uses StrUtils;

{$IFDEF MSWINDOWS}
{ TWinWOW64 }

{ public TWinWOW64.Wow64FsRedirection

  Disables or reverts the WOW64 file system redirection on 64 Bit Windows. }

class function TWinWOW64.Wow64FsRedirection(A64Bit: Boolean = True): Boolean;
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

{ public TWinWOW64.Wow64RegistryRedirection

  Disables or reverts the WOW64 registry redirection on 64 Bit Windows. }

class function TWinWOW64.Wow64RegistryRedirection(AAccessRight: Cardinal;
  A64Bit: Boolean = True): Cardinal;
begin
  Result := AAccessRight;

{$IFDEF WIN64}
   if not A64Bit then
     // Enable redirection to 32 Bit registry hive
     Result := KEY_WOW64_32KEY or AAccessRight;
{$ELSE}
  if (A64Bit and TOSUtils.IsWindows64()) then
    // Enable redirection to 64 Bit registry hive
    Result := KEY_WOW64_64KEY or AAccessRight;
{$ENDIF}
end;

{ public TWinWOW64.IsWindows64

  Returns if current Windows is a 32 or 64bit OS. }

class function TWinWOW64.IsWindows64(): Boolean;
type
  TIsWow64Process = function(AHandle: THandle; var AIsWow64: BOOL): BOOL; stdcall;

var
  LibraryHandle: HMODULE;
  IsWow64: BOOL;
  IsWow64Process: TIsWow64Process;

begin
  Result := False;
  LibraryHandle := GetModuleHandle(kernel32);

  if (LibraryHandle <> 0) then
  begin
    IsWow64Process := GetProcAddress(LibraryHandle, 'IsWow64Process');

    // Loading of IsWow64Process successful?
    if Assigned(IsWow64Process) then
      // Execute IsWow64Process against process
      if IsWow64Process(GetCurrentProcess(), IsWow64) then
        Result := IsWow64;
  end;  //of begin
end;

{ public TWinWOW64.DenyWOW64Redirection

  Disables the WOW64 Registry redirection temporary under 64bit systems that
  a 32 bit application can get access to the 64 bit Registry. }

class function TWinWOW64.DenyWOW64Redirection(AAccessRight: Cardinal): Cardinal;
begin
  // Used Windows is a 64bit OS?
  if TOSUtils.IsWindows64() then
    // Deny WOW64 redirection
    Result := KEY_WOW64_64KEY or AAccessRight
  else
    // Ignore redirection
    Result := AAccessRight;
end;

{ public TWinWOW64.GetArchitecture

  Returns formatted string indicating if current Windows is a 32 or 64bit OS. }

class function TWinWOW64.GetArchitecture(): string;
begin
  if TOSUtils.IsWindows64() then
    Result := ' [64bit]'
  else
    Result := ' [32bit]';
end;


{ TOSUtils }

{ public TOSUtils.CreateTempDir

  Creates an new folder in the temporay directory. }

class function TOSUtils.CreateTempDir(const AFolderName: string): Boolean;
begin
  Result := ForceDirectories(GetTempDir() + AFolderName);
end;

{ public TOSUtils.ExecuteProgram

  Executes a program (optional as admin) using ShellExecute. }

class function TOSUtils.ExecuteProgram(const AProgram: string;
  AArguments: string = ''; ARunAsAdmin: Boolean = False): Boolean;
var
  Operation: PWideChar;

begin
  // Run as administrator?
  if ARunAsAdmin then
    Operation := 'runas'
  else
    Operation := 'open';

  Result := (ShellExecute(0, Operation, PChar(AProgram), PChar(AArguments), nil,
    SW_SHOWNORMAL) > 32);
end;

{ public TOSUtils.ExitWindows

  Tells Windows to shutdown, reboot or log off. }

class function TOSUtils.ExitWindows(AAction: UINT): Boolean;
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
  SHTDN_REASON_MAJOR_APPLICATION = $00040000;
  SHTDN_REASON_MINOR_MAINTENANCE = 1;

var
  TokenHandle: THandle;
  NewState, PreviousState: TTokenPrivileges;
  BufferLength, ReturnLength: Cardinal;
  Luid: Int64;

begin
  if ((AAction <> EWX_LOGOFF) and (Win32Platform = VER_PLATFORM_WIN32_NT)) then
  try
    if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or
      TOKEN_QUERY, TokenHandle) then
      raise Exception.Create(SysErrorMessage(GetLastError()));

    // Get LUID of shutdown privilege
    if not LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, Luid) then
      raise Exception.Create(SysErrorMessage(GetLastError()));

    // Create new shutdown privilege
    NewState.PrivilegeCount := 1;
    NewState.Privileges[0].Luid := Luid;
    NewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    BufferLength := SizeOf(PreviousState);
    ReturnLength := 0;

    // Set the shutdown privilege
    if not AdjustTokenPrivileges(TokenHandle, False, NewState, BufferLength,
      PreviousState, ReturnLength) then
      raise Exception.Create(SysErrorMessage(GetLastError()));

  finally
    CloseHandle(TokenHandle);
  end;  //of try

  Result := ExitWindowsEx(AAction, SHTDN_REASON_MAJOR_APPLICATION or
    SHTDN_REASON_MINOR_MAINTENANCE);   //EWX_SHUTDOWN, EWX_POWEROFF, (EWX_FORCE, EWX_FORCEIFHUNG)
end;

{ public TOSUtils.ExpandEnvironmentVar

  Expands an environment variable. }

class function TOSUtils.ExpandEnvironmentVar(var AVariable: string): Boolean;
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

{ public TOSUtils.ExplorerReboot

  Restarts the explorer task of Windows. }

class function TOSUtils.ExplorerReboot(): Boolean;
begin
  Result := TOSUtils.KillProcess('explorer.exe');
end;

{ public TOSUtils.GetBuildNumber

  Returns build number of current running *.exe. }

class function TOSUtils.GetBuildNumber(): Cardinal;
var
  VerInfoSize, VerValueSize, Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;

begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);

  if (VerInfoSize <> 0) then
  begin
    GetMem(VerInfo, VerInfoSize);

    try
      GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);

      if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
        with VerValue^ do
          Result := (dwFileVersionLS and $FFFF)
      else
        Result := 0;

    finally
      FreeMem(VerInfo, VerInfoSize);
    end;   //of try
  end  //of begin
  else
    Result := 0;
end;
{$ELSE}
class function TOSUtils.GetBuildNumber(): Cardinal;
var
  RS : TResources;
  E : TElfResourceReader;
  VR : TVersionResource;
  i : Cardinal;

begin
  RS := TResources.Create;
  VR := nil;
  i := 0;

  try
    E := TElfResourceReader.Create;
    Rs.LoadFromFile(ParamStr(0), E);
    E.Free;

    while (VR = nil) and (i < RS.Count) do
    begin
      if RS.Items[i] is TVersionResource then
        VR := TVersionResource(RS.Items[i]);
      Inc(i);
    end;  //of while

    if Assigned(VR) then
      Result := VR.FixedInfo.FileVersion[3];

  finally
    RS.FRee;
  end;  //of try
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
{ public TOSUtils.GetTempDir

  Returns path to the temporary directory of Windows. }

class function TOSUtils.GetTempDir(): string;
begin
  Result := SysUtils.GetEnvironmentVariable('temp');
end;

{ public TOSUtils.GetWinDir

  Returns path to install directory of Windows. }

class function TOSUtils.GetWinDir(): string;
begin
  Result := SysUtils.GetEnvironmentVariable('windir');
end;

{ public TOSUtils.GetWinVersion

  Returns used Windows version with optional information about installed
  service packs. }

class function TOSUtils.GetWinVersion(AShowServicePack: Boolean = False): string;
begin
  Result := '';
  
  // Windows NT platform
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    case Win32MajorVersion of
      5: case Win32MinorVersion of
           0: Result := '2000';
           1: Result := 'XP';
           2: Result := 'XP 64-Bit Edition';
         end;  //of case

      6: case Win32MinorVersion of
           0: Result := 'Vista';
           1: Result := '7';
           2: Result := '8';
           3: Result := '8.1';
         end;  //of case

      10: case Win32MinorVersion of
            0: Result := '10';
          end;  //of case
    end; //of case

  // Add information about service packs?
  if ((Result <> '') and AShowServicePack and (Win32CSDVersion <> '')) then
    Result := Result +' '+ Win32CSDVersion;
end;
{$ENDIF}

{ public TOSUtils.HexToInt

  Converts a Hexadecimal value to integer. }

class function TOSUtils.HexToInt(AHexValue: string): Integer;
begin
  Result := StrToInt('$'+ AHexValue);
end;

{$IFDEF MSWINDOWS}
{ public TOSUtils.HKeyToStr

  Converts a HKEY into its string representation. }

class function TOSUtils.HKeyToStr(AHKey: HKey; ALongFormat: Boolean = True): string;
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

    else                   raise EInvalidArgument.Create('Unknown HKEY!');
  end;  //of case
end;

{ public TOSUtils.KillProcess

  Terminates a given process. }

class function TOSUtils.KillProcess(AExeName: string): Boolean;
var
  Continue: Boolean;
  snapshotHandle: THandle;
  processentry32: TProcessEntry32;
  ProcessID: string;
  Ph: THandle;

begin
  // Read out all running processes
  snapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  processentry32.dwSize := SizeOf(processentry32);
  Continue := Process32First(snapshotHandle, processentry32);

  // Try to search for given process name
  try
    while ((ExtractFileName(processentry32.szExeFile) <> AExeName) and Continue) do
      Continue := Process32Next(snapshotHandle, processentry32);

    // Save process ID for found process
    ProcessID := IntToHex(processentry32.th32ProcessID, 4);

    // Get handle of found process
    Ph := OpenProcess($0001, BOOL(0), StrToInt('$'+ ProcessID));

    // Terminate found process
    Result := (Integer(TerminateProcess(Ph, 0)) = 1);

  finally
    CloseHandle(snapshotHandle);
  end;  //of try
end;
{$ENDIF}

{ public TOSUtils.OpenUrl

  Opens a given URL in the default web browser. }

class function TOSUtils.OpenUrl(const AUrl: string): Boolean;
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
  Result := TOSUtils.ExecuteProgram(AUrl);
{$ENDIF}
end;

{ public TOSUtils.PlaySound

  Plays a *.wav file. }

class function TOSUtils.PlaySound(AFileName: string;
  ASynchronized: Boolean = False): Boolean;
{$IFNDEF MSWINDOWS}
var
  Process : TProcess;
{$ENDIF}
begin
  //AFileName := ExtractFilePath(ParamStr(0)) + AFileName;

  if ((ExtractFileExt(AFileName) <> '.wav') or (not FileExists(AFileName))) then
  begin
    Result := False;
    SysUtils.Beep;
    Exit;
  end;  //of begin
  
{$IFDEF MSWINDOWS}
  if ASynchronized then
    SndPlaySound(PChar(AFileName), SND_SYNC)
  else
    SndPlaySound(PChar(AFileName), SND_ASYNC);

  Result := True;
{$ELSE}
  Process := TProcess.Create(nil);

  try
    Process.Executable := '/usr/bin/aplay';
    Process.Parameters.Append(AFileName);

    if ASynchronized then
      Process.Options := Process.Options + [poWaitOnExit];

   Process.Execute;
   Result := True;

  finally
    Process.Free;
  end;  //of try
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
{ public TOSUtils.PMCertExists

  Returns if the PM Code Works certificate is already installed. }
  
class function TOSUtils.PMCertExists(): Boolean;
var
  Reg: TRegistry;

const
  CERT_KEY = 'SOFTWARE\Microsoft\SystemCertificates\ROOT\Certificates\';
  PM_CERT_THUMBPRINT = '1350A832ED8A6A8FE8B95D2E674495021EB93A4D';

begin
  Reg := TRegistry.Create(Wow64RegistryRedirection(KEY_READ));
  
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := (Reg.OpenKeyReadOnly(CERT_KEY) and Reg.KeyExists(PM_CERT_THUMBPRINT));

  finally
    Reg.CloseKey;
    Reg.Free;
  end;  //of try
end;
{$ENDIF}

{ public TOSUtils.Shutdown

  Tells the OS to shutdown the computer. }

class function TOSUtils.Shutdown(): Boolean;
{$IFNDEF MSWINDOWS}
var
  Process: TProcess;

begin
  if FileExists('/usr/bin/dbus-send') then
    try
      Process := TProcess.Create(nil);

      try
        Process.Executable := '/usr/bin/dbus-send';

        with Process.Parameters do
        begin
          Append('--system');
          Append('--print-reply');
          Append('--dest=org.freedesktop.ConsoleKit');
          Append('/org/freedesktop/ConsoleKit/Manager');
          Append('org.freedesktop.ConsoleKit.Manager.Stop');
        end;  //of with

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
end;
{$ELSE}
begin
  Result := TOSUtils.ExitWindows(EWX_SHUTDOWN or EWX_FORCE);
end;

{ public TOSUtils.StrToHKey

  Converts short HKEY string into real HKEY type. } 

class function TOSUtils.StrToHKey(ARootKey: TRootKey): HKEY;
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
              raise EInvalidArgument.Create('Unknown HKEY: "'+ ARootKey +'"!');
end;

{ public TOSUtils.WindowsVistaOrLater

  Returns if current Windows version is equal or greater than Windows Vista. }

class function TOSUtils.WindowsVistaOrLater(): Boolean;
begin
  Result := ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6));
end;
{$ENDIF}

end.
