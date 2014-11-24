{ *********************************************************************** }
{                                                                         }
{ PM Code Works Operating System Utilities Unit v1.7                      }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit OSUtils;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Classes, SysUtils, TLHelp32, Registry, ShellAPI, MMSystem;
{$ELSE}
  Process, Resource, ElfReader, VersionResource, LResources;
{$ENDIF}

const
  { PMCW Website URLs }
  URL_BASE = 'http://www.pm-codeworks.de/';
  URL_CONTACT = URL_BASE +'kontakt.html';

type
  { Exception class }
  EOSUtilsException = class(Exception);

{$IFDEF MSWINDOWS}  
  { TWinWOW64 }
  TWinWOW64 = class(TObject)
  private
    class function GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean; //not by me
  protected
    class function ChangeFSRedirection(AAccess: Boolean): Boolean;            //not by me
   	class function IsWindows64(): Boolean;                                    //not by me
    class function SetKeyAccessMode(): Cardinal; deprecated;
  public
    class function DenyWOW64Redirection(AAccessRight: Cardinal): Cardinal;
    class function GetArchitecture(): string;
  end;

  { TOSUtils }
  TOSUtils = class(TWinWOW64)
  protected
    class function KillProcess(AExeName: string): Boolean;
  public
    class function CheckWindows(): Boolean;
    class function CreateTempDir(const AFolderName: string): Boolean;
    class function ExecuteFile(const AFilePath: string): Boolean;
    class function ExitWindows(AAction: Word): Boolean;
    class function ExplorerReboot(): Boolean;
    class function GetBuildNumber(): Cardinal;
    class function GetTempDir(): string;
    class function GetWinDir(): string;
    class function GetWinVersion(AShowServicePack: Boolean = False): string;  //not by me
    class function HexToInt(AHexValue: string): Integer;
    class function HKeyToStr(AHKey: HKey): string;
    class function MakeUACShieldButton(AButtonHandle: HWND): Integer;
    class function OpenUrl(const AUrl: string): Boolean;
    class function PlaySound(AFileName: string; ASynchronized: Boolean = False): Boolean;
    class function PMCertExists(): Boolean;
    class function ShowAddRegistryDialog(const ARegFilePath: string): Boolean;
    class function Shutdown(): Boolean;
   	class function StrToHKey(AMainKey: string): HKEY;
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

{$IFDEF MSWINDOWS}
{ TWinWOW64 }

{ private TWinWOW64.GetNativeSystemInfo

  Returns basic system information of current used Windows. }

class function TWinWOW64.GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean;
type
  TGetNativeSystemInfo = procedure (var SystemInfo: TSystemInfo) stdcall;

var
  LibraryHandle: HMODULE;
  GetNativeSystemInfo: TGetNativeSystemInfo;

begin
  result := False;
  LibraryHandle := GetModuleHandle(kernel32);

  if (LibraryHandle <> 0) then
  begin
    GetNativeSystemInfo := GetProcAddress(LibraryHandle, 'GetNativeSystemInfo');

    if Assigned(GetNativeSystemInfo) then
    begin
      GetNativeSystemInfo(SystemInfo);
      result := True;
    end  //of begin
    else
      GetSystemInfo(SystemInfo);
  end  //of begin
  else
    GetSystemInfo(SystemInfo);
end;

{ protected TWinWOW64.ChangeFSRedirection

  Forces enabling or disabling WOW64 redirection on 64bit Windows. }

class function TWinWOW64.ChangeFSRedirection(AAccess: Boolean): Boolean;
type
  TWow64DisableWow64FsRedirection = function(var Wow64FsEnableRedirection: LongBool): LongBool; stdCall;
  TWow64EnableWow64FsRedirection = function(var Wow64FsEnableRedirection: LongBool): LongBool; stdCall;

var
  hHandle: THandle;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
  Wow64EnableWow64FsRedirection: TWow64EnableWow64FsRedirection;
  Wow64FsEnableRedirection: LongBool;

begin
  result := True;

  // WOW64 only available on 64bit Windows
  if not IsWindows64 then
    Exit;

  try
    hHandle := GetModuleHandle('kernel32.dll');
    @Wow64EnableWow64FsRedirection := GetProcAddress(hHandle, 'Wow64EnableWow64FsRedirection');
    @Wow64DisableWow64FsRedirection := GetProcAddress(hHandle, 'Wow64DisableWow64FsRedirection');

    if AAccess then
    begin
      if ((hHandle <> 0) and (@Wow64EnableWow64FsRedirection <> nil) and
        (@Wow64DisableWow64FsRedirection <> nil)) then
        Wow64DisableWow64FsRedirection(Wow64FsEnableRedirection);
    end
    else
      if ((hHandle <> 0) and (@Wow64EnableWow64FsRedirection <> nil) and
        (@Wow64DisableWow64FsRedirection <> nil)) then
        Wow64EnableWow64FsRedirection(Wow64FsEnableRedirection);

  except
    result := False;
  end;  //of try
end;

{ protected TWinWOW64.IsWindows64

  Returns if current Windows is a 32 or 64bit OS. }

class function TWinWOW64.IsWindows64(): Boolean;
var                                                     
  SystemInfo: TSystemInfo;

const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;

begin
  GetNativeSystemInfo(SystemInfo);
  result := SystemInfo.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_IA64,PROCESSOR_ARCHITECTURE_AMD64];
end;

{ public TWinWOW64.GetArchitecture

  Returns formatted string indicating if current Windows is a 32 or 64bit OS. }

class function TWinWOW64.GetArchitecture(): string;
begin
  if IsWindows64() then
    result := ' [64bit]'
  else
    result := ' [32bit]';
end;

{ protected TWinWOW64.SetKeyAccessMode

  Returns optimal access rights for writing a registry key under 64bit systems. }

class function TWinWOW64.SetKeyAccessMode(): Cardinal;
const
  KEY_WOW64_64KEY = $0100;

begin
  // Used Windows is a 64bit OS?
  if IsWindows64() then
    // Deny WOW64 redirection
    result := KEY_ALL_ACCESS or KEY_WOW64_64KEY
  else
    result := KEY_ALL_ACCESS or KEY_WRITE;
end;

{ protected TWinWOW64.DenyWOW64Redirection

  Disables the WOW64 Registry redirection temporary under 64bit systems that
  a 32 bit application can get access to the 64 bit Registry. }

class function TWinWOW64.DenyWOW64Redirection(AAccessRight: Cardinal): Cardinal;
const
  KEY_WOW64_64KEY = $0100;

begin
  // Used Windows is a 64bit OS?
  if IsWindows64() then
    // Deny WOW64 redirection
    result := KEY_WOW64_64KEY or AAccessRight
  else
    // Ignore redirection
    result := AAccessRight;
end;

{ TOSUtils }

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
    Ph := OpenProcess($0001, BOOL(0), StrToInt('$'+ProcessID));

    // Terminate found process
    result := (Integer(TerminateProcess(Ph, 0)) = 1);

  finally
    CloseHandle(snapshotHandle);
  end;  //of try
end;

{ public TOSUtils.CheckWindows

  Returns if current Windows version is newer than Windows XP. }

class function TOSUtils.CheckWindows: Boolean;
begin
  result := GetWinVersion()[1] in ['V','7','8'];
end;

{ public TOSUtils.CreateTempDir

  Creates an new folder in the temporay directory. }

class function TOSUtils.CreateTempDir(const AFolderName: string): Boolean;
begin
  result := ForceDirectories(GetTempDir() + AFolderName);
end;

{ public TOSUtils.ExecuteFile

  Executes an executable file using ShellExecute. }

class function TOSUtils.ExecuteFile(const AFilePath: string): Boolean;
begin
  result := (ShellExecute(0, 'open', PChar(AFilePath), nil, nil, SW_SHOWNORMAL) >= 32);
end;

{ public TOSUtils.ExitWindows

  Tells Windows to shutdown, reboot or log off. }

class function TOSUtils.ExitWindows(AAction: Word): Boolean;
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';

var
  TTokenHd : THandle;
  TTokenPvg : TTokenPrivileges;
  cbtpPrevious : DWORD;
  rTTokenPvg : TTokenPrivileges;
  pcbtpPreviousRequired : DWORD;
  tpResult : Boolean;

begin
  if (AAction <> EWX_LOGOFF) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
     begin
     tpResult := OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, TTokenHd);

     if tpResult then
        begin
        tpResult := LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, TTokenPvg.Privileges[0].Luid);
        TTokenPvg.PrivilegeCount := 1;
        TTokenPvg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        cbtpPrevious := SizeOf(rTTokenPvg);
        pcbtpPreviousRequired := 0;

        if tpResult then
           Windows.AdjustTokenPrivileges(TTokenHd, False, TTokenPvg, cbtpPrevious, rTTokenPvg, pcbtpPreviousRequired);
        end;  //of begin
     end;  //begin

  result := ExitWindowsEx(AAction, 0);     //EWX_SHUTDOWN, EWX_POWEROFF, (EWX_FORCE, EWX_FORCEIFHUNG)
end;

{ public TOSUtils.ExplorerReboot

  Restarts the explorer task of Windows. }

class function TOSUtils.ExplorerReboot(): Boolean;
begin
  result := KillProcess('explorer.exe');
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
          result := (dwFileVersionLS and $FFFF)
      else
        result := 0;

    finally
      FreeMem(VerInfo, VerInfoSize);
    end;   //of try
  end  //of begin
  else
    result := 0;
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
      result := VR.FixedInfo.FileVersion[3];

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
  result := SysUtils.GetEnvironmentVariable('temp');
end;

{ public TOSUtils.GetWinDir

  Returns path to install directory of Windows. }

class function TOSUtils.GetWinDir(): string;
begin
  result := SysUtils.GetEnvironmentVariable('windir');
end;

{ public TOSUtils.GetWinVersion

  Returns used Windows version with optional information about installed
  service packs. }

class function TOSUtils.GetWinVersion(AShowServicePack: Boolean = False): string;
begin
  result := 'unbekannt';

  case Win32Platform of

    // Windows 9x
    1: if (Win32MajorVersion = 4) then
          case Win32MinorVersion of
            0: result := '95';
           10: result := '98';
           90: result := 'Me';
          end;  //of case

    // Windows NT
    2: case Win32MajorVersion of            
         3: if (Win32MinorVersion = 51) then
               result := 'NT 3.51';

         4: if (Win32MinorVersion = 0) then
               result := 'NT 4';

         5: case Win32MinorVersion of
              0: result := '2000';
              1: result := 'XP';
              2: result := '.NET Server';
            end; //of case

         6: case Win32MinorVersion of
              0: result := 'Vista';
              1: result := '7';
              2: result := '8';
            end; //of case
       end; //of case
  end; //of case

  // Add information about service packs?
  if (AShowServicePack and (Win32CSDVersion <> '')) then
      result := result +' '+ Win32CSDVersion;
end;
{$ENDIF}

{ public TOSUtils.HexToInt

  Converts a Hexadecimal value to integer. }

class function TOSUtils.HexToInt(AHexValue: string): Integer;
begin
  result := StrToInt('$'+ AHexValue);
end;

{$IFDEF MSWINDOWS}
{ public TOSUtils.HKeyToStr

  Converts a HKEY into its string representation. }

class function TOSUtils.HKeyToStr(AHKey: HKey): string;
begin
  if (AHKey = HKEY_CURRENT_USER) then
    result := 'HKEY_CURRENT_USER'
  else
    if (AHKey = HKEY_LOCAL_MACHINE) then
      result := 'HKEY_LOCAL_MACHINE'
    else
      if (AHKey = HKEY_CLASSES_ROOT) then
        result := 'HKEY_CLASSES_ROOT'
      else
        raise EOSUtilsException.Create('Bad format error! Unknown HKEY!');
end;

{ public TOSUtils.MakeUACShieldButton

  Adds the Windows UAC shield to a button. }

class function TOSUtils.MakeUACShieldButton(AButtonHandle: HWND): Integer;
const
  BCM_FIRST = $1600;
  BCM_SETSHIELD = BCM_FIRST + $000C;

begin
  result := SendMessage(AButtonHandle, BCM_SETSHIELD, 0, Integer(True));
end;
{$ENDIF}

{ public TOSUtils.OpenUrl

  Opens a given URL in the default web browser. }

class function TOSUtils.OpenUrl(const AUrl: string): Boolean;
{$IFNDEF MSWINDOWS}
var
  Process : TProcess;
  
begin
  if FileExists('/usr/bin/xdg-open') then
    try
      Process := TProcess.Create(nil);

      try
        Process.Executable := '/usr/bin/xdg-open';
        Process.Parameters.Append(AUrl);
        Process.Execute;
        result := True;

      finally
        Process.Free;
      end;  //of try

    except
      result := False;
    end  //of try
  else
    result := False;
{$ELSE}
begin
  result := (ShellExecute(0, 'open', PChar(AUrl), nil, nil, SW_SHOWNORMAL) >= 32);
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
    result := False;
    SysUtils.Beep;
    Exit;
  end;  //of begin
  
{$IFDEF MSWINDOWS}
  if ASynchronized then
    SndPlaySound(PChar(AFileName), SND_SYNC)
  else
    SndPlaySound(PChar(AFileName), SND_ASYNC);

  result := True;
{$ELSE}
  Process := TProcess.Create(nil);

  try
    Process.Executable := '/usr/bin/aplay';
    Process.Parameters.Append(AFileName);

    if ASynchronized then
      Process.Options := Process.Options + [poWaitOnExit];

   Process.Execute;
   result := True;

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
  reg: TRegistry;
  Exists: Boolean;

const
  CERT_KEY = 'SOFTWARE\Microsoft\SystemCertificates\ROOT\Certificates\';
  PM_CERT_ID = '9954401782F317187F6E692C5C5DB00D008FF741';

begin
  Exists := False;
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ));
  
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    Exists := reg.OpenKeyReadOnly(CERT_KEY) and reg.KeyExists(PM_CERT_ID);

  finally
    reg.Free;
    result := Exists;
  end;  //of try
end;

{ public TOSUtils.ShowAddRegistryDialog

  Shows an dialog where user has the choice to add a *.reg file.  }

class function TOSUtils.ShowAddRegistryDialog(const ARegFilePath: string): Boolean;
var
  FileName, FilePath: PAnsiChar;

begin
  FileName := PChar(ExtractFileName(ARegFilePath));
  FilePath := PChar(ExtractFilePath(ARegFilePath));
  result := (ShellExecute(0, nil, PChar('regedit'), FileName, FilePath, SW_SHOWNORMAL) >= 32); 
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

      finally
        Process.Free;
      end;  //of try

    except
      result := False;
    end  //of try
  else
    result := False;
{$ELSE}
begin
  result := TOSUtils.ExitWindows(EWX_SHUTDOWN or EWX_FORCE);
end;

{ public TOSUtils.StrToHKey

  Converts short HKEY string into real HKEY type. } 

class function TOSUtils.StrToHKey(AMainKey: string): HKEY;
begin
  if (AMainKey = 'HKCU') then
    result := HKEY_CURRENT_USER
  else
    if (AMainKey = 'HKLM') then
      result := HKEY_LOCAL_MACHINE
    else
      if (AMainKey = 'HKCR') then
        result := HKEY_CLASSES_ROOT
      else
        raise EOSUtilsException.Create('Bad format error! Unknown rootkey!');
end;
{$ENDIF}

end.

