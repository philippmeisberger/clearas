{ *********************************************************************** }
{                                                                         }
{ Windows Utilities Unit v1.2                                             }
{                                                                         }
{ Copyright (c) 2011-2013 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit WinUtils;

interface

uses
  Windows, Classes, SysUtils, TLHelp32;
 
type
  { Exception class }
  EWinUtilsException = class(Exception);
  
  { WOW64 Utils class }
  TWinWOW64 = class(TObject)
  private
    class function GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean; //not by me
  protected
    class function ChangeFSRedirection(AAccess: Boolean): Boolean;            //not by me
    class function SetKeyAccessMode(): Cardinal;
  public
    class function GetArchitecture(): string;
    class function IsWindows64(): Boolean;                                    //not by me
  end;

  { General Utils class }
  TWinUtils = class(TWinWOW64)
  protected
    class function KillProcess(AExeName: string): Boolean;
  public
    class function CheckWindows(): Boolean;
    class function ExitWindows(AAction: Word): Boolean;
    class function ExplorerReboot(): Boolean;
    class function GetBuildNumber(): Word;
  	class function GetWinDir(): string;
    class function GetWinVersion(AShowServicePack: Boolean = false): string;   //not by me
    class function HKeyToStr(AHKey: HKey): string;
    class function LockWindows(): Boolean;
   	class function StrToHKey(AMainKey: string): HKEY;
  end;
  
implementation
  
{ TWinWOW64 }
class function TWinWOW64.GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean;  //not by me
type
  TGetNativeSystemInfo = procedure (var SystemInfo: TSystemInfo) stdcall;

var
  LibraryHandle: HMODULE;
  GetNativeSystemInfo: TGetNativeSystemInfo;

begin
  result := false;
  LibraryHandle := GetModuleHandle(kernel32);

  if (LibraryHandle <> 0) then
     begin
     GetNativeSystemInfo := GetProcAddress(LibraryHandle,'GetNativeSystemInfo');

     if Assigned(GetNativeSystemInfo) then
        begin
        GetNativeSystemInfo(SystemInfo);
        result := true;
        end //of begin
     else
        GetSystemInfo(SystemInfo);
     end //of begin
  else
     GetSystemInfo(SystemInfo);
end;

{ protected }
class function TWinWOW64.ChangeFSRedirection(AAccess: Boolean): Boolean;  //32Bit Zugriffe auf
type                                                                      //64Bit Systemen emulieren
  TWow64DisableWow64FsRedirection = function(var Wow64FsEnableRedirection: LongBool): LongBool; stdCall;
  TWow64EnableWow64FsRedirection = function(var Wow64FsEnableRedirection: LongBool): LongBool; stdCall;

var
  hHandle: THandle;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
  Wow64EnableWow64FsRedirection: TWow64EnableWow64FsRedirection;
  Wow64FsEnableRedirection: LongBool;

begin
  result := true;

  if not IsWindows64 then           //kein 64-Bit-Win?
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
    result := false;
  end;  //of except
end;


class function TWinWOW64.IsWindows64(): Boolean;        //pr�fen, ob 64Bit System
var                                                     
  ASystemInfo: TSystemInfo;

const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;

begin
  GetNativeSystemInfo(ASystemInfo);
  result := ASystemInfo.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_IA64,PROCESSOR_ARCHITECTURE_AMD64];
end;

{ public }
class function TWinWOW64.GetArchitecture(): string;
begin
  if IsWindows64() then
     result := ' [64bit]'
  else
     result := ' [32bit]';
end;


class function TWinWOW64.SetKeyAccessMode: Cardinal;    //Key-Access f�r Schreibzugriff
const                                                   //auf Registry
  KEY_WOW64_64KEY = $0100;

begin
  if IsWindows64() then                                 //32 oder 64bit Windows?
     result := KEY_ALL_ACCESS or KEY_WOW64_64KEY
  else
     result := KEY_ALL_ACCESS or KEY_WRITE;
end;
{ of TWinWOW64 }

{######################################################################################}
{ TWinUtils }

class function TWinUtils.KillProcess(AExeName: string): Boolean;
var
  Continue: Boolean;
  snapshotHandle: THandle;
  processentry32: TProcessEntry32;
  ProcessID: string;
  Ph: THandle;

begin
  snapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);  //Prozesse auslesen
  processentry32.dwSize := SizeOf(processentry32);
  Continue := Process32First(snapshotHandle, processentry32);         //Startprozess

  try                                                                 //nach "explorer.exe" suchen
    while ((ExtractFileName(processentry32.szExeFile) <> AExeName) and Continue) do
      Continue := Process32Next(snapshotHandle, processentry32);

    ProcessID := IntToHex(processentry32.th32ProcessID, 4);           //wenn gefunden: ProcessID speichern
    Ph := OpenProcess($0001, BOOL(0), StrToInt('$'+ProcessID));       //Prozess mit ProcessID �ffnen
    result := (Integer(TerminateProcess(Ph, 0)) = 1);                 //und terminieren

  finally                                                             //freigeben
    CloseHandle(snapshotHandle);
  end;  //of finally
end;

{ public }
class function TWinUtils.CheckWindows: Boolean;       //Pr�fen, ob Win Vista oder Win 7
begin
  result := GetWinVersion()[1] in ['V','7','8'];
end;


class function TWinUtils.ExitWindows(AAction: Word): Boolean;
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
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
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

  result := ExitWindowsEx(AAction, 0);
end;


class function TWinUtils.ExplorerReboot(): Boolean; //"Explorer"-Task neustarten
begin
  result := KillProcess('explorer.exe');
end;


class function TWinUtils.GetBuildNumber(): Word;
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
              result := dwFileVersionLS and $FFFF
         else
            result := 0;

       finally
         FreeMem(VerInfo, VerInfoSize);
       end;   //of finally
     end  //of begin
  else
     result := 0;
end;


class function TWinUtils.GetWinDir(): string;
begin
  result := SysUtils.GetEnvironmentVariable('windir');
end;


class function TWinUtils.GetWinVersion(AShowServicePack: Boolean = false): string;  //Windows-Version auslesen
begin                                                                               //not by me
  result := 'unbekannt';

  case Win32Platform of                     // 9x-Reihe
    1: if (Win32MajorVersion = 4) then
          case Win32MinorVersion of
            0: result := '95';
           10: result := '98';
           90: result := 'Me';
          end; //of case

    2: case Win32MajorVersion of            // NT-Reihe
         3: if (Win32MinorVersion = 51) then
               result := 'NT 3.51';

         4: if (Win32MinorVersion = 0) then
               result := 'NT 4';

         5: case Win32MinorVersion of       //W2K Reihe
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

  if (AShowServicePack and (Win32CSDVersion <> '')) then
      result := result +' '+ Win32CSDVersion;    //Informationen zu Servicepacks
end;


class function TWinUtils.HKeyToStr(AHKey: HKey): string;
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
		   raise EWinUtilsException.Create('Bad format error! Unknown HKEY!');
end;


class function TWinUtils.LockWindows(): Boolean;
type
  TLockWorkStation = function: Boolean;

var
  hUser32: HMODULE;
  LockWorkStation: TLockWorkStation;

begin
  result := false;
  hUser32 := GetModuleHandle('User32.dll');

  if (hUser32 <> 0) then
     begin
     @LockWorkStation := GetProcAddress(hUser32, 'LockWorkStation');

	   if (@LockWorkStation <> nil) then
        begin
        LockWorkStation;
        result := true;
        end;  //of begin
     end;  //of begin
end;


class function TWinUtils.StrToHKey(AMainKey: string): HKEY;   //wandelt HKEY-Kurzschreibweise
begin                                                         //in Langfassung vom Typ HKEY
  if (AMainKey = 'HKCU') then
     result := HKEY_CURRENT_USER
  else
     if (AMainKey = 'HKLM') then
        result := HKEY_LOCAL_MACHINE
     else
        if (AMainKey = 'HKCR') then
           result := HKEY_CLASSES_ROOT
        else
           raise EWinUtilsException.Create('Bad format error! Unknown rootkey!');
end;
{ of TWinUtils }
 
end.