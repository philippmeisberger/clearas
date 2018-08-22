#define MyAppName "Clearas"
#define MyAppURL "http://www.pm-codeworks.de"
#define MyAppExeName "Clearas.exe"
#define MyAppExePath32 "..\bin\Win32\Release\"
#define MyAppExePath64 "..\bin\Win64\Release\"
#define FileVersion GetFileVersion(MyAppExePath32 + MyAppExeName)
#define ProductVersion GetFileProductVersion(MyAppExePath32 + MyAppExeName)

[Setup]
AppId={{84F0A7A3-D57D-4A6F-8A0D-2019B11A5091}
AppName={#MyAppName}
AppVersion={#FileVersion}
AppVerName={#MyAppName} {#ProductVersion}
AppCopyright=Philipp Meisberger
AppPublisher=PM Code Works
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}/clearas.html
CreateAppDir=yes
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableDirPage=auto
DisableProgramGroupPage=yes
LicenseFile=..\LICENSE.txt
OutputDir=.
OutputBaseFilename=clearas_setup
Compression=lzma
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64
UninstallDisplayIcon={app}\{#MyAppExeName}
VersionInfoVersion={#FileVersion}
MinVersion=6.0
SignTool=MySignTool sign /v /n "PM Code Works" /tr http://timestamp.globalsign.com/scripts/timstamp.dll /td SHA256 /fd SHA256 $f

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "ContextMenuRecycleBin"; Description: "{cm:ContextMenuRecycleBin}"; GroupDescription: "{cm:ContextMenu}"; Flags: unchecked

[Files]
Source: "{#MyAppExePath32}{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Check: not Is64BitInstallMode
Source: "{#MyAppExePath64}{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Check: Is64BitInstallMode

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#MyAppName}}"; Flags: postinstall shellexec

[Registry]
Root: HKCR; Subkey: "CLSID\{{645FF040-5081-101B-9F08-00AA002F954E}\shell\{#MyAppName}"; Flags: uninsdeletekey; Tasks: "ContextMenuRecycleBin"
Root: HKCR; Subkey: "CLSID\{{645FF040-5081-101B-9F08-00AA002F954E}\shell\{#MyAppName}"; ValueType: string; ValueName: ""; ValueData: "{cm:LaunchProgram,{#MyAppName}}"; Tasks: "ContextMenuRecycleBin"
Root: HKCR; Subkey: "CLSID\{{645FF040-5081-101B-9F08-00AA002F954E}\shell\{#MyAppName}\command"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName}"; Tasks: "ContextMenuRecycleBin"

[Messages]
BeveledLabel=Inno Setup

[CustomMessages]
ContextMenu=Integrate {#MyAppName} into contextmenu
ContextMenuRecycleBin=Recycle Bin

[Code]
procedure UrlLabelClick(Sender: TObject);
var
  ErrorCode : Integer;

begin
  ShellExec('open', ExpandConstant('{#MyAppURL}'), '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;


procedure InitializeWizard;
var
  UrlLabel: TNewStaticText;
  CancelBtn: TButton;

begin
  CancelBtn := WizardForm.CancelButton;
  UrlLabel := TNewStaticText.Create(WizardForm);
  UrlLabel.Top := CancelBtn.Top + (CancelBtn.Height div 2) -(UrlLabel.Height div 2);
  UrlLabel.Left := WizardForm.ClientWidth - CancelBtn.Left - CancelBtn.Width;
  UrlLabel.Caption := 'www.pm-codeworks.de';
  UrlLabel.Font.Style := UrlLabel.Font.Style + [fsUnderline];
  UrlLabel.Cursor := crHand;
  UrlLabel.Font.Color := clHighlight;
  UrlLabel.OnClick := @UrlLabelClick;
  UrlLabel.Parent := WizardForm;
end;
