{ *********************************************************************** }
{                                                                         }
{ PM Code Works Certificate unit                                          }
{                                                                         }
{ Copyright (c) 2011-2019 Phlipp Meisberger (PM Code Works)               }
{                                                                         }
{ *********************************************************************** }

unit PMCW.CA;

interface

uses
  Windows, Classes, SysUtils, Registry, ShellAPI;

const
  /// <summary>
  ///   Registry key where certificates are stored.
  /// </summary>
  KEY_CERTIFICATE_STORE        = 'Software\Microsoft\SystemCertificates\ROOT\Certificates';

  /// <summary>
  ///   SHA-160 fingerprint of the PM Code Works CA certificate.
  /// </summary>
  CERTIFICATE_FINGERPRINT_SHA1 = '9e358f6fdd418335baf6217d432e45ec36a4ffe1';

  /// <summary>
  ///   Resource name of the PM Code Works CA certificate.
  /// </summary>
  RESOURCE_CA                  = 'CA';

/// <summary>
///   Checks if the certificate exists in Windows certificate store.
/// </summary>
/// <returns>
///   <c>True</c> if certificate exists or <c>False</c> otherwise.
/// </returns>
function CertificateExists(): Boolean;

/// <summary>
///   Installs the PM Code Works CA certificate for SSL updates and code signing
///   verification.
/// </summary>
/// <exception>
///   <c>EOSError</c> if installing failed.
/// </exception>
procedure InstallCertificate();

implementation

{$R *.res}

function CertificateExists(): Boolean;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    // Check user root certificate store
    Reg.RootKey := HKEY_CURRENT_USER;
    Result := (Reg.OpenKeyReadOnly(KEY_CERTIFICATE_STORE) and Reg.KeyExists(
      CERTIFICATE_FINGERPRINT_SHA1));

  finally
    Reg.CloseKey;
    Reg.Free;
  end;  //of try
end;

procedure InstallCertificate();
var
  ResourceStream: TResourceStream;
  FileName: string;
  ShellExecuteInfo: TShellExecuteInfo;

begin
  ResourceStream := TResourceStream.Create(HInstance, RESOURCE_CA, RT_RCDATA);
  FileName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +'CA.crt';

  try
    // Extract certificate from resource
    ResourceStream.SaveToFile(FileName);

    // Install certificate
    ZeroMemory(@ShellExecuteInfo, SizeOf(TShellExecuteInfo));

    with ShellExecuteInfo do
    begin
      cbSize := SizeOf(TShellExecuteInfo);
      Wnd := 0;
      lpVerb := 'open';
      lpFile := 'certutil.exe';
      lpParameters := PChar(Format('-user -addstore ROOT "%s"', [FileName]));
      lpDirectory := nil;
      nShow := SW_HIDE;
    end;  //of with

    if not {$IFDEF UNICODE}ShellExecuteExW{$ELSE}ShellExecuteExA{$ENDIF}(@ShellExecuteInfo) then
      raise EOSError.Create(SysErrorMessage(GetLastError()));

  finally
    ResourceStream.Free;
  end;  //of try
end;

end.
