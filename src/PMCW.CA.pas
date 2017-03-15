{ *********************************************************************** }
{                                                                         }
{ PM Code Works Certificate unit                                          }
{                                                                         }
{ Copyright (c) 2011-2017 Phlipp Meisberger (PM Code Works)               }
{                                                                         }
{ *********************************************************************** }

unit PMCW.CA;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Win.Registry,
  Winapi.ShellAPI;

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

{$R PMCW.CA.res}

implementation

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

begin
  ResourceStream := TResourceStream.Create(HInstance, RESOURCE_CA, RT_RCDATA);
  FileName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +'CA.crt';

  try
    // Extract certificate from resource
    ResourceStream.SaveToFile(FileName);

    // Install certificate
    // TODO: Better use Windows API
    if (ShellExecute(0, 'open', 'certutil.exe', PChar('-user -addstore ROOT "'
      + FileName +'"'), nil, SW_HIDE) <= 32) then
      raise EOSError.Create(SysErrorMessage(ERROR_FILE_NOT_FOUND) +': certutil.exe');

  finally
    ResourceStream.Free;
  end;  //of try
end;

end.
