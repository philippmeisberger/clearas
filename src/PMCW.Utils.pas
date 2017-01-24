{ *********************************************************************** }
{                                                                         }
{ PM Code Works Utilities Unit v2.4                                       }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Utils;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, ShellAPI,
{$ELSE}
  Process,
{$ENDIF}
  SysUtils, StrUtils;

{$IFDEF MSWINDOWS}
const
{$IFDEF WIN64}
  PLATFORM_ARCH = ' [64-Bit]';
{$ELSE}
  PLATFORM_ARCH = ' [32-Bit]';
{$ENDIF}
{$ENDIF}

{$IFDEF LINUX}
type
  EArgumentException = class(Exception);
{$ELSE}
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

implementation

{$IFDEF MSWINDOWS}
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
  Result := (ShellExecute(0, nil, PChar(AUrl), nil, nil, SW_SHOWNORMAL) > 32);
{$ENDIF}
end;

end.

