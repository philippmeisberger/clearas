{ *********************************************************************** }
{                                                                         }
{ PM Code Works Windows Mutex Unit v1.1                                   }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit Mutex;

interface

uses
  Windows, Forms, SysUtils;

implementation

var
  Handle: THandle;
  FileName: string;

initialization

  FileName := Application.Title;
  Handle := CreateMutex(nil, True, PChar(FileName));

  if (GetLastError() = ERROR_ALREADY_EXISTS) then
  begin
    Application.MessageBox(PChar('Another instance of '+ FileName
       +' already exists!'), PChar(FileName), MB_ICONERROR);
    Application.Terminate;
  end;  //of begin


finalization

  if (Handle <> 0) then
    CloseHandle(Handle);

end.
