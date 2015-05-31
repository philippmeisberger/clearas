{ *********************************************************************** }
{                                                                         }
{ PM Code Works Windows Mutex Unit v1.3                                   }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Mutex;

interface

uses
  Windows, Forms, Dialogs;

implementation

var
  Handle: THandle;

initialization

  Handle := CreateMutex(nil, True, PChar(Application.Title));

  if (GetLastError() = ERROR_ALREADY_EXISTS) then
  begin
    MessageDlg('Another instance of '+ Application.Title +' already exists! Abort...',
      mtWarning, [mbOK], 0);
    Application.Terminate;
  end;  //of begin


finalization

  if (Handle <> 0) then
    CloseHandle(Handle);

end.
