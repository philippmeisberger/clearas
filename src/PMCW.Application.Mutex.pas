{ *********************************************************************** }
{                                                                         }
{ PM Code Works Application Mutex Unit                                    }
{                                                                         }
{ Copyright (c) 2011-2019 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Application.Mutex;

interface

uses
  Windows, Forms, Dialogs, UITypes;

implementation

var
  Mutex: THandle;

initialization

  Mutex := CreateMutex(nil, True, PChar(Application.Title));

  if (GetLastError() = ERROR_ALREADY_EXISTS) then
  begin
    MessageDlg('Another instance of '+ Application.Title +' already exists! Abort...',
      mtWarning, [mbOK], 0);
    Application.Terminate;
  end;  //of begin

finalization

  if (Mutex <> 0) then
  begin
    ReleaseMutex(Mutex);
    CloseHandle(Mutex);
  end;  //of begin

end.
