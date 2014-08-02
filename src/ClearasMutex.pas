unit ClearasMutex;

interface

uses
  Windows, Dialogs, SysUtils;

implementation

var
  mHandle: THandle;    // Mutexhandle

Initialization

  mHandle := CreateMutex(nil, true, PChar('Clearas'));

  if (GetLastError = ERROR_ALREADY_EXISTS) then 
     begin
     MessageDlg('An other instance of Clearas already exists!', mtERROR, [mbOK], 0);
     Halt;
     end;

finalization

  if (mHandle <> 0) then
     CloseHandle(mHandle);

end.

 