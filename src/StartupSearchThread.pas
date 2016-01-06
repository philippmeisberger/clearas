{ *********************************************************************** }
{                                                                         }
{ Clearas startup search thread                                           }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit StartupSearchThread;

{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, ClearasSearchThread, ClearasAPI;

type
  TStartupSearchThread = class(TClearasSearchThread)
  protected
    procedure DoExecute; override;
  end;

implementation

{ TStartupSearchThread }

procedure TStartupSearchThread.DoExecute;
var
  ApprovedLocation: TStartupApprovedLocation;
  Location: TStartupLocation;

begin
  for Location := Low(TStartupLocation) to High(TStartupLocation) do
  begin
    // Include RunOnce in expert mode only
    if ((Location in [slHkcuRunOnce, slHklmRunOnce, slHklmRunOnce32]) and
      not FExpertMode) then
      Continue;

    TStartupList(FSelectedList).Load(Location);
  end;  //of for

  // Windows 8?
  if CheckWin32Version(6, 2) then
  begin
    for ApprovedLocation := Low(TStartupApprovedLocation) to High(TStartupApprovedLocation) do
      TStartupList(FSelectedList).LoadStatus(ApprovedLocation);

    TStartupList(FSelectedList).RefreshCounter();
  end  //of begin
  else
  begin
    // Load WOW6432 Registry key only on 64-Bit Windows (deprecated since Windows 8!)
    TStartupList(FSelectedList).LoadDisabled(False);
    TStartupList(FSelectedList).LoadDisabled(True);
  end;  //of if
end;

end.
