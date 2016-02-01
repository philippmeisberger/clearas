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

  // Load disabled items from deprecated Registry keys only prior to Windows 7
  TStartupList(FSelectedList).LoadDisabled(False);
  TStartupList(FSelectedList).LoadDisabled(True);
end;

end.
