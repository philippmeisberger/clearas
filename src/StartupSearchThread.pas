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
  Windows, Classes, SysUtils, SyncObjs, ClearasSearchThread, ClearasAPI,
  PMCWOSUtils;

type
  TStartupSearchThread = class(TClearasSearchThread)
  private
    FIncludeRunOnce,
    FWin64: Boolean;
    procedure Load(AStartupLocation: TStartupLocation);
    procedure LoadDisabled(AStartupUser: Boolean); deprecated 'Since Windows 8';
  protected
    procedure Execute; override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TStartupSearchThread</c> instance.
    /// </summary>
    /// <param name="AStartupList">
    ///   A <see cref="TStartupList"/> to be filled.
    /// </param>
    /// <param name="ALock">
    ///   The mutex.
    /// </param>
    constructor Create(AStartupList: TStartupList; ALock: TCriticalSection);

    /// <summary>
    ///   Include RunOnce items or not.
    /// </summary>
    property IncludeRunOnce: Boolean read FIncludeRunOnce write FIncludeRunOnce;

    /// <summary>
    ///   Use the WOW64 technology.
    /// </summary>
    property Win64: Boolean read FWin64 write FWin64;
  end;

implementation

{ TStartupSearchThread }

constructor TStartupSearchThread.Create(AStartupList: TStartupList;
  ALock: TCriticalSection);
begin
  inherited Create(TRootList<TRootItem>(AStartupList), ALock);
end;

procedure TStartupSearchThread.Load(AStartupLocation: TStartupLocation);
begin
  if ((AStartupLocation in [slHkcuRunOnce, slHklmRunOnce, slHklmRunOnce32]) and
    not FIncludeRunOnce) then
    Exit;

  Synchronize(DoNotifyOnSearching);
  TStartupList(FSelectedList).Load(AStartupLocation);
end;

procedure TStartupSearchThread.LoadDisabled(AStartupUser: Boolean);
begin
  Synchronize(DoNotifyOnSearching);
  TStartupList(FSelectedList).LoadDisabled(AStartupUser);
end;

procedure TStartupSearchThread.Execute;
const
  KEYS_COUNT_MAX = 11;

var
  ApprovedLocation: TStartupApprovedLocation;
  Location: TStartupLocation;

begin
  FLock.Acquire();

  try
    try
      // Clear data
      FSelectedList.Clear;

      // Calculate key count for events
      FProgressMax := KEYS_COUNT_MAX;

      if not FWin64 then
        Dec(FProgressMax, 3);

      if not FIncludeRunOnce then
        Dec(FProgressMax, 2);

      if CheckWin32Version(6, 2) then
        Dec(FProgressMax, 2);

      // Notify start of search
      Synchronize(DoNotifyOnStart);

      for Location := Low(TStartupLocation) to High(TStartupLocation) do
        Load(Location);

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
        LoadDisabled(False);
        LoadDisabled(True);
      end;  //of if

    finally
      FLock.Release();

      // Notify end of search
      Synchronize(DoNotifyOnFinish);
    end;  //of try

  except
    on E: Exception do
    begin
      FErrorMessage := Format('%s: %s', [ToString(), E.Message]);
      Synchronize(DoNotifyOnError);
    end;
  end;  //of try
end;

end.
