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
  Windows, Classes, SysUtils, SyncObjs, ClearasSearchThread, ClearasAPI;

type
  { TStartupSearchThread }
  TStartupSearchThread = class(TClearasSearchThread)
  private
    FIncludeRunOnce,
    FWin64: Boolean;
    procedure LoadEnabled(AAllUsers: Boolean); overload;
    procedure LoadEnabled(AHKey: HKEY; ARunOnce: Boolean = False;
      AWow64: Boolean = False); overload;
    procedure LoadDisabled(AStartupUser: Boolean; AIncludeWow64: Boolean = False);
  protected
    procedure Execute; override;
  public
    constructor Create(AStartupList: TStartupList; ALock: TCriticalSection);
    { external }
    property IncludeRunOnce: Boolean read FIncludeRunOnce write FIncludeRunOnce;
    property Win64: Boolean read FWin64 write FWin64;
  end;

implementation

{ TStartupSearchThread }

{ public TStartupSearchThread.Create

  Constructor for creating a TStartupSearchThread instance. }

constructor TStartupSearchThread.Create(AStartupList: TStartupList;
  ALock: TCriticalSection);
begin
  inherited Create(TRootList<TRootItem>(AStartupList), ALock);
end;

{ public TStartupList.LoadEnabled

  Searches for enabled startup user items and adds them to the list. }

procedure TStartupSearchThread.LoadEnabled(AAllUsers: Boolean);
begin
  Synchronize(DoNotifyOnSearching);
  TStartupList(FSelectedList).LoadStartup(AAllUsers);
end;

{ private TStartupSearchThread.LoadEnabled

  Searches for enabled startup items and adds them to the list. }

procedure TStartupSearchThread.LoadEnabled(AHKey: HKEY;
  ARunOnce: Boolean = False; AWow64: Boolean = False);
begin
  Synchronize(DoNotifyOnSearching);
  TStartupList(FSelectedList).LoadStartup(AHKey, ARunOnce, AWow64);
end;

{ private TStartupSearchThread.LoadDisabled

  Searches for disabled startup user items and adds them to the list. }

procedure TStartupSearchThread.LoadDisabled(AStartupUser: Boolean;
  AIncludeWow64: Boolean = False);
begin
  Synchronize(DoNotifyOnSearching);
  TStartupList(FSelectedList).LoadDisabled(AStartupUser, AIncludeWow64);
end;

{ protected TStartupSearchThread.Execute

  Searches for startup items in Registry. }

procedure TStartupSearchThread.Execute;
const
  KEYS_COUNT_MAX = 11;

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

      // Start loading...
      LoadEnabled(HKEY_LOCAL_MACHINE);

      // Load WOW6432 Registry key only on 64bit Windows
      if FWin64 then
        LoadEnabled(HKEY_LOCAL_MACHINE, False, True);

      LoadEnabled(HKEY_CURRENT_USER);

      // Read RunOnce entries?
      if FIncludeRunOnce then
      begin
        LoadEnabled(HKEY_LOCAL_MACHINE, True);
        LoadEnabled(HKEY_CURRENT_USER, True);

        // Load WOW6432 Registry keys only on 64bit Windows
        if FWin64 then
          LoadEnabled(HKEY_LOCAL_MACHINE, True, True);
      end;  //of begin

      // Load WOW6432 Registry key only on 64-Bit Windows (deprecated since Windows 8!)
      LoadDisabled(False, FWin64);
      LoadDisabled(True, FWin64);

      // Load startup user items
      LoadEnabled(True);
      LoadEnabled(False);

      // Windows 8?
      if CheckWin32Version(6, 2) then
        with TStartupList(FSelectedList) do
        begin
          if FWin64 then
          begin
            LoadStatus(HKEY_CURRENT_USER, KEY_STARTUP_RUN32_APPROVED);
            LoadStatus(HKEY_LOCAL_MACHINE, KEY_STARTUP_RUN32_APPROVED);
          end;  //of begin

          LoadStatus(HKEY_LOCAL_MACHINE, KEY_STARTUP_USER_APPROVED);
          LoadStatus(HKEY_CURRENT_USER, KEY_STARTUP_USER_APPROVED);
          LoadStatus(HKEY_CURRENT_USER, KEY_STARTUP_RUN_APPROVED);
          LoadStatus(HKEY_LOCAL_MACHINE, KEY_STARTUP_RUN_APPROVED);
          RefreshCounter();
        end;  //of begin

    finally
      // Notify end of search
      Synchronize(DoNotifyOnFinish);
      FLock.Release();
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
