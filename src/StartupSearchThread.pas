{ *********************************************************************** }
{                                                                         }
{ Clearas startup search thread                                           }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit StartupSearchThread;

interface

uses
  Windows, Classes, SyncObjs, ClearasAPI;

type
  { TStartupSearchThread }
  TStartupSearchThread = class(TThread)
  private
    FStartupList: TStartupList;
    FIncludeRunOnce, FWin64: Boolean;
    FProgress, FProgressMax: Byte;
    FOnSearching, FOnStart: TSearchEvent;
    FOnFinish: TNotifyEvent;
    FLock: TCriticalSection;
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnSearching();
    procedure DoNotifyOnStart();
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
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
    property Win64: Boolean read FWin64 write FWin64;
  end;

implementation

{ TStartupSearchThread }

{ public TStartupSearchThread.Create

  Constructor for creating a TStartupSearchThread instance. }

constructor TStartupSearchThread.Create(AStartupList: TStartupList;
  ALock: TCriticalSection);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FStartupList := AStartupList;
  FLock := ALock;
end;

{ private TStartupSearchThread.DoNotifyOnFinish

  Synchronizable event method that is called when search has finished. }

procedure TStartupSearchThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

{ private TStartupSearchThread.DoNotifyOnSearching

  Synchronizable event method that is called when search is in progress. }

procedure TStartupSearchThread.DoNotifyOnSearching();
begin
  if Assigned(FOnSearching) then
  begin
    Inc(FProgress);
    FOnSearching(Self, FProgress);
  end;  //of begin
end;

{ private TStartupSearchThread.DoNotifyOnStart

  Synchronizable event method that is called when search has started. }

procedure TStartupSearchThread.DoNotifyOnStart();
begin
  if Assigned(FOnStart) then
    FOnStart(Self, FProgressMax);
end;

{ public TStartupList.LoadEnabled

  Searches for enabled startup user items and adds them to the list. }

procedure TStartupSearchThread.LoadEnabled(AAllUsers: Boolean);
begin
  Synchronize(DoNotifyOnSearching);
  FStartupList.LoadEnabled(AAllUsers);
end;

{ private TStartupSearchThread.LoadEnabled

  Searches for enabled startup items and adds them to the list. }

procedure TStartupSearchThread.LoadEnabled(AHKey: HKEY;
  ARunOnce: Boolean = False; AWow64: Boolean = False);
begin
  Synchronize(DoNotifyOnSearching);
  FStartupList.LoadEnabled(AHKey, ARunOnce, AWow64);
end;

{ private TStartupSearchThread.LoadEnabled

  Searches for disabled startup user items and adds them to the list. }

procedure TStartupSearchThread.LoadDisabled(AStartupUser: Boolean;
  AIncludeWow64: Boolean = False);
begin
  Synchronize(DoNotifyOnSearching);
  FStartupList.LoadDisabled(AStartupUser, AIncludeWow64);
end;

{ protected TContextMenuSearchThread.Execute

  Searches for startup items in Registry. }

procedure TStartupSearchThread.Execute;
const
  KEYS_COUNT_MAX = 11;

begin
  FLock.Acquire;
  
  // Clear selected item
  FStartupList.Selected := nil;

  // Clear data
  FStartupList.Clear;

  // Calculate key count for events
  if FWin64 then
    FProgressMax := KEYS_COUNT_MAX
  else
    FProgressMax := KEYS_COUNT_MAX - 3;

  if not FIncludeRunOnce then
    FProgressMax := FProgressMax - 2;

  // Notify start of search
  Synchronize(DoNotifyOnStart);
  FProgress := 0;

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
    begin
      LoadEnabled(HKEY_LOCAL_MACHINE, True, True);
      LoadEnabled(HKEY_CURRENT_USER, True, True);
    end;  //of begin
  end;  //of begin

  // Load WOW6432 Registry key only on 64bit Windows
  LoadDisabled(False, FWin64);
  LoadDisabled(True, FWin64);

  // Load startup user items
  LoadEnabled(True);
  LoadEnabled(False);

  // Notify end of search
  Synchronize(DoNotifyOnFinish);
  FLock.Release;
end;

end.
