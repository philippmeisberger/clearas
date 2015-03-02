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
  Classes, SyncObjs, ClearasAPI;

type
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
    procedure LoadEnabled(const AAllUsers: Boolean); overload;
    procedure LoadEnabled(const AHKey, AKeyName: string); overload;
    procedure LoadDisabled(const AKeyName: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AStartupList: TStartupList; AWin64, AIncludeRunOnce: Boolean;
      ALock: TCriticalSection);
    { external }
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

implementation

{ TStartupSearchThread }

{ public TStartupSearchThread.Create

  Constructor for creating a TStartupSearchThread instance. }

constructor TStartupSearchThread.Create(AStartupList: TStartupList;
  AWin64, AIncludeRunOnce: Boolean; ALock: TCriticalSection);
const
  KEYS_COUNT_MAX = 11;

begin
  inherited Create(True);
  FreeOnTerminate := True;
  FStartupList := AStartupList;
  FWin64 := AWin64;
  FIncludeRunOnce := AIncludeRunOnce;
  FLock := ALock;

  // Calculate key count for events
  if FWin64 then
    FProgressMax := KEYS_COUNT_MAX
  else
    FProgressMax := KEYS_COUNT_MAX - 3;

  if not AIncludeRunOnce then
    FProgressMax := FProgressMax - 2;
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
  // Notify searching
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
  if Assigned(FOnSearching) then
    FOnStart(Self, FProgressMax);
end;

{ public TStartupList.LoadEnabled

  Searches for enabled startup user items and adds them to the list. }

procedure TStartupSearchThread.LoadEnabled(const AAllUsers: Boolean);
begin
  Synchronize(DoNotifyOnSearching);
  FStartupList.LoadEnabled(AAllUsers);
end;

{ private TStartupSearchThread.LoadEnabled

  Searches for enabled startup items and adds them to the list. }

procedure TStartupSearchThread.LoadEnabled(const AHKey, AKeyName: string);
begin
  Synchronize(DoNotifyOnSearching);
  FStartupList.LoadEnabled(AHKey, AKeyName);
end;

{ private TStartupSearchThread.LoadEnabled

  Searches for disabled startup user items and adds them to the list. }

procedure TStartupSearchThread.LoadDisabled(const AKeyName: string);
begin
  Synchronize(DoNotifyOnSearching);
  FStartupList.LoadDisabled(AKeyName);
end;

{ protected TContextMenuSearchThread.Execute

  Searches for startup items in Registry. }

procedure TStartupSearchThread.Execute;
begin
  FLock.Acquire;

  // Notify start of search
  Synchronize(DoNotifyOnStart);
  FProgress := 0;
  LoadEnabled('HKLM', KEY_STARTUP);

  // Load WOW6432 Registry key only on 64bit Windows
  if FWin64 then
    LoadEnabled('HKLM', KEY_STARTUP32);

  // Read RunOnce entries?
  if FIncludeRunOnce then
  begin
    LoadEnabled('HKLM', KEY_RUNONCE);
    LoadEnabled('HKCU', KEY_RUNONCE);

    // Load WOW6432 Registry keys only on 64bit Windows
    if FWin64 then
    begin
      LoadEnabled('HKLM', KEY_RUNONCE32);
      LoadEnabled('HKCU', KEY_RUNONCE32);
    end;  //of begin
  end;  //of begin

  LoadEnabled('HKCU', KEY_STARTUP);
  LoadEnabled(True);
  LoadEnabled(False);
  LoadDisabled(KEY_DEACT);
  LoadDisabled(KEY_DEACT_FOLDER);

  // Notify end of search
  Synchronize(DoNotifyOnFinish);

  FLock.Release;
end;

end.
