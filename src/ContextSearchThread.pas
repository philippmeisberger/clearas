{ *********************************************************************** }
{                                                                         }
{ Clearas general context menu search thread                              }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit ContextSearchThread;

interface

uses
  Classes, SyncObjs, ClearasAPI;

type
  { TContextSearchThread }
  TContextSearchThread = class(TThread)
  private
    FLocations: TStringList;
    FProgress: Cardinal;
    FContextList: TContextList;
    FOnStart, FOnSearching: TSearchEvent;
    FOnFinish: TNotifyEvent;
    FLock: TCriticalSection;
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
    procedure DoNotifyOnSearching();
  protected
    procedure Execute; override;
  public
    constructor Create(AContextList: TContextList; ALock: TCriticalSection);
    destructor Destroy(); override;
    { external }
    property Locations: TStringList read FLocations;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

implementation

{ TContextSearchThread }

{ public TContextSearchThread.Create

  Constructor for creating a TContextSearchThread instance. }

constructor TContextSearchThread.Create(AContextList: TContextList;
  ALock: TCriticalSection);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FContextList := AContextList;
  FLock := ALock;
  FLocations := TStringList.Create;
end;

{ public TContextSearchThread.Destroy

  Destructor for destroying a TContextSearchThread instance. }

destructor TContextSearchThread.Destroy();
begin
  FLocations.Free;
  inherited Destroy;
end;

{ private TContextSearchThread.DoNotifyOnFinish

  Synchronizable event method that is called when search has finished. }

procedure TContextSearchThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

{ private TContextSearchThread.DoNotifyOnSearching

  Synchronizable event method that is called when search is in progress. }

procedure TContextSearchThread.DoNotifyOnSearching();
begin
  if Assigned(FOnSearching) then
    FOnSearching(Self, FProgress);
end;

{ private TContextSearchThread.DoNotifyOnStart

  Synchronizable event method that is called when search has started. }

procedure TContextSearchThread.DoNotifyOnStart();
begin
  if Assigned(FOnSearching) then
    FOnStart(Self, FLocations.Count);
end;

{ protected TContextSearchThread.Execute

  Searches for context menu items in Registry. }

procedure TContextSearchThread.Execute;
var
  i: Integer;

begin
  FLock.Acquire;

  // Notify start of search
  Synchronize(DoNotifyOnStart);

  // Clear selected item
  FContextList.Selected := nil;

  // Clear data
  FContextList.Clear;

  // Search ...
  for i := 0 to FLocations.Count - 1 do
  begin
    Synchronize(DoNotifyOnSearching);
    FContextList.LoadContextmenu(FLocations[i]);
    Inc(FProgress);
  end;  //of for

  // Notify end of search
  Synchronize(DoNotifyOnFinish);
  FLock.Release;
end;

end.
 