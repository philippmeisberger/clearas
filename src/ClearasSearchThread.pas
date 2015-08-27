{ *********************************************************************** }
{                                                                         }
{ Clearas search thread                                                   }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasSearchThread;

interface

uses
  Classes, SyncObjs, ClearasAPI;

type
  { TClearasSearchThread }
  TClearasSearchThread = class(TThread)
  private
    FProgress: Cardinal;
    FOnStart,
    FOnSearching: TSearchEvent;
    FOnFinish: TNotifyEvent;
    FOnError: TSearchErrorEvent;
  protected
    FLock: TCriticalSection;
    FProgressMax: Cardinal;
    FErrorMessage: string;
    procedure DoNotifyOnError();
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
    procedure DoNotifyOnSearching();
  public
    constructor Create(ALock: TCriticalSection);
    { external }
    property OnError: TSearchErrorEvent read FOnError write FOnError;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
  end;

implementation

{ TClearasSearchThread }

{ public TClearasSearchThread.Create

  Constructor for creating a TClearasSearchThread instance. }

constructor TClearasSearchThread.Create(ALock: TCriticalSection);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FLock := ALock;
end;

{ protected TClearasSearchThread.DoNotifyOnError

  Synchronizable event method that is called when an error has occured. }

procedure TClearasSearchThread.DoNotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FErrorMessage);
end;

{ protected TClearasSearchThread.DoNotifyOnFinish

  Synchronizable event method that is called when search has finished. }

procedure TClearasSearchThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

{ protected TContextSearchThread.DoNotifyOnSearching

  Synchronizable event method that is called when search is in progress. }

procedure TClearasSearchThread.DoNotifyOnSearching();
begin
  if Assigned(FOnSearching) then
  begin
    Inc(FProgress);
    FOnSearching(Self, FProgress);
  end;  //of begin
end;

{ protected TContextSearchThread.DoNotifyOnStart

  Synchronizable event method that is called when search has started. }

procedure TClearasSearchThread.DoNotifyOnStart();
begin
  if Assigned(FOnStart) then
    FOnStart(Self, FProgressMax);

  FProgress := 0;
end;

end.
