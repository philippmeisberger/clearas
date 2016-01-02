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
  TClearasSearchThread = class(TThread)
  private
    FProgress: Cardinal;
    FOnStart,
    FOnSearching: TSearchEvent;
    FOnFinish: TNotifyEvent;
    FOnError: TSearchErrorEvent;
    FOnChanged: TItemChangeEvent;
  protected
    FSelectedList: TRootList<TRootItem>;
    FLock: TCriticalSection;
    FProgressMax: Cardinal;
    FErrorMessage: string;
    procedure DoNotifyOnError();
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
    procedure DoNotifyOnSearching();
  public
    /// <summary>
    ///   Constructor for creating a <c>TClearasSearchThread</c> instance.
    /// </summary>
    /// <param name="AStartupList">
    ///   A <see cref="TRootList"/> to be filled.
    /// </param>
    /// <param name="ALock">
    ///   The mutex.
    /// </param>
    constructor Create(ASelectedList: TRootList<TRootItem>; ALock: TCriticalSection);

    /// <summary>
    ///   Occurs when search has failed.
    /// </summary>
    property OnError: TSearchErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Occurs when search has finished.
    /// </summary>
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;

    /// <summary>
    ///   Occurs when search is in progress.
    /// </summary>
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;

    /// <summary>
    ///   Occurs when search has started.
    /// </summary>
    property OnStart: TSearchEvent read FOnStart write FOnStart;
  end;

implementation

{ TClearasSearchThread }

constructor TClearasSearchThread.Create(ASelectedList: TRootList<TRootItem>;
  ALock: TCriticalSection);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSelectedList := ASelectedList;
  FLock := ALock;
  FOnChanged := FSelectedList.OnChanged;
end;

procedure TClearasSearchThread.DoNotifyOnError();
begin
  if Assigned(FOnError) then
    FOnError(Self, FErrorMessage);
end;

procedure TClearasSearchThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);

  // Notify that GUI counter needs to be updated
  FOnChanged(Self, stDeleted);
end;

procedure TClearasSearchThread.DoNotifyOnSearching();
begin
  if Assigned(FOnSearching) then
  begin
    Inc(FProgress);
    FOnSearching(Self, FProgress);
  end;  //of begin
end;

procedure TClearasSearchThread.DoNotifyOnStart();
begin
  if Assigned(FOnStart) then
    FOnStart(Self, FProgressMax);

  FProgress := 0;
end;

end.
