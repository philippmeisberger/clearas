{ *********************************************************************** }
{                                                                         }
{ Clearas search thread                                                   }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasSearchThread;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils, ClearasAPI;

type
  TClearasSearchThread = class(TThread)
  private
    FOnStart,
    FOnFinish: TNotifyEvent;
    FOnSearching: TSearchEvent;
    FOnError: TSearchErrorEvent;
    FOnChanged: TItemChangeEvent;
    FErrorMessage: string;
    FLock: TCriticalSection;
    procedure DoNotifyOnError();
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
  protected
    FProgress,
    FProgressMax: Cardinal;
    FExpertMode,
    FWin64: Boolean;
    FSelectedList: TRootList<TRootItem>;
    procedure DoNotifyOnSearching();
    procedure Execute; override; final;
    procedure DoExecute; virtual; abstract;
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
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </param>
    constructor Create(ASelectedList: TRootList<TRootItem>; ALock: TCriticalSection;
      AExpertMode: Boolean = False);

    /// <summary>
    ///   Occurs when search has failed.
    /// </summary>
    property OnError: TSearchErrorEvent read FOnError write FOnError;

    /// <summary>
    ///   Occurs when search has finished.
    /// </summary>
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;

    /// <summary>
    ///   Occurs when search has started.
    /// </summary>
    property OnStart: TNotifyEvent read FOnStart write FOnStart;

    /// <summary>
    ///   Use the WOW64 technology.
    /// </summary>
    property Win64: Boolean read FWin64 write FWin64;
  end;

implementation

{ TClearasSearchThread }

constructor TClearasSearchThread.Create(ASelectedList: TRootList<TRootItem>;
  ALock: TCriticalSection; AExpertMode: Boolean = False);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSelectedList := ASelectedList;
  FLock := ALock;
  FExpertMode := AExpertMode;
  FOnChanged := FSelectedList.OnChanged;
  FProgress := 0;
  FProgressMax := 0;
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
  if Assigned(FOnChanged) then
    FOnChanged(Self, stNone);
end;

procedure TClearasSearchThread.DoNotifyOnSearching();
begin
  if Assigned(FOnSearching) then
  begin
    Inc(FProgress);
    FOnSearching(Self, FProgress, FProgressMax);
  end;  //of begin
end;

procedure TClearasSearchThread.DoNotifyOnStart();
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TClearasSearchThread.Execute;
begin
  FLock.Acquire();
  Synchronize(DoNotifyOnStart);
  FSelectedList.Clear();

  try
    try
      DoExecute;

    finally
      FLock.Release();
      Synchronize(DoNotifyOnFinish);
    end;  //of try

  except
    on E: Exception do
    begin
      FErrorMessage := Format('%s: %s', [ClassName, E.Message]);
      Synchronize(DoNotifyOnError);
    end;
  end;  //of try
end;

end.
