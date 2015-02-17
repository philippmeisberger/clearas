{ *********************************************************************** }
{                                                                         }
{ Clearas context menu search thread                                      }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SearchThread;

interface

uses
  Windows, Classes, Registry, OSUtils, ClearasAPI;

type
  { TContextMenuSearchThread }
  TContextMenuSearchThread = class(TThread)
  private
    FReg: TRegistry;
    FProgress, FProgressMax: Cardinal;
    FContextList: TContextList;
    FOnStart, FOnSearching: TSearchEvent;
    FOnFinish: TNotifyEvent;
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
    procedure DoNotifyOnSearching();
    procedure SearchSubkey(AKeyName, ASearchPattern: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AContextList: TContextList);
    destructor Destroy(); override;
    { external }
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

implementation

uses StrUtils;

{ TContextMenuSearchThread }

{ public TContextMenuSearchThread.Create

  Constructor for creating a TContextMenuSearchThread instance. }

constructor TContextMenuSearchThread.Create(AContextList: TContextList);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FContextList := AContextList;

  // Init Registry access with read-only
  FReg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));
  FReg.RootKey := HKEY_CLASSES_ROOT;
end;

{ public TContextMenuSearchThread.Destroy

  Destructor for destroying a TContextMenuSearchThread instance. }

destructor TContextMenuSearchThread.Destroy();
begin
  FReg.CloseKey;
  FReg.Free;
  inherited Destroy;
end;

{ private TContextMenuSearchThread.DoNotifyOnFinish

  Synchronizable event method that is called when search has finished. }

procedure TContextMenuSearchThread.DoNotifyOnFinish();
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

{ private TContextMenuSearchThread.DoNotifyOnSearching

  Synchronizable event method that is called when search is in progress. }

procedure TContextMenuSearchThread.DoNotifyOnSearching();
begin
  if Assigned(FOnSearching) then
    FOnSearching(Self, FProgress);
end;

{ private TContextMenuSearchThread.DoNotifyOnStart

  Synchronizable event method that is called when search has started. }

procedure TContextMenuSearchThread.DoNotifyOnStart();
begin
  if Assigned(FOnSearching) then
    FOnStart(Self, FProgressMax);
end;

{ private TContextMenuSearchThread.SearchSubkey

  Searches for pattern keys in a Registry subkey. }

procedure TContextMenuSearchThread.SearchSubkey(AKeyName, ASearchPattern: string);
var
  Keys: TStringList;
  i: Integer;

begin
  Keys := TStringList.Create();

  try
    FReg.CloseKey;
    FReg.OpenKey(AKeyName, False);

    if FReg.HasSubKeys then
    begin
      // Load subkeys
      FReg.GetKeyNames(Keys);

      for i := 0 to Keys.Count - 1 do
        // Pattern matches?
        if AnsiContainsText(Keys[i], ASearchPattern) then
        begin
          // Load context menu in current key
          FContextList.LoadContextmenu(AKeyName);
          Break;
        end;  //of for
    end;  //of begin

  finally
    Keys.Free;
  end;  //of try
end;

{ protected TContextMenuSearchThread.Execute

  Searches for context menu items in Registry. }

procedure TContextMenuSearchThread.Execute;
var
  i: Integer;
  Hkcr: TStringList;

begin
  Hkcr := TStringList.Create;

  try
    FReg.OpenKey('', False);
    FReg.GetKeyNames(Hkcr);

    FProgressMax := Hkcr.Count;
    FProgress := 0;

    // Notify start of search
    Synchronize(DoNotifyOnStart);

    for i := 0 to Hkcr.Count -1 do
    begin
      SearchSubkey(Hkcr[i], 'shell');
      Inc(FProgress);

      // Refresh current progress
      Synchronize(DoNotifyOnSearching);
    end;  //of for

    // Notify end of search
    Synchronize(DoNotifyOnFinish);

  finally
    Hkcr.Free;
  end;  //of try
end;

end.
 