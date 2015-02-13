unit SearchThread;

interface

uses
  Windows, Classes, Registry, OSUtils, ClearasAPI;

type
  { TContextMenuSearchThread }
  TContextMenuSearchThread = class(TThread)
  private
    FProgress, FProgressMax: Cardinal;
    FContextList: TContextList;
    FOnStart, FOnSearching: TSearchEvent;
    FOnFinish: TNotifyEvent;
    procedure DoNotifyOnFinish();
    procedure DoNotifyOnStart();
    procedure DoNotifyOnSearching();
  protected
    procedure Execute; override;
  public
    constructor Create(AContextList: TContextList);
    { external }
    property OnSearching: TSearchEvent read FOnSearching write FOnSearching;
    property OnStart: TSearchEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

implementation

{ TContextMenuSearchThread }

{ public TContextMenuSearchThread.Create

  Constructor for creating a TContextMenuSearchThread instance. }

constructor TContextMenuSearchThread.Create(AContextList: TContextList);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FContextList := AContextList;
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

{ protected TContextMenuSearchThread.Execute

  Searches for context menu items in Registry. }

procedure TContextMenuSearchThread.Execute;
var
  reg: TRegistry;
  i, j, k: integer;
  Hkcr, Temp, Shellex: TStringList;

begin
  reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));
  Hkcr := TStringList.Create;
  Temp := TStringList.Create;
  Shellex := TStringList.Create;

  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.OpenKey('', False);
    reg.GetKeyNames(Hkcr);

    FProgressMax := Hkcr.Count;
    FProgress := 0;

    // Notify start of search
    Synchronize(DoNotifyOnStart);

    for i := 0 to Hkcr.Count -1 do
    begin
      // Refresh current progress
      Synchronize(DoNotifyOnSearching);
      Inc(FProgress);

      reg.CloseKey;
      reg.OpenKey(Hkcr[i], False);

      if reg.HasSubKeys then
      begin
        Temp.Clear;
        reg.GetKeyNames(Temp);

        for j := 0 to Temp.Count -1 do
          if ((Temp[j] = 'shellex') or (Temp[j] = 'ShellEx')) then
          begin
            reg.CloseKey;
            reg.OpenKey(Hkcr[i] +'\'+ Temp[j], False);

            if reg.HasSubKeys then
            begin
              Shellex.Clear;
              reg.GetKeyNames(Shellex);

              for k := 0 to Shellex.Count -1 do
                if (Shellex[k] = 'ContextMenuHandlers') then
                begin
                  reg.CloseKey;
                  reg.OpenKey(Hkcr[i] +'\'+ Temp[j] +'\'+ Shellex[k], False);

                  if reg.HasSubKeys then
                    FContextList.LoadContextmenu(Hkcr[i]);
                end;  //of for
            end;  //of begin
          end;  //of for
      end;  //of begin
    end;  //of for

  finally
    Temp.Free;
    Hkcr.Free;
    Shellex.Free;
    reg.Free;

    // Notify end of search
    Synchronize(DoNotifyOnFinish);
  end;  //of try
end;

end.
 