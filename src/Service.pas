unit Service;

interface

uses
  Windows, Registry, Classes, SysUtils, StrUtils, ClearasAPI, OSUtils,
  IniFileParser;

const
  KEY_STARTUP_DISABLE = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupreg\';
  KEY_USER_DISABLE = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\startupfolder\';
  KEY_SERVICE_DISABLE = 'SOFTWARE\Microsoft\Shared Tools\MSConfig\services\';
  KEY_SERVICE_ENABLED = 'SYSTEM\CurrentControlSet\services\';

type
  { TServiceStart enum }
  TServiceStart = (ssBoot, ssSystem, ssAutomatic, ssManual, ssDisabled);

  { TServiceListItem }
  TServiceListItem = class(TRootRegItem)
  private
    FCaption, FTime: string;
    FServiceStart: TServiceStart;
  protected
    function GetFullLocation(): string; override;
  public
    function ChangeFilePath(const ANewFilePath: string): Boolean; override;
    function Delete(): Boolean; override;
    function Disable(): Boolean; override;
    function Enable(): Boolean; override;
    procedure ExportItem(const AFileName: string); override;
    { external }
    property Caption: string read FCaption write FCaption;
    property Start: TServiceStart read FServiceStart write FServiceStart;
    property Time: string read FTime write FTime;
  end;

  { TServiceList }
  TServiceList = class(TRootRegList)
  private
    function ItemAt(AIndex: Word): TServiceListItem;
    function GetSelectedItem(): TServiceListItem;
  protected
    function AddService(AName, ACaption: string; AStart: TServiceStart;
      AReg: TRegistry; AWow64: Boolean = False): Integer;
  public
    procedure ExportList(const AFileName: string); override;
    function IndexOf(const ACaption: string): Integer;
    procedure LoadService(AName: string; AReg: TRegistry);
    procedure LoadServices();
    { external }
    property Item: TServiceListItem read GetSelectedItem;
    property Items[AIndex: Word]: TServiceListItem read ItemAt; default;
  end;


implementation

uses ServiceSearchThread;

{ TServiceListItem }

{ protected TServiceListItem.GetFullLocation

  Returns the full Registry path to a TServiceListItem. }

function TServiceListItem.GetFullLocation(): string;
begin
  Result := TOSUtils.HKeyToStr(HKEY_LOCAL_MACHINE) +'\'+ FLocation;
end;

{ public TServiceListItem.ChangeFilePath

  Changes the file path of an TServiceListItem item. }

function TServiceListItem.ChangeFilePath(const ANewFilePath: string): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey(FLocation, False) then
      raise Exception.Create('Key does not exist!');

    Reg.WriteExpandString('ImagePath', ANewFilePath);
    FilePath := ANewFilePath;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TServiceListItem.Delete

  Deletes a TServiceListItem object and returns True if successful. }

function TServiceListItem.Delete(): Boolean;
begin
  Result := DeleteKey('HKLM', ExtractFileDir(FLocation), Name);

  // Delete key of deactivated item
  if not FEnabled then
    DeleteKey('HKLM', KEY_SERVICE_DISABLE, Name, False);
end;

{ public TServiceListItem.Disable

  Disables an TServiceListItem object and returns True if successful. }

function TServiceListItem.Disable(): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey(FLocation, False) then
      raise Exception.Create('Key does not exist!');

    // Disable service
    Reg.WriteInteger('Start', Ord(ssDisabled));
    Reg.CloseKey();

    // Write disable key
    if not Reg.OpenKey(KEY_SERVICE_DISABLE + Name, True) then
      raise Exception.Create('Could not create disable key!');

    // Save service start
    Reg.WriteInteger(Name, Ord(Start));

    // Update information
    FTime := WriteTimestamp(Reg);
    FEnabled := False;
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TServiceListItem.Enable

  Enables an TServiceListItem object and returns True if successful. }

function TServiceListItem.Enable(): Boolean;
var
  Reg: TRegistry;

begin
  Result := False;
  Reg := TRegistry.Create(TOSUtils.DenyWOW64Redirection(KEY_READ));

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey(FLocation, False) then
      raise Exception.Create('Key does not exist!');

    // Enable service
    Reg.WriteInteger('Start', Ord(Start));
    Reg.CloseKey();

    // Open disable key
    if not Reg.OpenKey(KEY_SERVICE_DISABLE, False) then
      raise Exception.Create('Key does not exist!');

    // Delete disable key
    if (Reg.KeyExists(Name) and not Reg.DeleteKey(Name)) then
      raise Exception.Create('Could not delete disabled key!');

    // Update information
    FEnabled := True;
    FTime := '';
    Result := True;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TServiceListItem.ExportItem

  Exports a list item as .reg file. }

procedure TServiceListItem.ExportItem(const AFileName: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    if not FEnabled then
    begin
      RegFile.ExportKey(HKEY_LOCAL_MACHINE, KEY_SERVICE_DISABLE + Name, False);
      RegFile.ExportKey(HKEY_LOCAL_MACHINE, FLocation, False);
      RegFile.Save();
    end  //of begin
    else
      RegFile.ExportReg(HKEY_LOCAL_MACHINE, FLocation, False);

  finally
    RegFile.Free;
  end;  //of try
end;


{ TServiceList }

{ private TServiceList.ItemAt

  Returns a TServiceListItem object at index. }

function TServiceList.ItemAt(AIndex: Word): TServiceListItem;
begin
  Result := TServiceListItem(RootItemAt(AIndex));
end;

{ private TServiceList.GetSelectedItem

  Returns the current selected item as TServiceListItem. }

function TServiceList.GetSelectedItem(): TServiceListItem;
begin
  Result := TServiceListItem(Selected);
end;

{ protected TServiceList.AddService

  Adds a service item to the list. }

function TServiceList.AddService(AName, ACaption: string; AStart: TServiceStart;
  AReg: TRegistry; AWow64: Boolean = False): Integer;
var
  Item: TServiceListItem;

begin
  Item := TServiceListItem.Create(Count, (AStart <> ssDisabled), AWow64);

  try
    with Item do
    begin
      Name := AName;
      Caption := ACaption;
      FLocation := AReg.CurrentPath;
      FilePath := AReg.ReadString('ImagePath');
      TypeOf := 'Service';
    end;  //of with

    if not Item.Enabled then
    begin
      AReg.CloseKey();

      if not AReg.OpenKey(KEY_SERVICE_DISABLE + AName, False) then
        raise ERegistryException.Create('Key does not exist!');

      // Try to read deactivation timestamp
      Item.Time := Item.GetTimestamp(AReg);

      // Read status
      Item.Start := TServiceStart(AReg.ReadInteger(AName));
    end  //of begin
    else
    begin
      Item.Start := AStart;
      Inc(FActCount);
    end;  //of if

    Result := Add(Item);

  except
    Item.Free;
    Result := -1;
  end;  //of try
end;

{ public TServiceList.ExportList

  Exports the complete list as .reg file. }

procedure TServiceList.ExportList(const AFileName: string);
var
  RegFile: TRegistryFile;
  i: Integer;
  Item: TServiceListItem;

begin
  RegFile := TRegistryFile.Create(AFileName, True);

  try
    for i := 0 to Count - 1 do
    begin
      Item := ItemAt(i);
      RegFile.ExportKey(HKEY_LOCAL_MACHINE, Item.Location, False);

      if not Item.Enabled then
        RegFile.ExportKey(HKEY_LOCAL_MACHINE, KEY_SERVICE_DISABLE + Item.Name, False);
    end;  //of for

    // Save .reg file
    RegFile.Save();

  finally
    RegFile.Free;
  end;  //of try
end;

{ public TServiceList.IndexOf

  Returns the index of an item checking caption only. }

function TServiceList.IndexOf(const ACaption: string): Integer;
var
  i: Integer;

begin
  Result := -1;

  for i := 0 to Count - 1 do
    if (ItemAt(i).Caption = ACaption) then
    begin
      Result := i;
      Break;
    end;  //of begin
end;

{ public TServiceList.LoadService

  Searches for service items. }

procedure TServiceList.LoadService(AName: string; AReg: TRegistry);
var
  Caption: string;
  Start: TServiceStart;
  Wow64: Boolean;

begin
  try
    if not AReg.OpenKey(KEY_SERVICE_ENABLED + AName, False) then
      Exit;

    // Skip items without file path
    if (not AReg.ValueExists('ImagePath') or not AReg.ValueExists('Type')) then
      Exit;

    // Read status
    Start := TServiceStart(AReg.ReadInteger('Start'));

    // Skip driver services
    if ((AReg.ReadInteger('Type') < 16) or (Start <= ssSystem)) then
      Exit;

    // Display name must be string
    if (AReg.GetDataType('DisplayName') <> rdString) then
      Exit;

    // Read caption
    Caption := AReg.ReadString('DisplayName');

    // Filter only non-Windows services
    if (Caption <> '') and (Caption[1] = '@') then
      Exit;

    Wow64 := (AReg.ValueExists('WOW64') and AReg.ReadBool('WOW64'));

    // Add service to list
    AddService(AName, Caption, Start, AReg, Wow64);

  finally
    AReg.CloseKey();
  end;  //of try
end;

{ public TServiceList.LoadServices

  Searches for service items. }

procedure TServiceList.LoadServices();
var
  SearchThread: TServiceSearchThread;

begin
  SearchThread := TServiceSearchThread.Create(Self, FLock);

  with SearchThread do
  begin
    OnStart := OnSearchStart;
    OnSearching := OnSearching;
    OnFinish := OnSearchFinish;
    Resume;
  end;  //of with
end;

end.
