{ *********************************************************************** }
{                                                                         }
{ Clearas service search thread                                           }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ServiceSearchThread;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, WinSvc, ClearasSearchThread, ClearasAPI;

type
  { TServiceSearchThread }
  TServiceSearchThread = class(TClearasSearchThread)
  private
    FServiceList: TServiceList;
    FManager: SC_HANDLE;
    FOnFinish: TNotifyEvent;
    FIncludeShared: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AServiceList: TServiceList; AManager: SC_HANDLE;
      ALock: TCriticalSection);
    { external }
    property IncludeShared: Boolean read FIncludeShared write FIncludeShared;
  end;

implementation

{ TServiceSearchThread }

{ public TServiceSearchThread.Create

  Constructor for creating a TServiceSearchThread instance. }

constructor TServiceSearchThread.Create(AServiceList: TServiceList;
  AManager: SC_HANDLE; ALock: TCriticalSection);
begin
  inherited Create(ALock);
  FServiceList := AServiceList;
  FManager := AManager;
end;

{ protected TServiceSearchThread.Execute

  Searches for service items. }

procedure TServiceSearchThread.Execute;
var
  Service: SC_HANDLE;
  Services, ServicesCopy: PEnumServiceStatus;
  BytesNeeded, ServicesReturned, ResumeHandle, LastError, ServiceType: Cardinal;
  i: Integer;

begin
  FLock.Acquire;

  try
    // Clear data
    FServiceList.Clear;
    ServicesReturned := 0;
    ResumeHandle := 0;
    Services := nil;

    // Include services that are shared with other processes?
    if FIncludeShared then
      ServiceType := SERVICE_WIN32
    else
      ServiceType := SERVICE_WIN32_OWN_PROCESS;

    // Determine the required size for buffer
    EnumServicesStatus(FManager, ServiceType, SERVICE_STATE_ALL, Services^, 0,
      BytesNeeded, ServicesReturned, ResumeHandle);

    LastError := GetLastError();

    // ERROR_MORE_DATA will be fired normally
    if (LastError <> ERROR_MORE_DATA) then
      raise EServiceException.Create(SysErrorMessage(LastError));

    GetMem(Services, BytesNeeded);

    try
      ServicesReturned := 0;
      ResumeHandle := 0;
      ServicesCopy := Services;

      // Read all services matching
      if not EnumServicesStatus(FManager, ServiceType, SERVICE_STATE_ALL,
        Services^, BytesNeeded, BytesNeeded, ServicesReturned, ResumeHandle) then
        raise EServiceException.Create(SysErrorMessage(GetLastError()));

      // Notify start of search
      FProgressMax := ServicesReturned;
      Synchronize(DoNotifyOnStart);

      // Add services to list
      for i := 0 to ServicesReturned - 1 do
      begin
        Synchronize(DoNotifyOnSearching);
        Service := OpenService(FManager, ServicesCopy^.lpServiceName, SERVICE_QUERY_CONFIG);

        // Skip corrupted service
        if (Service <> 0) then
          FServiceList.LoadService(ServicesCopy^.lpServiceName, Service, FIncludeShared);

        Inc(ServicesCopy);
      end;  //of for

    finally
      FreeMem(Services);

      // Notify end of search
      Synchronize(DoNotifyOnFinish);
      FLock.Release;
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
