{ *********************************************************************** }
{                                                                         }
{ Clearas service search thread                                           }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ServiceSearchThread;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, WinSvc, ClearasSearchThread, ClearasAPI;

type
  TServiceSearchThread = class(TClearasSearchThread)
  private
    FManager: SC_HANDLE;
  protected
    procedure DoExecute; override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TServiceSearchThread</c> instance.
    /// </summary>
    /// <param name="AServiceList">
    ///   A <see cref="TServiceList"/> to be filled.
    /// </param>
    /// <param name="ALock">
    ///   The mutex.
    /// </param>
    /// <param name="AManager">
    ///   The service manager.
    /// </param>
    /// <param name="AExpertMode">
    ///   If set to <c>True</c> use the expert search mode. Otherwise use the
    ///   default search mode.
    /// </param>
    constructor Create(AServiceList: TServiceList; ALock: TCriticalSection;
      AManager: SC_HANDLE; AExpertMode: Boolean = False);
  end;

implementation

{ TServiceSearchThread }

constructor TServiceSearchThread.Create(AServiceList: TServiceList;
  ALock: TCriticalSection; AManager: SC_HANDLE; AExpertMode: Boolean = False);
begin
  inherited Create(TRootList<TRootItem>(AServiceList), ALock, AExpertMode);
  FManager := AManager;
end;

procedure TServiceSearchThread.DoExecute;
var
  Service: SC_HANDLE;
  Services, ServicesCopy: PEnumServiceStatus;
  BytesNeeded, ServicesReturned, ResumeHandle, LastError, ServiceType: DWORD;
  i: Integer;

begin
  ServicesReturned := 0;
  ResumeHandle := 0;
  Services := nil;

  // Include services that are shared with other processes?
  if FExpertMode then
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

    // Add services to list
    for i := 0 to ServicesReturned - 1 do
    begin
      Synchronize(DoNotifyOnSearching);
      Service := OpenService(FManager, ServicesCopy^.lpServiceName, SERVICE_QUERY_CONFIG);

      // Skip corrupted service
      if (Service <> 0) then
        TServiceList(FSelectedList).LoadService(ServicesCopy^.lpServiceName,
          Service, FExpertMode);

      Inc(ServicesCopy);
    end;  //of for

  finally
    FreeMem(Services, BytesNeeded);
  end;  //of try
end;

end.
