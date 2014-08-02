{ *********************************************************************** }
{                                                                         }
{ Clearas Update Check Thread v1.3                                        }
{                                                                         }
{ Copyright (c) 2011-2013 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit UpdateCheckThread;

interface

uses
  Classes, SysUtils, IdHTTP;

type
  TOnUpdateAvailableEvent = procedure(Sender: TThread; const ANewBuild: integer) of object;
  TOnNoUpdateAvailableEvent = procedure(Sender: TThread) of object;
  TOnErrorEvent = procedure(Sender: TThread) of object;

  TUpdateCheckThread = class(TThread)
  private
    HTTP: TIdHTTP;
    FOnUpdate: TOnUpdateAvailableEvent;
    FOnNoUpdate: TOnNoUpdateAvailableEvent;
    FOnError: TOnErrorEvent;
    FCurBuild, FNewBuild: integer;
    procedure DoNotifyOnError;
    procedure DoNotifyOnNoUpdate;
    procedure DoNotifyOnUpdate;
  protected
    procedure Execute; override;
  public
    constructor Create(ACurrentBuild: integer);
    destructor Destroy; override;
    property OnError: TOnErrorEvent read FOnError write FOnError;
    property OnNoUpdate: TOnNoUpdateAvailableEvent read FOnNoUpdate write FOnNoUpdate;
    property OnUpdate: TOnUpdateAvailableEvent read FOnUpdate write FOnUpdate;
  end;

implementation

uses ClearasMain, ClearasAPI;


constructor TUpdateCheckThread.Create(ACurrentBuild: integer);     //Konstruktor
begin
  inherited Create(true);                      //Super Konstruktor Aufruf
  FreeOnTerminate := true;                     //Thread beendet sich selbst
  HTTP := TIdHTTP.Create(nil);                 //HTTP-Komponente erstellen
  //IOHandler := TIdSSLIOHandlerSocket.Create(nil);  //SSL-Handler erstellen
  FCurBuild := ACurrentBuild;
end;


destructor TUpdateCheckThread.Destroy;                               //freigeben
begin
  HTTP.Free;
  inherited Destroy;
end;


procedure TUpdateCheckThread.Execute;                  //auf Server nach neuer Version suchen
begin
  try
    FNewBuild := StrToInt(HTTP.Get(URL_DIR +'Clearas/version.txt'));  //Build ermitteln

    if (FNewBuild > FCurBuild) then                    //falls neuere Build verf�gbar
       Synchronize(DoNotifyOnUpdate)                   //Update Melden
    else
       Synchronize(DoNotifyOnNoUpdate);                //kein Update

  except
    Synchronize(DoNotifyOnError);                      //im Fehlerfall: kein Update
  end;  //of except
end;

{ Sync Events }
procedure TUpdateCheckThread.DoNotifyOnError;               //Sync OnError-Event
begin
  if Assigned(OnError) then
     OnError(Self);
end;


procedure TUpdateCheckThread.DoNotifyOnNoUpdate;         //Sync OnNoUpdate-Event
begin
  if Assigned(OnNoUpdate) then
     OnNoUpdate(Self);
end;


procedure TUpdateCheckThread.DoNotifyOnUpdate;             //Sync OnUpdate-Event
begin
  if Assigned(OnUpdate) then
     OnUpdate(Self, FNewBuild);
end;

end.
