{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Language Handler Unit v1.2                }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit LanguageFile;

{$IFDEF LINUX} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  Classes, SysUtils, Forms,
{$IFDEF MSWINDOWS}
  Windows;
{$ELSE}
  IniFiles, LCLType;

const
  { Interval for next language }
  LANGUAGE_INTERVAL = 100;
{$ENDIF}

type
  { Exception class }
  ELanguageException = class(Exception);

  { IChangeLanguageListener }
  IChangeLanguageListener = interface
  ['{FF4AAD19-49DC-403B-8EA0-3E24D984B603}']
    procedure SetLanguage(Sender: TObject);
  end;

  { MessageBox look }
  TMessageType = (mtInfo, mtWarning, mtQuestion, mtConfirm, mtError);

  { TLanguageFile }
  TLanguageFile = class(TObject)
  private
  {$IFDEF LINUX}
    FLang: string;
    FIni: TIniFile;
  {$ELSE}
    FLang: Word;
  {$ENDIF}
    FApplication: TApplication;
  protected
    FListeners: TInterfaceList;
  public
  {$IFDEF MSWINDOWS}
    constructor Create(ALanguage: Word; AApplication: TApplication);
  {$ELSE}
    constructor Create(ALanguage: string; AConfig: string = '';
      AApplication: TApplication = nil);
  {$ENDIF}
    destructor Destroy; override;
    procedure AddListener(AListener: IChangeLanguageListener);
    procedure ChangeLanguage(ASender: TObject; ALangID: Word);
  {$IFDEF LINUX}
    procedure GetLanguages(ASections: TStrings);
  {$ENDIF}
    function GetString(const AIndex: Word): string;
    function MessageBox(AText: string; AType: TMessageType = mtInfo;
      AUpdate: Boolean = False): Integer; overload;
    function MessageBox(TextID: Word; AType: TMessageType = mtInfo;
      AUpdate: Boolean = False): Integer; overload;
    procedure RemoveListener(AListener: IChangeLanguageListener);
    { external }
    property Lang: {$IFDEF LINUX}string{$ELSE}Word{$ENDIF} read FLang write FLang;
  end;

implementation

{ TLanguageFile }

{ public TLanguageFile.Create

  Constructor for creating a TLanguageFile instance. }

{$IFDEF MSWINDOWS}
{$R lang.res}

constructor TLanguageFile.Create(ALanguage: Word; AApplication: TApplication);
begin
  inherited Create;
  FLang := ALanguage;
  FApplication := AApplication;
  FListeners := TInterfaceList.Create;
end;

{ public TLanguageFile.Destroy

  Destructor for destroying a TLanguageFile instance. }

destructor TLanguageFile.Destroy;
begin
  FreeAndNil(FListeners);
  inherited Destroy;
end;
{$ENDIF}

{$IFDEF LINUX}
constructor TLanguageFile.Create(ALanguage: string; AConfig: string = '';
  AApplication: TApplication = nil);
begin
  if (AConfig = '') then
    AConfig := ExtractFilePath(ParamStr(0)) +'lang';

  if not FileExists(AConfig) then
    raise ELanguageException.Create('"'+ AConfig +'" not found!');

  FLang := ALanguage;
  FIni := TIniFile.Create(AConfig);
  FApplication := AApplication;
  FListeners := TInterfaceList.Create;
end;

{ public TLanguageFile.Destroy

  Destructor for destroying a TLanguageFile instance. }

destructor TLanguageFile.Destroy;
begin
  FIni.Free;
  FreeAndNil(FListeners);
  inherited Destroy;
end;

{ public TLanguageFile.GetString

  Loads a string from a *.ini file based language file. }

function TLanguageFile.GetString(const AIndex: Word) : string;
begin
  result := FIni.ReadString(FLang, IntToStr(AIndex + LANGUAGE_INTERVAL), '');
end;

{ public TLanguageFile.GetLanguages

  Returns a list containing all available languages. }

procedure TLanguageFile.GetLanguages(ASections: TStrings);
begin
  FIni.ReadSections(ASections);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

{ public TLanguageFile.GetString

  Loads a string from a StringTable file based language file. }

function TLanguageFile.GetString(const AIndex: Word): string;
var
  Buffer : array[0..80] of char;
  ls : Integer;

begin
  result := '';
  ls := LoadString(hInstance, AIndex + FLang, Buffer, SizeOf(buffer));

  if (ls <> 0) then
    result := Buffer;
end;
{$ENDIF}

{ public TLanguageFile.AddListener

  Adds a listener to the notification list. }

procedure TLanguageFile.AddListener(AListener: IChangeLanguageListener);
begin
  FListeners.Add(AListener);
end;

{ public TLanguageFile.ChangeLanguage

  Allows users to change the language. }

procedure TLanguageFile.ChangeLanguage(ASender: TObject; ALangID: Word);
var
  i: Word;
  Listener: IChangeLanguageListener;

begin
  FLang := ALangID;

  // Notify all listeners
  for i := 0 to FListeners.Count -1 do
    if Supports(FListeners[i], IChangeLanguageListener, Listener) then
      Listener.SetLanguage(Self);
end;

{ public TLanguageFile.MessageBox

  Shows a MessageBox with text and specific look. }

function TLanguageFile.MessageBox(AText: string; AType: TMessageType = mtInfo;
  AUpdate: Boolean = False): Integer;
var
  Title: string;
  Flags: Integer;

begin
  Flags := 0;

  case AType of
    mtInfo:
      begin
        Title := GetString(0);
        Flags := MB_ICONINFORMATION;
      end;

    mtWarning:
      begin
        Title := GetString(1);
        Flags := MB_ICONWARNING;
      end;

    mtQuestion:
      begin
        Title := GetString(3);
        Flags := MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1;
      {$IFDEF MSWINDOWS}
        MessageBeep(MB_ICONWARNING);
      {$ENDIF}
      end;

    mtConfirm:
      begin
        Title := GetString(4);
        Flags := MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2;
      {$IFDEF MSWINDOWS}
        MessageBeep(MB_ICONWARNING);
      {$ENDIF}
      end;

    mtError:
      begin
        Title := GetString(2);
        Flags := MB_ICONERROR;
      end;
  end;  //of case

  if AUpdate then
    Title := GetString(5);

  result := FApplication.MessageBox(PChar(AText), PChar(Title), Flags);
end;

{ public TLanguageFile.MessageBox

  Shows a MessageBox with text and specific look. }

function TLanguageFile.MessageBox(TextID: Word; AType: TMessageType = mtInfo;
  AUpdate: Boolean = False): Integer;
begin
  result := MessageBox(GetString(TextID), AType, AUpdate);
end;

{ public TLanguageFile.RemoveListener

  Removes a listener from the notification list. }

procedure TLanguageFile.RemoveListener(AListener: IChangeLanguageListener);
begin
  FListeners.Remove(AListener);
end;

end.

