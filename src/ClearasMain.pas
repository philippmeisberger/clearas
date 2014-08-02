{ *********************************************************************** }
{                                                                         }
{ Clearas Main Unit                                                       }
{                                                                         }
{ Copyright (c) 2011-2013 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit ClearasMain;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, ClearasAPI, Menus, Graphics, ShellAPI;

type
  TForm1 = class(TForm)
    PopupMenu: TPopupMenu;
    pmDeactivate: TMenuItem;
    N1: TMenuItem;
    pmExport: TMenuItem;
    pmDelete: TMenuItem;
    MainMenu: TMainMenu;
    mmView: TMenuItem;
    mmRefresh: TMenuItem;
    N2: TMenuItem;
    pmInfos: TMenuItem;
    mmHelp: TMenuItem;
    mmInfo: TMenuItem;
    mmEdit: TMenuItem;
    mmContext: TMenuItem;
    mmFile: TMenuItem;
    mmExportList: TMenuItem;
    N3: TMenuItem;
    mmClose: TMenuItem;
    mmExport: TMenuItem;
    mmAdd: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    mmDate: TMenuItem;
    mmImport: TMenuItem;
    mmDelBackup: TMenuItem;
    mmStandard: TMenuItem;
    N7: TMenuItem;
    mmOptimate: TMenuItem;
    N8: TMenuItem;
    pmSearch: TMenuItem;
    N6: TMenuItem;
    mmLang: TMenuItem;
    mmGer: TMenuItem;
    mmEng: TMenuItem;
    PageControl: TPageControl;
    tsStartup: TTabSheet;
    tsContext: TTabSheet;
    lwList: TListView;
    lStartup: TLabel;
    bActivate: TButton;
    bClose: TButton;
    bDeactivate: TButton;
    bDelete: TButton;
    bExport: TButton;
    lCopy1: TLabel;
    lCopy2: TLabel;
    bExportContext: TButton;
    bDeleteContext: TButton;
    bClose2: TButton;
    bDeactContext: TButton;
    bActContext: TButton;
    lwContext: TListView;
    lContext: TLabel;
    lVersion2: TLabel;
    lVersion: TLabel;
    pbLoad: TProgressBar;
    cbExpert: TCheckBox;
    lWindows2: TLabel;
    lWindows: TLabel;
    mmFra: TMenuItem;
    mmUpdate: TMenuItem;
    N9: TMenuItem;
    mmDwnldCert: TMenuItem;
    N10: TMenuItem;
    pmEdit: TMenuItem;
    mmReport: TMenuItem;
    mmRunOnce: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bDeleteContextClick(Sender: TObject);
    procedure bActivateClick(Sender: TObject);
    procedure bActContextClick(Sender: TObject);
    procedure bDeactivateClick(Sender: TObject);
    procedure bDeactContextClick(Sender: TObject);
    procedure bExportClick(Sender: TObject);
    procedure bExportContextClick(Sender: TObject);
    procedure lwListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lwContextSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lwContextDblClick(Sender: TObject);
    procedure lwListDblClick(Sender: TObject);
    procedure lwListColumnClick(Sender: TObject; Column: TListColumn);  //http://www.delphipraxis.net/283-wie-kann-ich-eine-listview-sortieren.html
    procedure lwListCompare(Sender: TObject; Item1, Item2: TListItem;   //http://www.delphipraxis.net/283-wie-kann-ich-eine-listview-sortieren.html
      Data: Integer; var Compare: Integer);
    procedure pmDeactivateClick(Sender: TObject);
    procedure pmInfosClick(Sender: TObject);
    procedure pmDeleteClick(Sender: TObject);
    procedure pmSearchClick(Sender: TObject);
    procedure pmEditClick(Sender: TObject);
    procedure mmImportClick(Sender: TObject);
    procedure mmRefreshClick(Sender: TObject);
    procedure mmContextClick(Sender: TObject);
    procedure mmExportListClick(Sender: TObject);
    procedure mmExportClick(Sender: TObject);
    procedure mmAddClick(Sender: TObject);
    procedure mmGerClick(Sender: TObject);
    procedure mmEngClick(Sender: TObject);
    procedure mmFraClick(Sender: TObject);
    procedure mmStandardClick(Sender: TObject);
    procedure mmOptimateClick(Sender: TObject);
    procedure mmDateClick(Sender: TObject);
    procedure mmDelBackupClick(Sender: TObject);
    procedure mmInfoClick(Sender: TObject);
    procedure mmUpdateClick(Sender: TObject);
    procedure mmDwnldCertClick(Sender: TObject);
    procedure mmReportClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure cbExpertClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lCopy1MouseLeave(Sender: TObject);
    procedure lCopy1MouseEnter(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lCopy1Click(Sender: TObject);
  private
    FLang, FColumnToSort: Word;
    Startup: TStartupList;
    Context: TContextList;
    FTab: TTabSheet;
    procedure ButtonHandle;
    procedure CountContext;
    procedure CountStartups;
    procedure EditPath(APath: string);
    procedure GetContext;
    procedure GetStartups(AGetNewData: Boolean = true);
    procedure OnSearchProgress(Sender: TObject; AWorkCount: integer);
    procedure OnSearchStart(Sender: TObject; AWorkCountMax: integer);
    procedure OnSearchEnd(Sender: TObject);
    procedure RegExport;
    procedure SetLanguage(ALangID: integer);
  public
    procedure ChangeLanguage(AMenuItem: TMenuItem; ALangID: integer);
    function MessageBox(AText: string; ACaptionID: Word = 0; AUpdate: Boolean = false): Integer; overload;
    function MessageBox(ATextID: Word; ACaptionID: Word = 0; AUpdate: Boolean = false): Integer; overload;
    function MessageBox(AText, ACaption: string; AFlags: Integer = MB_ICONINFORMATION): Integer; overload;
    property StartupProp: TStartupList read Startup;
    property Lang: Word read FLang;
  end;

var
  Form1: TForm1;

implementation

uses ClearasInfo, ClearasUpdate;

{$R *.dfm}
{$R manifest.res}
{$R lang.res}

{ private }
procedure TForm1.ButtonHandle;                          //Buttons de-/aktivieren
begin
  if (FTab = tsStartup) then                            //welche Page?
     begin
     if Startup.Item.Enabled then                       //Autostart
        begin
        bActivate.Enabled := false;
        bDeactivate.Enabled := true;
        bDeactivate.Default := true;
        end  //of begin
     else
        begin
        bDeactivate.Enabled := false;
        bActivate.Enabled := true;
        bActivate.Default := true;
        end;  //of if

     CountStartups;                                     //Z�hler Refresh
     end  //of begin
  else
     begin                                              //Kontextmen�
     if Context.Item.Enabled then
        begin
        bActContext.Enabled := false;
        bDeactContext.Enabled := true;
        bDeactContext.Default := true;
        end  //of begin
     else
        begin
        bDeactContext.Enabled := false;
        bActContext.Enabled := true;
        bActContext.Default := true;
        end;  //of if

     CountContext;                                      //Z�hler Refresh
     end;  //of begin
end;


procedure TForm1.CountContext;
begin
  lwContext.Columns[1].Caption := TClearas.GetString(87) +' ('+IntToStr(Context.ActCount)
                                   +'/'+ IntToStr(Context.Count)+')';     //Kontextmen�-Eintr�ge z�hlen
end;


procedure TForm1.CountStartups;
begin
  lwList.Columns[1].Caption := TClearas.GetString(2)+'('+IntToStr(Startup.ActCount)
                               +'/'+IntToStr(Startup.Count)+')';        //aktive Programme z�hlen
end;


procedure TForm1.EditPath(APath: string);
var
  FullPath: string;

  function AddCommas(APath: string): string;
  begin
    if (APath[1] <> '"') then
       result := '"'+ APath +'"'
    else
       result := APath;
  end;

begin
  with Startup do
    begin
    FullPath := InputBox(TClearas.GetString(53), TClearas.GetString(54), AddCommas(APath));  //Pfad bearbeiten Dialog

    if ((FullPath = '') or (FullPath = ' '))then
       FullPath := AddCommas(APath);                                   //Fehlervermeidung: Standard-Wert

    if Item.Enabled then                                               //Item aktiviert?
       TClearas.WriteStrValue(Item.RootKey, Item.KeyPath, Item.Name, FullPath)  //neuen Pfad in REG schreiben
    else
       begin                                                           //Item deaktiviert
       TClearas.WriteStrValue(Item.RootKey, Item.KeyPath, 'Command', FullPath); //neuen Pfad in REG schreiben
       TClearas.WriteStrValue(Item.RootKey, Item.KeyPath, 'Item', Item.Name);   //Name in REG schreiben
       end;  //of if

    lwList.ItemFocused.SubItems[1] := FullPath;                        //in Liste schreiben
    end;  //of begin
end;


procedure TForm1.GetContext;                             //Kontextmen�s auslesen
var
  i: integer;

begin
  pbLoad.Visible := true;                            //Progressbar sichtbar machen
  pbLoad.Position := 0;                              //Progressbar auf 0
  Context.Clear;                                     //Kontextmen� Liste leeren

  try
    if cbExpert.Checked then
       begin
       Context.OnSearchBegin := OnSearchStart;       //Events verkn�pfen
       Context.OnSearch := OnSearchProgress;
       Context.OnSearchEnd := OnSearchEnd;
       Application.ProcessMessages;                  //auslagern
       Context.AddEntry();                           //Eintr�ge suchen
       end  //of begin
    else
       Context.Load;                                 //Kontextmen�-Eintr�ge in Liste

    lwContext.Clear;                                 //Kontextmen�-ListView leeren

    for i := 0 to Context.Count -1 do                //Kontextmen�s ausgeben
      with lwContext.Items.Add do
        begin
        Caption := Context[i].GetStatus;
        SubItems.Append(Context[i].Name);
        SubItems.Append(Context[i].Location);
        SubItems.Append(Context[i].TypeOf);
        end; //of with

  finally
    CountContext;                                    //Z�hler Refresh
    pbLoad.Visible := false;                         //Progressbar unsichtbar
  end;  //of finally
end;


procedure TForm1.GetStartups(AGetNewData: Boolean = true);  //Autostart auslesen
var
  i: integer;

begin
  lwList.Clear;                                  //Autostart-ListView leeren

  try                                            
    if AGetNewData then                          //Daten neu auslesen?
       begin
       Startup.Clear;                            //init bzw Reset Z�hler
       Startup.Load(mmRunOnce.Checked);          //Autostart auslesen
       end;  //of begin

    for i := 0 to Startup.Count -1 do            //Autostart-Programme ausgeben
      with lwList.Items.Add do
        begin
        Caption := Startup[i].GetStatus;
        SubItems.Append(Startup[i].Name);
        SubItems.Append(Startup[i].FilePath);
        SubItems.Append(Startup[i].TypeOf);

        if mmDate.Checked then
           SubItems.Append(Startup[i].Time);
        end;  //of with

    if bActivate.Enabled then
       bActivate.Enabled := false
    else
       bDeactivate.Enabled := false;

  finally                                        //freigeben
    CountStartups;                               //Z�hler Refresh
  end;  //of finally
end;

{ Thread Events }
procedure TForm1.OnSearchProgress(Sender: TObject; AWorkCount: integer);
begin
  pbLoad.Position := AWorkCount;                //ProgressBar Position
end;


procedure TForm1.OnSearchStart(Sender: TObject; AWorkCountMax: integer);
begin
  pbLoad.Max := AWorkCountMax;                  //ProgressBar Ende
end;


procedure TForm1.OnSearchEnd(Sender: TObject);
begin
  pbLoad.Visible := false;
  pbLoad.Position := 0;
end;


procedure TForm1.RegExport;                                         //REG-Export
var
  SaveDialog: TSaveDialog;

begin
  SaveDialog := TSaveDialog.Create(Self);           //init SaveDialog

  with SaveDialog do
    begin
    Options := Options + [ofOverwritePrompt];       //�berschreiben-Dialog
    Filter := TClearas.GetString(36);                //Filter nur f�r *.reg festlegen
    DefaultExt := '.reg';                           //*.reg-Endung
    Title := TClearas.GetString(33);                 //Fenstername

    if (FTab = tsStartup) then
       FileName := Startup.Item.Name + DefaultExt   //default-Name
    else
       FileName := Context.Item.Name +'_'+ Context.Item.Location + DefaultExt;  //default-Name
    end;  //of with

  try
    if SaveDialog.Execute then                      //"speichern" Klick
       if (FTab = tsStartup) then
          Startup.ExportItem(SaveDialog.FileName)   //"Speichern"
       else
          Context.ExportItem(SaveDialog.FileName);  //"Speichern"

  finally
    SaveDialog.Free;
  end;  //of finally
end;


procedure TForm1.SetLanguage(ALangID: integer);           //Sprache lokalisieren
begin
  FLang := ALangID;                                       //Sprache w�hlen

  with TClearas do
    begin
    tsContext.Caption := GetString(84);                   //Buttons Autostart
    bActivate.Caption := GetString(6);
    bDeactivate.Caption := GetString(7);
    bExport.Caption := GetString(8);
    bDelete.Caption := GetString(9);
    bClose.Caption := GetString(10);

    lStartup.Caption := GetString(0);                     //Liste Autostart
    lwList.Columns[0].Caption := GetString(1);
    lwList.Columns[2].Caption := GetString(11);
    lwList.Columns[3].Caption := GetString(4);

    bActContext.Caption := bActivate.Caption;             //Buttons Kontextmen�
    bDeactContext.Caption := bDeactivate.Caption;
    bExportContext.Caption := bExport.Caption;
    bDeleteContext.Caption := bDelete.Caption;
    bClose2.Caption := bClose.Caption;
    cbExpert.Caption := GetString(90);
    tsStartup.Caption := GetString(83);

    lContext.Caption := GetString(86);                    //Liste Kontextmen�
    lwContext.Columns[0].Caption := lwList.Columns[0].Caption;
    lwContext.Columns[1].Caption := GetString(87);
    lwContext.Columns[2].Caption := GetString(88);
    lwContext.Columns[3].Caption := GetString(4);

    mmFile.Caption := GetString(11);                      //"Datei"-Men�
    mmAdd.Caption := GetString(15);
    mmImport.Caption := GetString(16);
    mmExport.Caption := GetString(17);
    mmExportlist.Caption := GetString(18);
    mmClose.Caption := GetString(10);

    mmEdit.Caption := GetString(12);                      //"Bearbeiten"-Men�
    mmContext.Caption := GetString(19);
    mmDelBackup.Caption := GetString(20);

    mmView.Caption := GetString(13);                      //"Ansicht"-Men�
    mmRefresh.Caption := GetString(21);
    mmStandard.Caption := GetString(22);
    mmOptimate.Caption := GetString(23);
    mmDate.Caption := GetString(24);
    mmRunOnce.Caption := GetString(37);
    mmLang.Caption := GetString(3);

    mmHelp.Caption := GetString(14);                      //"Hilfe"-Men�
    mmUpdate.Caption := GetString(91);
    mmDwnldCert.Caption := GetString(97);
    mmReport.Caption := GetString(108);
    mmInfo.Caption := GetString(26);

    pmDeactivate.Caption := GetString(6);                 //Kontextmen�
    pmSearch.Caption := GetString(34);
    pmEdit.Caption := GetString(53);
    pmExport.Caption := GetString(8);
    pmDelete.Caption := GetString(9);
    pmInfos.Caption := GetString(35);
    end;  //of with
end;
{ of private }

{ public }
procedure TForm1.ChangeLanguage(AMenuItem: TMenuItem; ALangID: integer);
begin
  AMenuItem.Checked := not AMenuItem.Checked;
  SetLanguage(ALangID);
  GetStartups;
  GetContext;
end;


function TForm1.MessageBox(AText: string; ACaptionID: Word = 0; AUpdate: Boolean = false): Integer;
var
  Title: string;
  Flags: Integer;

begin
  case ACaptionID of
    0: begin
       Title := TClearas.GetString(28);
       Flags := MB_ICONINFORMATION;
       end;

    1: begin
       Title := TClearas.GetString(29);
       Flags := MB_ICONWARNING;
       end;

    2: begin
       Title := TClearas.GetString(27);
       Flags := MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1;
       end;

    3: begin
       Title := TClearas.GetString(30);
       Flags := MB_ICONERROR;
       end;
    else
      Flags := 0;
  end;  //of case

  if AUpdate then
     Title := TClearas.GetString(100);

  result := Application.MessageBox(PChar(AText), PChar(Title), Flags);
end;


function TForm1.MessageBox(ATextID: Word; ACaptionID: Word = 0; AUpdate: Boolean = false): Integer;
begin
  result := MessageBox(TClearas.GetString(ATextID), ACaptionID, AUpdate);
end;


function TForm1.MessageBox(AText, ACaption: string; AFlags: Integer = MB_ICONINFORMATION): Integer;
begin
  result := Application.MessageBox(PChar(AText), PChar(ACaption), AFlags);
end;
{ of public }

{##############################################################################}
{ Events }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTab := tsStartup;                            //ActivePage auf Autostart
  Startup := TStartupList.Create;               //init Autostart-Objekt
  Context := TContextList.Create;               //init Kontextmen�-Objekt
  Caption := Caption + TClearas.GetArchitecture; //Bits in Caption schreiben
end;


procedure TForm1.FormDestroy(Sender: TObject);      //Objekte + Listen freigeben
begin
  Startup.Free;
  Context.Free;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  BringToFront;                              //Form in Vordergrund
  lWindows.Caption := 'Windows '+ TClearas.GetWinVersion(true);  //Windows-Version in Label
  lWindows2.Caption := lWindows.Caption;
  mmDate.Enabled := TClearas.CheckWindows;   //"Deaktivierungsdatum" nur ab Vista

  if (TClearas.CheckWindows or (TClearas.GetWinVersion(false)[1] in ['X','2'])) then
     begin
     SetLanguage(100);                       //Standard Sprache (Deutsch)
     mmContext.Checked := TClearas.UpdateContextPath;  //Pfad in Kontextmen� aktualisieren
     GetStartups;
     end  //of begin
  else                                       //bei falschem Betriebssystem
     begin                                   //Message ausgeben
     MessageBox(TClearas.GetString(81) + TClearas.GetWinVersion(false)
               + TClearas.GetString(82), 1);
     mmExportList.Enabled := false;
     mmRefresh.Enabled := false;
     mmContext.Enabled := false;
     lwList.Enabled := false;
     lwContext.Enabled := false;
     cbExpert.Enabled := false;
     end;  //of if
end;

{ Exportieren Events }
procedure TForm1.bExportClick(Sender: TObject);              //Export als Backup
begin
  with Startup do
    if (Item.Enabled and Item.StartupUser) then
       begin
       if CreateBackup() then                                //Backup erstellen
          begin
          MessageBox(TClearas.GetString(44) +'"'+ TClearas.GetWinDir +'\pss\"'+ TClearas.GetString(45));
          bExport.Enabled := false;
          end  //of begin
       else
          MessageBox(46, 3);
       end  //of begin
  else
     RegExport;                                              //Export als *.reg
end;


procedure TForm1.bExportContextClick(Sender: TObject);     //Kontextmen�-Eintrag
begin                                                      //exportieren
  RegExport;
end;

{ L�schen Events }
procedure TForm1.bDeleteClick(Sender: TObject);
var
  DelBackup, Exported: Boolean;

begin
  DelBackup := Startup.DeleteBackup;                     //Backup l�schen speichern
  Exported := false;

  with TClearas do
    begin
    if (MessageBox(GetString(48) +'"'+ Startup.Item.Name +'"'+ GetString(49) +^J
       + GetString(50) + GetString(57), GetString(27), MB_ICONWARNING or MB_YESNO or
       MB_DEFBUTTON2) = IDYES) then                      //wirklich l�schen?
       begin
       if (bExport.Enabled and (MessageBox(GetString(52), 2) = IDYES)) then  //existiert Backup?
          begin
          bExport.Click;                                                     //falls nein: Backup erstellen?
          DelBackup := false;
          Exported := true;
          end;  //of begin

       if ((not Exported) and Startup.Item.StartupUser and Startup.BackupExists) then
          Startup.DeleteBackup := (MessageBox(GetString(47), 2) = IDYES);  //Backup l�schen?

       if Startup.DeleteItem() then                      //L�schen + auf Fehler pr�fen
          begin
          lwList.DeleteSelected;                         //Element aus ListView l�schen
          CountStartups;                                 //Z�hler Refresh
          end  //of begin
       else
          MessageBox(TClearas.GetString(65) + TClearas.GetString(66), 3);
       end;  //of if
    end;  //of with

  Startup.DeleteBackup := DelBackup;                     //Backup l�schen zur�cksetzen
end;


procedure TForm1.bDeleteContextClick(Sender: TObject);     //Kontextmen�-Eintrag
begin                                                      //l�schen
  with TClearas do
    begin
    if (MessageBox(GetString(85) +'"'+ Context.Item.Name +'"'+ GetString(49) +^J
       + GetString(50), GetString(27), MB_ICONWARNING or MB_YESNO or
       MB_DEFBUTTON2) = IDYES) then                       //wirklich l�schen?
       begin
       if (MessageBox(GetString(52), 2) = IDYES) then     //exportieren?
          RegExport;

       if Context.DeleteItem() then                       //l�schen
          begin
          lwContext.DeleteSelected;                       //Element aus ListView l�schen
          CountContext;                                   //Z�hler Refresh
          end  //of begin
       else
          MessageBox(TClearas.GetString(65) + TClearas.GetString(66), 3);
       end;  //of begin
    end;  //of with
end;

{ Aktivieren Events }
procedure TForm1.bActivateClick(Sender: TObject);
begin
  if Startup.ChangeItemStatus() then                       //aktivieren + auf Fehler pr�fen
     begin
     lwList.ItemFocused.Caption := Startup.Item.GetStatus; //optisch "Aktiviert" setzen
     ButtonHandle;                                         //"aktivieren"-Button deaktivieren
     bExport.Enabled := not Startup.BackupExists;

     if mmDate.Checked then                                //Deaktivierungsdatum verf�gbar?
        lwList.ItemFocused.SubItems[3] := '';              //...dann Zeitpunkt aus Liste l�schen
     end  //of begin
  else
     MessageBox(TClearas.GetString(63) + TClearas.GetString(66), 3);
end;


procedure TForm1.bActContextClick(Sender: TObject);        //Kontextmen�-Eintrag
begin                                                      //aktivieren
  if Context.ChangeItemStatus() then
     begin
     lwContext.ItemFocused.Caption := Context.Item.GetStatus;
     ButtonHandle;
     end  //of begin
  else
     MessageBox(TClearas.GetString(63) + TClearas.GetString(66), 3);
end;

{ Deaktivieren Events }
procedure TForm1.bDeactivateClick(Sender: TObject);
begin
  if Startup.ChangeItemStatus() then                        //Deaktivieren!
     begin
     lwList.ItemFocused.Caption := Startup.Item.GetStatus;  //optisch "Deaktiviert" setzen
     ButtonHandle;                                          //"deaktivieren"-Button deaktivieren

     if mmDate.Checked then                                 //Deaktivierungsdatum verf�gbar
        lwList.ItemFocused.SubItems[3] := Startup.Item.Time;  //...dann in Liste schreiben

     if (Startup.Item.StartupUser and not bExport.Enabled) then
        bExport.Enabled := true;
     end  //of begin
  else
     MessageBox(TClearas.GetString(64) + TClearas.GetString(66), 3);
end;


procedure TForm1.bDeactContextClick(Sender: TObject);      //Kontextmen�-Eintrag
begin                                                      //deaktivieren
  if Context.ChangeItemStatus() then
     begin
     lwContext.ItemFocused.Caption := Context.Item.GetStatus;
     ButtonHandle;
     end  //of begin
  else
     MessageBox(TClearas.GetString(64) + TClearas.GetString(66), 3);
end;

{ Selektion Events }
procedure TForm1.lwListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Index: Word;

  function IsDouble(AName: string; AStartIndex: Integer): Integer;
  var
    i, j, k: Integer;

  begin
    result := -1;
    j := 0;

    for i := AStartIndex +1 to lwList.Items.Count -1 do
      if (AName = lwList.Items.Item[i].SubItems[0]) then
         begin
         Inc(j);
         result := i;
         Break;
         end;  //of begin

    if (j = 0) then
       for k := 0 to Item.Index -1 do
         if (AName = lwList.Items.Item[k].SubItems[0]) then
            begin
            result := k;
            Break;
            end;  //of begin
  end;

begin
  if (Selected and (Item <> nil)) then         //Item selektiert?
      begin
      Index := Startup.IndexOf(Item.SubItems[0], (Item.Caption = TClearas.GetString(31)));
      Startup.Item := Startup.Items[Index];    //Pointer setzen
      bClose.Default := false;

      if (Startup.Item.StartupUser and Startup.Item.Enabled and Startup.BackupExists) then
         begin
         bExport.Enabled := false;
         pmExport.Enabled := false;
         end //of begin
      else
         begin
         bExport.Enabled := true;
         pmExport.Enabled := true;
         end;  //of if

      bDelete.Enabled := true;
      pmInfos.Enabled := true;
      pmDelete.Enabled := true;

      if ((Item.SubItems[2] <> '') and ((Item.SubItems[1] = '') or
         (Item.SubItems[1] = ' '))) then     //Schl�ssel existiert und Pfad fehlt
         begin                               //...dann kann User danach suchen
         bActivate.Enabled := false;
         bDeactivate.Enabled := false;
         pmDeactivate.Enabled := false;
         bExport.Enabled := false;
         pmExport.Enabled := false;
         bDelete.Default := true;
         pmSearch.Visible := true;
         end  //of begin
      else
         begin
         if ((Item.Data = Startup.AppIndex) and (Assigned(Startup.AppIndex) or
            Assigned(Item.Data))) then     //ist Item das ge�nderte Programm?
            begin                         
            pmSearch.Visible := true;

            if pmEdit.Enabled then        //dann erlauben, den Pfad zu �ndern
               pmEdit.Visible := true;
            end  //of begin
         else
            begin
            pmSearch.Visible := false;    
            pmEdit.Visible := false;
            end;  //of if

         if (Item.SubItems[2] = '') then  //Schl�ssel fehlt
            begin
            bActivate.Enabled := false;
            bDeactivate.Enabled := false;
            bExport.Enabled := false;
            pmExport.Enabled := false;
            pmDeactivate.Enabled := false;
            bDelete.Default := true;
            end  //of begin
         else
            begin
            if (IsDouble(Item.SubItems[0], Item.Index) <> -1) then
               begin
               bActivate.Enabled := false;
               bDeactivate.Enabled := false;
               bDelete.Default := true;
               end  //of begin
            else
               begin
               if bDelete.Default then
                  bDelete.Default := false;

               if Startup.Item.Enabled then                       //aktiviert?
                  begin                                           //JA
                  bDelete.Default := false;
                  bActivate.Enabled := false;
                  bDeactivate.Enabled := true;
                  bDeactivate.Default := true;
                  pmDeactivate.Enabled := true;
                  pmDeactivate.Caption := TClearas.GetString(7);  //Beschriftung �ndern
                  end //of begin
               else
                  begin                                           //NEIN
                  bDelete.Default := false;
                  bActivate.Enabled := true;
                  bDeactivate.Enabled := false;
                  bActivate.Default := true;
                  pmDeactivate.Enabled := true;
                  pmDeactivate.Caption := TClearas.GetString(6);  //Beschriftung �ndern
                  end; //of if

               if not KeyPreview then                 //Hotkeys aktiviert?
                  KeyPreview := true;
               end; //of if
            end;  //of if
         end;  //of if
      end  //of begin
  else                                             //nichts angew�hlt, dann
     begin                                         //Buttons deaktivieren
     if bActivate.Enabled then
        begin
        bActivate.Default := false;
        bActivate.Enabled := false;
        end  //of begin
     else
        begin
        bDeactivate.Default := false;
        bDeactivate.Enabled := false;
        end;  //of if

     pmDeactivate.Enabled := false;
     pmDelete.Enabled := false;
     pmExport.Enabled := false;
     pmInfos.Enabled := false;
     bExport.Enabled := false;
     bDelete.Enabled := false;
     bClose.Default := true;
     end;  //of if
end;


procedure TForm1.lwContextSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Index: Word;

begin
  if (Selected and (Item <> nil)) then                  //Item selektiert?
      begin
      Index := Context.IndexOf(Item.SubItems[0], Item.SubItems[1]);
      Context.Item := Context.Items[Index];             //Pointer setzen
      bClose2.Default := false;
      bDeleteContext.Enabled := true;
      pmDelete.Enabled := true;
      bExportContext.Enabled := true;
      pmExport.Enabled := true;
      pmInfos.Enabled := true;
      pmInfos.Visible := (Item.SubItems[2] = 'Shell');  //Eigenschaften f�r Shell anzeigen

      if bDeletecontext.Default then
         bDeletecontext.Default := false;

      if (Item.Caption = TClearas.GetString(31)) then    //aktiviert?
         begin
         bActContext.Enabled := false;                  //JA
         bDeactContext.Enabled := true;
         bDeactContext.Default := true;
         pmDeactivate.Enabled := true;
         pmDeactivate.Caption := TClearas.GetString(7);  //Beschriftung �ndern
         end //of begin
      else
         begin                                          //NEIN
         bActContext.Enabled := true;
         bDeactContext.Enabled := false;
         bActContext.Default := true;
         pmDeactivate.Enabled := true;
         pmDeactivate.Caption := TClearas.GetString(6);  //Beschriftung �ndern
         end; //of if
      end  //of begin
  else                                                  //nichts angew�hlt, dann
     begin                                              //Buttons deaktivieren
     if bActContext.Enabled then
        begin
        bActContext.Default := false;
        bActContext.Enabled := false;
        end  //of begin
     else
        begin
        bDeactcontext.Default := false;
        bDeactcontext.Enabled := false;
        end;  //of if

     pmDeactivate.Enabled := false;
     pmExport.Enabled := false;
     pmDelete.Enabled := false;
     pmInfos.Enabled := false;
     bExportContext.Enabled := false;
     bDeleteContext.Enabled := false;
     bClose2.Default := true;
     end;  //of if
end;

{ Doppelklick Events }
procedure TForm1.lwListDblClick(Sender: TObject);
begin
  if bActivate.Enabled then
     bActivate.Click
  else
     if bDeactivate.Enabled then
        bDeactivate.Click
     else
        if bDelete.Enabled then
           bDelete.Click;
end;


procedure TForm1.lwContextDblClick(Sender: TObject);
begin
  if bActContext.Default then
     bActContext.Click
  else
     if bDeactContext.Default then
        bDeactContext.Click
     else
        if bDeleteContext.Default then
           bDeleteContext.Click;
end;

{ Liste sortieren Event }
procedure TForm1.lwListColumnClick(Sender: TObject; Column: TListColumn);
begin
  FColumnToSort := Column.Index;
  (Sender as TCustomListView).AlphaSort;
end;


procedure TForm1.lwListCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);                     //ListView sortieren
begin
  if (FColumnToSort = 0) then                               //"Aktiv"-Spalte
     Compare := CompareText(Item1.Caption, Item2.Caption)
  else                                                      //andere Spalte
     begin
     Data := FColumnToSort -1;

     if (Data < 3) then
        Compare := CompareText(Item1.SubItems[Data], Item2.SubItems[Data]);
     end;  //of if
end;

{##############################################################################}
{ Kontextmen� }

procedure TForm1.pmDeactivateClick(Sender: TObject);            //de-/aktivieren
begin
  if (FTab = tsStartup) then                    //Autostart
     lwListDblClick(Sender)
  else
     lwContextDblClick(Sender);                 //Kontext
end;


procedure TForm1.pmDeleteClick(Sender: TObject);                       //l�schen
begin
  if (FTab = tsStartup) then                    //Autostart
     bDelete.Click
  else
     bDeleteContext.Click;                      //Kontext
end;


procedure TForm1.pmInfosClick(Sender: TObject);                  //Eigenschaften
var
  Path, Name: string;

begin
  if (FTab = tsStartup) then
    Startup.Item.GetItemInfo(Name, Path)
  else
    Context.Item.GetItemInfo(Name, Path);

  MessageBox(Path, Name);        //Text ausgeben
end;


procedure TForm1.pmSearchClick(Sender: TObject);          //nach Programm suchen
var
  OpenDialog: TOpenDialog;
  index: integer;

begin
  OpenDialog := TOpenDialog.Create(Self);                   //init OpenDialog

  with OpenDialog do
    begin
    InitialDir := TClearas.GetWinDir[1] +':\Program Files';  //Programme Verzeichnis
    Filter := TClearas.GetString(38);                        //Filter
    Title := TClearas.GetString(34);                         //Fenstername
    end;  //of with

  try
    if OpenDialog.Execute then                              //"�ffnen" Klick
       begin
       index := lwList.ItemFocused.Index;                   //Index speichern
       lwList.ItemFocused.Data := Pointer(index);           //in Data schreiben
       Startup.AppIndex := lwList.ItemFocused.Data;         //Pointer auf Item
       EditPath(OpenDialog.FileName);                       //"Pfad bearbeiten"-Dialog
       lwListSelectItem(Self, lwList.ItemFocused, true);    //Eintrag selektieren
       pmEdit.Enabled := true;                              //"Pfad bearbeiten" in Kontextmen�
       end;  //of begin

  finally                                                   //freigeben
    OpenDialog.Free;
  end;  //of finally
end;


procedure TForm1.pmEditClick(Sender: TObject);               //"Pfad bearbeiten"
begin
  EditPath(lwList.ItemFocused.SubItems[1]);
end;
{ of Kontextmen� }

{##############################################################################}
{ Mainmenu }

procedure TForm1.mmAddClick(Sender: TObject);              //Programm hinzuf�gen
var
  OpenDialog: TOpenDialog;
  Name, Params: string;
  
  procedure CreateLnkFile(AErrorID: Integer = 55);       //*.Lnk Datei erstellen
  begin
    with TClearas do
	  if (Startup.IndexOf(Name) = -1) then                  //existiert Programm?
         if not CreateLnk(OpenDialog.FileName, TClearas.GetStartUpDir(false) + Name) then
            MessageBox(GetString(AErrorID), 3)
         else
            GetStartups
      else
         MessageBox(GetString(40) +'"'+ Name +'" '+ GetString(42), 1);
  end;
  
begin
  OpenDialog := TOpenDialog.Create(Self);                   //init OpenDialog

  with OpenDialog do
    begin
    InitialDir := TClearas.GetWinDir[1] +':\Program Files';  //Programme Verzeichnis
    Filter := TClearas.GetString(38);                        //Filter
    Title := TClearas.GetString(15);                         //Fenstername
    end;  //of with

  try
    if OpenDialog.Execute then                              //"�ffnen" Klick
       with TClearas do
         case OpenDialog.FilterIndex of
           1: begin
              Name := DeleteExt(ExtractFileName(OpenDialog.FileName)) +'.lnk';
              CreateLnkFile(55);                            //*.Lnk Datei erstellen
              end;  //of begin

           2: begin
              Name := ExtractFileName(OpenDialog.FileName);
			        CreateLnkFile(56);                            //*.Lnk Datei erstellen
              end;  //of begin

           3: begin
              Name := DeleteExt(ExtractFileName(OpenDialog.FileName));  //Name f�r Eintrag vorschlagen
              Name := InputBox(GetString(106), GetString(107), Name);   //Name bearbeiten

              if (Name = '') then
                 Exit;
                 
              if not InputQuery(GetString(109), GetString(110), Params) then  //evtl. Parameter
                 Exit;

              if (Startup.IndexOf(Name) = -1) then                      //existiert Programm?
                 try
                   WriteStrValue('HKCU', STARTUP_KEY, Name, OpenDialog.FileName +' '+ Params);
                   GetStartups;

                 except
                   MessageBox(GetString(40) +'"'+ Name +'" '+ GetString(42), 1);
                 end;  //of except
              end;  //of begin
         end;  //of case

  finally                                                   //freigeben
    OpenDialog.Free;
  end;  //of finally
end;


procedure TForm1.mmImportClick(Sender: TObject);            //Backup importieren
var
  OpenDialog: TOpenDialog;
  Path, Name: string;
  
  procedure CreateLnkFile(AAllUsers: Boolean);      //*.Lnk Datei erstellen
  begin
    with TClearas do
      if (Startup.IndexOf(Name) = -1) then                  //existiert Programm?
         begin
         CreateLnk(Path, GetStartupDir(AAllUsers) + Name); //Currnt User Dir
         GetStartups;                                       //aktualisieren
         end  //of begin
      else
         MessageBox(GetString(40) +'"'+ Name +'" '+ GetString(42), 1);
  end;
  
begin
  OpenDialog := TOpenDialog.Create(Self);                   //init OpenDialog

  if not DirectoryExists(TClearas.GetWinDir +'\pss') then   //existiert Dir nicht?
     ForceDirectories(TClearas.GetWinDir +'\pss');          //dann Dir erstellen

  with OpenDialog do
    begin
    InitialDir := TClearas.GetWinDir +'\pss';               //Zeiger auf Dir
    Filter := TClearas.GetString(39) +' *'+ COMMON_EXT +'|*'+ COMMON_EXT;
    Title := TClearas.GetString(16);
    end;  //of with

  try
    if OpenDialog.Execute then                              //"speichern" Klick
       with TClearas do
         begin
         ReadLnkFile(OpenDialog.FileName, Path);            //Verkn�pfung auslesen
         Name := ExtractFileName(DeleteExt(OpenDialog.FileName));
         CreateLnkFile(OpenDialog.FilterIndex = 2);
         end;  //of with

  finally                             //freigeben
    OpenDialog.Free;
  end;   //of finally
end;


procedure TForm1.mmExportClick(Sender: TObject);         //exportieren als *.reg
begin
  if ((FTab = tsStartup) and bExport.Enabled) then
     bExport.Click
  else
     if ((FTab = tsContext) and bExportContext.Enabled) then
        RegExport
     else
        MessageBox(TClearas.GetString(58) + TClearas.GetString(66) +^J
                  + TClearas.GetString(59), 1);
end;


procedure TForm1.mmExportListClick(Sender: TObject);     //exportieren als Liste
var
  SaveDialog: TSaveDialog;

begin
  SaveDialog := TSaveDialog.Create(Self);           //init SaveDialog

  with SaveDialog do
    begin
    Options := Options + [ofOverwritePrompt];       //�berschreiben-Dialog
    Filter := TClearas.GetString(36);               //Filter festlegen
    DefaultExt := '.reg';                           //*.reg-Endung
    Title := TClearas.GetString(18);                 //Fenstername
    FileName := TClearas.GetString(11) + DefaultExt; //default-Name
    end;  //of with

  try
    if SaveDialog.Execute then                      //"speichern" Klick
       Startup.ExportList(SaveDialog.FileName);     //speichern als *.reg

  finally
    SaveDialog.Free;
  end;  //of finally
end;


procedure TForm1.mmDelBackupClick(Sender: TObject);
begin
  Startup.DeleteBackup := not Startup.DeleteBackup;
end;


procedure TForm1.mmContextClick(Sender: TObject);          //Kontextmen�-Eintrag
begin
  mmContext.Checked := TClearas.RegisterInContextMenu(mmContext.Checked);
end;


procedure TForm1.mmRefreshClick(Sender: TObject);              //"aktualisieren"
begin
  if (FTab = tsStartup) then              //Reiter Autostart aktiv?
     GetStartups
  else
     GetContext;                          //Reiter Kontextmen� aktiv?
end;


procedure TForm1.mmStandardClick(Sender: TObject);       //Standard Spaltengr��e
begin
  if (FTab = tsStartup) then                             //Reiter Autostart
     begin
     lwList.Columns[1].Width := 125;
     lwList.Columns[2].Width := 124;
     lwList.Columns[3].Width := 80;
     end  //of begin
  else
     begin                                               //Reiter Kontextmen�
     lwContext.Columns[1].Width := 150;
     lwContext.Columns[2].Width := 115;
     lwContext.Columns[3].Width := 65;
     end;  //of if
end;


procedure TForm1.mmOptimateClick(Sender: TObject);     //Spaltengr��e optimieren
begin
  if (FTab = tsStartup) then                           //Reiter Autostart
     begin
     lwList.Columns[1].Width := ColumnTextWidth;
     lwList.Columns[2].Width := ColumnTextWidth;
     lwList.Columns[3].Width := ColumnTextWidth;
     end  //of begin
  else
     begin                                             //Reiter Kontextmen�
     lwContext.Columns[1].Width := ColumnTextWidth;
     lwContext.Columns[2].Width := ColumnTextWidth;
     end;  //of if
end;


procedure TForm1.mmDateClick(Sender: TObject);                  //Datum anzeigen
var
  endOf: integer;

begin
  if not (WindowState = wsMaximized) then
     lwList.Clear;

  if mmDate.Checked then
     begin                            //Spalte l�schen
     mmDate.Checked := false;

     if (lwList.Columns.Count > 4) then
        begin
        Constraints.MinWidth := Constraints.MinWidth - lwList.Column[4].Width;
        lwList.Column[4].MinWidth := 0;
        endOf := Width - 120;

        repeat                        //Animation "langsam zuschieben"
          Width := Width - 1;

          if (WindowState = wsMaximized) then
             endOf := Width;

          if (lwList.Column[4].Width <= 120) then
             lwList.Column[4].Width := lwList.Column[4].Width - 1;

          Update;
          Sleep(0);
        until ((lwList.Column[4].Width <= 0) and (Width = endOf));

        lwList.Columns.Delete(4);
        GetStartups(false);           //aktualisieren
        end;   //of if
     end   //of begin
  else
     begin                            //neue Spalte anlegen
     mmDate.Checked := true;
     lwList.Columns.Add.Caption := TClearas.GetString(5);
     endOf := Width + 120;

     if (WindowState = wsMaximized) then
        GetStartups(false);           //aktualisieren

     repeat                           //Animation "langsam aufschieben"
       Width := Width + 1;

       if (WindowState = wsMaximized) then
          endOf := Width;

       if (lwList.Column[4].Width < 120) then
          lwList.Column[4].Width := lwList.Column[4].Width + 1;

       Update;
       Sleep(0);
     until ((lwList.Column[4].Width >= 120) and (Width = endOf));

     lwList.Column[4].MinWidth := lwList.Column[4].Width;
     lwList.Column[4].MaxWidth := lwList.Column[4].Width;
     Constraints.MinWidth := Constraints.MinWidth + lwList.Column[4].Width;

     if not (WindowState = wsMaximized) then
        GetStartups(false);           //aktualisieren
     end;  //of if
end;


procedure TForm1.mmGerClick(Sender: TObject);                //Sprache "Deutsch"
begin
  ChangeLanguage(mmGer, 100);
end;


procedure TForm1.mmEngClick(Sender: TObject);               //Sprache "Englisch"
begin
  ChangeLanguage(mmEng, 300);
end;


procedure TForm1.mmFraClick(Sender: TObject);            //Sprache "Franz�sisch"
begin
  ChangeLanguage(mmFra, 500);
end;


procedure TForm1.mmUpdateClick(Sender: TObject);                        //Update
begin
  with Form3 do
    begin
    UserUpdate := true;             //Messaging einschalten

    if UpdateExists then            //falls Update existiert...
       DoUpdate                     //Download
    else
       CheckForUpdate;              //andernfalls danach suchen
	end;  //of with
end;


procedure TForm1.mmDwnldCertClick(Sender: TObject);      //Zertifikat downloaden
begin
  mmDwnldCert.Enabled := false;
  Enabled := false;
  Form3.Initialize(97, utCert);
end;


procedure TForm1.mmReportClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(URL_CONTACT), nil, nil,
    SW_ShowNormal);
end;


procedure TForm1.mmInfoClick(Sender: TObject);              //Info-Form anzeigen
begin
  Application.CreateForm(TForm2, Form2);
  Form2.ShowModal;
end;


procedure TForm1.PageControlChange(Sender: TObject);            //Modus wechseln
begin
  FTab := PageControl.ActivePage;
  mmExport.Enabled := true;
  mmExport.Visible := true;

  if (FTab = tsContext) then                       //Reiter Kontextmen� aktiv?
     begin
     mmAdd.Visible := false;                       //Eintr�ge unsichtbar
     mmImport.Visible := false;
     mmExportList.Visible := false;
     mmDate.Visible := false;
     mmRunOnce.Visible := false;
     N2.Visible := false;
     pmInfos.Visible := false;

     if (Context.Count = 0) then
        GetContext;
     end  //of begin
  else
     begin                                         //Reiter Autostart
     mmAdd.Visible := true;                        //Eintr�ge sichtbar
     mmImport.Visible := true;
     mmExportList.Visible := true;
     mmDate.Visible := true;
     mmRunOnce.Visible := true;
     N2.Visible := true;
     pmInfos.Visible := true;
     end;  //of if
end;


procedure TForm1.cbExpertClick(Sender: TObject);
begin
  GetContext;
end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);                                                  //Hotkey
begin
  if (Key = VK_DELETE) then                   //"l�schen"-Hotkey
     if ((FTab = tsStartup) and bDelete.Enabled) then
        bDelete.Click
     else
        if ((FTab = tsContext) and bDeleteContext.Enabled) then
           bDeleteContext.Click;
end;


{ Hyperlink zur Website }
procedure TForm1.lCopy1Click(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(URL_BASE), nil, nil, SW_ShowNormal);
end;


procedure TForm1.lCopy1MouseEnter(Sender: TObject);
begin
  with (Sender as TLabel) do
    begin
    Font.Style := Font.Style + [fsUnderline];
    Font.Color := clBlue;
    Cursor := crHandPoint;
    end;  //of with
end;


procedure TForm1.lCopy1MouseLeave(Sender: TObject);
begin
  with (Sender as TLabel) do
    begin
    Font.Style := Font.Style - [fsUnderline];
    Font.Color := clBlack;
    Cursor := crDefault;
    end;  //of with
end;


procedure TForm1.bCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Form3.Close;
end;

end.