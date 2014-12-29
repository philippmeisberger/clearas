unit Contextmenu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LanguageFile;

type
  TAddContextmenuDialog = class(TForm, IChangeLanguageListener)
    eName: TLabeledEdit;
    eCommand: TLabeledEdit;
    cbxLocation: TComboBox;
    bOk: TButton;
    bCancel: TButton;
    lLocation: TLabel;
    eText: TLabeledEdit;
    bOpen: TButton;
    procedure bOkClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
  private
    FLang: TLanguageFile;
    FName, FText, FLocation, FCommand: string;
  public
    constructor Create(AOwner: TComponent; ALangFile: TLanguageFile); reintroduce;
    function Execute(): Boolean;
    procedure SetLanguage(Sender: TObject);
    { external }
    property Command: string read FCommand;
    property Location: string read FLocation;
    property Name: string read FName;
    property Text: string read FText;
  end;


implementation

{$R *.dfm}

{ TAddContextmenuDialog }

{ public TAddContextmenuDialog.Create

  Constructor for creating a TAddContextmenuDialog instance. }

constructor TAddContextmenuDialog.Create(AOwner: TComponent; ALangFile: TLanguageFile);
begin
  inherited Create(AOwner);
  FLang := ALangFile;
  SetLanguage(Self);
end;

{ public TAddContextmenuDialog.Execute

  Executes this dialog in a modal way and returns true if user clicked "OK". }

function TAddContextmenuDialog.Execute(): Boolean;
begin
  result := (ShowModal = mrOk);
end;

{ private TAddContextmenuDialog.SetLanguage

  Updates all component captions with new language text. }

procedure TAddContextmenuDialog.SetLanguage(Sender: TObject);
begin
  {with FLang do
  begin
    eName.EditLabel.Caption := GetString(100);
    eCommand.EditLabel.Caption := GetString(101);
    lLocation.Caption := GetString(90);
    bCancel.Caption := GetString(6);
  end;  //of with }
end;

{ TAddContextmenuDialog.bOkClick

  VCL event that is called when user clicked "OK". }

procedure TAddContextmenuDialog.bOkClick(Sender: TObject);
begin
  try
    if (Trim(eName.Text) = '') then
      raise EAbort.Create('Missing name!');

    if (Trim(eText.Text) = '') then
      raise EAbort.Create('Missing text!');

    if (Trim(eCommand.Text) = '') then
      raise EAbort.Create('Missing command!');

    // Save information
    FName := eName.Text;
    FText := eText.Text;
    FLocation := cbxLocation.Text;
    FCommand := eCommand.Text;

    // Close form successfully
    ModalResult := mrOK;

  except
    on E: EAbort do
      FLang.MessageBox(E.Message, mtWarning);
  end;  //of try
end;


procedure TAddContextmenuDialog.bOpenClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;

begin
  OpenDialog := TOpenDialog.Create(Self);

  try
    // Set TOpenDialog options
    with OpenDialog do
    begin
      Title := FLang.GetString(69);
      InitialDir := '%ProgramFiles%';

      // Filter .exe files
      Filter := FLang.GetString(38);
    end;  //of with

    // User clicked "open"?
    if OpenDialog.Execute then
      eCommand.Text := '"'+ OpenDialog.FileName +'" "%1"';

  finally
    OpenDialog.Free;
  end;  //of try
end;

end.
