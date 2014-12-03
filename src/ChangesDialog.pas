{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Updater v2.2                              }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ChangesDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  { TChangesDialog }
  TChangesDialog = class(TForm)
    mChanges: TMemo;
    lText: TLabel;
    bYes: TButton;
    bNo: TButton;
    lQuestion: TLabel;
  private
    FTitle, FText, FQuestion, FChanges: string;
  public
    function Execute(): Boolean;
    property Title: string read FTitle write FTitle;
    property Text: string read FText write FText;
    property Question: string read FQuestion write FQuestion;
    property Changes: string read FChanges write FChanges;
  end;

var
  Changes: TChangesDialog;

implementation

{$R *.dfm}

{ public TChangesDialog.Execute

  Shows a dialog with a changelog. }

function TChangesDialog.Execute(): Boolean;
var
  i: Integer;
  Line: string;

begin
  Caption := FTitle;
  lText.Caption := FText;

  for i := 1 to Length(FChanges) do
    if (FChanges[i] = #10) then
    begin
      mChanges.Lines.Append(Line);
      Line := '';
    end  //of begin
    else
      Line := Line + FChanges[i];

  lQuestion.Caption := FQuestion;
  result := (ShowModal = mrYes);
end;

end.
