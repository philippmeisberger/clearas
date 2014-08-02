{ *********************************************************************** }
{                                                                         }
{ Clearas Info Pages                                                      }
{                                                                         }
{ Copyright (c) 2011-2013 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit ClearasInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, ComCtrls, ClearasAPI;

type
  TForm2 = class(TForm)
    PageControl: TPageControl;
    tsInfo: TTabSheet;
    tsHistory: TTabSheet;
    bOk: TButton;
    lCopy: TLabel;
    bOk2: TButton;
    lCopy2: TLabel;
    Image: TImage;
    lVersion: TLabel;
    lBuild: TLabel;
    reInfo: TRichEdit;
    reHistory: TRichEdit;
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses ClearasMain;

{$R *.dfm}

procedure TForm2.bOkClick(Sender: TObject);
begin
  Close;
end;


procedure TForm2.FormCreate(Sender: TObject);
begin
  lBuild.Caption := '(Build: '+ IntToStr(TClearas.GetBuildNumber) +')';
end;

end.
