{ *********************************************************************** }
{                                                                         }
{ Clearas Info Pages                                                      }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit ClearasInfo;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls,
  PMCWUpdater;

type
  TInfo = class(TForm)
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
    procedure FormCreate(Sender: TObject);
  end;


implementation

{$R *.dfm}

procedure TInfo.FormCreate(Sender: TObject);
begin
  lBuild.Caption := '(Build: '+ IntToStr(TUpdateCheck.GetBuildNumber()) +')';
end;

end.
