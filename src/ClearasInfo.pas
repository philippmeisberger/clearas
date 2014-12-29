{ *********************************************************************** }
{                                                                         }
{ Clearas Info Pages                                                      }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit ClearasInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, ComCtrls, OSUtils;

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
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;


implementation

{$R *.dfm}

procedure TInfo.bOkClick(Sender: TObject);
begin
  Close;
end;


procedure TInfo.FormCreate(Sender: TObject);
begin
  lBuild.Caption := '(Build: '+ IntToStr(TOSUtils.GetBuildNumber()) +')';
end;

end.
