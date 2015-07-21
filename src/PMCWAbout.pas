{ *********************************************************************** }
{                                                                         }
{ PM Code Works About Form                                                }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCWAbout;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  ComCtrls, PMCWUpdater;

type
  { TInfo }
  TInfo = class(TForm)
    PageControl: TPageControl;
    tsDescription: TTabSheet;
    tsChangelog: TTabSheet;
    bOk: TButton;
    lCopy: TLabel;
    bOk2: TButton;
    lCopy2: TLabel;
    Image: TImage;
    lVersion: TLabel;
    lBuild: TLabel;
    mCopying: TRichEdit;
    mChangelog: TRichEdit;
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadResourceIntoMemo(AResourceName: string; AMemo: TRichEdit);
  end;

implementation

{$R *.dfm}
{$R changelog.res}
{$R description.res}

procedure TInfo.FormCreate(Sender: TObject);
var
  VersionInfo: TFileProductVersion;

begin
  Caption := 'Über '+ Application.Title;

  // Show version information
  if TUpdateCheck.GetFileVersion(Application.ExeName, VersionInfo) then
  begin
    if (VersionInfo[VERSION_SERVICE] <> 0) then
      lVersion.Caption := Format('v%d.%d.%d', [VersionInfo[VERSION_MAJOR],
        VersionInfo[VERSION_MINOR], VersionInfo[VERSION_SERVICE]])
    else
      lVersion.Caption := Format('v%d.%d', [VersionInfo[VERSION_MAJOR],
        VersionInfo[VERSION_MINOR]]);

    lVersion.Left := Image.Left + ((Image.Width - lVersion.Width) div 2);
    lBuild.Caption := '(Build: '+ IntToStr(VersionInfo[VERSION_BUILD]) +')';
    lBuild.Left := ((mCopying.Left + lBuild.Left) - lBuild.Width) div 4;
  end;  //of begin

  // Load application description (including copyright) and changelog
  LoadResourceIntoMemo('description', mCopying);
  LoadResourceIntoMemo('changelog', mChangelog);

  // Load application icon into TImage
  Image.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
    Image.Width, Image.Height, LR_DEFAULTCOLOR);
end;

{ private TInfo.LoadResourceIntoMemo

  Loads a resource text into a specified TRichEdit. }

procedure TInfo.LoadResourceIntoMemo(AResourceName: string; AMemo: TRichEdit);
var
  ResourceStream: TResourceStream;

begin
  ResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);

  try
    AMemo.Lines.LoadFromStream(ResourceStream);

  finally
    ResourceStream.Free;
  end;  //of try
end;

end.
