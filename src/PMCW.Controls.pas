{ *********************************************************************** }
{                                                                         }
{ PM Code Works Controls Unit v1.0                                        }
{                                                                         }
{ Copyright (c) 2011-2018 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Controls;

interface

uses
  Winapi.Windows, Winapi.CommCtrl, Vcl.StdCtrls;

type
  /// <summary>
  ///   Possible icons in the balloon tip.
  /// </summary>
  TBalloonIcon = (

    /// <summary>
    ///  Show no icon.
    /// </summary>
    biNone = TTI_NONE,

    /// <summary>
    ///   Show info icon.
    /// </summary>
    biInfo = TTI_INFO,

    /// <summary>
    ///   Show warning icon.
    /// </summary>
    biWarning = TTI_WARNING,

    /// <summary>
    ///   Show error icon.
    /// </summary>
    biError = TTI_ERROR,

    /// <summary>
    ///   Show large info icon.
    /// </summary>
    biInfoLarge = TTI_INFO_LARGE,

    /// <summary>
    ///   Show large warning icon.
    /// </summary>
    biWarningLarge = TTI_WARNING_LARGE,

    /// <summary>
    ///   Show large error icon.
    /// </summary>
    biErrorLarge = TTI_ERROR_LARGE
  );

  TCustomEditHelper = class helper for TCustomEdit
    /// <summary>
    ///   Shows a balloon tip.
    /// </summary>
    /// <param name="ATitle">
    ///   The title to display.
    /// </param>
    /// <param name="AText">
    ///   The text to display.
    /// </param>
    /// <param name="AIcon">
    ///   Optional: A <see cref="TBalloonIcon"/> icon to use.
    /// </param>
    /// <returns>
    ///   <c>True</c> if balloon tip was shown successful or <c>False</c>
    ///   otherwise.
    /// </returns>
    function ShowBalloonTip(const ATitle, AText: string;
      AIcon: TBalloonIcon = biInfo): Boolean;
  end;

implementation

{ TCustomEditHelper }

function TCustomEditHelper.ShowBalloonTip(const ATitle, AText: string;
  AIcon: TBalloonIcon = biInfo): Boolean;
var
  BalloonTip: TEditBalloonTip;

begin
  ZeroMemory(@BalloonTip, SizeOf(TEditBalloonTip));

  with BalloonTip do
  begin
    cbStruct := SizeOf(TEditBalloonTip);
    pszTitle := PChar(ATitle);
    pszText := PChar(AText);
    ttiIcon := Ord(AIcon);
  end;  //of with

  Result := Edit_ShowBalloonTip(Handle, BalloonTip);
end;

end.
