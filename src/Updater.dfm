object Update: TUpdate
  Left = 525
  Top = 308
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Update'
  ClientHeight = 110
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 14
  object lSize: TLabel
    Left = 280
    Top = 16
    Width = 53
    Height = 14
    Caption = '000/000KB'
    Transparent = True
  end
  object pbProgress: TProgressBar
    Left = 24
    Top = 32
    Width = 313
    Height = 25
    TabOrder = 0
  end
  object bFinished: TButton
    Left = 259
    Top = 64
    Width = 78
    Height = 33
    Caption = 'Fertig!'
    TabOrder = 1
    OnClick = bFinishedClick
  end
end
