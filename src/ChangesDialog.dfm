object ChangesDialog: TChangesDialog
  Left = 192
  Top = 116
  BorderStyle = bsDialog
  Caption = 'Title'
  ClientHeight = 273
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lText: TLabel
    Left = 24
    Top = 16
    Width = 23
    Height = 13
    Caption = 'lText'
  end
  object lQuestion: TLabel
    Left = 24
    Top = 208
    Width = 44
    Height = 13
    Caption = 'lQuestion'
  end
  object mChanges: TMemo
    Left = 24
    Top = 40
    Width = 369
    Height = 153
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object bYes: TButton
    Left = 24
    Top = 232
    Width = 75
    Height = 25
    Caption = 'ja'
    Default = True
    ModalResult = 6
    TabOrder = 1
  end
  object bNo: TButton
    Left = 112
    Top = 232
    Width = 75
    Height = 25
    Caption = 'nein'
    ModalResult = 7
    TabOrder = 2
  end
end
