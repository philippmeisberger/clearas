object AddContextmenuDialog: TAddContextmenuDialog
  Left = 567
  Top = 267
  BorderStyle = bsDialog
  Caption = 'Add contextmenu shell item'
  ClientHeight = 158
  ClientWidth = 410
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
  object lLocation: TLabel
    Left = 264
    Top = 24
    Width = 44
    Height = 13
    Caption = 'Location:'
  end
  object eName: TLabeledEdit
    Left = 24
    Top = 40
    Width = 105
    Height = 21
    EditLabel.Width = 31
    EditLabel.Height = 13
    EditLabel.Caption = 'Name:'
    MaxLength = 80
    TabOrder = 0
  end
  object eCommand: TLabeledEdit
    Left = 24
    Top = 88
    Width = 321
    Height = 21
    EditLabel.Width = 50
    EditLabel.Height = 13
    EditLabel.Caption = 'Command:'
    TabOrder = 3
  end
  object cbxLocation: TComboBox
    Left = 264
    Top = 40
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = 'AllFileSystemObjects'
    Items.Strings = (
      'AllFileSystemObjects'
      'Directory'
      'Folder'
      'Drive')
  end
  object bOk: TButton
    Left = 312
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = bOkClick
  end
  object bCancel: TButton
    Left = 224
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object eText: TLabeledEdit
    Left = 136
    Top = 40
    Width = 121
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'Text:'
    MaxLength = 80
    TabOrder = 1
  end
  object bOpen: TButton
    Left = 352
    Top = 88
    Width = 33
    Height = 21
    Caption = '...'
    TabOrder = 4
    OnClick = bOpenClick
  end
end
