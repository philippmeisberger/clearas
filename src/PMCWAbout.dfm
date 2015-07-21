object Info: TInfo
  Left = 248
  Top = 197
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = #220'ber ...'
  ClientHeight = 257
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    464
    257)
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl: TPageControl
    Left = 0
    Top = -1
    Width = 468
    Height = 259
    ActivePage = tsDescription
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    MultiLine = True
    ParentFont = False
    TabOrder = 0
    object tsDescription: TTabSheet
      Caption = 'Infos'
      DesignSize = (
        460
        230)
      object lCopy: TLabel
        Left = 173
        Top = 208
        Width = 112
        Height = 14
        Anchors = [akBottom]
        Caption = #169' P.Meisberger 2015'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 169
      end
      object Image: TImage
        Left = 16
        Top = 16
        Width = 48
        Height = 48
      end
      object lVersion: TLabel
        Left = 29
        Top = 68
        Width = 21
        Height = 14
        Caption = 'v1.0'
      end
      object lBuild: TLabel
        Left = 14
        Top = 83
        Width = 37
        Height = 14
        Caption = '(Build: )'
      end
      object bOk: TButton
        Left = 368
        Top = 200
        Width = 75
        Height = 25
        Anchors = [akBottom]
        Cancel = True
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object mCopying: TRichEdit
        Left = 82
        Top = 16
        Width = 362
        Height = 177
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Copyright (C) 2011 Philipp Meisberger <team@pm-codeworks.de> '
          ''
          
            'This Program may be used by anyone in accordance with the terms ' +
            'of '
          'the German Free Software License.'
          ''
          'The License may be obtained under <http://www.d-fsl.org>.')
        ReadOnly = True
        TabOrder = 1
        Zoom = 100
      end
    end
    object tsChangelog: TTabSheet
      Caption = 'Changelog'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        460
        230)
      object lCopy2: TLabel
        Left = 173
        Top = 208
        Width = 112
        Height = 14
        Anchors = [akBottom]
        Caption = #169' P.Meisberger 2015'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 169
      end
      object bOk2: TButton
        Left = 368
        Top = 200
        Width = 75
        Height = 25
        Anchors = [akBottom]
        Cancel = True
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object mChangelog: TRichEdit
        Left = 15
        Top = 17
        Width = 428
        Height = 177
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Changelog')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
        Zoom = 100
      end
    end
  end
end
