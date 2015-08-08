object Main: TMain
  Left = 371
  Top = 165
  Caption = 'Clearas'
  ClientHeight = 307
  ClientWidth = 541
  Color = clBtnFace
  Constraints.MinHeight = 365
  Constraints.MinWidth = 557
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    541
    307)
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 543
    Height = 313
    ActivePage = tsStartup
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageControlChange
    object tsStartup: TTabSheet
      Caption = 'Autostart'
      DesignSize = (
        535
        284)
      object lStartup: TLabel
        Left = 21
        Top = 32
        Width = 367
        Height = 14
        Caption = 
          'Programme aus dieser Liste (mit einem "ja") werden bei jedem Sta' +
          'rt geladen.'
      end
      object lCopy1: TLabel
        Left = 211
        Top = 262
        Width = 112
        Height = 14
        Hint = 'Zur Website'
        Anchors = [akBottom]
        Caption = #169' P.Meisberger 2015'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lCopy1Click
        OnMouseEnter = lCopy1MouseEnter
        OnMouseLeave = lCopy1MouseLeave
      end
      object lVersion: TLabel
        Left = 510
        Top = 4
        Width = 21
        Height = 14
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'v4.1'
      end
      object lWindows: TLabel
        Left = 21
        Top = 4
        Width = 46
        Height = 14
        Caption = 'Windows'
      end
      object lwStartup: TListView
        Left = 21
        Top = 56
        Width = 396
        Height = 193
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Aktiviert'
            MaxWidth = 53
            MinWidth = 53
            Width = 53
          end
          item
            Caption = 'Programm (0/0)'
            Width = 125
          end
          item
            Caption = 'Datei'
            Width = 122
          end
          item
            Caption = 'Schl'#252'ssel'
            MaxWidth = 105
            Width = 75
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        ShowWorkAreas = True
        SmallImages = IconList
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lwStartupColumnClick
        OnCompare = lwStartupCompare
        OnDblClick = lwStartupDblClick
        OnKeyPress = lwStartupKeyPress
        OnSelectItem = lwStartupSelectItem
      end
      object bCloseStartup: TButton
        Left = 428
        Top = 216
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Beenden'
        TabOrder = 5
        OnClick = bCloseStartupClick
      end
      object bDisableStartupItem: TButton
        Left = 428
        Top = 96
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'deaktivieren'
        Enabled = False
        TabOrder = 2
        OnClick = bDisableStartupItemClick
      end
      object bDeleteStartupItem: TButton
        Left = 428
        Top = 176
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'l'#246'schen'
        Enabled = False
        TabOrder = 4
        OnClick = bDeleteStartupItemClick
      end
      object bExportStartupItem: TButton
        Left = 428
        Top = 136
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'exportieren'
        Enabled = False
        TabOrder = 3
        OnClick = bExportStartupItemClick
      end
      object bEnableStartupItem: TButton
        Left = 428
        Top = 56
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'aktivieren'
        Enabled = False
        TabOrder = 1
        OnClick = bEnableStartupItemClick
      end
      object cbRunOnce: TCheckBox
        Left = 414
        Top = 258
        Width = 105
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'RunOnce-Eintr'#228'ge'
        TabOrder = 6
        OnClick = mmRefreshClick
      end
    end
    object tsContext: TTabSheet
      Caption = 'Kontextmen'#252
      ImageIndex = 1
      DesignSize = (
        535
        284)
      object lCopy2: TLabel
        Left = 211
        Top = 262
        Width = 112
        Height = 14
        Hint = 'Zur Website'
        Anchors = [akBottom]
        Caption = #169' P.Meisberger 2015'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lCopy1Click
        OnMouseEnter = lCopy1MouseEnter
        OnMouseLeave = lCopy1MouseLeave
      end
      object lContext: TLabel
        Left = 21
        Top = 32
        Width = 310
        Height = 14
        Caption = 'Eintr'#228'ge dieser Liste sehen Sie in verschiedenen Kontextmen'#252's.'
      end
      object lVersion2: TLabel
        Left = 510
        Top = 4
        Width = 21
        Height = 14
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'v4.1'
      end
      object lWindows2: TLabel
        Left = 21
        Top = 4
        Width = 46
        Height = 14
        Caption = 'Windows'
      end
      object bExportContextItem: TButton
        Left = 428
        Top = 136
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'exportieren'
        Enabled = False
        TabOrder = 3
        OnClick = bExportContextItemClick
      end
      object bDeleteContextItem: TButton
        Left = 428
        Top = 176
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'l'#246'schen'
        Enabled = False
        TabOrder = 4
        OnClick = bDeleteContextItemClick
      end
      object bCloseContext: TButton
        Left = 428
        Top = 216
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Beenden'
        TabOrder = 5
        OnClick = bCloseStartupClick
      end
      object bDisableContextItem: TButton
        Left = 428
        Top = 96
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'deaktivieren'
        Enabled = False
        TabOrder = 2
        OnClick = bDisableContextItemClick
      end
      object bEnableContextItem: TButton
        Left = 428
        Top = 56
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'aktivieren'
        Enabled = False
        TabOrder = 1
        OnClick = bEnableContextItemClick
      end
      object lwContext: TListView
        Left = 21
        Top = 56
        Width = 396
        Height = 193
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Aktiviert'
            MaxWidth = 53
            MinWidth = 53
            Width = 53
          end
          item
            Caption = 'Eintrag (0/0)'
            Width = 150
          end
          item
            Caption = 'Ort'
            Width = 107
          end
          item
            Caption = 'Schl'#252'ssel'
            MaxWidth = 65
            MinWidth = 65
            Width = 65
          end>
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lwStartupColumnClick
        OnCompare = lwStartupCompare
        OnDblClick = lwContextDblClick
        OnKeyPress = lwStartupKeyPress
        OnSelectItem = lwContextSelectItem
      end
      object pbContextLoad: TProgressBar
        Left = 21
        Top = 255
        Width = 148
        Height = 18
        Anchors = [akLeft, akBottom]
        Smooth = True
        TabOrder = 7
        Visible = False
      end
      object cbContextExpert: TCheckBox
        Left = 414
        Top = 258
        Width = 105
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Expertenmodus'
        TabOrder = 6
        OnClick = mmRefreshClick
      end
      object eContextSearch: TButtonedEdit
        Left = 21
        Top = 255
        Width = 148
        Height = 22
        Anchors = [akLeft, akBottom]
        Images = QuickSearchIconList
        RightButton.DisabledImageIndex = 0
        RightButton.HotImageIndex = 0
        RightButton.ImageIndex = 0
        RightButton.PressedImageIndex = 0
        RightButton.Visible = True
        TabOrder = 8
        TextHint = 'Suchen ...'
        OnChange = eContextSearchChange
        OnRightButtonClick = eContextSearchRightButtonClick
      end
    end
    object tsService: TTabSheet
      Caption = 'Dienste'
      ImageIndex = 2
      DesignSize = (
        535
        284)
      object lWindows3: TLabel
        Left = 21
        Top = 4
        Width = 46
        Height = 14
        Caption = 'Windows'
      end
      object lService: TLabel
        Left = 21
        Top = 32
        Width = 309
        Height = 14
        Caption = 'Eintr'#228'ge dieser Liste werden beim Start von Windows gestartet.'
      end
      object lCopy3: TLabel
        Left = 211
        Top = 262
        Width = 112
        Height = 14
        Hint = 'Zur Website'
        Anchors = [akBottom]
        Caption = #169' P.Meisberger 2015'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lCopy1Click
        OnMouseEnter = lCopy1MouseEnter
        OnMouseLeave = lCopy1MouseLeave
      end
      object lVersion3: TLabel
        Left = 510
        Top = 4
        Width = 21
        Height = 14
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'v4.1'
      end
      object lwService: TListView
        Left = 21
        Top = 56
        Width = 396
        Height = 193
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Aktiviert'
            MaxWidth = 53
            MinWidth = 53
            Width = 53
          end
          item
            Caption = 'Dienst (0/0)'
            Width = 125
          end
          item
            Caption = 'Datei'
            Width = 122
          end
          item
            Caption = 'Typ'
            MaxWidth = 75
            MinWidth = 75
            Width = 75
          end>
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = lwStartupColumnClick
        OnCompare = lwStartupCompare
        OnDblClick = lwServiceDblClick
        OnKeyPress = lwStartupKeyPress
        OnSelectItem = lwServiceSelectItem
      end
      object bExportServiceItem: TButton
        Left = 428
        Top = 136
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'exportieren'
        Enabled = False
        TabOrder = 3
        OnClick = bExportServiceItemClick
      end
      object bDeleteServiceItem: TButton
        Left = 428
        Top = 176
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'l'#246'schen'
        Enabled = False
        TabOrder = 4
        OnClick = bDeleteServiceItemClick
      end
      object bCloseService: TButton
        Left = 428
        Top = 216
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Beenden'
        TabOrder = 5
        OnClick = bCloseStartupClick
      end
      object bDisableServiceItem: TButton
        Left = 428
        Top = 96
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'deaktivieren'
        Enabled = False
        TabOrder = 2
        OnClick = bDisableServiceItemClick
      end
      object bEnableServiceItem: TButton
        Left = 428
        Top = 56
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'aktivieren'
        Enabled = False
        TabOrder = 1
        OnClick = bEnableServiceItemClick
      end
      object cbServiceExpert: TCheckBox
        Left = 414
        Top = 258
        Width = 105
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Expertenmodus'
        TabOrder = 6
        OnClick = mmRefreshClick
      end
      object eServiceSearch: TButtonedEdit
        Left = 21
        Top = 255
        Width = 148
        Height = 22
        Anchors = [akLeft, akBottom]
        Images = QuickSearchIconList
        RightButton.DisabledImageIndex = 0
        RightButton.HotImageIndex = 0
        RightButton.ImageIndex = 0
        RightButton.PressedImageIndex = 0
        RightButton.Visible = True
        TabOrder = 7
        TextHint = 'Suchen ...'
        OnChange = eContextSearchChange
        OnRightButtonClick = eContextSearchRightButtonClick
      end
    end
  end
  object PopupMenu: TPopupMenu
    AutoPopup = False
    Left = 336
    object pmChangeStatus: TMenuItem
      Caption = 'deaktivieren'
      Default = True
      Enabled = False
      OnClick = pmChangeStatusClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmCopyLocation: TMenuItem
      Caption = 'Ort kopieren'
      ShortCut = 16451
      OnClick = pmCopyLocationClick
    end
    object pmEdit: TMenuItem
      Caption = 'Pfad bearbeiten'
      Enabled = False
      OnClick = pmEditClick
    end
    object pmExport: TMenuItem
      Caption = 'exportieren'
      Enabled = False
      OnClick = mmExportClick
    end
    object pmDelete: TMenuItem
      Caption = 'l'#246'schen'
      Enabled = False
      ShortCut = 46
      OnClick = pmDeleteClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmOpenRegedit: TMenuItem
      Caption = 'In RegEdit '#246'ffnen'
      OnClick = pmOpenRegeditClick
    end
    object pmOpenExplorer: TMenuItem
      Caption = 'In Explorer '#246'ffnen'
      OnClick = pmOpenExplorerClick
    end
  end
  object MainMenu: TMainMenu
    Left = 440
    object mmFile: TMenuItem
      Caption = 'Datei'
      object mmAdd: TMenuItem
        Caption = 'Programm hinzuf'#252'gen'
        OnClick = mmAddClick
      end
      object mmImport: TMenuItem
        Caption = 'Backup importieren'
        OnClick = mmImportClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mmExport: TMenuItem
        Caption = 'Eintrag exportieren'
        OnClick = mmExportClick
      end
      object mmExportList: TMenuItem
        Caption = 'Eintr'#228'ge exportieren'
        OnClick = mmExportListClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mmClose: TMenuItem
        Caption = 'Beenden'
        OnClick = bCloseStartupClick
      end
    end
    object mmEdit: TMenuItem
      Caption = 'Bearbeiten'
      object mmContext: TMenuItem
        Caption = 'Papierkorb Kontextmen'#252'-Eintrag'
        OnClick = mmContextClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mmDelBackup: TMenuItem
        AutoCheck = True
        Caption = 'Backup beim Aktivieren l'#246'schen'
        Checked = True
        OnClick = mmDelBackupClick
      end
    end
    object mmView: TMenuItem
      Caption = 'Ansicht'
      object mmRefresh: TMenuItem
        Caption = 'Aktualisieren'
        ShortCut = 116
        OnClick = mmRefreshClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object mmDefault: TMenuItem
        Caption = 'Standard Spaltengr'#246#223'e'
        ShortCut = 117
        OnClick = mmDefaultClick
      end
      object mmDate: TMenuItem
        AutoCheck = True
        Caption = 'Deaktivierungsdatum anzeigen'
        OnClick = mmDateClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mmLang: TMenuItem
        Caption = 'Sprache w'#228'hlen'
      end
    end
    object mmHelp: TMenuItem
      Caption = 'Hilfe'
      object mmUpdate: TMenuItem
        Caption = 'Nach Update suchen'
        OnClick = mmUpdateClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object mmInstallCertificate: TMenuItem
        Caption = 'Zertifikat installieren'
        OnClick = mmInstallCertificateClick
      end
      object mmReport: TMenuItem
        Caption = 'Fehler melden'
        OnClick = mmReportClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object mmInfo: TMenuItem
        Caption = #220'ber Clearas'
        OnClick = mmInfoClick
      end
    end
  end
  object IconList: TImageList
    Left = 392
  end
  object QuickSearchIconList: TImageList
    Left = 256
    Bitmap = {
      494C0101020008007C0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E2E2E235898989DD0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E3E3E334898989DDE4E4E4320000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E3E3E334898989DDE4E4E432000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E3E3
      E333898989DDE3E3E3330000000000000000000000000000000000000000F0F0
      FC12ECECFC16000000000000000000000000000000000000000000000000D0D0
      F739D4D4F8340000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F4F4
      F413C4C4C46EA9A9A9A1A7A7A7A6BDBDBD7BECECEC2200000000E4E4E4328989
      89DDE3E3E3340000000000000000000000000000000000000000000000006262
      E5BE4141E0E6D7D7F83000000000000000000000000000000000C3C3F5483434
      DEF66F6FE7AE0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D0D0D0578383
      83F0AEAEAEFFD0D0D0FFD3D3D3FFB7B7B7FF848484FAB1B1B191898989DDE2E2
      E23500000000000000000000000000000000000000000000000000000000C5C5
      F5463636DEF33939DFF0C9C9F6410000000000000000C0C0F44C3232DDF85757
      E3CCF0F0FC120000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6D6D64D888888FDEAEA
      EAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F5FF969696FFB1B1B1910000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D4D4F8343F3FDFE93232DDF8B8B8F356BDBDF4503131DDF95959E4C9F1F1
      FC11000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FCFCFC05878787E2E3E3E3FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FF838383F9EEEE
      EE1F000000000000000000000000000000000000000000000000000000000000
      000000000000E1E1FA244949E1DD2E2EDDFD3131DDFA5B5BE4C6F2F2FD0F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D9D9D9469A9A9AFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB4B4B4FFC0C0
      C075000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A6A6F06C2D2DDDFF2D2DDDFF8A8AEC8DFEFEFE010000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4C4C46EB5B5B5FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFCFFFABAB
      AB9E000000000000000000000000000000000000000000000000000000000000
      000000000000B2B2F25D2F2FDDFC6161E5BF6565E6BB2D2DDDFF7C7CE99FFBFB
      FE04000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C7C7C769B1B1B1FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCBCBCBFFADAD
      AD98000000000000000000000000000000000000000000000000000000000000
      0000AFAFF2612E2EDDFD6565E6BBF6F6FD0AFAFAFE067575E8A72D2DDDFF6969
      E6B5F5F5FD0B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E3E3E3348F8F8FFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA8A8A8FFCACA
      CA6300000000000000000000000000000000000000000000000000000000C7C7
      F5442E2EDDFD6868E6B7F7F7FD090000000000000000FDFDFE028989EB8F2D2D
      DDFF7373E8A90000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000949494C7CECECEFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5E5E5FF848484EAF8F8
      F80D00000000000000000000000000000000000000000000000000000000EBEB
      FB189797EE7EF9F9FE070000000000000000000000000000000000000000AAAA
      F167C6C6F5450000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E9E9E928858585EDCCCC
      CCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDDDDDDFF868686FAD7D7D74A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EAEAEA279696
      96C48C8C8CFFAEAEAEFFB0B0B0FF959595FF8A8A8ADADCDCDC41000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E6E6E62ECBCBCB61C9C9C966DFDFDF3BFDFDFD0200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFCFFFF00000000FFF8FFFF00000000
      FFF1FFFF00000000FFE3E7E700000000E047E3C700000000C00FE18700000000
      801FF00F00000000000FF81F00000000000FFC1F00000000000FF80F00000000
      000FF00700000000000FE18700000000800FE3E700000000801FFFFF00000000
      C03FFFFF00000000F07FFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
