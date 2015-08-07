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
      object eServiceSearch: TEdit
        Left = 21
        Top = 255
        Width = 148
        Height = 22
        Anchors = [akLeft, akBottom]
        TabOrder = 7
        TextHint = 'Suchen ...'
        OnChange = mmShowIconsClick
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
      494C010102000800280010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000077777731777777AB0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000077777730777777DD777777360000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007777772F777777DD777777370000000000000000000000007777771F7777
      7787777777020000000000000000000000000000000000000000000000007777
      77057777778C7777771700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007777
      772E777777DD7777773800000000000000000000000000000000777777BF7777
      77FF7777779F7777770200000000000000000000000000000000777777057777
      77AD777777FF777777AF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007777770477777716777777060000000000000000000000007777772E7777
      77DD777777390000000000000000000000000000000000000000777777217777
      77E1777777FF7777779F77777702000000000000000077777705777777AD7777
      77FF777777D87777771900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000777777317777
      77B17B7B7BF7878787FF7C7C7CFA777777BA7777773D7777772D777777DD7777
      773A000000000000000000000000000000000000000000000000000000007777
      7721777777E1777777FF7777779F7777770277777705777777AD777777FF7777
      77D8777777190000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000077777758838383F9CDCD
      CDFFFBFBFBFFFFFFFFFFFCFCFCFFD3D3D3FF878787FD777777E67777773B0000
      0000000000000000000000000000000000000000000000000000000000000000
      000077777721777777E0777777FF777777A0777777AD777777FF777777D87777
      7719000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007777772A818181F8ECECECFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2F2F2FF878787FD7777773D0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000077777721777777E0777777FF777777FF777777D8777777190000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000777777A2C6C6C6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD3D3D3FF777777BA0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000077777705777777B3777777FF777777FF777777A5777777020000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000777777E8F4F4F4FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFCFF7C7C7CFA7777
      7706000000000000000000000000000000000000000000000000000000000000
      000077777705777777AD777777FF777777D8777777E0777777FF7777779F7777
      7702000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007A7A7AFDFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF878787FF7777
      7716000000000000000000000000000000000000000000000000000000007777
      7705777777AD777777FF777777D87777771977777721777777E0777777FF7777
      779F777777020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000777777E4F1F1F1FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFF7B7B7BF77777
      7704000000000000000000000000000000000000000000000000777777057777
      77AD777777FF777777D877777719000000000000000077777721777777E07777
      77FF7777779F7777770200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000077777799C0C0C0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCDCDCDFF777777B10000
      0000000000000000000000000000000000000000000000000000777777A87777
      77FF777777D87777771900000000000000000000000000000000777777217777
      77E0777777FF7777779800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000777777207E7E7EF2E4E4E4FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECECECFF838383F9777777310000
      0000000000000000000000000000000000000000000000000000777777527777
      77D3777777190000000000000000000000000000000000000000000000007777
      7721777777D87777774600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000777777457E7E7EF2C0C0
      C0FFF1F1F1FFFFFFFFFFF4F4F4FFC6C6C6FF818181F877777757000000000000
      0000000000000000000000000000000000000000000000000000000000007777
      7704000000000000000000000000000000000000000000000000000000000000
      0000777777040000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000777777207777
      7799777777E37A7A7AFD777777E8777777A27777772A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFCFFFF00000000FFF8FFFF00000000
      FFF1C7E300000000FFE3C3C300000000F1C7C18300000000C00FE00700000000
      801FF00F00000000001FF81F00000000001FF81F00000000000FF00F00000000
      000FE00700000000000FC18300000000001FC3C300000000001FC7E300000000
      803FEFF700000000C07FFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
