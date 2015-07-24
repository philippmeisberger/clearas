object Main: TMain
  Left = 371
  Top = 165
  Caption = 'Clearas'
  ClientHeight = 307
  ClientWidth = 541
  Color = clBtnFace
  Constraints.MinHeight = 365
  Constraints.MinWidth = 557
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
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
        TabOrder = 8
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
      object eContextSearch: TEdit
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
        object mmGer: TMenuItem
          AutoCheck = True
          Caption = 'Deutsch (German)'
          RadioItem = True
          OnClick = mmGerClick
        end
        object mmEng: TMenuItem
          AutoCheck = True
          Caption = 'Englisch (English)'
          RadioItem = True
          OnClick = mmEngClick
        end
        object mmFre: TMenuItem
          AutoCheck = True
          Caption = 'Franz'#246'sisch (Fran'#231'ais)'
          RadioItem = True
          OnClick = mmFreClick
        end
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
end
