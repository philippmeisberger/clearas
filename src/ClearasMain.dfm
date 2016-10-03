object Main: TMain
  Left = 371
  Top = 165
  Caption = 'Clearas'
  ClientHeight = 313
  ClientWidth = 543
  Color = clBtnFace
  Constraints.MinHeight = 372
  Constraints.MinWidth = 559
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
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 543
    Height = 313
    ActivePage = tsStartup
    Align = alClient
    TabOrder = 0
    OnChange = PageControlChange
    object tsStartup: TTabSheet
      Caption = 'Startup'
      DesignSize = (
        535
        284)
      object lStartup: TLabel
        Left = 21
        Top = 32
        Width = 235
        Height = 14
        Caption = 'The following programs are run at every startup.'
      end
      object lCopy1: TLabel
        Left = 223
        Top = 261
        Width = 88
        Height = 14
        Hint = 'Go to website'
        Anchors = [akBottom]
        Caption = 'PM Code Works'
        Font.Charset = DEFAULT_CHARSET
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
      object lVersion1: TLabel
        Left = 510
        Top = 4
        Width = 21
        Height = 14
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'v4.4'
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
            Caption = 'Enabled'
            Width = 53
          end
          item
            Caption = 'Program (0/0)'
            Width = 125
          end
          item
            Caption = 'File'
            Width = 122
          end
          item
            Caption = 'Key'
            Width = 75
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        ShowWorkAreas = True
        SmallImages = IconList
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = ListViewColumnClick
        OnCompare = ListViewCompare
        OnCustomDrawItem = ListViewCustomDrawItem
        OnDblClick = lwStartupDblClick
        OnKeyPress = ListViewKeyPress
        OnSelectItem = lwStartupSelectItem
      end
      object bCloseStartup: TButton
        Left = 428
        Top = 216
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Quit'
        TabOrder = 5
        OnClick = bCloseStartupClick
      end
      object bDisableStartupItem: TButton
        Left = 428
        Top = 96
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Disable'
        Enabled = False
        TabOrder = 2
        OnClick = bDisableItemClick
      end
      object bDeleteStartupItem: TButton
        Left = 428
        Top = 176
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Delete'
        Enabled = False
        TabOrder = 4
        OnClick = bDeleteItemClick
      end
      object bExportStartupItem: TButton
        Left = 428
        Top = 136
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Export'
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
        Caption = 'Enable'
        Enabled = False
        TabOrder = 1
        OnClick = bEnableItemClick
      end
      object cbRunOnce: TCheckBox
        Left = 414
        Top = 258
        Width = 105
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'RunOnce items'
        TabOrder = 6
        OnClick = mmRefreshClick
      end
    end
    object tsContext: TTabSheet
      Caption = 'Context menu'
      ImageIndex = 1
      DesignSize = (
        535
        284)
      object lContext: TLabel
        Left = 21
        Top = 32
        Width = 287
        Height = 14
        Caption = 'The following items are included in different context menus.'
      end
      object lVersion2: TLabel
        Left = 510
        Top = 4
        Width = 21
        Height = 14
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'v4.4'
      end
      object lWindows2: TLabel
        Left = 21
        Top = 4
        Width = 46
        Height = 14
        Caption = 'Windows'
      end
      object lCopy2: TLabel
        Left = 223
        Top = 261
        Width = 88
        Height = 14
        Hint = 'Go to website'
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 8
        Anchors = [akBottom]
        Caption = 'PM Code Works'
        Font.Charset = DEFAULT_CHARSET
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
      object bExportContextItem: TButton
        Left = 428
        Top = 136
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Export'
        Enabled = False
        TabOrder = 3
        OnClick = bExportItemClick
      end
      object bDeleteContextItem: TButton
        Left = 428
        Top = 176
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Delete'
        Enabled = False
        TabOrder = 4
        OnClick = bDeleteItemClick
      end
      object bCloseContext: TButton
        Left = 428
        Top = 216
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Quit'
        TabOrder = 5
        OnClick = bCloseStartupClick
      end
      object bDisableContextItem: TButton
        Left = 428
        Top = 96
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Disable'
        Enabled = False
        TabOrder = 2
        OnClick = bDisableItemClick
      end
      object bEnableContextItem: TButton
        Left = 428
        Top = 56
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Enable'
        Enabled = False
        TabOrder = 1
        OnClick = bEnableItemClick
      end
      object lwContext: TListView
        Left = 21
        Top = 56
        Width = 396
        Height = 193
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Enabled'
            Width = 53
          end
          item
            Caption = 'Item (0/0)'
            Width = 150
          end
          item
            Caption = 'Location'
            Width = 107
          end
          item
            Caption = 'Key'
            Width = 65
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = ListViewColumnClick
        OnCompare = ListViewCompare
        OnCustomDrawItem = ListViewCustomDrawItem
        OnDblClick = lwContextDblClick
        OnKeyPress = ListViewKeyPress
        OnSelectItem = lwContextSelectItem
      end
      object pbContextProgress: TProgressBar
        Left = 21
        Top = 255
        Width = 148
        Height = 18
        Anchors = [akLeft, akBottom]
        Style = pbstMarquee
        TabOrder = 7
        Visible = False
      end
      object cbContextExpert: TCheckBox
        Left = 414
        Top = 258
        Width = 105
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Expert mode'
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
        TextHint = 'Search ...'
        OnChange = eSearchChange
        OnRightButtonClick = eSearchRightButtonClick
      end
    end
    object tsService: TTabSheet
      Caption = 'Services'
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
        Width = 235
        Height = 14
        Caption = 'The following programs are run at every startup.'
      end
      object lVersion3: TLabel
        Left = 510
        Top = 4
        Width = 21
        Height = 14
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'v4.4'
      end
      object lCopy3: TLabel
        Left = 223
        Top = 261
        Width = 88
        Height = 14
        Hint = 'Go to website'
        Anchors = [akBottom]
        Caption = 'PM Code Works'
        Font.Charset = DEFAULT_CHARSET
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
      object pbServiceProgress: TProgressBar
        Left = 21
        Top = 255
        Width = 148
        Height = 18
        Anchors = [akLeft, akBottom]
        Style = pbstMarquee
        TabOrder = 8
        Visible = False
      end
      object lwService: TListView
        Left = 21
        Top = 56
        Width = 396
        Height = 193
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Enabled'
            Width = 53
          end
          item
            Caption = 'Service (0/0)'
            Width = 125
          end
          item
            Caption = 'File'
            Width = 122
          end
          item
            Caption = 'Type'
            Width = 75
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = ListViewColumnClick
        OnCompare = ListViewCompare
        OnCustomDrawItem = ListViewCustomDrawItem
        OnDblClick = lwServiceDblClick
        OnKeyPress = ListViewKeyPress
        OnSelectItem = lwServiceSelectItem
      end
      object bExportServiceItem: TButton
        Left = 428
        Top = 136
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Export'
        Enabled = False
        TabOrder = 3
        OnClick = bExportItemClick
      end
      object bDeleteServiceItem: TButton
        Left = 428
        Top = 176
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Delete'
        Enabled = False
        TabOrder = 4
        OnClick = bDeleteItemClick
      end
      object bCloseService: TButton
        Left = 428
        Top = 216
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Quit'
        TabOrder = 5
        OnClick = bCloseStartupClick
      end
      object bDisableServiceItem: TButton
        Left = 428
        Top = 96
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Disable'
        Enabled = False
        TabOrder = 2
        OnClick = bDisableItemClick
      end
      object bEnableServiceItem: TButton
        Left = 428
        Top = 56
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Enable'
        Enabled = False
        TabOrder = 1
        OnClick = bEnableItemClick
      end
      object cbServiceExpert: TCheckBox
        Left = 414
        Top = 258
        Width = 105
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Expert mode'
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
        TextHint = 'Search ...'
        OnChange = eSearchChange
        OnRightButtonClick = eSearchRightButtonClick
      end
    end
    object tsTasks: TTabSheet
      Caption = 'Tasks'
      ImageIndex = 3
      DesignSize = (
        535
        284)
      object lTasks: TLabel
        Left = 21
        Top = 32
        Width = 232
        Height = 14
        Caption = 'The following tasks are executed as scheduled.'
      end
      object lVersion4: TLabel
        Left = 510
        Top = 4
        Width = 21
        Height = 14
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'v4.4'
      end
      object lWindows4: TLabel
        Left = 21
        Top = 4
        Width = 46
        Height = 14
        Caption = 'Windows'
      end
      object lCopy4: TLabel
        Left = 223
        Top = 261
        Width = 88
        Height = 14
        Hint = 'Go to website'
        Anchors = [akBottom]
        Caption = 'PM Code Works'
        Font.Charset = DEFAULT_CHARSET
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
      object pbTaskProgress: TProgressBar
        Left = 21
        Top = 255
        Width = 148
        Height = 18
        Anchors = [akLeft, akBottom]
        Style = pbstMarquee
        TabOrder = 8
        Visible = False
      end
      object bCloseTasks: TButton
        Left = 428
        Top = 216
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Quit'
        TabOrder = 5
        OnClick = bCloseStartupClick
      end
      object bDeleteTaskItem: TButton
        Left = 428
        Top = 176
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Delete'
        Enabled = False
        TabOrder = 4
        OnClick = bDeleteItemClick
      end
      object bDisableTaskitem: TButton
        Left = 428
        Top = 96
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Disable'
        Enabled = False
        TabOrder = 2
        OnClick = bDisableItemClick
      end
      object bEnableTaskItem: TButton
        Left = 428
        Top = 56
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Enable'
        Enabled = False
        TabOrder = 1
        OnClick = bEnableItemClick
      end
      object bExportTaskItem: TButton
        Left = 428
        Top = 136
        Width = 91
        Height = 33
        Anchors = [akTop, akRight]
        Caption = 'Export'
        Enabled = False
        TabOrder = 3
        OnClick = bExportItemClick
      end
      object cbTaskExpert: TCheckBox
        Left = 414
        Top = 258
        Width = 105
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Expert mode'
        TabOrder = 6
        OnClick = mmRefreshClick
      end
      object eTaskSearch: TButtonedEdit
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
        TextHint = 'Search ...'
        OnChange = eSearchChange
        OnRightButtonClick = eSearchRightButtonClick
      end
      object lwTasks: TListView
        Left = 21
        Top = 56
        Width = 396
        Height = 193
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Enabled'
            Width = 53
          end
          item
            Caption = 'Task (0/0)'
            Width = 125
          end
          item
            Caption = 'File'
            Width = 122
          end
          item
            Caption = 'Location'
            Width = 75
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = ListViewColumnClick
        OnCompare = ListViewCompare
        OnDblClick = lwTasksDblClick
        OnKeyPress = ListViewKeyPress
        OnSelectItem = lwTasksSelectItem
      end
    end
  end
  object PopupMenu: TPopupMenu
    AutoPopup = False
    Left = 336
    object pmChangeStatus: TMenuItem
      Caption = 'deaktivieren'
      Default = True
      OnClick = pmChangeStatusClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmCopyLocation: TMenuItem
      Caption = 'Copy location'
      ShortCut = 16451
      OnClick = pmCopyLocationClick
    end
    object pmEdit: TMenuItem
      Caption = 'Pfad bearbeiten'
      OnClick = pmEditClick
    end
    object pmExport: TMenuItem
      Caption = 'Export'
      OnClick = mmExportClick
    end
    object pmDelete: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = pmDeleteClick
    end
    object pmRename: TMenuItem
      Caption = 'Rename'
      ShortCut = 113
      OnClick = pmRenameClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object pmChangeIcon: TMenuItem
      Caption = 'Change icon'
      Visible = False
      OnClick = pmChangeIconClick
    end
    object pmDeleteIcon: TMenuItem
      Caption = 'Delete icon'
      Visible = False
      OnClick = pmDeleteIconClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmOpenRegedit: TMenuItem
      Caption = 'Open in RegEdit'
      OnClick = pmOpenRegeditClick
    end
    object pmOpenExplorer: TMenuItem
      Caption = 'Open in Explorer'
      OnClick = pmOpenExplorerClick
    end
  end
  object MainMenu: TMainMenu
    Left = 440
    object mmFile: TMenuItem
      Caption = 'File'
      object mmAdd: TMenuItem
        Caption = 'Add program'
        OnClick = mmAddClick
      end
      object mmImport: TMenuItem
        Caption = 'Import backup'
        OnClick = mmImportClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mmExport: TMenuItem
        Caption = 'Export item'
        OnClick = mmExportClick
      end
      object mmExportList: TMenuItem
        Caption = 'Export items'
        OnClick = mmExportListClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mmClose: TMenuItem
        Caption = 'Quit'
        OnClick = bCloseStartupClick
      end
    end
    object mmEdit: TMenuItem
      Caption = 'Edit'
      object mmContext: TMenuItem
        Caption = 'Recycle bin context menu item'
        OnClick = mmContextClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object mmDeleteEraseable: TMenuItem
        Caption = 'Delete invalid items'
        OnClick = mmDeleteEraseableClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mmDelBackup: TMenuItem
        AutoCheck = True
        Caption = 'Delete backup after enabling'
        Checked = True
        OnClick = mmDelBackupClick
      end
    end
    object mmView: TMenuItem
      Caption = 'View'
      object mmRefresh: TMenuItem
        Caption = 'Refresh'
        ShortCut = 116
        OnClick = mmRefreshClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object mmDefault: TMenuItem
        Caption = 'Default column width'
        ShortCut = 117
        OnClick = mmDefaultClick
      end
      object mmShowCaptions: TMenuItem
        AutoCheck = True
        Caption = 'Show description'
        Checked = True
        OnClick = mmShowCaptionsClick
      end
      object mmDate: TMenuItem
        AutoCheck = True
        Caption = 'Date of deactivation'
        OnClick = mmDateClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mmLang: TMenuItem
        Caption = 'Choose language'
      end
    end
    object mmHelp: TMenuItem
      Caption = 'Help'
      object mmUpdate: TMenuItem
        Caption = 'Search for Update'
        OnClick = mmUpdateClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object mmInstallCertificate: TMenuItem
        Caption = 'Install certificate'
        OnClick = mmInstallCertificateClick
      end
      object mmReport: TMenuItem
        Caption = 'Report a bug'
        OnClick = mmReportClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object mmAbout: TMenuItem
        Caption = 'About Clearas'
        OnClick = mmAboutClick
      end
    end
  end
  object IconList: TImageList
    BkColor = clWhite
    Left = 392
  end
  object QuickSearchIconList: TImageList
    Left = 256
    Bitmap = {
      494C010103000800600210001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C2C2C2008B8B8B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C5C5C5008D8D8D00EAEAEA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D3D3D30081818100EDEDED00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFF0
      DE00FFF1DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFF3
      DE00FFF3DE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FDFDFD00FDFDFD00FEFEFE00000000000000000000000000DEDE
      DE008A8A8A00F0F0F0000000000000000000000000000000000000000000F7F7
      FE00EFEFFD00000000000000000000000000000000000000000000000000E0E0
      FA00E4E4FB00000000000000000000000000FFEBDE00FFEBDE00FFEEDE00E8D6
      DE00E4D2DE00FFF5DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFF6DE00C8BA
      DE00C2B3DE00FFF0DE00FFECDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DCDC
      DC0097979700919191009393930091919100ABABAB00EDEDED00CCCCCC008888
      8800F9F9F9000000000000000000000000000000000000000000000000007A7A
      EA002424DC00C7C7F60000000000000000000000000000000000BBBBF4001919
      DA007B7BEB00000000000000000000000000FFEBDE00FFECDE00FFF3DE00645E
      DE002C2DDE00CCBFDE00FFF8DE00FFECDE00FFECDE00FFF9DE00C1B4DE001D1E
      DE006761DE00FFF4DE00FFECDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D5D5D5008C8C
      8C00D5D5D500F6F6F600FBFBFB00EEEEEE00BEBEBE008181810086868600F1F1
      F10000000000000000000000000000000000000000000000000000000000D1D1
      F8003333DE003030DE00BBBBF4000000000000000000BBBBF4002D2DDD005252
      E300EFEFFC00000000000000000000000000FFEBDE00FFECDE00FFF1DE00C7BB
      DE003130DE002727DE00BCB0DE00FFF9DE00FFF9DE00BDB1DE002021DE00534F
      DE00ECDEDE00FFEFDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE0091919100F8F8
      F8000000000000000000000000000000000000000000DBDBDB0085858500F7F7
      F700000000000000000000000000000000000000000000000000000000000000
      0000D6D6F8004545E1002D2DDD00B0B0F200BDBDF4002D2DDD005A5AE400F0F0
      FD0000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFF7
      DE00D9CCDE003F3DDE001E1FDD00B2A6DE00BEB2DE001F20DE005652DE00F1E4
      DE00FFF3DE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE008C8C8C00E4E4E4000000
      0000000000000000000000000000000000000000000000000000AFAFAF00C4C4
      C400000000000000000000000000000000000000000000000000000000000000
      000000000000E5E5FB005B5BE4002D2DDD002D2DDD006767E600F3F3FD000000
      000000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFF6DE00E6D8DE005753DE002626DE002627DE00645FDE00F5E7DE00FFF3
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7F7F70091919100000000000000
      0000000000000000000000000000000000000000000000000000DEDEDE00A7A7
      A700000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A7A7F1002D2DDD002D2DDD008383EB00000000000000
      000000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFECDE00FFFFDE00A99DDE001B1DDD00191BDD00857CDE00FFF6DE00FFF0
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F3F3F3009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000E9E9E900A4A4
      A400000000000000000000000000000000000000000000000000000000000000
      000000000000AEAEF2002D2DDD006B6BE7007575E9002D2DDD007575E900F8F8
      FE0000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFEC
      DE00FFF9DE00B0A4DE001E1FDD006A63DE00766FDE001416DD00746CDE00FAEC
      DE00FFF2DE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7F7F70090909000000000000000
      0000000000000000000000000000000000000000000000000000DCDCDC00A6A6
      A600000000000000000000000000000000000000000000000000000000000000
      0000A7A7F1002D2DDD006767E600F9F9FE00000000008383EB002D2DDD005F5F
      E500F0F0FD00000000000000000000000000FFEBDE00FFEBDE00FFECDE00FFF3
      DE00ACA1DE00191BDD00645FDE00FAECDE00FFF4DE00837BDE001215DD00625D
      DE00EEE0DE00FFEFDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE008E8E8E00DFDFDF000000
      0000000000000000000000000000000000000000000000000000A7A7A700D3D3
      D30000000000000000000000000000000000000000000000000000000000CFCF
      F8001919DA006262E600F7F7FE000000000000000000000000008F8FED001919
      DA007C7CEB00000000000000000000000000FFEBDE00FFEBDE00FFF4DE00C8BA
      DE001316DD006862DE00F9EBDE00FFF1DE00FFEEDE00FFF5DE00968DDE001013
      DD006962DE00FFF4DE00FFECDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D4D4D4008F8F8F00F4F4
      F4000000000000000000000000000000000000000000D2D2D20085858500FAFA
      FA0000000000000000000000000000000000000000000000000000000000F3F3
      FD00C5C5F600FBFBFE000000000000000000000000000000000000000000CDCD
      F700DFDFFA00000000000000000000000000FFEBDE00FFEBDE00FFEFDE00E6D5
      DE009C92DE00FAE6DE00FFF1DE00FFEBDE00FFEBDE00FFEDDE00FFF5DE00AEA2
      DE00BCAEDE00FFF1DE00FFECDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D9D9D9008686
      8600CDCDCD00F0F0F000F6F6F600E7E7E700B3B3B30090909000F1F1F1000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFEE
      DE00FFF7DE00FFEEDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFECDE00FFF5
      DE00FFF3DE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEDE
      DE00A7A7A7009A9A9A009A9A9A009B9B9B00B8B8B800F3F3F300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEFEFE00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEBDE00FFEB
      DE00FFEBDE00FFEBDE00FFEBDE00FFEBDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFCFFFF00000000FFF8FFFF00000000
      FFF1FFFF00000000F8E3E7E700000000E007E3C700000000C00FE18700000000
      8F8FF00F000000001FCFF81F000000003FCFFC3F000000003FCFF80F00000000
      3FCFF087000000001FCFE1C7000000008F8FE3E700000000C01FFFFF00000000
      E03FFFFF00000000F9FFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
