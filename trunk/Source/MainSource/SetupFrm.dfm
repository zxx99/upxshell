object SetupForm: TSetupForm
  Left = 407
  Top = 234
  BorderStyle = bsDialog
  Caption = 'Advanced options'
  ClientHeight = 288
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 161
    Width = 357
    Height = 86
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object lblCommands: TLabel
      Left = 12
      Top = 8
      Width = 128
      Height = 13
      Caption = 'Additional UPX commands:'
    end
    object chkCommands: TCheckBox
      Left = 8
      Top = 54
      Width = 177
      Height = 17
      Hint = 'Allways use the entered commands'
      Caption = 'Save commands'
      TabOrder = 2
    end
    object txtCommands: TEdit
      Left = 6
      Top = 28
      Width = 319
      Height = 21
      TabOrder = 1
      OnChange = txtCommandsChange
    end
    object btnCommands: TButton
      Left = 271
      Top = 52
      Width = 75
      Height = 21
      Caption = 'Commands'
      TabOrder = 0
      OnClick = btnCommandsClick
    end
  end
  object btnOk: TButton
    Left = 254
    Top = 257
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 357
    Height = 48
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object chkScramble: TCheckBox
      Left = 7
      Top = 8
      Width = 178
      Height = 17
      Hint = 'Scramble files compressed with UPX'
      Caption = 'Use file Scrambler'
      TabOrder = 0
    end
    object btnScramble: TButton
      Left = 271
      Top = 6
      Width = 75
      Height = 21
      Hint = 'Scramble the selected file now'
      Caption = 'Scramble'
      TabOrder = 1
      OnClick = btnScrambleClick
    end
    object chkIntegrate: TCheckBox
      Left = 8
      Top = 24
      Width = 241
      Height = 17
      Hint = 'Add '#39'Compress with UPX'#39' option to Explorer left-click menu'
      Caption = 'Integrate into context menu'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chkIntegrateClick
    end
  end
  object pnlMiddle: TPanel
    Left = 0
    Top = 48
    Width = 357
    Height = 113
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object lblAdvacedOpts: TLabel
      Left = 8
      Top = 8
      Width = 114
      Height = 13
      Caption = 'Advanced UPX options:'
      Transparent = True
    end
    object lblPriority: TLabel
      Left = 144
      Top = 14
      Width = 70
      Height = 13
      Caption = 'Packer priority:'
      Transparent = True
    end
    object lblIcons: TLabel
      Left = 144
      Top = 54
      Width = 29
      Height = 13
      Caption = 'Icons:'
    end
    object chkForce: TCheckBox
      Left = 8
      Top = 78
      Width = 129
      Height = 17
      Hint = 'Force compression of suspicious files'
      Caption = 'Force compression'
      TabOrder = 3
    end
    object chkResources: TCheckBox
      Left = 8
      Top = 27
      Width = 129
      Height = 17
      Hint = 'Compress resources (better compression)'
      Caption = 'Compress resources'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkRelocs: TCheckBox
      Left = 8
      Top = 61
      Width = 129
      Height = 17
      Hint = 'Strip relocations'
      Caption = 'Strip relocations'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cmbPriority: TComboBox
      Left = 140
      Top = 30
      Width = 189
      Height = 21
      Hint = 'Select packer process priority'
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 4
      Text = 'NORMAL (default)'
      Items.Strings = (
        'IDLE'
        'NORMAL (default)'
        'HIGH'
        'REALTIME')
    end
    object cmbIcons: TComboBox
      Left = 140
      Top = 70
      Width = 189
      Height = 21
      Hint = 'Select what to do with icons'
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 5
      Text = 'Don'#39't compress'
      Items.Strings = (
        'All but 1st directory'
        'All but 1st icon'
        'Don'#39't compress')
    end
    object chkExports: TCheckBox
      Left = 8
      Top = 44
      Width = 129
      Height = 17
      Hint = 'Compress exports'
      Caption = 'Compress exports'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
end
