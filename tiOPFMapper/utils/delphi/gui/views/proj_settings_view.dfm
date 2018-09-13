inherited ProjectSettingsView: TProjectSettingsView
  Caption = 'Project Settings'
  ClientHeight = 179
  ClientWidth = 367
  ExplicitWidth = 373
  ExplicitHeight = 208
  PixelsPerInch = 96
  TextHeight = 13
  object lblProjName: TLabel [0]
    Left = 3
    Top = 33
    Width = 64
    Height = 13
    Caption = 'Project Name'
  end
  object lblFileName: TLabel [1]
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 361
    Height = 24
    Align = alTop
    AutoSize = False
    Caption = 'Project Name'
    WordWrap = True
  end
  object lblOutDir: TLabel [2]
    Left = 3
    Top = 79
    Width = 234
    Height = 13
    Caption = 'Output Folder (Relative to Project File'#39's FilePath)'
  end
  object lblEnumType: TLabel [3]
    Left = 254
    Top = 33
    Width = 90
    Height = 13
    Caption = 'Enum Type Format'
  end
  object Label1: TLabel [4]
    Left = 3
    Top = 121
    Width = 89
    Height = 13
    Caption = 'Indentation width:'
  end
  inherited btnOK: TButton
    Left = 274
    Top = 145
    TabOrder = 3
    ExplicitLeft = 274
    ExplicitTop = 145
  end
  object eProjName: TEdit
    Left = 3
    Top = 48
    Width = 215
    Height = 21
    TabOrder = 0
  end
  object eOutDir: TEdit
    Left = 3
    Top = 94
    Width = 299
    Height = 21
    TabOrder = 2
  end
  object EnumTypeCombo: TComboBox
    Left = 254
    Top = 48
    Width = 104
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 1
    Text = 'int'
    Items.Strings = (
      'int'
      'string')
  end
  object edtIndentationWidth: TSpinEdit
    Left = 3
    Top = 136
    Width = 53
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
  end
end
