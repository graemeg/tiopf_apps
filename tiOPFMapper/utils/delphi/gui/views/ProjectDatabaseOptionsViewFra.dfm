object ProjectDatabaseOptionsView: TProjectDatabaseOptionsView
  Left = 0
  Top = 0
  Width = 425
  Height = 367
  TabOrder = 0
  object lblEnumType: TLabel
    Left = 16
    Top = 54
    Width = 90
    Height = 13
    Caption = 'Enum Type Format'
  end
  object Label3: TLabel
    Left = 16
    Top = 8
    Width = 78
    Height = 23
    Caption = 'Database'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object EnumTypeCombo: TComboBox
    Left = 16
    Top = 72
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'int'
    Items.Strings = (
      'int'
      'string')
  end
  object ckDoubleQuoteDBFieldNames: TCheckBox
    Left = 16
    Top = 114
    Width = 185
    Height = 17
    Caption = 'Double quote DB field names'
    TabOrder = 1
  end
end
