object ProjectCodeGenerationOptionsView: TProjectCodeGenerationOptionsView
  Left = 0
  Top = 0
  Width = 425
  Height = 367
  TabOrder = 0
  object Label4: TLabel
    Left = 16
    Top = 8
    Width = 140
    Height = 23
    Caption = 'Code Generation'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 16
    Top = 55
    Width = 85
    Height = 13
    Caption = 'Indentation width'
  end
  object Label6: TLabel
    Left = 16
    Top = 103
    Width = 72
    Height = 13
    Caption = 'Begin/End tabs'
  end
  object Label7: TLabel
    Left = 16
    Top = 151
    Width = 106
    Height = 13
    Caption = 'Max editor code width'
  end
  object Label8: TLabel
    Left = 16
    Top = 199
    Width = 61
    Height = 13
    Caption = 'Visibility tabs'
  end
  object edtIndentationWidth: TSpinEdit
    Left = 16
    Top = 72
    Width = 153
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object edtBeginEndTabs: TSpinEdit
    Left = 16
    Top = 120
    Width = 153
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object edtMaxEditoCodeWidth: TSpinEdit
    Left = 16
    Top = 168
    Width = 153
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object edtVisibilityTabs: TSpinEdit
    Left = 16
    Top = 216
    Width = 153
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
end
