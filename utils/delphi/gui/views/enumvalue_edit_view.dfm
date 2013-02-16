inherited EnumValueEditView: TEnumValueEditView
  Caption = 'Edit Value'
  ClientHeight = 141
  ClientWidth = 173
  ExplicitWidth = 179
  ExplicitHeight = 167
  PixelsPerInch = 96
  TextHeight = 13
  object lblValueName: TLabel [0]
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'Value Name'
  end
  object lblValue: TLabel [1]
    Left = 8
    Top = 52
    Width = 102
    Height = 13
    Caption = 'Value (-1 = no value)'
  end
  inherited btnOK: TButton
    Left = 80
    Top = 107
    TabOrder = 2
    ExplicitLeft = 538
    ExplicitTop = 403
  end
  object eName: TEdit
    Left = 8
    Top = 27
    Width = 154
    Height = 21
    TabOrder = 0
  end
  object eValue: TEdit
    Left = 8
    Top = 71
    Width = 154
    Height = 21
    TabOrder = 1
  end
end
