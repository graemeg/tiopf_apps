inherited EnumValueEditView: TEnumValueEditView
  Caption = 'Edit Value'
  ClientHeight = 169
  ClientWidth = 211
  ExplicitWidth = 217
  ExplicitHeight = 198
  PixelsPerInch = 96
  TextHeight = 13
  object lblValueName: TLabel [0]
    Left = 16
    Top = 13
    Width = 56
    Height = 13
    Caption = 'Value Name'
  end
  object lblValue: TLabel [1]
    Left = 16
    Top = 68
    Width = 102
    Height = 13
    Caption = 'Value (-1 = no value)'
  end
  inherited btnOK: TButton
    Left = 16
    Top = 128
    Anchors = [akRight, akBottom]
    TabOrder = 2
    ExplicitLeft = 16
    ExplicitTop = 128
  end
  inherited btnCancel: TButton
    Left = 112
    Top = 128
    Anchors = [akRight, akBottom]
    TabOrder = 3
    ExplicitLeft = 112
    ExplicitTop = 128
  end
  object eName: TEdit
    Left = 16
    Top = 32
    Width = 178
    Height = 21
    TabOrder = 0
  end
  object eValue: TEdit
    Left = 16
    Top = 87
    Width = 178
    Height = 21
    TabOrder = 1
  end
end
