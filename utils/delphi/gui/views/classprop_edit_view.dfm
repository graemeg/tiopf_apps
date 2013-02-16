inherited ClassPropEditView: TClassPropEditView
  Caption = 'Edit Class Property Definition'
  ClientHeight = 166
  ClientWidth = 243
  ExplicitWidth = 249
  ExplicitHeight = 192
  PixelsPerInch = 96
  TextHeight = 13
  object lblBaseClassName: TLabel [0]
    Left = 8
    Top = 8
    Width = 100
    Height = 13
    Caption = 'Class Property Name'
  end
  object lblPropType: TLabel [1]
    Left = 8
    Top = 54
    Width = 69
    Height = 13
    Caption = 'Property Type'
  end
  inherited btnOK: TButton
    Left = 150
    Top = 132
    TabOrder = 3
    ExplicitLeft = 150
    ExplicitTop = 132
  end
  object ePropName: TEdit
    Left = 8
    Top = 27
    Width = 227
    Height = 21
    TabOrder = 0
  end
  object cboPropType: TComboBox
    Left = 8
    Top = 73
    Width = 227
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'String'
    Items.Strings = (
      'String'
      'AnsiString'
      'Double'
      'Single'
      'Currency'
      'Integer'
      'Int64'
      'TDateTime'
      'Boolean'
      'Enum')
  end
  object ckReadOnly: TCheckBox
    Left = 8
    Top = 100
    Width = 218
    Height = 17
    Caption = 'Read Only Property'
    TabOrder = 2
  end
end
