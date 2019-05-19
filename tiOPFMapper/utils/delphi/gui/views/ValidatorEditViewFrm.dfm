inherited ValidatorEditView: TValidatorEditView
  Caption = 'Edit Validator'
  ClientHeight = 230
  ClientWidth = 218
  ExplicitWidth = 224
  ExplicitHeight = 259
  PixelsPerInch = 96
  TextHeight = 13
  object lblValue: TLabel [0]
    Left = 16
    Top = 125
    Width = 26
    Height = 13
    Caption = 'Value'
  end
  object lblClassProp: TLabel [1]
    Left = 16
    Top = 14
    Width = 70
    Height = 13
    Caption = 'Class Property'
  end
  object lblValidatorType: TLabel [2]
    Left = 16
    Top = 70
    Width = 69
    Height = 13
    Caption = 'Validator Type'
  end
  inherited btnOK: TButton
    Left = 16
    Top = 192
    TabOrder = 3
    ExplicitLeft = 16
    ExplicitTop = 192
  end
  inherited btnCancel: TButton
    Left = 119
    Top = 192
    TabOrder = 4
    ExplicitLeft = 119
    ExplicitTop = 192
  end
  object eValue: TEdit
    Left = 16
    Top = 143
    Width = 185
    Height = 21
    TabOrder = 2
  end
  object cboClassProp: TComboBox
    Left = 16
    Top = 32
    Width = 185
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      '')
  end
  object cboValidatorType: TComboBox
    Left = 16
    Top = 88
    Width = 185
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    Items.Strings = (
      'vtRequired'
      'vtGreater'
      'vtGreaterEqual'
      'vtLess'
      'vtLessEqual'
      'vtNotEqual'
      'vtRegExp')
  end
end
