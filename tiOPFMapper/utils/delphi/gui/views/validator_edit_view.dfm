inherited ValidatorEditView: TValidatorEditView
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Edit Validator'
  ClientHeight = 193
  ClientWidth = 199
  Position = poMainFormCenter
  ExplicitWidth = 205
  ExplicitHeight = 219
  PixelsPerInch = 96
  TextHeight = 13
  object lblValue: TLabel [0]
    Left = 8
    Top = 100
    Width = 26
    Height = 13
    Caption = 'Value'
  end
  object lblClassProp: TLabel [1]
    Left = 8
    Top = 8
    Width = 70
    Height = 13
    Caption = 'Class Property'
  end
  object lblValidatorType: TLabel [2]
    Left = 8
    Top = 54
    Width = 69
    Height = 13
    Caption = 'Validator Type'
  end
  inherited btnOK: TButton
    Left = 106
    Top = 159
  end
  object eValue: TEdit
    Left = 8
    Top = 119
    Width = 181
    Height = 21
    TabOrder = 1
  end
  object cboClassProp: TComboBox
    Left = 8
    Top = 27
    Width = 181
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      '')
  end
  object cboValidatorType: TComboBox
    Left = 8
    Top = 73
    Width = 181
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
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
