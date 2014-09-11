inherited ParamEditView: TParamEditView
  Caption = 'Edit Select Parameter'
  ClientHeight = 194
  ClientWidth = 375
  ExplicitWidth = 381
  ExplicitHeight = 220
  PixelsPerInch = 96
  TextHeight = 13
  object lblSelectName: TLabel [0]
    Left = 8
    Top = 8
    Width = 80
    Height = 13
    Caption = 'Parameter Name'
  end
  object lblSQLParamName: TLabel [1]
    Left = 188
    Top = 8
    Width = 82
    Height = 13
    Caption = 'SQL Param Name'
  end
  object lblPropType: TLabel [2]
    Left = 8
    Top = 54
    Width = 57
    Height = 13
    Caption = 'Param Type'
  end
  object lblPassBy: TLabel [3]
    Left = 188
    Top = 54
    Width = 109
    Height = 13
    Caption = 'Pass Method Param By'
  end
  object lblParamTypeName: TLabel [4]
    Left = 8
    Top = 100
    Width = 164
    Height = 13
    Caption = 'Enum Name (Param Type = enum)'
  end
  inherited btnOK: TButton
    Left = 282
    Top = 160
    TabOrder = 5
    ExplicitLeft = 282
    ExplicitTop = 115
  end
  object eParamName: TEdit
    Left = 8
    Top = 27
    Width = 174
    Height = 21
    TabOrder = 0
  end
  object eSQLParamName: TEdit
    Left = 188
    Top = 27
    Width = 174
    Height = 21
    TabOrder = 1
  end
  object cboParamType: TComboBox
    Left = 8
    Top = 73
    Width = 174
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      'ptString'
      'ptAnsiString'
      'ptDouble'
      'ptSingle'
      'ptCurrency'
      'ptInteger'
      'ptInt64'
      'ptDateTime'
      'ptBoolean'
      'ptEnum')
  end
  object cboPassBy: TComboBox
    Left = 188
    Top = 73
    Width = 174
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      ''
      'const'
      'var')
  end
  object cboParamTypeName: TComboBox
    Left = 8
    Top = 119
    Width = 174
    Height = 21
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'ptString'
      'ptAnsiString'
      'ptDouble'
      'ptSingle'
      'ptCurrency'
      'ptInteger'
      'ptInt64'
      'ptDateTime'
      'ptBoolean'
      'ptEnum')
  end
end
