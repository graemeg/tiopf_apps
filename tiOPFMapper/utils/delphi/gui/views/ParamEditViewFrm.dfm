inherited ParamEditView: TParamEditView
  Caption = 'Edit Select Parameter'
  ClientHeight = 233
  ClientWidth = 401
  ExplicitWidth = 407
  ExplicitHeight = 262
  PixelsPerInch = 96
  TextHeight = 13
  object lblSelectName: TLabel [0]
    Left = 16
    Top = 14
    Width = 80
    Height = 13
    Caption = 'Parameter Name'
  end
  object lblSQLParamName: TLabel [1]
    Left = 208
    Top = 14
    Width = 82
    Height = 13
    Caption = 'SQL Param Name'
  end
  object lblPropType: TLabel [2]
    Left = 16
    Top = 70
    Width = 57
    Height = 13
    Caption = 'Param Type'
  end
  object lblPassBy: TLabel [3]
    Left = 16
    Top = 126
    Width = 109
    Height = 13
    Caption = 'Pass Method Param By'
  end
  inherited btnOK: TButton
    Left = 207
    Top = 192
    Anchors = [akRight, akBottom]
    TabOrder = 4
    ExplicitLeft = 207
    ExplicitTop = 192
  end
  inherited btnCancel: TButton
    Left = 303
    Top = 192
    Anchors = [akRight, akBottom]
    TabOrder = 5
    ExplicitLeft = 303
    ExplicitTop = 192
  end
  object eParamName: TEdit
    Left = 16
    Top = 32
    Width = 177
    Height = 21
    TabOrder = 0
  end
  object eSQLParamName: TEdit
    Left = 208
    Top = 32
    Width = 177
    Height = 21
    TabOrder = 1
  end
  object cboParamType: TComboBox
    Left = 16
    Top = 88
    Width = 177
    Height = 21
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
    Left = 16
    Top = 144
    Width = 177
    Height = 21
    TabOrder = 3
    Items.Strings = (
      ''
      'const'
      'var')
  end
end
