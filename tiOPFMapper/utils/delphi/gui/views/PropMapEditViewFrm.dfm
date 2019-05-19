inherited PropMapEditView: TPropMapEditView
  Caption = 'Edit Property Mapping'
  ClientHeight = 209
  ClientWidth = 225
  ExplicitWidth = 231
  ExplicitHeight = 238
  PixelsPerInch = 96
  TextHeight = 13
  object lblTableName: TLabel [0]
    Left = 16
    Top = 14
    Width = 100
    Height = 13
    Caption = 'Class Property Name'
  end
  object lblPropType: TLabel [1]
    Left = 16
    Top = 62
    Width = 97
    Height = 13
    Caption = 'Class Property Type'
  end
  object lblPKField: TLabel [2]
    Left = 16
    Top = 110
    Width = 101
    Height = 13
    Caption = 'Database Field Name'
  end
  inherited btnOK: TButton
    Left = 16
    Top = 168
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    ExplicitLeft = 16
    ExplicitTop = 168
  end
  inherited btnCancel: TButton
    Left = 129
    Top = 168
    Anchors = [akRight, akBottom]
    TabOrder = 4
    ExplicitLeft = 129
    ExplicitTop = 168
  end
  object eFieldName: TEdit
    Left = 16
    Top = 128
    Width = 195
    Height = 21
    TabOrder = 2
  end
  object cboPropType: TComboBox
    Left = 16
    Top = 80
    Width = 195
    Height = 21
    Style = csDropDownList
    TabOrder = 1
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
  object cboPropName: TComboBox
    Left = 16
    Top = 32
    Width = 195
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
end
