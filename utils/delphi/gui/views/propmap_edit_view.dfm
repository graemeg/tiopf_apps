inherited PropMapEditView: TPropMapEditView
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Edit Property Mapping'
  ClientHeight = 192
  ClientWidth = 210
  Position = poMainFormCenter
  ExplicitWidth = 216
  ExplicitHeight = 218
  PixelsPerInch = 96
  TextHeight = 13
  object lblTableName: TLabel [0]
    Left = 8
    Top = 8
    Width = 100
    Height = 13
    Caption = 'Class Property Name'
  end
  object lblPropType: TLabel [1]
    Left = 8
    Top = 54
    Width = 97
    Height = 13
    Caption = 'Class Property Type'
  end
  object lblPKField: TLabel [2]
    Left = 8
    Top = 100
    Width = 101
    Height = 13
    Caption = 'Database Field Name'
  end
  inherited btnOK: TButton
    Left = 117
    Top = 158
    TabOrder = 3
    ExplicitLeft = 117
    ExplicitTop = 157
  end
  object eFieldName: TEdit
    Left = 8
    Top = 119
    Width = 195
    Height = 21
    TabOrder = 2
  end
  object cboPropType: TComboBox
    Left = 8
    Top = 73
    Width = 181
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
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
    Left = 8
    Top = 27
    Width = 181
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
end
