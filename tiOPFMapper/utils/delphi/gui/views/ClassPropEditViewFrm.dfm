inherited ClassPropEditView: TClassPropEditView
  Caption = 'Edit Class Property Definition'
  ClientHeight = 209
  ClientWidth = 257
  ExplicitWidth = 263
  ExplicitHeight = 238
  PixelsPerInch = 96
  TextHeight = 13
  object lblBaseClassName: TLabel [0]
    Left = 16
    Top = 15
    Width = 100
    Height = 13
    Caption = 'Class Property Name'
  end
  object lblPropType: TLabel [1]
    Left = 16
    Top = 62
    Width = 69
    Height = 13
    Caption = 'Property Type'
  end
  object ePropName: TEdit [2]
    Left = 16
    Top = 32
    Width = 225
    Height = 21
    TabOrder = 0
  end
  object cboPropType: TComboBox [3]
    Left = 16
    Top = 80
    Width = 225
    Height = 21
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
  object ckReadOnly: TCheckBox [4]
    Left = 16
    Top = 124
    Width = 218
    Height = 17
    Caption = 'Read Only Property'
    TabOrder = 2
  end
  inherited btnOK: TButton
    Left = 64
    Top = 167
    Anchors = [akRight, akBottom]
    TabOrder = 3
    ExplicitLeft = 64
    ExplicitTop = 167
  end
  inherited btnCancel: TButton
    Left = 159
    Top = 167
    Anchors = [akRight, akBottom]
    TabOrder = 4
    ExplicitLeft = 159
    ExplicitTop = 167
  end
end
