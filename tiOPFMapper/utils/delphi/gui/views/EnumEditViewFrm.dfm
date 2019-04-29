inherited EnumEditView: TEnumEditView
  Caption = 'Edit Enumeration'
  ClientHeight = 369
  ClientWidth = 401
  ExplicitWidth = 407
  ExplicitHeight = 398
  DesignSize = (
    401
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object lblEnumName: TLabel [0]
    Left = 16
    Top = 13
    Width = 90
    Height = 13
    Caption = 'Enumeration Name'
  end
  object lblEnumValues: TLabel [1]
    Left = 16
    Top = 62
    Width = 31
    Height = 13
    Caption = 'Values'
  end
  inherited btnOK: TButton
    Left = 207
    Top = 328
    Anchors = [akRight, akBottom]
    TabOrder = 2
    ExplicitLeft = 207
    ExplicitTop = 328
  end
  inherited btnCancel: TButton
    Left = 303
    Top = 328
    Anchors = [akRight, akBottom]
    TabOrder = 3
    ExplicitLeft = 303
    ExplicitTop = 328
  end
  object eEnumName: TEdit
    Left = 16
    Top = 32
    Width = 369
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object lvValue: TListView
    Left = 16
    Top = 80
    Width = 369
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PopupMenu = pmValues
    TabOrder = 1
  end
  object ckCreateEnumerationSet: TCheckBox
    Left = 16
    Top = 264
    Width = 225
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Create enumeration set (Enter name)'
    TabOrder = 4
  end
  object eEnumerationSetName: TEdit
    Left = 16
    Top = 287
    Width = 369
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 5
  end
  object pmValues: TPopupMenu
    Left = 164
    Top = 100
    object mnuNewValue: TMenuItem
      Caption = 'New Value'
    end
    object mnuEditValue: TMenuItem
      Caption = 'Edit Value'
    end
    object mnuDeleteValue: TMenuItem
      Caption = 'Delete Value'
    end
  end
end
