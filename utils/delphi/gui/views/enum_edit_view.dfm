inherited EnumEditView: TEnumEditView
  Caption = 'Edit Enumeration'
  ClientHeight = 203
  ClientWidth = 351
  ExplicitWidth = 357
  ExplicitHeight = 229
  DesignSize = (
    351
    203)
  PixelsPerInch = 96
  TextHeight = 13
  object lblEnumName: TLabel [0]
    Left = 8
    Top = 8
    Width = 90
    Height = 13
    Caption = 'Enumeration Name'
  end
  object lblEnumValues: TLabel [1]
    Left = 8
    Top = 54
    Width = 31
    Height = 13
    Caption = 'Values'
  end
  inherited btnOK: TButton
    Left = 258
    Top = 169
  end
  object eEnumName: TEdit
    Left = 8
    Top = 27
    Width = 202
    Height = 21
    TabOrder = 1
  end
  object lvValue: TListView
    Left = 8
    Top = 73
    Width = 335
    Height = 90
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PopupMenu = pmValues
    TabOrder = 2
    ExplicitHeight = 289
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
