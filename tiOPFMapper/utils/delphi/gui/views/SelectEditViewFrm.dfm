inherited SelectEditView: TSelectEditView
  Caption = 'Edit Selection'
  ClientHeight = 457
  ClientWidth = 482
  ExplicitWidth = 488
  ExplicitHeight = 486
  PixelsPerInch = 96
  TextHeight = 13
  object lblSQL: TLabel [0]
    Left = 16
    Top = 173
    Width = 104
    Height = 13
    Caption = 'Select SQL Statement'
  end
  object lblParams: TLabel [1]
    Left = 16
    Top = 62
    Width = 55
    Height = 13
    Caption = 'Parameters'
  end
  object lblSelectName: TLabel [2]
    Left = 16
    Top = 14
    Width = 99
    Height = 13
    Caption = 'Select/Method Name'
  end
  inherited btnOK: TButton
    Left = 287
    Top = 416
    Anchors = [akRight, akBottom]
    TabOrder = 3
    ExplicitLeft = 287
    ExplicitTop = 416
  end
  inherited btnCancel: TButton
    Left = 383
    Top = 416
    Anchors = [akRight, akBottom]
    TabOrder = 4
    ExplicitLeft = 383
    ExplicitTop = 416
  end
  object memSQL: TMemo
    Left = 16
    Top = 192
    Width = 449
    Height = 209
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    WantTabs = True
    WordWrap = False
  end
  object lvParams: TListView
    Left = 16
    Top = 80
    Width = 449
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Columns = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PopupMenu = pmParams
    TabOrder = 1
  end
  object eName: TEdit
    Left = 16
    Top = 32
    Width = 449
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object pmParams: TPopupMenu
    Left = 157
    Top = 99
    object mnuNewParam: TMenuItem
      Caption = 'New Param'
    end
    object mnuEditParam: TMenuItem
      Caption = 'Edit Param'
    end
    object mnuDeleteParam: TMenuItem
      Caption = 'Delete Param'
    end
  end
end
