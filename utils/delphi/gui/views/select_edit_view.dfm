inherited SelectEditView: TSelectEditView
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Edit Selection'
  ClientHeight = 437
  ClientWidth = 483
  Position = poMainFormCenter
  ExplicitWidth = 489
  PixelsPerInch = 96
  TextHeight = 13
  object lblSQL: TLabel [0]
    Left = 8
    Top = 160
    Width = 104
    Height = 13
    Caption = 'Select SQL Statement'
  end
  object lblParams: TLabel [1]
    Left = 8
    Top = 54
    Width = 55
    Height = 13
    Caption = 'Parameters'
  end
  object lblSelectName: TLabel [2]
    Left = 8
    Top = 8
    Width = 99
    Height = 13
    Caption = 'Select/Method Name'
  end
  inherited btnOK: TButton
    Left = 390
    Top = 403
    TabOrder = 3
    ExplicitLeft = 390
    ExplicitTop = 403
  end
  object memSQL: TMemo
    Left = 8
    Top = 179
    Width = 467
    Height = 210
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
    Left = 8
    Top = 73
    Width = 467
    Height = 81
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    Left = 8
    Top = 27
    Width = 467
    Height = 21
    TabOrder = 0
  end
  object pmParams: TPopupMenu
    Left = 157
    Top = 91
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
