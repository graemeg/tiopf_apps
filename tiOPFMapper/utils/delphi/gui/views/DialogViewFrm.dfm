inherited DialogView: TDialogView
  Caption = ''
  ClientHeight = 127
  ClientWidth = 305
  ExplicitWidth = 311
  ExplicitHeight = 156
  PixelsPerInch = 96
  TextHeight = 13
  object lblMsg: TLabel [0]
    AlignWithMargins = True
    Left = 6
    Top = 4
    Width = 293
    Height = 13
    Margins.Left = 6
    Margins.Top = 4
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alTop
    Caption = 'lblMsg'
    ExplicitWidth = 29
  end
  inherited btnOK: TButton
    Left = 112
    Top = 88
    TabOrder = 2
    OnClick = btnOKClick
    ExplicitLeft = 112
    ExplicitTop = 88
  end
  inherited btnCancel: TButton
    Left = 208
    Top = 88
    TabOrder = 3
    OnClick = btnCancelClick
    ExplicitLeft = 208
    ExplicitTop = 88
  end
  object eValue: TEdit
    Left = 18
    Top = 43
    Width = 272
    Height = 21
    TabOrder = 0
    Visible = False
  end
  object dtpValue: TDateTimePicker
    Left = 16
    Top = 43
    Width = 274
    Height = 21
    Date = 40502.000000000000000000
    Time = 0.410873877306585200
    TabOrder = 1
    Visible = False
  end
end
