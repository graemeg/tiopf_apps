inherited DialogView: TDialogView
  BorderStyle = bsDialog
  Caption = ''
  ClientHeight = 96
  ClientWidth = 270
  Position = poMainFormCenter
  ExplicitWidth = 276
  ExplicitHeight = 122
  PixelsPerInch = 96
  TextHeight = 13
  object lblMsg: TLabel [0]
    AlignWithMargins = True
    Left = 6
    Top = 4
    Width = 258
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
    Left = 86
    Top = 62
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
    ExplicitLeft = 86
    ExplicitTop = 62
  end
  object eValue: TEdit
    Left = 8
    Top = 35
    Width = 252
    Height = 21
    TabOrder = 0
    Visible = False
  end
  object btnCancel: TButton
    Left = 177
    Top = 62
    Width = 85
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object dtpValue: TDateTimePicker
    Left = 8
    Top = 35
    Width = 254
    Height = 21
    Date = 40502.410873877310000000
    Time = 40502.410873877310000000
    TabOrder = 1
    Visible = False
  end
end
