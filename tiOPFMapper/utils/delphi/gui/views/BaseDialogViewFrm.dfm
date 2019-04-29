inherited BaseDialogView: TBaseDialogView
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'BaseDialogView'
  ClientHeight = 441
  ClientWidth = 633
  KeyPreview = True
  Position = poMainFormCenter
  ExplicitWidth = 639
  ExplicitHeight = 470
  DesignSize = (
    633
    441)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 276
    Top = 408
    Width = 82
    Height = 26
    Anchors = [akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 368
    Top = 407
    Width = 82
    Height = 26
    Anchors = [akBottom]
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
  end
end
