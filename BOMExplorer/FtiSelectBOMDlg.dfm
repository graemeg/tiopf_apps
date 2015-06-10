object tiSelectBOMDlg: TtiSelectBOMDlg
  Left = 248
  Top = 119
  Width = 613
  Height = 443
  Caption = 'SSelectBOM'
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    605
    416)
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 425
    Top = 384
    Width = 76
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 505
    Top = 384
    Width = 76
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'SCancelBtn'
    ModalResult = 2
    TabOrder = 1
  end
  object pnlFrameHolder: TPanel
    Left = 0
    Top = 0
    Width = 605
    Height = 378
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
  end
  object ActionList: TActionList
    Left = 128
    Top = 64
    object actOK: TAction
      Caption = 'SactOK'
      OnExecute = actOKExecute
      OnUpdate = actOKUpdate
    end
  end
end
