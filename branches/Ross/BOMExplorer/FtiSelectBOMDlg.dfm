object tiSelectBOMDlg: TtiSelectBOMDlg
  Left = 245
  Top = 108
  Caption = 'SSelectBOM'
  ClientHeight = 416
  ClientWidth = 589
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    589
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
    ExplicitLeft = 427
    ExplicitTop = 386
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
    ExplicitLeft = 507
    ExplicitTop = 386
  end
  object pnlFrameHolder: TPanel
    Left = 0
    Top = 0
    Width = 589
    Height = 378
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 591
    ExplicitHeight = 380
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
