object tiValidationErrorsDlg: TtiValidationErrorsDlg
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'SCaption'
  ClientHeight = 246
  ClientWidth = 453
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    453
    246)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 437
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
    ExplicitWidth = 297
    ExplicitHeight = 161
  end
  object lblMessage: TLabel
    Left = 24
    Top = 16
    Width = 58
    Height = 13
    Caption = 'SlblMessage'
  end
  object OKBtn: TButton
    Left = 370
    Top = 213
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object mmErrors: TMemo
    Left = 24
    Top = 35
    Width = 405
    Height = 150
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
end
