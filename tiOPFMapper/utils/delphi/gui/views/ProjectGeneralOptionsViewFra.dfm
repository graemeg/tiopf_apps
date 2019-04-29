object ProjectGeneralOptionsView: TProjectGeneralOptionsView
  Left = 0
  Top = 0
  Width = 425
  Height = 367
  TabOrder = 0
  DesignSize = (
    425
    367)
  object lblProjName: TLabel
    Left = 16
    Top = 64
    Width = 64
    Height = 13
    Caption = 'Project Name'
  end
  object lblOutDir: TLabel
    Left = 16
    Top = 159
    Width = 234
    Height = 13
    Caption = 'Output Folder (Relative to Project File'#39's FilePath)'
  end
  object Label2: TLabel
    Left = 16
    Top = 111
    Width = 129
    Height = 13
    Caption = 'Current path and file name'
  end
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 65
    Height = 23
    Caption = 'General'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object eProjName: TEdit
    Left = 16
    Top = 80
    Width = 393
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object eOutDir: TEdit
    Left = 16
    Top = 176
    Width = 393
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object eCurrentPathAndFileName: TEdit
    Left = 16
    Top = 128
    Width = 393
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
end
