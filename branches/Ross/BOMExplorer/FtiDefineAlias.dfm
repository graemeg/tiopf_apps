inherited tiDefineAlias: TtiDefineAlias
  Caption = 'Define Alias'
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton [1]
    Left = 460
    Top = 163
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'btnCancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOK: TButton [2]
    Left = 374
    Top = 163
    Width = 75
    Height = 25
    Action = actOK
    Default = True
    TabOrder = 2
  end
  inherited AL: TActionList
    object actOK: TAction
      Caption = 'actOK'
      OnExecute = actOKExecute
      OnUpdate = actOKUpdate
    end
  end
end
