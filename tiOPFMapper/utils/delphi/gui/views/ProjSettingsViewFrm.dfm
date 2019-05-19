inherited ProjectSettingsView: TProjectSettingsView
  Caption = 'Project Settings'
  ClientHeight = 378
  ClientWidth = 498
  ExplicitWidth = 504
  ExplicitHeight = 407
  DesignSize = (
    498
    378)
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TButton
    Left = 308
    Top = 336
    Width = 81
    Anchors = [akRight, akBottom]
    ExplicitLeft = 267
    ExplicitTop = 239
    ExplicitWidth = 81
  end
  inherited btnCancel: TButton
    Left = 400
    Top = 336
    Anchors = [akRight, akBottom]
    ExplicitLeft = 359
    ExplicitTop = 239
  end
  object pgcOptions: TPageControl
    Left = 16
    Top = 16
    Width = 466
    Height = 299
    ActivePage = shtCode
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    object shtGeneral: TTabSheet
      Caption = '&General'
      ExplicitLeft = 0
      ExplicitWidth = 449
      ExplicitHeight = 270
      object lblEnumType: TLabel
        Left = 16
        Top = 159
        Width = 90
        Height = 13
        Caption = 'Enum Type Format'
      end
      object lblProjName: TLabel
        Left = 16
        Top = 16
        Width = 64
        Height = 13
        Caption = 'Project Name'
      end
      object lblOutDir: TLabel
        Left = 16
        Top = 111
        Width = 234
        Height = 13
        Caption = 'Output Folder (Relative to Project File'#39's FilePath)'
      end
      object Label2: TLabel
        Left = 16
        Top = 64
        Width = 129
        Height = 13
        Caption = 'Current path and file name'
      end
      object EnumTypeCombo: TComboBox
        Left = 16
        Top = 176
        Width = 105
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 3
        Text = 'int'
        Items.Strings = (
          'int'
          'string')
      end
      object eProjName: TEdit
        Left = 16
        Top = 33
        Width = 425
        Height = 21
        TabOrder = 0
      end
      object eOutDir: TEdit
        Left = 16
        Top = 128
        Width = 425
        Height = 21
        TabOrder = 2
      end
      object eCurrentPathAndFileName: TEdit
        Left = 16
        Top = 81
        Width = 425
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
      end
    end
    object shtCode: TTabSheet
      Caption = '&Code'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 85
        Height = 13
        Caption = 'Indentation width'
      end
      object edtIndentationWidth: TSpinEdit
        Left = 16
        Top = 33
        Width = 89
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object ckDoubleQuoteDBFieldNames: TCheckBox
        Left = 16
        Top = 80
        Width = 185
        Height = 17
        Caption = 'Double quote DB field names'
        TabOrder = 1
      end
    end
  end
end
