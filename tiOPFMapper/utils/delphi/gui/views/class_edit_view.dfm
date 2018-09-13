inherited ClassEditView: TClassEditView
  Caption = 'Edit Class Definition'
  ClientHeight = 524
  ClientWidth = 545
  OnCreate = FormCreate
  ExplicitWidth = 551
  ExplicitHeight = 553
  PixelsPerInch = 96
  TextHeight = 13
  object lblBaseClassName: TLabel [0]
    Left = 8
    Top = 8
    Width = 55
    Height = 13
    Caption = 'Class Name'
  end
  object lblBaseClassParent: TLabel [1]
    Left = 247
    Top = 8
    Width = 88
    Height = 13
    Caption = 'Base (Super) class'
  end
  inherited btnOK: TButton
    Left = 452
    Top = 490
    TabOrder = 5
    ExplicitLeft = 452
    ExplicitTop = 490
  end
  object eBaseClassName: TEdit
    Left = 8
    Top = 23
    Width = 215
    Height = 21
    TabOrder = 0
  end
  object eBaseClassParent: TEdit
    Left = 247
    Top = 23
    Width = 215
    Height = 21
    TabOrder = 1
  end
  object ckAutoMap: TCheckBox
    Left = 8
    Top = 54
    Width = 298
    Height = 17
    Caption = 'Register Auto Map (Required for Querying)'
    TabOrder = 2
  end
  object ckAutoCreateList: TCheckBox
    Left = 8
    Top = 73
    Width = 298
    Height = 17
    Caption = 'Create List Class for resulting Class'
    TabOrder = 3
  end
  object pcClass: TPageControl
    Left = 8
    Top = 120
    Width = 529
    Height = 364
    ActivePage = tsMapping
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    object tsProps: TTabSheet
      Caption = 'Properties'
      DesignSize = (
        521
        336)
      object lvProps: TListView
        Left = 3
        Top = 3
        Width = 515
        Height = 330
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PopupMenu = pmProps
        TabOrder = 0
      end
    end
    object tsMapping: TTabSheet
      Caption = 'ORM Mapping'
      ImageIndex = 1
      DesignSize = (
        521
        336)
      object lblPropMappings: TLabel
        Left = 3
        Top = 125
        Width = 70
        Height = 13
        Caption = 'Property Maps'
      end
      object lvMapping: TListView
        Left = 3
        Top = 144
        Width = 515
        Height = 189
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PopupMenu = pmMapping
        TabOrder = 1
      end
      object gbORM: TGroupBox
        Left = 3
        Top = 0
        Width = 515
        Height = 119
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Class <--> Database'
        TabOrder = 0
        object lblTableName: TLabel
          Left = 8
          Top = 21
          Width = 56
          Height = 13
          Caption = 'Table Name'
        end
        object lblPropType: TLabel
          Left = 168
          Top = 21
          Width = 46
          Height = 13
          Caption = 'OID Type'
        end
        object lblPKField: TLabel
          Left = 8
          Top = 67
          Width = 114
          Height = 13
          Caption = 'Database PK field Name'
        end
        object lblPKName: TLabel
          Left = 168
          Top = 67
          Width = 115
          Height = 13
          Caption = 'Class PK Property Name'
        end
        object ePKField: TEdit
          Left = 8
          Top = 86
          Width = 144
          Height = 21
          TabOrder = 2
        end
        object cboOIDType: TComboBox
          Left = 168
          Top = 40
          Width = 181
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          Items.Strings = (
            'string'
            'int')
        end
        object eTableName: TEdit
          Left = 8
          Top = 40
          Width = 144
          Height = 21
          TabOrder = 0
        end
        object ePKName: TEdit
          Left = 168
          Top = 86
          Width = 181
          Height = 21
          TabOrder = 3
          Text = 'OID'
        end
      end
    end
    object tsValidators: TTabSheet
      Caption = 'Validators'
      ImageIndex = 2
      DesignSize = (
        521
        336)
      object lvValidators: TListView
        Left = 3
        Top = 3
        Width = 515
        Height = 330
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PopupMenu = pmValidators
        TabOrder = 0
      end
    end
    object tsSelections: TTabSheet
      Caption = 'Selections'
      ImageIndex = 3
      DesignSize = (
        521
        336)
      object lvSelections: TListView
        Left = 3
        Top = 3
        Width = 515
        Height = 330
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PopupMenu = pmSelects
        TabOrder = 0
      end
    end
  end
  object ckListSavesDBName: TCheckBox
    Left = 8
    Top = 92
    Width = 298
    Height = 17
    Caption = 'List saves database name'
    TabOrder = 6
  end
  object ckNotifyObservers: TCheckBox
    Left = 328
    Top = 54
    Width = 209
    Height = 17
    Caption = 'Generate NotifyObserver calls'
    TabOrder = 7
  end
  object pmProps: TPopupMenu
    Left = 371
    Top = 56
    object mnuAddProp: TMenuItem
      Caption = 'New Property'
    end
    object mnuEditProp: TMenuItem
      Caption = 'Edit Property'
    end
    object mnuDeleteProp: TMenuItem
      Caption = 'Delete Property'
    end
    object mnuPropSort: TMenuItem
      Caption = 'Sort'
      object mnuSortProps: TMenuItem
        Caption = 'Sort By Property Name'
      end
    end
  end
  object pmMapping: TPopupMenu
    Left = 427
    Top = 57
    object mnuNewMap: TMenuItem
      Caption = 'New Property Map'
    end
    object mnuEditMap: TMenuItem
      Caption = 'Edit Property Map'
    end
    object mnuDeleteMap: TMenuItem
      Caption = 'Delete Property Map'
    end
    object mnuMapSort: TMenuItem
      Caption = 'Sort'
      object mnuSortMapsByField: TMenuItem
        Caption = 'Sort By Field Name'
      end
      object mnuSortMapsByProp: TMenuItem
        Caption = 'Sort By Property Name'
      end
    end
  end
  object pmValidators: TPopupMenu
    Left = 494
    Top = 58
    object mnuNewValidator: TMenuItem
      Caption = 'New Validator'
    end
    object mnuEditValidator: TMenuItem
      Caption = 'Edit Validator'
    end
    object mnuDeleteValidator: TMenuItem
      Caption = 'Delete Validator'
    end
  end
  object pmSelects: TPopupMenu
    Left = 562
    Top = 57
    object mnuNewSelect: TMenuItem
      Caption = 'New Selection'
    end
    object mnuEditSelect: TMenuItem
      Caption = 'Edit Selection'
    end
    object mnuDeleteSelect: TMenuItem
      Caption = 'Delete Selection'
    end
  end
end
