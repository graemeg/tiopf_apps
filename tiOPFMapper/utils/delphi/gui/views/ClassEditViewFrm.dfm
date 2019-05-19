inherited ClassEditView: TClassEditView
  Caption = 'Edit Class Definition'
  ClientHeight = 561
  ClientWidth = 554
  OnCreate = FormCreate
  ExplicitWidth = 560
  ExplicitHeight = 590
  PixelsPerInch = 96
  TextHeight = 13
  object lblBaseClassName: TLabel [0]
    Left = 16
    Top = 13
    Width = 55
    Height = 13
    Caption = 'Class Name'
  end
  object lblBaseClassParent: TLabel [1]
    Left = 291
    Top = 13
    Width = 88
    Height = 13
    Caption = 'Base (Super) class'
  end
  inherited btnOK: TButton
    Left = 358
    Top = 520
    Anchors = [akRight, akBottom]
    TabOrder = 7
    ExplicitLeft = 358
    ExplicitTop = 520
  end
  inherited btnCancel: TButton
    Left = 455
    Top = 520
    Anchors = [akRight, akBottom]
    TabOrder = 8
    ExplicitLeft = 455
    ExplicitTop = 520
  end
  object eBaseClassName: TEdit
    Left = 16
    Top = 31
    Width = 249
    Height = 21
    TabOrder = 0
  end
  object eBaseClassParent: TEdit
    Left = 288
    Top = 31
    Width = 249
    Height = 21
    TabOrder = 1
  end
  object ckAutoMap: TCheckBox
    Left = 16
    Top = 62
    Width = 249
    Height = 17
    Caption = 'Register Auto Map (Required for Querying)'
    TabOrder = 2
  end
  object ckAutoCreateList: TCheckBox
    Left = 16
    Top = 85
    Width = 249
    Height = 17
    Caption = 'Create List Class for resulting Class'
    TabOrder = 3
  end
  object pcClass: TPageControl
    Left = 16
    Top = 144
    Width = 521
    Height = 361
    ActivePage = tsMapping
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 6
    object tsProps: TTabSheet
      Caption = 'Properties'
      object lvProps: TListView
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 481
        Height = 301
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alClient
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
      object lblPropMappings: TLabel
        Left = 16
        Top = 160
        Width = 70
        Height = 13
        Caption = 'Property Maps'
      end
      object lvMapping: TListView
        AlignWithMargins = True
        Left = 16
        Top = 179
        Width = 481
        Height = 138
        Margins.Left = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alBottom
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
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 481
        Height = 132
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alTop
        Caption = 'Class <--> Database'
        TabOrder = 0
        object lblTableName: TLabel
          Left = 16
          Top = 22
          Width = 56
          Height = 13
          Caption = 'Table Name'
        end
        object lblPropType: TLabel
          Left = 280
          Top = 22
          Width = 46
          Height = 13
          Caption = 'OID Type'
        end
        object lblPKField: TLabel
          Left = 16
          Top = 75
          Width = 114
          Height = 13
          Caption = 'Database PK field Name'
        end
        object lblPKName: TLabel
          Left = 280
          Top = 75
          Width = 115
          Height = 13
          Caption = 'Class PK Property Name'
        end
        object ePKField: TEdit
          Left = 16
          Top = 94
          Width = 181
          Height = 21
          TabOrder = 2
        end
        object cboOIDType: TComboBox
          Left = 280
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
          Left = 16
          Top = 40
          Width = 241
          Height = 21
          TabOrder = 0
        end
        object ePKName: TEdit
          Left = 280
          Top = 94
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
      object lvValidators: TListView
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 481
        Height = 301
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alClient
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
      object lvSelections: TListView
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 481
        Height = 301
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alClient
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
    Left = 16
    Top = 108
    Width = 249
    Height = 17
    Caption = 'List saves database name'
    TabOrder = 4
  end
  object ckNotifyObservers: TCheckBox
    Left = 288
    Top = 62
    Width = 249
    Height = 17
    Caption = 'Generate NotifyObserver calls'
    TabOrder = 5
  end
  object pmProps: TPopupMenu
    Left = 336
    Top = 88
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
    Left = 384
    Top = 88
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
    Left = 440
    Top = 88
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
    Left = 504
    Top = 88
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
