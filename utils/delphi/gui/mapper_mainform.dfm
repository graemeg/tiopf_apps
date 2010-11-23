object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Class Mapping Designer'
  ClientHeight = 571
  ClientWidth = 783
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object splLeft: TSplitter
    Left = 209
    Top = 0
    Width = 6
    Height = 552
    ExplicitHeight = 485
  end
  object pnlClientAlign: TPanel
    Left = 215
    Top = 0
    Width = 568
    Height = 552
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    object CurrentUnitLabel: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 560
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Current Unit:'
      ExplicitWidth = 63
    end
    object MainPageControl: TPageControl
      AlignWithMargins = True
      Left = 3
      Top = 24
      Width = 562
      Height = 525
      ActivePage = tsEnums
      Align = alClient
      TabOrder = 0
      object tsClasses: TTabSheet
        Caption = 'Classes'
        object lvClasses: TListView
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 548
          Height = 491
          Align = alClient
          Columns = <>
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = pmClasses
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsEnums: TTabSheet
        Caption = 'Enumerations'
        ImageIndex = 1
        object lvEnums: TListView
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 548
          Height = 491
          Align = alClient
          Columns = <>
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = pmEnums
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
    end
  end
  object pnlLeftAlign: TPanel
    Left = 0
    Top = 0
    Width = 209
    Height = 552
    Align = alLeft
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object UnitLabel: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 201
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Project Units'
      ExplicitWidth = 61
    end
    object lvUnits: TListView
      AlignWithMargins = True
      Left = 3
      Top = 24
      Width = 203
      Height = 525
      Align = alClient
      Columns = <>
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      PopupMenu = pmUnits
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object statMain: TStatusBar
    Left = 0
    Top = 552
    Width = 783
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object mmMain: TMainMenu
    Left = 338
    Top = 71
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuNewProject: TMenuItem
        Caption = 'New Project'
      end
      object mnuOpen: TMenuItem
        Caption = 'Open Project'
      end
      object mnuSave: TMenuItem
        Caption = 'Save Project'
      end
      object mnuSaveAs: TMenuItem
        Caption = 'Save Project As ...'
      end
      object mnuClose: TMenuItem
        Caption = 'Close Project'
      end
      object mnuSettings: TMenuItem
        Caption = 'Project Settings'
      end
      object mnuGenerate: TMenuItem
        Caption = 'Generate Project'
      end
      object mnuExit: TMenuItem
        Caption = 'E&xit'
      end
    end
  end
  object pmClasses: TPopupMenu
    Left = 280
    Top = 72
    object mnuNewClass: TMenuItem
      Caption = 'New Class'
    end
    object mnuEditClass: TMenuItem
      Caption = 'Edit Class'
    end
    object mnuDeleteClass: TMenuItem
      Caption = 'Delete Class'
    end
  end
  object pmUnits: TPopupMenu
    Left = 89
    Top = 65
    object mnuAddUnit: TMenuItem
      Caption = 'New Unit'
    end
    object mnuUnitChangeName: TMenuItem
      Caption = 'Change Unit Name'
    end
    object mnuDeleteUnit: TMenuItem
      Caption = 'Delete Unit'
    end
  end
  object pmEnums: TPopupMenu
    Left = 404
    Top = 72
    object mnuNewEnum: TMenuItem
      Caption = 'New Enum'
    end
    object mnuEditEnum: TMenuItem
      Caption = 'Edit Enum'
    end
    object mnuDeleteEnum: TMenuItem
      Caption = 'Delete Enum'
    end
  end
end
