object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Class Mapping Designer'
  ClientHeight = 571
  ClientWidth = 834
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
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
    Width = 619
    Height = 552
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    object CurrentUnitLabel: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 611
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Current Unit:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 72
    end
    object MainPageControl: TPageControl
      AlignWithMargins = True
      Left = 3
      Top = 24
      Width = 613
      Height = 525
      ActivePage = tsEnums
      Align = alClient
      TabOrder = 0
      object tsClasses: TTabSheet
        Caption = 'Classes'
        object lvClasses: TListView
          Left = 0
          Top = 0
          Width = 605
          Height = 497
          Margins.Left = 16
          Margins.Top = 16
          Margins.Right = 16
          Margins.Bottom = 16
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
          Left = 0
          Top = 0
          Width = 605
          Height = 497
          Margins.Left = 16
          Margins.Top = 16
          Margins.Right = 16
          Margins.Bottom = 16
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
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 73
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
    Width = 834
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object mmMain: TMainMenu
    AutoHotkeys = maManual
    Left = 256
    Top = 72
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuNewProject: TMenuItem
        Caption = '&New Project'
      end
      object mnuOpen: TMenuItem
        Caption = '&Open Project...'
      end
      object mnuSave: TMenuItem
        Caption = '&Save Project'
      end
      object mnuSaveAs: TMenuItem
        Caption = 'Save Project &As...'
      end
      object mnuClose: TMenuItem
        Caption = '&Close Project'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuGenerate: TMenuItem
        Caption = '&Generate Project'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuSettings: TMenuItem
        Caption = 'Project Op&tions...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuMRU: TMenuItem
        Caption = '&Recent Projects'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = 'E&xit'
      end
    end
  end
  object pmClasses: TPopupMenu
    Left = 368
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
    Left = 312
    Top = 72
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
    Left = 432
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
