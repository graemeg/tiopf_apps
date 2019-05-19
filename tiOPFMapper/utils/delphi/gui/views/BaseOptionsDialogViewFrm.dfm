inherited BaseOptionsDialogView: TBaseOptionsDialogView
  Caption = 'Options'
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TButton
    Left = 444
    Top = 400
    ExplicitLeft = 444
    ExplicitTop = 400
  end
  inherited btnCancel: TButton
    Left = 536
    Top = 399
    ExplicitLeft = 536
    ExplicitTop = 399
  end
  object pnlClient: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 385
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    Padding.Top = 8
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    object splVertical: TSplitter
      Left = 201
      Top = 8
      Height = 377
      Color = clBtnFace
      ParentColor = False
      ResizeStyle = rsUpdate
      ExplicitLeft = 256
      ExplicitTop = 160
      ExplicitHeight = 100
    end
    object tvSections: TTreeView
      Left = 0
      Top = 8
      Width = 201
      Height = 377
      Align = alLeft
      BorderStyle = bsNone
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      ExplicitTop = 0
    end
    object scbOptionsPropertiesScrollArea: TScrollBox
      Left = 204
      Top = 8
      Width = 429
      Height = 377
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      ParentBackground = True
      TabOrder = 1
      ExplicitTop = 0
    end
  end
end
