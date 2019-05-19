inherited ProjectOptionsView: TProjectOptionsView
  Caption = 'Project options'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlClient: TPanel
    inherited tvSections: TTreeView
      Constraints.MinWidth = 150
      RowSelect = True
      ShowLines = False
      Items.NodeData = {
        03030000002C0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        00000000000107470065006E006500720061006C003C00000000000000010000
        00FFFFFFFFFFFFFFFF000000000000000000000000010F43006F006400650020
        00470065006E00650072006100740069006F006E002E00000000000000020000
        00FFFFFFFFFFFFFFFF0000000000000000000000000108440061007400610062
        00610073006500}
    end
    inherited scbOptionsPropertiesScrollArea: TScrollBox
      ExplicitTop = 8
    end
  end
end
