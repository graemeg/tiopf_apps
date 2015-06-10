program BOMExplorer;

uses
  Forms,
  Transit_BOM in 'Transit_BOM.pas',
  Transit_Cli in 'Transit_Cli.pas',
  Transit_AutoMap_Svr in 'Transit_AutoMap_Svr.pas',
  tiHelperClasses in 'tiHelperClasses.pas',
  tiBOMExploration in 'tiBOMExploration.pas',
  Main in 'Main.pas' {FtiBOMExplorer},
  FtiEditFrame in 'FtiEditFrame.pas' {tiEditFrame: TFrame},
  FtiAutoEditFrame in 'FtiAutoEditFrame.pas' {tiAutoEditFrame: TFrame},
  FtiValidationErrors in 'FtiValidationErrors.pas' {tiValidationErrorsDlg},
  FtiSelectBOMFrame in 'FtiSelectBOMFrame.pas' {tiSelectBOMFrame: TFrame},
  FtiSelectBOMDlg in 'FtiSelectBOMDlg.PAS' {tiSelectBOMDlg},
  FtiDefineAlias in 'FtiDefineAlias.pas' {tiDefineAlias},
  Transit_DBIndependentVisitors_Svr in 'Transit_DBIndependentVisitors_Svr.pas',
  Transit_HardCodeVisitors_Svr in 'Transit_HardCodeVisitors_Svr.pas',
  FPickDatabase in 'FPickDatabase.pas' {FormPickDatabase};

{$R *.res}

var
  AExplorableBOM: TtiExplorableBOM;
  AAlias: TtiAlias;
  ARegistrar: TtiBOMERegistrar;

  function SelectBOM: Boolean;
  var
    Dialog: TtiSelectBOMDlg;
  begin
    Dialog := TtiSelectBOMDlg.Create(nil);
    try
      Result := Dialog.Execute(AExplorableBOM, AAlias, ARegistrar);
    finally
      Dialog.Free;
    end;
  end;

begin
  Application.Initialize;
  if not SelectBOM then
    Exit;
  Application.Title := 'BOM Explorer';
  Application.CreateForm(TFtiBOMExplorer, FtiBOMExplorer);
  FtiBOMExplorer.SetBOM(AExplorableBOM, AAlias, ARegistrar);
  Application.Run;
end.
