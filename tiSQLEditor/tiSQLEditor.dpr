program tiSQLEditor;

uses
  Forms,
  FMainSQLEditor in 'FMainSQLEditor.pas' {FMainSQLEditor},
  tiLogReg,
  tiOIDGUID,
  tiOPFManager,
  FSQLEditor in 'FSQLEditor.pas' {FSQLEditor},
  FSQLMgrBrowse in 'FSQLMgrBrowse.pas' {FSQLMgrBrowse},
  tiSQLMgrDataSet_Srv in 'tiSQLMgrDataSet_Srv.pas',
  tiSQLMgrDataSet_BOM in 'tiSQLMgrDataSet_BOM.pas',
  tiCommandLineParams,
  SysUtils,
  FRunAsScript in 'FRunAsScript.pas';

{$R *.res}

var
  lDatabaseName: string;
  lUserName: string;
  lPassword: string;
  lPerLayerName: string;
begin
  Application.Initialize;
  

  lDatabaseName := gCommandLineParams.GetParam('d');
  lUserName     := gCommandLineParams.GetParam('u');
  lPassword     := gCommandLineParams.GetParam('p');

  if gCommandLineParams.IsParam(['help', 'h', '?']) then
  begin
    if IsConsole then
    begin
      writeln('Connect to the appropriate database using -d -u and -p command line params.');
    end
    else
    begin
      tiAppMessage('Connect to the appropriate database using -d -u and -p command line params.');
    end;
    Exit;
  end
  else if (lDatabaseName = '') or (lUserName = '') or (lPassword = '') then
  begin
    if IsConsole then
    begin
      writeln('Connect to the appropriate database using -d -u and -p command line params.');
    end
    else
    begin
      tiAppMessage('Connect to the appropriate database using -d -u and -p command line params.');
    end;
    Exit;
  end;

  // Connect to the appropriate database using -d -u and -p command line params.
  // persistence layer linked in via a compiler directive. eg: LINK_FBL = Firebird FBLib
  gTIOPFManager.ConnectDatabase(lDatabaseName, lUserName, lPassword);

  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

  gTIOPFManager.DisconnectDatabase;
  gTIOPFManager.Terminate;
end.

