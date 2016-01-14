program sqleditor;

{$mode objfpc}{$H+}
{$ifdef windows}{$apptype gui}{$endif}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fpg_main, fpg_form, fpg_button, fpg_dialogs, tiOPFManager,
  tiConstants, tiCommandLineParams, tiLog, tiLogToConsole, tiLogToGUI,
  tiLogToFile, tiLogToDebugSvr, frm_main, frm_login, frm_editor,
  frm_pickdatabaseobject, commands, constants, frm_browseresult,
  frm_runasscript, tiSQLMgrDataSet_BOM, tiSQLMgrDataSet_Srv;

{$IFDEF WINDOWS}
  {$R sqleditor.rc}
  {.$R sqleditor.res}
{$ENDIF}

var
  lDatabaseName: string;
  lUserName: string;
  lPassword: string;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;

  gLog.SevToLog :=  [
    lsNormal
    ,lsUserInfo
    ,lsObjCreation
    ,lsVisitor
    ,lsConnectionPool
    ,lsAcceptVisitor
    ,lsQueryTiming
    ,lsDebug
    ,lsWarning
    ,lsError
    ,lsSQL
    ];

  // Logging
  if gCommandLineParams.IsParam(csLogConsole) then
    gLog.RegisterLog(TtiLogToConsole);
  if gCommandLineParams.IsParam(csLog) then
    gLog.RegisterLog(TtiLogToFile.CreateWithFileName('.','tiSQLEditor.log', True));
  if gCommandLineParams.IsParam(csLogVisual) then
    gLog.RegisterLog(TtiLogToGUI);
  if gCommandLineParams.IsParam(csLogDebugSvr) then
    gLog.RegisterLog(TtiLogToDebugSvr);


  lDatabaseName := gCommandLineParams.GetParam('d');
  lUserName     := gCommandLineParams.GetParam('u');
  lPassword     := gCommandLineParams.GetParam('p');

  GTIOPFManager.DefaultPersistenceLayerName := cTIPersistSqldbIB;
//  writeln('Known persistence layers:');
//  for i := 0 to GTIOPFManager.PersistenceLayers.Count-1 do
//    writeln('  ' + GTIOPFManager.PersistenceLayers.Items[i].PersistenceLayerName);
//  writeln('');


//  if (lDatabaseName = '') or (lUserName = '') or (lPassword = '') then
//  begin
//    writeln('ERROR: Connect to the appropriate database using -d -u and -p command line params.');
//    writeln('');
//    exit;
//  end;


  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
    if gTIOPFManager.DefaultPerLayer.DBConnectionPools.Count > 0 then
      gTIOPFManager.DisconnectDatabase;
    gTIOPFManager.Terminate;
  end;
end;

begin
  MainProc;
end.
