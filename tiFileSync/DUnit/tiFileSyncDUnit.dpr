program tiFileSyncDUnit;

{$I tiDefines.inc}

{ In Project Settings -> Compiler Settings -> Other,
  enable the compiler define "UseRemoteSync" if you want to
  test remote functionality too. }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  tiBaseObject,
  tiLog,
  tiLogToFile,
  tiLogToGUI,
  tiOPFManager,
  tiQueryXMLLight,
  tiUtils,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  Forms,
  TestFramework,
  GUITestRunner,
  tiFileName_TST in 'tiFileName_TST.pas',
  tiFileSyncSetup_TST in 'tiFileSyncSetup_TST.pas',
  tiFileSyncMgr_TST in 'tiFileSyncMgr_TST.pas',
  tiFileSyncReader_TST in 'tiFileSyncReader_TST.pas',
  tiFileSyncDependencies in '..\Core\tiFileSyncDependencies.pas';

{$R *.res}

begin
  gLog.RegisterLog(TtiLogToFile);
  gLog.RegisterLog(TtiLogToGUI);
  Application.Initialize;
  tiFileSyncDependencies.RegisterMappings;
  // Probably don't want this here - should connect as required in the tests
  tiFileSyncDependencies.ConnectToDatabase;
  tiFileName_TST.RegisterTests ;
  tiFileSyncSetup_TST.RegisterTests ;
  tiFileSyncMgr_TST.RegisterTests ;
  tiFileSyncReader_TST.RegisterTests ;
  GUITestRunner.RunRegisteredTests;

end.
