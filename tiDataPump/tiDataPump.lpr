program tiDataPump;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  frm_main,
  tiOIDGUID,
  tiLog,
  tiCommandLineParams,
  tiLogToFile,
  tiLogToGUI,
  tiLogToConsole;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;

  // fpGUI will handle this for us soon!
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  if gCommandLineParams.IsParam(csLogConsole) then
    gLog.RegisterLog(TtiLogToConsole);
  if gCommandLineParams.IsParam(csLog) then
    gLog.RegisterLog(TtiLogToFile.CreateWithFileName('.','tiDataPump.log', True));
  if gCommandLineParams.IsParam(csLogVisual) then
    gLog.RegisterLog(TtiLogToGUI);

  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

