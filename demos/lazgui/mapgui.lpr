program mapgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
    cwstring,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, zvdatetimectrls, laz_langfactory_pkg, main_form, base_edit_ctrl,
  base_list_view_ctrl, base_edit_view, laz_timvc, base_form_view,
  base_list_view, event_const, laz_timapper, app_model, app_ctl,
  proj_settings_ctrl, app_cmds, edit_class_view, language_strings, input_query,
  project_info_view, app_events, edit_class_ctrl;

{$R *.res}

begin
  Application.Initialize;
  SetHeapTraceOutput('heap.txt');
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

