program MapperGUI;

uses
  Forms,
  mapper_mainform in 'mapper_mainform.pas' {MainForm},
  base_view_form in 'views\base_view_form.pas' {BaseViewForm},
  base_dialog_view in 'views\base_dialog_view.pas' {BaseDialogView},
  app_ctrl in 'controllers\app_ctrl.pas',
  app_mdl in 'models\app_mdl.pas',
  event_const in 'events\event_const.pas',
  app_commands in 'commands\app_commands.pas',
  dialog_ctrl in 'controllers\dialog_ctrl.pas',
  dialog_view in 'views\dialog_view.pas' {DialogView},
  proj_settings_view in 'views\proj_settings_view.pas' {ProjectSettingsView},
  project_settings_ctrl in 'controllers\project_settings_ctrl.pas',
  class_edit_view in 'views\class_edit_view.pas' {ClassEditView},
  class_edit_ctrl in 'controllers\class_edit_ctrl.pas',
  class_edit_cmd in 'commands\class_edit_cmd.pas',
  classprop_edit_view in 'views\classprop_edit_view.pas' {ClassPropEditView},
  classprop_edit_ctrl in 'controllers\classprop_edit_ctrl.pas',
  propmap_edit_view in 'views\propmap_edit_view.pas' {PropMapEditView},
  propmap_edit_ctrl in 'controllers\propmap_edit_ctrl.pas',
  app_consts in 'app_consts.pas',
  validator_edit_view in 'views\validator_edit_view.pas' {ValidatorEditView},
  validator_edit_ctrl in 'controllers\validator_edit_ctrl.pas',
  select_edit_view in 'views\select_edit_view.pas' {SelectEditView},
  select_edit_ctrl in 'controllers\select_edit_ctrl.pas',
  param_edit_view in 'views\param_edit_view.pas' {ParamEditView},
  param_edit_ctrl in 'controllers\param_edit_ctrl.pas',
  enum_edit_view in 'views\enum_edit_view.pas' {EnumEditView},
  select_edit_cmd in 'commands\select_edit_cmd.pas',
  enum_edit_ctrl in 'controllers\enum_edit_ctrl.pas',
  enumvalue_edit_view in 'views\enumvalue_edit_view.pas' {EnumValueEditView},
  enum_edit_cmd in 'commands\enum_edit_cmd.pas',
  enumvalue_edit_ctrl in 'controllers\enumvalue_edit_ctrl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
