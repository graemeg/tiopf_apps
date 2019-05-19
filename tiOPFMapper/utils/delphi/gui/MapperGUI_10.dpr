program MapperGUI_10;

uses
  {$IFDEF EUREKALOG}
  ExceptionLog,
  {$ENDIF }
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  BaseViewFrm in 'views\BaseViewFrm.pas' {BaseViewForm},
  BaseDialogViewFrm in 'views\BaseDialogViewFrm.pas' {BaseDialogView},
  AppController in 'controllers\AppController.pas',
  AppModel in 'models\AppModel.pas',
  EventsConsts in 'events\EventsConsts.pas',
  AppCommands in 'commands\AppCommands.pas',
  DialogController in 'controllers\DialogController.pas',
  DialogViewFrm in 'views\DialogViewFrm.pas' {DialogView},
  ProjSettingsViewFrm in 'views\ProjSettingsViewFrm.pas' {ProjectSettingsView},
  ProjectOptionsController in 'controllers\ProjectOptionsController.pas',
  ClassEditViewFrm in 'views\ClassEditViewFrm.pas' {ClassEditView},
  ClassEditController in 'controllers\ClassEditController.pas',
  ClassEditCommands in 'commands\ClassEditCommands.pas',
  ClassPropEditViewFrm in 'views\ClassPropEditViewFrm.pas' {ClassPropEditView},
  ClassPropsEditController in 'controllers\ClassPropsEditController.pas',
  PropMapEditViewFrm in 'views\PropMapEditViewFrm.pas' {PropMapEditView},
  PropMapEditController in 'controllers\PropMapEditController.pas',
  AppConsts in 'AppConsts.pas',
  ValidatorEditViewFrm in 'views\ValidatorEditViewFrm.pas' {ValidatorEditView},
  ValidatorEditController in 'controllers\ValidatorEditController.pas',
  SelectEditViewFrm in 'views\SelectEditViewFrm.pas' {SelectEditView},
  SelectEditController in 'controllers\SelectEditController.pas',
  ParamEditViewFrm in 'views\ParamEditViewFrm.pas' {ParamEditView},
  ParamEditController in 'controllers\ParamEditController.pas',
  EnumEditViewFrm in 'views\EnumEditViewFrm.pas' {EnumEditView},
  SelectEditCommands in 'commands\SelectEditCommands.pas',
  EnumEditController in 'controllers\EnumEditController.pas',
  EnumValueEditViewFrm in 'views\EnumValueEditViewFrm.pas' {EnumValueEditView},
  EnumEditCommands in 'commands\EnumEditCommands.pas',
  EnumValueEditController in 'controllers\EnumValueEditController.pas',
  mvc_base in '..\..\..\..\..\timvc\src\mvc_base.pas',
  mvc_criteria in '..\..\..\..\..\timvc\src\mvc_criteria.pas',
  mvc_events in '..\..\..\..\..\timvc\src\mvc_events.pas',
  vcl_controllers in '..\..\..\..\..\timvc\src\vcl_controllers.pas',
  widget_controllers in '..\..\..\..\..\timvc\src\widget_controllers.pas',
  BaseOkCancelDialogController in 'controllers\BaseOkCancelDialogController.pas',
  BaseOptionsDialogViewFrm in 'views\BaseOptionsDialogViewFrm.pas' {BaseOptionsDialogView},
  BaseOptionsDialogController in 'controllers\BaseOptionsDialogController.pas',
  ProjectOptionsViewFrm in 'views\ProjectOptionsViewFrm.pas' {ProjectOptionsView},
  ProjectGeneralOptionsViewFra in 'views\ProjectGeneralOptionsViewFra.pas' {ProjectGeneralOptionsView: TFrame},
  ProjectCodeGenerationOptionsViewFra in 'views\ProjectCodeGenerationOptionsViewFra.pas' {ProjectCodeGenerationOptionsView: TFrame},
  ProjectDatabaseOptionsViewFra in 'views\ProjectDatabaseOptionsViewFra.pas' {ProjectDatabaseOptionsView: TFrame},
  ProjectGeneralOptionsController in 'controllers\ProjectGeneralOptionsController.pas',
  ProjectCodeGenerationOptionsController in 'controllers\ProjectCodeGenerationOptionsController.pas',
  ProjectDatabaseOptionsController in 'controllers\ProjectDatabaseOptionsController.pas',
  BaseFrameController in 'controllers\BaseFrameController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
