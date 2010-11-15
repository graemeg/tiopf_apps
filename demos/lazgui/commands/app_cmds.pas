unit app_cmds;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,Dialogs
  ,Controls
  ,mvc_base
  ,app_model
  ,app_ctl
  ,event_const
  ,tiUtils
  ;


type

  {: Base command with overridden Controller function as TAppController. }
  TBaseAppCtrlCmd = class(TMVCCommand)
  public
    function Controller: TAppController; reintroduce;
    destructor Destroy; override;
  end;

  {: Add Unit Command }
  TAddUnitCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Add Unit Command }
  TDeleteUnitCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Change Unit Name command. }
  TChangeUnitNameCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Locks down gui controls when project is closed. }
  TLockDownGUICmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Enabled (unlocks) gui. }
  TUnlockGUICmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Prompt for project file name and open project. }
  THandleDoOpenProjectCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handle View signal to close current project. }
  THandleDoCloseProjectCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles signal from view that new unit is selected. }
  THandleUnitSelectedCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles signal to "Save Project As..." . }
  THandleSaveAsCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles signal to save project. }
  THandleSaveCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles updating the GUI with information about the currently loaded project. }
  TUpdateGUIProjectInfoCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles signal from view to edit the project settings. }
  THandleEditProjSettingsCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles Handles removal of a subcontroller. }
  THandleCloseSubController = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles signal from View to close application. }
  THandleCloseApplicationCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles signal from View to create a new class definition. }
  THandleCreateClassDefCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles signal from View to edit current class definition. }
  THandleEditClassDefCmd = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Handles signal from View to shut down application. }
  THandleAppShutDown = class(TBaseAppCtrlCmd)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  end;

  {: Registers commands with the application controller. }
  procedure RegisterApplicationCommands(AController: TAppController);

implementation

uses
  mapper
  ,lcl_controllers
  ,input_query
  ,proj_settings_ctrl
  ,project_info_view
  ,edit_class_ctrl
  ,edit_class_view
  ,app_events
  ;

procedure RegisterApplicationCommands(AController: TAppController);
begin
  AController.AddCommand(TAddUnitCmd.Create(AController));
  AController.AddCommand(TDeleteUnitCmd.Create(AController));
  AController.AddCommand(TDeleteUnitCmd.Create(AController));
  AController.AddCommand(TLockDownGUICmd.Create(AController));
  AController.AddCommand(TUnlockGUICmd.Create(AController));
  AController.AddCommand(THandleDoOpenProjectCmd.Create(AController));
  AController.AddCommand(THandleDoCloseProjectCmd.Create(AController));
  AController.AddCommand(THandleUnitSelectedCmd.Create(AController));
  AController.AddCommand(THandleSaveAsCmd.Create(AController));
  AController.AddCommand(TUpdateGUIProjectInfoCmd.Create(AController));
  AController.AddCommand(THandleEditProjSettingsCmd.Create(AController));
  AController.AddCommand(THandleCloseSubController.Create(AController));
  AController.AddCommand(THandleCloseApplicationCmd.Create(AController));
  AController.AddCommand(THandleSaveCmd.Create(AController));
  AController.AddCommand(THandleEditClassDefCmd.Create(AController));
  AController.AddCommand(THandleAppShutDown.Create(AController));
end;


{ TAddUnitCmd }

procedure TAddUnitCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_ADD_UNIT);
end;

procedure TAddUnitCmd.DoExecute;
var
  lUnit: TMapUnitDef;
  lName: string;
begin

  if CurrentEvent.Name <> DO_ADD_UNIT then
    exit;

  if not TInputQuery.GetInputAsString(lName, 'Enter a new Unit Name.', dtInput) then
    exit;

  if lName = '' then
    begin
      MessageDlg(ClassName + '.DoExecute: Invalid name', mtError, [mbOK], 0);
      exit;
    end;

  if Pos(' ', lName) > 0 then
    begin
      MessageDlg(ClassName + '.DoExecute: Attempt to save unit with spaces in the name', mtError, [mbOK], 0);
      exit;
    end;

  lUnit := Controller.Model.Project.Units.FindByName(lName);
  if lUnit <> nil then
    begin
      MessageDlg(ClassName + '.DoExecute: Duplicate unit name: ' + lName, mtError, [mbOK], 0);
    end;

  lUnit := TMapUnitDef.Create;
  lUnit.UnitName := lName;
  Controller.Model.Project.Units.Add(lUnit);

end;

{ TBaseAppCtrlCmd }

function TBaseAppCtrlCmd.Controller: TAppController;
begin
  result := inherited Controller as TAppController;
end;

destructor TBaseAppCtrlCmd.Destroy;
begin
  gEventManager.RemoveListenersByTarget(self);
  inherited Destroy;
end;

{ TDeleteUnitCmd }

procedure TDeleteUnitCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, Controller.View, DO_DELETE_UNIT);
end;

procedure TDeleteUnitCmd.DoExecute;
var
  lUnit: TMapUnitDef;
  lName: string;
  lProject: TMapProject;
  lCtrl: TListViewController;
begin
  lProject := Controller.Model.Project;

  lCtrl := TListViewController(Controller.Controllers.FindByModel(Controller.Model.Project.Units));
  Assert(lCtrl<> nil, ClassName + '.DoExecute: Controller not found');

  if lCtrl.SelectedItem = nil then
    exit;

  lUnit :=  TMapUnitDef(lCtrl.SelectedItem);

  if lUnit <> nil then
    begin
      lProject.Units.Remove(lUnit);
      lProject.NotifyObservers;
    end;

end;

{ TChangeUnitNameCmd }

procedure TChangeUnitNameCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, Controller.View, DO_CHANGE_UNIT_NAME);
end;

procedure TChangeUnitNameCmd.DoExecute;
var
  lUnit: TMapUnitDef;
  lName: string;
  lProject: TMapProject;
  lListCtrl: TListViewController;
begin
  lProject := Controller.Model.Project;

  lListCtrl := TListViewController(Controller.Controllers.FindByName('units'));
  if lListCtrl = nil then
    raise Exception.Create(ClassName + '.DoExecute: Units controller not found');

  lUnit :=  TMapUnitDef(lListCtrl.SelectedItem);

  if lUnit <> nil then
    begin
      lUnit.UnitName := lName;
      lUnit.NotifyObservers;
    end;

end;

{ TLockDownGUICmd }

procedure TLockDownGUICmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, PROJ_CLOSED);
end;

procedure TLockDownGUICmd.DoExecute;
begin
  // menus
  Controller.View.mnuCloseProj.Enabled := false;
  Controller.View.mnuSaveProj.Enabled := false;
  Controller.View.mnuSaveAs.Enabled := false;
  Controller.View.mnuProjSettings.Enabled := false;
  // menu - classes
  Controller.View.popClasses.Items.Enabled := false;
  // menu - enums
  Controller.View.popUnits.Items.Enabled := false;

  //Controller.View.mnuOpenProj.Enabled := true;
  Controller.View.mnuExit.Enabled := true;

  // Listview clean up
  Controller.View.lvClasses.Items.Clear;
  Controller.View.lvEnums.Items.Clear;

  // Labels updated
  Controller.View.CurrentUnitLabel.Caption := '';
end;

{ TUnlockGUICmd }

procedure TUnlockGUICmd.DoAddListeners;
begin
  gEventManager.AddListener(self, Controller.Model, PROJ_LOADED);
end;

procedure TUnlockGUICmd.DoExecute;
begin
  Controller.View.popUnits.Items.Enabled := true;
  Controller.View.mnuCloseProj.Enabled := true;
  Controller.View.mnuSaveProj.Enabled := true;
  Controller.View.mnuSaveAs.Enabled := true;
  Controller.View.mnuProjSettings.Enabled := true;
  // menu - classes
  Controller.View.popClasses.Items.Enabled := true;
end;

{ THandleDoOpenProjectCmd }

procedure THandleDoOpenProjectCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_LOAD_PROJECT);
end;

procedure THandleDoOpenProjectCmd.DoExecute;
var
  lOD: TOpenDialog;
begin

  lOD := TOpenDialog.Create(nil);
  try
    lOD.Filter := 'XML Files|*.xml';
    if lOD.Execute then
      begin
        if Controller.Model.State = mpsLoaded then
          Controller.Model.CloseProject;

        Controller.Model.LoadProject(lOD.FileName);

      end;
  finally
    lOD.Free;
  end;
end;

{ THandleDoCloseProjectCmd }

procedure THandleDoCloseProjectCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_CLOSE_PROJECT);
end;

procedure THandleDoCloseProjectCmd.DoExecute;
begin
  if MessageDlg('Close Project?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    exit;
  Controller.Model.CloseProject;
end;

{ THandleUnitSelectedCmd }

procedure THandleUnitSelectedCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, UNIT_SELECTED);
end;

procedure THandleUnitSelectedCmd.DoExecute;
var
  lListCtrl: TListViewController;
  lUnit: TMapUnitDef;
begin


  lListCtrl := Controller.Controllers.FindByName('units') as TListViewController;
  if lListCtrl = nil then
    exit;

  // Delete the selected classes and enums from the selection lists
  // and notify any mediator/controllers to clean up.
  with Controller.Model do
    begin
      CurrentClasses.Clear;
      CurrentClasses.NotifyObservers;
      CurrentEnums.Clear;
      CurrentEnums.NotifyObservers;
    end;

  if lListCtrl.SelectedItem = nil then
    exit;

  // Get the currently selected TMapUnitDef and re-populate the selection lists
  // with classes, enums from the selected unit.
  lUnit := lListCtrl.SelectedItem as TMapUnitDef;

  Controller.Controllers.ChangeGroupActive('units_dependant', false);
  try
    Controller.Model.SelectClassesForUnit(lUnit);
    Controller.Model.SelectEnumsForUnit(lUnit);
  finally;
    Controller.Controllers.ChangeGroupActive('units_dependant', True);
  end;

  // Update the label to indicate currently selected unit on the view.
  Controller.View.UpdateCurrentUnitName('Current Unit: ' + lUnit.UnitName);

  // Ensure that the classes tab is the selected tab
  Controller.View.MainPageControl.ActivePageIndex := 0;

end;

{ THandleSaveAsCmd }

procedure THandleSaveAsCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_SAVE_AS);
end;

procedure THandleSaveAsCmd.DoExecute;
var
  lSD: TSaveDialog;
begin

  lSD := TSaveDialog.Create(nil);
  try
    lSD.Filter := 'XML Files|*.xml';
    lSD.DefaultExt := 'xml';
    if lSD.Execute then
      begin
        Controller.Model.SaveProjectAs(lSD.FileName);
        Controller.Model.CloseProject;
        Controller.Model.LoadProject(lSD.FileName);
      end;
  finally
    lSD.Free;
  end;

end;

{ TUpdateGUIProjectInfoCmd }

procedure TUpdateGUIProjectInfoCmd.DoAddListeners;
begin
  // These listeners are required.
  gEventManager.AddListener(self, nil, PROJ_CLOSED);
  gEventManager.AddListener(self, nil, PROJ_LOADED);
  gEventManager.AddListener(self, nil, PROJ_SAVED);
end;

procedure TUpdateGUIProjectInfoCmd.DoExecute;
begin
  if CurrentEvent.Name = PROJ_LOADED then
    Controller.View.UpdateViewFileName(Controller.Model.Project.FileName)
  else if CurrentEvent.Name = PROJ_CLOSED then
    Controller.View.UpdateViewFileName('No project loaded');
end;

{ THandleEditProjSettingsCmd }

procedure THandleEditProjSettingsCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_EDIT_PROJ_SETTINGS);
end;

procedure THandleEditProjSettingsCmd.DoExecute;
var
  lProjSettingCtrl: TProjectSettingsController;
  lView: TProjectInfoEdit;
begin
  lView := TProjectInfoEdit.Create(nil);
  lProjSettingCtrl := TProjectSettingsController.Create(Controller.Model.Project, lView);
  lProjSettingCtrl.Name := 'project_settings';
  lProjSettingCtrl.Init;
  Controller.Controllers.Add(lProjSettingCtrl);
  lProjSettingCtrl.Active := true;
end;

{ THandleCloseSubController }

procedure THandleCloseSubController.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_CLOSE_SUBCONTROLLER);
end;

procedure THandleCloseSubController.DoExecute;
var
  lCtrl: TMVCController;
begin
  lCtrl := Controller.Controllers.FindByName(TRemoveSubControllerEvent(CurrentEvent).ControllerName);
  lCtrl.Active := false;
  Controller.Controllers.Extract(lCtrl);
  lCtrl.Free;
  Controller.View.BringToFront;
  Controller.View.Show;
end;

{ THandleCloseApplicationCmd }

procedure THandleCloseApplicationCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_CLOSE_APP);
end;

procedure THandleCloseApplicationCmd.DoExecute;
begin
  Controller.View.Close;
end;

{ THandleSaveCmd }

procedure THandleSaveCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_SAVE);
end;

procedure THandleSaveCmd.DoExecute;
begin
  Controller.Model.SaveProject;
end;

{ THandleEditClassDefCmd }

procedure THandleEditClassDefCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_EDIT_CLASS);
end;

procedure THandleEditClassDefCmd.DoExecute;
var
  lView: TClassEditView;
  lCtrl: TClassEditController;
  lListCtrl: TListViewController;
  lClassDef: TMapClassDef;
  lCtrlName: string;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('classes'));

  lClassDef := TMapClassDef(lListCtrl.SelectedItem);

  if lClassDef = nil then
    exit;

  lCtrlName := 'edit' + lClassDef.BaseClassName;
  lCtrl := TClassEditController(Controller.Controllers.FindByName(lCtrlName));
  if lCtrl <> nil then
    begin
      lCtrl.View.Show;
      exit;
    end;

  lView := TClassEditView.Create(nil);
  lCtrl := TClassEditController.Create(lClassDef, lView);
  // use the name of the class being edited to find controller later on
  lCtrl.Name := 'edit' + lClassDef.BaseClassName;
  // use the group name to find them all
  lCtrl.GroupName := 'edit_class';

  lCtrl.Init;
  Controller.Controllers.Add(lCtrl);
  lCtrl.Active := true;
end;

{ THandleCreateClassDefCmd }

procedure THandleCreateClassDefCmd.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, DO_CREATE_CLASS);
end;

procedure THandleCreateClassDefCmd.DoExecute;
begin

end;

{ THandleAppShutDown }

procedure THandleAppShutDown.DoAddListeners;
begin
  gEventManager.AddListener(self, nil, BEFORE_APP_TERM);
end;

procedure THandleAppShutDown.DoExecute;
begin
  Controller.Controllers.ChangeAllActive(False);
  Controller.Controllers.Clear;
end;

end.

