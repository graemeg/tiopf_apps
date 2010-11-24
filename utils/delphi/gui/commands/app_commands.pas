unit app_commands;

interface
uses
  SysUtils
  ,Classes
  ,tiObject
  ,mvc_base
  ,app_mdl
  ,app_ctrl
  ,mapper_mainform
  ,ComCtrls
  ;

type

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  

  {: App command which listens for standard TNotifyEvent. }
  TCmdNotifyEvent = class(TMVCCommand)
  private
    FSender: TObject;
    procedure SetSender(const Value: TObject);
  protected
    procedure   HandleNotifyEvent(Sender: TObject); virtual;
  public
    property    Sender: TObject read FSender write SetSender;
  end;

  {: Base application command that reintroduces the controller. }
  TAppCommand = class(TCmdNotifyEvent)
  public
    function    Controller: TAppController; reintroduce;
  end;

  {: Command to load a project. }
  TCmdLoadProject = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Save project command object. }
  TCmdSaveProject = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Save project as... command object. }
  TCmdSaveProjectAs = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: close project command object. }
  TCmdCloseProject = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Create a new project. }
  TCmdCreateProject = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Exits the application}
  TCmdDoExistApp = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Command object that executes a group of command objects when a
  a project is loaded. }

  TCmdGrpOnProjectLoaded = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Update the MRL list with the new project filename. }
  TCmdDoUpdateMRUList = class(TAppCommand)
  protected
    procedure   DoExecute; override;
  public
    constructor Create(AController: TMVCController); override;
  end;

  {: Updates the main forms menu to show most recent projects/quick loading. }
  TCmdDoUpdateMRUMenus = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
  public
    constructor Create(AController: TMVCController); override;
  end;

  {: Updates the main form when a project is loaded. }
  TCmdUpdateMainFormProjLoaded = class(TAppCommand)
  protected
    procedure   DoExecute; override;
  public
    constructor Create(AController: TMVCController); override;
  end;

  {: Grouped command that calls a other commands assigned to a group name. }
  TCmdGrpOnProjectClosed = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Grouped command that updates the general labels and such of the main form. }
  TCmdUpdateMainFormProjClose = class(TAppCommand)
  protected
    procedure   DoExecute; override;
  public
    constructor Create(AController: TMVCController); override;
  end;

  {: Listens for unit selection in the units listview and updates the classes
  and enums for that unit. }
  TCmdHandleUnitSelected = class(TAppCommand)
  private
    FItem: TListItem;
    procedure SetItem(const Value: TListItem);
  protected
    property    Item: TListItem read FItem write SetItem;
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
    procedure   HandleNotifyEvent(Sender: TObject; Item: TListItem;
      Selected: Boolean); reintroduce;
  end;

  {: Listen for unit list pop up menu New Unit gesture. }
  TCmdHandleDoAddUnit = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Listen for delete unit gesture and execute. }
  TCmdhandleDoDeleteUnit = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Listens for edit project settings gesture. }
  TCmdHandleDoEditProjectSettings = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Listens for OnBeforeTerminate of App model and cleans up internal controllers
    before shutdown. }
  TCmdHandleOnBeforeAppTerminate = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  // -----------------------------------------------------------------
  //  Class defintions related
  // -----------------------------------------------------------------

  {: Listens for gesture to edit a selected unit class. }
  TCmdHandleDoEditClassDef = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Creates new Class definition }
  TCmdHandleDoCreateClassDef = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Deletes new Class definition }
  TCmdHandleDoDeleteClassDef = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  // -----------------------------------------------------------------
  //  Enumerations
  // -----------------------------------------------------------------

  {: Create a new Enumeration for the unit.}
  TCmdHandleDoCreateEnum = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Edit an Enumeration for the unit.}
  TCmdHandleDoEditEnum = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Delete an Enumeration for the unit.}
  TCmdHandleDoDeleteEnum = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  // -----------------------------------------------------------------
  //  Project genderation
  // -----------------------------------------------------------------

  {: Generate project file(s).}
  TCmdHandleDoGenerateProject = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  // -----------------------------------------------------------------
  //  Other/Misc
  // -----------------------------------------------------------------

  {: Load project from a MRU menu item.}
  TCmdHandleDoProjectMRU = class(TAppCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;







  procedure RegisterCommands(AController: TMVCController);

implementation

uses
  Dialogs
  ,Controls
  ,Menus
  ,tiUtils
  ,event_const
  ,dialog_view
  ,mapper
  ,project_settings_ctrl
  ,proj_settings_view
  ,class_edit_view
  ,class_edit_ctrl
  ,enum_edit_view
  ,enum_edit_ctrl
  ,vcl_controllers
  ,app_consts
  ,tiOPFStreamer
  ,tiopf_super_streamer
  ;

procedure RegisterCommands(AController: TMVCController);
begin
  AController.AddCommand(TCmdDoExistApp.Create(AController));
  AController.AddCommand(TCmdCreateProject.Create(AController));
  AController.AddCommand(TCmdLoadProject.Create(AController));
  AController.AddCommand(TCmdSaveProject.Create(AController));
  AController.AddCommand(TCmdSaveProjectAs.Create(AController));
  AController.AddCommand(TCmdCloseProject.Create(AController));
  AController.AddCommand(TCmdGrpOnProjectLoaded.Create(AController));
  AController.AddCommand(TCmdUpdateMainFormProjLoaded.Create(AController));
  AController.AddCommand(TCmdUpdateMainFormProjClose.Create(AController));
  AController.AddCommand(TCmdGrpOnProjectClosed.Create(AController));
  AController.AddCommand(TCmdHandleUnitSelected.Create(AController));
  AController.AddCommand(TCmdHandleDoAddUnit.Create(AController));
  AController.AddCommand(TCmdHandleDoEditProjectSettings.Create(AController));
  AController.AddCommand(TCmdhandleDoDeleteUnit.Create(AController));
  AController.AddCommand(TCmdHandleOnBeforeAppTerminate.Create(AController));
  AController.AddCommand(TCmdHandleDoEditClassDef.Create(AController));
  AController.AddCommand(TCmdHandleDoCreateClassDef.Create(AController));
  AController.AddCommand(TCmdHandleDoDeleteClassDef.Create(AController));
  AController.AddCommand(TCmdCreateProject.Create(AController));
  AController.AddCommand(TCmdHandleDoCreateEnum.Create(AController));
  AController.AddCommand(TCmdHandleDoEditEnum.Create(AController));
  AController.AddCommand(TCmdHandleDoDeleteEnum.Create(AController));
  AController.AddCommand(TCmdHandleDoGenerateProject.Create(AController));
  AController.AddCommand(TCmdDoUpdateMRUMenus.Create(AController));
  AController.AddCommand(TCmdDoUpdateMRUList.Create(AController));
  AController.AddCommand(TCmdHandleDoProjectMRU.Create(AController));
end;


{ TAppCommand }

function TAppCommand.Controller: TAppController;
begin
  result := inherited Controller as TAppController;
end;

{ TCmdLoadProject }

procedure TCmdNotifyEvent.HandleNotifyEvent(Sender: TObject);
begin
  if Enabled then
    begin
      self.Sender := Sender;
      Execute;
    end;
end;

{ TCmdLoadProject }

procedure TCmdLoadProject.DoAddListeners;
var
  lCtrl: TAppController;
  lView: TMainForm;
begin
  lCtrl := Controller;
  if lCtrl <> nil then
    begin
      lView := lCtrl.View;
      if lView <> nil then
        begin
          lView.mnuOpen.OnClick := HandleNotifyEvent;
        end;
    end;
end;

procedure TCmdLoadProject.DoExecute;
var
  lDialog: TOpenDialog;
begin
  lDialog := TOpenDialog.Create(nil);
  try
    lDialog.Filter := 'XML files (.xml)|*.xml';
    lDialog.DefaultExt := 'xml';
    if Controller.Model.LastDirectoryUsed <> '' then
      lDialog.InitialDir := Controller.Model.LastDirectoryUsed
    else
      lDialog.InitialDir := ExtractFilePath(ParamStr(0));
    if lDialog.Execute then
      begin
        if not FileExists(lDialog.FileName) then
          raise Exception.Create(ClassName + '.DoExecute: File is does not exist');

        Controller.Model.LoadProject(lDialog.FileName);

      end;
  finally
    lDialog.Free;
  end;
end;

procedure TCmdLoadProject.DoRemoveListeners;
begin
  Controller.View.mnuOpen.OnClick := nil;
end;

{ TCmdSaveProject }

procedure TCmdSaveProject.DoAddListeners;
begin
  Controller.View.mnuSave.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdSaveProject.DoExecute;
begin
  if Controller.Model.State = mpsClosed then
    raise Exception.Create(ClassName + '.DoExecute: No project open');
  Controller.Model.SaveProject;
end;

procedure TCmdSaveProject.DoRemoveListeners;
begin
  Controller.View.mnuSave.OnClick := nil;
end;

{ TCmdSaveProjectAs }

procedure TCmdSaveProjectAs.DoAddListeners;
begin
  Controller.View.mnuSaveAs.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdSaveProjectAs.DoExecute;
var
  lDialog: TSaveDialog;
begin
  lDialog := TSaveDialog.Create(nil);
  try
    lDialog.Filter := 'XML Files|*.xml';
    if Controller.Model.LastDirectoryUsed <> '' then
      lDialog.InitialDir := Controller.Model.LastDirectoryUsed
    else
      lDialog.InitialDir := ExtractFilePath(ParamStr(0));
    if lDialog.Execute then
      begin
        Controller.Model.SaveProjectAs(lDialog.FileName);
      end;
  finally
    lDialog.Free;
  end;
end;

procedure TCmdSaveProjectAs.DoRemoveListeners;
begin
  Controller.View.mnuSaveAs.OnClick := nil;
end;

{ TCmdCloseProject }

procedure TCmdCloseProject.DoAddListeners;
begin
  Controller.View.mnuClose.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdCloseProject.DoExecute;
begin
  Controller.Model.CloseProject;
end;

procedure TCmdCloseProject.DoRemoveListeners;
begin
  Controller.View.mnuClose.OnClick := nil;
end;

{ TCmdGrpOnProjectLoaded }

procedure TCmdGrpOnProjectLoaded.DoAddListeners;
begin
  Controller.Model.OnProjectLoaded := Self.HandleNotifyEvent;
end;

procedure TCmdGrpOnProjectLoaded.DoExecute;
begin
  Controller.Commands.ExecuteGroup(ON_PROJ_LOADED);
end;

procedure TCmdGrpOnProjectLoaded.DoRemoveListeners;
begin
  Controller.Model.OnProjectLoaded := nil;
end;

procedure TCmdNotifyEvent.SetSender(const Value: TObject);
begin
  FSender := Value;
end;

{ TCmdUpdateMainFormInfo }

constructor TCmdUpdateMainFormProjClose.Create(AController: TMVCController);
begin
  inherited;
  Group := PROJ_CLOSED;
end;

procedure TCmdUpdateMainFormProjClose.DoExecute;
var
  lProjLoaded: Boolean;
begin
  lProjLoaded := Controller.Model.State <> mpsClosed;

  with Controller.View do
    begin

      // Menus -->
      mnuSave.Enabled := lProjLoaded;
      mnuSaveAs.Enabled := lProjLoaded;
      mnuClose.Enabled := lProjLoaded;
      mnuAddUnit.Enabled := lProjLoaded;
      mnuUnitChangeName.Enabled := lProjLoaded;
      mnuDeleteUnit.Enabled := lProjLoaded;
      mnuNewClass.Enabled := lProjLoaded;
      mnuEditClass.Enabled := lProjLoaded;
      mnuDeleteClass.Enabled := lProjLoaded;
      mnuSettings.Enabled := lProjLoaded;
      mnuGenerate.Enabled := lProjLoaded;
      mnuOpen.Enabled := True;
      mnuNewProject.Enabled := True;
      mnuNewEnum.Enabled := lProjLoaded;
      mnuEditEnum.Enabled := lProjLoaded;
      mnuDeleteEnum.Enabled := lProjLoaded;

      // General -->
      CurrentUnitLabel.Caption := '';
      tsClasses.Caption := 'Classes';
      tsEnums.Caption := 'Enumerations';
      // status bar
      statMain.SimpleText := '';
    end;

  Controller.View.Caption := 'Mapping Designer';

end;

{ TCmdHandleUnitSelected }

procedure TCmdHandleUnitSelected.DoAddListeners;
begin
  Controller.View.lvUnits.OnSelectItem := self.HandleNotifyEvent;
end;

procedure TCmdHandleUnitSelected.DoExecute;
begin
  if Controller.View.lvUnits.Selected = nil then
    exit;

  Controller.Model.CurrentUnit :=
    Controller.Model.Project.Units.Items[Controller.View.lvUnits.Selected.Index];
  Controller.Model.UpdateUnitClasses;
  Controller.Model.UpdateUnitEnums;

  Controller.View.CurrentUnitLabel.Caption := 'Current Unit: ' +
    Controller.Model.CurrentUnit.Name;

  Controller.View.tsClasses.Caption := 'Classes (' +
    IntToStr(Controller.Model.CurrentClasses.Count) + ')';
  Controller.View.tsEnums.Caption := 'Enumerations (' +
    IntToStr(Controller.Model.CurrentEnums.Count) + ')';
end;

procedure TCmdHandleUnitSelected.DoRemoveListeners;
begin
  Controller.View.lvUnits.OnSelectItem := nil;
end;

procedure TCmdHandleUnitSelected.HandleNotifyEvent(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  FItem := Item;
  inherited HandleNotifyEvent(Sender);
end;

procedure TCmdHandleUnitSelected.SetItem(const Value: TListItem);
begin
  FItem := Value;
end;

{ TCmdHandleDoAddUnit }

procedure TCmdHandleDoAddUnit.DoAddListeners;
begin
  Controller.View.mnuAddUnit.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdHandleDoAddUnit.DoExecute;
var
  lVal: string;
  lNewUnit: TMapUnitDef;
begin
  lVal := 'New Unit Name';

  if TDialogView.GetString(lVal, 'Enter new unit name') then
    begin
      // Get rid of spaces if any
      if Pos(' ', lVal) > 0 then
        lVal := StringReplace(lVal, ' ', '_', [rfReplaceAll]);
      // Is there one already present?
      lNewUnit := TMapUnitDef(Controller.Model.Project.Units.FindByName(lVal));
      if lNewUnit <> nil then
        raise Exception.Create(ClassName + '.DoExecute: Attempt to add duplicate unit "' +
          lNewUnit.Name + '".');

      lNewUnit := TMapUnitDef.CreateNew;
      lNewUnit.Name := lVal;
      Controller.Model.Project.Units.Add(lNewUnit);
      Controller.Model.Project.Units.NotifyObservers;
    end;

end;

procedure TCmdHandleDoAddUnit.DoRemoveListeners;
begin
  Controller.View.mnuAddUnit.OnClick := nil;
end;

{ TCmdGrpOnProjectClosed }

procedure TCmdGrpOnProjectClosed.DoAddListeners;
begin
  Controller.Model.OnProjectUnloaded := self.HandleNotifyEvent;
end;

procedure TCmdGrpOnProjectClosed.DoExecute;
begin
  Controller.Commands.ExecuteGroup(PROJ_CLOSED);
end;

procedure TCmdGrpOnProjectClosed.DoRemoveListeners;
begin
  Controller.Model.OnProjectUnloaded := nil;
end;

{ TCmdUpdateMainFormProjLoaded }

constructor TCmdUpdateMainFormProjLoaded.Create(AController: TMVCController);
begin
  inherited;
  Group := ON_PROJ_LOADED;
end;

procedure TCmdUpdateMainFormProjLoaded.DoExecute;
var
  lProjLoaded: Boolean;
begin
  lProjLoaded := Controller.Model.State <> mpsClosed;

  with Controller.View do
    begin
      mnuSave.Enabled := lProjLoaded;
      mnuSaveAs.Enabled := lProjLoaded;
      mnuClose.Enabled := lProjLoaded;
      mnuAddUnit.Enabled := lProjLoaded;
      mnuUnitChangeName.Enabled := lProjLoaded;
      mnuDeleteUnit.Enabled := lProjLoaded;
      mnuNewClass.Enabled := lProjLoaded;
      mnuEditClass.Enabled := lProjLoaded;
      mnuDeleteClass.Enabled := lProjLoaded;
      mnuSettings.Enabled := lProjLoaded;
      mnuGenerate.Enabled := lProjLoaded;
      mnuOpen.Enabled := lProjLoaded;
      mnuNewProject.Enabled := lProjLoaded;
      mnuNewEnum.Enabled := lProjLoaded;
      mnuEditEnum.Enabled := lProjLoaded;
      mnuDeleteEnum.Enabled := lProjLoaded;
      CurrentUnitLabel.Caption := '';

      statMain.SimpleText := self.Controller.Model.Project.FileName;
    end;

  Controller.View.Caption := 'Mapping Designer [' + Controller.Model.Project.ProjectName + ']';
end;

{ TCmdHandleDoEditProjectSettings }

procedure TCmdHandleDoEditProjectSettings.DoAddListeners;
begin
  Controller.View.mnuSettings.OnClick := Self.HandleNotifyEvent;
end;

procedure TCmdHandleDoEditProjectSettings.DoExecute;
var
  lCtrl: TProjectSettingsController;
  lView: TProjectSettingsView;
begin
  lView := TProjectSettingsView.Create(nil);
  lCtrl := TProjectSettingsController.Create(Controller.Model.Project,
      lView);
  try
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;
end;

procedure TCmdHandleDoEditProjectSettings.DoRemoveListeners;
begin
  Controller.View.mnuSettings.OnClick := nil;
end;

{ TCmdhandleDoDeleteUnit }

procedure TCmdhandleDoDeleteUnit.DoAddListeners;
begin
  Controller.View.mnuDeleteUnit.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdhandleDoDeleteUnit.DoExecute;
var
  lListCtrl:  TListViewController;
  lUnit: TMapUnitDef;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('units_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lUnit := TMapUnitDef(lListCtrl.SelectedItem);

  if MessageDlg('Delete Unit: ' + lUnit.Name + '?', mtConfirmation, [mbYes,mbNo], 0) = mrNo then
    exit;

  Controller.Model.Project.Units.Extract(lUnit);
  Controller.Model.Project.Units.NotifyObservers;

  lUnit.Free;

end;

procedure TCmdhandleDoDeleteUnit.DoRemoveListeners;
begin
  Controller.View.mnuDeleteUnit.OnClick := nil;
end;

{ TCmdHandleOnBeforeAppTerminate }

procedure TCmdHandleOnBeforeAppTerminate.DoAddListeners;
begin
  Controller.Model.OnBeforeAppTerminate := self.HandleNotifyEvent;
end;

procedure TCmdHandleOnBeforeAppTerminate.DoExecute;
begin
  Controller.Controllers.ChangeAllActive(False);
  controller.Controllers.Clear;
end;

procedure TCmdHandleOnBeforeAppTerminate.DoRemoveListeners;
begin
  Controller.Model.OnBeforeAppTerminate := nil;
end;

{ TCmdHandleDoEditClassDef }

procedure TCmdHandleDoEditClassDef.DoAddListeners;
begin
  Controller.View.mnuEditClass.OnClick := self.HandleNotifyEvent;
  Controller.View.lvClasses.OnDblClick := self.HandleNotifyEvent;
end;

procedure TCmdHandleDoEditClassDef.DoExecute;
var
  lListCtrl: TListViewController;
  lClassDef: TMapClassDef;
  lEditCtrl: TClassEditController;
  lView: TClassEditView;
begin

  if Controller.Model.CurrentUnit = nil then
    exit;

  lListCtrl := TListViewController(Controller.Controllers.FindByName('classes_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lClassDef := TMapClassDef(lListCtrl.SelectedItem);

  lView := TClassEditView.Create(nil);
  lEditCtrl := TClassEditController.Create(lClassDef, lView);
  try
    lEditCtrl.ParentController := Controller;
    lEditCtrl.Init;
    lEditCtrl.Active := True;
    lEditCtrl.Active := False;
  finally
    lEditCtrl.Free;
  end;

  lClassDef.NotifyObservers;

end;

procedure TCmdHandleDoEditClassDef.DoRemoveListeners;
begin
  Controller.View.mnuEditClass.OnClick := nil;
  Controller.View.lvClasses.OnDblClick := nil;
end;

{ TCmdHandleDoCreateClassDef }

procedure TCmdHandleDoCreateClassDef.DoAddListeners;
begin
  Controller.View.mnuNewClass.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdHandleDoCreateClassDef.DoExecute;
var
  lCtrl: TClassEditController;
  lView: TClassEditView;
  lClassDef: TMapClassDef;
  lSelected: Boolean;
begin

  if Controller.Model.CurrentUnit = nil then
    exit;

  lView := TClassEditView.Create(nil);
  lClassDef := TMapClassDef.Create;
  lCtrl := TClassEditController.Create(lClassDef, lView);
  try
    lClassDef.BaseClassParent := 'TtiObject';
    lClassDef.BaseClassName := 'TMyNewClass';
    lClassDef.ClassMapping.TableName := 'db_table_name_here';
    lClassDef.ClassMapping.PKName := 'OID';
    lClassDef.ClassMapping.PKField := 'OID';
    Controller.Model.CurrentUnit.UnitClasses.Add(lClassDef);
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  Controller.Model.UpdateUnitClasses;
  Controller.Model.CurrentClasses.NotifyObservers;

  lSelected := Controller.View.lvUnits.Selected <> nil;

  Controller.View.lvUnits.OnSelectItem(Controller.View.lvUnits,
    Controller.View.lvUnits.Selected, lSelected);


end;

procedure TCmdHandleDoCreateClassDef.DoRemoveListeners;
begin
  Controller.View.mnuNewClass.OnClick := nil;
end;

{ TCmdHandleDoDeleteClassDef }

procedure TCmdHandleDoDeleteClassDef.DoAddListeners;
begin
  Controller.View.mnuDeleteClass.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdHandleDoDeleteClassDef.DoExecute;
var
  lClassDef: TMapClassDef;
  lListCtrl: TListViewController;
  lMsg: string;
begin
  if Controller.Model.CurrentUnit = nil then
    exit;

  lListCtrl := TListViewController(Controller.Controllers.FindByName('classes_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lClassDef := TMapClassDef(lListCtrl.SelectedItem);
  lMsg := Format(CONFIRM_PROP_DELETE, [lClassDef.BaseClassName]);
  if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], Controller.View.Handle) = mrNo then
    exit;

  Controller.Model.CurrentClasses.Extract(lClassDef);
  Controller.Model.CurrentUnit.UnitClasses.Extract(lClassDef);

  lClassDef.Free;

  Controller.Model.CurrentClasses.NotifyObservers;

end;

procedure TCmdHandleDoDeleteClassDef.DoRemoveListeners;
begin
  Controller.View.mnuDeleteClass.OnClick := nil;
end;

{ TCmdCreateProject }

procedure TCmdCreateProject.DoAddListeners;
begin
  Controller.View.mnuNewProject.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdCreateProject.DoExecute;
var
  lDialog: TSaveDialog;
begin
  lDialog := TSaveDialog.Create(nil);
  try
    lDialog.Filter := 'XML files (.xml)|*.xml';
    lDialog.DefaultExt := 'xml';
    lDialog.Options := lDialog.Options + [ofOverwritePrompt];
    lDialog.InitialDir := ExtractFilePath(ParamStr(0));
    if lDialog.Execute then
      begin
        Controller.Model.CreateNewProject(lDialog.FileName);
      end;
  finally
    lDialog.Free;
  end;

end;

procedure TCmdCreateProject.DoRemoveListeners;
begin
  Controller.View.mnuNewProject.OnClick := nil;
end;

{ TCmdHandleDoCreateEnum }

procedure TCmdHandleDoCreateEnum.DoAddListeners;
begin
  Controller.View.mnuNewEnum.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdHandleDoCreateEnum.DoExecute;
var
  lView: TEnumEditView;
  lCtrl: TEnumEditController;
  lEnum: TMapEnum;
begin
  lView := TEnumEditView.Create(nil);
  lEnum := TMapEnum.Create;
  lCtrl := TEnumEditController.Create(lEnum, lView);
  try
    lEnum.EnumName := 'My_New_Enum';
    Controller.Model.CurrentUnit.UnitEnums.Add(lEnum);
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := false;
  finally
    lCtrl.Free;
  end;

  Controller.Model.UpdateUnitEnums;
  Controller.Model.CurrentEnums.NotifyObservers;

  Controller.View.lvUnits.OnSelectItem(Controller.View.lvUnits,
    Controller.View.lvUnits.Selected, Controller.View.lvUnits.Selected <> nil);

end;

procedure TCmdHandleDoCreateEnum.DoRemoveListeners;
begin
  Controller.View.mnuNewEnum.OnClick := nil;
end;

{ TCmdHandleDoEditEnum }

procedure TCmdHandleDoEditEnum.DoAddListeners;
begin
  Controller.View.mnuEditEnum.OnClick := self.HandleNotifyEvent;
  Controller.View.lvEnums.OnDblClick := self.HandleNotifyEvent;
end;

procedure TCmdHandleDoEditEnum.DoExecute;
var
  lListCtrl: TListViewController;
  lEnum: TMapEnum;
  lEditCtrl: TEnumEditController;
  lView: TEnumEditView;
begin

  if Controller.Model.CurrentUnit = nil then
    exit;

  lListCtrl := TListViewController(Controller.Controllers.FindByName('enums_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lEnum := TMapEnum(lListCtrl.SelectedItem);

  lView := TEnumEditView.Create(nil);
  lEditCtrl := TEnumEditController.Create(lEnum, lView);
  try
    lEditCtrl.Init;
    lEditCtrl.Active := True;
    lEditCtrl.Active := False;
  finally
    lEditCtrl.Free;
  end;

  lEnum.NotifyObservers;

end;

procedure TCmdHandleDoEditEnum.DoRemoveListeners;
begin
  Controller.View.mnuEditEnum.OnClick := nil;
  Controller.View.lvEnums.OnDblClick := nil;
end;

{ TCmdHandleDoDeleteEnum }

procedure TCmdHandleDoDeleteEnum.DoAddListeners;
begin
  Controller.View.mnuDeleteEnum.OnClick := Self.HandleNotifyEvent;
end;

procedure TCmdHandleDoDeleteEnum.DoExecute;
var
  lEnum: TMapEnum;
  lListCtrl: TListViewController;
  lMsg: string;
begin
  if Controller.Model.CurrentUnit = nil then
    exit;

  lListCtrl := TListViewController(Controller.Controllers.FindByName('enums_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lEnum := TMapEnum(lListCtrl.SelectedItem);
  lMsg := Format(CONFIRM_PROP_DELETE, [lEnum.EnumName]);
  if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], Controller.View.Handle) = mrNo then
    exit;

  Controller.Model.CurrentEnums.Extract(lEnum);
  Controller.Model.CurrentUnit.UnitEnums.Extract(lEnum);

  lEnum.Free;

  Controller.Model.CurrentEnums.NotifyObservers;

  Controller.View.lvUnits.OnSelectItem(Controller.View.lvUnits,
    Controller.View.lvUnits.Selected, Controller.View.lvUnits.Selected <> nil);
end;

procedure TCmdHandleDoDeleteEnum.DoRemoveListeners;
begin
  Controller.View.mnuDeleteEnum.OnClick := nil;
end;

{ TCmdHandleDoGenerateProject }

procedure TCmdHandleDoGenerateProject.DoAddListeners;
begin
  Controller.View.mnuGenerate.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdHandleDoGenerateProject.DoExecute;
begin
  Controller.Model.WriteProject;
end;

procedure TCmdHandleDoGenerateProject.DoRemoveListeners;
begin
  Controller.View.mnuGenerate.OnClick := nil;
end;

{ TCmdDoExistApp }

procedure TCmdDoExistApp.DoAddListeners;
begin
  Controller.View.mnuExit.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoExistApp.DoExecute;
begin
  Controller.View.Close;
end;

procedure TCmdDoExistApp.DoRemoveListeners;
begin
  Controller.View.mnuExit.OnClick := nil;
end;

{ TCmdDoUpdateMRU }

constructor TCmdDoUpdateMRUMenus.Create(AController: TMVCController);
begin
  inherited;
  //Group := ON_PROJ_LOADED;
end;

procedure TCmdDoUpdateMRUMenus.DoAddListeners;
begin
  Controller.Model.OnAppLoaded := Self.HandleNotifyEvent;
end;

procedure TCmdDoUpdateMRUMenus.DoExecute;
var
  lSL: TStringList;
  lItem: TMenuItem;
  lSubItem: TMenuItem;
  lCtr: Integer;
  lDir: string;
  lFile: string;
begin

  lItem := Controller.View.mnuMRU;

  lDir := tiGetUserLocalAppDataDir('mapper');
  if not DirectoryExists(lDir) then
    CreateDir(lDir);

  lFile := lDir + PathDelim + 'mru.txt';

  for lCtr := lItem.Count - 1 downto 0 do
    begin
      lItem.Delete(lCtr);
    end;

  lSL := TStringList.Create;
  try
    // create if not exist already
    if not FileExists(lFile) then
      lSL.SaveToFile(lFile)
    else
      lSL.LoadFromFile(lFile);

    lItem := Controller.View.mnuMRU;
    for lCtr := 0 to lSL.Count - 1 do
      begin
        lSubItem := TMenuItem.Create(lItem);
        lSubItem.Caption := lSL[lCtr];
        lSubItem.OnClick := Controller.HandleMRLClick;
        lItem.Add(lSubItem);
      end;
  finally
    lSL.Free;
  end;

end;

{ TCmdDoUpdateMRUList }

constructor TCmdDoUpdateMRUList.Create(AController: TMVCController);
begin
  inherited;
  Group := ON_PROJ_LOADED;
end;

procedure TCmdDoUpdateMRUList.DoExecute;
var
  lSL: TStringList;
  lCtr: Integer;
  lDir: string;
  lFile: string;
  lPOS: Integer;
begin

  lDir := tiGetUserLocalAppDataDir('mapper');
  if not DirectoryExists(lDir) then
    CreateDir(lDir);

  lFile := lDir + PathDelim + 'mru.txt';

  lSL := TStringList.Create;
  try
    // create if not exist already
    if not FileExists(lFile) then
      lSL.SaveToFile(lFile)
    else
      lSL.LoadFromFile(lFile);

    lPOS := lSL.IndexOf(Controller.Model.Project.FileName);
    if lPOS >= 0 then
      begin
        if lPOS <> 0 then
          begin
            lSL.Delete(lPOS);
            lSL.Insert(0, Controller.Model.Project.FileName);
          end;
      end
    else
      begin
        lSL.Insert(0, Controller.Model.Project.FileName);
      end;

    if lSL.Count > 10 then
      begin
        while lSL.Count > 10 do
          lSL.Delete(lSL.Count -1);

      end;

    lSL.SaveToFile(lFile);
    Controller.Model.OnAppLoaded(Controller.Model);


  finally
    lSL.Free;
  end;

end;

{ TCmdHandleDoProjectMRU }

procedure TCmdHandleDoProjectMRU.DoAddListeners;
begin
  Controller.OnMRLClicked := self.HandleNotifyEvent;
end;

procedure TCmdHandleDoProjectMRU.DoExecute;
begin
  Controller.Model.LoadProject(Controller.SelectedMRU);
end;

procedure TCmdHandleDoProjectMRU.DoRemoveListeners;
begin
  Controller.OnMRLClicked := nil;
end;

end.
