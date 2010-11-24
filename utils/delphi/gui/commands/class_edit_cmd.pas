unit class_edit_cmd;

interface
uses
  SysUtils
  ,Classes
  ,mapper
  ,mvc_base
  ,tiObject
  ,class_edit_view
  ,class_edit_ctrl
  ,app_commands
  ;

type

  TClassEditCommand = class(TCmdNotifyEvent)
  public
    function Controller: TClassEditController; reintroduce;
  end;

  {: Edit Class Property. }
  TCmdDoEditProperty = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Add new property. }
  TCmdDoAddProperty = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Add new property. }
  TCmdDoDeleteProperty = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Sort Properties. }
  TCmdDoSortPropsByName = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;


  {: Add new Mapping. }
  TCmdDoAddPropMapping = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Edit an existing mapping. }
  TCmdDoEditPropMapping = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Delete an existing prop mapping. }
  TCmdDoDeletePropMapping = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Sort prop maps by prop name. }
  TCmdDoSortPropMapsByPropName = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Sort prop maps by field name. }
  TCmdDoSortPropMapsByFieldName = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Create a new validator and add it. }
  TCmdDoAddValidator = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Edit an existing validator. }
  TCmdDoEditValidator = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Deleted an existing validator. }
  TCmdDoDeleteValidator = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Create New Selection }
  TCmdDoCreateSelect = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Edit existing Selection }
  TCmdDoEditSelect = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Delete existing Selection }
  TCmdDoDeleteSelect = class(TClassEditCommand)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  procedure   RegisterClassEditControllerCommands(AController: TMVCController);



implementation
uses
  Dialogs
  ,Controls
  ,classprop_edit_view
  ,classprop_edit_ctrl
  ,propmap_edit_view
  ,propmap_edit_ctrl
  ,validator_edit_view
  ,validator_edit_ctrl
  ,select_edit_view
  ,select_edit_ctrl
  ,vcl_controllers
  ,app_consts
  ;

procedure   RegisterClassEditControllerCommands(AController: TMVCController);
begin
  AController.AddCommand(TCmdDoEditProperty.Create(AController));
  AController.AddCommand(TCmdDoAddProperty.Create(AController));
  AController.AddCommand(TCmdDoDeleteProperty.Create(AController));
  AController.AddCommand(TCmdDoAddPropMapping.Create(AController));
  AController.AddCommand(TCmdDoEditPropMapping.Create(AController));
  AController.AddCommand(TCmdDoDeletePropMapping.Create(AController));
  AController.AddCommand(TCmdDoSortPropsByName.Create(AController));
  AController.AddCommand(TCmdDoSortPropMapsByPropName.Create(AController));
  AController.AddCommand(TCmdDoSortPropMapsByFieldName.Create(AController));
  AController.AddCommand(TCmdDoAddValidator.Create(AController));
  AController.AddCommand(TCmdDoEditValidator.Create(AController));
  AController.AddCommand(TCmdDoDeleteValidator.Create(AController));
  AController.AddCommand(TCmdDoCreateSelect.Create(AController));
  AController.AddCommand(TCmdDoEditSelect.Create(AController));
  AController.AddCommand(TCmdDoDeleteSelect.Create(AController));
end;

{ TCmdDoEditProperty }

procedure TCmdDoEditProperty.DoAddListeners;
begin
  Controller.View.mnuEditProp.OnClick := self.HandleNotifyEvent;
  Controller.View.lvProps.OnDblClick := self.HandleNotifyEvent;
end;

procedure TCmdDoEditProperty.DoExecute;
var
  lCtrl: TClassPropEditController;
  lView: TClassPropEditView;
  lListCtrl: TListViewController;
  lProp: TMapClassProp;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('props_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lProp := TMapClassProp(lListCtrl.SelectedItem);

  lView := TClassPropEditView.Create(nil);
  lCtrl := TClassPropEditController.Create(lProp, lView);
  try

    // If enum type then add extra string to combo box
    if lProp.PropertyType = ptEnum then
      lView.cboPropType.Items.Add(lProp.PropTypeName);

    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  if (Copy(lProp.PropTypeName, 1, 1) = 'T') and (LowerCase(lProp.PropTypeName) <> 'tdatetime') then
    begin
      lProp.PropertyType := ptEnum;
    end
  else
    begin
      lProp.PropertyType := mapper.gStrToPropType(lProp.PropTypeName);
    end;

  lProp.NotifyObservers;

end;

procedure TCmdDoEditProperty.DoRemoveListeners;
begin
  Controller.View.mnuEditProp.OnClick := nil;
end;

{ TClassEditCommand }

function TClassEditCommand.Controller: TClassEditController;
begin
  Result := inherited Controller as TClassEditController;
end;

{ TCmdDoAddProperty }

procedure TCmdDoAddProperty.DoAddListeners;
begin
  Controller.View.mnuAddProp.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoAddProperty.DoExecute;
var
  lNewProp: TMapClassProp;
  lView: TClassPropEditView;
  lCtrl: TClassPropEditController;
begin
  lView := TClassPropEditView.Create(nil);
  lNewProp := TMapClassProp.CreateNew;
  lNewProp.Name := 'Prop_' + IntToStr(Controller.Model.ClassProps.Count);
  lNewProp.PropertyType := ptString;
  lNewProp.PropTypeName := 'String';
  Controller.Model.ClassProps.Add(lNewProp);

  lCtrl := TClassPropEditController.Create(lNewProp, lView);
  try
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  if (Copy(lNewProp.PropTypeName, 1, 1) = 'T') and (LowerCase(lNewProp.PropTypeName) <> 'tdatetime') then
    begin
      lNewProp.PropertyType := ptEnum;
    end
  else
    begin
      lNewProp.PropertyType := mapper.gStrToPropType(lNewProp.PropTypeName);
    end;

  Controller.Model.ClassProps.NotifyObservers;

end;

procedure TCmdDoAddProperty.DoRemoveListeners;
begin
  Controller.View.mnuAddProp.OnClick := nil;
end;

{ TCmdDoDeleteProperty }

procedure TCmdDoDeleteProperty.DoAddListeners;
begin
  Controller.View.mnuDeleteProp.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoDeleteProperty.DoExecute;
var
  lListCtrl: TListViewController;
  lProp: TMapClassProp;
  lMsg: string;
  lMap: TPropMapping;
  lCtr: Integer;
  lVal: TMapValidator;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('props_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lProp := TMapClassProp(lListCtrl.SelectedItem);

  lMsg := Format(CONFIRM_PROP_DELETE, [lProp.Name]);
  if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], Controller.View.Handle) = mrNo then
    exit;

  Controller.Model.ClassProps.Extract(lProp);
  Controller.Model.ClassProps.NotifyObservers;

  // Remove any related mappings to the delete property
  lMap := TPropMapping(Controller.Model.ClassMapping.PropMappings.FindByProps(['PropName'],
    [lProp.Name]));

  if lMap <> nil then
    begin
      Controller.Model.ClassMapping.PropMappings.Extract(lMap);
      lMap.Free;
      Controller.Model.ClassMapping.PropMappings.NotifyObservers;
    end;


  // Remove any validators related to deleted property
  for lCtr := Controller.Model.Validators.Count -1 downto 0 do
    begin
      lVal := Controller.Model.Validators.Items[lCtr];
      if SameText(lVal.ClassProp, lProp.Name) then
        begin
          Controller.Model.Validators.Extract(lVal);
          lVal.Free;
        end;
    end;

  Controller.Model.Validators.NotifyObservers;

  lProp.Free;


end;

procedure TCmdDoDeleteProperty.DoRemoveListeners;
begin
  Controller.View.mnuDeleteProp.OnClick := nil;
end;

{ TCmdDoAddMapping }

procedure TCmdDoAddPropMapping.DoAddListeners;
begin
  Controller.View.mnuNewMap.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoAddPropMapping.DoExecute;
var
  lView: TPropMapEditView;
  lCtrl: TPropMapEditController;
  lMapping: TPropMapping;
begin
  lMapping := TPropMapping.Create;
  lMapping.PropName := 'MyPropertyName' + IntToStr(Controller.Model.ClassMapping.PropMappings.Count + 1);
  lMapping.PropertyType := ptString;
  lMapping.FieldName := 'database_field_name';
  Controller.Model.ClassMapping.PropMappings.Add(lMapping);

  lView := TPropMapEditView.Create(nil);
  lCtrl := TPropMapEditController.Create(lMapping, lView);
  try
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lctrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  lMapping.NotifyObservers;

end;

procedure TCmdDoAddPropMapping.DoRemoveListeners;
begin
  Controller.View.mnuNewMap.OnClick := nil;
end;

{ TCmdDoEditPropMapping }

procedure TCmdDoEditPropMapping.DoAddListeners;
begin
  Controller.View.mnuEditMap.OnClick := self.HandleNotifyEvent;
  Controller.View.lvMapping.OnDblClick := self.HandleNotifyEvent;
end;

procedure TCmdDoEditPropMapping.DoExecute;
var
  lCtrl: TPropMapEditController;
  lView: TPropMapEditView;
  lListCtrl: TListViewController;
  lMap: TPropMapping;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('mapping_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lMap := TPropMapping(lListCtrl.SelectedItem);

  lView := TPropMapEditView.Create(nil);
  lCtrl := TPropMapEditController.Create(lMap, lView);
  try
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  lMap.NotifyObservers;

end;

procedure TCmdDoEditPropMapping.DoRemoveListeners;
begin
  Controller.View.mnuEditMap.OnClick := nil;
end;

{ TCmdDoDeletePropMapping }

procedure TCmdDoDeletePropMapping.DoAddListeners;
begin
  Controller.View.mnuDeleteMap.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoDeletePropMapping.DoExecute;
var
  lListCtrl: TListViewController;
  lMap: TPropMapping;
  lMsg: string;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('mapping_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lMap := TPropMapping(lListCtrl.SelectedItem);

  lMsg := Format(CONFIRM_PROP_DELETE, [lMap.PropName]);
  if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], Controller.View.Handle) = mrNo then
    exit;

  Controller.Model.ClassMapping.PropMappings.Extract(lMap);
  Controller.Model.ClassMapping.PropMappings.NotifyObservers;

  lMap.Free;


end;

procedure TCmdDoDeletePropMapping.DoRemoveListeners;
begin
  Controller.View.mnuDeleteMap.OnClick := nil;
end;

{ TCmdDoSortProps }

procedure TCmdDoSortPropsByName.DoAddListeners;
begin
  Controller.View.mnuSortProps.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoSortPropsByName.DoExecute;
begin
  Controller.Model.ClassProps.SortByProps(['Name'], true);
  Controller.Model.ClassProps.NotifyObservers;
end;

procedure TCmdDoSortPropsByName.DoRemoveListeners;
begin
  Controller.View.mnuSortProps.OnClick := nil;
end;

{ TCmdDoSortPropMapsByPropName }

procedure TCmdDoSortPropMapsByPropName.DoAddListeners;
begin
  Controller.View.mnuSortMapsByProp.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoSortPropMapsByPropName.DoExecute;
begin
  Controller.Model.ClassProps.SortByProps(['Name']);
  Controller.Model.ClassProps.NotifyObservers;
end;

procedure TCmdDoSortPropMapsByPropName.DoRemoveListeners;
begin
  Controller.View.mnuSortMapsByProp.OnClick := nil;
end;

{ TCmdDoSortPropMapsByFieldName }

procedure TCmdDoSortPropMapsByFieldName.DoAddListeners;
begin
  Controller.View.mnuSortMapsByField.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoSortPropMapsByFieldName.DoExecute;
begin
  Controller.Model.ClassMapping.PropMappings.SortByProps(['PropName']);
  Controller.Model.ClassMapping.PropMappings.NotifyObservers;
end;

procedure TCmdDoSortPropMapsByFieldName.DoRemoveListeners;
begin
  Controller.View.mnuSortMapsByField.OnClick := nil;
end;

{ TCmdDoAddValidator }

procedure TCmdDoAddValidator.DoAddListeners;
begin
  Controller.View.mnuNewValidator.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoAddValidator.DoExecute;
var
  lNewVal: TMapValidator;
  lView: TValidatorEditView;
  lCtrl: TValidatorEditController;
begin
  lView := TValidatorEditView.Create(nil);
  lNewVal := TMapValidator.Create;
  lCtrl := TValidatorEditController.Create(lNewVal, lView);
  try
    Controller.Model.Validators.Add(lNewVal);
    lCtrl.ParentController := Controller;
    lNewVal.ValidatorType := vtRequired;
    lNewVal.ClassProp := 'class_prop_name';
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  lNewVal.NotifyObservers;

end;

procedure TCmdDoAddValidator.DoRemoveListeners;
begin
  Controller.View.mnuNewValidator.OnClick := nil;
end;

{ TCmdDoEditValidator }

procedure TCmdDoEditValidator.DoAddListeners;
begin
  Controller.View.mnuEditValidator.OnClick := self.HandleNotifyEvent;
  Controller.View.lvValidators.OnDblClick := self.HandleNotifyEvent;
end;

procedure TCmdDoEditValidator.DoExecute;
var
  lCtrl: TValidatorEditController;
  lView: TValidatorEditView;
  lListCtrl: TListViewController;
  lVal: TMapValidator;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('validators_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lVal := TMapValidator(lListCtrl.SelectedItem);

  lView := TValidatorEditView.Create(nil);
  lCtrl := TValidatorEditController.Create(lVal, lView);
  try
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  lVal.NotifyObservers;
end;

procedure TCmdDoEditValidator.DoRemoveListeners;
begin
  Controller.View.mnuEditValidator.OnClick := nil;
end;

{ TCmdDoDeleteValidator }

procedure TCmdDoDeleteValidator.DoAddListeners;
begin
  Controller.View.mnuDeleteValidator.OnClick := Self.HandleNotifyEvent;
end;

procedure TCmdDoDeleteValidator.DoExecute;
var
  lListCtrl: TListViewController;
  lVal: TMapValidator;
  lMsg: string;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('validators_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lVal := TMapValidator(lListCtrl.SelectedItem);

  lMsg := Format(CONFIRM_PROP_DELETE, [lVal.Caption]);
  if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], Controller.View.Handle) = mrNo then
    exit;

  Controller.Model.Validators.Extract(lVal);
  Controller.Model.Validators.NotifyObservers;

  lVal.Free;
end;

procedure TCmdDoDeleteValidator.DoRemoveListeners;
begin
  Controller.View.mnuDeleteValidator.OnClick := nil;
end;

{ TCmdDoCreateSelect }

procedure TCmdDoCreateSelect.DoAddListeners;
begin
  Controller.View.mnuNewSelect.OnClick := Self.HandleNotifyEvent;
end;

procedure TCmdDoCreateSelect.DoExecute;
var
  lNewSelect: TClassMappingSelect;
  lView: TSelectEditView;
  lCtrl: TSelectEditController;
begin
  lView := TSelectEditView.Create(nil);
  lNewSelect := TClassMappingSelect.Create;
  lCtrl := TSelectEditController.Create(lNewSelect, lView);
  try
    Controller.Model.Selections.Add(lNewSelect);
    lCtrl.ParentController := Controller;
    lNewSelect.Name := 'FindBySomeCriteria';
    lNewSelect.SQL:= 'SELECT * FROM YOUR_TABLE';
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  lNewSelect.NotifyObservers;


end;

procedure TCmdDoCreateSelect.DoRemoveListeners;
begin
  Controller.View.mnuNewSelect.OnClick := nil;
end;

{ TCmdDoEditSelect }

procedure TCmdDoEditSelect.DoAddListeners;
begin
  Controller.View.mnuEditSelect.OnClick := Self.HandleNotifyEvent;
  Controller.View.lvSelections.OnDblClick := Self.HandleNotifyEvent;
end;

procedure TCmdDoEditSelect.DoExecute;
var
  lCtrl: TSelectEditController;
  lView: TSelectEditView;
  lListCtrl: TListViewController;
  lSelect: TClassMappingSelect;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('selects_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lSelect := TClassMappingSelect(lListCtrl.SelectedItem);

  lView := TSelectEditView.Create(nil);
  lCtrl := TSelectEditController.Create(lSelect, lView);
  try
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  lSelect.NotifyObservers;

end;

procedure TCmdDoEditSelect.DoRemoveListeners;
begin
  Controller.View.mnuEditSelect.OnClick := nil;
end;

{ TCmdDoDeleteSelect }

procedure TCmdDoDeleteSelect.DoAddListeners;
begin
  Controller.View.mnuDeleteSelect.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoDeleteSelect.DoExecute;
var
  lListCtrl: TListViewController;
  lSelect: TClassMappingSelect;
  lMsg: string;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('selects_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lSelect := TClassMappingSelect(lListCtrl.SelectedItem);

  lMsg := Format(CONFIRM_PROP_DELETE, [lSelect.Caption]);
  if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], Controller.View.Handle) = mrNo then
    exit;

  Controller.Model.Selections.Extract(lSelect);
  Controller.Model.Selections.NotifyObservers;

  lSelect.Free;
end;

procedure TCmdDoDeleteSelect.DoRemoveListeners;
begin
  Controller.View.mnuDeleteSelect.OnClick := nil;
end;

end.
