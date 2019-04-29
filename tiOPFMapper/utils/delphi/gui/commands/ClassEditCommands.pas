unit ClassEditCommands;

interface

uses
  SysUtils, Classes, mapper, mvc_base, tiObject, ClassEditViewFrm,
  ClassEditController, AppCommands;

type
  TClassEditCommand = class(TCmdNotifyEvent)
  public
    function Controller: TClassEditController; reintroduce;
  end;

  {: Edit Class Property. }
  TCmdDoEditProperty = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Add new property. }
  TCmdDoAddProperty = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Add new property. }
  TCmdDoDeleteProperty = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Sort Properties. }
  TCmdDoSortPropsByName = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;


  {: Add new Mapping. }
  TCmdDoAddPropMapping = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Edit an existing mapping. }
  TCmdDoEditPropMapping = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Delete an existing prop mapping. }
  TCmdDoDeletePropMapping = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Sort prop maps by prop name. }
  TCmdDoSortPropMapsByPropName = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Sort prop maps by field name. }
  TCmdDoSortPropMapsByFieldName = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Create a new validator and add it. }
  TCmdDoAddValidator = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Edit an existing validator. }
  TCmdDoEditValidator = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Deleted an existing validator. }
  TCmdDoDeleteValidator = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Create New Selection }
  TCmdDoCreateSelect = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Edit existing Selection }
  TCmdDoEditSelect = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Delete existing Selection }
  TCmdDoDeleteSelect = class(TClassEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

procedure RegisterClassEditControllerCommands(AController: TMVCController);

implementation

uses
  Dialogs, Controls, ClassPropEditViewFrm, ClassPropsEditController, PropMapEditViewFrm,
  PropMapEditController, ValidatorEditViewFrm, ValidatorEditController, SelectEditViewFrm,
  SelectEditController, vcl_controllers, AppConsts, AppModel;

procedure RegisterClassEditControllerCommands(AController: TMVCController);
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
  lProp, lPropTmp: TMapClassProp;
begin
  lListCtrl := TListViewController(Controller.Controllers.FindByName('props_ctl'));

  if lListCtrl.SelectedItem = nil then
    exit;

  lProp := TMapClassProp(lListCtrl.SelectedItem);

  lPropTmp := TMapClassProp.Create;
  lPropTmp.Assign(lProp);

  lView := TClassPropEditView.Create(nil);
  lCtrl := TClassPropEditController.Create(lPropTmp, lView);

  try
    lCtrl.Init;
    lCtrl.Active := True;

    if lCtrl.Ok then
    begin
      lProp.Assign(lPropTmp);
      lProp.NotifyObservers;
    end;

    lCtrl.Active := False;
  finally
    lCtrl.Free;
    FreeAndNil(lPropTmp);
  end;
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

  try
    lNewProp := TMapClassProp.CreateNew;
    lNewProp.Name := 'Prop_' + IntToStr(Controller.Model.ClassProps.Count);
    lNewProp.PropertyType := TAppModel.Instance.CurrentPropertyTypes.FindByTypeName('String');
//    lNewProp.PropTypeName := 'String';

    lCtrl := TClassPropEditController.Create(lNewProp, lView);
    try
      lCtrl.Init;
      lCtrl.Active := True;

      if lCtrl.Ok then
      begin
        Controller.Model.ClassProps.Add(lNewProp);

//        if (Copy(lNewProp.PropTypeName, 1, 1) = 'T') and (LowerCase(lNewProp.PropTypeName) <> 'tdatetime') then
//          lNewProp.PropertyType := ptEnum
//        else
//          lNewProp.PropertyType := mapper.gStrToPropType(lNewProp.PropTypeName);

        Controller.Model.ClassProps.NotifyObservers;
      end
      else
        FreeAndNil(lNewProp);

      lCtrl.Active := False;
    finally
      lCtrl.Free;
    end;
  except
    lNewProp.Free;
  end;
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
  lMap := TPropMapping(Controller.Model.ClassMapping.PropMappings.FindByProps(['PropName'], [lProp.Name]));

  if lMap <> nil then
  begin
    Controller.Model.ClassMapping.PropMappings.Extract(lMap);
    lMap.Free;
    Controller.Model.ClassMapping.PropMappings.NotifyObservers;
  end;

  // Remove any validators related to deleted property
  for lCtr := Controller.Model.Validators.Count - 1 downto 0 do
  begin
    lVal := Controller.Model.Validators.Items[lCtr];

    if SameText(lVal.ClassProp.Name, lProp.Name) then
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

  try
    lMapping.PropName := 'MyPropertyName' + IntToStr(Controller.Model.ClassMapping.PropMappings.Count + 1);
    lMapping.PropertyType := TAppModel.Instance.CurrentPropertyTypes.FindByTypeName('String');
    lMapping.FieldName := 'database_field_name';

    lView := TPropMapEditView.Create(nil);
    lCtrl := TPropMapEditController.Create(lMapping, lView);

    try
      lCtrl.ParentController := Controller;
      lCtrl.Init;
      lCtrl.Active := True;

      if lCtrl.Ok then
      begin
        Controller.Model.ClassMapping.PropMappings.Add(lMapping);
        Controller.Model.ClassMapping.PropMappings.NotifyObservers;
      end
      else
        FreeAndNil(lMapping);

      lCtrl.Active := False;
    finally
      lCtrl.Free;
    end;
  except
    lMapping.Free;
  end;
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
  lMap, lMapTmp: TPropMapping;
begin

  lListCtrl := TListViewController(Controller.Controllers.FindByName('mapping_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lMap := TPropMapping(lListCtrl.SelectedItem);

  lMapTmp := TPropMapping.Create;
  lMapTmp.Assign(lMap);

  lView := TPropMapEditView.Create(nil);
  lCtrl := TPropMapEditController.Create(lMapTmp, lView);

  try
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;

    if lCtrl.Ok then
    begin
      lMap.Assign(lMapTmp);
      lMap.NotifyObservers;
    end;

    lCtrl.Active := False;
  finally
    lCtrl.Free;
    FreeAndNil(lMapTmp);
  end;
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
  lNewVal := TMapValidator.Create;

  try
    lView := TValidatorEditView.Create(nil);
    lCtrl := TValidatorEditController.Create(lNewVal, lView);

    try
      lCtrl.ParentController := Controller;
      lNewVal.ValidatorType := vtRequired;
      lNewVal.ClassProp := Nil;
      lCtrl.Init;
      lCtrl.Active := True;

      if lCtrl.Ok then
      begin
        Controller.Model.Validators.Add(lNewVal);
        Controller.Model.Validators.NotifyObservers;
      end
      else
        FreeAndNil(lNewVal);

      lCtrl.Active := False;
    finally
      lCtrl.Free;
    end;
  except
    lNewVal.Free;
  end;
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
  lVal, lValTmp: TMapValidator;
begin
  lListCtrl := TListViewController(Controller.Controllers.FindByName('validators_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lVal := TMapValidator(lListCtrl.SelectedItem);
  lValTmp := TMapValidator.Create;
  lValTmp.Assign(lVal);

  lView := TValidatorEditView.Create(nil);
  lCtrl := TValidatorEditController.Create(lValTmp, lView);

  try
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;

    if lCtrl.Ok then
    begin
      lVal.Assign(lValTmp);
      lVal.NotifyObservers;
    end;

    lCtrl.Active := False;
  finally
    lCtrl.Free;
    FreeAndNil(lValTmp);
  end;
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

  try
    lNewSelect := TClassMappingSelect.Create;
    lCtrl := TSelectEditController.Create(lNewSelect, lView);

    try
      lCtrl.ParentController := Controller;
      lNewSelect.Name := 'FindBySomeCriteria';
      lNewSelect.SQL := 'SELECT * FROM YOUR_TABLE';
      lCtrl.Init;
      lCtrl.Active := True;

      if lCtrl.Ok then
      begin
        Controller.Model.Selections.Add(lNewSelect);
        Controller.Model.Selections.NotifyObservers;
      end
      else
        FreeAndNil(lNewSelect);

      lCtrl.Active := False;
    finally
      lCtrl.Free;
    end;
  except
    FreeAndNil(lNewSelect);
  end;
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
  lSelect, lSelectTmp: TClassMappingSelect;
begin
  lListCtrl := TListViewController(Controller.Controllers.FindByName('selects_ctl'));
  if lListCtrl.SelectedItem = nil then
    exit;

  lSelect := TClassMappingSelect(lListCtrl.SelectedItem);
  lSelectTmp := TClassMappingSelect.Create;
  lSelectTmp.Assign(lSelect);

  lView := TSelectEditView.Create(nil);
  lCtrl := TSelectEditController.Create(lSelectTmp, lView);

  try
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;

    if lCtrl.Ok then
    begin
      lSelect.Assign(lSelectTmp);
      lSelect.NotifyObservers;
    end;

    lCtrl.Active := False;
  finally
    lCtrl.Free;
    FreeAndNil(lSelectTmp);
  end;
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

