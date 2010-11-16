unit edit_class_ctrl;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,edit_class_view
  ,event_const
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TClassEditController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   DoTearDownMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleCloseButtonClick(Sender: TObject);
    procedure   HandleEditProp(Sender: TObject);
    procedure   HandleCreateProp(Sender: TObject);
    procedure   HandleDeleteProp(Sender: TObject);
  public
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation);
       overload; override;
    function    Model: TMapClassDef; reintroduce;
    function    View: TClassEditView; reintroduce;
    constructor Create(AModel: TMapClassDef; AView: TClassEditView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;


implementation
uses
  lcl_controllers
  ,app_events
  ,edit_class_prop_view
  ,prop_edit_ctrl
  ;

{ TClassEditController }

constructor TClassEditController.Create(AModel: TMapClassDef;
  AView: TClassEditView);
begin
  inherited Create(AModel, AView);
  View.btnClose.OnClick := @HandleCloseButtonClick;
end;

destructor TClassEditController.Destroy;
begin
  inherited Destroy;
end;

procedure TClassEditController.DoCreateMediators;
var
  lListCtrl: TListViewController;
begin

  AddController(TEditController.Create(Model, View.eClassName, 'BaseClassName'));
  AddController(TComboBoxController.Create(Model, View.cboClassParent, 'BaseClassParent'));
  AddController(TCheckBoxController.Create(Model, View.ckAutoMap, 'AutoMap'));
  AddController(TCheckBoxController.Create(Model, View.ckCreateList, 'AutoCreateListClass'));

  // Class Properties
  lListCtrl := TListViewController.Create(Model.ClassProps, View.lvProps, '');
  lListCtrl.Name := 'class_edit_props';
  lListCtrl.AddRenderer(TBooleanRenderer.Create);

  with lListCtrl.AddNewCol('PropName') do
    begin
      Width := 200;
      ColCaption := 'Prop Name';
    end;

  with lListCtrl.AddNewCol('PropType') do
    begin
      Width := 100;
      ColCaption := 'Type';
    end;

  with lListCtrl.AddNewCol('PropTypeName') do
    begin
      Width := 100;
      ColCaption := 'Type Name';
    end;

  with lListCtrl.AddNewCol('IsReadOnly') do
    begin
      Width := 100;
      ColCaption := 'Read Only';
      RendererIndex := 0;
    end;

  AddController(lListCtrl);

  // Class Mappings
  lListCtrl := TListViewController.Create(Model.ClassMapping.PropMappings, View.lvMappings, '');
  lListCtrl.Name := 'class_edit_mappings';

  with lListCtrl.AddNewCol('PropName') do
    begin
      Width := 200;
      ColCaption := 'Prop Name';
    end;

  with lListCtrl.AddNewCol('PropType') do
    begin
      Width := 100;
      ColCaption := 'Type';
    end;

  with lListCtrl.AddNewCol('FieldName') do
    begin
      Width := 100;
      ColCaption := 'DB Field';
    end;

  AddController(lListCtrl);

  // Class Validators
  lListCtrl := TListViewController.Create(Model.Validators, View.lvValiadators, '');
  lListCtrl.Name := 'class_edit_validators';

  with lListCtrl.AddNewCol('ClassProp') do
    begin
      Width := 200;
      ColCaption := 'Prop Name';
    end;

  with lListCtrl.AddNewCol('ValidatorType') do
    begin
      Width := 100;
      ColCaption := 'Validator';
    end;

  with lListCtrl.AddNewCol('Value') do
    begin
      Width := 100;
      ColCaption := 'Value';
    end;

  AddController(lListCtrl);

  // Class Selections
  lListCtrl := TListViewController.Create(Model.Selections, View.lvSelects, '');
  lListCtrl.Name := 'class_edit_selections';

  with lListCtrl.AddNewCol('Caption') do
    begin
      Width := 400;
      ColCaption := 'Select Name';
    end;

  AddController(lListCtrl);

end;

procedure TClassEditController.DoTearDownMediators;
begin
  Controllers.ChangeAllActive(false);
  Controllers.Clear;
  View.btnClose.OnClick := nil;
  View.Close;
end;

procedure TClassEditController.HandleCloseButtonClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(TRemoveSubControllerEvent.Create(self, 'edit' + Model.BaseClassName));
end;

procedure TClassEditController.HandleCreateProp(Sender: TObject);
var
  lCtrl: TClassPropEditController;
  lListCtrl: TListViewController;
  lClassProp: TMapClassProp;
begin
  lListCtrl := TListViewController(Controllers.FindByName('class_edit_props'));
  if lListCtrl <>nil then
    begin
      lClassProp := TMapClassProp(lListCtrl.SelectedItem);
      if lClassProp = nil then
        exit;
      lCtrl := TClassPropEditController.Create(lClassProp, TClassPropEditView.Create(View));
      lCtrl.Init;
      lCtrl.Active := true;
    end;
end;

procedure TClassEditController.HandleDeleteProp(Sender: TObject);
begin

end;

procedure TClassEditController.HandleEditProp(Sender: TObject);
var
  lCtrl: TClassPropEditController;
  lListCtrl: TListViewController;
  lClassProp: TMapClassProp;
begin
  lListCtrl := TListViewController(Controllers.FindByName('class_edit_props'));
  if lListCtrl <>nil then
    begin
      lClassProp := TMapClassProp(lListCtrl.SelectedItem);
      if lClassProp = nil then
        exit;
      lCtrl := TClassPropEditController.Create(lClassProp, TClassPropEditView.Create(View));
      lCtrl.Init;
      lCtrl.Active := true;
    end;

end;

function TClassEditController.Model: TMapClassDef;
begin
  result := inherited Model as TMapClassDef;
end;

procedure TClassEditController.SetActive(const AValue: Boolean);
begin
  inherited SetActive(AValue);
  View.Visible := AValue;
end;

procedure TClassEditController.Update(ASubject: TtiObject;
  AOperation: TNotifyOperation);
begin
end;

function TClassEditController.View: TClassEditView;
begin
  result := inherited View as TClassEditView;
end;

end.

