unit ClassEditController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  ClassEditViewFrm, EventsConsts, BaseOkCancelDialogController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TClassEditController = class(TBaseOkCancelDialogController)
  protected
    procedure DoCreateMediators; override;
  public
    constructor Create(AModel: TMapClassDef; AView: TClassEditView); reintroduce; overload; virtual;

    function Model: TMapClassDef; reintroduce;
    function View: TClassEditView; reintroduce;
  end;

implementation

uses
  Controls, vcl_controllers, ClassEditCommands;

{ TProjectSettingsController }

constructor TClassEditController.Create(AModel: TMapClassDef; AView: TClassEditView);
begin
  inherited Create(AModel, AView);
  RegisterClassEditControllerCommands(self);
end;

procedure TClassEditController.DoCreateMediators;
var
  lListCtrl: TListViewController;
begin
  inherited;

  AddController(TEditController.Create(Model, View.eBaseClassName, 'BaseClassName'));
  AddController(TEditController.Create(Model, View.eBaseClassParent, 'BaseClassParent'));
  AddController(TCheckBoxController.Create(Model, View.ckAutoMap, 'AutoMap'));
  AddController(TCheckBoxController.Create(Model, View.ckAutoCreateList, 'AutoCreateListClass'));
  AddController(TCheckBoxController.Create(Model, View.ckListSavesDBName, 'ListSavesDatabaseName'));
  AddController(TCheckBoxController.Create(Model, View.ckNotifyObservers, 'NotifyObserversOfPropertyChanges'));

  // Class mapping
  AddController(TEditController.Create(Model.ClassMapping, View.ePKName, 'PKName'));
  AddController(TEditController.Create(Model.ClassMapping, View.ePKField, 'PKField'));
  AddController(TEditController.Create(Model.ClassMapping, View.eTableName, 'TableName'));
  AddController(TComboBoxIndexController.Create(Model.ClassMapping, View.cboOIDType, 'OIDType'));

  // Class Properties
  lListCtrl := TListViewController.Create(Model.ClassProps, View.lvProps, '');
  lListCtrl.Name := 'props_ctl';
  lListCtrl.AddRenderer(TBooleanRenderer.Create);

  with lListCtrl.AddNewCol('Name') do
  begin
    Width := 200;
    ColCaption := 'Prop Name';
  end;

  with lListCtrl.AddNewCol('PropertyType.BaseType') do
  begin
    Width := 100;
    ColCaption := 'Type';
  end;

  with lListCtrl.AddNewCol('PropertyType.TypeName') do
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
  lListCtrl := TListViewController.Create(Model.ClassMapping.PropMappings, View.lvMapping, '');
  lListCtrl.Name := 'mapping_ctl';

  with lListCtrl.AddNewCol('PropName') do
  begin
    Width := 200;
    ColCaption := 'Prop Name';
  end;

  with lListCtrl.AddNewCol('FieldName') do
  begin
    Width := 200;
    ColCaption := 'DB Field';
  end;

  with lListCtrl.AddNewCol('PropertyType.TypeName') do
  begin
    Width := 100;
    ColCaption := 'Type';
  end;

  AddController(lListCtrl);

  // Class Validators
  lListCtrl := TListViewController.Create(Model.Validators, View.lvValidators, '');
  lListCtrl.Name := 'validators_ctl';

  with lListCtrl.AddNewCol('ClassProp.Name') do
  begin
    Width := 200;
    ColCaption := 'Prop Name';
  end;

  with lListCtrl.AddNewCol('ValidatorType') do
  begin
    Width := 130;
    ColCaption := 'Validator';
  end;

  with lListCtrl.AddNewCol('Value') do
  begin
    Width := 100;
    ColCaption := 'Value';
  end;

  AddController(lListCtrl);

  // Class Selections
  lListCtrl := TListViewController.Create(Model.Selections, View.lvSelections, '');
  lListCtrl.Name := 'selects_ctl';

  with lListCtrl.AddNewCol('Caption') do
  begin
    Width := 400;
    ColCaption := 'Select Name';
  end;

  AddController(lListCtrl);

  // Native events
  Model.NotifyObservers;
end;

function TClassEditController.Model: TMapClassDef;
begin
  result := inherited Model as TMapClassDef;
end;

function TClassEditController.View: TClassEditView;
begin
  result := inherited View as TClassEditView;
end;

end.

