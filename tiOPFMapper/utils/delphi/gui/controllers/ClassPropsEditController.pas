unit ClassPropsEditController;

interface

uses
  Classes, StdCtrls, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  ClassPropEditViewFrm, BaseOkCancelDialogController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TClassPropEditController = class(TBaseOkCancelDialogController)
  private
    FPropertyTypes: TMapPropertyTypeList;
  protected
    procedure DoCreateMediators; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init; override;
    function Model: TMapClassProp; reintroduce;
    function View: TClassPropEditView; reintroduce;
  end;

implementation

uses
  vcl_controllers, Controls;

{ TClassPropEditController }

constructor TClassPropEditController.Create;
begin
  inherited;

  FPropertyTypes := TMapPropertyTypeList.Create;
  FPropertyTypes.OwnsObjects := False;
end;

destructor TClassPropEditController.Destroy;
begin
  FPropertyTypes.Free;

  inherited;
end;

procedure TClassPropEditController.DoCreateMediators;
var
  lCboCtrl: TComboBoxController;
begin
  inherited;

  AddController(TEditController.Create(Model, View.ePropName, 'Name'));

  lCboCtrl := TComboBoxController.Create(Model, View.cboPropType, 'PropertyType');
  lCboCtrl.ListData := FPropertyTypes;
  lCboCtrl.ListDisplayProp := 'TypeName';
  lCboCtrl.ListValueProp := 'TypeName';
  AddController(lCboCtrl);

  AddController(TCheckBoxController.Create(Model, View.ckReadOnly, 'IsReadOnly'));
end;

procedure TClassPropEditController.Init;
begin
//  FPropertyTypes := TMapPropertyTypeList(TAppModel.Instance.CurrentPropertyTypes.Clone);
  FPropertyTypes.Assign(TAppModel.Instance.CurrentPropertyTypes);
  FPropertyTypes.SortByProps(['TypeName']);

  inherited;
end;

function TClassPropEditController.Model: TMapClassProp;
begin
  Result := inherited Model as TMapClassProp;
end;

function TClassPropEditController.View: TClassPropEditView;
begin
  result := inherited View as TClassPropEditView;
end;

end.

