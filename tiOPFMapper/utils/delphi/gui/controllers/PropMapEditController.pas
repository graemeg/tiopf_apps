unit PropMapEditController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  PropMapEditViewFrm, BaseOkCancelDialogController, BaseDialogViewFrm;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TPropMapEditController = class(TBaseOkCancelDialogController)
  private
    FPropertyTypes: TMapPropertyTypeList;
    FClassProps: TMapClassPropList;
  protected
    procedure DoCreateMediators; override;
    procedure HandleOKClick(Sender: TObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init; override;
    function Model: TPropMapping; reintroduce;
    function View: TPropMapEditView; reintroduce;
  end;

implementation

uses
  vcl_controllers, StdCtrls, Controls, Dialogs;

{ TClassPropEditController }

constructor TPropMapEditController.Create;
begin
  inherited;

  FClassProps := TMapClassPropList.Create;
  FClassProps.OwnsObjects := false;

  FPropertyTypes := TMapPropertyTypeList.Create;
  FPropertyTypes.OwnsObjects := false;
end;

destructor TPropMapEditController.Destroy;
begin
  FClassProps.Free;
  FPropertyTypes.Free;

  inherited;
end;

procedure TPropMapEditController.DoCreateMediators;
var
  lCboCtrl: TComboBoxController;
begin
  inherited;

  lCboCtrl := TComboBoxController.Create(Model, View.cboPropName, 'PropName');
  lCboCtrl.ListData := FClassProps;
  lCboCtrl.ListDisplayProp := 'Name';
  lCboCtrl.ListValueProp := 'Name';
  AddController(lCboCtrl);

  lCboCtrl := TComboBoxController.Create(Model, View.cboPropType, 'PropertyType');
  lCboCtrl.ListData := FPropertyTypes;
  lCboCtrl.ListDisplayProp := 'TypeName';
  lCboCtrl.ListValueProp := 'TypeName';
  AddController(lCboCtrl);

  AddController(TEditController.Create(Model, View.eFieldName, 'FieldName'));
end;

procedure TPropMapEditController.HandleOKClick(Sender: TObject);
var
  lMsg: string;
begin
  if not Model.IsValid(lMsg) then
  begin
    MessageDlg('Error(s): ' + sLineBreak + lMsg, mtError, [mbOK], 0);
    exit;
  end;

  inherited;
end;

procedure TPropMapEditController.Init;
var
  lMapClass: TMapClassDef;
begin
  lMapClass := TMapClassDef(ParentController.Model);

  FClassProps.Assign(lMapClass.ClassProps);
  FClassProps.SortByProps(['Name']);

  FPropertyTypes.Assign(TAppModel.Instance.CurrentPropertyTypes);
  FPropertyTypes.SortByProps(['TypeName']);

  inherited;
end;

function TPropMapEditController.Model: TPropMapping;
begin
  result := inherited Model as TPropMapping;
end;

function TPropMapEditController.View: TPropMapEditView;
begin
  result := inherited View as TPropMapEditView;
end;

end.

