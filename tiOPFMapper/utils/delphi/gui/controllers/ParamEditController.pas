unit ParamEditController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  ParamEditViewFrm, BaseOkCancelDialogController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TParamEditController = class(TBaseOkCancelDialogController)
  private
    FPropertyTypes: TMapPropertyTypeList;
  protected
    procedure DoCreateMediators; override;
    procedure HandleOKClick(Sender: TObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init; override;
    function Model: TSelectParam; reintroduce;
    function View: TParamEditView; reintroduce;
  end;

implementation

uses
  vcl_controllers, StdCtrls, Controls, Dialogs, AppController;

{ TParamEditController }

constructor TParamEditController.create;
begin
  inherited;

  FPropertyTypes := TMapPropertyTypeList.Create;
  FPropertyTypes.OwnsObjects := false;
end;

destructor TParamEditController.Destroy;
begin
  FPropertyTypes.Free;

  inherited;
end;

procedure TParamEditController.DoCreateMediators;
var
  lComboCtrl: TComboBoxController;
begin
  inherited;

  AddController(TEditController.Create(Model, View.eParamName, 'ParamName'));
  AddController(TEditController.Create(Model, View.eSQLParamName, 'SQLParamName'));
  AddController(TComboBoxIndexController.Create(Model, View.cboParamType, 'ParamType'));
  AddController(TComboBoxController.Create(Model, View.cboPassBy, 'PassBy'));

  lComboCtrl := TComboBoxController.Create(Model, View.cboParamType, 'ParamType');
  lComboCtrl.ListData := FPropertyTypes;
  lComboCtrl.ListDisplayProp := 'TypeName';
  lComboCtrl.ListValueProp := 'TypeName';
  AddController(lComboCtrl);
end;

procedure TParamEditController.HandleOKClick(Sender: TObject);
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

procedure TParamEditController.Init;
begin
  FPropertyTypes.Assign(TAppModel.Instance.CurrentPropertyTypes);
  FPropertyTypes.SortByProps(['TypeName']);

  inherited;
end;

function TParamEditController.Model: TSelectParam;
begin
  result := inherited Model as TSelectParam;
end;

function TParamEditController.View: TParamEditView;
begin
  result := inherited View as TParamEditView;
end;

end.

