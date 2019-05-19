unit ValidatorEditController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  ValidatorEditViewFrm, BaseOkCancelDialogController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Validator Edit Controller. }
  TValidatorEditController = class(TBaseOkCancelDialogController)
  protected
    procedure DoCreateMediators; override;
    procedure HandleOkClick(Sender: TObject); override;
  public
    function Model: TMapValidator; reintroduce;
    function View: TValidatorEditView; reintroduce;
    procedure Update(ASubject: TtiObject); override;
  end;

implementation

uses
  vcl_controllers, StdCtrls, Controls, Dialogs;

{ TValidatorEditController }


procedure TValidatorEditController.DoCreateMediators;
var
  lCboCtrl: TComboBoxController;
begin
  inherited;

  AddController(TComboBoxIndexController.Create(Model, View.cboValidatorType, 'ValidatorType'));
  AddController(TEditController.Create(Model, View.eValue, 'Value'));

  lCboCtrl := TComboBoxController.Create(Model, View.cboClassProp, 'ClassProp');
  lCboCtrl.ListData := TMapClassDef(ParentController.Model).ClassProps;
  lCboCtrl.ListDisplayProp := 'Name';
  lCboCtrl.ListValueProp := 'Name';
  AddController(lCboCtrl);
end;

procedure TValidatorEditController.HandleOkClick(Sender: TObject);
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

function TValidatorEditController.Model: TMapValidator;
begin
  Result := inherited Model as TMapValidator;
end;

procedure TValidatorEditController.Update(ASubject: TtiObject);
begin
  inherited;

  View.eValue.Enabled := Model.ValidatorType <> vtRequired;
end;

function TValidatorEditController.View: TValidatorEditView;
begin
  result := inherited View as TValidatorEditView;
end;

end.

