unit validator_edit_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_mdl
  ,validator_edit_view
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Validator Edit Controller. }
  TValidatorEditController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleOKClick(Sender: TObject);
  public
    function    Model: TMapValidator; reintroduce;
    function    View: TValidatorEditView; reintroduce;
    constructor Create(AModel: TMapValidator; AView: TValidatorEditView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;

implementation

uses
  vcl_controllers
  ,StdCtrls
  ,Controls
  ,Dialogs
  ;

{ TValidatorEditController }

constructor TValidatorEditController.Create(AModel: TMapValidator;
  AView: TValidatorEditView);
begin
  inherited Create(AModel, AView);
end;

destructor TValidatorEditController.Destroy;
begin
  View.Hide;
  View.Free;
  inherited;
end;

procedure TValidatorEditController.DoCreateMediators;
var
  lCboCtrl: TComboBoxController;
begin
  AddController(TComboBoxIndexController.Create(Model, View.cboValidatorType, 'ValidatorType'));
  AddController(TEditController.Create(Model, View.eValue, 'Value'));

  lCboCtrl := TComboBoxController.Create(Model, View.cboClassProp, 'ClassProp');
  lCboCtrl.ListData := TMapClassDef(ParentController.Model).ClassProps;
  lCboCtrl.ListDisplayProp := 'Name';
  lCboCtrl.ListValueProp := 'Name';
  AddController(lCboCtrl);

  // Native events
  View.btnOK.OnClick := self.HandleOKClick;
end;

procedure TValidatorEditController.HandleOKClick(Sender: TObject);
var
  lMsg: string;
begin
  if not Model.IsValid(lMsg) then
    begin
      MessageDlg('Error(s): ' + sLineBreak + lMsg, mtError, [mbOK], 0);
      exit;
    end;
  View.ModalResult := mrOk;
end;

function TValidatorEditController.Model: TMapValidator;
begin
  Result := inherited Model as TMapValidator;
end;

procedure TValidatorEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    View.ShowModal;
end;

function TValidatorEditController.View: TValidatorEditView;
begin
  result := inherited View as TValidatorEditView;
end;

end.
