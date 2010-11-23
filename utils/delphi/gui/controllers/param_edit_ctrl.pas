unit param_edit_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_mdl
  ,param_edit_view
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TParamEditController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleOKClick(Sender: TObject);
  public
    function    Model: TSelectParam; reintroduce;
    function    View: TParamEditView; reintroduce;
    constructor Create(AModel: TSelectParam; AView: TParamEditView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;

implementation

uses
  vcl_controllers
  ,StdCtrls
  ,Controls
  ,Dialogs
  ;

{ TParamEditController }

constructor TParamEditController.Create(AModel: TSelectParam;
  AView: TParamEditView);
begin
  inherited Create(AModel, AView);
end;

destructor TParamEditController.Destroy;
begin
  View.Hide;
  View.Free;
  inherited;
end;

procedure TParamEditController.DoCreateMediators;
begin
  AddController(TEditController.Create(Model, View.eParamName, 'ParamName'));
  AddController(TEditController.Create(Model, View.eSQLParamName, 'SQLParamName'));
  AddController(TComboBoxIndexController.Create(Model, View.cboParamType, 'ParamType'));
  AddController(TComboBoxController.Create(Model, View.cboPassBy, 'PassBy'));

  // Native events
  View.btnOK.OnClick := Self.HandleOKClick;
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

  View.ModalResult := mrOk;

end;

function TParamEditController.Model: TSelectParam;
begin
  result := inherited Model as TSelectParam;
end;

procedure TParamEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    View.ShowModal;
end;

function TParamEditController.View: TParamEditView;
begin
  result := inherited View as TParamEditView;
end;

end.
