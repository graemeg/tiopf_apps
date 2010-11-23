unit select_edit_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_mdl
  ,select_edit_view
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TSelectEditController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleOKClick(Sender: TObject);
  public
    function    Model: TClassMappingSelect; reintroduce;
    function    View: TSelectEditView; reintroduce;
    constructor Create(AModel: TClassMappingSelect; AView: TSelectEditView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;

implementation

uses
  vcl_controllers
  ,select_edit_cmd
  ,StdCtrls
  ,Controls
  ,Dialogs
  ;

{ TSelectEditController }

constructor TSelectEditController.Create(AModel: TClassMappingSelect;
  AView: TSelectEditView);
begin
  inherited Create(AModel, AView);
  RegisterCommands(self);
end;

destructor TSelectEditController.Destroy;
begin
  View.Hide;
  View.Free;
  inherited;
end;

procedure TSelectEditController.DoCreateMediators;
var
  lListCtrl: TListViewController;
begin
  AddController(TEditController.Create(Model, View.eName, 'Name'));
  AddController(TMemoController.Create(Model, View.memSQL, 'SQL'));

  lListCtrl := TListViewController.Create(Model.Params, View.lvParams, '');
  lListCtrl.Name := 'params_ctl';

  with lListCtrl.AddNewCol('ParamName') do
    begin
      Width := 120;
      ColCaption := 'Param Name';
    end;

  with lListCtrl.AddNewCol('SQLParamName') do
    begin
      Width := 120;
      ColCaption := 'SQL Param';
    end;

  with lListCtrl.AddNewCol('ParamType') do
    begin
      Width := 100;
      ColCaption := 'Param Type';
    end;

  with lListCtrl.AddNewCol('PassBy') do
    begin
      Width := 100;
      ColCaption := 'Pass By';
    end;

  Controllers.Add(lListCtrl);
  // Native events
  View.btnOK.OnClick := self.HandleOKClick;
end;

procedure TSelectEditController.HandleOKClick(Sender: TObject);
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

function TSelectEditController.Model: TClassMappingSelect;
begin
  result := inherited Model as TClassMappingSelect;
end;

procedure TSelectEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    View.ShowModal;
end;

function TSelectEditController.View: TSelectEditView;
begin
  result := inherited View as TSelectEditView;
end;

end.
