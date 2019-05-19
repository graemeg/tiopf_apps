unit SelectEditController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  SelectEditViewFrm, BaseOkCancelDialogController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TSelectEditController = class(TBaseOkCancelDialogController)
  protected
    procedure DoCreateMediators; override;
    procedure HandleOKClick(Sender: TObject); override;
  public
    constructor Create(AModel: TClassMappingSelect; AView: TSelectEditView); reintroduce; overload; virtual;

    function Model: TClassMappingSelect; reintroduce;
    function View: TSelectEditView; reintroduce;
  end;

implementation

uses
  vcl_controllers, SelectEditCommands, StdCtrls, Controls, Dialogs;

{ TSelectEditController }

constructor TSelectEditController.Create(AModel: TClassMappingSelect; AView: TSelectEditView);
begin
  inherited Create(AModel, AView);
  RegisterCommands(self);
end;

procedure TSelectEditController.DoCreateMediators;
var
  lListCtrl: TListViewController;
begin
  inherited;

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

  with lListCtrl.AddNewCol('ParamType.TypeName') do
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
end;

function TSelectEditController.Model: TClassMappingSelect;
begin
  result := inherited Model as TClassMappingSelect;
end;

function TSelectEditController.View: TSelectEditView;
begin
  result := inherited View as TSelectEditView;
end;

end.

