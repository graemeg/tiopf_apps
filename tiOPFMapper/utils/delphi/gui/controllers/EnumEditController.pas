unit EnumEditController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  EnumEditViewFrm, BaseOkCancelDialogController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Edit an enumeration. }
  TEnumEditController = class(TBaseOkCancelDialogController)
  protected
    procedure DoCreateMediators; override;
    procedure HandleOKClick(Sender: TObject); override;
  public
    constructor Create(AModel: TMapEnum; AView: TEnumEditView); reintroduce; overload; virtual;

    function Model: TMapEnum; reintroduce;
    function View: TEnumEditView; reintroduce;
    procedure Update(ASubject: TtiObject); override;
  end;

implementation

uses
  vcl_controllers, StdCtrls, Controls, Dialogs, EnumEditCommands;

{ TEnumEditController }

constructor TEnumEditController.Create(AModel: TMapEnum; AView: TEnumEditView);
begin
  inherited Create(AModel, AView);
  RegisterCommands(self);
end;

procedure TEnumEditController.DoCreateMediators;
var
  lListCtrl: TListViewController;
begin
  inherited;

  AddController(TEditController.Create(Model, View.eEnumName, 'TypeName'));

  lListCtrl := TListViewController.Create(Model.Values, View.lvValue, '');
  lListCtrl.Name := 'enumvalues_ctl';

  with lListCtrl.AddNewCol('EnumValueName') do
  begin
    Width := 140;
    ColCaption := 'Value Name';
  end;

  with lListCtrl.AddNewCol('EnumValue') do
  begin
    Width := 100;
    ColCaption := 'Value';
  end;

  AddController(lListCtrl);

  AddController(TCheckBoxController.Create(Model, View.ckCreateEnumerationSet, 'EnumerationSet'));
  AddController(TEditController.Create(Model, View.eEnumerationSetName, 'EnumerationSetName'));

  Model.NotifyObservers;
end;

procedure TEnumEditController.HandleOKClick(Sender: TObject);
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

function TEnumEditController.Model: TMapEnum;
begin
  result := inherited Model as TMapEnum;
end;

procedure TEnumEditController.Update(ASubject: TtiObject);
begin
  inherited;

  View.eEnumerationSetName.Enabled := Model.EnumerationSet;
end;

function TEnumEditController.View: TEnumEditView;
begin
  Result := inherited View as TEnumEditView;
end;

end.

