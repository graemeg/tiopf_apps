unit enum_edit_cmd;

interface
uses
  SysUtils
  ,Classes
  ,mapper
  ,mvc_base
  ,tiObject
  ,enum_edit_view
  ,enum_edit_ctrl
  ,app_commands
  ;

type

  TCmdEditEnumValue = class(TCmdNotifyEvent)
  public
    function Controller: TEnumEditController; reintroduce;
  end;

  {: Create a new enum vale. }
  TCmdDoCreateEnumValue = class(TCmdEditEnumValue)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Edit existing enume value. }
  TCmdDoEditEnumValue = class(TCmdEditEnumValue)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  {: Delete existing enum value. }
  TCmdDoDeleteEnumValue = class(TCmdEditEnumValue)
  protected
    procedure   DoExecute; override;
    procedure   DoAddListeners; override;
    procedure   DoRemoveListeners; override;
  end;

  procedure   RegisterCommands(AController: TMVCController);

implementation

uses
  param_edit_view
  ,enumvalue_edit_view
  ,enumvalue_edit_ctrl
  ,vcl_controllers
  ,app_consts
  ,Dialogs
  ,Controls
  ;


procedure   RegisterCommands(AController: TMVCController);
begin
  AController.AddCommand(TCmdDoCreateEnumValue.Create(AController));
  AController.AddCommand(TCmdDoEditEnumValue.Create(AController));
  AController.AddCommand(TCmdDoDeleteEnumValue.Create(AController));
end;

{ TCmdCreateEnumValue }

procedure TCmdDoCreateEnumValue.DoAddListeners;
begin
  Controller.View.mnuNewValue.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoCreateEnumValue.DoExecute;
var
  lView: TEnumValueEditView;
  lCtrl: TEnumValueEditController;
  lEnumValue: TMapEnumValue;
begin
  lView := TEnumValueEditView.Create(nil);
  lEnumValue := TMapEnumValue.Create;
  lCtrl := TEnumValueEditController.Create(lEnumValue, lView);
  try
    lEnumValue.EnumValueName := 'nvNewValue';
    Controller.Model.Values.Add(lEnumValue);
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := true;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  Controller.Model.Values.NotifyObservers;

end;

procedure TCmdDoCreateEnumValue.DoRemoveListeners;
begin
  Controller.View.mnuNewValue.OnClick := nil;
end;

{ TCmdEditEnumValue }

function TCmdEditEnumValue.Controller: TEnumEditController;
begin
  result := inherited Controller as TEnumEditController;
end;

{ TCmdDoEditEnumValue }

procedure TCmdDoEditEnumValue.DoAddListeners;
begin
  Controller.View.mnuEditValue.OnClick := Self.HandleNotifyEvent;
end;

procedure TCmdDoEditEnumValue.DoExecute;
var
  lView: TEnumValueEditView;
  lCtrl: TEnumValueEditController;
  lListCtrl: TListViewController;
begin
  lListCtrl := TListViewController(Controller.Controllers.FindByName('enumvalues_ctl'));
  if lListCtrl.SelectedItem = nil then
    Exit;

  lView := TEnumValueEditView.Create(nil);
  lCtrl := TEnumValueEditController.Create(TMapEnumValue(lListCtrl.SelectedItem), lView);
  try
    lCtrl.Init;
    lCtrl.Active := True;
    lCtrl.Active := False;
  finally
    lCtrl.Free;
  end;

  lListCtrl.SelectedItem.NotifyObservers;
end;

procedure TCmdDoEditEnumValue.DoRemoveListeners;
begin
  Controller.View.mnuEditValue.OnClick := nil;
end;

{ TCmdDoDeleteEnumValue }

procedure TCmdDoDeleteEnumValue.DoAddListeners;
begin
  inherited;

end;

procedure TCmdDoDeleteEnumValue.DoExecute;
var
  lListCtrl: TListViewController;
  lMsg: string;
  lEnumValue: TMapEnumValue;
begin
  lListCtrl := TListViewController(Controller.Controllers.FindByName('params_ctl'));
  if lListCtrl.SelectedItem = nil then
    Exit;

  lEnumValue := TMapEnumValue(lListCtrl.SelectedItem);

  lMsg := Format(CONFIRM_PROP_DELETE, [lEnumValue.EnumValueName]);
  if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], Controller.View.Handle) = mrNo then
    exit;

  Controller.Model.Values.Extract(lEnumValue);
  Controller.Model.Values.NotifyObservers;

  lEnumValue.Free;

end;

procedure TCmdDoDeleteEnumValue.DoRemoveListeners;
begin
  inherited;

end;

end.
