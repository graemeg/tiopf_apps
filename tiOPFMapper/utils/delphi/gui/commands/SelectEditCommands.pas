unit SelectEditCommands;

interface

uses
  SysUtils, Classes, mapper, mvc_base, tiObject, SelectEditController,
  SelectEditViewFrm, AppCommands;

type
  TSelectEditCommand = class(TCmdNotifyEvent)
  public
    function Controller: TSelectEditController; reintroduce;
  end;

  {: Create a new select param. }
  TCmdDoCreateParam = class(TSelectEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Edit existing select param. }
  TCmdDoEditParam = class(TSelectEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

  {: Delete existing select param. }
  TCmdDoDeleteParam = class(TSelectEditCommand)
  protected
    procedure DoExecute; override;
    procedure DoAddListeners; override;
    procedure DoRemoveListeners; override;
  end;

procedure RegisterCommands(AController: TMVCController);

implementation

uses
  ParamEditViewFrm, ParamEditController, vcl_controllers, AppConsts, Dialogs,
  Controls;

procedure RegisterCommands(AController: TMVCController);
begin
  AController.AddCommand(TCmdDoCreateParam.Create(AController));
  AController.AddCommand(TCmdDoEditParam.Create(AController));
  AController.AddCommand(TCmdDoDeleteParam.Create(AController));
end;

{ TSelectEditCommand }

function TSelectEditCommand.Controller: TSelectEditController;
begin
  result := inherited Controller as TSelectEditController;
end;

{ TCmdDoEditProperty }

procedure TCmdDoCreateParam.DoAddListeners;
begin
  Controller.View.mnuNewParam.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoCreateParam.DoExecute;
var
  lView: TParamEditView;
  lCtrl: TParamEditController;
  lParam: TSelectParam;
begin
  lView := TParamEditView.Create(nil);

  try
    lParam := TSelectParam.Create;
    lCtrl := TParamEditController.Create(lParam, lView);

    try
      lParam.ParamName := 'New_Param';
      lParam.SQLParamName := 'New_SQL_Param_Name';
      lCtrl.ParentController := Controller;
      lCtrl.Init;
      lCtrl.Active := true;

      if lCtrl.Ok then
      begin
        Controller.Model.Params.Add(lParam);
        Controller.Model.Params.NotifyObservers;
      end
      else
        FreeAndNil(lParam);

      lCtrl.Active := False;
    finally
      lCtrl.Free;
    end;
  except
    FreeAndNil(lParam);
  end;
end;

procedure TCmdDoCreateParam.DoRemoveListeners;
begin
  Controller.View.mnuNewParam.OnClick := nil;
end;

{ TCmdDoEditParam }

procedure TCmdDoEditParam.DoAddListeners;
begin
  Controller.View.mnuEditParam.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoEditParam.DoExecute;
var
  lView: TParamEditView;
  lCtrl: TParamEditController;
  lListCtrl: TListViewController;
  lParam, lParamTmp: TSelectParam;
begin
  lListCtrl := TListViewController(Controller.Controllers.FindByName('params_ctl'));
  if lListCtrl.SelectedItem = nil then
    Exit;

  lParam := TSelectParam(lListCtrl.SelectedItem);
  lParamTmp := TSelectParam.Create;
  lParamTmp.Assign(lParam);

  lView := TParamEditView.Create(nil);
  lCtrl := TParamEditController.Create(TSelectParam(lListCtrl.SelectedItem), lView);

  try
    lCtrl.ParentController := Controller;
    lCtrl.Init;
    lCtrl.Active := True;

    if lCtrl.Ok then
    begin
      lParam.Assign(lParamTmp);

//      if lParam.ParamType <> ptEnum then
//      begin
//        lParam.ParamTypeName := gPropTypeToStr(lParam.ParamType);
//        lParam.TypeName := lParam.ParamTypeName;
//      end
//      else
//      begin
//        lParam.TypeName := 'enum';
//        lParam.ParamTypeName := lView.cboParamType.Text;
//      end;

      lParam.NotifyObservers;
    end;

    lCtrl.Active := False;
  finally
    lCtrl.Free;
    FreeAndNil(lParamTmp);
  end;
end;

procedure TCmdDoEditParam.DoRemoveListeners;
begin
  Controller.View.mnuEditParam.OnClick := nil;
end;

{ TCmdDoDeleteParam }

procedure TCmdDoDeleteParam.DoAddListeners;
begin
  Controller.View.mnuDeleteParam.OnClick := self.HandleNotifyEvent;
end;

procedure TCmdDoDeleteParam.DoExecute;
var
  lListCtrl: TListViewController;
  lMsg: string;
  lParam: TSelectParam;
begin
  lListCtrl := TListViewController(Controller.Controllers.FindByName('params_ctl'));
  if lListCtrl.SelectedItem = nil then
    Exit;

  lParam := TSelectParam(lListCtrl.SelectedItem);

  lMsg := Format(CONFIRM_PROP_DELETE, [lParam.ParamName]);
  if MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], Controller.View.Handle) = mrNo then
    exit;

  Controller.Model.Params.Extract(lParam);
  Controller.Model.Params.NotifyObservers;

  lParam.Free;
end;

procedure TCmdDoDeleteParam.DoRemoveListeners;
begin
  Controller.View.mnuDeleteParam.OnClick := nil;
end;

end.

