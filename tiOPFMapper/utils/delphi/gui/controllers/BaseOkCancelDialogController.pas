unit BaseOkCancelDialogController;

interface

uses
  Classes, SysUtils, Controls, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  BaseDialogViewFrm, EventsConsts;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Application base controller for dialogs. }
  TBaseOkCancelDialogController = class(TMVCController)
  private
    FModalResult: TModalResult;
    function GetCancel: boolean;
    function GetOk: boolean;
  protected
    procedure DoAfterInit; override;
    procedure DoCreateMediators; override;
    procedure SetActive(const AValue: Boolean); override;
    procedure HandleOKClick(Sender: TObject); virtual;
    procedure HandleCancelClick(Sender: TObject); virtual;
    procedure HandleKeyPress(Sender: TObject; var Key: Char); virtual;
  public
    constructor Create(AModel: TMapProject; AView: TBaseDialogView); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure ChangeModel(ANewModel: TObject); override;
    procedure Update(ASubject: TtiObject); override;
    procedure Update(ASubject: TtiObject; AOperation: TNotifyOperation); override;
    function Model: TBaseMapObject; virtual;
    function View: TBaseDialogView; reintroduce;

    property Ok: boolean read GetOk;
    property Cancel: boolean read GetCancel;
  end;

implementation

uses
  vcl_controllers, Forms, Dialogs;

{ TProjectSettingsController }

procedure TBaseOkCancelDialogController.ChangeModel(ANewModel: TObject);
begin
  Model.DetachObserver(Self);

  inherited;

  if Assigned(ANewModel) then
    Model.AttachObserver(Self);
end;

constructor TBaseOkCancelDialogController.Create(AModel: TMapProject; AView: TBaseDialogView);
begin
  inherited Create(AModel, AView);

  FModalResult := mrNone;
end;

destructor TBaseOkCancelDialogController.Destroy;
begin
  if Initialized and (Model <> nil) then
    Model.DetachObserver(Self);

  View.Hide;
  View.Free;

  inherited;
end;

procedure TBaseOkCancelDialogController.DoAfterInit;
begin
  inherited;

  Model.AttachObserver(Self);

  // Signals observers
  Model.NotifyObservers;
end;

procedure TBaseOkCancelDialogController.DoCreateMediators;
begin
  // hook up native event
  View.btnOK.OnClick := Self.HandleOKClick;
  View.btnCancel.OnClick := Self.HandleCancelClick;
  View.OnKeyPress := Self.HandleKeyPress;
end;

function TBaseOkCancelDialogController.GetCancel: boolean;
begin
  result := FModalResult = mrCancel;
end;

function TBaseOkCancelDialogController.GetOk: boolean;
begin
  result := FModalResult = mrOk;
end;

procedure TBaseOkCancelDialogController.HandleCancelClick(Sender: TObject);
begin
  View.ModalResult := mrCancel;
end;

procedure TBaseOkCancelDialogController.HandleKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    View.ModalResult := mrCancel;
  end;
end;

procedure TBaseOkCancelDialogController.HandleOKClick(Sender: TObject);
begin
//  Model.NotifyObservers;
  View.ModalResult := mrOk;
end;

function TBaseOkCancelDialogController.Model: TBaseMapObject;
begin
  result := inherited Model as TBaseMapObject;
end;

procedure TBaseOkCancelDialogController.SetActive(const AValue: Boolean);
begin
  inherited;

  if AValue then
    FModalResult := View.ShowModal;
end;

procedure TBaseOkCancelDialogController.Update(ASubject: TtiObject);
begin

end;

procedure TBaseOkCancelDialogController.Update(ASubject: TtiObject; AOperation: TNotifyOperation);
begin
  inherited;
end;

function TBaseOkCancelDialogController.View: TBaseDialogView;
begin
  Result := inherited View as TBaseDialogView;
end;

end.

