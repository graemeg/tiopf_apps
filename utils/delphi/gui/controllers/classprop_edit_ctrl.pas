unit classprop_edit_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_mdl
  ,classprop_edit_view
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TClassPropEditController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleOKClick(Sender: TObject);
  public
    function    Model: TMapClassProp; reintroduce;
    function    View: TClassPropEditView; reintroduce;
    constructor Create(AModel: TMapClassProp; AView: TClassPropEditView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;

implementation

uses
  vcl_controllers
  ,StdCtrls
  ,Controls
  ;

{ TClassPropEditController }

constructor TClassPropEditController.Create(AModel: TMapClassProp;
  AView: TClassPropEditView);
begin
  inherited Create(AModel, AView);
end;

destructor TClassPropEditController.Destroy;
begin
  View.Hide;
  View.Free;
  inherited;
end;

procedure TClassPropEditController.DoCreateMediators;
begin
  AddController(TEditController.Create(Model, View.ePropName, 'Name'));
  AddController(TComboBoxController.Create(Model, View.cboPropType, 'PropTypeName'));
  AddController(TCheckBoxController.Create(Model, View.ckReadOnly, 'IsReadOnly'));

  // Native events
  View.btnOK.OnClick := self.HandleOKClick;
end;

procedure TClassPropEditController.HandleOKClick(Sender: TObject);
begin
  View.ModalResult := mrOk;
end;

function TClassPropEditController.Model: TMapClassProp;
begin
  Result := inherited Model as TMapClassProp;
end;

procedure TClassPropEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    View.ShowModal;
end;

function TClassPropEditController.View: TClassPropEditView;
begin
  result := inherited View as TClassPropEditView;
end;

end.
