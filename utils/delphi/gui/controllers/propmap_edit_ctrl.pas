unit propmap_edit_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_mdl
  ,propmap_edit_view
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TPropMapEditController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleOKClick(Sender: TObject);
  public
    function    Model: TPropMapping; reintroduce;
    function    View: TPropMapEditView; reintroduce;
    constructor Create(AModel: TPropMapping; AView: TPropMapEditView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;

implementation

uses
  vcl_controllers
  ,StdCtrls
  ,Controls
  ,Dialogs
  ;

{ TClassPropEditController }

constructor TPropMapEditController.Create(AModel: TPropMapping;
  AView: TPropMapEditView);
begin
  inherited Create(AModel, AView);
end;

destructor TPropMapEditController.Destroy;
begin
  View.Hide;
  View.Free;
  inherited;
end;

procedure TPropMapEditController.DoCreateMediators;
var
  lCboCtrl: TComboBoxController;
  lMapClass: TMapClassDef;
begin
  AddController(TEditController.Create(Model, View.eFieldName, 'FieldName'));
  AddController(TComboBoxIndexController.Create(Model, View.cboPropType, 'PropertyType'));

  lMapClass := TMapClassDef(ParentController.Model);

  lCboCtrl := TComboBoxController.Create(Model, View.cboPropName, 'PropName');
  lCboCtrl.ListData := lMapClass.ClassProps;
  lCboCtrl.ListDisplayProp := 'Name';
  lCboCtrl.ListValueProp := 'Name';
  AddController(lCboCtrl);

  // Native events
  View.btnOK.OnClick := Self.HandleOKClick;


end;

procedure TPropMapEditController.HandleOKClick(Sender: TObject);
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

function TPropMapEditController.Model: TPropMapping;
begin
  result := inherited Model as TPropMapping;
end;

procedure TPropMapEditController.SetActive(const AValue: Boolean);
begin
  inherited SetActive(AValue);
  if Active then
    View.ShowModal;
end;

function TPropMapEditController.View: TPropMapEditView;
begin
  result := inherited View as TPropMapEditView;
end;

end.
