unit enumvalue_edit_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_mdl
  ,enumvalue_edit_view
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Enum Value edit controller. }
  TEnumValueEditController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleOKClick(Sender: TObject);
  public
    function    Model: TMapEnumValue; reintroduce;
    function    View: TEnumValueEditView; reintroduce;
    constructor Create(AModel: TMapEnumValue; AView: TEnumValueEditView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;

implementation

uses
  vcl_controllers
  ,StdCtrls
  ,Controls
  ,Dialogs
  ;

{ TEnumValueEditController }

constructor TEnumValueEditController.Create(AModel: TMapEnumValue;
  AView: TEnumValueEditView);
begin
  inherited Create(AModel, AView);
end;

destructor TEnumValueEditController.Destroy;
begin
  View.Hide;
  View.Free;
  inherited;
end;

procedure TEnumValueEditController.DoCreateMediators;
begin
  AddController(TEditController.Create(Model, View.eName, 'EnumValueName'));
  AddController(TEditController.Create(Model, View.eValue, 'EnumValue'));

  View.btnOK.OnClick := self.HandleOKClick;
end;

procedure TEnumValueEditController.HandleOKClick(Sender: TObject);
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

function TEnumValueEditController.Model: TMapEnumValue;
begin
  Result := inherited Model as TMapEnumValue;
end;

procedure TEnumValueEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    View.ShowModal;
end;

function TEnumValueEditController.View: TEnumValueEditView;
begin
  result := inherited View as TEnumValueEditView;
end;

end.
