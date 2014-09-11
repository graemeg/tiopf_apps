unit enum_edit_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_mdl
  ,enum_edit_view
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Edit an enumeration. }
  TEnumEditController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleOKClick(Sender: TObject);
  public
    function    Model: TMapEnum; reintroduce;
    function    View: TEnumEditView; reintroduce;
    constructor Create(AModel: TMapEnum; AView: TEnumEditView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;

implementation

uses
  vcl_controllers
  ,StdCtrls
  ,Controls
  ,Dialogs
  ,enum_edit_cmd
  ;

{ TEnumEditController }

constructor TEnumEditController.Create(AModel: TMapEnum; AView: TEnumEditView);
begin
  inherited Create(AModel, AView);
  enum_edit_cmd.RegisterCommands(self);
end;

destructor TEnumEditController.Destroy;
begin
  View.Hide;
  View.Free;
  inherited;
end;

procedure TEnumEditController.DoCreateMediators;
var
  lListCtrl: TListViewController;
begin
  inherited;

  AddController(TEditController.Create(Model, View.eEnumName, 'EnumName'));

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

  // Events
  View.btnOK.OnClick := self.HandleOKClick;
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

procedure TEnumEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  if Active then
    View.ShowModal;
end;

function TEnumEditController.View: TEnumEditView;
begin
  Result := inherited View as TEnumEditView;
end;

end.
