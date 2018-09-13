unit project_settings_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_mdl
  ,proj_settings_view
  ,event_const
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TProjectSettingsController = class(TMVCController)
  protected
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
    procedure   HandleOKClick(Sender: TObject);
  public
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation); overload; override;
    function    Model: TMapProject; reintroduce;
    function    View: TProjectSettingsview; reintroduce;
    constructor Create(AModel: TMapProject; AView: TProjectSettingsView); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;

implementation

uses
  vcl_controllers
  ,Controls
  ,Forms;

{ TProjectSettingsController }

constructor TProjectSettingsController.Create(AModel: TMapProject;
  AView: TProjectSettingsView);
begin
  inherited Create(AModel, AView);
end;

destructor TProjectSettingsController.Destroy;
begin
  View.Hide;
  View.Free;
  inherited;
end;

procedure TProjectSettingsController.DoCreateMediators;
var
  lCboCtrl: TComboBoxIndexController;
begin
  AddController(TEditController.Create(Model, View.eProjName, 'ProjectName'));
  AddController(TEditController.Create(Model, View.eOutDir, 'OrigOutDirectory'));
  AddController(TComboBoxIndexController.Create(Model, View.EnumTypeCombo, 'EnumType'));
  AddController(TLabelController.Create(Model, View.lblFileName, 'FileName'));
  AddController(TSpinEditController.Create(Model, View.edtIndentationWidth, 'TabSpaces'));

  // hook up native event
  View.btnOK.OnClick := Self.HandleOKClick;
end;

procedure TProjectSettingsController.HandleOKClick(Sender: TObject);
begin
  view.ModalResult := mrOk;
end;

function TProjectSettingsController.Model: TMapProject;
begin
  result := inherited Model as TMapProject;
end;

procedure TProjectSettingsController.SetActive(const AValue: Boolean);
begin
  inherited;
  if AValue then
    View.ShowModal;
end;

procedure TProjectSettingsController.Update(ASubject: TtiObject;
  AOperation: TNotifyOperation);
begin
  inherited;
end;

function TProjectSettingsController.View: TProjectSettingsview;
begin
  Result := inherited View as TProjectSettingsView;
end;

end.
