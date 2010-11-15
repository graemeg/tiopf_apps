unit proj_settings_ctrl; 

{$mode objfpc}{$H+}

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,widget_controllers
  ,app_model
  ,project_info_view
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
  public
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation);
       overload; override;
    function    Model: TMapProject; reintroduce;
    function    View: TProjectInfoEdit; reintroduce;
    constructor Create(AModel: TMapProject; AView: TProjectInfoEdit); reintroduce; overload; virtual;
    destructor  Destroy; override;
  end;


implementation
uses
  lcl_controllers
  ;

{ TProjectSettingsController }

constructor TProjectSettingsController.Create(AModel: TMapProject;
  AView: TProjectInfoEdit);
begin
  inherited Create(AModel, AView);
end;

destructor TProjectSettingsController.Destroy;
begin
  View.Close;
  inherited Destroy;
end;

procedure TProjectSettingsController.DoCreateMediators;
var
  lCboCtrl: TComboBoxIndexController;
begin
  AddController(TEditController.Create(Model, View.ProjectNameEdit, 'ProjectName'));

  lCboCtrl := TComboBoxIndexController.Create(Model, View.EnumTypeCombo, 'EnumType');
  AddController(lCboCtrl);

  AddController(TEditController.Create(Model, View.OutputDirEdit, 'OrigOutDirectory'));
end;

function TProjectSettingsController.Model: TMapProject;
begin
  result := inherited Model as TMapProject;
end;

procedure TProjectSettingsController.SetActive(const AValue: Boolean);
begin
  inherited SetActive(AValue);
  if AValue then
    View.ShowModal;
end;

procedure TProjectSettingsController.Update(ASubject: TtiObject;
  AOperation: TNotifyOperation);
var
  lEvent: TMVCEvent;
begin
  lEvent := TMVCEvent(ASubject);
  if (lEvent.Name = DO_CLOSE_VIEW) or (lEvent.Name = BEFORE_PROJ_CLOSE) then
    begin
      Active := false;
      gEventManager.DispatchEvent(gCreateQuickEvent(DO_CLOSE_SUBCONTROLLER, self));
    end;
end;

function TProjectSettingsController.View: TProjectInfoEdit;
begin
  result := inherited View as TProjectInfoEdit;
end;

end.

