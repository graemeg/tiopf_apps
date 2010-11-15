unit app_ctl;

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
  ,main_form
  ,event_const
  ;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TAppController = class(TMVCController)
  protected
    procedure   DoCreateModel; override;
    procedure   DoCreateMediators; override;
    procedure   SetActive(const AValue: Boolean); override;
  public
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation);
       overload; override;
    function    Model: TAppModel; reintroduce;
    function    View: TMainForm; reintroduce;
    constructor Create(AModel: TObject; AView: TObject); overload; override;
    destructor  Destroy; override;
  end;


implementation
uses
  lcl_controllers
  ;

{ TAppController }

constructor TAppController.Create(AModel: TObject; AView: TObject);
begin
  inherited Create(AModel, AView);

  gEventManager.AddListener(self, nil, PROJ_CLOSED);
  gEventManager.AddListener(self, nil, PROJ_LOADED);
  gEventManager.AddListener(self, nil, BEFORE_PROJ_CLOSE);
end;

destructor TAppController.Destroy;
begin
  gEventManager.RemoveListenersByTarget(self);
  inherited Destroy;
end;

procedure TAppController.DoCreateMediators;
var
  lListCtrl: TListViewController;
begin
  lListCtrl := TListViewController.Create(Model.CurrentClasses, View.lvClasses, '');
  lListCtrl.Name := 'classes';
  lListCtrl.GroupName := 'units_dependant';  // group this controller
  lListCtrl.AddRenderer(TBooleanRenderer.Create);

  with lListCtrl.AddNewCol('BaseClassName') do
    begin
      Width := 200;
      ColCaption := 'Class Name';
    end;

  with lListCtrl.AddNewCol('BaseClassParent') do
    begin
      Width := 200;
      ColCaption := 'Parent';
    end;

  with lListCtrl.AddNewCol('AutoMap') do
    begin
      Width := 100;
      ColCaption := 'AutoMap';
      RendererIndex := 0;
    end;

  with lListCtrl.AddNewCol('AutoCreateListClass') do
    begin
      Width := 100;
      ColCaption := 'List';
      RendererIndex := 0;
    end;

  AddController(lListCtrl);

  // Enumerations List mediator
  lListCtrl := TListViewController.Create(Model.CurrentEnums, View.lvEnums, '');
  lListCtrl.Name := 'enums';
  lListCtrl.GroupName := 'units_dependant';  // group this controller

  with lListCtrl.AddNewCol('EnumName') do
    begin
      Width := 300;
      ColCaption := 'Enum Name';
    end;

  AddController(lListCtrl);

  // Units List View
  lListCtrl := TListViewController.Create(Model.Project.Units, View.lvUnits, '');
  lListCtrl.Name := 'units';

  with lListCtrl.AddNewCol('UnitName') do
    begin
      Width := 200;
      ColCaption := 'Unit Name';
    end;

  AddController(lListCtrl);


end;

procedure TAppController.DoCreateModel;
begin
  inherited DoCreateModel;
end;

function TAppController.Model: TAppModel;
begin
  result := inherited Model as TAppModel;
end;

procedure TAppController.SetActive(const AValue: Boolean);
begin
  inherited SetActive(AValue);
  Model.Project.ClearAll;
  Model.ClearUnitSelection;
end;

procedure TAppController.Update(ASubject: TtiObject;
  AOperation: TNotifyOperation);
begin
  if not (ASubject is TMVCEvent) then
    exit;

  //if TMVCEvent(ASubject).Name = BEFORE_APP_TERM then
  //  begin
  //    gEventManager.Terminated := true;
  //    Controllers.ChangeAllActive(true);
  //    Controllers.Clear;
  //  end;

end;

function TAppController.View: TMainForm;
begin
  result := inherited View as TMainForm;
end;

end.

