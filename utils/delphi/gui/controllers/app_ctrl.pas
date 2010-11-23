unit app_ctrl;

interface
uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,app_mdl
  ,mvc_base
  ,widget_controllers
  ,mapper_mainform
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
  event_const
  ,vcl_controllers
  ;

{ TAppController }

constructor TAppController.Create(AModel, AView: TObject);
begin
  inherited;
  // hook into mvc events
  gEventManager.AddListener(self, nil, PROJ_CLOSED);
  gEventManager.AddListener(self, nil, ON_PROJ_LOADED);
  gEventManager.AddListener(self, nil, DO_CLOSE_SUBCONTROLLER);
end;

destructor TAppController.Destroy;
begin
  gEventManager.RemoveListenersByTarget(self);
  inherited;
end;

procedure TAppController.DoCreateMediators;
var
  lListCtrl: TListViewController;
begin
  lListCtrl := TListViewController.Create(Model.Project.Units, View.lvUnits, '');
  lListCtrl.Name := 'units_ctl';
  lListCtrl.GroupName := 'main_lists';

  with lListCtrl.AddNewCol('Name') do
    begin
      Width := 200;
      ColCaption := 'Unit Name';
    end;

  AddController(lListCtrl);

  lListCtrl := TListViewController.Create(Model.CurrentClasses, View.lvClasses, '');
  lListCtrl.Name := 'classes_ctl';
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
  lListCtrl.Name := 'enums_ctl';
  lListCtrl.GroupName := 'units_dependant';  // group this controller

  with lListCtrl.AddNewCol('EnumName') do
    begin
      Width := 300;
      ColCaption := 'Enum Name';
    end;

  AddController(lListCtrl);

end;

procedure TAppController.DoCreateModel;
begin
  inherited;

end;

function TAppController.Model: TAppModel;
begin
  result := inherited Model as TAppModel;
end;

procedure TAppController.SetActive(const AValue: Boolean);
begin
  inherited;

end;

procedure TAppController.Update(ASubject: TtiObject;
  AOperation: TNotifyOperation);
begin

end;

function TAppController.View: TMainForm;
begin
  Result := inherited View as TMainForm;
end;

end.
