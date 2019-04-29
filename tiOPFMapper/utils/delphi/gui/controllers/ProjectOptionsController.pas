unit ProjectOptionsController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  ProjectOptionsViewFrm, EventsConsts, BaseOptionsDialogController, Vcl.ComCtrls,
  Vcl.Forms;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TProjectOptionsController = class(TBaseOptionsDialogController)
  private
    FCurrentCtrl: TMVCController;
    function CreateGeneralOptionsController: TMVCController;
    function CreateCodeGenerationOptionsController: TMVCController;
    function CreateDatabaseOptionsController: TMVCController;
    procedure InitSubView(AView: TFrame);
  protected
    procedure DoCreateMediators; override;
    procedure DoSectionChange(Node: TTreeNode); override;

    property CurrentCtrl: TMVCController read FCurrentCtrl;
  public
    procedure Init; override;
    procedure Update(ASubject: TtiObject; AOperation: TNotifyOperation); overload; override;
    function View: TProjectOptionsView; reintroduce;
    function Model: TMapProject; reintroduce;
  end;

implementation

uses
  vcl_controllers, Controls, BaseFrameController, ProjectGeneralOptionsController,
  ProjectCodeGenerationoptionsController, ProjectDatabaseOptionsController,
  ProjectGeneralOptionsViewFra, ProjectCodeGenerationOptionsViewFra,
  ProjectDatabaseOptionsViewFra;

{ TProjectSettingsController }

function TProjectOptionsController.CreateCodeGenerationOptionsController: TMVCController;
var
  lView: TProjectCodeGenerationOptionsView;
  lCtrl: TBaseFrameController;
begin
  lView := TProjectCodeGenerationOptionsView.Create(nil);
  InitSubView(lView);
  lCtrl := TProjectCodeGenerationOptionsController.Create(Model.CodeGenerationOptions, lView);
  lCtrl.ParentController := Self;
  result := lCtrl;
end;

function TProjectOptionsController.CreateDatabaseOptionsController: TMVCController;
var
  lView: TProjectDatabaseOptionsView;
  lCtrl: TBaseFrameController;
begin
  lView := TProjectDatabaseOptionsView.Create(nil);
  InitSubView(lView);
  lCtrl := TProjectDatabaseOptionsController.Create(Model.DatabaseOptions, lView);
  lCtrl.ParentController := Self;
  result := lCtrl;
end;

function TProjectOptionsController.CreateGeneralOptionsController: TMVCController;
var
  lView: TProjectGeneralOptionsView;
  lCtrl: TBaseFrameController;
begin
  lView := TProjectGeneralOptionsView.Create(nil);
  InitSubView(lView);
  lCtrl := TProjectGeneralOptionsController.Create(Model.GeneralOptions, lView);
  lCtrl.ParentController := Self;
  result := lCtrl;
end;

procedure TProjectOptionsController.DoCreateMediators;
begin
  inherited;
end;

procedure TProjectOptionsController.DoSectionChange(Node: TTreeNode);
begin
  inherited;

  if Assigned(FCurrentCtrl) then
  begin
    FCurrentCtrl.Active := False;
    FreeAndNil(FCurrentCtrl);
  end;

  if Assigned(View.tvSections.Selected) then
  begin
    case View.tvSections.Selected.SelectedIndex of
      0: FCurrentCtrl := CreateGeneralOptionsController;
      1: FCurrentCtrl := CreateCodeGenerationOptionsController;
      2: FCurrentCtrl := CreateDatabaseOptionsController;
    end;

    if Assigned(FCurrentCtrl) then
    begin
      FCurrentCtrl.Init;
      FCurrentCtrl.Active := True;
    end;
  end;
end;

procedure TProjectOptionsController.Init;
begin
  inherited;

  FCurrentCtrl := nil;

  View.tvSections.Select(View.tvSections.Items[0]);
  View.tvSections.Selected := View.tvSections.Items[0];
end;

procedure TProjectOptionsController.InitSubView(AView: TFrame);
var
  lScrollBox: TScrollBox;
begin
  if Assigned(AView) then
  begin
    lScrollBox := View.scbOptionsPropertiesScrollArea;

    lScrollBox.VertScrollBar.Position := 0;
    lScrollBox.HorzScrollBar.Position := 0;

    AView.Parent := lScrollBox;
    AView.Left := 0;
    AView.Top := 0;
  end;
end;

function TProjectOptionsController.Model: TMapProject;
begin
  result := inherited Model as TMapProject;
end;

procedure TProjectOptionsController.Update(ASubject: TtiObject; AOperation: TNotifyOperation);
begin
  inherited;

end;

function TProjectOptionsController.View: TProjectOptionsView;
begin
  Result := inherited View as TProjectOptionsView;
end;

end.

