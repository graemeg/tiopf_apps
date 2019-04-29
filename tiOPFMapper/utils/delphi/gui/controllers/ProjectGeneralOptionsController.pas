unit ProjectGeneralOptionsController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  ProjectGeneralOptionsViewFra, EventsConsts, BaseFrameController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TProjectGeneralOptionsController = class(TBaseFrameController)
  protected
    procedure DoCreateMediators; override;
  public
    function View: TProjectGeneralOptionsView; reintroduce;
    function Model: TMapGeneralProjectOptions; reintroduce;
  end;

implementation

uses
  vcl_controllers, Controls, Forms;


{ TProjectGeneralOptionsController }

procedure TProjectGeneralOptionsController.DoCreateMediators;
begin
  inherited;

  AddController(TEditController.Create(ParentController.Model as TMapProject, View.eCurrentPathAndFileName, 'FileName'));
  AddController(TEditController.Create(Model, View.eProjName, 'ProjectName'));
  AddController(TEditController.Create(Model, View.eOutDir, 'OrigOutDirectory'));
end;

function TProjectGeneralOptionsController.Model: TMapGeneralProjectOptions;
begin
  result := inherited Model as TMapGeneralProjectOptions;
end;

function TProjectGeneralOptionsController.View: TProjectGeneralOptionsView;
begin
  result := inherited View as TProjectGeneralOptionsView;
end;

end.
