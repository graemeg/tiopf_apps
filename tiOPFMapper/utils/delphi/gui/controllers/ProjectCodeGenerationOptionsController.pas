unit ProjectCodeGenerationOptionsController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  ProjectCodeGenerationOptionsViewFra, EventsConsts, BaseFrameController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TProjectCodeGenerationOptionsController = class(TBaseFrameController)
  protected
    procedure DoCreateMediators; override;
  public
    function View: TProjectCodeGenerationOptionsView; reintroduce;
    function Model: TMapCodeGenerationProjectOptions; reintroduce;
  end;

implementation

uses
  vcl_controllers, Controls, Forms;


{ TProjectGeneralOptionsController }

procedure TProjectCodeGenerationOptionsController.DoCreateMediators;
begin
  inherited;

  AddController(TSpinEditController.Create(Model, View.edtIndentationWidth, 'TabSpaces'));
  AddController(TSpinEditController.Create(Model, View.edtBeginEndTabs, 'BeginEndTabs'));
  AddController(TSpinEditController.Create(Model, View.edtMaxEditoCodeWidth, 'MaxEditorCodeWidth'));
  AddController(TSpinEditController.Create(Model, View.edtVisibilityTabs, 'VisibilityTabs'));
end;

function TProjectCodeGenerationOptionsController.Model: TMapCodeGenerationProjectOptions;
begin
  result := inherited Model as TMapCodeGenerationProjectOptions;
end;

function TProjectCodeGenerationOptionsController.View: TProjectCodeGenerationOptionsView;
begin
  result := inherited View as TProjectCodeGenerationOptionsView;
end;

end.
