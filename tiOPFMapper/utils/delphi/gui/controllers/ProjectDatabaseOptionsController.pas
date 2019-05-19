unit ProjectDatabaseOptionsController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  ProjectDatabaseOptionsViewFra, EventsConsts, BaseFrameController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TProjectDatabaseOptionsController = class(TBaseFrameController)
  protected
    procedure DoCreateMediators; override;
  public
    function View: TProjectDatabaseOptionsView; reintroduce;
    function Model: TMapDatabaseProjectOptions; reintroduce;
  end;

implementation

uses
  vcl_controllers, Controls, Forms;


{ TProjectDatabaseOptionsController }

procedure TProjectDatabaseOptionsController.DoCreateMediators;
begin
  inherited;

  AddController(TComboBoxIndexController.Create(Model, View.EnumTypeCombo, 'EnumerationType'));
  AddController(TCheckBoxController.Create(Model, View.ckDoubleQuoteDBFieldNames, 'DoubleQuoteDBFieldNames'));
end;

function TProjectDatabaseOptionsController.Model: TMapDatabaseProjectOptions;
begin
  result := inherited Model as TMapDatabaseProjectOptions;
end;

function TProjectDatabaseOptionsController.View: TProjectDatabaseOptionsView;
begin
  result := inherited View as TProjectDatabaseOptionsView;
end;

end.
