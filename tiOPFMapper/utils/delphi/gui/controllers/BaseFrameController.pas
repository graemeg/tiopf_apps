unit BaseFrameController;

interface

uses
  Classes, SysUtils, Controls, tiObject, mapper, mvc_base, widget_controllers,
  AppModel, EventsConsts, VCL.Forms;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Application base controller for dialogs. }
  TBaseFrameController = class(TMVCController)
  protected
    procedure DoAfterInit; override;
    procedure SetActive(const AValue: Boolean); override;
  public
    function View: TFrame; reintroduce;
  end;

implementation

uses
  vcl_controllers, Dialogs;

{ TBaseFrameController }

procedure TBaseFrameController.DoAfterInit;
begin
  inherited;

end;

procedure TBaseFrameController.SetActive(const AValue: Boolean);
begin
  inherited;

  View.Visible := AValue;
end;

function TBaseFrameController.View: TFrame;
begin
  result := inherited View as TFrame;
end;

end.
