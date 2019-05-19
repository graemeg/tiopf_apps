unit BaseOptionsDialogController;

interface

uses
  Classes, StdCtrls, SysUtils, tiObject, mapper, mvc_base, widget_controllers,
  AppModel, BaseOptionsDialogViewFrm, BaseOkCancelDialogController,
  Vcl.ComCtrls;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller. }
  TBaseOptionsDialogController = class(TBaseOkCancelDialogController)
  private
    procedure tvSectionsChange(Sender: TObject; Node: TTreeNode);
  protected
    procedure DoCreateMediators; override;
    procedure DoSectionChange(Node: TTreeNode); virtual;
  public
    function View: TBaseOptionsDialogView; reintroduce;
  end;

implementation


{ TBaseOptionsDialogController }

procedure TBaseOptionsDialogController.DoCreateMediators;
begin
  inherited;

  View.tvSections.OnChange := tvSectionsChange;
  View.ActiveControl := View.tvSections;
end;

procedure TBaseOptionsDialogController.DoSectionChange(Node: TTreeNode);
begin

end;

procedure TBaseOptionsDialogController.tvSectionsChange(Sender: TObject;
  Node: TTreeNode);
begin
  DoSectionChange(Node);
end;

function TBaseOptionsDialogController.View: TBaseOptionsDialogView;
begin
  result := inherited View as TBaseOptionsDialogView;
end;

end.
