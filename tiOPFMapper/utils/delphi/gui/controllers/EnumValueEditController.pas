unit EnumValueEditController;

interface

uses
  Classes, SysUtils, tiObject, mapper, mvc_base, widget_controllers, AppModel,
  EnumValueEditViewFrm, BaseOkCancelDialogController;

type
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Enum Value edit controller. }
  TEnumValueEditController = class(TBaseOkCancelDialogController)
  protected
    procedure DoCreateMediators; override;
    procedure HandleOKClick(Sender: TObject); override;
  public
    function Model: TMapEnumValue; reintroduce;
    function View: TEnumValueEditView; reintroduce;
  end;

implementation

uses
  vcl_controllers, StdCtrls, Controls, Dialogs;

{ TEnumValueEditController }

procedure TEnumValueEditController.DoCreateMediators;
begin
  inherited;

  AddController(TEditController.Create(Model, View.eName, 'EnumValueName'));
  AddController(TEditController.Create(Model, View.eValue, 'EnumValue'));
end;

procedure TEnumValueEditController.HandleOKClick(Sender: TObject);
var
  lMsg: string;
begin
  if not Model.IsValid(lMsg) then
  begin
    MessageDlg('Error(s): ' + sLineBreak + lMsg, mtError, [mbOK], 0);
    exit;
  end;

  inherited;
end;

function TEnumValueEditController.Model: TMapEnumValue;
begin
  Result := inherited Model as TMapEnumValue;
end;

function TEnumValueEditController.View: TEnumValueEditView;
begin
  result := inherited View as TEnumValueEditView;
end;

end.

