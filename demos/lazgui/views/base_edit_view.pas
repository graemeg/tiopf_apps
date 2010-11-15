unit base_edit_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  base_form_view, mvc_base;

type
  TBaseEditView = class(TBaseFormView)
    btnCancel: TButton;
    btnOK: TButton;
    btnSaveNew: TButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FController: TMVCController;
    procedure SetController(const AValue: TMVCController);
    { private declarations }
  public
    property    Controller: TMVCController read FController write SetController;
  end; 

var
  BaseEditView: TBaseEditView;

implementation

{$R *.lfm}

{ TBaseEditView }

procedure TBaseEditView.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caFree;
end;

procedure TBaseEditView.SetController(const AValue: TMVCController);
begin
  if FController=AValue then exit;
  FController:=AValue;
end;

end.

