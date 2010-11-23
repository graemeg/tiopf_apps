unit base_view_form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, mvc_base;

type
  TBaseViewForm = class(TForm)
  private
    FController: TMVCController;
    procedure SetController(const Value: TMVCController);
    { Private declarations }
  public
    property    Controller: TMVCController read FController write SetController;
  end;

var
  BaseViewForm: TBaseViewForm;

implementation

{$R *.dfm}

{ TBaseViewForm }

procedure TBaseViewForm.SetController(const Value: TMVCController);
begin
  FController := Value;
end;

end.
