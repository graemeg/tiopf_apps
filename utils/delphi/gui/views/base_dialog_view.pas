unit base_dialog_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_view_form, StdCtrls;

type
  TBaseDialogView = class(TBaseViewForm)
    btnOK: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BaseDialogView: TBaseDialogView;

implementation

{$R *.dfm}

end.
