unit BaseDialogViewFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseViewFrm, StdCtrls;

type
  TBaseDialogView = class(TBaseViewForm)
    btnOK: TButton;
    btnCancel: TButton;
  private
  public
  end;

//var
//  BaseDialogView: TBaseDialogView;

implementation

{$R *.dfm}

end.
