unit EnumValueEditViewFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDialogViewFrm, StdCtrls;

type
  TEnumValueEditView = class(TBaseDialogView)
    eName: TEdit;
    lblValueName: TLabel;
    lblValue: TLabel;
    eValue: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EnumValueEditView: TEnumValueEditView;

implementation

{$R *.dfm}

end.
