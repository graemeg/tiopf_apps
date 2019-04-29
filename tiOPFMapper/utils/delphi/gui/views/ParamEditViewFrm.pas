unit ParamEditViewFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDialogViewFrm, StdCtrls;

type
  TParamEditView = class(TBaseDialogView)
    eParamName: TEdit;
    lblSelectName: TLabel;
    eSQLParamName: TEdit;
    lblSQLParamName: TLabel;
    lblPropType: TLabel;
    cboParamType: TComboBox;
    cboPassBy: TComboBox;
    lblPassBy: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ParamEditView: TParamEditView;

implementation

{$R *.dfm}

end.
