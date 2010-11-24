unit param_edit_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_dialog_view, StdCtrls;

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
    lblParamTypeName: TLabel;
    cboParamTypeName: TComboBox;
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
