unit classprop_edit_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_dialog_view, StdCtrls;

type
  TClassPropEditView = class(TBaseDialogView)
    ePropName: TEdit;
    lblBaseClassName: TLabel;
    lblPropType: TLabel;
    cboPropType: TComboBox;
    ckReadOnly: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ClassPropEditView: TClassPropEditView;

implementation

{$R *.dfm}

end.
