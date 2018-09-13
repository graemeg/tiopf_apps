unit proj_settings_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_dialog_view, StdCtrls, Vcl.Samples.Spin;

type
  TProjectSettingsView = class(TBaseDialogView)
    lblProjName: TLabel;
    eProjName: TEdit;
    lblFileName: TLabel;
    eOutDir: TEdit;
    lblOutDir: TLabel;
    EnumTypeCombo: TComboBox;
    lblEnumType: TLabel;
    edtIndentationWidth: TSpinEdit;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProjectSettingsView: TProjectSettingsView;

implementation

{$R *.dfm}

end.
