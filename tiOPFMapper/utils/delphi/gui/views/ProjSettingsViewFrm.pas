unit ProjSettingsViewFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDialogViewFrm, StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls;

type
  TProjectSettingsView = class(TBaseDialogView)
    pgcOptions: TPageControl;
    shtGeneral: TTabSheet;
    shtCode: TTabSheet;
    Label1: TLabel;
    edtIndentationWidth: TSpinEdit;
    EnumTypeCombo: TComboBox;
    lblEnumType: TLabel;
    lblProjName: TLabel;
    eProjName: TEdit;
    lblOutDir: TLabel;
    eOutDir: TEdit;
    ckDoubleQuoteDBFieldNames: TCheckBox;
    eCurrentPathAndFileName: TEdit;
    Label2: TLabel;
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
