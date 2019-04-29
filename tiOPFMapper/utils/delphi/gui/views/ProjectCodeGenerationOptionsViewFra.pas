unit ProjectCodeGenerationOptionsViewFra;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Samples.Spin;

type
  TProjectCodeGenerationOptionsView = class(TFrame)
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edtIndentationWidth: TSpinEdit;
    edtBeginEndTabs: TSpinEdit;
    edtMaxEditoCodeWidth: TSpinEdit;
    edtVisibilityTabs: TSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
