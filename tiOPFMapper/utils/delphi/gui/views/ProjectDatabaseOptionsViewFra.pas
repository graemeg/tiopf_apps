unit ProjectDatabaseOptionsViewFra;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TProjectDatabaseOptionsView = class(TFrame)
    lblEnumType: TLabel;
    Label3: TLabel;
    EnumTypeCombo: TComboBox;
    ckDoubleQuoteDBFieldNames: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
