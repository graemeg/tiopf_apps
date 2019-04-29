unit BaseOptionsDialogViewFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseDialogViewFrm, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TBaseOptionsDialogView = class(TBaseDialogView)
    pnlClient: TPanel;
    splVertical: TSplitter;
    tvSections: TTreeView;
    scbOptionsPropertiesScrollArea: TScrollBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BaseOptionsDialogView: TBaseOptionsDialogView;

implementation

{$R *.dfm}

end.
