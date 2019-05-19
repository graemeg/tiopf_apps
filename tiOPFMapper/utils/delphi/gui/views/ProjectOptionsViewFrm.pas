unit ProjectOptionsViewFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsDialogViewFrm, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TProjectOptionsView = class(TBaseOptionsDialogView)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProjectOptionsView: TProjectOptionsView;

implementation

{$R *.dfm}

end.
