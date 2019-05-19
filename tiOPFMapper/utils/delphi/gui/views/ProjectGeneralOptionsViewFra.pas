unit ProjectGeneralOptionsViewFra;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TProjectGeneralOptionsView = class(TFrame)
    lblProjName: TLabel;
    lblOutDir: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    eProjName: TEdit;
    eOutDir: TEdit;
    eCurrentPathAndFileName: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
