unit enumvalue_edit_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_dialog_view, StdCtrls;

type
  TEnumValueEditView = class(TBaseDialogView)
    eName: TEdit;
    lblValueName: TLabel;
    lblValue: TLabel;
    eValue: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EnumValueEditView: TEnumValueEditView;

implementation

{$R *.dfm}

end.
