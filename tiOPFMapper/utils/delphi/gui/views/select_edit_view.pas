unit select_edit_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_dialog_view, StdCtrls, ComCtrls, Menus;

type
  TSelectEditView = class(TBaseDialogView)
    lblSQL: TLabel;
    memSQL: TMemo;
    lvParams: TListView;
    lblParams: TLabel;
    eName: TEdit;
    lblSelectName: TLabel;
    pmParams: TPopupMenu;
    mnuNewParam: TMenuItem;
    mnuEditParam: TMenuItem;
    mnuDeleteParam: TMenuItem;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectEditView: TSelectEditView;

implementation

{$R *.dfm}

end.
