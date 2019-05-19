unit EnumEditViewFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDialogViewFrm, StdCtrls, ComCtrls, Menus;

type
  TEnumEditView = class(TBaseDialogView)
    eEnumName: TEdit;
    lblEnumName: TLabel;
    lvValue: TListView;
    lblEnumValues: TLabel;
    pmValues: TPopupMenu;
    mnuNewValue: TMenuItem;
    mnuEditValue: TMenuItem;
    mnuDeleteValue: TMenuItem;
    ckCreateEnumerationSet: TCheckBox;
    eEnumerationSetName: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EnumEditView: TEnumEditView;

implementation

{$R *.dfm}

end.
