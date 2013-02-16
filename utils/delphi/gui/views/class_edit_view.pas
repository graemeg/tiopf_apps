unit class_edit_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_dialog_view, StdCtrls, ComCtrls, Menus;

type
  TClassEditView = class(TBaseDialogView)
    eBaseClassName: TEdit;
    lblBaseClassName: TLabel;
    eBaseClassParent: TEdit;
    lblBaseClassParent: TLabel;
    ckAutoMap: TCheckBox;
    ckAutoCreateList: TCheckBox;
    pcClass: TPageControl;
    tsProps: TTabSheet;
    tsMapping: TTabSheet;
    tsValidators: TTabSheet;
    tsSelections: TTabSheet;
    lvMapping: TListView;
    lvProps: TListView;
    lvValidators: TListView;
    lvSelections: TListView;
    pmProps: TPopupMenu;
    mnuAddProp: TMenuItem;
    mnuEditProp: TMenuItem;
    mnuDeleteProp: TMenuItem;
    pmMapping: TPopupMenu;
    mnuNewMap: TMenuItem;
    mnuEditMap: TMenuItem;
    mnuDeleteMap: TMenuItem;
    pmValidators: TPopupMenu;
    mnuNewValidator: TMenuItem;
    mnuEditValidator: TMenuItem;
    mnuDeleteValidator: TMenuItem;
    pmSelects: TPopupMenu;
    mnuNewSelect: TMenuItem;
    mnuEditSelect: TMenuItem;
    mnuDeleteSelect: TMenuItem;
    lblPropMappings: TLabel;
    gbORM: TGroupBox;
    ePKField: TEdit;
    cboOIDType: TComboBox;
    eTableName: TEdit;
    ePKName: TEdit;
    lblTableName: TLabel;
    lblPropType: TLabel;
    lblPKField: TLabel;
    lblPKName: TLabel;
    mnuSortMapsByProp: TMenuItem;
    mnuSortProps: TMenuItem;
    mnuSortMapsByField: TMenuItem;
    mnuMapSort: TMenuItem;
    mnuPropSort: TMenuItem;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ClassEditView: TClassEditView;

implementation

{$R *.dfm}

procedure TClassEditView.FormCreate(Sender: TObject);
begin
  inherited;
  pcClass.ActivePageIndex := 0;
end;

end.
