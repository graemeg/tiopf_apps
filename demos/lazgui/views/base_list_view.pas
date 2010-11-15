unit base_list_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, StdCtrls, Menus, base_form_view;

type
  TBaseListView = class(TBaseFormView)
    btnClose: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    btnNew: TButton;
    lblCount: TLabel;
    List: TListView;
    mnuDelete: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFilter: TMenuItem;
    mnuNew: TMenuItem;
    pmShorts: TPopupMenu;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  BaseListView: TBaseListView;

implementation

{$R *.lfm}

end.

