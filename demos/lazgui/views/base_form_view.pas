unit base_form_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus;

type
  TBaseFormView = class(TForm)
    mnuCloseOthers: TMenuItem;
    mnuCloseTab: TMenuItem;
    pmCloseMenu: TPopupMenu;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  BaseFormView: TBaseFormView;

implementation

{$R *.lfm}

end.

