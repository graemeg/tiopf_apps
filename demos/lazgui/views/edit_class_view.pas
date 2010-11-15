unit edit_class_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, ComCtrls, SynMemo, SynHighlighterXML, base_edit_view, types;

type
  TClassEditView = class(TBaseEditView)
    BottomPanel: TPanel;
    cboClassParent: TComboBox;
    ckAutoMap: TCheckBox;
    ckCreateList: TCheckBox;
    btnClose: TButton;
    eClassName: TEdit;
    lblClassName: TLabel;
    lblClassName1: TLabel;
    lvMappings: TListView;
    lvProps: TListView;
    lvSelects: TListView;
    lvValiadators: TListView;
    MainPanel: TPanel;
    PageControl1: TPageControl;
    synXML: TSynXMLSyn;
    tsMappings: TTabSheet;
    tsProps: TTabSheet;
    tsSelections: TTabSheet;
    tsValidators: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ClassEditView: TClassEditView;

implementation

{$R *.lfm}

uses
  mvc_base
  ,app_events
  ;

{ TClassEditView }

procedure TClassEditView.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caNone;
  btnClose.Click;
end;

end.

