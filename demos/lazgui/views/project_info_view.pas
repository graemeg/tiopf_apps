unit project_info_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, base_edit_view;

type
  TProjectInfoEdit = class(TBaseEditView)
    EnumTypeCombo: TComboBox;
    ProjectNameEdit: TEdit;
    OutputDirEdit: TEdit;
    ProjectNameLabel: TLabel;
    EnumTypeLabel: TLabel;
    OutputDirLabel: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ProjectInfoEdit: TProjectInfoEdit;

implementation

{$R *.lfm}

uses
  mvc_base
  ,event_const
  ,app_events
  ;

{ TProjectInfoEdit }

procedure TProjectInfoEdit.btnOKClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(TRemoveSubControllerEvent.Create(self, 'project_settings'));
end;

procedure TProjectInfoEdit.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.

