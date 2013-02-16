unit mapper_mainform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls, base_view_form, app_mdl;

type
  TMainForm = class(TBaseViewForm)
    mmMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuSaveAs: TMenuItem;
    mnuClose: TMenuItem;
    mnuSettings: TMenuItem;
    pnlClientAlign: TPanel;
    pnlLeftAlign: TPanel;
    UnitLabel: TLabel;
    lvUnits: TListView;
    splLeft: TSplitter;
    CurrentUnitLabel: TLabel;
    MainPageControl: TPageControl;
    tsClasses: TTabSheet;
    tsEnums: TTabSheet;
    pmClasses: TPopupMenu;
    mnuNewClass: TMenuItem;
    mnuEditClass: TMenuItem;
    mnuDeleteClass: TMenuItem;
    pmUnits: TPopupMenu;
    mnuAddUnit: TMenuItem;
    mnuUnitChangeName: TMenuItem;
    mnuDeleteUnit: TMenuItem;
    lvEnums: TListView;
    lvClasses: TListView;
    mnuNewProject: TMenuItem;
    statMain: TStatusBar;
    pmEnums: TPopupMenu;
    mnuNewEnum: TMenuItem;
    mnuEditEnum: TMenuItem;
    mnuDeleteEnum: TMenuItem;
    mnuExit: TMenuItem;
    mnuGenerate: TMenuItem;
    mnuMRU: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  app_ctrl
  ,app_commands
  ,event_const
  ;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  lModel: TAppModel;
begin

  MainPageControl.ActivePageIndex := 0;

  lModel := TAppModel.Instance;
  Controller := TAppController.Create(lModel, self);
  RegisterCommands(Controller);
  Controller.Init;
  Controller.Active := True;

  if Assigned(TAppModel.Instance.OnProjectUnloaded) then
    TAppModel.Instance.OnProjectUnloaded(TAppModel.Instance);
  if Assigned(TAppModel.Instance.OnAppLoaded) then
    TAppModel.Instance.OnAppLoaded(TAppModel.Instance);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(TAppModel.Instance.OnBeforeAppTerminate) then
    TAppModel.Instance.OnBeforeAppTerminate(TAppModel.Instance);

  Controller.Active := False;
  Controller.Free;
end;

end.
