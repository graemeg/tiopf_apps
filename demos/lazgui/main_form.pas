unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Menus, SynMemo, SynHighlighterXML, SynEdit, mvc_base;

type
  TMainForm = class(TForm)
    LeftPanel: TPanel;
    ListView1: TListView;
    lvUnits: TListView;
    lvEnums: TListView;
    lvClasses: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuDeleteClass: TMenuItem;
    mnuEditClass: TMenuItem;
    mnuNewClass: TMenuItem;
    mnuProjSettings: TMenuItem;
    mnuExit: TMenuItem;
    mnuSaveAs: TMenuItem;
    mnuDeleteUnit: TMenuItem;
    mnuAddUnit: TMenuItem;
    mnuUnitChangeName: TMenuItem;
    mnuSaveProj: TMenuItem;
    mnuCloseProj: TMenuItem;
    mnuOpenProj: TMenuItem;
    LeftSplitter: TSplitter;
    ClientPanel: TPanel;
    MainPageControl: TPageControl;
    popClasses: TPopupMenu;
    popUnits: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SBMain: TStatusBar;
    tsEnums: TTabSheet;
    tsUnits: TTabSheet;
    UnitLabel: TLabel;
    CurrentUnitLabel: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvClassesDblClick(Sender: TObject);
    procedure lvUnitsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mnuAddUnitClick(Sender: TObject);
    procedure mnuCloseProjClick(Sender: TObject);
    procedure mnuEditClassClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuOpenProjClick(Sender: TObject);
    procedure mnuProjSettingsClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure mnuSaveProjClick(Sender: TObject);
    procedure popClassesPopup(Sender: TObject);
    procedure popUnitsPopup(Sender: TObject);
  private
    FController: TMVCController;
  public
    procedure   UpdateViewStatus(const AStatus: string);
    procedure   UpdateCurrentUnitName(const AStatus: string);
    procedure   UpdateViewFileName(const AFileName: string);
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  app_ctl
  ,app_model
  ,app_cmds
  ,event_const
  ;

{ TMainForm }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  gEventManager.DispatchEvent(gCreateQuickEvent(BEFORE_APP_TERM, self));
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  lAppCtrl: TAppController;
begin
  // Necessary to avoid flicker with ListViews
  DoubleBuffered := true;
  LeftPanel.DoubleBuffered := true;
  ClientPanel.DoubleBuffered := true;

  lAppCtrl := TAppController.Create(TAppModel.Instance, self);
  FController := lAppCtrl;
  // setup commands
  app_cmds.RegisterApplicationCommands(TAppController(FController));

  if not FController.Active then
    begin
      FController.Init;
      FController.Active := true;
      TAppModel.Instance.State := mpsClosed;
      TAppModel.Instance.NotifyObservers;
    end;

  gEventManager.DispatchEvent(gCreateQuickEvent(PROJ_CLOSED, self));

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FController <> nil then
    begin
      if FController.Active then
        FController.Active := false;
      FController.Free;
    end;
end;

procedure TMainForm.lvClassesDblClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_EDIT_CLASS, self));
end;

procedure TMainForm.lvUnitsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if lvUnits.Selected <> nil then
    gEventManager.DispatchEvent(gCreateQuickEvent(UNIT_SELECTED, self));
end;

procedure TMainForm.mnuAddUnitClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_ADD_UNIT, self));
end;

procedure TMainForm.mnuCloseProjClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_CLOSE_PROJECT, self));
end;

procedure TMainForm.mnuEditClassClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_EDIT_CLASS, self));
end;

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_CLOSE_APP, self));
end;

procedure TMainForm.mnuOpenProjClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_LOAD_PROJECT, self));
end;

procedure TMainForm.mnuProjSettingsClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_EDIT_PROJ_SETTINGS, self));
end;

procedure TMainForm.mnuSaveAsClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_SAVE_AS, self));
end;

procedure TMainForm.mnuSaveProjClick(Sender: TObject);
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_SAVE, self));
end;

procedure TMainForm.popClassesPopup(Sender: TObject);
var
  lCtr: Integer;
  lEnabled: boolean;
begin
  if TAppModel(FController.Model).State = mpsClosed then
    begin
      for lCtr := 0 to popClasses.Items.Count - 1 do
        begin
          popClasses.Items[lCtr].Enabled := false;
        end;
      exit;
    end;

  mnuNewClass.Enabled := true;

  lEnabled := lvClasses.Selected <> nil;

  mnuEditClass.Enabled := lEnabled;
  mnuDeleteClass.Enabled := lEnabled;
end;

procedure TMainForm.popUnitsPopup(Sender: TObject);
var
  lEnabled: boolean;
  lCtr: integer;
begin
  if TAppModel(FController.Model).State = mpsClosed then
    begin
      for lCtr := 0 to popUnits.Items.Count - 1 do
        begin
          popUnits.Items[lCtr].Enabled := false;
        end;
      exit;
    end;

  lEnabled := lvUnits.Selected <> nil;

  mnuUnitChangeName.Enabled := lEnabled;
  mnuDeleteUnit.Enabled := lEnabled;
  mnuAddUnit.Enabled := true;
end;

procedure TMainForm.UpdateCurrentUnitName(const AStatus: string);
begin
  CurrentUnitLabel.Caption := AStatus;
end;

procedure TMainForm.UpdateViewFileName(const AFileName: string);
begin
  SBMain.Panels[0].Text := AFileName;
end;

procedure TMainForm.UpdateViewStatus(const AStatus: string);
begin
  SBMain.Panels[0].Text := AStatus;
end;

end.

