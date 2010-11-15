unit base_list_view_ctrl;

interface
uses
  SysUtils
  ,Classes
  ,Forms
  ,tiObject
  ,tiFilteredObjectList
  ,tiOPFManager
  ,tiCriteria
  ,mvc_base
  ,mvc_criteria
  ,base_list_view
  ,base_edit_view
  ,base_edit_ctrl
  ;

type

  {: Basic controller for listbased browser views. }
  TBaseListViewController = class(TMVCController)
  private
    FEditView: TBaseEditView;
    FEdited: TtiObject;
    FEditedClone: TtiObject;
    procedure   DoSetCloned(AOriginal: TtiObject);
    procedure   DoCleanUpEdited;
    procedure   SetEdited(const Value: TtiObject);
  protected
    FFilter: TObjectFilter;
    FSimpleFilterOn: Boolean;
    procedure   ClearList; virtual;
    procedure   SaveList; virtual;
    procedure   DoAfterModelSaved; virtual;
    procedure   DoAfterObjectDelete(AObject: TtiObject); virtual;
    procedure   DoAfterObjectCreate(AObject: TtiObject); virtual;
    procedure   DoBeforeObjectDelete(AObject: TtiObject; var CancelDelete: Boolean;
      const AMsg: string); virtual;
    procedure   DoAfterObjectEdit; virtual;
    procedure   SaveObject(AObject: TtiObject); virtual;
    procedure   ReadObject(AObject: TtiObject); virtual;
    procedure   ReadList(AObject: TtiObjectList); virtual; abstract;
    {: Provides descendant controllers a chance to apply a default filter. }
    procedure   DoApplyDefaultFilter; virtual; abstract;
    procedure   DoTearmDownMediators;
    procedure   DoCreateCommands; override;
    function    DoGetObjectClass: TtiObjectClass; virtual;
    function    DoGetEditControllerClass: TMVCControllerClass; virtual;
    function    DoGetNewEditController: TMVCController; virtual;
    function    DoGetViewClass: TFormClass; virtual;
    procedure   AfterNewObjectCreate(ANewObject: TtiObject); virtual;
    {: Handles creating and setting up a new object to be edited. }
    procedure   HandleCreateCommand(Sender: TObject);
    procedure   HandleDeleteCommand(Sender: TObject);
    procedure   HandleEditCommand(Sender: TObject);
    procedure   HandleCloseCommand(Sender: TObject);
    procedure   HandleCancelEditCommand(Sender: TObject);
    procedure   HandleCommitCommand(Sender: TObject);
    procedure   HandleViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure   HandleCloseAllCmd(Sender: TObject);
    procedure   DoCreateView; override;
    procedure   DoUpdateCount;
  public
    constructor Create(AModel: TObject; AView: TObject); override;
    procedure   Update(ASubject: TtiObject); override;
    property    Edited: TtiObject read FEdited write SetEdited;
    property    Filter: TObjectFilter read FFilter;
    destructor  Destroy; override;
  end;


implementation

uses
  Dialogs
  ,Controls
  ,lcl_controllers
  ,event_const
  ;

{ TBaseListViewController }

procedure TBaseListViewController.AfterNewObjectCreate(ANewObject: TtiObject);
begin
  // dummy method
end;

procedure TBaseListViewController.ClearList;
var
  lList: TtiObjectList;
  lCounter: integer;
begin
  lList := TtiObjectList(Model);

  try
    for lCounter := lList.Count - 1 downto 0 do
      begin
        lList.Items[lCounter].DetachObserver(self);
      end;
  except on e: Exception do

  end;

  lList.Clear;
end;

constructor TBaseListViewController.Create(AModel, AView: TObject);
begin
  inherited;
  FEdited := nil;
  FEditedClone := nil;
  FFilter := TObjectFilter.Create;
end;

destructor TBaseListViewController.Destroy;
var
  lView: TBaseListView;
begin
  if Assigned(FEditView) then
    begin
      FEditView.Parent := nil;
      FEditView.Free;
    end;

  if (FEdited <> nil) and (FEdited.ObjectState = posCreate) then
    FreeAndNil(FEdited);

  if Active then
    Active := False;

  if FModel <> nil then
    begin
      ClearList;
      FModel.Free;
    end;

  if FEditedClone <> nil then
    FEditedClone.Free;

  if FFilter <> nil then
    FFilter.Free;

  if FView <> nil then
    FView.Free;

  inherited;
end;

procedure TBaseListViewController.DoAfterModelSaved;
begin
  // dummy fall through method
end;

procedure TBaseListViewController.DoAfterObjectCreate(AObject: TtiObject);
begin
  // null method
end;

procedure TBaseListViewController.DoAfterObjectDelete(AObject: TtiObject);
begin
  // null method
end;

procedure TBaseListViewController.DoAfterObjectEdit;
begin
//   null method
end;

procedure TBaseListViewController.DoBeforeObjectDelete(AObject: TtiObject;
  var CancelDelete: Boolean; const AMsg: string);
begin
  // dummy method fall through.
end;

procedure TBaseListViewController.DoCleanUpEdited;
begin
  if FEditedClone <> nil then
    FreeAndNil(FEditedClone);
  FEdited := nil;
end;

procedure TBaseListViewController.DoCreateCommands;
var
  lView: TBaseListView;
begin
  inherited;

  // Main view event hookup
  lView := TBaseListView(View);
  lView.btnClose.OnClick := @self.HandleCloseCommand;
  // edit
  lView.btnEdit.OnClick := @self.HandleEditCommand;
  lView.List.OnDblClick := @self.HandleEditCommand;
  lView.mnuEdit.OnClick := @self.HandleEditCommand;
  // delete
  lView.btnDelete.OnClick := @self.HandleDeleteCommand;
  lView.mnuDelete.OnClick := @self.HandleDeleteCommand;
  // new
  lView.btnNew.OnClick := @self.HandleCreateCommand;
  lView.mnuNew.OnClick := @self.HandleCreateCommand;

  lView.OnKeyDown := @self.HandleViewKeyDown;

  // Close menu
  lView.mnuCloseTab.OnClick := @Self.HandleCloseCommand;
  lView.mnuCloseOthers.OnClick := @self.HandleCloseAllCmd;
  // Hook up events for editing form.
//  FEditView.btnCancel.OnClick := Self.HandleCancelEditCommand;
//  FEditView.btnOK.OnClick := Self.HandleCommitCommand;
end;

procedure TBaseListViewController.DoCreateView;
begin
  inherited;
  FEditView := DoGetViewClass.Create(nil) as TBaseEditView;
  FEditView.Parent := TForm(View).Parent;
  FEditView.Left := 0;
  FEditView.Top := 0;
  FEditView.Width := FEditView.Parent.Width;
  FEditView.Height := FEditView.Parent.Height;
  FEditView.Anchors := [akLeft, akTop, akRight, akBottom];
  DoUpdateCount;
end;

function TBaseListViewController.DoGetEditControllerClass: TMVCControllerClass;
begin
  Assert(false, ClassName + 'DoGetEditController must be implemented in descendant class.');
end;

function TBaseListViewController.DoGetNewEditController: TMVCController;
begin
  result := nil;
  Assert(false, ClassName + 'DoGetNewEditController must be implemented in descendant class.');
end;

function TBaseListViewController.DoGetObjectClass: TtiObjectClass;
begin
  result := nil;
  Assert(false, ClassName + 'DoGetObjectClass must be implemented in descendant class.');
end;

function TBaseListViewController.DoGetViewClass: TFormClass;
begin
  result := nil;
  Assert(false, ClassName + '.DoGetViewClass must be implemented in descendant class.');
end;

procedure TBaseListViewController.DoSetCloned(AOriginal: TtiObject);
begin
  if FEditedClone <> nil then
    FreeAndNil(FEditedClone);
  FEditedClone := AOriginal.Clone;
end;

procedure TBaseListViewController.DoTearmDownMediators;
begin

end;

procedure TBaseListViewController.DoUpdateCount;
var
  lList: TtiObjectList;
begin
  lList := TtiObjectList(Model);


  TBaseListView(View).lblCount.Caption :=
    'Count: ' + IntToStr(lList.Count);

end;

procedure TBaseListViewController.HandleCancelEditCommand(Sender: TObject);
var
  lList: TtiObjectList;
  lCtrl: TMVCController;
begin
  lList := TtiObjectList(Model);

  // Kill the sub-controller
  lCtrl := Controllers.FindByModel(FEdited);
  lCtrl.Active := False;
  gEventManager.RemoveListenersBySource(lCtrl); // remove events
  Controllers.Remove(lCtrl);

  if FEdited.ObjectState = posCreate then
    begin
      if TtiObjectList(Model).IndexOf(FEdited) > 0 then
        TtiObjectList(Model).Remove(FEdited)
      else
        FEdited.Free;
    end
  else
    begin
      // Reassign values from clone back to the orginal.
      FEdited.Assign(FEditedClone);
    end;

  DoCleanUpEdited;

  TBaseListView(View).SetFocus;
end;

procedure TBaseListViewController.HandleCloseAllCmd(Sender: TObject);
begin
  Active := false;
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_CLOSE_ALL_SUBCONTROLLERS, Self));
end;

procedure TBaseListViewController.HandleCloseCommand(Sender: TObject);
begin
  Active := False;
  gEventManager.DispatchEvent(gCreateQuickEvent(DO_CLOSE_SUBCONTROLLER, self));
end;

procedure TBaseListViewController.HandleCommitCommand(Sender: TObject);
var
  lCtrl: TMVCController;
  lMsg: string;
begin

  //  add onl if doesn't exist
  if TtiObjectList(Model).IndexOf(FEdited) < 0 then
    begin
      TtiObjectList(Model).Add(FEdited);
      DoUpdateCount;
    end;

  lCtrl := Controllers.FindByModel(FEdited);
  lCtrl.Active := False;

  FEdited.NotifyObservers;
  // remove mvc listener
  gEventManager.RemoveListenersBySource(lCtrl);
  Controllers.Remove(lCtrl);
  DoCleanUpEdited;

  // ensure focus for this view.
  TBaseListView(View).SetFocus;
end;

procedure TBaseListViewController.HandleCreateCommand(Sender: TObject);
var
  lNewCtrl: TMVCController;
  lClass: TMVCControllerClass;
  lNewObj: TtiObject;
begin

  if FEdited <> nil then
    raise Exception.Create(ClassName + '.HandleCreateCommand: Existing object ' +
      'being edited');

  lNewObj := DoGetObjectClass.CreateNew;

  // Allow descendant classes to set defaults, etc.
  AfterNewObjectCreate(lNewObj);

  Edited := lNewObj;
  //TtiObjectList(Model).Add(lNewObj);

  lClass := DoGetEditControllerClass;
  lNewCtrl := lClass.Create(Edited, FEditView) as lClass;
  TBaseEditController(lNewCtrl).CloseAfterSave := false;
  // attach an mvc event to its commit signal
  gEventManager.AddListener(self, lNewCtrl, DO_COMMIT_CHANGE);
  gEventManager.AddListener(self, lNewCtrl, DO_CANCEL_OP);

  lNewCtrl.Init;
  lNewCtrl.Active := True;
  TForm(lNewCtrl.View).SetFocus;
  AddController(lNewCtrl);

end;

procedure TBaseListViewController.HandleDeleteCommand(Sender: TObject);
var
//  lLvMed: TtiListViewMediatorView;
  lMsg: string;
  lObj: TtiObject;
  lCtrl: TGridController;// TVCLGridController;
  lCancelDelete: Boolean;
begin
  lCtrl := TGridController(Controllers.FindByModel(Model));
  lObj := lCtrl.SelectedItem;
  if lObj = nil then
    exit;


  //lMsg := Format(GLang.GetLang(cConfirmDeleteObj), [lCtrl.SelectedItem.Caption]);
  lMsg := 'Delete the selected objected?';
  if MessageDlg(lMsg, mtWarning, [mbYes, mbNo], 0) = mrNo then
    exit;



  DoBeforeObjectDelete(lObj, lCancelDelete, lMsg);
  if lCancelDelete then
    begin
      MessageDlg(lMsg, mtError, [mbOK], 0);
      exit;
    end;

  lObj.ObjectState := posDelete;
  SaveObject(lObj);

  DoAfterObjectDelete(lObj);

  with TtiObjectList(Model) do
    begin
      Remove(lObj);
      NotifyObservers;
      DoUpdateCount;
    end;

end;

procedure TBaseListViewController.HandleEditCommand(Sender: TObject);
var
  lCtrl: TGridController;
  lNewCtrl: TMVCController;
  lClass: TMVCControllerClass;
begin

  lCtrl := Controllers.FindByModel(Model) as TGridController;

  if (lCtrl <> nil) and (lCtrl.SelectedItem <> nil) then
    begin
      Edited := lCtrl.SelectedItem;
      if Edited.ObjectState = posPK then
        ReadObject(Edited);
    end
  else
    begin
      exit;
    end;

  // Make a clone of the object to be edited.
  DoSetCloned(Edited);

  lClass := DoGetEditControllerClass;
  lNewCtrl := lClass.Create(Edited, FEditView) as lClass;
  TBaseEditController(lNewCtrl).CloseAfterSave := True;
  // attach an mvc event to its commit signal
  gEventManager.AddListener(self, lNewCtrl, DO_COMMIT_CHANGE);
  gEventManager.AddListener(self, lNewCtrl, DO_CANCEL_OP);

  lNewCtrl.Init;
  lNewCtrl.Active := True;
  TForm(lNewCtrl.View).SetFocus;
  AddController(lNewCtrl);
end;

procedure TBaseListViewController.HandleViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin

  if (ssCtrl in Shift) then
    begin
      case Key of
        78: // new
        begin
          TBaseListView(View).btnNew.Click;
        end;
        69: // edit
        begin
          TBaseListView(View).btnEdit.Click;
        end;
        68: // delete
        begin
          TBaseListView(View).btnDelete.Click;
        end;
      end;
    end
  else
    begin
      if Key = 27 then // close
        begin
          TBaseListView(View).btnClose.Click;
        end;
    end;

  Key := 0;
end;

procedure TBaseListViewController.ReadObject(AObject: TtiObject);
begin
    Assert(false, ClassName + '.ReadObject must be implemented in a descendant class');
end;

procedure TBaseListViewController.SaveList;
begin
  // do nothing.  let descendants override;
  Assert(false, ClassName + '.SaveList must be implemented in a descendant class');
end;

procedure TBaseListViewController.SaveObject(AObject: TtiObject);
begin
    Assert(false, ClassName + '.SaveObject must be implemented in a descendant class');
end;

procedure TBaseListViewController.SetEdited(const Value: TtiObject);
begin
  FEdited := Value;
//  if FEditedClone <> nil then
//    FreeAndNil(FEditedClone);
  //FEditedClone := FEdited.Clone;
end;

procedure TBaseListViewController.Update(ASubject: TtiObject);
var
  lEvent: TMVCEvent;
begin
  if not (ASubject is TMVCEvent) then
    Exit;

  lEvent := TMVCEvent(ASubject);

  if lEvent.Name = DO_COMMIT_CHANGE then
    begin
      HandleCommitCommand(ASubject);
      lEvent.Handled := True;
    end
  else if lEvent.Name = DO_CANCEL_OP then
    begin
      HandleCancelEditCommand(ASubject);
      lEvent.Handled := True;
    end;

end;

end.
