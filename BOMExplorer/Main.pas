unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ExtCtrls, Dialogs, StdCtrls, ImgList, ActnList,
  ToolWin, ComCtrls,
  tiFocusPanel, tiVTTreeView, tiRoundedPanel, tiSplitter, tiObject,
  tiVirtualTrees,
  FtiAutoEditFrame, tiBOMExploration, tiVTAbstract;

type
  TFtiBOMExplorer = class(TForm)
    spnlMain: TtiSplitterPanel;
    TV: TtiVTTreeView;
    pnlRight: TPanel;
    actConnectDB: TAction;
    actDisconnectDB: TAction;
    ilNormal: TImageList;
    actRefresh: TAction;
    actPost: TAction;
    actAppend: TAction;
    actDelete: TAction;
    actCull: TAction;
    ilDisabled: TImageList;
    ilHot: TImageList;
    ActionList: TActionList;
    tbDataRow: TToolBar;
    tbRefresh: TToolButton;
    tbAppend: TToolButton;
    tbDelete: TToolButton;
    tbPost: TToolButton;
    tbCull: TToolButton;
    actSelectBOM: TAction;
    pnlBOMStatus: TPanel;
    imgBOMIcon: TImage;
    lblBOMName: TLabel;
    imgAliasIcon: TImage;
    lblAliasName: TLabel;
    imgRegistrarIcon: TImage;
    lblRegistrarName: TLabel;
    lblBOM: TLabel;
    Separator2: TLabel;
    lblAlias: TLabel;
    Separator3: TLabel;
    lblRegistrar: TLabel;
    pnlView: TPanel;
    imgViewIcon: TImage;
    lblView: TLabel;
    pnlViewHolder: TPanel;
    cbView: TComboBox;
    Separator5: TLabel;
    imgViewPointIcon: TImage;
    lblViewPoint: TLabel;
    pnlViewPointHolder: TPanel;
    cbViewPoint: TComboBox;
    actExit: TAction;
    tbMain: TToolBar;
    tbSelectBOM: TToolButton;
    tbConnect: TToolButton;
    tbDisconnect: TToolButton;
    tbExit: TToolButton;
    procedure actExitExecute(Sender: TObject);
    procedure cbViewPointChange(Sender: TObject);
    procedure cbViewChange(Sender: TObject);
    procedure actSelectBOMExecute(Sender: TObject);
    procedure actCullExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actDeleteExecute(Sender: TObject);
    procedure actAppendExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actRefreshExecute(Sender: TObject);
    procedure actPostExecute(Sender: TObject);
    procedure actDisconnectDBExecute(Sender: TObject);
    procedure actConnectDBExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TVSelectNode(ptiVTTreeView: TtiVTTreeView; pNode: PVirtualNode;
      AData: TtiObject);
  private
    ChildFrame: TtiAutoEditFrame;
    DBConnected: Boolean;
    View: TtiBOMEView;
    ViewPoint: TtiBOMEViewPoint;
    ExplorableBOM: TtiExplorableBOM;
    Alias: TtiAlias;
    Registrar: TtiBOMERegistrar;
    procedure RefreshTree(NodeToInit: PVirtualNode;
      Expanded: Boolean = False; SelectLastChild: Boolean = False);
    function DeletedDetected(AData: TtiObject): Boolean;
    procedure CullNode(Recursive: Boolean = True);
    procedure BOMChanged;
    procedure ViewChanged;
    procedure ViewPointChanged;
    procedure PopulateViews;
    procedure PopulateViewPoints;
    procedure UpdateView;
    procedure UpdateViewPoint;
    function AttemptDisconnect: Boolean;
    procedure ApplyResourceStrings;
    procedure OnChildFrameValidate(Sender: TObject; Buffer: TtiObject;
      var Valid: Boolean);
    procedure OnChildFrameCommit(Sender: TObject; Buffer: TtiObject);
    procedure ConnectDB;
    procedure DisconnectDB;
    procedure CancelEditing;
    function QueryFinishEditing(AllowCancel: Boolean = True): Boolean;
    function FinishEditing(AllowCancel: Boolean = True): Boolean;
    procedure SetupEditing(Node: PVirtualNode);
    function CommitChildFrame: Boolean;
    procedure RollbackChildFrame;
    function SelectedNode: PVirtualNode;
    function QueryDisconnect: Boolean;
  public
    procedure SetBOM(AExplorableBOM: TtiExplorableBOM; AAlias: TtiAlias;
      ARegistrar: TtiBOMERegistrar);
  end;

var
  FtiBOMExplorer: TFtiBOMExplorer;

implementation

{$R *.dfm}

uses
  tiConstants
  , tiDialogs
  , tiOPFManager
  , FtiValidationErrors
  , tiHelperClasses
  , FtiSelectBOMDlg
  , tiVisitor
  , Transit_BOM
  ;

resourcestring
  SApplyBufferChanges = 'Changes have been made in the edit buffer. Apply changes?';
  SCaption = 'tiOPF Business Object Model Explorer';
  SactConnectDB = 'Connect';
  SactDisconnectDB = 'Disconnect';
  SactRefresh = 'Refresh';
  SactPost = 'Post';
  SactAppend = 'Append';
  SactDelete = 'Delete';
  SactCull = 'Cull';
  SSaveChanges = 'Changes have not been saved.  Save Now?';
  SactSelectBOM = ' Select BOM';
  SlblBOMCaption = 'BOM: ';
  SlblAliasCaption = 'Alias: ';
  SlblRegistrarCaption = 'Registrar: ';
  SlblViewCaption = 'View: ';
  SlblViewPointCaption = 'ViewPoint: ';
  SactExitCaption = 'Exit';

{ TFtiBOMExplorer }

procedure TFtiBOMExplorer.actConnectDBExecute(Sender: TObject);
begin
  ConnectDB;
  UpdateView;
  UpdateViewPoint;
end;

procedure TFtiBOMExplorer.actCullExecute(Sender: TObject);
begin
  if not FinishEditing then
    Exit;
  CullNode;
end;

procedure TFtiBOMExplorer.actDisconnectDBExecute(Sender: TObject);
begin
  AttemptDisconnect;
end;

procedure TFtiBOMExplorer.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFtiBOMExplorer.actAppendExecute(Sender: TObject);
var
  AData: TtiObject;
  ANode: PVirtualNode;
  Mapping: TtiVTTVDataMapping;
  CanInsert: Boolean;
begin
  if not FinishEditing then
    Exit;
  ANode := SelectedNode;
  AData := TV.GetObjectFromNode(ANode);

  //GOTCHA: ideally the code would work like this
  //TV.DoInsert(Self);
  //but it doesn't work because it checks for GUI button visibility, which
  //we don't use, so we need to re-invent the wheel
  Mapping := TV.GetMappingForNode(ANode);
  if not (Assigned(Mapping) and Mapping.CanInsert and Assigned(Mapping.OnInsert)) then
    Exit;
  CanInsert := True;
  if Assigned(Mapping.OnCanInsert) then
    Mapping.OnCanInsert(TV, ANode, AData, CanInsert);
  if not CanInsert then
    Exit;
  Mapping.OnInsert(TV, ANode, AData);

  //reset GUI - select newly appended child
  RefreshTree(ANode, False, True);
  SetupEditing(ANode);
end;

procedure TFtiBOMExplorer.actDeleteExecute(Sender: TObject);
var
  Mapping: TtiVTTVDataMapping;
  CanDelete: Boolean;
  AData: TtiObject;
  ANode: PVirtualNode;
begin
  CancelEditing;

  //ideally the code would work like this
  //TV.DoDelete(Self);

  //GOTCHA: doesn't work because it checks for GUI button visibility too
  //instead, re-invent the wheel
  ANode := SelectedNode;
  AData := TV.GetObjectFromNode(ANode);
  Mapping := TV.GetMappingForNode(ANode);
  if not (Assigned(Mapping) and Mapping.CanDelete) then
    Exit;
  CanDelete := True;
  if Assigned(Mapping.OnCanDelete) then
    Mapping.OnCanDelete(TV, ANode, AData, CanDelete);
  if not CanDelete then
    Exit;
  if Assigned(Mapping.OnDelete) then
    Mapping.OnDelete(TV, ANode, AData);

  //mark the object for deletion, and update the GUI
  AData.Deleted := True;
  SetupEditing(SelectedNode);
end;

procedure TFtiBOMExplorer.actRefreshExecute(Sender: TObject);
var
  AData: TtiObject;
  ANode: PVirtualNode;
  PurgeVisitor: TVisSkeletonPurge;
begin
  //this routine refeshes the selected object with current info in the database
  //through iteration, all objects lower in the tree are also affected
  //posClean -> no change (database is the same as in-memory copy)
  //posPK -> posClean (rest of object will be retrieved - but this app doesn't use this state)
  //posCreate -> will disappear (never having been persisted)
  //posUpdate -> posClean (changes discarded)
  //posDelete -> posClean (pending deletion cancelled)
  //posDeleted -> will disappear (no longer in the database)

  CancelEditing;
  AData := TV.SelectedData;
  ANode := SelectedNode;

  //in-memory-only objects should be culled rather than refreshed
  if AData.ObjectState in [posCreate, posDeleted] then
  begin
    CullNode(False);
    Exit;
  end;

  {GOTCHA: There is no way in TtiObject to tell whether an object is merely a
  container, such as the statically-created skeleton of the BOM, or the list
  holding details in a master-detail relationship between peristent objects.
  ObjectState is not helpful, since reading such objects sets ObjectState to
  posClean.  Such objects need to 'purge' themselves of persisted objects
  so they can be reloaded.  This function is not in TtiObject, so two klugey
  classes have been created, called TtiSkeletonObject/TtiSkeletonList,
  for this purpose. Purge is not the same as TtiObjectList.Clear, since
  skeletons could implement 0..1 relationship, or be a compound list
  (e.g. a list of lists).  Purge sets ObjectState to posEmpty.}

  {so rather than calling
  AData.Purge;
  instead create a visitor to purge TtiSkeletonX descendents}
  PurgeVisitor := TVisSkeletonPurge.Create;
  try
    AData.Iterate(PurgeVisitor);
  finally
    PurgeVisitor.Free;
  end;

  {when a non-skeleton is selected, we need to trick tiOPF into re-loading
  the object based on its primary key
  the automap will do this as-is, but the DBIndependant achieves this
  through the _ReadFromPK visitors}
  if AData.ObjectState <> posEmpty then
    AData.ObjectState := posPK;

  { See the code comments in Transit_BOM for the class TCustomObject }
  if AData is TCustomObject then
    TCustomObject(AData).Read;

  //update GUI
  RefreshTree(ANode);
  SetupEditing(SelectedNode);
end;

procedure TFtiBOMExplorer.actSelectBOMExecute(Sender: TObject);
var
  Dialog: TtiSelectBOMDlg;
  AExplorableBOM: TtiExplorableBOM;
  AAlias: TtiAlias;
  ARegistrar: TtiBOMERegistrar;
begin
  if not QueryDisconnect then
    Exit;
  Dialog := TtiSelectBOMDlg.Create(nil);
  try
    AExplorableBOM := ExplorableBOM;
    AAlias := Alias;
    ARegistrar := Registrar;
    if Dialog.Execute(AExplorableBOM, AAlias, ARegistrar) then
    begin
      //undo existing settings
      DisconnectDB;
      Registrar.UnregisterItems;

      //copy new values
      ExplorableBOM := AExplorableBOM;
      Alias := AAlias;
      Registrar := ARegistrar;

      //reflect new settings
      BOMChanged;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TFtiBOMExplorer.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  Mapping: TtiVTTVDataMapping;
  Node: PVirtualNode;
  SelectionMade: Boolean;
  AData: TtiObject;
begin
  Mapping := nil; //compiler warning scarecrow
  AData := nil; //compiler warning scarecrow
  actSelectBOM.Enabled := BOMEDictionary.BOMs.Count > 0;
  actConnectDB.Enabled := not DBConnected;
  actDisconnectDB.Enabled := DBConnected;
  Node := SelectedNode;
  SelectionMade := DBConnected and Assigned(Node);
  if SelectionMade then
  begin
    Mapping := TV.GetMappingForNode(Node);
    AData := TV.GetObjectFromNode(Node);
  end;
  actRefresh.Enabled := SelectionMade;

  //Detecting Dirty and Deleted applies throughout hierarchy
  actPost.Enabled := SelectionMade and AData.Dirty;
  actCull.Enabled := SelectionMade and DeletedDetected(AData);

  actAppend.Enabled := SelectionMade and Mapping.CanInsert;
  actDelete.Enabled := SelectionMade and Mapping.CanDelete and
    (AData.ObjectState in [posUpdate, posClean]);
end;

procedure TFtiBOMExplorer.ApplyResourceStrings;
begin
  Caption := SCaption;
  actConnectDB.Caption := SactConnectDB;
  actDisconnectDB.Caption := SactDisconnectDB;
  actRefresh.Caption := SactRefresh;
  actPost.Caption := SactPost;
  actAppend.Caption := SactAppend;
  actDelete.Caption := SactDelete;
  actCull.Caption := SactCull;
  actSelectBOM.Caption := SactSelectBOM;
  lblBOM.Caption := SlblBOMCaption;
  lblAlias.Caption := SlblAliasCaption;
  lblRegistrar.Caption := SlblRegistrarCaption;
  lblView.Caption := SlblViewCaption;
  lblViewPoint.Caption := SlblViewPointCaption;
  actExit.Caption := SactExitCaption;
end;

function TFtiBOMExplorer.AttemptDisconnect: Boolean;
begin
  Result := QueryDisconnect;
  if Result then
    DisconnectDB;
end;

procedure TFtiBOMExplorer.actPostExecute(Sender: TObject);
begin
  if FinishEditing then
  begin
  { See the code comments in Transit_BOM for the class TCustomObject }
  if TV.SelectedData is TCustomObject then
    TCustomObject(TV.SelectedData).Save;

    //ObjectState can change to posClean or posDeleted, so we must update GUI
    SetupEditing(SelectedNode);
  end;
end;

procedure TFtiBOMExplorer.BOMChanged;
begin
  if not Assigned(ExplorableBOM) then
    Exit;
  Registrar.RegisterItems;
  lblBOMName.Caption := ExplorableBOM.Name;
  lblAliasName.Caption := Alias.Name;
  lblRegistrarName.Caption := Registrar.Name;
  PopulateViews;
  PopulateViewPoints;
  ViewChanged;
  ViewPointChanged;
end;

procedure TFtiBOMExplorer.CancelEditing;
begin
  if Assigned(ChildFrame) then
  begin
    RollbackChildFrame;
    FreeAndNil(ChildFrame);
  end;
end;

procedure TFtiBOMExplorer.cbViewChange(Sender: TObject);
begin
  if cbView.ItemIndex >= 0 then
    if (View <> cbView.Items.Objects[cbView.ItemIndex]) then
    begin
      View := TtiBOMEView(cbView.Items.Objects[cbView.ItemIndex]);
      ViewChanged;
    end;
end;

procedure TFtiBOMExplorer.cbViewPointChange(Sender: TObject);
begin
  if cbViewPoint.ItemIndex >= 0 then
    if (ViewPoint <> cbViewPoint.Items.Objects[cbViewPoint.ItemIndex]) then
    begin
      ViewPoint := TtiBOMEViewPoint(
        cbViewPoint.Items.Objects[cbViewPoint.ItemIndex]);
      ViewPointChanged;
    end;
end;

function TFtiBOMExplorer.CommitChildFrame: Boolean;
begin
  Result := True;
  if Assigned(ChildFrame) then
    if Assigned(ChildFrame.Data) then
    begin
      if ChildFrame.DataModified then
      begin
        Result := ChildFrame.Validate;
        if Result then
          ChildFrame.CommitBuffer;
      end;
    end;
end;

procedure TFtiBOMExplorer.ConnectDB;
begin
  with Alias do
    gTIOPFManager.ConnectDatabase(
      DatabaseName,
      UserName,
      Password,
      Params, 
      PerLayerName);
//  gTIOPFManager.DefaultDBConnectionName :=
  DBConnected := True;
end;

procedure TFtiBOMExplorer.CullNode(Recursive: Boolean = True);
var
  ANode: PVirtualNode;
  AData: TtiObject;
  FreeDeletedVisitor: TVisFreeDeleted;
  RemoveSubject: Boolean;
begin
  {this routine culls objects in memory that have been are not in the database
  because they have already been deleted or were never persisted (posCreate)
  Unlike TtiObjectList.FreeDeleted, this routine uses a visitor to
  cull down the hierarchy}
  ANode := SelectedNode;
  AData := TV.GetObjectFromNode(ANode);
  RemoveSubject := AData.ObjectState in [posCreate, posDeleted];
  if Recursive then
  begin
    {GOTCHA: Objects don't notify their owners when they are freed,
    so we must do it manually. There is no polymorphic way to do this if the
    owner is TtiObject, so we do NOT notify those owners here
    IF AData is not owned by a list, this will blow up unless AData's class has
    implemented free notification}
    FreeDeletedVisitor := TVisFreeDeleted.Create;
    try
      FreeDeletedVisitor.IterationStyle := isBottomUpSinglePass;
      AData.Iterate(FreeDeletedVisitor);
    finally
      FreeDeletedVisitor.Free;
    end;
  end
  else //only remove current node (e.g. posCreate)
  begin
    if Assigned(AData.Owner) and (AData.Owner is TtiObjectList) then
      TtiObjectLIst(AData.Owner).Remove(AData)
    else
      //GOTCHA: this will blow up if concrete class doesn't notify its owner
      AData.Free;
  end;

  //reset GUI
  if RemoveSubject then
  begin
    TV.SetObjectForNode(ANode, nil);
    ANode := ANode.Parent;
  end;
  RefreshTree(ANode, RemoveSubject);
  SetupEditing(ANode);
end;

function TFtiBOMExplorer.DeletedDetected(AData: TtiObject): Boolean;
var
  Visitor: TVisDetectDeleted;
begin
  Visitor := TvisDetectDeleted.Create;
  try
    AData.Iterate(Visitor);
    Result:=Visitor.Detected;
  finally
    Visitor.Free;
  end;
end;

procedure TFtiBOMExplorer.DisconnectDB;
begin
  TV.Data := nil;
  with Alias do
    gTIOPFManager.DisconnectDatabase(
      DatabaseName,
      PerLayerName);
  DBConnected := False;
end;

function TFtiBOMExplorer.QueryDisconnect: Boolean;
var
  DlgResult: Integer;
begin
  //there are 2 stages to allow disconnecting
  //1) ask what to do with the dirty edit buffer
  //2) ask what to do with dirty objects
  Result := True;
  if DBConnected then
  begin
    if not QueryFinishEditing then
    begin
      Result := False;
      Exit;
    end;
    if ViewPoint.Root.Dirty then
    begin
      DlgResult := MessageDlg(SSaveChanges, mtConfirmation,
        [mbYes, mbNo, mbCancel], 0);
      case DlgResult of
        mrYes:
          begin
            //save all changes to db
            { See the code comments in Transit_BOM for the class TCustomObject }
            if ViewPoint.Root is TCustomObject then
              TCustomObject(ViewPoint.Root).Save;
          end;
        mrNo:
          begin
          end; //do nothing - abandon changes
        else
          Result := False;
      end;
    end;
    if Result and Assigned(ChildFrame) then
      FreeAndNil(ChildFrame);
  end;
end;

function TFtiBOMExplorer.FinishEditing(AllowCancel: Boolean = True): Boolean;
begin
  Result := QueryFinishEditing(AllowCancel);
  if Result then
    FreeAndNil(ChildFrame);
end;

procedure TFtiBOMExplorer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DBConnected then
    CanClose := AttemptDisconnect
  else
    CanClose := True;
  if CanClose then
    Registrar.UnregisterItems;
end;

procedure TFtiBOMExplorer.FormCreate(Sender: TObject);
begin
  ApplyResourceStrings;
  DBConnected := False;
end;

procedure TFtiBOMExplorer.OnChildFrameCommit(Sender: TObject; Buffer: TtiObject);
begin
  TV.RefreshCurrentNode;
end;

procedure TFtiBOMExplorer.OnChildFrameValidate(Sender: TObject;
  Buffer: TtiObject; var Valid: Boolean);
var
  ErrorMsgs: TStringList;
begin
  ErrorMsgs := TStringList.Create;
  try
    Valid := Buffer.IsValid(ErrorMsgs);
    if not Valid then
      TtiValidationErrorsDlg.Execute(Buffer, ErrorMsgs);
  finally
    ErrorMsgs.Free;
  end;
end;

procedure TFtiBOMExplorer.PopulateViewPoints;
var
  I: Integer;
begin
  if not Assigned(ExplorableBOM) then
    Exit;
  cbViewPoint.Items.Clear;
  if ExplorableBOM.ViewPoints.Count > 0 then
  begin
    for I := 0 to ExplorableBOM.ViewPoints.Count - 1 do
      cbViewPoint.Items.AddObject(ExplorableBOM.ViewPoints[I].Name, ExplorableBOM.ViewPoints[I]);
    ViewPoint := ExplorableBOM.ViewPoints[0];
    cbViewPoint.ItemIndex := 0;
  end;
end;

procedure TFtiBOMExplorer.PopulateViews;
var
  I: Integer;
begin
  if not Assigned(ExplorableBOM) then
    Exit;
  cbView.Items.Clear;
  if ExplorableBOM.Views.Count > 0 then
  begin
    for I := 0 to ExplorableBOM.Views.Count - 1 do
      cbView.Items.AddObject(ExplorableBOM.Views[I].Name, ExplorableBOM.Views[I]);
    View := ExplorableBOM.Views[0];
    cbView.ItemIndex := 0;
  end;
end;

function TFtiBOMExplorer.QueryFinishEditing(AllowCancel: Boolean): Boolean;
var
  DlgResult: Integer;
begin
  Result := True;
  if Assigned(ChildFrame) then
  begin
    if ChildFrame.DataModified then
    begin
      if AllowCancel then
        DlgResult := MessageDlg(SApplyBufferChanges, mtConfirmation,
          [mbYes, mbNo, mbCancel], 0)
      else
        DlgResult := MessageDlg(SApplyBufferChanges, mtConfirmation,
          [mbYes, mbNo], 0);
      case DlgResult of
        mrYes:
        begin
          //try committing, but if it doesn't validate, treat like a cancel operation, if allowed
          if not CommitChildFrame then
            if AllowCancel then
              Result := False;
          ;
        end;
        mrNo: RollbackChildFrame;
        else
          Result := False;
      end;
    end;
  end;
end;

procedure TFtiBOMExplorer.RefreshTree(NodeToInit: PVirtualNode;
  Expanded, SelectLastChild: Boolean);
var
  NodeToSelect: PVirtualNode;
begin
  TV.VT.ReinitNode(NodeToInit,True);
  NodeToSelect := nil;
  if SelectLastChild then
    NodeToSelect := TV.VT.GetLastChild(NodeToInit);
  if not Assigned(NodeToSelect) then
    NodeToSelect := NodeToInit;
  TV.VT.Selected[NodeToSelect] := True;
  TV.VT.FocusedNode := NodeToSelect;
  if Expanded then
    TV.VT.Expanded[NodeToSelect] := True;
  TV.VT.ScrollIntoView(NodeToSelect, True);
end;

procedure TFtiBOMExplorer.RollbackChildFrame;
begin
  if Assigned(ChildFrame) then
    if Assigned(ChildFrame.Data) then
      ChildFrame.RollbackBuffer;
end;

function TFtiBOMExplorer.SelectedNode: PVirtualNode;
begin
  Result := TV.VT.GetFirstSelected;
end;

procedure TFtiBOMExplorer.SetBOM(AExplorableBOM: TtiExplorableBOM;
  AAlias: TtiAlias; ARegistrar: TtiBOMERegistrar);
begin
  ExplorableBOM := AExplorableBOM;
  Alias := AAlias;
  Registrar := ARegistrar;
  BOMChanged;
end;

procedure TFtiBOMExplorer.SetupEditing(Node: PVirtualNode);
var
  Mapping: TtiVTTVDataMapping;
begin
  //SetupEditing can be called twice when selected node changes within another operation
  if Assigned(ChildFrame) then
    Exit;
  ChildFrame := TtiAutoEditFrame.Create(Self);
  ChildFrame.OnCommit := OnChildFrameCommit;
  ChildFrame.OnValidate := OnChildFrameValidate;
  Mapping := TV.GetMappingForNode(Node);
  ChildFrame.ReadOnly := not Mapping.CanEdit;
  ChildFrame.Data := TV.GetObjectFromNode(Node);
  ChildFrame.Parent := pnlRight;
  ChildFrame.Align := alClient;
  ChildFrame.Show;
end;

procedure TFtiBOMExplorer.TVSelectNode(ptiVTTreeView: TtiVTTreeView;
  pNode: PVirtualNode; AData: TtiObject);
begin
  //GOTCHA: sometimes TreeView fires this event before node re-initialization
  //is complete, causes AVs

  //GOTCHA: When the user changes nodes, we can't prevent it even when the
  //current node editing dowsn't validate.  Our only option is to rollback
  //the edit buffer, with no option for the user to cancel
  //to avoid this problem, the TreeView needs a OnCanSelect event
  FinishEditing(False); //tear-down
  SetupEditing(pNode); //setup
end;

procedure TFtiBOMExplorer.UpdateView;
var
  I: Integer;
begin
  //GOTCHA: assigning DataMappings triggers a tiOPF bug,
  //TtiVTTVDataMapping.Assign must be implemented for this to work
  //TV.DataMappings := View.TreeMappings;

  //assign manually
  TV.DataMappings.Clear;
  for I := 0 to View.TreeMappings.Count - 1 do
    with TV.DataMappings.Add do
    begin
      CanDelete := View.TreeMappings.Items[I].CanDelete;
      CanEdit := View.TreeMappings.Items[I].CanEdit;
      CanInsert := View.TreeMappings.Items[I].CanInsert;
      CanView := View.TreeMappings.Items[I].CanView;
      ImageIndex := View.TreeMappings.Items[I].ImageIndex;
      DisplayPropName := View.TreeMappings.Items[I].DisplayPropName;
      DataClass := View.TreeMappings.Items[I].DataClass;
      OnDeriveNodeText := View.TreeMappings.Items[I].OnDeriveNodeText;
      OnCanEdit := View.TreeMappings.Items[I].OnCanEdit;
      OnCanInsert := View.TreeMappings.Items[I].OnCanInsert;
      OnCanDelete := View.TreeMappings.Items[I].OnCanDelete;
      OnInsert := View.TreeMappings.Items[I].OnInsert;
      OnEdit := View.TreeMappings.Items[I].OnEdit;
      OnDelete := View.TreeMappings.Items[I].OnDelete;
    end;
end;

procedure TFtiBOMExplorer.UpdateViewPoint;
begin
  { See the code comments in Transit_BOM for the class TCustomObject }
  if ViewPoint.Root is TCustomObject then
    TCustomObject(ViewPoint.Root).Read;

  TV.Data := nil;
  TV.Data := ViewPoint.Root;
end;

procedure TFtiBOMExplorer.ViewChanged;
begin
  if DBConnected then
  begin
    FinishEditing(False);
    UpdateView;
    SetupEditing(SelectedNode);
  end;
end;

procedure TFtiBOMExplorer.ViewPointChanged;
begin
  if DBConnected then
  begin
    FinishEditing(False);
    UpdateViewPoint;
  end;
end;

end.
