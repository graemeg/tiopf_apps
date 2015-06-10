unit FtiAutoEditFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiEditFrame, tiObject, ActnList, StdCtrls, Buttons, Contnrs, ImgList,
  ComCtrls, ToolWin,
  tiPerAwareCtrls, ExtCtrls, tiFocusPanel, tiVTListView, tiVTAbstract;

type
  TtiAutoEditFrame = class(TtiEditFrame)
    ActionList: TActionList;
    actCommit: TAction;
    actRollback: TAction;
    ilNormal: TImageList;
    ToolBar: TToolBar;
    tbApply: TToolButton;
    tbRollback: TToolButton;
    tbSeparator: TToolButton;
    tbSeparator2: TToolButton;
    PageControl: TPageControl;
    tsBasic: TTabSheet;
    tsItems: TTabSheet;
    ilDisabled: TImageList;
    ilHot: TImageList;
    actGetOID: TAction;
    tbSeparator3: TToolButton;
    tbGetOID: TToolButton;
    LVItems: TtiVTListView;
    procedure actGetOIDUpdate(Sender: TObject);
    procedure actGetOIDExecute(Sender: TObject);
    procedure actCommitUpdate(Sender: TObject);
    procedure actRollbackExecute(Sender: TObject);
    procedure actCommitExecute(Sender: TObject);
  private
    { Private declarations }
    PropertyControls: TObjectList;
    edOID: TtiPerAwareEdit;
    edObjectState: TtiPerAwareEdit;
    function GetPropertyEditor(Index: Integer): TtiPerAwareEdit;
    procedure AutoPopulate;
  protected
    procedure AssignResourceStrings; virtual;
    procedure SetData(const AValue: TtiObject); override;
    procedure SetReadOnly(const Value: Boolean); override;
    procedure UpdateControls;
    procedure UpdateObjectState;
    procedure UpdateOID;
    property PropertyEditors[Index: Integer]: TtiPerAwareEdit read GetPropertyEditor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CommitBuffer; override;
    procedure RollbackBuffer; override;
  end;

var
  tiAutoEditFrame: TtiAutoEditFrame;

implementation

{$R *.dfm}

uses
  tiUtils
  , tiVisitor
  , TypInfo
  , tiOPFManager
  , tiRTTI
  ;

resourcestring

  SObjectState = 'Object State';
  SactCommitCaption = 'Apply';
  SactRollbackCaption = 'Discard';
  StbRollbackCaption = '  Discard'; //provides better spacing
  StsItemsCaption = 'Items';
  SactGetOIDCaption = 'Get OID';

{ TtiAutoEditFrame }

procedure TtiAutoEditFrame.actCommitExecute(Sender: TObject);
begin
  inherited;
  if Validate then
    CommitBuffer;
end;

procedure TtiAutoEditFrame.actCommitUpdate(Sender: TObject);
begin
  inherited;
  actCommit.Enabled := DataModified;
  actRollback.Enabled := actCommit.Enabled;
end;

procedure TtiAutoEditFrame.actGetOIDExecute(Sender: TObject);
begin
  inherited;
  DataBuffer.ObjectState := posCreate;
  {$IFDEF OID_AS_INT64}
    DataBuffer.OID := gTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID;
  {$ELSE}
    GTIOPFManager.DefaultOIDGenerator.AssignNextOID(DataBuffer.OID);
  {$ENDIF}
  UpdateObjectState;
  UpdateOID;
end;

procedure TtiAutoEditFrame.actGetOIDUpdate(Sender: TObject);
begin
  inherited;
  actGetOID.Enabled := Assigned(DataBuffer) and (DataBuffer.ObjectState = posEmpty);
end;

procedure TtiAutoEditFrame.actRollbackExecute(Sender: TObject);
begin
  inherited;
  RollbackBuffer;
end;

procedure TtiAutoEditFrame.AssignResourceStrings;
begin
  actCommit.Caption := SactCommitCaption;
  actRollback.Caption := SactRollbackCaption;
  tbRollback.Caption := StbRollbackCaption;
  tsItems.Caption := StsItemsCaption;
  actGetOID.Caption := SactGetOIDCaption;
end;

procedure TtiAutoEditFrame.AutoPopulate;
var
  PropertyNames: TStringList;
  I: Integer;
  edTemp: TtiPerAwareEdit;
  Row, Col: Integer;
  AList: TtiObjectList;
  IsList: Boolean;
  ListItem: TtiObject;
  TypeKind: TTypeKind;
const
  cBorder = 6;
  cCtrlHeight = 21;
  cCtrlWidth = 300;
  cMaxRowCount = 10;

  function CreateControl(const Caption: String; AReadOnly: Boolean): TtiPerAwareEdit;
  begin
    Result := TtiPerAwareEdit.Create(Self);
    Result.LabelWidth := 100;
    Result.Parent := tsBasic;
    Result.Top := cBorder + (cCtrlHeight + cBorder) * Row;
    Result.Left := cBorder + (cBorder + cCtrlWidth) * Col;
    Result.Width := cCtrlWidth;
    Result.Caption := Caption;
    Result.ReadOnly := AReadOnly;
  end;

begin
  PropertyControls.Clear; //destroys existing controls
  if not Assigned(Databuffer) then
    Exit;

  PropertyNames := TStringList.Create;
  Row := 0;
  Col := 0;
  try
    //add title
    tsBasic.Caption := DataBuffer.ClassName;

    //add OID
    edOID := CreateControl('OID', True);
    edOID.Value := DataBuffer.OID.AsString;
    Inc(Row);

    //add ObjectState
    edObjectState := CreateControl(SObjectState, True);
    UpdateObjectState;
    Inc(Row);

    //add other properties
    tiGetPropertyNames(TtiObject(DataBuffer), PropertyNames,
      ctkSimple + [tkVariant, tkEnumeration]);
    for I := 0 to PropertyNames.Count - 1 do
    begin
      edTemp := CreateControl(PropertyNames[I], Self.ReadOnly);
      edTemp.LinkToData(DataBuffer, PropertyNames[I]);
      PropertyControls.Add(edTemp);
      Inc(Row);
      if Row >= cMaxRowCount then
      begin
        Row := 0;
        if I <> PropertyNames.Count - 1 then
          Inc(Col);
      end;
    end;
    if PropertyNames.Count - 1 > cMaxRowCount then
      Row := cMaxRowCount;
    if not (Align in [alClient, alLeft, alRight]) then
      Self.ClientHeight := (cCtrlHeight + cBorder) * Row + cBorder * 2;
    if not (Align in [alClient, alTop, alBottom]) then
      Self.ClientWidth := (cCtrlWidth + cBorder) * (Col + 1) + cBorder;

    //handle lists
    LVItems.Header.Columns.Clear;
    LVItems.Data := nil;
    IsList :=DataBuffer is TtiObjectList;
    tsItems.TabVisible := IsList;
    if IsList then
    begin
      AList := DataBuffer as TtiObjectList;
      if AList.Count > 0 then
      begin
        ListItem := AList.Items[0];
        PropertyNames.Clear;
        tiGetPropertyNames(TtiObject(ListItem), PropertyNames,
          ctkSimple + [tkVariant, tkEnumeration]);
        for I := 0 to PropertyNames.Count - 1 do
        begin
            TypeKind := PropType(ListItem,PropertyNames[I]);
            case TypeKind of
              tkInteger, tkInt64:
                LVItems.AddColumn(PropertyNames[I], vttkInt, PropertyNames[I], 40);
              tkString, tkWChar, tkLString, tkWString, tkChar:
                LVItems.AddColumn(PropertyNames[I], vttkString, PropertyNames[I], 120);
              tkFloat:
                LVItems.AddColumn(PropertyNames[I], vttkFloat, PropertyNames[I], 60);
              //tkUnknown, tkEnumeration, tkSet, tkClass, tkMethod, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray not handled
            end; //case
        end; //for each property
      end; //if items exist
      LVItems.Data := AList;
    end; //if IsList
  finally
    PropertyNames.Free;
  end;
end;

procedure TtiAutoEditFrame.CommitBuffer;
begin
  inherited CommitBuffer;
  UpdateObjectState;
end;

constructor TtiAutoEditFrame.Create(AOwner: TComponent);
begin
  inherited;
  AssignResourceStrings;
  PropertyControls := TObjectList.Create;
  PropertyControls.OwnsObjects := True;
end;

destructor TtiAutoEditFrame.Destroy;
begin
  PropertyControls.Free;
  inherited;
end;

function TtiAutoEditFrame.GetPropertyEditor(Index: Integer): TtiPerAwareEdit;
begin
  Result := TtiPerAwareEdit(PropertyControls[Index]);
end;

procedure TtiAutoEditFrame.RollbackBuffer;
begin
  inherited;

  //many properties may have changed - update them all
  UpdateOID;
  UpdateObjectState;
  UpdateControls;
end;

procedure TtiAutoEditFrame.SetData(const AValue: TtiObject);
begin
  inherited;
  PageControl.ActivePage := tsBasic;
  AutoPopulate;
end;

procedure TtiAutoEditFrame.SetReadOnly(const Value: Boolean);
var
  I: Integer;
begin
  inherited;
  for I := 0 to PropertyControls.Count - 1 do
    PropertyEditors[I].ReadOnly := Value;
end;

procedure TtiAutoEditFrame.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to PropertyControls.Count - 1 do
    PropertyEditors[I].Refresh;
end;

procedure TtiAutoEditFrame.UpdateObjectState;
begin
  if Assigned(DataBuffer) and Assigned(edObjectState) then
    edObjectState.Value := DataBuffer.ObjectStateAsString;
end;

procedure TtiAutoEditFrame.UpdateOID;
begin
  if Assigned(DataBuffer) and Assigned(edOID) then
    edOID.Value := DataBuffer.OID.AsString;
end;

end.
