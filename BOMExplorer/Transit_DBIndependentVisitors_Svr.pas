unit Transit_DBIndependentVisitors_Svr;

interface

uses
  tiVisitorDBAutoGen
  , Transit_BOM
  , SysUtils
  ;

type
  {Each persisted type has a Read, ReadFromPK, Create, Update and Delete visitor
  Read visitors create new objects and populate a collection class
  ReadFromPK visitors use the OID of already-existing objects to re-fetch
  the non-PK fields from the database into the memory object}

  TVisPeriod_Read = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisPeriod_ReadFromPK = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisPeriod_Create = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisPeriod_Update = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisPeriod_Delete = class( TVisDBAutoGenDelete )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisShowMethod_Read = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisShowMethod_ReadFromPK = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisShowMethod_Create = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisShowMethod_Update = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisShowMethod_Delete = class( TVisDBAutoGenDelete )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisMidpointList_Read = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisMidpointList_ReadFromPK = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisMidpointList_Create = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisMidpointList_Update = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisMidpointList_Delete = class( TVisDBAutoGenDelete )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisMidpoint_Read = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisMidpoint_ReadFromPK = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisMidpoint_Create = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisMidpoint_Update = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisMidpoint_Delete = class( TVisDBAutoGenDelete )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisRTSQuestion_Read = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisRTSQuestion_ReadFromPK = class( TVisDBAutoGenRead )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisRTSQuestion_Create = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisRTSQuestion_Update = class( TVisDBAutoGenUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;

  TVisRTSQuestion_Delete = class( TVisDBAutoGenDelete )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure SetupParams; override;
  end;


procedure RegisterVisitors;

implementation

uses
  tiObject
  , tiLog
  , tiQuery
  , tiOPFManager
  ;

procedure RegisterVisitors;
begin
  gTIOPFManager.RegReadVisitor(TVisPeriod_Read);
  gTIOPFManager.RegReadVisitor(TVisPeriod_ReadFromPK);
  gTIOPFManager.RegSaveVisitor(TVisPeriod_Create);
  gTIOPFManager.RegSaveVisitor(TVisPeriod_Update);
  gTIOPFManager.RegSaveVisitor(TVisPeriod_Delete);

  gTIOPFManager.RegReadVisitor(TVisShowMethod_Read);
  gTIOPFManager.RegReadVisitor(TVisShowMethod_ReadFromPK);
  gTIOPFManager.RegSaveVisitor(TVisShowMethod_Create);
  gTIOPFManager.RegSaveVisitor(TVisShowMethod_Update);
  gTIOPFManager.RegSaveVisitor(TVisShowMethod_Delete);

  //One-2-Many visitor order is important
  gTIOPFManager.RegReadVisitor(TVisMidpointList_Read);
  gTIOPFManager.RegReadVisitor(TVisMidpointList_ReadFromPK);
  gTIOPFManager.RegReadVisitor(TVisMidpoint_Read);
  gTIOPFManager.RegReadVisitor(TVisMidpoint_ReadFromPK);

  gTIOPFManager.RegSaveVisitor(TVisMidpoint_Update);
  gTIOPFManager.RegSaveVisitor(TVisMidpointList_Update);

  gTIOPFManager.RegSaveVisitor(TVisMidpoint_Delete);
  gTIOPFManager.RegSaveVisitor(TVisMidpointList_Delete);

  gTIOPFManager.RegSaveVisitor(TVisMidpointList_Create);
  gTIOPFManager.RegSaveVisitor(TVisMidpoint_Create);

  //association - is order important?
  gTIOPFManager.RegReadVisitor(TVisRTSQuestion_Read);
  gTIOPFManager.RegReadVisitor(TVisRTSQuestion_ReadFromPK);
  gTIOPFManager.RegSaveVisitor(TVisRTSQuestion_Create);
  gTIOPFManager.RegSaveVisitor(TVisRTSQuestion_Update);
  gTIOPFManager.RegSaveVisitor(TVisRTSQuestion_Delete);

end;

{ TVisPeriod_Read }

function TVisPeriod_Read.AcceptVisitor: Boolean;
begin
  Result := (Visited is TPeriodCollection)
    and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisPeriod_Read.Init;
begin
  TableName:= 'period';
end;

procedure TVisPeriod_Read.MapRowToObject;
var
  LData: TPeriod;
begin
  LData := TPeriod.Create;
  LData.OID.AssignFromTIQuery('OID',Query);
  LData.ObjectState := posClean;
  TPeriodCollection(Visited).Add(LData);
end;

procedure TVisPeriod_Read.SetupParams;
begin
  // Do nothing
end;

{ TVisPeriod_ReadFromPK }

function TVisPeriod_ReadFromPK.AcceptVisitor: Boolean;
begin
  Result := (Visited is TPeriod)
    and (Visited.ObjectState = posPK);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisPeriod_ReadFromPK.Init;
begin
  TableName:= 'period';
end;

procedure TVisPeriod_ReadFromPK.MapRowToObject;
begin
  //since period has no non-PK data, there is nothing to do here
  TPeriod(Visited).ObjectState := posClean;
end;

procedure TVisPeriod_ReadFromPK.SetupParams;
begin
  // Do nothing
end;

{ TVisPeriod_Create }

function TVisPeriod_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TPeriod)
    and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisPeriod_Create.SetupParams;
var
  LData: TPeriod;
begin
  LData := Visited as TPeriod;
  TableName := 'period';
  QueryType := qtInsert;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
end;

{ TVisPeriod_Update }

function TVisPeriod_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TPeriod)
    and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisPeriod_Update.SetupParams;
var
  LData: TPeriod;
begin
  LData := Visited as TPeriod;
  TableName := 'period';
  QueryType := qtUpdate;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
end;

{ TVisPeriod_Delete }

function TVisPeriod_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TPeriod)
    and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisPeriod_Delete.SetupParams;
begin
  inherited;
  TableName := 'period';
end;

{ TVisShowMethod_Read }

function TVisShowMethod_Read.AcceptVisitor: Boolean;
begin
  Result := (Visited is TShowMethodCollection)
    and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisShowMethod_Read.Init;
begin
  TableName:= 'show_method';
end;

procedure TVisShowMethod_Read.MapRowToObject;
var
  LData: TShowMethod;
begin
  LData := TShowMethod.Create;
  LData.OID.AssignFromTIQuery('OID',Query);
  LData.ObjectState := posClean;
  LData.Name := Query.FieldAsString['show_method_name'];
  TShowMethodCollection(Visited).Add(LData);
end;

procedure TVisShowMethod_Read.SetupParams;
begin
  //do nothing
end;

{ TVisShowMethod_ReadFromPK }

function TVisShowMethod_ReadFromPK.AcceptVisitor: Boolean;
begin
  Result := (Visited is TShowMethod)
    and (Visited.ObjectState = posPK);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisShowMethod_ReadFromPK.Init;
begin
  //do nothing
end;

procedure TVisShowMethod_ReadFromPK.MapRowToObject;
var
  LData: TShowMethod;
begin
  LData := Visited as TShowMethod;
  LData.Name := Query.FieldAsString['show_method_name'];
  LData.ObjectState := posClean;
end;

procedure TVisShowMethod_ReadFromPK.SetupParams;
var
  LData: TShowMethod;
begin
  LData := Visited as TShowMethod;
  TableName := 'show_method';
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
end;

{ TVisShowMethod_Create }

function TVisShowMethod_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TShowMethod)
    and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisShowMethod_Create.SetupParams;
var
  LData: TShowMethod;
begin
  LData := Visited as TShowMethod;
  TableName := 'show_method';
  QueryType := qtInsert;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Name', LData.Name);
end;

{ TVisShowMethod_Update }

function TVisShowMethod_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TShowMethod) 
    and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisShowMethod_Update.SetupParams;
var
  LData: TShowMethod;
begin
  LData := Visited as TShowMethod;
  TableName := 'show_method';
  QueryType := qtInsert;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Name', LData.Name);
end;

{ TVisShowMethod_Delete }

function TVisShowMethod_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TShowMethod) 
    and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisShowMethod_Delete.SetupParams;
begin
  inherited;
  TableName := 'show_method';
end;

{ TVisMidpointList_Read }

function TVisMidpointList_Read.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointListCollection)
  	and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpointList_Read.Init;
begin
  TableName:= 'rts_midpoint_list';
end;

procedure TVisMidpointList_Read.MapRowToObject;
var
  LData: TMidpointList;
begin
  LData := TMidpointList.Create;
  LData.OID.AssignFromTIQuery('OID',Query);
  LData.ObjectState := posClean;
  LData.Name := Query.FieldAsString['midpoint_list_name'];
  TMidpointListCollection(Visited).Add(LData);
end;

procedure TVisMidpointList_Read.SetupParams;
begin
  //do nothing
end;

{ TVisMidpointList_ReadFromPK }

function TVisMidpointList_ReadFromPK.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointList)
    and (Visited.ObjectState = posPK);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpointList_ReadFromPK.Init;
begin
  //do nothing
end;

procedure TVisMidpointList_ReadFromPK.MapRowToObject;
var
  LData: TMidpointList;
begin
  LData := Visited as TMidpointList;
  LData.Name := Query.FieldAsString['midpoint_list_name'];
  LData.ObjectState := posClean;
end;

procedure TVisMidpointList_ReadFromPK.SetupParams;
var
  LData: TMidpointList;
begin
  LData := Visited as TMidpointList;
  TableName := 'rts_midpoint_list';
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
end;

{ TVisMidpointList_Create }

function TVisMidpointList_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointList)
  	and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpointList_Create.SetupParams;
var
  LData: TMidpointList;
begin
  LData := Visited as TMidpointList;
  TableName := 'rts_midpoint_list';
  QueryType := qtInsert;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('midpoint_list_name', LData.Name);
end;

{ TVisMidpointList_Update }

function TVisMidpointList_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointList)
  	and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpointList_Update.SetupParams;
var
  LData: TMidpointList;
begin
  LData := Visited as TMidpointList;
  TableName := 'rts_midpoint_list';
  QueryType := qtInsert;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('midpoint_list_name', LData.Name);
end;

{ TVisMidpointList_Delete }

function TVisMidpointList_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointList)
  	and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpointList_Delete.SetupParams;
begin
  inherited;
  TableName := 'rts_midpoint_list';
end;

{ TVisMidpoint_Read }

function TVisMidpoint_Read.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointCollection)
  	and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpoint_Read.Init;
begin
  TableName := 'rts_midpoint';
end;

procedure TVisMidpoint_Read.MapRowToObject;
var
  LData: TMidpoint;
begin
  {this code was for loading from TMidpointListCollection, which is too
  far up our Meta-Tree for the 'refresh' operation to work
  LOwnerOID := Query.FieldAsString['rts_midpoint_list_oid'];
  if (FMidpointList = nil) or (FMidpointList.OID.AsString <> LOwnerOID) then
    FMidpointList := TMidpointListCollection(Visited).Find(LOwnerOID) as TMidpointList;
  if not Assigned(FMidpointList) then
    raise Exception.CreateFmt('Can not find rts_midpoint_list OID=%s', [LOwnerOID]);
  LData := FMidpointList.Midpoints.Add;}

  //the above is replaced by this line for loading from TMidpointCollection
  LData := TMidpointCollection(Visited).Add;

  LData.OID.AssignFromTIQuery(Query);
  LData.Value := Query.FieldAsFloat['midpoint_value'];
  LData.Punch := Query.FieldAsInteger['midpoint_punch'];
  LData.Name := Query.FieldAsString['midpoint_name'];
  LData.ObjectState := posClean;
end;

procedure TVisMidpoint_Read.SetupParams;
var
  LCollection: TMidpointCollection;
begin
  LCollection := TMidpointCollection(Visited);
  QueryParams.SetValueAsString('rts_midpoint_list_oid', LCollection.OID.AsString);
end;

{ TVisMidpoint_ReadFromPK }

function TVisMidpoint_ReadFromPK.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpoint)
    and (Visited.ObjectState = posPK);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpoint_ReadFromPK.Init;
begin
  //do nothing
end;

procedure TVisMidpoint_ReadFromPK.MapRowToObject;
var
  LData: TMidpoint;
begin
  LData := Visited as TMidpoint;
  LData.Value := Query.FieldAsFloat['midpoint_value'];
  LData.Punch := Query.FieldAsInteger['midpoint_punch'];
  LData.Name := Query.FieldAsString['midpoint_name'];
  LData.ObjectState := posClean;
end;

procedure TVisMidpoint_ReadFromPK.SetupParams;
var
  LData: TMidpoint;
begin
  LData := Visited as TMidpoint;
  TableName := 'rts_midpoint';
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
end;

{ TVisMidpoint_Create }

function TVisMidpoint_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpoint)
  	and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpoint_Create.SetupParams;
var
  LData: TMidpoint;
begin
  LData := Visited as TMidpoint;
  TableName := 'rts_midpoint';
  QueryType := qtInsert;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('rts_midpoint_list_oid', LData.Owner.OID.AsString);
  QueryParams.SetValueAsInteger('midpoint_punch', LData.Punch);
  QueryParams.SetValueAsFloat('midpoint_value', LData.Value);
  QueryParams.SetValueAsString('midpoint_name', LData.Name);
end;

{ TVisMidpoint_Update }

function TVisMidpoint_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpoint)
  	and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpoint_Update.SetupParams;
var
  LData: TMidpoint;
begin
  LData := Visited as TMidpoint;
  TableName := 'rts_midpoint';
  QueryType := qtUpdate;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsInteger('midpoint_punch', LData.Punch);
  QueryParams.SetValueAsFloat('midpoint_value', LData.Value);
  QueryParams.SetValueAsString('midpoint_name', LData.Name);
end;

{ TVisMidpoint_Delete }

function TVisMidpoint_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpoint)
  	and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpoint_Delete.SetupParams;
begin
  inherited;
  TableName := 'rts_midpoint';
end;

{ TVisRTSQuestion_Read }

function TVisRTSQuestion_Read.AcceptVisitor: Boolean;
begin
  Result := (Visited is TRTSQuestionCollection)
  	and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisRTSQuestion_Read.Init;
begin
  TableName := 'rts_question';
end;

procedure TVisRTSQuestion_Read.MapRowToObject;
var
  LData: TRTSQuestion;
  AssocOID: String;
begin
  LData := TRTSQuestion.Create;
  LData.OID.AssignFromTIQuery('OID',Query);
  LData.Name := Query.FieldAsString['rts_question_name'];
  LData.Number := Query.FieldAsString['rts_question_number'];
  TRTSQuestionCollection(Visited).Add(LData);

  //locate association
  AssocOID := Query.FieldAsString['rts_midpoint_list_oid'];
  LData.MidpointList := MapRTSQuestionToMidpointList(LData, AssocOID);

  LData.ObjectState := posClean;
end;

procedure TVisRTSQuestion_Read.SetupParams;
begin
  //do nothing
end;

{ TVisRTSQuestion_ReadFromPK }

function TVisRTSQuestion_ReadFromPK.AcceptVisitor: Boolean;
begin
  Result := (Visited is TRTSQuestion)
  	and (Visited.ObjectState = posPK);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisRTSQuestion_ReadFromPK.Init;
begin
  //do nothing
end;

procedure TVisRTSQuestion_ReadFromPK.MapRowToObject;
var
  LData: TRTSQuestion;
  AssocOID: String;
begin
  LData := Visited as TRTSQuestion;
  LData.Name := Query.FieldAsString['rts_question_name'];
  LData.Number := Query.FieldAsString['rts_question_number'];

  //locate association
  AssocOID := Query.FieldAsString['rts_midpoint_list_oid'];
  LData.MidpointList := MapRTSQuestionToMidpointList(LData, AssocOID);

  LData.ObjectState := posClean;
end;

procedure TVisRTSQuestion_ReadFromPK.SetupParams;
var
  LData: TRTSQuestion;
begin
  LData := Visited as TRTSQuestion;
  TableName := 'rts_question';
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
end;

{ TVisRTSQuestion_Create }

function TVisRTSQuestion_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TRTSQuestion)
  	and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisRTSQuestion_Create.SetupParams;
var
  LData: TRTSQuestion;
begin
  LData := Visited as TRTSQuestion;
  TableName := 'rts_question';
  QueryType := qtInsert;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('rts_question_name', LData.Name);
  QueryParams.SetValueAsString('rts_question_number', LData.Number);
  if Assigned(LData.MidpointList) then
    QueryParams.SetValueAsString('rts_midpoint_list_oid', LData.MidpointList.OID.AsString)
  else
    QueryParams.ParamIsNull['rts_midpoint_list_oid'] := True;
end;

{ TVisRTSQuestion_Update }

function TVisRTSQuestion_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TRTSQuestion)
  	and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisRTSQuestion_Update.SetupParams;
var
  LData: TRTSQuestion;
begin
  LData := Visited as TRTSQuestion;
  TableName := 'rts_question';
  QueryType := qtUpdate;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('rts_question_name', LData.Name);
  QueryParams.SetValueAsString('rts_question_number', LData.Number);
  if Assigned(LData.MidpointList) then
    QueryParams.SetValueAsString('rts_midpoint_list_oid', LData.MidpointList.OID.AsString)
  else
    QueryParams.ParamIsNull['rts_midpoint_list_oid'] := True;
end;

{ TVisRTSQuestion_Delete }

function TVisRTSQuestion_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TRTSQuestion)
  	and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisRTSQuestion_Delete.SetupParams;
begin
  TableName := 'rts_question';
end;

end.
