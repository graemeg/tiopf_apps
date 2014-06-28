unit Transit_HardCodeVisitors_Svr;

interface

uses
   tiVisitorDB
  , tiLog
  , tiOPFManager
  , tiObject
  , Transit_BOM
  ;

type
  TVisPeriod_Read = class( TVisOwnedQrySelect )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisPeriod_ReadFromPK = class( TVisOwnedQrySelect )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisPeriod_Create = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisPeriod_Update = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisPeriod_Delete = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisShowMethod_Read = class( TVisOwnedQrySelect )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisShowMethod_ReadFromPK = class( TVisOwnedQrySelect )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisShowMethod_Create = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisShowMethod_Update = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisShowMethod_Delete = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisMidpointList_Read = class( TVisOwnedQrySelect )
  private
    FMaster: TMidpointList;
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisMidpointList_ReadFromPK = class( TVisOwnedQrySelect )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisMidpointList_Create = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisMidpointList_Update = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisMidpointList_Delete = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
    procedure Final         ; override ;
  end;

  TVisMidpoint_ReadFromPK = class( TVisOwnedQrySelect )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisMidpoint_Create = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisMidpoint_Update = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisMidpoint_Delete = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisRTSQuestion_Read = class( TVisOwnedQrySelect )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisRTSQuestion_ReadFromPK = class( TVisOwnedQrySelect )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  	procedure MapRowToObject; override;
  end;

  TVisRTSQuestion_Create = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisRTSQuestion_Update = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

  TVisRTSQuestion_Delete = class( TVisOwnedQryUpdate )
  protected
  	function AcceptVisitor: Boolean; override;
  	procedure Init; override;
  	procedure SetupParams; override;
  end;

procedure RegisterVisitors;

implementation

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
  gTIOPFManager.RegReadVisitor(TVisMidpoint_ReadFromPK);

  gTIOPFManager.RegSaveVisitor(TVisMidpointList_Create);
  gTIOPFManager.RegSaveVisitor(TVisMidpointList_Update);
  gTIOPFManager.RegSaveVisitor(TVisMidpoint_Create);
  gTIOPFManager.RegSaveVisitor(TVisMidpoint_Update);

  gTIOPFManager.RegSaveVisitor(TVisMidpoint_Delete);
  gTIOPFManager.RegSaveVisitor(TVisMidpointList_Delete);

  gTIOPFManager.RegSaveVisitor(TVisMidpoint_Create);
  gTIOPFManager.RegSaveVisitor(TVisMidpointList_Create);

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
  Query.SQLText :=
    'select OID from period';
end;

procedure TVisPeriod_Read.MapRowToObject;
var
  LData: TPeriod;
begin
  LData := TPeriodCollection(Visited).Add;
  LData.OID.AssignFromTIQuery('OID',Query);
  LData.ObjectState := posClean ;
end;

procedure TVisPeriod_Read.SetupParams;
begin
  //do nothing
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
  Query.SQLText :=
    'select OID from period' +
    'where OID = :OID';
end;

procedure TVisPeriod_ReadFromPK.MapRowToObject;
begin
  //nothing to read in this class, since period doesn't have any non-PK fields
  TPeriod(Visited).ObjectState := posClean;
end;

procedure TVisPeriod_ReadFromPK.SetupParams;
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
end;

{ TVisPeriod_Create }

function TVisPeriod_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TPeriod)
  	and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisPeriod_Create.Init;
begin
  Query.SQLText :=
    'Insert into period ( OID ) ' +
    'Values ' +
    '( :OID )';
end;

procedure TVisPeriod_Create.SetupParams;
var
  LData: TPeriod;
begin
  LData := Visited as TPeriod;
  LData.OID.AssignToTIQuery('OID', Query);
end;

{ TVisPeriod_Update }

function TVisPeriod_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TPeriod)
  	and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisPeriod_Update.Init;
begin
  Query.SQLText :=
    'Update period Set ' +
    'where ' +
    ' OID = :OID';

  //there are no non-PK fields, so there's nothing to ever update
end;

procedure TVisPeriod_Update.SetupParams;
var
  LData: TPeriod;
begin
  LData := Visited as TPeriod;
  lData.OID.AssignToTIQuery('OID', Query);
end;

{ TVisPeriod_Delete }

function TVisPeriod_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TPeriod)
  	and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisPeriod_Delete.Init;
begin
  Query.SQLText :=
    'delete from period where oid = :oid';
end;

procedure TVisPeriod_Delete.SetupParams;
var
  LData: TPeriod;
begin
  lData := Visited as TPeriod;
  lData.OID.AssignToTIQuery('OID', Query);
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
  Query.SQLText :=
    'select OID, show_method_name from show_method';
end;

procedure TVisShowMethod_Read.MapRowToObject;
var
  LData: TShowMethod;
begin
  LData := TShowMethodCollection(Visited).Add;
  LData.OID.AssignFromTIQuery('OID',Query);
  LData.Name := Query.FieldAsString['show_method_name'];
  LData.ObjectState := posClean;
end;

procedure TVisShowMethod_Read.SetupParams;
begin
  // Do nothing
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
  Query.SQLText :=
    'select OID, show_method_name from show_method' +
    'where ' +
    ' OID = :OID';
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
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
end;

{ TVisShowMethod_Create }

function TVisShowMethod_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TShowMethod)
  	and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisShowMethod_Create.Init;
begin
  Query.SQLText :=
    'Insert into show_method ( OID, show_method_name ) ' +
    'Values ' +
    '( :OID, :show_method_name )';
end;

procedure TVisShowMethod_Create.SetupParams;
var
  LData: TShowMethod;
begin
  LData := Visited as TShowMethod;
  LData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['show_method_name'] := LData.Name;
end;

{ TVisShowMethod_Update }

function TVisShowMethod_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TShowMethod)
  	and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisShowMethod_Update.Init;
begin
  Query.SQLText :=
    'Update show_method Set ' +
    '  show_method_name = :show_method_name ' +
    'where ' +
    ' OID = :OID';
end;

procedure TVisShowMethod_Update.SetupParams;
var
  LData: TShowMethod;
begin
  LData := Visited as TShowMethod;
  LData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['show_method_name'] := LData.Name;
end;

{ TVisShowMethod_Delete }

function TVisShowMethod_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TShowMethod)
  	and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisShowMethod_Delete.Init;
begin
  Query.SQLText :=
    'delete from show_method where oid = :oid';
end;

procedure TVisShowMethod_Delete.SetupParams;
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
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
  Query.SQL.Text :=
    'select ' +
    '   ml.OID          as midpoint_list_oid ' +
    '  ,ml.midpoint_list_name  as midpoint_list_name ' +
    '  ,m.OID         as midpoint_oid ' +
    '  ,m.midpoint_name as midpoint_name ' +
    '  ,m.midpoint_value as midpoint_value ' +
    '  ,m.midpoint_value as midpoint_punch ' +
    'from ' +
    '          rts_midpoint_list ml ' +
    'left join rts_midpoint m on m.rts_midpoint_list_oid = ml.OID ' +
    'order by ' +
    'midpoint_list_name ' ;
end;

procedure TVisMidpointList_Read.MapRowToObject;
var
  LDetail: TMidpoint;
begin
	if (FMaster = nil) or
		 (FMaster.OID.AsString <> Query.FieldAsString['midpoint_list_oid']) then
	begin
    FMaster := TMidpointListCollection(Visited).Add;
		FMaster.OID.AssignFromTIQuery('midpoint_list_oid', Query);
		FMaster.Name := Query.FieldAsString['midpoint_list_name'];
		FMaster.ObjectState := posClean;
		FMaster.Midpoints.ObjectState := posClean;
	end ;

	if Query.FieldAsString['midpoint_oid'] <> '' then
	begin
    LDetail := FMaster.Midpoints.Add;
		LDetail.OID.AssignFromTIQuery('midpoint_oid', Query );
		LDetail.Name := Query.FieldAsString['midpoint_name'];
		LDetail.Value := Query.FieldAsFloat['midpoint_value'];
		LDetail.Punch := Query.FieldAsInteger['midpoint_punch'];
    LDetail.ObjectState := posClean ;
  end ;
end;

procedure TVisMidpointList_Read.SetupParams;
begin
  // Do nothing
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
  Query.SQLText :=
    'select OID, midpoint_list_name from rts_midpoint_list' +
    'where ' +
    ' OID = :OID';
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
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
end;

{ TVisMidpointList_Create }

function TVisMidpointList_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointList)
  	and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpointList_Create.Init;
begin
  Query.SQLText :=
    'Insert into rts_midpoint_list ( OID, midpoint_list_name ) ' +
    'Values ' +
    '( :OID, :midpoint_list_name )';
end;

procedure TVisMidpointList_Create.SetupParams;
var
  LData: TMidpointList;
begin
  LData := Visited as TMidpointList;
  LData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['midpoint_list_name'] := LData.Name;
end;

{ TVisMidpointList_Update }

function TVisMidpointList_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointList)
  	and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpointList_Update.Init;
begin
  Query.SQLText :=
    'Update rts_midpoint_list Set ' +
    '  midpoint_list_name = :midpoint_list_name ' +
    'where ' +
    ' OID = :OID';
end;

procedure TVisMidpointList_Update.SetupParams;
var
  LData: TMidpointList;
begin
  LData := Visited as TMidpointList;
  LData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['midpoint_list_name'] := LData.Name;
end;

{ TVisMidpointList_Delete }

function TVisMidpointList_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpointList)
  	and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpointList_Delete.Final;
var
  LData: TMidpointList;
begin
  inherited;
  LData := Visited as TMidpointList;
  lData.Midpoints.ObjectState := posClean ;
end;

procedure TVisMidpointList_Delete.Init;
begin
  Query.SQLText :=
    'delete from rts_midpoint_list where oid = :oid';
end;

procedure TVisMidpointList_Delete.SetupParams;
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
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
  Query.SQLText :=
    'select OID, midpoint_name from rts_midpoint' +
    'where ' +
    ' OID = :OID';
end;

procedure TVisMidpoint_ReadFromPK.MapRowToObject;
var
  LData: TMidpoint;
begin
  LData := Visited as TMidpoint;
  LData.Name := Query.FieldAsString['midpoint_name'];
  LData.ObjectState := posClean;
end;

procedure TVisMidpoint_ReadFromPK.SetupParams;
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
end;

{ TVisMidpoint_Create }

function TVisMidpoint_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpoint)
  	and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpoint_Create.Init;
begin
  Query.SQLText :=
    'Insert into rts_midpoint ( OID, rts_midpoint_list_oid, midpoint_name, midpoint_value, midpoint_punch ) ' +
    'Values ' +
    '( :OID, :rts_midpoint_list_oid, :midpoint_name, :midpoint_value, :midpoint_punch )';
end;

procedure TVisMidpoint_Create.SetupParams;
var
  LData: TMidpoint;
begin
  LData := Visited as TMidpoint;
  LData.OID.AssignToTIQuery('OID', Query);
	LData.Owner.OID.AssignToTIQuery('rts_midopoint_list_oid', Query);
  Query.ParamAsString['midpoint_name'] := LData.Name;
  Query.ParamAsFloat['midpoint_value'] := LData.Value;
  Query.ParamAsInteger['midpoint_punch'] := LData.Punch;
end;

{ TVisMidpoint_Update }

function TVisMidpoint_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpoint)
  	and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpoint_Update.Init;
begin
  Query.SQLText :=
    'Update rts_midpoint Set ' +
    '  midpoint_name = :midpoint_name ' +
    ' ,rts_midpoint_list_oid = :rts_midpoint_list_oid ' +
    ' ,midpoint_value = :midpoint_value ' +
    ' ,midpoint_punch = :midpoint_punch ' +
    'where ' +
    ' OID = :OID';
end;

procedure TVisMidpoint_Update.SetupParams;
var
  LData: TMidpoint;
begin
  LData := Visited as TMidpoint;
  LData.OID.AssignToTIQuery('OID', Query);
	LData.Owner.OID.AssignToTIQuery('rts_midopoint_list_oid', Query);
  Query.ParamAsString['midpoint_name'] := LData.Name;
  Query.ParamAsFloat['midpoint_value'] := LData.Value;
  Query.ParamAsInteger['midpoint_punch'] := LData.Punch;
end;

{ TVisMidpoint_Delete }

function TVisMidpoint_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TMidpoint)
  	and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisMidpoint_Delete.Init;
begin
  Query.SQLText :=
    'delete from rts_midpoint where oid = :oid';
end;

procedure TVisMidpoint_Delete.SetupParams;
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
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
  Query.SQLText :=
    'select OID, rts_question_name, rts_question_number, rts_midpoint_list_oid from rts_question';
end;

procedure TVisRTSQuestion_Read.MapRowToObject;
var
  LData: TRTSQuestion;
  AssocOID: String;
begin
  LData := TRTSQuestionCollection(Visited).Add;
  LData.OID.AssignFromTIQuery('OID',Query);
  LData.Name := Query.FieldAsString['rts_question_name'];
  LData.Number := Query.FieldAsString['rts_question_number'];

  //locate association
  AssocOID := Query.FieldAsString['rts_midpoint_list_oid'];
  LData.MidpointList := MapRTSQuestionToMidpointList(LData, AssocOID);

  LData.ObjectState := posClean;
end;

procedure TVisRTSQuestion_Read.SetupParams;
begin
  // Do nothing
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
  Query.SQLText :=
    'select OID, rts_question_name, rts_question_number, rts_midpoint_list_oid from rts_question' +
    'where ' +
    ' OID = :OID';
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
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
end;

{ TVisRTSQuestion_Create }

function TVisRTSQuestion_Create.AcceptVisitor: Boolean;
begin
  Result := (Visited is TRTSQuestion)
  	and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisRTSQuestion_Create.Init;
begin
  Query.SQLText :=
    'Insert into rts_question ( OID, rts_question_name, rts_question_number, rts_midpoint_list_oid ) ' +
    'Values ' +
    '( :OID, :rts_question_name, :rts_question_number, :rts_midpoint_list_oid )';
end;

procedure TVisRTSQuestion_Create.SetupParams;
var
  LData: TRTSQuestion;
begin
  LData := Visited as TRTSQuestion;
  LData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['rts_question_name'] := LData.Name;
  Query.ParamAsString['rts_question_number'] := LData.Number;
	LData.Owner.OID.AssignToTIQuery('rts_midopoint_list_oid', Query);
end;

{ TVisRTSQuestion_Update }

function TVisRTSQuestion_Update.AcceptVisitor: Boolean;
begin
  Result := (Visited is TRTSQuestion)
  	and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisRTSQuestion_Update.Init;
begin
  Query.SQLText :=
    'Update rts_question Set ' +
    '  rts_question_name = :rts_question_name ' +
    '  rts_question_number = :rts_question_number ' +
    '  rts_midpoint_list_oid = :rts_midpoint_list_oid ' +
    'where ' +
    ' OID = :OID';
end;

procedure TVisRTSQuestion_Update.SetupParams;
var
  LData: TRTSQuestion;
begin
  LData := Visited as TRTSQuestion;
  LData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['rts_question_name'] := LData.Name;
  Query.ParamAsString['rts_question_number'] := LData.Number;
	LData.Owner.OID.AssignToTIQuery('rts_midopoint_list_oid', Query);
end;

{ TVisRTSQuestion_Delete }

function TVisRTSQuestion_Delete.AcceptVisitor: Boolean;
begin
  Result := (Visited is TRTSQuestion)
  	and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result]);
end;

procedure TVisRTSQuestion_Delete.Init;
begin
  Query.SQLText :=
    'delete from rts_question where oid = :oid';
end;

procedure TVisRTSQuestion_Delete.SetupParams;
begin
  TtiObject(Visited).OID.AssignToTIQuery('OID', Query);
end;

end.
