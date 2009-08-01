unit tiSQLMgr_Svr;

{$I tiDefines.inc}

interface
uses
   classes
  ,tiSQLMgr_BOM
  ,tiVisitor
  ,tiVisitorDB
  ,tiVisitorDBAutoGen
  ,ComCtrls
  ,tiQuery
  ;

type

  // Read Group primary key info
  //----------------------------------------------------------------------------
  TVisReadGroupPK = class( TVisDBAutoGenRead )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  MapRowToObject ; override ;
  end ;

  // Read Query primary key info
  //----------------------------------------------------------------------------
  TVisReadQueryPK = class( TVisDBAutoGenRead )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  MapRowToObject ; override ;
  end ;

  // Read a queries SQL by the Querie's name
  //----------------------------------------------------------------------------
  TVisReadQueryByName = class( TVisDBAutoGenRead )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  SetupParams    ; override ;
    procedure  MapRowToObject ; override ;
  end ;

{
  // Read the details of a query
  //----------------------------------------------------------------------------
  TVisReadQueryDetail = class( TVisOwnedQrySelect )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  SetupParams    ; override ;
    procedure  MapRowToObject ; override ;
  end ;

  // Read the parameters for a given query
  //----------------------------------------------------------------------------
  TVisReadParams = class( TVisOwnedQrySelect )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  SetupParams    ; override ;
    procedure  MapRowToObject ; override ;
  end ;

  // Create (insert) a query
  //----------------------------------------------------------------------------
  TVisCreateQuery = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Update a query
  //----------------------------------------------------------------------------
  TVisUpdateQuery = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Delete a query
  //----------------------------------------------------------------------------
  TVisDeleteQuery = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Create a group to hold a family of related queries
  //----------------------------------------------------------------------------
  TVisCreateGroup = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Update a group (change its name or position)
  //----------------------------------------------------------------------------
  TVisUpdateGroup = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Delete a group
  //----------------------------------------------------------------------------
  TVisDeleteGroup = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Update a parameter
  //----------------------------------------------------------------------------
  TVisUpdateParam = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Create (insert) a parameter
  //----------------------------------------------------------------------------
  TVisCreateParam = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Delete a parameter
  //----------------------------------------------------------------------------
  TVisDeleteParam = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Mark the container of the parameters as deleted
  //----------------------------------------------------------------------------
  TVisDeleteParamContainer = class( TVisQryObjectStateDeleted )
  protected
    function AcceptVisitor : boolean ; override ;
  end ;
}

implementation
uses
  Dialogs   // ShowMessage, for debugging
  ,SysUtils // IntToStr
  ,Windows
  ,tiLog
  ,tiUtils
  ,tiObject
  ,tiConstants
  ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisReadGroupPK
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisReadGroupPK.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgr ) and
            ( Visited.ObjectState = posEmpty ) ;
end;

// -----------------------------------------------------------------------------
procedure TVisReadGroupPK.Init;
begin
  TableName := cTableNameSQLManGroup ;
end;

procedure TVisReadGroupPK.MapRowToObject ;
var
  lData : TSQLMgrGroup ;
begin

  // ToDo: Change all references to SQLManager field names to constancts

  lData             := TSQLMgrGroup.Create ;
  lData.ObjectState := posClean ;
  {$IFDEF OID_AS_INT64}
    lData.OID := Query.FieldAsInteger['OID'];
  {$ELSE}
    lData.OID.AssignFromTIQuery( Query ) ;
  {$ENDIF}
  lData.GroupName   := Query.FieldAsString[  'group_name' ] ;
  ( Visited as TSQLMgr ).Add( lData ) ;
end;

function TVisReadQueryPK.AcceptVisitor: boolean;
begin
  result := (  Visited is TSQLMgr ) and
            (( Visited as TtiObject ).ObjectState = posEmpty ) ;
end;

// -----------------------------------------------------------------------------
procedure TVisReadQueryPK.Init;
begin
  TableName := cTableNameSQLManSQL ;
end;

procedure TVisReadQueryPK.MapRowToObject;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lData : TSQLMgrQuery ;
  i : integer ;
begin

  Assert( Visited.TestValid(TSQLMgr), CTIErrorInvalidObject);
  lSQLMgr := ( Visited as TSQLMgr ) ;

  lData             := TSQLMgrQuery.Create ;
  lData.ObjectState := posPK ;
  {$IFDEF OID_AS_INT64}
    lData.OID := Query.FieldAsInteger['OID'];
  {$ELSE}
    lData.OID.AssignFromTIQuery( Query ) ;
  {$ENDIF}
  lData.QueryName   := Query.FieldAsString[  'Query_name' ] ;
  lData.QueryLocked := Query.FieldAsBoolean[ 'Query_Locked' ];
  lData.TestInclude := Query.FieldAsBoolean[ 'Test_Include' ];

  // Now scan for the correct group to add the query to
  for i := 0 to lSQLMgr.Count - 1 do
  begin
    Assert( lSQLMgr.Items[i].TestValid(TSQLMgrGroup), CTIErrorInvalidObject);
    lSQLMgrGroup := (lSQLMgr.Items[i] as TSQLMgrGroup );
  {$IFDEF OID_AS_INT64}
    if lSQLMgrGroup.OID = Query.FieldAsInteger['Group_OID'] then
    begin
      lSQLMgrGroup.Add( lData, false ) ;
      Break ; //==>
    end ;
  {$ELSE}
    if lSQLMgrGroup.OID.EqualsQueryField( 'Group_OID', Query ) then
    begin
      lSQLMgrGroup.Add( lData ) ;
      Break ; //==>
    end ;
  {$ENDIF}
  end ;
  Assert( lData.Owner <> nil, 'SQLMgrQuery.Owner = nil');
end;

(*
procedure TVisReadQueryDetail.SetupParams;
begin
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := ( Visited as TPerObjAbs ).OID ;
  {$ELSE}
    ( Visited as TPerObjAbs ).OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
end;

procedure TVisReadParams.SetupParams;
begin
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['SQL_OID'] := ( Visited as TSQLMgrParams ).Owner.OID ;
  {$ELSE}
    ( Visited as TSQLMgrParams ).Owner.OID.AssignToTIQuery( 'SQL_OID', Query ) ;
  {$ENDIF}
end;

{ TVisDeleteParamContainer }

function TVisDeleteParamContainer.AcceptVisitor: boolean;
begin
  result := ( inherited AcceptVisitor ) and
            ( Visited is TSQLMgrParams ) ;
end;

procedure TVisCreateQuery.Init;
begin
  Query.SQLText :=
    UpperCase(
      tiStrTran( cusSQLMgrQueryCreate,
                 '&SQL_Col_Name',
                 cuSQLColName )) ;
end;

procedure TVisCreateQuery.SetupParams;
var
  lData : TSQLMgrQuery ;
begin
  lData := TSQLMgrQuery( Visited ) ;
  Query.ParamAsString[  'query_name'        ] := lData.QueryName ;
  Query.ParamAsString[  'Query_description' ] := lData.QueryDesc ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[ 'oid'               ] := lData.OID ;
    Query.ParamAsInteger[ 'Group_oid'         ] := lData.Owner.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
    lData.Owner.OID.AssignToTIQuery( 'Group_oid', Query ) ;
  {$ENDIF}
  Query.ParamAsInteger[ 'Disp_Order'        ] := lData.DispOrder ;
  Query.ParamAsBoolean[ 'Query_Locked'      ] := lData.QueryLocked;
  Query.ParamAsBoolean[ 'Test_Include'      ] := lData.TestInclude;
  //Query.ParamAsTextBLOB[ 'Query_SQL'              ] := lData.SQL ;
  Query.ParamAsString[ 'Query_SQL'              ] := lData.SQL ;
end;

procedure TVisUpdateQuery.Init;
begin
  Query.SQLText :=
    UpperCase(
      tiStrTran( cusSQLMgrQueryUpdate,
                 '&SQL_Col_Name',
                 cuSQLColName )) ;
end;

procedure TVisUpdateQuery.SetupParams;
var
  lData : TSQLMgrQuery ;
begin
  lData := TSQLMgrQuery( Visited ) ;
  Query.ParamAsString[   'query_name'        ] := lData.QueryName ;
  Query.ParamAsString[   'query_description' ] := lData.QueryDesc ;
  Query.ParamAsInteger[  'disp_order' ]        := lData.DispOrder ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[  'group_oid'  ]        := lData.Owner.OID ;
    Query.ParamAsInteger[  'oid' ]               := lData.OID ;
  {$ELSE}
    lData.Owner.OID.AssignToTIQuery( 'group_oid', Query ) ;
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}

  Query.ParamAsBoolean[  'query_locked'  ]     := lData.QueryLocked ;
  Query.ParamAsBoolean[  'Test_Include'  ]     := lData.TestInclude ;
  //Query.ParamAsTextBlob[ 'Query_SQL' ]         := lData.SQL ;
  Query.ParamAsString[ 'Query_SQL' ]         := lData.SQL ;
end;

procedure TVisDeleteQuery.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrQueryDelete ) ;
end;

procedure TVisDeleteQuery.SetupParams;
var
  lData : TSQLMgrQuery ;
begin
  lData := TSQLMgrQuery( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
end;

procedure TVisCreateGroup.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrGroupCreate ) ;
end;

procedure TVisCreateGroup.SetupParams;
var
  lData : TSQLMgrGroup ;
begin
  lData := TSQLMgrGroup( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
  Query.ParamAsString[   cFieldNameGroupName ] := lData.GroupName ;
  Query.ParamAsInteger[  cFieldNameGroupDispOrder  ] := lData.DispOrder ;
end;

procedure TVisUpdateGroup.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrGroupUpdate ) ;
end;

procedure TVisUpdateGroup.SetupParams;
var
  lData : TSQLMgrGroup ;
begin
  lData := TSQLMgrGroup( Visited ) ;
  Query.ParamAsString[  'group_Name' ] := lData.GroupName ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[ 'oid'        ] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
  Query.ParamAsInteger[ 'disp_order' ] := lData.DispOrder ;
end;

procedure TVisDeleteGroup.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrGroupDelete ) ;
end;

procedure TVisDeleteGroup.SetupParams;
var
  lData : TSQLMgrGroup ;
begin
  lData := TSQLMgrGroup( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[  'oid' ] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
end;

procedure TVisUpdateParam.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrParamUpdate ) ;
end;

procedure TVisUpdateParam.SetupParams;
var
  lData : TSQLMgrParam ;
begin
  lData := TSQLMgrParam( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[ 'oid'     ] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
  Query.ParamAsInteger[ cFieldNameParamDispOrder ] := lData.DispOrder ;
  Query.ParamAsString[ 'param_name'   ] := lData.ParamName ;
  Query.ParamAsString[ 'param_type'   ] := lData.ParamTypeStr;
  Query.ParamAsString[ 'param_value'  ] := lData.ParamValue ;
  Query.ParamAsBoolean[ 'param_isnull'] := lData.IsNull;
end;

procedure TVisCreateParam.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrParamCreate ) ;
end;

procedure TVisCreateParam.SetupParams;
var
  lData : TSQLMgrParam ;
begin
  lData := TSQLMgrParam( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[ 'sql_oid' ]   := lData.Owner.Owner.OID  ;
    Query.ParamAsInteger[ 'oid'     ]   := lData.OID  ;
  {$ELSE}
    lData.Owner.Owner.OID.AssignToTIQuery( 'SQL_OID', Query ) ;
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}

  Query.ParamAsInteger[ cFieldNameParamDispOrder ] := lData.DispOrder ;
  Query.ParamAsString[  'param_name'  ]   := lData.ParamName  ;
  Query.ParamAsString[  'param_type'  ]   := lData.ParamTypeStr  ;
  Query.ParamAsString[  'param_value' ]   := lData.ParamValue ;
  Query.ParamAsBoolean[  'param_isnull' ] := lData.IsNull ;
end;

procedure TVisDeleteParam.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrParamDelete ) ;
end;

procedure TVisDeleteParam.SetupParams;
var
  lData : TSQLMgrParam ;
begin
  lData := TSQLMgrParam( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
end;

{ TVisReadQueryByName }
*)

function TVisReadQueryByName.AcceptVisitor: boolean;
begin
  result := ( Visited is TSQLMgrQuery ) and
            (( Visited as TtiObject ).ObjectState = posEmpty ) ;
end;

procedure TVisReadQueryByName.Init;
begin
  TableName := cTableNameSQLManSQL ;
end;

procedure TVisReadQueryByName.MapRowToObject;
var
  lData : TSQLMgrQuery ;
begin
  lData           := TSQLMgrQuery( Visited ) ;
  lData.SQL       := Query.FieldAsString[ cFieldNameSQLSQL ] ;
  lData.ObjectState := posClean ;
end;

procedure TVisReadQueryByName.SetupParams;
begin
  QueryParams.SetValueAsString(cFieldNameSQLName,
                               ( Visited as TSQLMgrQuery ).QueryName );
end;


initialization


{
  // Register visitors with the VisitorCache
  gTIPerMgr.RegReadPKVisitor( TVisReadGroupPK ) ;
  gTIPerMgr.RegReadPKVisitor( TVisReadQueryPK ) ;

  gTIPerMgr.RegReadVisitor( TVisReadQueryDetail ) ;
  gTIPerMgr.RegReadVisitor( TVisReadParams ) ;

  // The order of these is important:
  // Child nodes must be deleted before parents,
  // Parent nodes must be added before children.
  gTIPerMgr.RegSaveVisitor( TVisDeleteParam ) ;
  gTIPerMgr.RegSaveVisitor( TVisDeleteParamContainer ) ;
  gTIPerMgr.RegSaveVisitor( TVisDeleteQuery ) ;
  gTIPerMgr.RegSaveVisitor( TVisDeleteGroup ) ;
  gTIPerMgr.RegSaveVisitor( TVisUpdateQuery ) ;
  gTIPerMgr.RegSaveVisitor( TVisUpdateParam ) ;
  gTIPerMgr.RegSaveVisitor( TVisUpdateGroup ) ;
  gTIPerMgr.RegSaveVisitor( TVisCreateGroup ) ;
  gTIPerMgr.RegSaveVisitor( TVisCreateQuery ) ;
  gTIPerMgr.RegSaveVisitor( TVisCreateParam ) ;

}


end.


