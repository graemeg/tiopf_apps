{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Revision history:
    Nov 2000, Peter Hinrichsen, Made open source
    Nov 2000, SM, Moved query find method to TSQLMgr
    Nov 2000, SM, Added ParamIsNull property to TSQLMgrParam

  Purpose:
    Family of objects to manage SQL Text as read from database

  Classes:

  ToDo:
    1. Manage relationship between persistence layer and SQLManager objects
    2. Unload SQLManager mappings when unloading a persistence layer

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit tiSQLMgr_BOM;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  tiVisitor,
  tiObject,
  tiQuery,
  SyncObjs,
  tiOID;

const
  cExcCanNotSetObjectStateDeleteWhenPK = 'Can not set ObjectState to posDelete when ObjectState = posPK';

type

  TSQLMgrParamName = string[30];

  // Forward declarations
  TSQLMgrs      = class;
  TSQLMgr       = class;
  TSQLMgrGroup  = class;
  TSQLMgrQuery  = class;
  TSQLMgrParams = class;
  TSQLMgrParam  = class;


  // A visitor for finding a group of queries based on a pattern match on the
  // queries name.
  TVisFindQueriesByName = class(TtiVisitor)
  private
    FsPattern: string;
    FList: TList;
  protected
    function AcceptVisitor: Boolean; override;
  public
    property List: TList read FList write FList;
    property Pattern: string read FsPattern write FsPattern;
    procedure Execute(const pVisited: TtiVisited); override;
  end;


  TSQLMgrs = class(TtiObjectList)
  private
    FCritSect: TCriticalSection;
  protected
    function GetCaption: string; override;
    function GetItems(i: integer): TSQLMgr; reintroduce;
    procedure SetItems(i: integer; const Value: TSQLMgr); reintroduce;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Items[i: integer]: TSQLMgr read GetItems write SetItems;
    procedure Add(pObject: TSQLMgr); reintroduce;
    function AddDatabase(const pDatabaseName: string): TSQLMgr;
    function FindByDatabaseName(const pDatabaseName: string): TSQLMgr;
    procedure Clear; override;
  published
  end;


  // The main container for SQLManager data
  TSQLMgr = class(TtiObjectList)
  private
    FDatabaseName: string;
    FQueries: TSQLMgrGroup;
    FCritSect: TCriticalSection;
    FFileName: string;
  protected
    function GetCaption: string; override;
    function GetItems(i: integer): TSQLMgrGroup; reintroduce;
    procedure SetItems(i: integer; const Value: TSQLMgrGroup); reintroduce;
  public
    constructor Create; override;
    destructor Destroy; override;
    property FileName: string read FFileName write FFileName;
    class procedure CreateFile(const pDBConnectionName: string);

    function FindQueryByName(const pQueryName: string): TSQLMgrQuery;
    function FindCreateQueryByName(const pQueryName: string): TSQLMgrQuery;
    procedure FindQueriesByName(const psPattern: string; pList: TList);
    procedure ValidateModel;
    procedure CreateTables;
    procedure DropTables;

    //class function  CheckTableStructure( const pDBConnectionName : string = '' ;
    //                          const pPerLayerName     : string = ''): boolean ;
    procedure Read(const pDBConnectionName: string; pPerLayerName: string = ''); override;
    procedure ReadPK(const pDBConnectionName: string; pPerLayerName: string = ''); override;
    procedure Save(const pDBConnectionName: string; pPerLayerName: string = ''); override;
    property Items[i: integer]: TSQLMgrGroup read GetItems write SetItems; default;
    procedure Add(pObject: TSQLMgrGroup); reintroduce;
    property Queries: TSQLMgrGroup read FQueries;
    function IsGroupNameUnique(const pGroup: TSQLMgrGroup): Boolean;
    function IsQueryNameUnique(const pQuery: TSQLMgrQuery): Boolean;
  published
    property DatabaseName: string read FDatabaseName write FDatabaseName;
  end;


  TSQLMgrGroup = class(TtiObjectList)
  private
    FStrGroupName: string;
    FDispOrder: integer;
  protected
    function GetCaption: string; override;
    function GetOwner: TSQLMgr; reintroduce;
    procedure SetOwner(const Value: TSQLMgr); reintroduce;
    function GetItems(i: integer): TSQLMgrQuery; reintroduce;
    procedure SetItems(i: integer; const Value: TSQLMgrQuery); reintroduce;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Owner: TSQLMgr read GetOwner write SetOwner;
    property Items[i: integer]: TSQLMgrQuery read GetItems write SetItems; default;
    function Add(const AObject: TtiObject): integer; override;
  published
    property GroupName: string read FStrGroupName write FStrGroupName;
    property Caption;
    property DispOrder: integer read FDispOrder write FDispOrder;
  end;


  TSQLMgrQuery = class(TtiObject)
  private
    FStrQueryName: string;
    FStrQueryDesc: string;
    FStrSQL: string;
    FParams: TSQLMgrParams;
    FbQueryLocked: Boolean;
    FbTestInclude: Boolean;
    FQueryVersion: integer;
    FDispOrder: integer;
    function GetQueryGroupName: string;
  protected
    function GetCaption: string; override;
    function GetOwner: TSQLMgrGroup; reintroduce;
    procedure SetOwner(const Value: TSQLMgrGroup); reintroduce;
    procedure SetObjectState(const Value: TPerObjectState); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Owner: TSQLMgrGroup read GetOwner write SetOwner;
    procedure ReadByQueryName(const pQueryName: string; const pSQLMgrFileName: string);
    function IsParamNameUnique(const pParam: TSQLMgrParam): Boolean;
  published
    property DispOrder: integer read FDispOrder write FDispOrder;
    property QueryName: string read FStrQueryName write FStrQueryName;
    property QueryDesc: string read FStrQueryDesc write FStrQueryDesc;
    property QueryGroupName: string read GetQueryGroupName;
    property QueryVersion: integer read FQueryVersion write FQueryVersion;
    property SQL: string read FStrSQL write FStrSQL;
    property Params: TSQLMgrParams read FParams write FParams;
    property QueryLocked: Boolean read FbQueryLocked write FbQueryLocked;
    property TestInclude: Boolean read FbTestInclude write FbTestInclude;
    property Caption;
    function Clone: TSQLMgrQuery; reintroduce;
    procedure Assign(pData: TSQLMgrQuery); reintroduce;
  end;


  TSQLMgrParams = class(TtiObjectList)
  private
  protected
    function GetItems(i: integer): TSQLMgrParam; reintroduce;
    procedure SetItems(i: integer; const Value: TSQLMgrParam); reintroduce;
    function GetOwner: TSQLMgrQuery; reintroduce;
    procedure SetOwner(const Value: TSQLMgrQuery); reintroduce;
    function GetOID: TtiOID; override;
  public
    function AsSetupParamsDelphiCode: string;
    property Items[i: integer]: TSQLMgrParam read GetItems write SetItems;
    procedure Add(pObject: TSQLMgrParam); reintroduce;
    property Owner: TSQLMgrQuery read GetOwner write SetOwner;
  end;


  TSQLMgrParam = class(TtiObject)
  private
    FStrParamName: TSQLMgrParamName;
    FParamType: TtiQueryFieldKind;
    FStrParamValue: string;
    FbIsNull: Boolean;
    FDispOrder: integer;
  protected
    function GetCaption: string; override;
    function GetParamTypeStr: string;
    procedure SetParamTypeStr(const Value: string);
  public
    function ParamTypeAsTIQueryParamType: string;
  published
    property Caption;
    property DispOrder: integer read FDispOrder write FDispOrder;
    property ParamName: TSQLMgrParamName read FStrParamName write FStrParamName;
    property ParamType: TtiQueryFieldKind read FParamType write FParamType;
    property ParamTypeStr: string read GetParamTypeStr write SetParamTypeStr;
    property ParamValue: string read FStrParamValue write FStrParamValue;
    property IsNull: Boolean read FbIsNull write FbIsNull;
    function Clone: TSQLMgrParam; reintroduce;
  end;


function gSQLMgrs: TSQLMgrs;
function SQLParamAsProp(const pParamName: string; pWidth: integer = 0): string;
procedure RegisterMappings;


const
  cTableNameSQLManGroup     = 'sqlman_group';
  cFieldNameGroupOID        = 'oid';
  cFieldNameGroupDispOrder  = 'disp_order';
  cFieldNameGroupName       = 'group_name';

  cTableNameSQLManSQL       = 'sqlman_sql';
  cFieldNameSQLOID          = 'oid';
  cFieldNameSQLOIDGroup     = 'group_oid';
  cFieldNameSQLDispOrder    = 'disp_order';
  cFieldNameSQLVersion      = 'query_version';
  cFieldNameSQLName         = 'query_name';
  cFieldNameSQLDesc         = 'query_description';
  cFieldNameSQLLocked       = 'query_locked';
  cFieldNameSQLTestInclude  = 'test_include';
  // Use this one for SQL Manager tables created before 12/11/2001
  //cFieldNameSQLSQL : string = 'SQL' ;
  // Use this one for SQL Manager tables created after 12/11/2001
  cFieldNameSQLSQL: string  = 'query_sql';
  // You can uncomment either line above depending on the structure of your
  // SQLManager tables, or add the following line to the application's DPR file.
  //tiSQLMgr_Svr.cFieldNameSQLSQL := 'SQL' ;

  cTableNameSQLManParam     = 'sqlman_param';
  cFieldNameParamOID        = 'oid';
  cFieldNameParamOIDSQL     = 'sql_oid';
  cFieldNameParamDispOrder  = 'disp_order';
  cFieldNameParamName       = 'param_name';
  cFieldNameParamType       = 'param_type';
  cFieldNameParamValue      = 'param_value';
  cFieldNameParamIsNull     = 'param_isnull';

  cVisSQLMgrReadPK          = 'VisSQLMgrReadPK';
  cVisSQLMgrReadByQueryName = 'VisSQLMgrReadByQueryName';

implementation

uses
//  tiSQLMgr_Svr,  // To force visitor registration
  tiUtils,
  tiDialogs,
  tiOPFManager,
  tiLog,
  tiExcept,
  tiConstants,
//  tiClassToDBMap_BOM,
  tiAutoMap,
  SysUtils,
  Math;

var
  uSQLMgrs: TSQLMgrs;
  uRegisterMappingsCalled: Boolean;

const
  cSemaphoreSQLMgrs = 'SQLMgrs';
  cSemaphoreSQLMgr  = 'SQLMgr ';

function gSQLMgrs: TSQLMgrs;
begin
  if uSQLMgrs = nil then
    uSQLMgrs := TSQLMgrs.Create;
  Result := uSQLMgrs;
end;

 // Tempting to put this in the Initialization section, but while there is the
 // option of overriding cFieldNameSQLSQL, the place that RegisterMappings is
 // called must be under control of the programmer.
procedure RegisterMappings;
begin
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrGroup, cTableNameSQLManGroup, 'OID', cFieldNameGroupOID, [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrGroup, cTableNameSQLManGroup, 'GroupName', cFieldNameGroupName, [pktReadable]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrGroup, cTableNameSQLManGroup, 'DispOrder', cFieldNameGroupDispOrder, [pktReadable]);
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TSQLMgr, TSQLMgrGroup);

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'OID', cFieldNameSQLOID, [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'Owner.OID', cFieldNameSQLOIDGroup, [pktFK]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'DispOrder', cFieldNameSQLDispOrder);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'QueryVersion', cFieldNameSQLVersion);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'QueryName', cFieldNameSQLName);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'QueryDesc', cFieldNameSQLDesc);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'QueryLocked', cFieldNameSQLLocked);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'TestInclude', cFieldNameSQLTestInclude);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery, cTableNameSQLManSQL, 'SQL', cFieldNameSQLSQL);
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TSQLMgrGroup, TSQLMgrQuery);

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam, cTableNameSQLManParam, 'OID', cFieldNameParamOID, [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam, cTableNameSQLManParam, 'Owner.OID', cFieldNameParamOIDSQL, [pktFK]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam, cTableNameSQLManParam, 'DispOrder', cFieldNameParamDispOrder);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam, cTableNameSQLManParam, 'ParamName', cFieldNameParamName);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam, cTableNameSQLManParam, 'ParamTypeStr', cFieldNameParamType);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam, cTableNameSQLManParam, 'ParamValue', cFieldNameParamValue);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam, cTableNameSQLManParam, 'IsNull', cFieldNameParamIsNull);
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TSQLMgrParams, TSQLMgrParam);

  uRegisterMappingsCalled := True;
end;

function SQLParamAsProp(const pParamName: string; pWidth: integer = 0): string;
begin
  Result := tiStrTran(pParamName, '_', ' ');
  Result := tiMixedCase(Result);
  Result := tiStrTran(Result, ' ', '');
  if pWidth <> 0 then
    Result := tiPadR(Result, pWidth);
end;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TSQLMgr
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TSQLMgr.GetCaption: string;
begin
  Result := 'SQL Manager on ' + DatabaseName;
end;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TSQLMgrGroup
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TSQLMgrGroup.Add(const AObject: TtiObject): integer;
begin
  Result := inherited Add(AObject);
  if Owner <> nil then
    Owner.Queries.Add(AObject);
end;

constructor TSQLMgrGroup.Create;
begin
  inherited;
  OwnsObjects      := False;
  AutoSetItemOwner := True;
end;

destructor TSQLMgrGroup.Destroy;
begin
  inherited;
end;

function TSQLMgrGroup.getCaption: string;
begin
  Result := GroupName;
end;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TSQLMgrQuery
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TSQLMgrQuery.Assign(pData: TSQLMgrQuery);
var
  i: integer;
begin
  // We can't use the parent classes Assign method because params in an owned
  // list, and the pointer to the list will be overwritten during the assign.
  // Assign must know how to copy owned objects!
  QueryName   := pData.QueryName;
  QueryDesc   := pData.QueryDesc;
  SQL         := pData.SQL;
  QueryLocked := pData.QueryLocked;
  TestInclude := pData.TestInclude;
  Params.Clear;
  for i := 0 to pData.Params.Count - 1 do
    Params.Add(TSQLMgrParam(pData.Params.Items[i]).Clone);
end;

function TSQLMgrQuery.Clone: TSQLMgrQuery;
begin
  Result := TSQLMgrQuery.Create;
  Result.Assign(Self);
end;

constructor TSQLMgrQuery.Create;
begin
  inherited Create;
  FParams       := TSQLMgrParams.Create;
  FParams.Owner := self;
  FbQueryLocked := False;
  FbTestInclude := False;
end;

destructor TSQLMgrQuery.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

function TSQLMgrQuery.GetCaption: string;
begin
  Result := QueryName;
end;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TSQLMgrParam
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TSQLMgrParam.Clone: TSQLMgrParam;
begin
  Result := TSQLMgrParam(inherited Clone);
end;

function TSQLMgrParam.GetCaption: string;
begin
  Result := ParamName + ', ' +
    cgaQueryFieldKind[ParamType] + ', ' +
    ParamValue;
end;

function TSQLMgrParam.GetParamTypeStr: string;
begin
  Result := cgaQueryFieldKindSQLMgr[FParamType];
end;

procedure TSQLMgr.FindQueriesByName(const psPattern: string; pList: TList);
var
  lVis: TVisFindQueriesByName;
begin

  Assert(pList <> nil, 'List not assigned');
  Assert(psPattern <> EmptyStr, 'Pattern not assigned');

  pList.Clear;

  lVis := TVisFindQueriesByName.Create;
  try
    lVis.List := pList;
    lVis.Pattern := psPattern;
    Iterate(lVis);
  finally
    lVis.Free;
  end;
end;

function TSQLMgr.FindQueryByName(const pQueryName: string): TSQLMgrQuery;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Queries.Count - 1 do
    if SameText(Queries.Items[i].QueryName, pQueryName) and
      (not Queries.Items[i].Deleted) then
    begin
      Result := Queries.Items[i];
      Exit; //==>
    end;
end;

procedure TSQLMgr.ValidateModel;
var
  i, j, k: integer;
  lSQLMgrParam: TSQLMgrParam;
  lSQLMgrQuery: TSQLMgrQuery;
  lGroup: TSQLMgrGroup;
  lStringList: TStringList;
begin
  lStringList := TStringList.Create;
  try
    for i := 0 to Count - 1 do
    begin
      lGroup := TSQLMgrGroup(Items[i]);
      lStringList.Add('Group: ' + lGroup.GroupName);
      for j := 0 to lGroup.Count - 1 do
        try
          lSQLMgrQuery := TSQLMgrQuery(lGroup.Items[j]);
          if lSQLMgrQuery.ObjectState <> posDelete then
          begin
            lStringList.Add('  Query: ' +
              lSQLMgrQuery.QueryName +
              ', ' + tiPadR(lSQLMgrQuery.SQL, 20));
            for k := 0 to lSQLMgrQuery.Params.Count - 1 do
              try
                lSQLMgrParam := TSQLMgrParam(lSQLMgrQuery.Params.Items[k]);
                if lSQLMgrParam.ObjectState <> posDelete then
                  lStringList.Add('    Param: ' + lSQLMgrParam.Caption);
              except
                on e: Exception do
                  lStringList.Add('    Param: Error: ' + e.message);
              end;
          end;
        except
          on e: Exception do
            lStringList.Add('  Query: Error: ' + e.message);
        end;
    end;
    tiShowStringList(lStringList);
  finally
    lStringList.Free;
  end;
end;

function TSQLMgrQuery.GetOwner: TSQLMgrGroup;
begin
  Result := TSQLMgrGroup(inherited GetOwner);
end;

function TSQLMgrQuery.GetQueryGroupName: string;
begin
  if Owner = nil then
    Result := 'N/A'
  else
    Result := TSQLMgrGroup(Owner).GroupName;
end;

function TSQLMgrGroup.GetItems(i: integer): TSQLMgrQuery;
begin
  Result := TSQLMgrQuery(inherited GetItems(i));
end;

function TSQLMgrGroup.GetOwner: TSQLMgr;
begin
  Result := TSQLMgr(inherited GetOwner);
end;

 //* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 //*
 //* TVisFindQueriesByName
 //*
 //* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisFindQueriesByName.AcceptVisitor: Boolean;
begin
  Result := (Visited is TSQLMgrQuery) and
    (not TSQLMgrQuery(Visited).Deleted);
end;

procedure TVisFindQueriesByName.Execute(const pVisited: TtiVisited);
begin
  inherited Execute(pVisited);

  Assert(FList <> nil, 'List not assigned');
  Assert(FsPattern <> EmptyStr, 'Pattern not assigned');

  if not AcceptVisitor then
    Exit; //==>

  if (tiWildCardMatch(TSQLMgrQuery(pVisited).QueryName, FsPattern)) or
    (tiWildCardMatch(TSQLMgrGroup(TSQLMgrQuery(pVisited).Owner).GroupName, FsPattern)) then
    FList.Add(pVisited);

end;

function TSQLMgrParam.ParamTypeAsTIQueryParamType: string;
begin
  Result := GetParamTypeStr;
  if Result = cgaQueryFieldKind[qfkDateTime] then
    Result := 'DateTime'
  else if Result = cgaQueryFieldKind[qfkLogical] then
    Result := 'Boolean';
end;

procedure TSQLMgrParam.SetParamTypeStr(const Value: string);
var
  lIndex: TtiQueryFieldKind;
begin
  ParamType := Low(TtiQueryFieldKind);
  for lIndex := Low(TtiQueryFieldKind) to High(TtiQueryFieldKind) do
    if SameText(cgaQueryFieldKindSQLMgr[lIndex], Value) then
    begin
      ParamType := lIndex;
      Exit; //==>
    end;
  raise EtiOPFInternalException('Invalid field kind <' + Value + '>');
end;

{ TSQLMgrs }

procedure TSQLMgrs.Add(pObject: TSQLMgr);
begin
  inherited Add(pObject);
end;

function TSQLMgrs.AddDatabase(const pDatabaseName: string): TSQLMgr;
begin
  Result := TSQLMgr.Create;
  Result.DatabaseName := pDatabaseName;
  Add(Result);
end;

procedure TSQLMgrs.Clear;
begin
  inherited;
  ObjectState := posEmpty;
end;

constructor TSQLMgrs.Create;
begin
  inherited;
  FCritSect := TCriticalSection.Create;
end;

destructor TSQLMgrs.Destroy;
begin
  FCritSect.Free;
  inherited;
end;

function TSQLMgrs.FindByDatabaseName(const pDatabaseName: string): TSQLMgr;
var
  i: integer;
begin
  Result := nil;
  FCritSect.Enter;
  try
    for i := 0 to Count - 1 do
      if SameText(Items[i].DatabaseName, pDatabaseName) then
      begin
        Result := Items[i];
        Break; //==>
      end;
    if Result = nil then
      Result := AddDatabase(pDatabaseName);
  finally
    FCritSect.Leave;
  end;
end;

function TSQLMgrs.GetCaption: string;
begin
  Result := 'SQLManager Databases';
end;

function TSQLMgrs.GetItems(i: integer): TSQLMgr;
begin
  Result := TSQLMgr(inherited GetItems(i));
end;

procedure TSQLMgrs.SetItems(i: integer; const Value: TSQLMgr);
begin
  inherited SetItems(i, Value);
end;

function TSQLMgrQuery.IsParamNameUnique(const pParam: TSQLMgrParam): Boolean;
var
  i: integer;
begin
  Result := True;
  {$IFNDEF OID_AS_INT64}
  for i := 0 to Params.Count - 1 do
    if SameText(Params.Items[i].ParamName, pParam.ParamName) and
      (not Params.Items[i].OID.Equals(pParam.OID)) and
      (not Params.Items[i].Deleted) then
    begin
      Result := False;
      Exit; //==>
    end;
  {$ELSE}
  for i := 0 to Params.Count - 1 do
    if SameText(Params.Items[i].ParamName, pParam.ParamName) and
      (not Params.Items[i].OID = pParam.OID) and
      (not Params.Items[i].Deleted) then
    begin
      Result := False;
      Exit; //==>
    end;
  {$ENDIF}
end;

procedure TSQLMgrQuery.ReadByQueryName(const pQueryName: string;
  const pSQLMgrFileName: string);
begin
  Assert(ObjectState = posEmpty, 'ObjectState <> posEmpty');
  Assert(pSQLMgrFileName <> '', 'pSQLMgrFileName not assigned');
  QueryName := pQueryName;
  gTIOPFManager.VisitorManager.Execute(cVisSQLMgrReadByQueryName, Self, pSQLMgrFileName, cTIPersistXMLLight);
end;

procedure TSQLMgrQuery.SetObjectState(const Value: TPerObjectState);
begin
  if (ObjectState = posPK) and
    (Value = posDelete) then
    raise EtiOPFProgrammerException.Create(cExcCanNotSetObjectStateDeleteWhenPK);
  if (ObjectState = posDelete) and
    (Value = posDeleted) then
    Params.ObjectState := posDeleted;
  inherited;
end;

procedure TSQLMgrQuery.SetOwner(const Value: TSQLMgrGroup);
begin
  inherited SetOwner(Value);
end;

procedure TSQLMgrGroup.SetItems(i: integer; const Value: TSQLMgrQuery);
begin
  inherited SetItems(i, Value);
end;

procedure TSQLMgrGroup.SetOwner(const Value: TSQLMgr);
begin
  inherited SetOwner(Value);
end;

{ TSQLMgrParams }

procedure TSQLMgrParams.Add(pObject: TSQLMgrParam);
begin
  inherited Add(pObject);
end;

function TSQLMgrParams.AsSetupParamsDelphiCode: string;
var
  i: integer;
  lAreParams: Boolean;
  lWidth: integer;
begin
  Result :=
    'var' + Cr +
    '  lData : TMyClassType ;' + Cr +
    'begin' + Cr +
    '  lData := ( Visited as TMyClassType ) ;' + Cr;

  lWidth := 0;
  for i := 0 to Count - 1 do
    if not Items[i].Deleted then
      lWidth := Max(lWidth, Length(TSQLMgrParam(Items[i]).ParamName));

  lAreParams := False;
  for i := 0 to Count - 1 do
  begin
    if lAreParams then
      Result := Result + Cr;
    if not Items[i].Deleted then
    begin
      lAreParams := True;
      Result     :=
        Result +
        '  ' +
        tiPadR('Query.ParamAs' + TSQLMgrParam(Items[i]).ParamTypeAsTIQueryParamType + '[', 22) +
        ' ''' +
        tiPadR(TSQLMgrParam(Items[i]).ParamName + '''', lWidth + 2) +
        '] := lData.' +
        SQLParamAsProp(TSQLMgrParam(Items[i]).ParamName, lWidth) + ' ;';
    end;
  end;

end;

procedure TSQLMgr.CreateTables;
  //----------------------------
  procedure _CreateTableSQLMan_Group;
  var
    lTable: TtiDBMetaDataTable;
  begin
    lTable := TtiDBMetaDataTable.Create;
    try
      lTable.Name := cTableNameSQLManGroup;
      lTable.AddInstance(cFieldNameGroupOID, qfkString, 36); // Should be Not Null & PK
      lTable.AddInstance(cFieldNameGroupName, qfkString, 50);
      lTable.AddInstance(cFieldNameGroupDispOrder, qfkInteger);
      gTIOPFManager.CreateTable(lTable, FFileName, cTIPersistXMLLight);
    finally
      lTable.Free;
    end;
  end;

  procedure _CreateTableSQLMan_SQL;
  var
    lTable: TtiDBMetaDataTable;
  begin
    lTable := TtiDBMetaDataTable.Create;
    try
      lTable.Name := cTableNameSQLManSQL;
      lTable.AddInstance(cFieldNameSQLOID, qfkString, 36); // Should be Not Null & PK
      lTable.AddInstance(cFieldNameSQLOIDGroup, qfkString, 36);
      lTable.AddInstance(cFieldNameSQLDispOrder, qfkInteger);
      lTable.AddInstance(cFieldNameSQLVersion, qfkInteger);
      lTable.AddInstance(cFieldNameSQLName, qfkString, 50);
      lTable.AddInstance(cFieldNameSQLDesc, qfkLongString);
      lTable.AddInstance(cFieldNameSQLLocked, qfkLogical);
      lTable.AddInstance(cFieldNameSQLTestInclude, qfkLogical);
      lTable.AddInstance(cFieldNameSQLSQL, qfkLongString);
      gTIOPFManager.CreateTable(lTable, FFileName, cTIPersistXMLLight);
    finally
      lTable.Free;
    end;
  end;

  procedure _CreateTableSQLMan_Param;
  var
    lTable: TtiDBMetaDataTable;
  begin
    lTable := TtiDBMetaDataTable.Create;
    try
      lTable.Name := cTableNameSQLManParam;
      lTable.AddInstance(cFieldNameParamOID, qfkString, 36); // Should be Not Null & PK
      lTable.AddInstance(cFieldNameParamOIDSQL, qfkString, 36);
      lTable.AddInstance(cFieldNameParamDispOrder, qfkInteger);
      lTable.AddInstance(cFieldNameParamName, qfkString, 20);
      lTable.AddInstance(cFieldNameParamType, qfkString, 20);
      lTable.AddInstance(cFieldNameParamValue, qfkString, 50);
      lTable.AddInstance(cFieldNameParamIsNull, qfkLogical);
      gTIOPFManager.CreateTable(lTable, FFileName, cTIPersistXMLLight);
    finally
      lTable.Free;
    end;
  end;

begin

  Assert(uRegisterMappingsCalled, 'tiSQLMgr_BOM.RegisterMappings has not been called.');
  Assert(FileName <> '', ClassName + '.FileName not assigned.');
  _CreateTableSQLMan_Group;
  _CreateTableSQLMan_SQL;
  _CreateTableSQLMan_Param;

end;

procedure TSQLMgr.DropTables;
begin
  Assert(uRegisterMappingsCalled, 'tiSQLMgr_BOM.RegisterMappings has not been called.');
  Assert(FileName <> '', ClassName + '.FileName not assigned');
  gTIOPFManager.DropTable(cTableNameSQLManGroup, FFileName, cTIPersistXMLLight);
  gTIOPFManager.DropTable(cTableNameSQLManSQL, FFileName, cTIPersistXMLLight);
  gTIOPFManager.DropTable(cTableNameSQLManParam, FFileName, cTIPersistXMLLight);
end;

procedure TSQLMgr.Read(const pDBConnectionName: string; pPerLayerName: string);
begin
  Assert(False, 'Don''t call ' + ClassName + '.Read Call ' +
    ClassName + '.ReadPK');
end;

procedure TSQLMgr.Add(pObject: TSQLMgrGroup);
begin
  inherited Add(pObject);
end;

function TSQLMgr.GetItems(i: integer): TSQLMgrGroup;
begin
  Result := TSQLMgrGroup(inherited GetItems(i));
end;

procedure TSQLMgr.SetItems(i: integer; const Value: TSQLMgrGroup);
begin
  inherited SetItems(i, Value);
end;

function TSQLMgrParams.GetItems(i: integer): TSQLMgrParam;
begin
  Result := TSQLMgrParam(inherited GetItems(i));
end;

function TSQLMgrParams.GetOID: TtiOID;
begin
  if Owner <> nil then
    Result := Owner.OID
  else
    Result := inherited GetOID;
end;

function TSQLMgrParams.GetOwner: TSQLMgrQuery;
begin
  Result := TSQLMgrQuery(inherited GetOwner);
end;

procedure TSQLMgrParams.SetItems(i: integer; const Value: TSQLMgrParam);
begin
  inherited SetItems(i, Value);
end;

procedure TSQLMgrParams.SetOwner(const Value: TSQLMgrQuery);
begin
  inherited SetOwner(Value);
end;

constructor TSQLMgr.Create;
begin
  inherited;
  FCritSect := TCriticalSection.Create;
  FQueries  := TSQLMgrGroup.Create;
  FQueries.OwnsObjects := True;
  FQueries.AutoSetItemOwner := False;
end;

destructor TSQLMgr.Destroy;
begin
  FCritSect.Free;
  FQueries.Free;
  inherited;
end;

function TSQLMgr.FindCreateQueryByName(const pQueryName: string): TSQLMgrQuery;
begin
  FCritSect.Enter;
  try
    Result := FindQueryByName(pQueryName);
    if Result <> nil then
      Exit; //==>
    Result := TSQLMgrQuery.Create;
    Result.ReadByQueryName(pQueryName, FFileName);
    if Result.ObjectState <> posClean then
    begin
      Result.Free;
      Result := nil;
    end
    else
      Queries.Add(Result);
  finally
    FCritSect.Leave;
  end;
end;

function TSQLMgr.IsGroupNameUnique(const pGroup: TSQLMgrGroup): Boolean;
var
  i: integer;
begin
  Result := True;
  {$IFNDEF OID_AS_INT64}
  for i := 0 to Count - 1 do
    if SameText(Items[i].GroupName, pGroup.GroupName) and
      (not Items[i].OID.Equals(pGroup.OID)) and
      (not Items[i].Deleted) then
    begin
      Result := False;
      Exit; //==>
    end;
  {$ELSE}
  for i := 0 to Count - 1 do
    if SameText(Items[i].GroupName, pGroup.GroupName) and
      (not Items[i].OID = pGroup.OID) and
      (not Items[i].Deleted) then
    begin
      Result := False;
      Exit; //==>
    end;
  {$ENDIF}
end;

function TSQLMgr.IsQueryNameUnique(const pQuery: TSQLMgrQuery): Boolean;
var
  i: integer;
begin
  Result := True;
  {$IFNDEF OID_AS_INT64}
  for i := 0 to Queries.Count - 1 do
    if SameText(Queries.Items[i].QueryName, pQuery.QueryName) and
      (not Queries.Items[i].OID.Equals(pQuery.OID)) and
      (not Queries.Items[i].Deleted) then
    begin
      Result := False;
      Exit; //==>
    end;
  {$ELSE}
  for i := 0 to Queries.Count - 1 do
    if SameText(Queries.Items[i].QueryName, pQuery.QueryName) and
      (not Queries.Items[i].OID = pQuery.OID) and
      (not Queries.Items[i].Deleted) then
    begin
      Result := False;
      Exit; //==>
    end;
  {$ENDIF}
end;

procedure TSQLMgr.ReadPK(const pDBConnectionName: string; pPerLayerName: string);
var
  i: integer;
begin
  Assert(uRegisterMappingsCalled, 'tiSQLMgr_BOM.RegisterMappings has not been called.');
  Assert(pDBConnectionName = '', ClassName + '.ReadPK() does not accept parameters.');
  Assert(pPerLayerName = '', ClassName + '.ReadPK() does not accept parameters.');
  Assert(FFileName <> '', ClassName + '.FileName not assigned');
  gTIOPFManager.VisitorManager.Execute(cVisSQLMgrReadPK, Self, FFileName, cTIPersistXMLLight);
  SortByProps(['DispOrder']);
  for i := 0 to Count - 1 do
    Items[i].SortByProps(['DispOrder']);
end;

procedure TSQLMgr.Save(const pDBConnectionName: string; pPerLayerName: string);
begin
  Assert(uRegisterMappingsCalled, 'tiSQLMgr_BOM.RegisterMappings has not been called.');
  Assert(pDBConnectionName = '', ClassName + '.Save() does not accept parameters.');
  Assert(pPerLayerName = '', ClassName + '.Save() does not accept parameters.');
  Assert(FFileName <> '', ClassName + '.FileName not assigned');
  inherited Save(FFileName, ctiPersistXMLLight);
end;

class procedure TSQLMgr.CreateFile(const pDBConnectionName: string);
begin
  gTIOPFManager.CreateDatabase(pDBConnectionName, 'null', 'null', cTIPersistXMLLight);
end;

initialization
  uRegisterMappingsCalled := False;

finalization
  uSQLMgrs.Free;

end.

