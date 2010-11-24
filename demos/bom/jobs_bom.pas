

unit jobs_bom;


{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}


interface


uses
  SysUtils
  ,tiObject
  ,typinfo
  ,tiAutoMap
  ,tiOPFManager
  ,tiVisitorDB
  ,tiVisitorCriteria
  ,tiCriteria
  ,tiSQLParser
  ,mapper
  ;

type


// Enumerations
  TJobStatus = (jsCreated
    ,jsStarted
    ,jsFinished);



  // ---------------------------------------------
  // Generated Classes
  // ---------------------------------------------


  { Generated Class: TJob}
  TJob = class(TtiObject)
  protected
    FJobDesc: String;
    FJobName: String;
    FStatus: TJobStatus;
    procedure SetJobDesc(const AValue: String); virtual;
    procedure SetJobName(const AValue: String); virtual;
    procedure SetStatus(const AValue: TJobStatus); virtual;
  public
    procedure   Read; override;
    procedure   Save; override;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; overload; override;
  published
    property    JobDesc: String read FJobDesc write SetJobDesc;
    property    JobName: String read FJobName write SetJobName;
    property    Status: TJobStatus read FStatus write SetStatus;
  end;
  
  { List of TJob.  TtiMappedFilteredObjectList descendant. }
  TJobList = class(TtiMappedFilteredObjectList)
  protected
    procedure   SetItems(i: integer; const AValue: TJob); reintroduce;
    function    GetItems(i: integer): TJob; reintroduce;
  public
    property    Items[i:integer] : TJob read GetItems write SetItems;
    procedure   Add(AObject: TJob); reintroduce;
    procedure   Read; override;
    procedure   Save; override;
    { Return count (1) if successful. }
    function    FindByOID(const AOID: string): integer;
    { Returns Number of objects retrieved. }
    function    FindByUser(const AUser: String): integer;
    { Returns Number of objects retrieved. }
    function    FindByStatus(const AStatus: enum): integer;
  end;
  
  { Generated Class: TUserJobRelation}
  TUserJobRelation = class(TtiObject)
  protected
    FJobOID: String;
    FUserOID: String;
    procedure SetJobOID(const AValue: String); virtual;
    procedure SetUserOID(const AValue: String); virtual;
  public
    procedure   Read; override;
    procedure   Save; override;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; overload; override;
  published
    property    JobOID: String read FJobOID write SetJobOID;
    property    UserOID: String read FUserOID write SetUserOID;
  end;
  
  { List of TUserJobRelation.  TtiMappedFilteredObjectList descendant. }
  TUserJobRelationList = class(TtiMappedFilteredObjectList)
  protected
    procedure   SetItems(i: integer; const AValue: TUserJobRelation); reintroduce;
    function    GetItems(i: integer): TUserJobRelation; reintroduce;
  public
    property    Items[i:integer] : TUserJobRelation read GetItems write SetItems;
    procedure   Add(AObject: TUserJobRelation); reintroduce;
    procedure   Read; override;
    procedure   Save; override;
    { Return count (1) if successful. }
    function    FindByOID(const AOID: string): integer;
    { Returns Number of objects retrieved. }
    function    FindByUser(const AUserOID: String): integer;
  end;
  
  { Read Visitor for TJob }
  TJob_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
    procedure   MapRowToObject; override;
  end;
  
  { Create Visitor for TJob }
  TJob_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { Update Visitor for TJob }
  TJob_Save = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { Delete Visitor for TJob }
  TJob_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Read Visitor for TJobList }
  TJobList_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   MapRowToObject; override;
  end;
  
  { List Create Visitor for TJobList }
  TJobList_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Update Visitor for TJobList }
  TJobList_Save = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Delete Visitor for TJobList }
  TJobList_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { TJobList_FindByUserVis }
  TJobList_FindByUserVis = class(TtiMapParameterListReadVisitor)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   MapRowToObject; override;
    procedure   SetupParams; override;
  end;
  
  { TJobList_FindByStatusVis }
  TJobList_FindByStatusVis = class(TtiMapParameterListReadVisitor)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   MapRowToObject; override;
    procedure   SetupParams; override;
  end;
  
  { Read Visitor for TUserJobRelation }
  TUserJobRelation_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
    procedure   MapRowToObject; override;
  end;
  
  { Create Visitor for TUserJobRelation }
  TUserJobRelation_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { Update Visitor for TUserJobRelation }
  TUserJobRelation_Save = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { Delete Visitor for TUserJobRelation }
  TUserJobRelation_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Read Visitor for TUserJobRelationList }
  TUserJobRelationList_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   MapRowToObject; override;
  end;
  
  { List Create Visitor for TUserJobRelationList }
  TUserJobRelationList_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Update Visitor for TUserJobRelationList }
  TUserJobRelationList_Save = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Delete Visitor for TUserJobRelationList }
  TUserJobRelationList_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { TUserJobRelationList_FindByUserVis }
  TUserJobRelationList_FindByUserVis = class(TtiMapParameterListReadVisitor)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   MapRowToObject; override;
    procedure   SetupParams; override;
  end;
  

  { Visitor Manager Registrations }
  procedure RegisterVisitors;

  { Register Auto Mappings }
  procedure RegisterMappings;


implementation


procedure RegisterMappings;
begin
  { Automap registrations for TJob }
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TJob, 
    'jobs', 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TJob,
    'jobs','JobName', 'job_name');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TJob,
    'jobs','JobDesc', 'job_desc');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TJob,
    'jobs','Status', 'job_status');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TJobList, TJob);
  
  { Automap registrations for TUserJobRelation }
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TUserJobRelation, 
    'user_job_relation', 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TUserJobRelation,
    'user_job_relation','JobOID', 'job_oid');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TUserJobRelation,
    'user_job_relation','UserOID', 'user_oid');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TUserJobRelationList, TUserJobRelation);
  
end;

procedure RegisterVisitors;
begin
  { Register Visitors for TJob }
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobList_listread', TJobList_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobList_listsave', TJobList_Create);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobList_listsave', TJobList_Save);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobList_listsave', TJobList_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobread', TJob_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobsave', TJob_Save);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobdelete', TJob_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobcreate', TJob_Create);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobList_FindByUserVis', TJobList_FindByUserVis);
  GTIOPFManager.VisitorManager.RegisterVisitor('TJobList_FindByStatusVis', TJobList_FindByStatusVis);
  
  { Register Visitors for TUserJobRelation }
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationList_listread', TUserJobRelationList_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationList_listsave', TUserJobRelationList_Create);
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationList_listsave', TUserJobRelationList_Save);
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationList_listsave', TUserJobRelationList_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationread', TUserJobRelation_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationsave', TUserJobRelation_Save);
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationdelete', TUserJobRelation_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationcreate', TUserJobRelation_Create);
  GTIOPFManager.VisitorManager.RegisterVisitor('TUserJobRelationList_FindByUserVis', TUserJobRelationList_FindByUserVis);
  
end;

procedure TJob.SetJobDesc(const AValue: String);
begin
  if FJobDesc <> AValue then
    FJobDesc := AValue;
end;

procedure TJob.SetJobName(const AValue: String);
begin
  if FJobName <> AValue then
    FJobName := AValue;
end;

procedure TJob.SetStatus(const AValue: TJobStatus);
begin
  if FStatus <> AValue then
    FStatus := AValue;
end;

procedure TJob.Read;
begin
  GTIOPFManager.VisitorManager.Execute(ClassName + 'read', self);
end;

procedure TJob.Save;
begin
  Case ObjectState of
    posDelete: GTIOPFManager.VisitorManager.Execute('TJobdelete', self);
    posUpdate: GTIOPFManager.VisitorManager.Execute('TJobsave', self);
    posCreate: GTIOPFManager.VisitorManager.Execute('TJobcreate', self);
  end;
end;

function TJob.IsValid(const AErrors: TtiObjectErrors): boolean;
var
  lMsg: string;
begin
  Result := inherited IsValid(AErrors);
  if not result then exit;
  
  if JobName = '' then 
    begin
      lMsg := ValidatorStringClass.CreateRequiredValidatorMsg(self, 'JobName');
      AErrors.AddError(lMsg);
    end;
  
  result := AErrors.Count = 0;
end;

 {TJobList }

procedure TJobList.Add(AObject: TJob);
begin
  inherited Add(AObject);
end;

function TJobList.GetItems(i: integer): TJob;
begin
  result := inherited GetItems(i) as TJob;
end;

procedure TJobList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('TJobList_listread', self);
end;

procedure TJobList.Save;
begin
  GTIOPFManager.VisitorManager.Execute('TJobList_listsave', self);
end;

procedure TJobList.SetItems(i: integer; const AValue: TJob);
begin
  inherited SetItems(i, AValue);
end;
function TJobList.FindByOID(const AOID: string): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Criteria.ClearAll;
  Criteria.AddEqualTo('OID', AOID);
  Read;
  result := Count;
end;

function TJobList.FindByUser(const AUser: String): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Params.Clear;
  AddParam('AUser', 'user_oid', ptString, AUser);
  self.SQL := 
    '  ' + 
    ' SELECT  ' + 
    '   JOBS.OID , JOBS.JOB_NAME, JOBS.JOB_DESC, JOBS.JOB_STATUS ' + 
    ' FROM  ' + 
    '     JOBS INNER JOIN USER_JOB_RELATION ON JOBS.OID =  ' + 
    '     USER_JOB_RELATION.JOB_OID ' + 
    ' WHERE  ' + 
    '     USER_JOB_RELATION.USER_OID = :user_oid ' + 
    ' ORDER BY  ' + 
    '     JOBS.JOB_NAME ' + 
    '                                 '; 
  GTIOPFManager.VisitorManager.Execute('TJobList_FindByUserVis', self);
  result := self.Count;
end;

function TJobList.FindByStatus(const AStatus: enum): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Params.Clear;
  AddParam('AStatus', 'status', ptEnum, AStatus);
  self.SQL := 
    '  ' + 
    ' SELECT  ' + 
    '  JOBS.OID , JOBS.JOB_NAME, JOBS.JOB_DESC, JOBS.JOB_STATUS ' + 
    ' FROM  ' + 
    '  JOBS  ' + 
    ' WHERE  ' + 
    '  JOBS.JOB_STATUS = :STATUS ' + 
    ' ORDER BY  ' + 
    '  JOBS.JOB_NAME ' + 
    '                                 '; 
  GTIOPFManager.VisitorManager.Execute('TJobList_FindByStatusVis', self);
  result := self.Count;
end;

procedure TUserJobRelation.SetJobOID(const AValue: String);
begin
  if FJobOID <> AValue then
    FJobOID := AValue;
end;

procedure TUserJobRelation.SetUserOID(const AValue: String);
begin
  if FUserOID <> AValue then
    FUserOID := AValue;
end;

procedure TUserJobRelation.Read;
begin
  GTIOPFManager.VisitorManager.Execute(ClassName + 'read', self);
end;

procedure TUserJobRelation.Save;
begin
  Case ObjectState of
    posDelete: GTIOPFManager.VisitorManager.Execute('TUserJobRelationdelete', self);
    posUpdate: GTIOPFManager.VisitorManager.Execute('TUserJobRelationsave', self);
    posCreate: GTIOPFManager.VisitorManager.Execute('TUserJobRelationcreate', self);
  end;
end;

function TUserJobRelation.IsValid(const AErrors: TtiObjectErrors): boolean;
var
  lMsg: string;
begin
  Result := inherited IsValid(AErrors);
  if not result then exit;
  
  if JobOID = '' then 
    begin
      lMsg := ValidatorStringClass.CreateRequiredValidatorMsg(self, 'JobOID');
      AErrors.AddError(lMsg);
    end;
  
  if UserOID = '' then 
    begin
      lMsg := ValidatorStringClass.CreateRequiredValidatorMsg(self, 'UserOID');
      AErrors.AddError(lMsg);
    end;
  
  result := AErrors.Count = 0;
end;

 {TUserJobRelationList }

procedure TUserJobRelationList.Add(AObject: TUserJobRelation);
begin
  inherited Add(AObject);
end;

function TUserJobRelationList.GetItems(i: integer): TUserJobRelation;
begin
  result := inherited GetItems(i) as TUserJobRelation;
end;

procedure TUserJobRelationList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('TUserJobRelationList_listread', self);
end;

procedure TUserJobRelationList.Save;
begin
  GTIOPFManager.VisitorManager.Execute('TUserJobRelationList_listsave', self);
end;

procedure TUserJobRelationList.SetItems(i: integer; const AValue: TUserJobRelation);
begin
  inherited SetItems(i, AValue);
end;
function TUserJobRelationList.FindByOID(const AOID: string): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Criteria.ClearAll;
  Criteria.AddEqualTo('OID', AOID);
  Read;
  result := Count;
end;

function TUserJobRelationList.FindByUser(const AUserOID: String): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Params.Clear;
  AddParam('AUserOID', 'user_oid', ptString, AUserOID);
  self.SQL := 
    '  ' + 
    ' SELECT  ' + 
    '   USER_JOB_RELATION.OID , USER_JOB_RELATION.JOB_OID, USER_JOB_RELATION.USER_OID ' + 
    ' FROM  ' + 
    '     USER_JOB_RELATION  ' + 
    ' WHERE  ' + 
    '     USER_JOB_RELATION.USER_OID = :USER_OID                                                                       ' + 
    '                                 '; 
  GTIOPFManager.VisitorManager.Execute('TUserJobRelationList_FindByUserVis', self);
  result := self.Count;
end;

{ TJob_Create }
function TJob_Create.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posCreate;
end;

procedure TJob_Create.Init;
begin
  Query.SQLText := 
    'INSERT INTO jobs(' + 
    ' OID, ' + 
    ' job_name, ' + 
    ' job_desc, ' + 
    ' job_status' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :job_name, ' + 
    ' :job_desc, ' + 
    ' :job_status' + 
    ') ';
end;

procedure TJob_Create.SetupParams;
var
  lObj: TJob;
begin
  lObj := TJob(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['job_name'] := lObj.JobName;
  Query.ParamAsString['job_desc'] := lObj.JobDesc;
  Query.ParamAsInteger['job_status'] := Integer(lObj.Status);
end;

{ TJob_Save }
function TJob_Save.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posUpdate;
end;

procedure TJob_Save.Init;
begin
  Query.SQLText := 
    'UPDATE jobs SET ' +
    ' job_name = :job_name, ' + 
    ' job_desc = :job_desc, ' + 
    ' job_status = :job_status ' + 
    'WHERE OID = :OID' ;
end;

procedure TJob_Save.SetupParams;
var
  lObj: TJob;
begin
  lObj := TJob(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['job_name'] := lObj.JobName;
  Query.ParamAsString['job_desc'] := lObj.JobDesc;
  Query.ParamAsInteger['job_status'] := Integer(lObj.Status);
end;

{ TJob_Read }
function TJob_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posPK) OR (Visited.ObjectState = posClean);
end;

procedure TJob_Read.Init;
begin
  Query.SQLText := 
    'SELECT ' + 
    ' OID, ' +
    ' job_name, ' + 
    ' job_desc, ' + 
    ' job_status ' + 
    'FROM  jobs WHERE OID = :OID' ;
end;

procedure TJob_Read.SetupParams;
var
  lObj: TJob;
begin
  lObj := TJob(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;

procedure TJob_Read.MapRowToObject;
var
  lObj: TJob;
begin
  lObj := TJob(Visited);
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.JobName := Query.FieldAsString['job_name'];
  lObj.JobDesc := Query.FieldAsString['job_desc'];
  lObj.Status := TJobStatus(Query.FieldAsInteger['job_status']);
end;

{ TJob_Delete }
function TJob_Delete.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posDelete;
end;

procedure TJob_Delete.Init;
begin
  Query.SQLText := 
    'DELETE FROM jobs ' +
    'WHERE OID = :OID';
end;

procedure TJob_Delete.SetupParams;
var
  lObj: TJob;
begin
  lObj := TJob(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;

{ TJobList_Read }
function TJobList_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;

procedure TJobList_Read.Init;
var
  lFiltered: ItiFiltered;
  lWhere: string;
  lOrder: string;
  lSQL: string;
begin
  if Supports(Visited, ItiFiltered, lFiltered) then
  begin
    if lFiltered.GetCriteria.HasCriteria then
      lWhere := ' WHERE ' + tiCriteriaAsSQL(lFiltered.GetCriteria)
    else
      lWhere := '';
    if lFiltered.GetCriteria.hasOrderBy then
      lOrder := tiCriteriaOrderByAsSQL(lFiltered.GetCriteria)
    else
      lOrder := '';
  end;
  
  lSQL := 
    'SELECT ' + 
    ' OID, ' +
    ' job_name, ' + 
    ' job_desc, ' + 
    ' job_status ' + 
    'FROM  jobs %s %s ;';
  
  Query.SQLText := gFormatSQL(Format(lSQL, [lWhere, lOrder]), TJob);
  
end;

procedure TJobList_Read.MapRowToObject;
var
  lObj: TJob;
begin
  lObj := TJob.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.JobName := Query.FieldAsString['job_name'];
  lObj.JobDesc := Query.FieldAsString['job_desc'];
  lObj.Status := TJobStatus(Query.FieldAsInteger['job_status']);
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

{ TJobList_Create }
function TJobList_Create.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posCreate;
end;

procedure TJobList_Create.Init;
begin
  Query.SQLText := 
    'INSERT INTO jobs(' + 
    ' OID, ' + 
    ' job_name, ' + 
    ' job_desc, ' + 
    ' job_status' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :job_name, ' + 
    ' :job_desc, ' + 
    ' :job_status' + 
    ') ';
end;

procedure TJobList_Create.SetupParams;
var
  lObj: TJob;
begin
  lObj := TJob(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['job_name'] := lObj.JobName;
  Query.ParamAsString['job_desc'] := lObj.JobDesc;
  Query.ParamAsInteger['job_status'] := Integer(lObj.Status);
end;

{ TJobList_Delete }
function TJobList_Delete.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posDelete;
end;

procedure TJobList_Delete.Init;
begin
  Query.SQLText := 
    'DELETE FROM jobs ' +
    'WHERE OID = :OID';
end;

procedure TJobList_Delete.SetupParams;
var
  lObj: TJob;
begin
  lObj := TJob(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;
{ TJobList_Save }
function TJobList_Save.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posUpdate;
end;

procedure TJobList_Save.Init;
begin
  Query.SQLText := 
    'UPDATE jobs SET ' +
    ' job_name = :job_name, ' + 
    ' job_desc = :job_desc, ' + 
    ' job_status = :job_status ' + 
    'WHERE OID = :OID' ;
end;

procedure TJobList_Save.SetupParams;
var
  lObj: TJob;
begin
  lObj := TJob(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['job_name'] := lObj.JobName;
  Query.ParamAsString['job_desc'] := lObj.JobDesc;
  Query.ParamAsInteger['job_status'] := Integer(lObj.Status);
end;

{ TJobList_FindByUserVis }
function TJobList_FindByUserVis.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;

procedure TJobList_FindByUserVis.MapRowToObject;
var
  lObj: TJob;
begin
  lObj := TJob.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.JobName := Query.FieldAsString['job_name'];
  lObj.JobDesc := Query.FieldAsString['job_desc'];
  lObj.Status := TJobStatus(Query.FieldAsInteger['job_status']);
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

procedure TJobList_FindByUserVis.SetupParams;
var
  lCtr: integer;
  lParam: TSelectParam;
  lList: TtiMappedFilteredObjectList;
begin
  lList := TtiMappedFilteredObjectList(Visited);
  
  lParam := TSelectParam(lList.Params.FindByProps(['ParamName'], ['AUser']));
  Query.ParamAsString['user_oid'] := lParam.Value;
end;

{ TJobList_FindByStatusVis }
function TJobList_FindByStatusVis.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;

procedure TJobList_FindByStatusVis.MapRowToObject;
var
  lObj: TJob;
begin
  lObj := TJob.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.JobName := Query.FieldAsString['job_name'];
  lObj.JobDesc := Query.FieldAsString['job_desc'];
  lObj.Status := TJobStatus(Query.FieldAsInteger['job_status']);
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

procedure TJobList_FindByStatusVis.SetupParams;
var
  lCtr: integer;
  lParam: TSelectParam;
  lList: TtiMappedFilteredObjectList;
begin
  lList := TtiMappedFilteredObjectList(Visited);
  
  lParam := TSelectParam(lList.Params.FindByProps(['ParamName'], ['AStatus']));
  Query.ParamAsInteger['status'] := Integer(enum(lParam.Value));
end;

{ TUserJobRelation_Create }
function TUserJobRelation_Create.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posCreate;
end;

procedure TUserJobRelation_Create.Init;
begin
  Query.SQLText := 
    'INSERT INTO user_job_relation(' + 
    ' OID, ' + 
    ' job_oid, ' + 
    ' user_oid' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :job_oid, ' + 
    ' :user_oid' + 
    ') ';
end;

procedure TUserJobRelation_Create.SetupParams;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['job_oid'] := lObj.JobOID;
  Query.ParamAsString['user_oid'] := lObj.UserOID;
end;

{ TUserJobRelation_Save }
function TUserJobRelation_Save.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posUpdate;
end;

procedure TUserJobRelation_Save.Init;
begin
  Query.SQLText := 
    'UPDATE user_job_relation SET ' +
    ' job_oid = :job_oid, ' + 
    ' user_oid = :user_oid ' + 
    'WHERE OID = :OID' ;
end;

procedure TUserJobRelation_Save.SetupParams;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['job_oid'] := lObj.JobOID;
  Query.ParamAsString['user_oid'] := lObj.UserOID;
end;

{ TUserJobRelation_Read }
function TUserJobRelation_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posPK) OR (Visited.ObjectState = posClean);
end;

procedure TUserJobRelation_Read.Init;
begin
  Query.SQLText := 
    'SELECT ' + 
    ' OID, ' +
    ' job_oid, ' + 
    ' user_oid ' + 
    'FROM  user_job_relation WHERE OID = :OID' ;
end;

procedure TUserJobRelation_Read.SetupParams;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;

procedure TUserJobRelation_Read.MapRowToObject;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation(Visited);
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.JobOID := Query.FieldAsString['job_oid'];
  lObj.UserOID := Query.FieldAsString['user_oid'];
end;

{ TUserJobRelation_Delete }
function TUserJobRelation_Delete.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posDelete;
end;

procedure TUserJobRelation_Delete.Init;
begin
  Query.SQLText := 
    'DELETE FROM user_job_relation ' +
    'WHERE OID = :OID';
end;

procedure TUserJobRelation_Delete.SetupParams;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;

{ TUserJobRelationList_Read }
function TUserJobRelationList_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;

procedure TUserJobRelationList_Read.Init;
var
  lFiltered: ItiFiltered;
  lWhere: string;
  lOrder: string;
  lSQL: string;
begin
  if Supports(Visited, ItiFiltered, lFiltered) then
  begin
    if lFiltered.GetCriteria.HasCriteria then
      lWhere := ' WHERE ' + tiCriteriaAsSQL(lFiltered.GetCriteria)
    else
      lWhere := '';
    if lFiltered.GetCriteria.hasOrderBy then
      lOrder := tiCriteriaOrderByAsSQL(lFiltered.GetCriteria)
    else
      lOrder := '';
  end;
  
  lSQL := 
    'SELECT ' + 
    ' OID, ' +
    ' job_oid, ' + 
    ' user_oid ' + 
    'FROM  user_job_relation %s %s ;';
  
  Query.SQLText := gFormatSQL(Format(lSQL, [lWhere, lOrder]), TUserJobRelation);
  
end;

procedure TUserJobRelationList_Read.MapRowToObject;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.JobOID := Query.FieldAsString['job_oid'];
  lObj.UserOID := Query.FieldAsString['user_oid'];
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

{ TUserJobRelationList_Create }
function TUserJobRelationList_Create.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posCreate;
end;

procedure TUserJobRelationList_Create.Init;
begin
  Query.SQLText := 
    'INSERT INTO user_job_relation(' + 
    ' OID, ' + 
    ' job_oid, ' + 
    ' user_oid' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :job_oid, ' + 
    ' :user_oid' + 
    ') ';
end;

procedure TUserJobRelationList_Create.SetupParams;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['job_oid'] := lObj.JobOID;
  Query.ParamAsString['user_oid'] := lObj.UserOID;
end;

{ TUserJobRelationList_Delete }
function TUserJobRelationList_Delete.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posDelete;
end;

procedure TUserJobRelationList_Delete.Init;
begin
  Query.SQLText := 
    'DELETE FROM user_job_relation ' +
    'WHERE OID = :OID';
end;

procedure TUserJobRelationList_Delete.SetupParams;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;
{ TUserJobRelationList_Save }
function TUserJobRelationList_Save.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posUpdate;
end;

procedure TUserJobRelationList_Save.Init;
begin
  Query.SQLText := 
    'UPDATE user_job_relation SET ' +
    ' job_oid = :job_oid, ' + 
    ' user_oid = :user_oid ' + 
    'WHERE OID = :OID' ;
end;

procedure TUserJobRelationList_Save.SetupParams;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['job_oid'] := lObj.JobOID;
  Query.ParamAsString['user_oid'] := lObj.UserOID;
end;

{ TUserJobRelationList_FindByUserVis }
function TUserJobRelationList_FindByUserVis.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;

procedure TUserJobRelationList_FindByUserVis.MapRowToObject;
var
  lObj: TUserJobRelation;
begin
  lObj := TUserJobRelation.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.JobOID := Query.FieldAsString['job_oid'];
  lObj.UserOID := Query.FieldAsString['user_oid'];
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

procedure TUserJobRelationList_FindByUserVis.SetupParams;
var
  lCtr: integer;
  lParam: TSelectParam;
  lList: TtiMappedFilteredObjectList;
begin
  lList := TtiMappedFilteredObjectList(Visited);
  
  lParam := TSelectParam(lList.Params.FindByProps(['ParamName'], ['AUserOID']));
  Query.ParamAsString['user_oid'] := lParam.Value;
end;

initialization
  RegisterVisitors;
  RegisterMappings;


end.
