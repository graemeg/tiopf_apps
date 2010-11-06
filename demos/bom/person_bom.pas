

unit person_bom;


{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}


interface


uses
  SysUtils
  ,tiObject
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
  TGenderType = (gtFemale = 0
    ,gtMale = 1);



  // ---------------------------------------------
  // Generated Classes
  // ---------------------------------------------


  { Generated Class: TPerson}
  TPerson = class(TtiObject)
  protected
    FFirstName: string;
    FLastName: string;
    FAge: Integer;
    FGender: TGenderType;
    FIsActive: Boolean;
    FActiveDate: TDateTime;
    FEmail: string;
    procedure SetFirstName(const AValue: string); virtual;
    procedure SetLastName(const AValue: string); virtual;
    procedure SetAge(const AValue: Integer); virtual;
    procedure SetGender(const AValue: TGenderType); virtual;
    procedure SetIsActive(const AValue: Boolean); virtual;
    procedure SetActiveDate(const AValue: TDateTime); virtual;
    procedure SetEmail(const AValue: string); virtual;
  public
    procedure   Read; override;
    procedure   Save; override;
  published
    property    FirstName: string read FFirstName write SetFirstName;
    property    LastName: string read FLastName write SetLastName;
    property    Age: Integer read FAge write SetAge;
    property    Gender: TGenderType read FGender write SetGender;
    property    IsActive: Boolean read FIsActive write SetIsActive;
    property    ActiveDate: TDateTime read FActiveDate write SetActiveDate;
    property    Email: string read FEmail write SetEmail;
  end;
  

  { List of TPerson.  TtiMappedFilteredObjectList descendant. }
  TPersonList = class(TtiMappedFilteredObjectList)
  protected
    procedure   SetItems(i: integer; const AValue: TPerson); reintroduce;
    function    GetItems(i: integer): TPerson; reintroduce;
  public
    property    Items[i:integer] : TPerson read GetItems write SetItems;
    procedure   Add(AObject: TPerson); reintroduce;
    procedure   Read; override;
    procedure   Save; override;
    { Return count (1) if successful. }
    function    FindByOID(const AOID: string): integer;
    { Returns Number of objects retrieved. }
    function    FindByFirstName(const AName: string): integer;
    { Returns Number of objects retrieved. }
    function    FindByFirstNameMatch(const AName: string): integer;
  end;
  

  { Read Visitor for TPerson }
  TPerson_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
    procedure   MapRowToObject; override;
  end;
  

  { Create Visitor for TPerson }
  TPerson_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  

  { Update Visitor for TPerson }
  TPerson_Save = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  

  { Delete Visitor for TPerson }
  TPerson_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  

  { List Read Visitor for TPersonList }
  TPersonList_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   MapRowToObject; override;
  end;
  

  { List Create Visitor for TPersonList }
  TPersonList_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  

  { List Update Visitor for TPersonList }
  TPersonList_Save = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  

  { List Delete Visitor for TPersonList }
  TPersonList_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  

  { TPersonList_FindByFirstNameVis }
  TPersonList_FindByFirstNameVis = class(TtiMapParameterListReadVisitor)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   MapRowToObject; override;
  end;
  

  { TPersonList_FindByFirstNameMatchVis }
  TPersonList_FindByFirstNameMatchVis = class(TtiMapParameterListReadVisitor)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   MapRowToObject; override;
  end;
  



  { Visitor Manager Registrations }
  procedure RegisterVisitors;


  { Register Auto Mappings }
  procedure RegisterMappings;


implementation


procedure RegisterMappings;
begin
  { Automap registrations for TPerson }
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 
    'person', 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson,
    'person','FirstName', 'first_name');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson,
    'person','LastName', 'last_name');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson,
    'person','Age', 'age');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson,
    'person','Gender', 'gender');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TPersonList, TPerson);
  

end;


procedure RegisterVisitors;
begin
  { Register Visitors for TPerson }
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonList_listread', TPersonList_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonList_listsave', TPersonList_Create);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonList_listsave', TPersonList_Save);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonList_listsave', TPersonList_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonread', TPerson_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonsave', TPerson_Save);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersondelete', TPerson_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersoncreate', TPerson_Create);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonList_FindByFirstNameVis', TPersonList_FindByFirstNameVis);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonList_FindByFirstNameMatchVis', TPersonList_FindByFirstNameMatchVis);
  

end;


procedure TPerson.SetFirstName(const AValue: string);
begin
  if FFirstName <> AValue then
    FFirstName := AValue;
end;


procedure TPerson.SetLastName(const AValue: string);
begin
  if FLastName <> AValue then
    FLastName := AValue;
end;


procedure TPerson.SetAge(const AValue: Integer);
begin
  if FAge <> AValue then
    FAge := AValue;
end;


procedure TPerson.SetGender(const AValue: TGenderType);
begin
  if FGender <> AValue then
    FGender := AValue;
end;


procedure TPerson.SetIsActive(const AValue: Boolean);
begin
  if FIsActive <> AValue then
    FIsActive := AValue;
end;


procedure TPerson.SetActiveDate(const AValue: TDateTime);
begin
  if FActiveDate <> AValue then
    FActiveDate := AValue;
end;


procedure TPerson.SetEmail(const AValue: string);
begin
  if FEmail <> AValue then
    FEmail := AValue;
end;


procedure TPerson.Read;
begin
  GTIOPFManager.VisitorManager.Execute(ClassName + 'read', self);
end;


procedure TPerson.Save;
begin
  Case ObjectState of
    posDelete: GTIOPFManager.VisitorManager.Execute('TPersondelete', self);
    posUpdate: GTIOPFManager.VisitorManager.Execute('TPersonsave', self);
    posCreate: GTIOPFManager.VisitorManager.Execute('TPersoncreate', self);
  end;
end;


 {TPersonList }


procedure TPersonList.Add(AObject: TPerson);
begin
  inherited Add(AObject);
end;


function TPersonList.GetItems(i: integer): TPerson;
begin
  result := inherited GetItems(i) as TPerson;
end;


procedure TPersonList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('TPersonList_listread', self);
end;


procedure TPersonList.Save;
begin
  GTIOPFManager.VisitorManager.Execute('TPersonList_listsave', self);
end;


procedure TPersonList.SetItems(i: integer; const AValue: TPerson);
begin
  inherited SetItems(i, AValue);
end;
function TPersonList.FindByOID(const AOID: string): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Criteria.ClearAll;
  Criteria.AddEqualTo('OID', AOID);
  Read;
  result := Count;
end;


function TPersonList.FindByFirstName(const AName: string): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Params.Clear;
  AddParam('AName', 'user_first', ptString, AName);
  self.SQL := 
    ' SELECT PERSON.OID , PERSON.FIRST_NAME, PERSON.LAST_NAME,  ' + 
    ' PERSON.AGE, PERSON.GENDER FROM PERSON WHERE PERSON.FIRST_NAME  ' + 
    ' = :USER_FIRST'; 
  GTIOPFManager.VisitorManager.Execute('TPersonList_FindByFirstNameVis', self);
  result := self.Count;
end;


function TPersonList.FindByFirstNameMatch(const AName: string): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Params.Clear;
  AddParam('AName', 'user_first', ptString, AName);
  self.SQL := 
    ' SELECT PERSON.OID , PERSON.FIRST_NAME, PERSON.LAST_NAME,  ' + 
    ' PERSON.AGE, PERSON.GENDER FROM PERSON WHERE PERSON.FIRST_NAME  ' + 
    ' STARTING WITH :USER_FIRST ORDER BY PERSON.FIRST_NAME,  ' + 
    ' PERSON.LAST_NAME'; 
  GTIOPFManager.VisitorManager.Execute('TPersonList_FindByFirstNameMatchVis', self);
  result := self.Count;
end;


{ TPerson_Create }
function TPerson_Create.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posCreate;
end;


procedure TPerson_Create.Init;
begin
  Query.SQLText := 
    'INSERT INTO person(' + 
    ' OID, ' + 
    ' first_name, ' + 
    ' last_name, ' + 
    ' age, ' + 
    ' gender' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :first_name, ' + 
    ' :last_name, ' + 
    ' :age, ' + 
    ' :gender' + 
    ') ';
end;


procedure TPerson_Create.SetupParams;
var
  lObj: TPerson;
begin
  lObj := TPerson(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['first_name'] := lObj.FirstName;
  Query.ParamAsString['last_name'] := lObj.LastName;
  Query.ParamAsInteger['age'] := lObj.Age;
  Query.ParamAsInteger['gender'] := Integer(lObj.Gender);
end;


{ TPerson_Save }
function TPerson_Save.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posUpdate;
end;


procedure TPerson_Save.Init;
begin
  Query.SQLText := 
    'UPDATE person SET ' +
    ' first_name = :first_name, ' + 
    ' last_name = :last_name, ' + 
    ' age = :age, ' + 
    ' gender = :gender ' + 
    'WHERE OID = :OID' ;
end;


procedure TPerson_Save.SetupParams;
var
  lObj: TPerson;
begin
  lObj := TPerson(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['first_name'] := lObj.FirstName;
  Query.ParamAsString['last_name'] := lObj.LastName;
  Query.ParamAsInteger['age'] := lObj.Age;
  Query.ParamAsInteger['gender'] := Integer(lObj.Gender);
end;


{ TPerson_Read }
function TPerson_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posPK) OR (Visited.ObjectState = posClean);
end;


procedure TPerson_Read.Init;
begin
  Query.SQLText := 
    'SELECT ' + 
    ' OID, ' +
    ' first_name, ' + 
    ' last_name, ' + 
    ' age, ' + 
    ' gender ' + 
    'FROM  person WHERE OID = :OID' ;
end;


procedure TPerson_Read.SetupParams;
var
  lObj: TPerson;
begin
  lObj := TPerson(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;


procedure TPerson_Read.MapRowToObject;
var
  lObj: TPerson;
begin
  lObj := TPerson(Visited);
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.FirstName := Query.FieldAsString['first_name'];
  lObj.LastName := Query.FieldAsString['last_name'];
  lObj.Age := Query.FieldAsInteger['age'];
  lObj.Gender := TGenderType(Query.FieldAsInteger['gender']);
end;


{ TPerson_Delete }
function TPerson_Delete.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posDelete;
end;


procedure TPerson_Delete.Init;
begin
  Query.SQLText := 
    'DELETE FROM person ' +
    'WHERE OID = :OID';
end;


procedure TPerson_Delete.SetupParams;
var
  lObj: TPerson;
begin
  lObj := TPerson(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;


{ TPersonList_Read }
function TPersonList_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;


procedure TPersonList_Read.Init;
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
    ' first_name, ' + 
    ' last_name, ' + 
    ' age, ' + 
    ' gender ' + 
    'FROM  person %s %s ;';
  

  Query.SQLText := gFormatSQL(Format(lSQL, [lWhere, lOrder]), TPerson);
  

end;


procedure TPersonList_Read.MapRowToObject;
var
  lObj: TPerson;
begin
  lObj := TPerson.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.FirstName := Query.FieldAsString['first_name'];
  lObj.LastName := Query.FieldAsString['last_name'];
  lObj.Age := Query.FieldAsInteger['age'];
  lObj.Gender := TGenderType(Query.FieldAsInteger['gender']);
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;


{ TPersonList_Create }
function TPersonList_Create.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posCreate;
end;


procedure TPersonList_Create.Init;
begin
  Query.SQLText := 
    'INSERT INTO person(' + 
    ' OID, ' + 
    ' first_name, ' + 
    ' last_name, ' + 
    ' age, ' + 
    ' gender' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :first_name, ' + 
    ' :last_name, ' + 
    ' :age, ' + 
    ' :gender' + 
    ') ';
end;


procedure TPersonList_Create.SetupParams;
var
  lObj: TPerson;
begin
  lObj := TPerson(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['first_name'] := lObj.FirstName;
  Query.ParamAsString['last_name'] := lObj.LastName;
  Query.ParamAsInteger['age'] := lObj.Age;
  Query.ParamAsInteger['gender'] := Integer(lObj.Gender);
end;


{ TPersonList_Delete }
function TPersonList_Delete.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posDelete;
end;


procedure TPersonList_Delete.Init;
begin
  Query.SQLText := 
    'DELETE FROM person ' +
    'WHERE OID = :OID';
end;


procedure TPersonList_Delete.SetupParams;
var
  lObj: TPerson;
begin
  lObj := TPerson(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;
{ TPersonList_Save }
function TPersonList_Save.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posUpdate;
end;


procedure TPersonList_Save.Init;
begin
  Query.SQLText := 
    'UPDATE person SET ' +
    ' first_name = :first_name, ' + 
    ' last_name = :last_name, ' + 
    ' age = :age, ' + 
    ' gender = :gender ' + 
    'WHERE OID = :OID' ;
end;


procedure TPersonList_Save.SetupParams;
var
  lObj: TPerson;
begin
  lObj := TPerson(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['first_name'] := lObj.FirstName;
  Query.ParamAsString['last_name'] := lObj.LastName;
  Query.ParamAsInteger['age'] := lObj.Age;
  Query.ParamAsInteger['gender'] := Integer(lObj.Gender);
end;


{ TPersonList_FindByFirstNameVis }
function TPersonList_FindByFirstNameVis.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;


procedure TPersonList_FindByFirstNameVis.MapRowToObject;
var
  lObj: TPerson;
begin
  lObj := TPerson.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.FirstName := Query.FieldAsString['first_name'];
  lObj.LastName := Query.FieldAsString['last_name'];
  lObj.Age := Query.FieldAsInteger['age'];
  lObj.Gender := TGenderType(Query.FieldAsInteger['gender']);
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;


{ TPersonList_FindByFirstNameMatchVis }
function TPersonList_FindByFirstNameMatchVis.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;


procedure TPersonList_FindByFirstNameMatchVis.MapRowToObject;
var
  lObj: TPerson;
begin
  lObj := TPerson.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.FirstName := Query.FieldAsString['first_name'];
  lObj.LastName := Query.FieldAsString['last_name'];
  lObj.Age := Query.FieldAsInteger['age'];
  lObj.Gender := TGenderType(Query.FieldAsInteger['gender']);
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;


initialization
  RegisterVisitors;
  RegisterMappings;


end.
