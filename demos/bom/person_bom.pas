

unit person_bom;


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
  TGenderType = (gtFemale = 0
    ,gtMale = 1);

  TPersonType = (ptEmployee
    ,ptCustomer
    ,ptVendor);



  // ---------------------------------------------
  // Generated Classes
  // ---------------------------------------------


  { Generated Class: TPerson}
  TPerson = class(TtiObject)
  protected
    FPersonType: TPersonType;
    FFirstName: string;
    FLastName: string;
    FAge: Integer;
    FGender: TGenderType;
    FIsActive: Boolean;
    FActiveDate: TDateTime;
    FEmail: string;
    procedure SetPersonType(const AValue: TPersonType); virtual;
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
    property    PersonType: TPersonType read FPersonType write SetPersonType;
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
    function    FindByGender(const AGender: TGenderType): integer;
    { Returns Number of objects retrieved. }
    function    FindByFirstNameMatch(const AName: string): integer;
  end;
  
  { Generated Class: TCustom}
  TCustom = class(TtiObject)
  protected
    FName: string;
    FAddress: string;
    FAddress2: string;
    procedure SetName(const AValue: string); virtual;
    procedure SetAddress(const AValue: string); virtual;
    procedure SetAddress2(const AValue: string); virtual;
  public
    procedure   Read; override;
    procedure   Save; override;
  published
    property    Name: string read FName write SetName;
    property    Address: string read FAddress write SetAddress;
    property    Address2: string read FAddress2 write SetAddress2;
  end;
  
  { List of TCustom.  TtiMappedFilteredObjectList descendant. }
  TCustomList = class(TtiMappedFilteredObjectList)
  protected
    procedure   SetItems(i: integer; const AValue: TCustom); reintroduce;
    function    GetItems(i: integer): TCustom; reintroduce;
  public
    property    Items[i:integer] : TCustom read GetItems write SetItems;
    procedure   Add(AObject: TCustom); reintroduce;
    procedure   Read; override;
    procedure   Save; override;
    { Return count (1) if successful. }
    function    FindByOID(const AOID: string): integer;
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
  
  { TPersonList_FindByGenderVis }
  TPersonList_FindByGenderVis = class(TtiMapParameterListReadVisitor)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   MapRowToObject; override;
    procedure   SetupParams; override;
  end;
  
  { TPersonList_FindByFirstNameMatchVis }
  TPersonList_FindByFirstNameMatchVis = class(TtiMapParameterListReadVisitor)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   MapRowToObject; override;
    procedure   SetupParams; override;
  end;
  
  { Read Visitor for TCustom }
  TCustom_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
    procedure   MapRowToObject; override;
  end;
  
  { Create Visitor for TCustom }
  TCustom_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { Update Visitor for TCustom }
  TCustom_Save = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { Delete Visitor for TCustom }
  TCustom_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Read Visitor for TCustomList }
  TCustomList_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   MapRowToObject; override;
  end;
  
  { List Create Visitor for TCustomList }
  TCustomList_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Update Visitor for TCustomList }
  TCustomList_Save = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;
  
  { List Delete Visitor for TCustomList }
  TCustomList_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
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
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson,
    'person','PersonType', 'person_type');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TPersonList, TPerson);
  
  { Automap registrations for TCustom }
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustom, 
    'person', 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustom,
    'person','Name', 'cust_name');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustom,
    'person','Address', 'address');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustom,
    'person','Address2', 'address2');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TCustomList, TCustom);
  
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
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonList_FindByGenderVis', TPersonList_FindByGenderVis);
  GTIOPFManager.VisitorManager.RegisterVisitor('TPersonList_FindByFirstNameMatchVis', TPersonList_FindByFirstNameMatchVis);
  
  { Register Visitors for TCustom }
  GTIOPFManager.VisitorManager.RegisterVisitor('TCustomList_listread', TCustomList_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('TCustomList_listsave', TCustomList_Create);
  GTIOPFManager.VisitorManager.RegisterVisitor('TCustomList_listsave', TCustomList_Save);
  GTIOPFManager.VisitorManager.RegisterVisitor('TCustomList_listsave', TCustomList_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('TCustomread', TCustom_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('TCustomsave', TCustom_Save);
  GTIOPFManager.VisitorManager.RegisterVisitor('TCustomdelete', TCustom_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('TCustomcreate', TCustom_Create);
  
end;

procedure TPerson.SetPersonType(const AValue: TPersonType);
begin
  if FPersonType <> AValue then
    FPersonType := AValue;
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

function TPersonList.FindByGender(const AGender: TGenderType): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Params.Clear;
  AddParam('AGender', 'gender_type', ptEnum, AGender);
  self.SQL := 
    ' SELECT PERSON.OID , PERSON.FIRST_NAME, PERSON.LAST_NAME,  ' + 
    ' PERSON.AGE, PERSON.GENDER, PERSON.PERSON_TYPE FROM  ' + 
    ' PERSON WHERE PERSON.GENDER = :gender_type'; 
  GTIOPFManager.VisitorManager.Execute('TPersonList_FindByGenderVis', self);
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
    ' PERSON.AGE, PERSON.GENDER, PERSON.PERSON_TYPE FROM  ' + 
    ' PERSON WHERE PERSON.FIRST_NAME STARTING WITH :USER_FIRST  ' + 
    ' ORDER BY PERSON.FIRST_NAME, PERSON.LAST_NAME'; 
  GTIOPFManager.VisitorManager.Execute('TPersonList_FindByFirstNameMatchVis', self);
  result := self.Count;
end;

procedure TCustom.SetName(const AValue: string);
begin
  if FName <> AValue then
    FName := AValue;
end;

procedure TCustom.SetAddress(const AValue: string);
begin
  if FAddress <> AValue then
    FAddress := AValue;
end;

procedure TCustom.SetAddress2(const AValue: string);
begin
  if FAddress2 <> AValue then
    FAddress2 := AValue;
end;

procedure TCustom.Read;
begin
  GTIOPFManager.VisitorManager.Execute(ClassName + 'read', self);
end;

procedure TCustom.Save;
begin
  Case ObjectState of
    posDelete: GTIOPFManager.VisitorManager.Execute('TCustomdelete', self);
    posUpdate: GTIOPFManager.VisitorManager.Execute('TCustomsave', self);
    posCreate: GTIOPFManager.VisitorManager.Execute('TCustomcreate', self);
  end;
end;

 {TCustomList }

procedure TCustomList.Add(AObject: TCustom);
begin
  inherited Add(AObject);
end;

function TCustomList.GetItems(i: integer): TCustom;
begin
  result := inherited GetItems(i) as TCustom;
end;

procedure TCustomList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('TCustomList_listread', self);
end;

procedure TCustomList.Save;
begin
  GTIOPFManager.VisitorManager.Execute('TCustomList_listsave', self);
end;

procedure TCustomList.SetItems(i: integer; const AValue: TCustom);
begin
  inherited SetItems(i, AValue);
end;
function TCustomList.FindByOID(const AOID: string): integer;
begin
  if self.Count > 0 then
    self.Clear;
    
  Criteria.ClearAll;
  Criteria.AddEqualTo('OID', AOID);
  Read;
  result := Count;
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
    ' gender, ' + 
    ' person_type' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :first_name, ' + 
    ' :last_name, ' + 
    ' :age, ' + 
    ' :gender, ' + 
    ' :person_type' + 
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
  Query.ParamAsInteger['person_type'] := Integer(lObj.PersonType);
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
    ' gender = :gender, ' + 
    ' person_type = :person_type ' + 
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
  Query.ParamAsInteger['person_type'] := Integer(lObj.PersonType);
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
    ' gender, ' + 
    ' person_type ' + 
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
  lObj.PersonType := TPersonType(Query.FieldAsInteger['person_type']);
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
    ' gender, ' + 
    ' person_type ' + 
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
  lObj.PersonType := TPersonType(Query.FieldAsInteger['person_type']);
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
    ' gender, ' + 
    ' person_type' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :first_name, ' + 
    ' :last_name, ' + 
    ' :age, ' + 
    ' :gender, ' + 
    ' :person_type' + 
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
  Query.ParamAsInteger['person_type'] := Integer(lObj.PersonType);
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
    ' gender = :gender, ' + 
    ' person_type = :person_type ' + 
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
  Query.ParamAsInteger['person_type'] := Integer(lObj.PersonType);
end;

{ TPersonList_FindByGenderVis }
function TPersonList_FindByGenderVis.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;

procedure TPersonList_FindByGenderVis.MapRowToObject;
var
  lObj: TPerson;
begin
  lObj := TPerson.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.FirstName := Query.FieldAsString['first_name'];
  lObj.LastName := Query.FieldAsString['last_name'];
  lObj.Age := Query.FieldAsInteger['age'];
  lObj.Gender := TGenderType(Query.FieldAsInteger['gender']);
  lObj.PersonType := TPersonType(Query.FieldAsInteger['person_type']);
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

procedure TPersonList_FindByGenderVis.SetupParams;
var
  lCtr: integer;
  lParam: TSelectParam;
  lList: TtiMappedFilteredObjectList;
  lOrdInfo: PTypeInfo;
  lOrdDate: PTypeData;
begin
  lList := TtiMappedFilteredObjectList(Visited);
  
  lParam := TSelectParam(lList.Params.FindByProps(['ParamName'], ['AGender']));
  Query.ParamAsInteger['gender_type'] := Integer(TGenderType(lParam.Value));
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
  lObj.PersonType := TPersonType(Query.FieldAsInteger['person_type']);
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

procedure TPersonList_FindByFirstNameMatchVis.SetupParams;
var
  lCtr: integer;
  lParam: TSelectParam;
  lList: TtiMappedFilteredObjectList;
  lOrdInfo: PTypeInfo;
  lOrdDate: PTypeData;
begin
  lList := TtiMappedFilteredObjectList(Visited);
  
  lParam := TSelectParam(lList.Params.FindByProps(['ParamName'], ['AName']));
  Query.ParamAsString['user_first'] := lParam.Value;
end;

{ TCustom_Create }
function TCustom_Create.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posCreate;
end;

procedure TCustom_Create.Init;
begin
  Query.SQLText := 
    'INSERT INTO person(' + 
    ' OID, ' + 
    ' cust_name, ' + 
    ' address, ' + 
    ' address2' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :cust_name, ' + 
    ' :address, ' + 
    ' :address2' + 
    ') ';
end;

procedure TCustom_Create.SetupParams;
var
  lObj: TCustom;
begin
  lObj := TCustom(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['cust_name'] := lObj.Name;
  Query.ParamAsString['address'] := lObj.Address;
  Query.ParamAsString['address2'] := lObj.Address2;
end;

{ TCustom_Save }
function TCustom_Save.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posUpdate;
end;

procedure TCustom_Save.Init;
begin
  Query.SQLText := 
    'UPDATE person SET ' +
    ' cust_name = :cust_name, ' + 
    ' address = :address, ' + 
    ' address2 = :address2 ' + 
    'WHERE OID = :OID' ;
end;

procedure TCustom_Save.SetupParams;
var
  lObj: TCustom;
begin
  lObj := TCustom(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['cust_name'] := lObj.Name;
  Query.ParamAsString['address'] := lObj.Address;
  Query.ParamAsString['address2'] := lObj.Address2;
end;

{ TCustom_Read }
function TCustom_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posPK) OR (Visited.ObjectState = posClean);
end;

procedure TCustom_Read.Init;
begin
  Query.SQLText := 
    'SELECT ' + 
    ' OID, ' +
    ' cust_name, ' + 
    ' address, ' + 
    ' address2 ' + 
    'FROM  person WHERE OID = :OID' ;
end;

procedure TCustom_Read.SetupParams;
var
  lObj: TCustom;
begin
  lObj := TCustom(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;

procedure TCustom_Read.MapRowToObject;
var
  lObj: TCustom;
begin
  lObj := TCustom(Visited);
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.Name := Query.FieldAsString['cust_name'];
  lObj.Address := Query.FieldAsString['address'];
  lObj.Address2 := Query.FieldAsString['address2'];
end;

{ TCustom_Delete }
function TCustom_Delete.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posDelete;
end;

procedure TCustom_Delete.Init;
begin
  Query.SQLText := 
    'DELETE FROM person ' +
    'WHERE OID = :OID';
end;

procedure TCustom_Delete.SetupParams;
var
  lObj: TCustom;
begin
  lObj := TCustom(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;

{ TCustomList_Read }
function TCustomList_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
end;

procedure TCustomList_Read.Init;
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
    ' cust_name, ' + 
    ' address, ' + 
    ' address2 ' + 
    'FROM  person %s %s ;';
  
  Query.SQLText := gFormatSQL(Format(lSQL, [lWhere, lOrder]), TCustom);
  
end;

procedure TCustomList_Read.MapRowToObject;
var
  lObj: TCustom;
begin
  lObj := TCustom.Create;
  lObj.OID.AssignFromTIQuery('OID',Query);
  lObj.Name := Query.FieldAsString['cust_name'];
  lObj.Address := Query.FieldAsString['address'];
  lObj.Address2 := Query.FieldAsString['address2'];
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

{ TCustomList_Create }
function TCustomList_Create.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posCreate;
end;

procedure TCustomList_Create.Init;
begin
  Query.SQLText := 
    'INSERT INTO person(' + 
    ' OID, ' + 
    ' cust_name, ' + 
    ' address, ' + 
    ' address2' + 
    ') VALUES (' +
    ' :OID, ' +
    ' :cust_name, ' + 
    ' :address, ' + 
    ' :address2' + 
    ') ';
end;

procedure TCustomList_Create.SetupParams;
var
  lObj: TCustom;
begin
  lObj := TCustom(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['cust_name'] := lObj.Name;
  Query.ParamAsString['address'] := lObj.Address;
  Query.ParamAsString['address2'] := lObj.Address2;
end;

{ TCustomList_Delete }
function TCustomList_Delete.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posDelete;
end;

procedure TCustomList_Delete.Init;
begin
  Query.SQLText := 
    'DELETE FROM person ' +
    'WHERE OID = :OID';
end;

procedure TCustomList_Delete.SetupParams;
var
  lObj: TCustom;
begin
  lObj := TCustom(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
end;
{ TCustomList_Save }
function TCustomList_Save.AcceptVisitor: Boolean;
begin
  result := Visited.ObjectState = posUpdate;
end;

procedure TCustomList_Save.Init;
begin
  Query.SQLText := 
    'UPDATE person SET ' +
    ' cust_name = :cust_name, ' + 
    ' address = :address, ' + 
    ' address2 = :address2 ' + 
    'WHERE OID = :OID' ;
end;

procedure TCustomList_Save.SetupParams;
var
  lObj: TCustom;
begin
  lObj := TCustom(Visited);
  lObj.OID.AssignToTIQuery('OID',Query);
  Query.ParamAsString['cust_name'] := lObj.Name;
  Query.ParamAsString['address'] := lObj.Address;
  Query.ParamAsString['address2'] := lObj.Address2;
end;

initialization
  RegisterVisitors;
  RegisterMappings;


end.
