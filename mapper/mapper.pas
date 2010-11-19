unit mapper;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}


interface

uses
  Classes
  ,SysUtils
  ,contnrs
  ,variants
  ,tiObject
  ,tiRTTI
  ,tiAutoMap
  ,typinfo
  ,tiVisitorDB
  ,tiOPFManager
  ,tiFilteredObjectList
  ;

type

  // -----------------------------------------------------------------
  //  Foward Declares
  // -----------------------------------------------------------------

  TBaseMapObject = class;
  TMapUnitDef = class;
  TMapUnitDefList = class;
  TMapEnum = class;
  TMapEnumList = class;
  TMapClassDef = class;
  TMapClassDefList = class;
  TClassMapping = class;

  // -----------------------------------------------------------------
  //  Events Signatures
  // -----------------------------------------------------------------

  TOnWriteUnit = procedure(AUnitDef: TMapUnitDef) of object;
  TOnWriteClassIntf = procedure(AClassDef: TMapClassDef) of object;
  TOnWriteEnum = procedure(AEnum: TMapEnum) of object;
  TOnWriteMapping = procedure(AClassDef: TMapClassDef; AMapping: TClassMapping) of object;

  // -----------------------------------------------------------------
  //  Enumerations
  // -----------------------------------------------------------------

  {: Type of class property. }
  TMapPropType = (ptString, ptAnsiString, ptDouble, ptSingle, ptCurrency, ptInteger, ptInt64,
    ptDateTime, ptBoolean, ptEnum);

  {: Type of class definition to create. }
  TClassDefType = (dtCreate, dtReference);

  {: Filter type to use when filtering objects. }
  TFilterType = (ftEqual, ftNotEqual, ftGreater, ftGreaterOrEqual, ftLess, ftLessOrEqual,
    ftLike, ftNotLike, ftStartingWith, ftNotEmpty);

  {: Indicates an objects state. }
  TORMObjectState = (osUnchanged, osChanged, osDeleted, osCreated);

  {: Indicates the state of an object list. }
  TORMListState = (lsEmpty, lsPopulated);

  {: Describes the type of OID to use for object. }
  TOIDType = (otString, otInt);

  {: Describes show enumeratated types should be handled. }
  TEnumType = (etInt, etString);

  {: Describes validators types. }
  TValidatorType = (vtRequired, vtGreater, vtGreaterEqual, vtLess, vtLessEqual,
    vtNotEqual, vtRegExp);


  // -----------------------------------------------------------------
  //  Method Objects
  // -----------------------------------------------------------------

  //TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TGetValidatorErrorString = function(const AValType: TValidatorType): string;

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Base Mapper Object. }
  {$M+}
  TBaseMapObject = class(TtiObject)
  end;
  {$M-}

  TBaseMapObjectList = class(TtiObjectList)
  protected
    function    GetItems(i: Integer): TBaseMapObject; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TBaseMapObject); reintroduce;
  public
    property    Items[Index: Integer]: TBaseMapObject read GetItems write SetItems; default;
    function    Add(AObject: TBaseMapObject): Integer; reintroduce;
  end;

  {: Base project class. }
  TMapProject = class(TBaseMapObject)
  private
    FBaseDirectory: string;
    FBeginEndTabs: integer;
    FEnumType: TEnumType;
    FIncludes: TStringList;
    FFileName: string;
    FMaxEditorCodeWidth: integer;
    FOrigOutDirectory: string;
    FOutputDirectory: string;
    FProjectClasses: TMapClassDefList;
    FProjectEnums: TMapEnumList;
    FProjectName: string;
    FTabSpaces: integer;
    FUnits: TMapUnitDefList;
    FVisibilityTabs: integer;
    procedure SetBaseDirectory(const AValue: string);
    procedure SetBeginEndTabs(const AValue: integer);
    procedure SetEnumType(const AValue: TEnumType);
    procedure SetFileName(const AValue: string);
    procedure SetMaxEditorCodeWidth(const AValue: integer);
    procedure SetOrigOutDirectory(const AValue: string);
    procedure SetOutputDirectory(const AValue: string);
    procedure SetProjectClasses(const AValue: TMapClassDefList);
    procedure SetProjectName(const AValue: string);
    procedure SetTabSpaces(const AValue: integer);
    procedure SetVisibilityTabs(const AValue: integer);
  published
    property    FileName: string read FFileName write SetFileName;
    property    ProjectName: string read FProjectName write SetProjectName;
    property    Includes: TStringList read FIncludes;
    property    OrigOutDirectory: string read FOrigOutDirectory write SetOrigOutDirectory;
    property    OutputDirectory: string read FOutputDirectory write SetOutputDirectory;
    property    BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property    TabSpaces: integer read FTabSpaces write SetTabSpaces;
    property    BeginEndTabs: integer read FBeginEndTabs write SetBeginEndTabs;
    property    VisibilityTabs: integer read FVisibilityTabs write SetVisibilityTabs;
    property    MaxEditorCodeWidth: integer read FMaxEditorCodeWidth write SetMaxEditorCodeWidth;
    property    EnumType: TEnumType read FEnumType write SetEnumType;
  public
    // object properties
    property    Units: TMapUnitDefList read FUnits;
    property    ProjectClasses: TMapClassDefList read FProjectClasses write SetProjectClasses;
    property    ProjectEnums: TMapEnumList read FProjectEnums;
    function    FindEnumForPropName(const AUnitName: string; const AClassName: string; const APropName: string): TMapEnum;
    function    HasCustomSelects: boolean;
    procedure   ClearAll;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TMapProjectList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TMapProject; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TMapProject); reintroduce;
  public
    property    Items[Index: Integer]: TMapProject read GetItems write SetItems; default;
    function    Add(AObject: TMapProject): Integer; reintroduce;
  end;


  TMapConnectionDef = class(TBaseMapObject)
  private
    FConnType: string;
    FDataSource: string;
    FHost: string;
    FParams: TStringList;
    procedure SetConnType(const AValue: string);
    procedure SetDataSource(const AValue: string);
    procedure SetHost(const AValue: string);
  public
    property    ConnType: string read FConnType write SetConnType;
    property    Host: string read FHost write SetHost;
    property    DataSource: string read FDataSource write SetDataSource;
    property    Params: TStringList read FParams;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TMapConnectionDefList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TMapConnectionDef; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TMapConnectionDef); reintroduce;
  public
    property    Items[Index: Integer]: TMapConnectionDef read GetItems write SetItems; default;
    function    Add(AObject: TMapConnectionDef): Integer; reintroduce;
  end;

  TMapEnumValue = class(TBaseMapObject)
  private
    FEnumValue: integer;
    FEnumValueName: string;
    procedure SetEnumValue(const AValue: integer);
    procedure SetEnumValueName(const AValue: string);
  public
    property    EnumValueName: string read FEnumValueName write SetEnumValueName;
    property    EnumValue: integer read FEnumValue write SetEnumValue;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TMapEnumValueList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TMapEnumValue; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TMapEnumValue); reintroduce;
  public
    property    Items[Index: Integer]: TMapEnumValue read GetItems write SetItems; default;
    function    Add(AObject: TMapEnumValue): Integer; reintroduce; overload;
    function    Add(const AName: string; const AValue: integer = -1): integer; overload;
  end;

  TMapEnum = class(TBaseMapObject)
  private
    FEnumName: string;
    FValues: TMapEnumValueList;
    procedure SetEnumName(const AValue: string);
    procedure SetValues(const AValue: TMapEnumValueList);
  published
    property    EnumName: string read FEnumName write SetEnumName;
  public
    property    Values: TMapEnumValueList read FValues write SetValues;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TMapEnumList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TMapEnum; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TMapEnum); reintroduce;
  public
    property    Items[i: Integer]: TMapEnum read GetItems write SetItems; default;
    function    Add(AObject: TMapEnum): Integer; reintroduce;
    function    FindByName(const AName: string): TMapEnum;
  end;

  TMapClassProp = class(TBaseMapObject)
  private
    FIsReadOnly: boolean;
    FPropName: string;
    FPropType: TMapPropType;
    FPropTypeName: string;
    procedure SetIsReadOnly(const AValue: boolean);
    procedure SetPropName(const AValue: string);
    procedure SetPropType(const AValue: TMapPropType);
    procedure SetPropTypeName(const AValue: string);
  published
    property    PropName: string read FPropName write SetPropName;
    property    PropType: TMapPropType read FPropType write SetPropType;
    property    PropTypeName: string read FPropTypeName write SetPropTypeName;
    property    IsReadOnly: boolean read FIsReadOnly write SetIsReadOnly;
  end;

  TMapClassPropList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TMapClassProp; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TMapClassProp); reintroduce;
  public
    property    Items[Index: Integer]: TMapClassProp read GetItems write SetItems; default;
    function    Add(AObject: TMapClassProp): Integer; reintroduce; overload;
    function    Add(const AName: string; const APropType: TMapPropType): integer; overload;
    function    FindByName(const AName: string): TMapClassProp;
  end;

  TPropMapping = class(TBaseMapObject)
  private
    FFieldName: string;
    FPropName: string;
    FPropType: TMapPropType;
    procedure SetFieldName(const AValue: string);
    procedure SetPropName(const AValue: string);
    procedure SetPropType(const AValue: TMapPropType);
  published
    property    PropName: string read FPropName write SetPropName;
    property    FieldName: string read FFieldName write SetFieldName;
    property    PropType: TMapPropType read FPropType write SetPropType;
  end;

  TPropMappingList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TPropMapping; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TPropMapping); reintroduce;
  public
    property    Items[Index: Integer]: TPropMapping read GetItems write SetItems; default;
    function    Add(AObject: TPropMapping): Integer; reintroduce;
  end;

  TClassMapping = class(TBaseMapObject)
  private
    FOIDType: TOIDType;
    FPKField: string;
    FPKName: string;
    FPropMappings: TPropMappingList;
    FTableName: string;
    procedure SetOIDType(const AValue: TOIDType);
    procedure SetPKField(const AValue: string);
    procedure SetPKName(const AValue: string);
    procedure SetTableName(const AValue: string);
  public
    property    TableName: string read FTableName write SetTableName;
    property    PKName: string read FPKName write SetPKName;
    property    PKField: string read FPKField write SetPKField;
    property    PropMappings: TPropMappingList read FPropMappings;
    property    OIDType: TOIDType read FOIDType write SetOIDType;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TSelectParam = class(TBaseMapObject)
  private
    FParamName: string;
    FParamType: TMapPropType;
    FParamTypeName: string;
    FPassBy: string;
    FSQLParamName: string;
    FTypeName: string;
    FValue: Variant;
    procedure SetParamName(const AValue: string);
    procedure SetParamType(const AValue: TMapPropType);
    procedure SetParamTypeName(const AValue: string);
    procedure SetPassBy(const AValue: string);
    procedure SetSQLParamName(const AValue: string);
    procedure SetTypeName(const AValue: string);
    procedure SetValue(const AValue: Variant);
  published
    property    ParamName: string read FParamName write SetParamName;
    property    SQLParamName: string read FSQLParamName write SetSQLParamName;
    property    ParamType: TMapPropType read FParamType write SetParamType;
    property    ParamTypeName: string read FParamTypeName write SetParamTypeName;
    property    TypeName: string read FTypeName write SetTypeName;
    property    PassBy: string read FPassBy write SetPassBy;
    property    Value: Variant read FValue write SetValue;
  end;

  TSelectParamList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TSelectParam; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TSelectParam); reintroduce;
  public
    property    Items[Index: Integer]: TSelectParam read GetItems write SetItems; default;
    function    Add(AObject: TSelectParam): Integer; reintroduce;
    function    FindByName(const AName: string): TSelectParam;
  end;

  TClassMappingSelect = class(TBaseMapObject)
  private
    FName: string;
    FParams: TSelectParamList;
    FSQL: TStringList;
    procedure SetName(const AValue: string);
  protected
    function    GetCaption: string; override;
  public
    property    Params: TSelectParamList read FParams;
    property    SQL: TStringList read FSQL;
    constructor Create; override;
    destructor  Destroy; override;
  published
    property    Name: string read FName write SetName;
  end;

  TClassMappingSelectList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TClassMappingSelect; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TClassMappingSelect); reintroduce;
  public
    property    Items[Index: Integer]: TClassMappingSelect read GetItems write SetItems; default;
    function    Add(AObject: TClassMappingSelect): Integer; reintroduce;
  end;

  TClassMappingList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TClassMapping; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TClassMapping); reintroduce;
  public
    property    Items[Index: Integer]: TClassMapping read GetItems write SetItems; default;
    function    Add(AObject: TClassMapping): Integer; reintroduce;
  end;

  TFilterDef = class(TBaseMapObject)
  private
    FField: String;
    FFilterType: TFilterType;
    procedure SetField(const AValue: String);
    procedure SetFilterType(const AValue: TFilterType);
  public
    property    FilterType: TFilterType read FFilterType write SetFilterType;
    property    Field: String read FField write SetField;
  end;

  TFilterDefList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TFilterDef; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TFilterDef); reintroduce;
  public
    property    Items[Index: Integer]: TFilterDef read GetItems write SetItems; default;
    function    Add(AObject: TFilterDef): Integer; reintroduce;
  end;

  {: Validators get written into the class object's IsValid override. }
  TMapValidator = class(TBaseMapObject)
  private
    FClassProp: string;
    FValidatorType: TValidatorType;
    FValue: variant;
    procedure SetClassProp(const AValue: string);
    procedure SetValidatorType(const AValue: TValidatorType);
    procedure SetValue(const AValue: variant);
  published
    property    ValidatorType: TValidatorType read FValidatorType write SetValidatorType;
    property    ClassProp : string read FClassProp write SetClassProp;
    property    Value: variant read FValue write SetValue;
  end;

  {: List of TValidator objects. }
  TMapValidatorList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TMapValidator; reintroduce;
    procedure   SetItems(i: integer; const AValue: TMapValidator); reintroduce;
  public
    property    Items[i:integer] : TMapValidator read GetItems write SetItems;
    procedure   Add(AObject : TMapValidator); reintroduce;
  end;

  TMapClassDef = class(TBaseMapObject)
  private
    FAutoCreateBase: boolean;
    FAutoCreateListClass: boolean;
    FAutoMap: boolean;
    FBaseClassName: string;
    FBaseClassParent: string;
    FBaseUnitName: string;
    FClassMapping: TClassMapping;
    FClassProps: TMapClassPropList;
    FCrud: string;
    FDefType: TClassDefType;
    FForwardDeclare: boolean;
    FORMClassName: string;
    FSelections: TClassMappingSelectList;
    FValidators: TMapValidatorList;
    procedure SetAutoCreateBase(const AValue: boolean);
    procedure SetAutoCreateListClass(const AValue: boolean);
    procedure SetAutoMap(const AValue: boolean);
    procedure SetBaseClassName(const AValue: string);
    procedure SetBaseClassParent(const AValue: string);
    procedure SetBaseUnitName(const AValue: string);
    procedure SetClassProps(const AValue: TMapClassPropList);
    procedure SetCrud(const AValue: string);
    procedure SetDefType(const AValue: TClassDefType);
    procedure SetForwardDeclare(const AValue: boolean);
    procedure SetORMClassName(const AValue: string);
  public
    // Object Props
    property    ClassProps: TMapClassPropList read FClassProps write SetClassProps;
    property    ClassMapping: TClassMapping read FClassMapping;
    property    Selections: TClassMappingSelectList read FSelections;
    property    Validators: TMapValidatorList read FValidators;

    constructor Create; override;
    destructor  Destroy; override;
  published
    property    AutoCreateBase: boolean read FAutoCreateBase write SetAutoCreateBase;
    property    AutoCreateListClass: boolean read FAutoCreateListClass write SetAutoCreateListClass;
    property    AutoMap: Boolean read FAutoMap write SetAutoMap;
    property    BaseClassName: string read FBaseClassName write SetBaseClassName;
    property    BaseClassParent: string read FBaseClassParent write SetBaseClassParent;
    property    BaseUnitName: string read FBaseUnitName write SetBaseUnitName;
    property    Crud: string read FCrud write SetCrud;
    property    DefType: TClassDefType read FDefType write SetDefType;
    property    FowardDeclare: boolean read FForwardDeclare write SetForwardDeclare;
    property    ORMClassName: string read FORMClassName write SetORMClassName;
  end;

  TMapClassDefList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TMapClassDef; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TMapClassDef); reintroduce;
  public
    property    Items[Index: Integer]: TMapClassDef read GetItems write SetItems; default;
    function    Add(AObject: TMapClassDef): Integer; reintroduce;
    function    FindByName(const AName: string): TMapClassDef;
  end;

  TMapUnitDef = class(TBaseMapObject)
  private
    FReferences: TStringList;
    FUnitClasses: TMapClassDefList;
    FUnitEnums: TMapEnumList;
    FUnitName: string;
    procedure SetUnitName(const AValue: string);
  published
    property    UnitName: string read FUnitName write SetUnitName;
    // Object properties
    property    UnitClasses: TMapClassDefList read FUnitClasses;
    property    UnitEnums: TMapEnumList read FUnitEnums;
    property    References: TStringList read FReferences;
  public
    function    HasValidators: boolean;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TMapUnitDefList = class(TBaseMapObjectList)
  protected
    function    GetItems(i: Integer): TMapUnitDef; reintroduce;
    procedure   SetItems(i: Integer;  AObject: TMapUnitDef); reintroduce;
  public
    property    Items[Index: Integer]: TMapUnitDef read GetItems write SetItems; default;
    function    Add(AObject: TMapUnitDef): Integer; reintroduce;
    function    FindByName(const AName: string): TMapUnitDef;
  end;

  {: Class of TMapSchemaReader. }
  TMapSchemaReader = class;
  TMapSchemaReaderClass = class of TMapSchemaReader;

  {: Abstract class used to read xml schema file into TMapProject. }
  TMapSchemaReader = class(TBaseMapObject)
  public
    procedure   ReadSchema(AProject: TMapProject; const AFileName: string); overload; virtual; abstract;
    procedure   WriteAll; virtual; abstract;
  end;


  TMapSchemaWriter = class(TBaseMapObject)
  private
    FCurrentIndent: integer;
    FProject: TMapProject;
    procedure SetCurrentIndent(const AValue: integer);
    procedure SetOnWriteClass(const AValue: TOnWriteClassIntf);
    procedure SetOnWriteEnum(const AValue: TOnWriteEnum);
    procedure SetOnWriteMapping(const AValue: TOnWriteMapping);
    procedure SetOnWriteUnit(const AValue: TOnWriteUnit);
    procedure SetProject(const AValue: TMapProject);
  protected
    FOnWriteClass: TOnWriteClassIntf;
    FOnWriteEnum: TOnWriteEnum;
    FOnWriteMapping: TOnWriteMapping;
    FOnWriteUnit: TOnWriteUnit;
    property    CurrentIndent: integer read FCurrentIndent write SetCurrentIndent;
    function    TabToSpaces(const ANumTabs: integer): string;
    procedure   IncTab(const ANum: integer = 1);
    procedure   DecTab;
    procedure   WriteLine(const AText: string; AList: TStringList);
    procedure   WriteBreak(AList: TStringList);
    property    Project: TMapProject read FProject write SetProject;
  public
    procedure   WriteProject(const ADirectory: String); overload; virtual; abstract;
    procedure   WriteProject(const ADirectory: string; ASL: TStringList); overload; virtual; abstract;
    // Events
    property    OnWriteClass: TOnWriteClassIntf read FOnWriteClass write SetOnWriteClass;
    property    OnWriteUnit: TOnWriteUnit read FOnWriteUnit write SetOnWriteUnit;
    property    OnWriteEnum: TOnWriteEnum read FOnWriteEnum write SetOnWriteEnum;
    property    OnWriteMapping: TOnWriteMapping read FOnWriteMapping write SetOnWriteMapping;
    constructor Create(AProject: TMapProject); reintroduce; virtual;
    destructor  Destroy; override;
  end;

  {: TtiFilteredObjectList descendant with extra properties to facility custom searching/queries. }
  TtiMappedFilteredObjectList = class(TtiFilteredObjectList)
  private
    FEnumType: TEnumType;
    FObjClass: TtiObjectClass;
    FParams: TSelectParamList;
    FSQL: String;
    procedure SetEnumType(const AValue: TEnumType);
    procedure SetObjClass(const AValue: TtiObjectClass);
    procedure SetSQL(const AValue: String);
  public
    //function IsValid(const AErrors: TtiObjectErrors): boolean; overload; override;
    property    SQL: String read FSQL write SetSQL;
    property    ObjClass: TtiObjectClass read FObjClass write SetObjClass;
    property    EnumType: TEnumType read FEnumType write SetEnumType;
    property    Params: TSelectParamList read FParams;
    procedure   AddParam(const AName: string; const ASQLParamName: string; AParamType: TMapPropType; AValue: Variant);
    constructor Create; override;
    destructor  Destroy; override;
  end;

  {: List read Visitor with parameters for use as a generic list visitor.}
  TtiMapParameterListReadVisitor = class(TtiVisitorSelect)
  private
    FClassDef: TMapClassDef;
    FEnumType: TEnumType;
    FObjClass: TtiObjectClass;
    FSQL: string;
    procedure SetClassDef(const AValue: TMapClassDef);
    procedure SetEnumType(const AValue: TEnumType);
    procedure SetObjClass(const AValue: TtiObjectClass);
    procedure SetSQL(const AValue: string);
  protected
    FParams: TSelectParamList;
    procedure   Init; override;
    procedure   SetupParams; override;
  public
    property    SQL: string read FSQL write SetSQL;
    property    ClassDef: TMapClassDef read FClassDef write SetClassDef;
    property    ObjClass: TtiObjectClass read FObjClass write SetObjClass;
    property    EnumType: TEnumType read FEnumType write SetEnumType;

    constructor Create; override;
    destructor  Destroy; override;
  end;

  {: Class for creating validator error strings. }
  TValidatorStringGenerator = class(TtiObject)
  public
    class function GetValueAsString(AObject: TtiObject; const APropName: string; AValue: Variant): string;
    class function CreateRequiredValidatorMsg(AObject: TtiObject; const APropName: string): string; virtual;
    class function CreateGreaterValidatorMsg(AObject: TtiObject; const APropName: string;
      AValue: Variant): string; virtual;
    class function CreateGreaterOrEqualValidatorMsg(AObject: TtiObject; const APropName: string;
      AValue: Variant): string; virtual;
    class function CreateLessThanValidatorMsg(AObject: TtiObject; const APropName: string;
      AValue: Variant): string; virtual;
    class function CreateLessThanOrEqualValidatorMsg(AObject: TtiObject; const APropName: string;
      AValue: Variant): string; virtual;
    class function CreateNotEqualToValidatorMsg(AObject: TtiObject; const APropName: string;
      AValue: Variant): string; virtual;
  end;

  {: Class of  }
  TValidatorStringGeneratorClass = class of TValidatorStringGenerator;


  procedure RegisterMappings;

  procedure gSetSchemaReaderClass(const AClass: TMapSchemaReaderClass);
  function  gGetSchemaReaderClass: TMapSchemaReaderClass;
  function  gStrToClassDefType(const AString: String): TClassDefType;
  function  gStrToPropType(const AString: string): TMapPropType;
  function  gPropTypeToStr(const APropType: TMapPropType): string;
  function  gFindAttrMap(const AClassName: string; const AAttrName: string): TtiAttrColMap;
  function  gStrToValType(const AString: string): TValidatorType;
  function  gValTypeToStr(const AValType: TValidatorType): string;
  function  GetabsolutePath(Source, Relative: string): string;

  // -----------------------------------------------------------------
  //  Glob vars
  // -----------------------------------------------------------------

var
  ValidatorStringClass: TValidatorStringGeneratorClass;

implementation

var
  mSchemaReaderClass: TMapSchemaReaderClass;


function GetappearNum(sub, st: string): integer;
var
    i: integer;
    P: integer;
begin

    p := Pos(sub, st);
    I := 0;
    while p > 0 do
    begin
        inc(i);
        delete(st, 1, p + length(sub) - 1);
        p := Pos(sub, st);
    end;
    result := i;
end;



function decomposestr(sub, st: string; var tst: TStringList): Boolean;
var
    num: integer;
    P: integer;

begin
    p := Pos(sub, st);
    tst.Clear;
    while p > 0 do
    begin
        num := p + length(sub) - 1;
        tst.Add(copy(st, 1, num));
        delete(st, 1, num);
        p := Pos(sub, st);
    end;
    tst.Add(st);
    Result := True;

end;



function CopyLeftNum(sub, st: string; num: integer): string;
var
    Tst: TStringList;
    I: integer;
begin
    tst := TStringList.Create;
    decomposestr(sub, st, Tst);
    if Num >= Tst.Count then
        Result := st
    else
    begin
        for i := 0 to num - 1 do
        begin
            Result := Result + Tst[i];
        end;
    end;
    Tst.Free;
end;


function CopyRightNum(sub, st: string; Num: integer): string;
var
    Tst: TStringList;
    I: integer;
begin
    Tst := TStringList.Create;
    try
        decomposestr(sub, st, Tst);
        Result := '';
        if Num < Tst.Count then
        begin
            for i := Tst.Count - Num to Tst.Count - 1 do
            begin
                Result := Result + Tst[i]
            end;
        end;
    finally
        Tst.Free;
    end;
end;

function gValTypeToStr(const AValType: TValidatorType): string;
begin

  case AValType of
    vtRequired: result := 'required';
    vtGreater: result := 'greater';
    vtGreaterEqual: result := 'greater-equal';
    vtLess: result := 'less';
    vtLessEqual: result := 'less-equal';
    vtNotEqual: result := 'not-equal';
    vtRegExp: result := 'reg-exp';
  else
    Raise Exception.Create('gValTypeToStr: Value out of range');
  end;
end;

function GetabsolutePath(Source, Relative: string): string;
var
    i, Num, num1: integer;
    St: TStringList;
    s: string;
begin
    Num := GetappearNum('..', Relative);
    St := TStringList.Create;
    decomposestr(PathDelim, ExcludeTrailingBackslash(Source), st);
    Num1 := st.Count;

    Result := '';

    for i := 0 to num1 - num - 1 do
    begin
        Result := Result + st[i];
    end;

    if Pos('\', Relative) > 0 then
      s := CopyRightNum('..\', Relative, 1)
    else
      s := CopyRightNum('../', Relative, 1);

    Result := Result + s;
    st.Free;
end;

procedure RegisterMappings;
begin

end;

procedure gSetSchemaReaderClass(const AClass: TMapSchemaReaderClass);
begin
  mSchemaReaderClass := AClass;
end;

function gGetSchemaReaderClass: TMapSchemaReaderClass;
begin
  result := mSchemaReaderClass;
end;

function gStrToClassDefType(const AString: String): TClassDefType;
begin
  if LowerCase(AString) = 'create' then
    result := dtCreate
  else if LowerCase(AString) = 'reference' then
    result := dtReference
  else
    Raise Exception.Create('gStrToClassDefType: invalid parameter');
end;

function gStrToPropType(const AString: string): TMapPropType;
var
  lType: string;
begin
  lType := LowerCase(AString);

  if lType = 'string' then
    result := ptString
  else if lType = 'integer' then
    result := ptInteger
  else if lType = 'int64' then
    result := ptInt64
  else if lType = 'ansistring' then
    result := ptAnsiString
  else if lType = 'tdatetime' then
    result := ptDateTime
  else if lType = 'boolean' then
    result := ptBoolean
  else if lType = 'enum' then
    result := ptEnum
  else if lType = 'currency' then
    result := ptCurrency
  else if lType = 'double' then
    Result := ptCurrency
  else if lType = 'single' then
    result := ptSingle
  else
    raise Exception.Create('gStrToPropType: Invalid parameter: ' + AString);

end;

function gPropTypeToStr(const APropType: TMapPropType): string;
begin
  case APropType of
    ptString: result := 'String';
    ptInt64: result := 'Int64';
    ptInteger: result := 'Integer';
    ptAnsiString: result := 'AnsiString';
    ptBoolean: result := 'Boolean';
    ptDateTime: Result := 'TDateTime';
    ptSingle: result := 'Single';
    ptDouble: result := 'Double';
    ptCurrency: result := 'Currency';
    ptEnum: result := 'enum';
  end;
end;

function gFindAttrMap(const AClassName: string; const AAttrName: string): TtiAttrColMap;
var
  lMapList: TtiAttrColMaps;
  lMap: TtiAttrColMap;
  lCtr: integer;
begin

  result := nil;

  lMapList := GTIOPFManager.ClassDBMappingMgr.AttrColMaps;
  for lCtr := 0 to lMapList.Count -1 do
    begin
      lMap := lMapList.Items[lCtr];
      if (lMap.AttrMap.Owner.PerObjAbsClass.ClassName = AClassName) and
        (SameText(lMap.AttrMap.AttrName, AAttrName)) then
        begin
          result := lMap;
          exit;
        end;
    end;
end;

function gStrToValType(const AString: string): TValidatorType;
var
  lStr: string;
begin
  lStr := LowerCase(AString);

  if lStr = 'required' then
    result := vtRequired
  else if lStr = 'greater' then
    result := vtGreater
  else if lStr = 'greater-equal' then
    result := vtGreaterEqual
  else if lStr = 'less' then
    result := vtLess
  else if lStr = 'less-equal' then
    result := vtLessEqual
  else if lStr = 'not-equal' then
    result := vtNotEqual
  else if lStr = 'req-exp' then
    result := vtRegExp
  else
    Raise Exception.Create('gStrToValType: Value out of range');
end;

{ TMapProject }

procedure TMapProject.ClearAll;
begin
  FIncludes.Clear;
  FUnits.Clear;
  ProjectClasses.Clear;
  FProjectEnums.Clear;
end;

constructor TMapProject.Create;
begin
  inherited Create;
  FIncludes := TStringList.Create;
  FUnits := TMapUnitDefList.Create;
  FProjectClasses := TMapClassDefList.Create;
  FProjectClasses.OwnsObjects := false;
  FProjectEnums := TMapEnumList.Create;
  FProjectEnums.OwnsObjects := false;
  FMaxEditorCodeWidth := 80;
end;

destructor TMapProject.Destroy;
begin
  FIncludes.Free;
  FUnits.Free;
  FProjectClasses.Free;
  FProjectEnums.Free;
  inherited Destroy;
end;

function TMapProject.FindEnumForPropName(const AUnitName: string; const AClassName: string;
  const APropName: string): TMapEnum;
var
  lProp: TMapClassProp;
  lClassDef: TMapClassDef;
  lPropDef: TMapClassProp;
  lUnit: TMapUnitDef;
begin
  lUnit := Units.FindByName(AUnitName);
  lClassDef := lUnit.UnitClasses.FindByName(AClassName);
  lPropDef := lClassDef.ClassProps.FindByName(APropName);
  result := lUnit.UnitEnums.FindByName(lProp.PropTypeName);
end;

function TMapProject.HasCustomSelects: boolean;
var
  lClassCtr, lUnitCtr: integer;
  lClassMap: TMapClassDef;
  lUnit: TMapUnitDef;
  lClassDef: TMapClassDef;
begin
  result := false;

  for lUnitCtr := 0 to Units.Count - 1 do
    begin
      lUnit := Units.Items[lUnitCtr];
      for lClassCtr := 0 to lUnit.UnitClasses.Count - 1 do
        begin
          lClassDef := lUnit.UnitClasses.Items[lClassCtr];
          if lClassDef.Selections.Count > 0 then
            begin
              result := true;
              exit;
            end;
        end;
    end;
end;

procedure TMapProject.SetBaseDirectory(const AValue: string);
begin
  if FBaseDirectory=AValue then exit;
  FBaseDirectory:=AValue;
end;

procedure TMapProject.SetBeginEndTabs(const AValue: integer);
begin
  if FBeginEndTabs=AValue then exit;
  FBeginEndTabs:=AValue;
end;

procedure TMapProject.SetEnumType(const AValue: TEnumType);
begin
  if FEnumType=AValue then exit;
  FEnumType:=AValue;
end;

procedure TMapProject.SetFileName(const AValue: string);
begin
  if FFileName=AValue then exit;
  FFileName:=AValue;
end;

procedure TMapProject.SetMaxEditorCodeWidth(const AValue: integer);
begin
  if FMaxEditorCodeWidth=AValue then exit;
  FMaxEditorCodeWidth:=AValue;
end;

procedure TMapProject.SetOrigOutDirectory(const AValue: string);
begin
  if FOrigOutDirectory=AValue then exit;
  FOrigOutDirectory:=AValue;
end;

procedure TMapProject.SetOutputDirectory(const AValue: string);
begin
  if FOutputDirectory=AValue then exit;
  FOutputDirectory:=AValue;
end;

procedure TMapProject.SetProjectClasses(const AValue: TMapClassDefList);
begin
  if FProjectClasses=AValue then exit;
  FProjectClasses:=AValue;
end;

procedure TMapProject.SetProjectName(const AValue: string);
begin
  if FProjectName=AValue then exit;
  FProjectName:=AValue;
end;

procedure TMapProject.SetTabSpaces(const AValue: integer);
begin
  if FTabSpaces=AValue then exit;
  FTabSpaces:=AValue;
end;

procedure TMapProject.SetVisibilityTabs(const AValue: integer);
begin
  if FVisibilityTabs=AValue then exit;
  FVisibilityTabs:=AValue;
end;


{ TBaseMapObjectList }

function TBaseMapObjectList.Add(AObject: TBaseMapObject): Integer;
begin
  inherited Add(AObject);
end;

function TBaseMapObjectList.GetItems(i: Integer): TBaseMapObject;
begin
  result := inherited GetItems(i) as TBaseMapObject;
end;

procedure TBaseMapObjectList.SetItems(i: Integer; AObject: TBaseMapObject);
begin
  inherited SetItems(i, AObject);
end;

{ TMapProjectList }

function TMapProjectList.Add(AObject: TMapProject): Integer;
begin
  result := inherited Add(AObject);
end;

function TMapProjectList.GetItems(i: Integer): TMapProject;
begin
  result := inherited GetItems(i) as TMapProject;
end;

procedure TMapProjectList.SetItems(i: Integer; AObject: TMapProject);
begin
  inherited SetItems(i, AObject);
end;

{ TMapConnectionDef }

constructor TMapConnectionDef.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TMapConnectionDef.Destroy;
begin
  FParams.free;
  inherited Destroy;
end;

procedure TMapConnectionDef.SetConnType(const AValue: string);
begin
  if FConnType=AValue then exit;
  FConnType:=AValue;
end;

procedure TMapConnectionDef.SetDataSource(const AValue: string);
begin
  if FDataSource=AValue then exit;
  FDataSource:=AValue;
end;

procedure TMapConnectionDef.SetHost(const AValue: string);
begin
  if FHost=AValue then exit;
  FHost:=AValue;
end;

{ TMapConnectionDefList }

function TMapConnectionDefList.Add(AObject: TMapConnectionDef): Integer;
begin
  result := inherited Add(AObject);
end;

function TMapConnectionDefList.GetItems(i: Integer): TMapConnectionDef;
begin
  result := TMapConnectionDef(inherited GetItems(i));
end;

procedure TMapConnectionDefList.SetItems(i: Integer;
  AObject: TMapConnectionDef);
begin
  inherited SetItems(i, AObject);
end;

{ TMapUnitDef }

constructor TMapUnitDef.Create;
begin
  inherited Create;
  FUnitClasses := TMapClassDefList.Create;
  FUnitEnums := TMapEnumList.Create;
  FReferences := TStringList.create;

end;

destructor TMapUnitDef.Destroy;
begin
  FUnitClasses.Free;
  FUnitEnums.Free;
  FReferences.Free;
  inherited Destroy;
end;

function TMapUnitDef.HasValidators: boolean;
var
  lClassCtr: integer;
begin
  result := false;

  for lClassCtr := 0 to UnitClasses.Count - 1 do
    begin
      if UnitClasses.Items[lClassCtr].Validators.Count > 0 then
        begin
          result := true;
          exit;
        end;
    end;
end;

procedure TMapUnitDef.SetUnitName(const AValue: string);
begin
  if FUnitName=AValue then exit;
  FUnitName:=AValue;
end;

{ TMapEnumValue }

constructor TMapEnumValue.Create;
begin
  inherited Create;
  FEnumValue := -1;
end;

destructor TMapEnumValue.Destroy;
begin
  inherited Destroy;
end;

procedure TMapEnumValue.SetEnumValue(const AValue: integer);
begin
  if FEnumValue=AValue then exit;
  FEnumValue:=AValue;
end;

procedure TMapEnumValue.SetEnumValueName(const AValue: string);
begin
  if FEnumValueName=AValue then exit;
  FEnumValueName:=AValue;
end;

{ TMapEnumValueList }

function TMapEnumValueList.Add(const AName: string; const AValue: integer
  ): integer;
var
  lValue: TMapEnumValue;
begin
  lValue := TMapEnumValue.Create;
  lValue.EnumValue := AValue;
  lValue.EnumValueName := AName;
  result := Self.Add(lValue);
end;

function TMapEnumValueList.Add(AObject: TMapEnumValue): Integer;
begin
  result := inherited Add(AObject);
end;

function TMapEnumValueList.GetItems(i: Integer): TMapEnumValue;
begin
  result := TMapEnumValue(inherited GetItems(i));
end;

procedure TMapEnumValueList.SetItems(i: Integer; AObject: TMapEnumValue);
begin
  inherited SetItems(i, AObject);
end;

{ TMapEnum }

constructor TMapEnum.Create;
begin
  inherited Create;
  FValues := TMapEnumValueList.create;
end;

destructor TMapEnum.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

procedure TMapEnum.SetEnumName(const AValue: string);
begin
  if FEnumName=AValue then exit;
  FEnumName:=AValue;
end;

procedure TMapEnum.SetValues(const AValue: TMapEnumValueList);
begin
  if FValues=AValue then exit;
  FValues:=AValue;
end;

{ TMapClassDef }

constructor TMapClassDef.Create;
begin
  inherited Create;
  FClassProps := TMapClassPropList.Create;
  FClassMapping := TClassMapping.Create;
  FSelections := TClassMappingSelectList.Create;
  FValidators := TMapValidatorList.Create;

end;

destructor TMapClassDef.Destroy;
begin
  FClassProps.Free;
  FClassMapping.free;
  FSelections.Free;
  FValidators.Free;
  inherited Destroy;
end;

procedure TMapClassDef.SetAutoCreateBase(const AValue: boolean);
begin
  if FAutoCreateBase=AValue then exit;
  FAutoCreateBase:=AValue;
end;

procedure TMapClassDef.SetAutoCreateListClass(const AValue: boolean);
begin
  if FAutoCreateListClass=AValue then exit;
  FAutoCreateListClass:=AValue;
end;

procedure TMapClassDef.SetAutoMap(const AValue: boolean);
begin
  if FAutoMap=AValue then exit;
  FAutoMap:=AValue;
end;

procedure TMapClassDef.SetBaseClassName(const AValue: string);
begin
  if FBaseClassName=AValue then exit;
  FBaseClassName:=AValue;
end;

procedure TMapClassDef.SetBaseClassParent(const AValue: string);
begin
  if FBaseClassParent=AValue then exit;
  FBaseClassParent:=AValue;
end;

procedure TMapClassDef.SetBaseUnitName(const AValue: string);
begin
  if FBaseUnitName=AValue then exit;
  FBaseUnitName:=AValue;
end;

procedure TMapClassDef.SetClassProps(const AValue: TMapClassPropList);
begin
  if FClassProps=AValue then exit;
  FClassProps:=AValue;
end;

procedure TMapClassDef.SetCrud(const AValue: string);
begin
  if FCrud=AValue then exit;
  FCrud:=AValue;
end;

procedure TMapClassDef.SetDefType(const AValue: TClassDefType);
begin
  if FDefType=AValue then exit;
  FDefType:=AValue;
end;

procedure TMapClassDef.SetForwardDeclare(const AValue: boolean);
begin
  if FForwardDeclare=AValue then exit;
  FForwardDeclare:=AValue;
end;

procedure TMapClassDef.SetORMClassName(const AValue: string);
begin
  if FORMClassName=AValue then exit;
  FORMClassName:=AValue;
end;

{ TMapClassDefList }

function TMapClassDefList.Add(AObject: TMapClassDef): Integer;
begin
  result := inherited Add(AObject);
end;

function TMapClassDefList.FindByName(const AName: string): TMapClassDef;
var
  lCtr: integer;
  lName: string;
begin
  result := nil;

  lName := LowerCase(AName);

  for lCtr := 0 to Count - 1 do
    begin
      if LowerCase(Items[lCtr].BaseClassName) = lName then
        begin
          result := Items[lCtr];
          exit;
        end;
    end;
end;

function TMapClassDefList.GetItems(i: Integer): TMapClassDef;
begin
  result := TMapClassDef(inherited GetItems(i));
end;

procedure TMapClassDefList.SetItems(i: Integer; AObject: TMapClassDef);
begin
  inherited SetItems(i, AObject);
end;

{ TMapClassProp }

procedure TMapClassProp.SetIsReadOnly(const AValue: boolean);
begin
  if FIsReadOnly=AValue then exit;
  FIsReadOnly:=AValue;
end;

procedure TMapClassProp.SetPropName(const AValue: string);
begin
  if FPropName=AValue then exit;
  FPropName:=AValue;
end;

procedure TMapClassProp.SetPropType(const AValue: TMapPropType);
begin
  if FPropType=AValue then exit;
  FPropType:=AValue;
end;

procedure TMapClassProp.SetPropTypeName(const AValue: string);
begin
  if FPropTypeName=AValue then exit;
  FPropTypeName:=AValue;
end;

{ TMapClassPropList }

function TMapClassPropList.Add(const AName: string;
  const APropType: TMapPropType): integer;
var
  lProp: TMapClassProp;
begin
  lProp := TMapClassProp.create;
  lProp.PropName := AName;
  lProp.PropType := APropType;
  result := self.Add(lProp);
end;

function TMapClassPropList.Add(AObject: TMapClassProp): Integer;
begin
  result := inherited Add(AObject);
end;

function TMapClassPropList.FindByName(const AName: string): TMapClassProp;
var
  lCtr: Integer;
begin
  result := nil;

  for lCtr := 0 to Count - 1 do
    begin
      if LowerCase(Items[lCtr].PropName) = LowerCase(AName) then
        begin
          result := Items[lCtr];
          exit;
        end;
    end;
end;

function TMapClassPropList.GetItems(i: Integer): TMapClassProp;
begin
  result := TMapClassProp(inherited GetItems(i));
end;

procedure TMapClassPropList.SetItems(i: Integer; AObject: TMapClassProp);
begin
  inherited SetItems(i, AObject);
end;

{ TClassMapping }

constructor TClassMapping.Create;
begin
  inherited Create;
  FPropMappings := TPropMappingList.Create;
end;

destructor TClassMapping.Destroy;
begin
  FPropMappings.Free;
  inherited Destroy;
end;

procedure TClassMapping.SetOIDType(const AValue: TOIDType);
begin
  if FOIDType=AValue then exit;
  FOIDType:=AValue;
end;

procedure TClassMapping.SetPKField(const AValue: string);
begin
  if FPKField=AValue then exit;
  FPKField:=AValue;
end;

procedure TClassMapping.SetPKName(const AValue: string);
begin
  if FPKName=AValue then exit;
  FPKName:=AValue;
end;

procedure TClassMapping.SetTableName(const AValue: string);
begin
  if FTableName=AValue then exit;
  FTableName:=AValue;
end;

{ TClassMappingList }

function TClassMappingList.Add(AObject: TClassMapping): Integer;
begin
  result := inherited Add(AObject);
end;

function TClassMappingList.GetItems(i: Integer): TClassMapping;
begin
  result := TClassMapping(inherited GetItems(i));
end;

procedure TClassMappingList.SetItems(i: Integer; AObject: TClassMapping);
begin
  inherited SetItems(i, AObject);
end;

{ TPropMapping }

procedure TPropMapping.SetFieldName(const AValue: string);
begin
  if FFieldName=AValue then exit;
  FFieldName:=AValue;
end;

procedure TPropMapping.SetPropName(const AValue: string);
begin
  if FPropName=AValue then exit;
  FPropName:=AValue;
end;

procedure TPropMapping.SetPropType(const AValue: TMapPropType);
begin
  if FPropType=AValue then exit;
  FPropType:=AValue;
end;

{ TPropMappingList }

function TPropMappingList.Add(AObject: TPropMapping): Integer;
begin
  result := inherited Add(AObject);
end;

function TPropMappingList.GetItems(i: Integer): TPropMapping;
begin
  result := TPropMapping(inherited GetItems(i));
end;

procedure TPropMappingList.SetItems(i: Integer; AObject: TPropMapping);
begin
  inherited SetItems(i, AObject);
end;

{ TFilterDef }

procedure TFilterDef.SetField(const AValue: String);
begin
  if FField=AValue then exit;
  FField:=AValue;
end;

procedure TFilterDef.SetFilterType(const AValue: TFilterType);
begin
  if FFilterType=AValue then exit;
  FFilterType:=AValue;
end;

{ TFilterDefList }

function TFilterDefList.Add(AObject: TFilterDef): Integer;
begin
  result := inherited Add(AObject);
end;

function TFilterDefList.GetItems(i: Integer): TFilterDef;
begin
  result := TFilterDef(inherited GetItems(i));
end;

procedure TFilterDefList.SetItems(i: Integer; AObject: TFilterDef);
begin
  inherited SetItems(i, AObject);
end;

{ TMapEnumList }

function TMapEnumList.Add(AObject: TMapEnum): Integer;
begin
  result := inherited Add(AObject);
end;

function TMapEnumList.FindByName(const AName: string): TMapEnum;
var
  lCtr: integer;
  lName: string;
begin
  result := nil;

  lName := LowerCase(AName);

  for lCtr := 0 to Count - 1 do
    begin
      if LowerCase(Items[lCtr].EnumName) = lName then
        begin
          result := Items[lCtr];
          exit;
        end;
    end;
end;

function TMapEnumList.GetItems(i: Integer): TMapEnum;
begin
  result := TMapEnum(inherited GetItems(i));
end;

procedure TMapEnumList.SetItems(i: Integer; AObject: TMapEnum);
begin
  inherited SetItems(i, AObject);
end;

{ TMapUnitDefList }

function TMapUnitDefList.Add(AObject: TMapUnitDef): Integer;
begin
  result := inherited Add(AObject);
end;

function TMapUnitDefList.FindByName(const AName: string): TMapUnitDef;
var
  lCtr: integer;
  lName: string;
begin
  result := nil;

  lName := LowerCase(AName);

  for lCtr := 0 to Count - 1 do
    begin
      if LowerCase(Items[lCtr].UnitName) = lName then
        begin
          result := Items[lCtr];
          exit;
        end;
    end;
end;

function TMapUnitDefList.GetItems(i: Integer): TMapUnitDef;
begin
  result := TMapUnitDef(inherited GetItems(i));
end;

procedure TMapUnitDefList.SetItems(i: Integer; AObject: TMapUnitDef);
begin
  inherited SetItems(i, AObject);
end;

{ TMapSchemaWriter }

constructor TMapSchemaWriter.Create(AProject: TMapProject);
begin
  inherited Create;
  FProject := AProject;
end;

procedure TMapSchemaWriter.DecTab;
begin
  IncTab(-1);
end;

destructor TMapSchemaWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TMapSchemaWriter.IncTab(const ANum: integer);
begin
  FCurrentIndent := (FCurrentIndent + ANum);
end;

procedure TMapSchemaWriter.SetCurrentIndent(const AValue: integer);
begin
  if FCurrentIndent=AValue then exit;
  FCurrentIndent:=AValue;
end;

procedure TMapSchemaWriter.SetOnWriteClass(const AValue: TOnWriteClassIntf);
begin
  FOnWriteClass:=AValue;
end;

procedure TMapSchemaWriter.SetOnWriteEnum(const AValue: TOnWriteEnum);
begin
  FOnWriteEnum:=AValue;
end;

procedure TMapSchemaWriter.SetOnWriteMapping(const AValue: TOnWriteMapping);
begin
  FOnWriteMapping:=AValue;
end;

procedure TMapSchemaWriter.SetOnWriteUnit(const AValue: TOnWriteUnit);
begin
  FOnWriteUnit:=AValue;
end;

procedure TMapSchemaWriter.SetProject(const AValue: TMapProject);
begin
  FProject:=AValue;
end;

function TMapSchemaWriter.TabToSpaces(const ANumTabs: integer): string;
begin
  result := StringOfChar(' ', ANumTabs * Project.TabSpaces);
end;

procedure TMapSchemaWriter.WriteLine(const AText: string; AList: TStringList);
var
  lPrefix: string;
begin
  lPrefix := TabToSpaces(CurrentIndent);
  AList.Add(lPrefix + AText);
end;

procedure TMapSchemaWriter.WriteBreak(AList: TStringList);
begin
  WriteLine('', AList);
end;

{ TSelectParam }

procedure TSelectParam.SetParamName(const AValue: string);
begin
  if FParamName=AValue then exit;
  FParamName:=AValue;
end;

procedure TSelectParam.SetParamType(const AValue: TMapPropType);
begin
  if FParamType=AValue then exit;
  FParamType:=AValue;
end;

procedure TSelectParam.SetParamTypeName(const AValue: string);
begin
  if FParamTypeName=AValue then exit;
  FParamTypeName:=AValue;
end;

procedure TSelectParam.SetPassBy(const AValue: string);
begin
  if FPassBy=AValue then exit;
  FPassBy:=AValue;
end;

procedure TSelectParam.SetSQLParamName(const AValue: string);
begin
  if FSQLParamName=AValue then exit;
  FSQLParamName:=AValue;
end;

procedure TSelectParam.SetTypeName(const AValue: string);
begin
  if FTypeName=AValue then exit;
  FTypeName:=AValue;
end;

procedure TSelectParam.SetValue(const AValue: Variant);
begin
  if FValue=AValue then exit;
  FValue:=AValue;
end;

{ TSelectParamList }

function TSelectParamList.Add(AObject: TSelectParam): Integer;
begin
  result := inherited Add(AObject);
end;

function TSelectParamList.FindByName(const AName: string): TSelectParam;
var
  lCtr: Integer;
begin
  result := nil;

  for lCtr := 0 to Count - 1 do
    begin
      if LowerCase(Items[lCtr].ParamName) = LowerCase(AName) then
        begin
          result := Items[lCtr];
          exit;
        end;
    end;
end;

function TSelectParamList.GetItems(i: Integer): TSelectParam;
begin
  result := TSelectParam(inherited GetItems(i));
end;

procedure TSelectParamList.SetItems(i: Integer; AObject: TSelectParam);
begin
  inherited SetItems(i, AObject);
end;

{ TClassMappingSelect }

constructor TClassMappingSelect.Create;
begin
  inherited Create;
  FSQL := TStringList.Create;
  FParams := TSelectParamList.Create;
end;

destructor TClassMappingSelect.Destroy;
begin
  FSQL.Free;
  FParams.Free;
  inherited Destroy;
end;

function TClassMappingSelect.GetCaption: string;
var
  lCtr: integer;
  lParams: string;
  lPar: TSelectParam;
begin
  lParams := '';
  result := FName + '(';
  for lCtr := 0 to FParams.Count - 1 do
    begin
      lPar := FParams.Items[lCtr];
      if lParams = '' then
        lParams := lPar.ParamName + ': ' + lPar.ParamTypeName
      else
        lParams := lParams + ', ' + lPar.ParamName + ':' + lPar.ParamTypeName;
    end;
  result := result + lParams + ')';

end;

procedure TClassMappingSelect.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

{ TClassMappingSelectList }

function TClassMappingSelectList.Add(AObject: TClassMappingSelect): Integer;
begin
  result := inherited Add(AObject);
end;

function TClassMappingSelectList.GetItems(i: Integer): TClassMappingSelect;
begin
  result := TClassMappingSelect(inherited GetItems(i));
end;

procedure TClassMappingSelectList.SetItems(i: Integer;
  AObject: TClassMappingSelect);
begin
  inherited SetItems(i, AObject);
end;

{ TtiMapParameterListReadVisitor }

constructor TtiMapParameterListReadVisitor.Create;
begin
  FParams := TSelectParamList.Create;
  inherited Create;
end;

destructor TtiMapParameterListReadVisitor.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TtiMapParameterListReadVisitor.Init;
begin
  Query.SQLText := TtiMappedFilteredObjectList(Visited).SQL;
  Query.SQL.SaveToFile('sql.txt');
end;

procedure TtiMapParameterListReadVisitor.SetClassDef(const AValue: TMapClassDef);
begin
  if FClassDef=AValue then exit;
  FClassDef:=AValue;
end;

procedure TtiMapParameterListReadVisitor.SetEnumType(const AValue: TEnumType);
begin
  if FEnumType=AValue then exit;
  FEnumType:=AValue;
end;

procedure TtiMapParameterListReadVisitor.SetObjClass(const AValue: TtiObjectClass
  );
begin
  if FObjClass=AValue then exit;
  FObjClass:=AValue;
end;

procedure TtiMapParameterListReadVisitor.SetSQL(const AValue: string);
begin
  if FSQL=AValue then exit;
  FSQL:=AValue;
end;

procedure TtiMapParameterListReadVisitor.SetupParams;
var
  lCtr: integer;
  lParam: TSelectParam;
  lList: TtiMappedFilteredObjectList;
  lProp: TMapClassProp;
begin

  inherited SetupParams;
  exit;

  lList := TtiMappedFilteredObjectList(Visited);

  for lCtr := 0 to lList.Params.Count - 1 do
    begin
      lParam := lList.Params.Items[lCtr];

      case lParam.ParamType of
        ptString, ptAnsiString:
          Query.ParamAsString[lParam.SQLParamName] := lParam.Value;
        ptBoolean:
          Query.ParamAsBoolean[lParam.SQLParamName] := lParam.Value;
        ptDateTime:
          Query.ParamAsDateTime[lParam.SQLParamName] := lParam.Value;
        ptDouble, ptCurrency, ptSingle:
          Query.ParamAsFloat[lParam.SQLParamName] := lParam.Value;
        ptInt64, ptInteger:
          Query.ParamAsInteger[lParam.SQLParamName] := lParam.Value;
        ptEnum:
          begin
            if lList.EnumType = etString then
              begin
                Query.ParamAsString[lParam.SQLParamName] := GetEnumName(TypeInfo(TMapPropType), Integer(lParam.Value));
              end
            else
              Query.ParamAsInteger[lParam.SQLParamName] := Integer(lParam.Value);
          end;
      end;
    end;
end;

{ TtiMappedFilteredObjectList }

procedure TtiMappedFilteredObjectList.AddParam(const AName: string; const ASQLParamName: string;
  AParamType: TMapPropType; AValue: Variant);
var
  lParam: TSelectParam;
begin
  lParam := FParams.FindByName(Aname);
  if lParam <> nil then
    exit;

  lParam := TSelectParam.Create;
  lParam.ParamName := AName;
  lParam.ParamType := AParamType;
  lParam.SQLParamName := ASQLParamName;
  lParam.Value := AValue;
  FParams.Add(lParam);
end;

constructor TtiMappedFilteredObjectList.Create;
begin
  inherited Create;
  FParams := TSelectParamList.Create;
end;

destructor TtiMappedFilteredObjectList.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TtiMappedFilteredObjectList.SetEnumType(const AValue: TEnumType);
begin
  if FEnumType=AValue then exit;
  FEnumType:=AValue;
end;

procedure TtiMappedFilteredObjectList.SetObjClass(const AValue: TtiObjectClass
  );
begin
  if FObjClass=AValue then exit;
  FObjClass:=AValue;
end;

procedure TtiMappedFilteredObjectList.SetSQL(const AValue: String);
begin
  if FSQL=AValue then exit;
  FSQL:=AValue;
end;

{ TMapValidator }

procedure TMapValidator.SetClassProp(const AValue: string);
begin
  if FClassProp=AValue then exit;
  FClassProp:=AValue;
end;

procedure TMapValidator.SetValidatorType(const AValue: TValidatorType);
begin
  if FValidatorType=AValue then exit;
  FValidatorType:=AValue;
end;

procedure TMapValidator.SetValue(const AValue: variant);
begin
  if FValue=AValue then exit;
  FValue:=AValue;
end;

{ TMapValidatorList }

procedure TMapValidatorList.Add(AObject: TMapValidator);
begin
  inherited Add(AObject);
end;

function TMapValidatorList.GetItems(i: integer): TMapValidator;
begin
  result := inherited GetItems(i) as TMapValidator;
end;

procedure TMapValidatorList.SetItems(i: integer; const AValue: TMapValidator);
begin
  inherited SetItems(i, AValue);
end;

{ TValidatorStringGenerator }
{
  TValidatorType = (vtRequired, vtGreater, vtGreaterEqual, vtLess, vtLessEqual,
    vtNotEqual, vtRegExp);
}


class function TValidatorStringGenerator.CreateGreaterOrEqualValidatorMsg(
  AObject: TtiObject; const APropName: string; AValue: Variant): string;
var
  lType: TtiTypeKind;
  lValue: string;
const
  MSG = 'Value of %s must be greater than or equal to ';
begin
  lType := tiGetSimplePropType(AObject, APropName);
  if lType = tiTKInteger then
    lValue := IntToStr(AValue)
  else
    lValue := formatFloat('#0.00', AValue);

  result := format(MSG, [APropName]) + lValue;

end;

class function TValidatorStringGenerator.CreateGreaterValidatorMsg(
  AObject: TtiObject; const APropName: string; AValue: Variant): string;
const
  MSG = 'Value of %s must be greater than %s.';
begin
  result := format(MSG, [APropName, GetValueAsString(AObject, APropName, AValue)]);
end;

class function TValidatorStringGenerator.CreateLessThanOrEqualValidatorMsg(
  AObject: TtiObject; const APropName: string; AValue: Variant): string;
const
  MSG = 'Value of %s must be greater than or equal to %s.';
begin
  result := format(MSG, [APropName, GetValueAsString(AObject, APropName, AValue)]);
end;

class function TValidatorStringGenerator.CreateLessThanValidatorMsg(
  AObject: TtiObject; const APropName: string; AValue: Variant): string;
const
  MSG = 'Value of %s must be less than or equal to %s.';
begin
  result := format(MSG, [APropName, GetValueAsString(AObject, APropName, AValue)]);
end;

class function TValidatorStringGenerator.CreateNotEqualToValidatorMsg(
  AObject: TtiObject; const APropName: string; AValue: Variant): string;
const
  MSG = 'Value of %s must not equal to %s.';
begin
  result := format(MSG, [APropName, GetValueAsString(AObject, APropName, AValue)]);
end;

class function TValidatorStringGenerator.CreateRequiredValidatorMsg(
  AObject: TtiObject; const APropName: string): string;
const
  MSG = '%s must have a value.';
begin
  result := format(MSG, [APropName]);
end;

class function TValidatorStringGenerator.GetValueAsString(AObject: TtiObject;
  const APropName: string; AValue: Variant): string;
var
  lType: TtiTypeKind;
  lValue: string;
begin
  lType := tiGetSimplePropType(AObject, APropName);

  case lType of
    tiTKInteger:
      result := IntToStr(AValue);
    tiTKBinary:
      result := BoolToStr(AValue, true);
    tiTKDateTime:
      result := DateToStr(AValue);
    tiTKFloat:
      result := FormatFloat('#0.00', AValue);
    tiTKString:
      result := AValue;
    else
      raise Exception.Create('Value out of range');
  end;
end;

initialization
  ValidatorStringClass := TValidatorStringGenerator;

end.

