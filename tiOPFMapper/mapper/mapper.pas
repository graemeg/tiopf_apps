unit mapper;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, contnrs, variants, tiObject, tiRTTI, tiAutoMap, typinfo,
  tiVisitorDB, tiOPFManager, tiFilteredObjectList;

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
  TMapPropType = (ptString, ptAnsiString, ptDouble, ptSingle, ptCurrency, ptInteger, ptInt64, ptDateTime, ptBoolean, ptEnum, ptEnumSet, ptStream);

  {: Type of class definition to create. }
  TClassDefType = (dtCreate, dtReference);

  {: Filter type to use when filtering objects. }
  TFilterType = (ftEqual, ftNotEqual, ftGreater, ftGreaterOrEqual, ftLess, ftLessOrEqual, ftLike, ftNotLike, ftStartingWith, ftNotEmpty);

  {: Indicates an objects state. }
  TORMObjectState = (osUnchanged, osChanged, osDeleted, osCreated);

  {: Indicates the state of an object list. }
  TORMListState = (lsEmpty, lsPopulated);

  {: Describes the type of OID to use for object. }
  TOIDType = (otString, otInt);

  {: Describes how enumeratated types should be handled. }
  TEnumType = (etInt, etString);

  {: Describes validators types. }
  TValidatorType = (vtRequired, vtGreater, vtGreaterEqual, vtLess, vtLessEqual, vtNotEqual, vtRegExp);


  // -----------------------------------------------------------------
  //  Method Objects
  // -----------------------------------------------------------------

  TGetValidatorErrorString = function(const AValType: TValidatorType): string;

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Base Mapper Object. }
  {$M+}
  TBaseMapObject = class(TtiObject)
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  end;
  {$M-}

  TBaseMapObjectList = class(TtiObjectList)
  protected
    function GetItems(i: Integer): TBaseMapObject; reintroduce;
    procedure SetItems(i: Integer; AObject: TBaseMapObject); reintroduce;
  public
    property Items[AIndex: Integer]: TBaseMapObject read GetItems write SetItems; default;
    function Add(AObject: TBaseMapObject): Integer; reintroduce;
  end;

  TBaseMapOptions = class(TBaseMapObject)
  private
    FGroupName: string;
    procedure SetGroupName(const Value: string);
  published
    property GroupName: string read FGroupName write SetGroupName;
  end;

  TBaseMapOptionsList = class(TBaseMapObjectList)

  end;

  TMapGeneralProjectOptions = class(TBaseMapOptions)
  private
    FFileName: string;
    FProjectName: string;
    FOrigOutDirectory: string;
    FBaseDirectory: string;
    procedure SetBaseDirectory(const Value: string);
    procedure SetOrigOutDirectory(const Value: string);
    procedure SetProjectName(const Value: string);
    function GetOutputDirectory: string;
  published
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property OrigOutDirectory: string read FOrigOutDirectory write SetOrigOutDirectory;
    property OutputDirectory: string read GetOutputDirectory;
    property ProjectName: string read FProjectName write SetProjectName;
  end;

  TMapCodeGenerationProjectOptions = class(TBaseMapOptions)
  private
    FBeginEndTabs: integer;
    FMaxEditorCodeWidth: integer;
    FTabSpaces: integer;
    FVisibilityTabs: integer;
    procedure SetBeginEndTabs(const Value: integer);
    procedure SetMaxEditorCodeWidth(const Value: integer);
    procedure SetTabSpaces(const Value: integer);
    procedure SetVisibilityTabs(const Value: integer);
  published
    property BeginEndTabs: integer read FBeginEndTabs write SetBeginEndTabs;
    property MaxEditorCodeWidth: integer read FMaxEditorCodeWidth write SetMaxEditorCodeWidth;
    property TabSpaces: integer read FTabSpaces write SetTabSpaces;
    property VisibilityTabs: integer read FVisibilityTabs write SetVisibilityTabs;
  end;

  TMapDatabaseProjectOptions = class(TBaseMapOptions)
  private
    FDoubleQuoteDBFieldNames: boolean;
    FEnumerationType: TEnumType;
    procedure SetDoubleQuoteDBFieldNames(const Value: boolean);
    procedure SetEnumerationType(const Value: TEnumType);
  published
    property DoubleQuoteDBFieldNames: boolean read FDoubleQuoteDBFieldNames write SetDoubleQuoteDBFieldNames;
    property EnumerationType: TEnumType read FEnumerationType write SetEnumerationType;
  end;


  {: Base project class. }
  TMapProject = class(TBaseMapObject)
  private
    FIncludes: TStringList;
    FFileName: string;
    FProjectClasses: TMapClassDefList;
    FProjectEnums: TMapEnumList;
    FUnits: TMapUnitDefList;
    FGeneralOptions: TMapGeneralProjectOptions;
    FCodeGenerationOptions: TMapCodeGenerationProjectOptions;
    FDatabaseOptions: TMapDatabaseProjectOptions;
    procedure SetFileName(const AValue: string);
    procedure SetProjectClasses(const AValue: TMapClassDefList);
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function FindEnumForPropName(const AUnitName: string; const AClassName: string; const APropName: string): TMapEnum;
    function HasCustomSelects: boolean;
    procedure ClearAll;
  published
    property CodeGenerationOptions: TMapCodeGenerationProjectOptions read FCodeGenerationOptions;
    property DatabaseOptions: TMapDatabaseProjectOptions read FDatabaseOptions;
    property FileName: string read FFileName write SetFileName;
    property GeneralOptions: TMapGeneralProjectOptions read FGeneralOptions;
    property Includes: TStringList read FIncludes;
    property ProjectClasses: TMapClassDefList read FProjectClasses write SetProjectClasses;
    property ProjectEnums: TMapEnumList read FProjectEnums;
    property Units: TMapUnitDefList read FUnits;
  end;

  TMapProjectList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TMapProject; reintroduce;
    procedure SetItems(i: Integer; AObject: TMapProject); reintroduce;
  public
    property Items[AIndex: Integer]: TMapProject read GetItems write SetItems; default;
    function Add(AObject: TMapProject): Integer; reintroduce;
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
    constructor Create; override;
    destructor Destroy; override;
    property ConnType: string read FConnType write SetConnType;
    property Host: string read FHost write SetHost;
    property DataSource: string read FDataSource write SetDataSource;
    property Params: TStringList read FParams;
  end;

  TMapConnectionDefList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TMapConnectionDef; reintroduce;
    procedure SetItems(i: Integer; AObject: TMapConnectionDef); reintroduce;
  public
    property Items[AIndex: Integer]: TMapConnectionDef read GetItems write SetItems; default;
    function Add(AObject: TMapConnectionDef): Integer; reintroduce;
  end;

  TMapPropertyType = class(TBaseMapObject)
  private
    FTypeName: string;
    FBaseType: TMapPropType;
    procedure SetTypeName(const Value: string);
    procedure SetBaseType(const Value: TMapPropType);
  published
    property BaseType: TMapPropType read FBaseType write SetBaseType;
    property TypeName: string read FTypeName write SetTypeName;
  end;

  TMapPropertyTypeList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TMapPropertyType; reintroduce;
    procedure SetItems(i: Integer; AObject: TMapPropertyType); reintroduce;
  public
    function Add(AObject: TMapPropertyType): Integer; reintroduce; overload;
    function Add(const AName: string; const AType: TMapPropType): integer; overload;
    function FindByTypeName(const ATypeName: string): TMapPropertyType;

    property Items[AIndex: Integer]: TMapPropertyType read GetItems write SetItems; default;
  end;

  {: Represent Pascal base types. ie: String, Integer, etc. }
  TMapPropertyBaseType = class(TMapPropertyType)

  end;

  {: Represent a pascal String type. }
  TMapStringProperty = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal AnsiString type. }
  TMapAnsiStringProperty = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal Double type. }
  TMapDoubleProperty = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal Single type. }
  TMapSingleProperty = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal Currency type. }
  TMapCurrencyProperty = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal Integer type. }
  TMapIntegerProperty = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal Int64 type. }
  TMapInt64Property = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal TDateTime type. }
  TMapDateTimeProperty = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal Boolean type. }
  TMapBooleanProperty = class(TMapPropertyBaseType)
  public
    constructor Create; override;
  end;

  {: Represent a pascal Enumerated type value.  ie: bsNone, bsSolid, etc. }
  TMapEnumValue = class(TMapPropertyBaseType)
  private
    FEnumValue: integer;
    FEnumValueName: string;
    procedure SetEnumValue(const AValue: integer);
    procedure SetEnumValueName(const AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property EnumValueName: string read FEnumValueName write SetEnumValueName;
    property EnumValue: integer read FEnumValue write SetEnumValue;
  end;

  TMapEnumValueList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TMapEnumValue; reintroduce;
    procedure SetItems(i: Integer; AObject: TMapEnumValue); reintroduce;
  public
    property Items[AIndex: Integer]: TMapEnumValue read GetItems write SetItems; default;
    function Add(AObject: TMapEnumValue): Integer; reintroduce; overload;
    function Add(const AName: string; const AValue: integer = -1): integer; overload;
  end;


  {: Represents an enumerated type. }
  TMapEnum = class(TMapPropertyBaseType)
  private
    FValues: TMapEnumValueList;
    FEnumerationSet: boolean;
    FEnumerationSetName: string;
    procedure SetValues(const AValue: TMapEnumValueList);
    procedure SetEnumerationSet(const Value: boolean);
    procedure SetEnumerationSetName(const Value: string);
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property EnumerationSet: boolean read FEnumerationSet write SetEnumerationSet;
    property EnumerationSetName: string read FEnumerationSetName write SetEnumerationSetName;
    property Values: TMapEnumValueList read FValues write SetValues;
  end;

  TMapEnumList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TMapEnum; reintroduce;
    procedure SetItems(i: Integer; AObject: TMapEnum); reintroduce;
  public
    property Items[AIndex: Integer]: TMapEnum read GetItems write SetItems; default;
    function Add(AObject: TMapEnum): Integer; reintroduce;
    function FindByName(const AName: string): TMapEnum;
  end;


  {: Stores information about a class property. }
  TMapClassProp = class(TBaseMapObject)
  private
    FIsReadOnly: boolean;
    FName: string;
    FPropertyType: TMapPropertyType;
    FPropTypeName: string;
    FVirtualGetter: boolean;
    procedure SetIsReadOnly(const AValue: boolean);
    procedure SetPropName(const AValue: string);
    procedure SetPropType(const AValue: TMapPropertyType);
    procedure SetVirtualGetter(AValue: boolean);
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  published
    property Name: string read FName write SetPropName;
    property PropertyType: TMapPropertyType read FPropertyType write SetPropType;
    property IsReadOnly: boolean read FIsReadOnly write SetIsReadOnly;
    property VirtualGetter: boolean read FVirtualGetter write SetVirtualGetter;
  end;

  TMapClassPropList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TMapClassProp; reintroduce;
    procedure SetItems(i: Integer; AObject: TMapClassProp); reintroduce;
  public
    property Items[AIndex: Integer]: TMapClassProp read GetItems write SetItems; default;
    function Add(AObject: TMapClassProp): Integer; reintroduce; overload;
    function Add(const AName: string; const APropType: TMapPropertyType): integer; overload;
    function FindByName(const AName: string): TMapClassProp;
  end;


  {: Stores info about the mapping between a class property and its corresponding database field name. }
  TPropMapping = class(TBaseMapObject)
  private
    FFieldName: string;
    FPropName: string;
    FPropertyType: TMapPropertyType;
    FPropertyGetter: string;
    FPropertySetter: string;
    FPropertyAccessorsAreAbstract: Boolean;
    procedure SetFieldName(const AValue: string);
    procedure SetPropName(const AValue: string);
    procedure SetPropType(const AValue: TMapPropertyType);
    procedure SetPropertyGetter(const AValue: string);
    procedure SetPropertySetter(const AValue: string);
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  public
    function IsValid(const AErrors: TtiObjectErrors): Boolean; override;
  published
    property PropName: string read FPropName write SetPropName;
    property FieldName: string read FFieldName write SetFieldName;
    property PropertyType: TMapPropertyType read FPropertyType write SetPropType;
    property PropertyGetter: string read FPropertyGetter write SetPropertyGetter;
    property PropertySetter: string read FPropertySetter write SetPropertySetter;
    property PropertyAccessorsAreAbstract: Boolean read FPropertyAccessorsAreAbstract write FPropertyAccessorsAreAbstract;
  end;

  TPropMappingList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TPropMapping; reintroduce;
    procedure SetItems(i: Integer; AObject: TPropMapping); reintroduce;
  public
    property Items[AIndex: Integer]: TPropMapping read GetItems write SetItems; default;
    function Add(AObject: TPropMapping): Integer; reintroduce;
  end;


  {: Stores information about the class <--> database mapping such as table name.
    Contains a TPropMappingList. }
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
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property TableName: string read FTableName write SetTableName;
    property PKName: string read FPKName write SetPKName;
    property PKField: string read FPKField write SetPKField;
    property PropMappings: TPropMappingList read FPropMappings;
    property OIDType: TOIDType read FOIDType write SetOIDType;
  end;


  {: Represent a parameter defintion in a selection.  Translates to the parameters of the
    method that will be created in the object list class. }
  TSelectParam = class(TBaseMapObject)
  private
    FParamName: string;
    FParamType: TMapPropertyType;
    FParamTypeName: string;
    FPassBy: string;
    FSQLParamName: string;
    FTypeName: string;
    FValue: Variant;
    procedure SetParamName(const AValue: string);
    procedure SetParamType(const AValue: TMapPropertyType);
    procedure SetPassBy(const AValue: string);
    procedure SetSQLParamName(const AValue: string);
    procedure SetValue(const AValue: Variant);
  published
    property ParamName: string read FParamName write SetParamName;
    property SQLParamName: string read FSQLParamName write SetSQLParamName;
    property ParamType: TMapPropertyType read FParamType write SetParamType;
    property PassBy: string read FPassBy write SetPassBy;
    property Value: Variant read FValue write SetValue;
  end;

  TSelectParamList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TSelectParam; reintroduce;
    procedure SetItems(i: Integer; AObject: TSelectParam); reintroduce;
  public
    property Items[AIndex: Integer]: TSelectParam read GetItems write SetItems; default;
    function Add(AObject: TSelectParam): Integer; reintroduce;
    function FindByName(const AName: string): TSelectParam;
  end;


  {: Represents a class section.  Contains SQL and name.   }
  TClassMappingSelect = class(TBaseMapObject)
  private
    FName: string;
    FParams: TSelectParamList;
    FSQL: string;
    procedure SetName(const AValue: string);
    procedure SetSQL(const Value: string);
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
    function GetCaption: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Name: string read FName write SetName;
    property Params: TSelectParamList read FParams;
    property SQL: string read FSQL write SetSQL;
  end;

  TClassMappingSelectList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TClassMappingSelect; reintroduce;
    procedure SetItems(i: Integer; AObject: TClassMappingSelect); reintroduce;
  public
    property Items[AIndex: Integer]: TClassMappingSelect read GetItems write SetItems; default;
    function Add(AObject: TClassMappingSelect): Integer; reintroduce;
  end;

  TClassMappingList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TClassMapping; reintroduce;
    procedure SetItems(i: Integer; AObject: TClassMapping); reintroduce;
  public
    property Items[AIndex: Integer]: TClassMapping read GetItems write SetItems; default;
    function Add(AObject: TClassMapping): Integer; reintroduce;
  end;

  TFilterDef = class(TBaseMapObject)
  private
    FField: string;
    FFilterType: TFilterType;
    procedure SetField(const AValue: string);
    procedure SetFilterType(const AValue: TFilterType);
  public
    property FilterType: TFilterType read FFilterType write SetFilterType;
    property Field: string read FField write SetField;
  end;

  TFilterDefList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TFilterDef; reintroduce;
    procedure SetItems(i: Integer; AObject: TFilterDef); reintroduce;
  public
    property Items[AIndex: Integer]: TFilterDef read GetItems write SetItems; default;
    function Add(AObject: TFilterDef): Integer; reintroduce;
  end;


  {: Validators get written into the class object's IsValid override. }
  TMapValidator = class(TBaseMapObject)
  private
    FClassProp: TMapClassProp;
    FValidatorType: TValidatorType;
    FValue: variant;
    procedure SetClassProp(const AValue: TMapClassProp);
    procedure SetValidatorType(const AValue: TValidatorType);
    procedure SetValue(const AValue: variant);
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  public
    function IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property ValidatorType: TValidatorType read FValidatorType write SetValidatorType;
    property ClassProp: TMapClassProp read FClassProp write SetClassProp;
    property Value: variant read FValue write SetValue;
  end;


  {: List of TValidator objects. }
  TMapValidatorList = class(TtiObjectList)
  protected
    function GetItems(i: integer): TMapValidator; reintroduce;
    procedure SetItems(i: integer; const AValue: TMapValidator); reintroduce;
  public
    property Items[i: integer]: TMapValidator read GetItems write SetItems;
    function Add(AObject: TMapValidator): integer; reintroduce;
  end;

  TMapClassDef = class(TBaseMapObject)
  private
    FAutoCreateBase: boolean;
    FAutoCreateListClass: boolean;
    FAutoMap: boolean;
    FBaseClassName: string;
    FBaseClassParent: string;
    FBaseListClassParent: string;
    FBaseUnitName: string;
    FClassMapping: TClassMapping;
    FClassProps: TMapClassPropList;
    FCrud: string;
    FDefType: TClassDefType;
    FForwardDeclare: boolean;
    FListSavesDatabaseName: boolean;
    FNotifyObserversOfPropertyChanges: boolean;
    FORMClassName: string;
    FSelections: TClassMappingSelectList;
    FValidators: TMapValidatorList;
    procedure SetAutoCreateBase(const AValue: boolean);
    procedure SetAutoCreateListClass(const AValue: boolean);
    procedure SetAutoMap(const AValue: boolean);
    procedure SetBaseClassName(const AValue: string);
    procedure SetBaseClassParent(const AValue: string);
    procedure SetBaseListClassParent(AValue: string);
    procedure SetBaseUnitName(const AValue: string);
    procedure SetClassProps(const AValue: TMapClassPropList);
    procedure SetCrud(const AValue: string);
    procedure SetDefType(const AValue: TClassDefType);
    procedure SetForwardDeclare(const AValue: boolean);
    procedure SetListSavesDatabaseName(const AValue: boolean);
    procedure SetNotifyObserversOfPropertyChanges(const AValue: boolean);
    procedure SetORMClassName(const AValue: string);
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    // Object Props
    property ClassProps: TMapClassPropList read FClassProps write SetClassProps;
    property ClassMapping: TClassMapping read FClassMapping;
    property Selections: TClassMappingSelectList read FSelections;
    property Validators: TMapValidatorList read FValidators;

    property AutoCreateBase: boolean read FAutoCreateBase write SetAutoCreateBase;
    property AutoCreateListClass: boolean read FAutoCreateListClass write SetAutoCreateListClass;
    property AutoMap: Boolean read FAutoMap write SetAutoMap;
    property BaseClassName: string read FBaseClassName write SetBaseClassName;
    property BaseClassParent: string read FBaseClassParent write SetBaseClassParent;
    property BaseListClassParent: string read FBaseListClassParent write SetBaseListClassParent;
    property BaseUnitName: string read FBaseUnitName write SetBaseUnitName;
    property Crud: string read FCrud write SetCrud;
    property DefType: TClassDefType read FDefType write SetDefType;
    property ForwardDeclare: boolean read FForwardDeclare write SetForwardDeclare;
    property ListSavesDatabaseName: boolean read FListSavesDatabaseName write SetListSavesDatabaseName;
    property ORMClassName: string read FORMClassName write SetORMClassName;
    property NotifyObserversOfPropertyChanges: boolean read FNotifyObserversOfPropertyChanges write SetNotifyObserversOfPropertyChanges default False;
  end;

  TMapClassDefList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TMapClassDef; reintroduce;
    procedure SetItems(i: Integer; AObject: TMapClassDef); reintroduce;
  public
    property Items[AIndex: Integer]: TMapClassDef read GetItems write SetItems; default;
    function Add(AObject: TMapClassDef): Integer; reintroduce;
    function FindByName(const AName: string): TMapClassDef;
  end;

  TMapUnitDef = class(TBaseMapObject)
  private
    FReferences: TStringList;
    FUnitClasses: TMapClassDefList;
    FUnitEnums: TMapEnumList;
    FName: string;
    procedure SetUnitName(const AValue: string);
  protected
    procedure AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function IsValid(const AErrors: TtiObjectErrors): boolean; override;
    function HasValidators: boolean;
  published
    property Name: string read FName write SetUnitName;
    // Object properties
    property UnitClasses: TMapClassDefList read FUnitClasses;
    property UnitEnums: TMapEnumList read FUnitEnums;
    property References: TStringList read FReferences;
  end;

  TMapUnitDefList = class(TBaseMapObjectList)
  protected
    function GetItems(i: Integer): TMapUnitDef; reintroduce;
    procedure SetItems(i: Integer; AObject: TMapUnitDef); reintroduce;
  public
    property Items[AIndex: Integer]: TMapUnitDef read GetItems write SetItems; default;
    function Add(AObject: TMapUnitDef): Integer; reintroduce;
    function FindByName(const AName: string): TMapUnitDef;
  end;

  {: Class of TMapSchemaReader. }
  TMapSchemaReader = class;

  TMapSchemaReaderClass = class of TMapSchemaReader;


  {: Abstract class used to read xml schema file into TMapProject. }
  TMapSchemaReader = class(TBaseMapObject)
  public
    procedure ReadSchema(AProject: TMapProject; const AFileName: string); overload; virtual; abstract;
    procedure WriteAll; virtual; abstract;
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
    property CurrentIndent: integer read FCurrentIndent write SetCurrentIndent;
    function TabToSpaces(const ANumTabs: integer): string;
    procedure IncTab(const ANum: integer = 1);
    procedure DecTab;
    procedure WriteLine(const AText: string; AList: TStringList);
    procedure WriteBreak(AList: TStringList);
    property Project: TMapProject read FProject write SetProject;
  public
    constructor Create(AProject: TMapProject); reintroduce; virtual;
    destructor Destroy; override;
    procedure WriteProject(const ADirectory: string); overload; virtual; abstract;
    procedure WriteProject(const ADirectory: string; ASL: TStringList); overload; virtual; abstract;
    // Events
    property OnWriteClass: TOnWriteClassIntf read FOnWriteClass write SetOnWriteClass;
    property OnWriteUnit: TOnWriteUnit read FOnWriteUnit write SetOnWriteUnit;
    property OnWriteEnum: TOnWriteEnum read FOnWriteEnum write SetOnWriteEnum;
    property OnWriteMapping: TOnWriteMapping read FOnWriteMapping write SetOnWriteMapping;
  end;


  {: TtiFilteredObjectList descendant with extra properties to facilitate custom searching/queries. }
  TtiMappedFilteredObjectList = class(TtiFilteredObjectList)
  private
    FEnumType: TEnumType;
    FObjClass: TtiObjectClass;
    FParams: TSelectParamList;
    FSQL: string;
    procedure SetEnumType(const AValue: TEnumType);
    procedure SetObjClass(const AValue: TtiObjectClass);
    procedure SetSQL(const AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    //function IsValid(const AErrors: TtiObjectErrors): boolean; overload; override;
    property SQL: string read FSQL write SetSQL;
    property ObjClass: TtiObjectClass read FObjClass write SetObjClass;
    property EnumType: TEnumType read FEnumType write SetEnumType;
    property Params: TSelectParamList read FParams;
    procedure AddParam(const AName: string; const ASQLParamName: string; AParamType: TMapPropertyType; AValue: Variant);
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
    procedure Init; override;
    procedure SetupParams; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property SQL: string read FSQL write SetSQL;
    property ClassDef: TMapClassDef read FClassDef write SetClassDef;
    property ObjClass: TtiObjectClass read FObjClass write SetObjClass;
    property EnumType: TEnumType read FEnumType write SetEnumType;
  end;


  {: Class for creating validator error strings. }
  TValidatorStringGenerator = class(TtiObject)
  public
    class function GetValueAsString(AObject: TtiObject; const APropName: string; AValue: Variant): string;
    class function CreateRequiredValidatorMsg(AObject: TtiObject; const APropName: string): string; virtual;
    class function CreateGreaterValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string; virtual;
    class function CreateGreaterOrEqualValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string; virtual;
    class function CreateLessThanValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string; virtual;
    class function CreateLessThanOrEqualValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string; virtual;
    class function CreateNotEqualToValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string; virtual;
  end;


  {: Class of  }
  TValidatorStringGeneratorClass = class of TValidatorStringGenerator;

procedure RegisterMappings;

  // Misc global methods.
  {: Set the class used for reading and writing schema projects from and to XML. }
procedure gSetSchemaReaderClass(const AClass: TMapSchemaReaderClass);
  {: Set the class of the default schema reader. }

function gGetSchemaReaderClass: TMapSchemaReaderClass;
  {: Converts AString into a TClassDefType. }

function gStrToClassDefType(const AString: string): TClassDefType;
  {: Converts AString into corresponding TMapPropType. }

function gStrToPropType(const AString: string): TMapPropType;
  {: Converts a TMapPropType into its streaming evquilv. }

function gPropTypeToStr(const APropType: TMapPropType): string;
  {: Finds a TtiAttrColMap given the AClassName.  Built in function uses the TClass instead of string. }

function gFindAttrMap(const AClassName: string; const AAttrName: string): TtiAttrColMap;
  {: Converts AString into corresponding TValidatorType. }

function gStrToValType(const AString: string): TValidatorType;
  {: Converts AValType into its string equiv. }

function gValTypeToStr(const AValType: TValidatorType): string;
  {: Returns the absolute path of Source Path given Relative path. }

function GetAbsolutePath(Source, Relative: string): string;
  {: Converts a string representation to TOIDType. }

function gStrToOIDType(const AString: string): TOIDType;

  // -----------------------------------------------------------------
  //  Glob vars
  // -----------------------------------------------------------------

var
  ValidatorStringClass: TValidatorStringGeneratorClass;

implementation

var
  mSchemaReaderClass: TMapSchemaReaderClass;

function gStrToOIDType(const AString: string): TOIDType;
var
  s: string;
begin
  s := LowerCase(AString);
  if (s = 'string') or (s = 'guid') then
    Result := otString
  else
    Result := otInt;
end;

function GetappearNum(sub, st: string): integer;
var
  i: integer;
  P: integer;
begin

  P := Pos(sub, st);
  i := 0;
  while P > 0 do
  begin
    inc(i);
    delete(st, 1, P + length(sub) - 1);
    P := Pos(sub, st);
  end;
  result := i;
end;

function decomposestr(sub, st: string; var tst: TStringList): Boolean;
var
  num: integer;
  P: integer;
begin
  P := Pos(sub, st);
  tst.Clear;
  while P > 0 do
  begin
    num := P + length(sub) - 1;
    tst.Add(copy(st, 1, num));
    delete(st, 1, num);
    P := Pos(sub, st);
  end;
  tst.Add(st);
  Result := True;

end;

function CopyLeftNum(sub, st: string; num: integer): string;
var
  Tst: TStringList;
  I: integer;
begin
  Tst := TStringList.Create;
  decomposestr(sub, st, Tst);
  if num >= Tst.Count then
    Result := st
  else
  begin
    for I := 0 to num - 1 do
    begin
      Result := Result + Tst[I];
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
      for I := Tst.Count - Num to Tst.Count - 1 do
      begin
        Result := Result + Tst[I]
      end;
    end;
  finally
    Tst.Free;
  end;
end;

function gValTypeToStr(const AValType: TValidatorType): string;
begin

  case AValType of
    vtRequired:
      result := 'required';
    vtGreater:
      result := 'greater';
    vtGreaterEqual:
      result := 'greater-equal';
    vtLess:
      result := 'less';
    vtLessEqual:
      result := 'less-equal';
    vtNotEqual:
      result := 'not-equal';
    vtRegExp:
      result := 'reg-exp';
  else
    raise Exception.Create('gValTypeToStr: Value out of range');
  end;
end;

function GetAbsolutePath(Source, Relative: string): string;
var
  i, Num, num1: integer;
  St: TStringList;
  s: string;
begin
  Num := GetappearNum('..', Relative);
  St := TStringList.Create;
  decomposestr(PathDelim, ExcludeTrailingBackslash(Source), St);
  num1 := St.Count;

  Result := '';

  for i := 0 to num1 - Num - 1 do
  begin
    Result := Result + St[i];
  end;

  if Pos('\', Relative) > 0 then
    s := CopyRightNum('..\', Relative, 1)
  else
  begin
    if Pos('/', Relative) > 0 then
      s := CopyRightNum('../', Relative, 1)
    else
        // if we got here it means the Relative value is simply a directory name with no slashes
      s := PathDelim + Relative;
  end;

  Result := Result + s;
  St.Free;
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

function gStrToClassDefType(const AString: string): TClassDefType;
begin
  if LowerCase(AString) = 'create' then
    result := dtCreate
  else if LowerCase(AString) = 'reference' then
    result := dtReference
  else
    raise Exception.Create('gStrToClassDefType: invalid parameter');
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
  else if lType = 'enumset' then
    result := ptEnumSet
  else if lType = 'currency' then
    result := ptCurrency
  else if lType = 'double' then
    Result := ptCurrency
  else if lType = 'single' then
    result := ptSingle
  else if lType = 'blob' then
    result := ptStream
  else
    raise Exception.Create('gStrToPropType: Invalid parameter: ' + AString);

end;

function gPropTypeToStr(const APropType: TMapPropType): string;
begin
  case APropType of
    ptString:
      result := 'String';
    ptInt64:
      result := 'Int64';
    ptInteger:
      result := 'Integer';
    ptAnsiString:
      result := 'AnsiString';
    ptBoolean:
      result := 'Boolean';
    ptDateTime:
      Result := 'TDateTime';
    ptSingle:
      result := 'Single';
    ptDouble:
      result := 'Double';
    ptCurrency:
      result := 'Currency';
    ptEnum:
      result := 'enum';
    ptEnumSet:
      result := 'enumset';
    ptStream:
      result := 'blob';
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
  for lCtr := 0 to lMapList.Count - 1 do
  begin
    lMap := lMapList.Items[lCtr];
    if (lMap.AttrMap.Owner.PerObjAbsClass.ClassName = AClassName) and (SameText(lMap.AttrMap.AttrName, AAttrName)) then
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
    raise Exception.Create('gStrToValType: Value out of range');
end;

{ TMapProject }

procedure TMapProject.AssignClassProps(ASource: TtiObject);
var
  lSource: TMapProject;
begin
  inherited;

  if ASource is TMapProject then
  begin
    lSource := ASource as TMapProject;

    GeneralOptions.Assign(lSource.GeneralOptions);
    CodeGenerationOptions.Assign(lSource.CodeGenerationOptions);
    DatabaseOptions.Assign(lSource.DatabaseOptions);
    Units.Assign(lSource.Units);
    ProjectClasses.Assign(lSource.ProjectClasses);
    ProjectEnums.Assign(lSource.ProjectEnums);
  end;
end;

procedure TMapProject.ClearAll;
begin
  Includes.Clear;
  Units.Clear;
  ProjectClasses.Clear;
  ProjectEnums.Clear;
end;

constructor TMapProject.Create;
begin
  inherited Create;

  FGeneralOptions := TMapGeneralProjectOptions.Create;
  FCodeGenerationOptions := TMapCodeGenerationProjectOptions.Create;
  FCodeGenerationOptions.MaxEditorCodeWidth := 80;

  FDatabaseOptions := TMapDatabaseProjectOptions.Create;
  FDatabaseOptions.DoubleQuoteDBFieldNames := false;

  FIncludes := TStringList.Create;
  FUnits := TMapUnitDefList.Create;

  FProjectClasses := TMapClassDefList.Create;
  FProjectClasses.OwnsObjects := false;

  FProjectEnums := TMapEnumList.Create;
  FProjectEnums.OwnsObjects := false;
end;

destructor TMapProject.Destroy;
begin
  FIncludes.Free;
  FUnits.Free;
  FProjectClasses.Free;
  FProjectEnums.Free;

  FCodeGenerationOptions.Free;
  FDatabaseOptions.Free;
  FGeneralOptions.Free;

  inherited Destroy;
end;

function TMapProject.FindEnumForPropName(const AUnitName: string; const AClassName: string; const APropName: string): TMapEnum;
var
  lClassDef: TMapClassDef;
  lProp: TMapClassProp;
  lUnit: TMapUnitDef;
begin
  try
    lUnit := Units.FindByName(AUnitName);
    lClassDef := lUnit.UnitClasses.FindByName(AClassName);
    lProp := lClassDef.ClassProps.FindByName(APropName);
    result := lUnit.UnitEnums.FindByName(lProp.PropertyType.TypeName);
  except
    result := nil;
  end;
end;

function TMapProject.HasCustomSelects: boolean;
var
  lClassCtr, lUnitCtr: integer;
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

procedure TMapProject.SetFileName(const AValue: string);
begin
  if FFileName = AValue then
    exit;
  FFileName := AValue;
  NotifyObservers;
end;

procedure TMapProject.SetProjectClasses(const AValue: TMapClassDefList);
begin
  if FProjectClasses = AValue then
    exit;
  FProjectClasses := AValue;
end;


{ TBaseMapObjectList }

function TBaseMapObjectList.Add(AObject: TBaseMapObject): Integer;
begin
  Result := inherited Add(AObject);
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
  if FConnType = AValue then
    exit;
  FConnType := AValue;
  NotifyObservers;
end;

procedure TMapConnectionDef.SetDataSource(const AValue: string);
begin
  if FDataSource = AValue then
    exit;
  FDataSource := AValue;
  NotifyObservers;
end;

procedure TMapConnectionDef.SetHost(const AValue: string);
begin
  if FHost = AValue then
    exit;

  FHost := AValue;
  NotifyObservers;
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

procedure TMapConnectionDefList.SetItems(i: Integer; AObject: TMapConnectionDef);
begin
  inherited SetItems(i, AObject);
end;

{ TMapUnitDef }

procedure TMapUnitDef.AssignClassProps(ASource: TtiObject);
var
  lSource: TMapUnitDef;
begin
  inherited;

  if ASource is TMapUnitDef then
  begin
    lSource := ASource as TMapUnitDef;

    UnitClasses.Assign(lSource.UnitClasses);
    UnitEnums.Assign(lSource.UnitEnums);
    References.Assign(lSource.References);
  end;
end;

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

function TMapUnitDef.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  result := false;

  if Assigned(AErrors) then
  begin
    if Name = EmptyStr then
      AErrors.AddError('Name', 'Unit must have a name.');

    result := AErrors.Count = 0;
  end;
end;

procedure TMapUnitDef.SetUnitName(const AValue: string);
begin
  if FName = AValue then
    exit;
  FName := AValue;
  NotifyObservers;
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
  if FEnumValue = AValue then
    exit;

  FEnumValue := AValue;
  NotifyObservers;
end;

procedure TMapEnumValue.SetEnumValueName(const AValue: string);
begin
  if FEnumValueName = AValue then
    exit;

  FEnumValueName := AValue;
  NotifyObservers;
end;

{ TMapEnumValueList }

function TMapEnumValueList.Add(const AName: string; const AValue: integer): integer;
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

procedure TMapEnum.AssignClassProps(ASource: TtiObject);
var
  lSource: TMapEnum;
begin
  inherited;

  if ASource is TMapEnum then
  begin
    lSource := ASource as TMapEnum;

    Values.Assign(lSource.Values);
  end;
end;

constructor TMapEnum.Create;
begin
  inherited Create;
  BaseType := ptEnum;
  TypeName := 'TEnumerated';
  EnumerationSet := false;
  EnumerationSetName := '';

  FValues := TMapEnumValueList.Create;
end;

destructor TMapEnum.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

procedure TMapEnum.SetEnumerationSet(const Value: boolean);
begin
  if FEnumerationSet = Value then
    exit;

  FEnumerationSet := Value;
  NotifyObservers;
end;

procedure TMapEnum.SetEnumerationSetName(const Value: string);
begin
  if FEnumerationSetName = Value then
    exit;

  FEnumerationSetName := Value;
  NotifyObservers;
end;

procedure TMapEnum.SetValues(const AValue: TMapEnumValueList);
begin
  if FValues = AValue then
    Exit;
  FreeAndNil(FValues);
  FValues := AValue;
end;

{ TMapClassDef }

procedure TMapClassDef.AssignClassProps(ASource: TtiObject);
var
  lSource: TMapClassDef;
begin
  inherited;

  if ASource is TMapClassDef then
  begin
    lSource := ASource as TMapClassDef;

    ClassProps.Assign(lSource.ClassProps);
    ClassMapping.Assign(lSource.ClassMapping);
    Selections.Assign(lSource.Selections);
    Validators.Assign(lSource.Validators);
  end;
end;

constructor TMapClassDef.Create;
begin
  inherited Create;

  FClassProps := TMapClassPropList.Create;
  FClassMapping := TClassMapping.Create;
  FSelections := TClassMappingSelectList.Create;
  FValidators := TMapValidatorList.Create;
  FNotifyObserversOfPropertyChanges := False;
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
  if FAutoCreateBase = AValue then
    exit;
  FAutoCreateBase := AValue;
end;

procedure TMapClassDef.SetAutoCreateListClass(const AValue: boolean);
begin
  if FAutoCreateListClass = AValue then
    exit;
  FAutoCreateListClass := AValue;
end;

procedure TMapClassDef.SetAutoMap(const AValue: boolean);
begin
  if FAutoMap = AValue then
    exit;
  FAutoMap := AValue;
end;

procedure TMapClassDef.SetBaseClassName(const AValue: string);
begin
  if FBaseClassName = AValue then
    exit;
  FBaseClassName := AValue;
end;

procedure TMapClassDef.SetBaseClassParent(const AValue: string);
begin
  if FBaseClassParent = AValue then
    exit;
  FBaseClassParent := AValue;
end;

procedure TMapClassDef.SetBaseListClassParent(AValue: string);
begin
  if FBaseListClassParent = AValue then
    Exit;
  FBaseListClassParent := AValue;
end;

procedure TMapClassDef.SetBaseUnitName(const AValue: string);
begin
  if FBaseUnitName = AValue then
    exit;
  FBaseUnitName := AValue;
end;

procedure TMapClassDef.SetClassProps(const AValue: TMapClassPropList);
begin
  if FClassProps = AValue then
    exit;
  FClassProps := AValue;
end;

procedure TMapClassDef.SetCrud(const AValue: string);
begin
  if FCrud = AValue then
    exit;
  FCrud := AValue;
end;

procedure TMapClassDef.SetDefType(const AValue: TClassDefType);
begin
  if FDefType = AValue then
    exit;
  FDefType := AValue;
end;

procedure TMapClassDef.SetForwardDeclare(const AValue: boolean);
begin
  if FForwardDeclare = AValue then
    exit;
  FForwardDeclare := AValue;
end;

procedure TMapClassDef.SetListSavesDatabaseName(const AValue: boolean);
begin
  if FListSavesDatabaseName = AValue then
    exit;
  FListSavesDatabaseName := AValue;
end;

procedure TMapClassDef.SetNotifyObserversOfPropertyChanges(const AValue: boolean);
begin
  if FNotifyObserversOfPropertyChanges = AValue then
    exit;
  FNotifyObserversOfPropertyChanges := AValue;
end;

procedure TMapClassDef.SetORMClassName(const AValue: string);
begin
  if FORMClassName = AValue then
    exit;
  FORMClassName := AValue;
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

procedure TMapClassProp.AssignClassProps(ASource: TtiObject);
var
  lSource: TMapClassProp;
begin
  inherited;

  if ASource is TMapClassProp then
  begin
    lSource := ASource as TMapClassProp;

    PropertyType := lSource.PropertyType;
  end;
end;

procedure TMapClassProp.SetIsReadOnly(const AValue: boolean);
begin
  if FIsReadOnly = AValue then
    exit;

  FIsReadOnly := AValue;
  NotifyObservers;
end;

procedure TMapClassProp.SetPropName(const AValue: string);
begin
  if FName = AValue then
    exit;

  FName := AValue;
  NotifyObservers;
end;

procedure TMapClassProp.SetPropType(const AValue: TMapPropertyType);
begin
  if FPropertyType = AValue then
    exit;

  FPropertyType := AValue;
  NotifyObservers;
end;

procedure TMapClassProp.SetVirtualGetter(AValue: boolean);
begin
  if FVirtualGetter = AValue then
    Exit;

  FVirtualGetter := AValue;
  NotifyObservers;
end;

{ TMapClassPropList }

function TMapClassPropList.Add(const AName: string; const APropType: TMapPropertyType): integer;
var
  lProp: TMapClassProp;
begin
  lProp := TMapClassProp.create;
  lProp.Name := AName;
  lProp.PropertyType := APropType;
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
    if LowerCase(Items[lCtr].Name) = LowerCase(AName) then
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

procedure TClassMapping.AssignClassProps(ASource: TtiObject);
var
  lSource: TClassMapping;
begin
  inherited;

  if ASource is TClassMapping then
  begin
    lSource := ASource as TClassMapping;

    PropMappings.Assign(lSource.PropMappings);
  end;
end;

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
  if FOIDType = AValue then
    exit;
  FOIDType := AValue;
  NotifyObservers;
end;

procedure TClassMapping.SetPKField(const AValue: string);
begin
  if FPKField = AValue then
    exit;
  FPKField := AValue;
  NotifyObservers;
end;

procedure TClassMapping.SetPKName(const AValue: string);
begin
  if FPKName = AValue then
    exit;
  FPKName := AValue;
  NotifyObservers;
end;

procedure TClassMapping.SetTableName(const AValue: string);
begin
  if FTableName = AValue then
    exit;
  FTableName := AValue;
  NotifyObservers;
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

procedure TPropMapping.AssignClassProps(ASource: TtiObject);
var
  lSource: TPropMapping;
begin
  inherited;

  if ASource is TPropMapping then
  begin
    lSource := ASource as TPropMapping;

    PropertyType := lSource.PropertyType;
  end;
end;

function TPropMapping.IsValid(const AErrors: TtiObjectErrors): Boolean;
begin
  result := inherited IsValid(AErrors);

  if not result then
    exit;

  if (PropName = '') or (Pos(' ', PropName) > 0) then
  begin
    AErrors.AddError('Property Name: Must be present and include no spaces');
  end;

  if (FieldName = '') or (Pos(' ', FieldName) > 0) then
  begin
    AErrors.AddError('Database Field Name: Must be present and include no spaces');
  end;

  Result := AErrors.Count = 0;

end;

procedure TPropMapping.SetFieldName(const AValue: string);
begin
  if FFieldName = AValue then
    exit;
  FFieldName := AValue;
  NotifyObservers;
end;

procedure TPropMapping.SetPropName(const AValue: string);
begin
  if FPropName = AValue then
    exit;
  FPropName := AValue;
  NotifyObservers;
end;

procedure TPropMapping.SetPropType(const AValue: TMapPropertyType);
begin
  if FPropertyType = AValue then
    exit;
  FPropertyType := AValue;
  NotifyObservers;
end;

procedure TPropMapping.SetPropertyGetter(const AValue: string);
begin
  if FPropertyGetter = AValue then
    exit;
  FPropertyGetter := AValue;
  NotifyObservers;
end;

procedure TPropMapping.SetPropertySetter(const AValue: string);
begin
  if FPropertySetter = AValue then
    exit;
  FPropertySetter := AValue;
  NotifyObservers;
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

procedure TFilterDef.SetField(const AValue: string);
begin
  if FField = AValue then
    exit;
  FField := AValue;
  NotifyObservers;
end;

procedure TFilterDef.SetFilterType(const AValue: TFilterType);
begin
  if FFilterType = AValue then
    exit;
  FFilterType := AValue;
  NotifyObservers;
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
    if LowerCase(Items[lCtr].TypeName) = lName then
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
    if LowerCase(Items[lCtr].Name) = lName then
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
  if FCurrentIndent = AValue then
    exit;
  FCurrentIndent := AValue;
end;

procedure TMapSchemaWriter.SetOnWriteClass(const AValue: TOnWriteClassIntf);
begin
  FOnWriteClass := AValue;
end;

procedure TMapSchemaWriter.SetOnWriteEnum(const AValue: TOnWriteEnum);
begin
  FOnWriteEnum := AValue;
end;

procedure TMapSchemaWriter.SetOnWriteMapping(const AValue: TOnWriteMapping);
begin
  FOnWriteMapping := AValue;
end;

procedure TMapSchemaWriter.SetOnWriteUnit(const AValue: TOnWriteUnit);
begin
  FOnWriteUnit := AValue;
end;

procedure TMapSchemaWriter.SetProject(const AValue: TMapProject);
begin
  FProject := AValue;
end;

function TMapSchemaWriter.TabToSpaces(const ANumTabs: integer): string;
begin
  result := StringOfChar(' ', ANumTabs * Project.CodeGenerationOptions.TabSpaces);
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
  if FParamName = AValue then
    exit;
  FParamName := AValue;
  NotifyObservers;
end;

procedure TSelectParam.SetParamType(const AValue: TMapPropertyType);
begin
  if FParamType = AValue then
    exit;
  FParamType := AValue;
  NotifyObservers;
end;

procedure TSelectParam.SetPassBy(const AValue: string);
begin
  if FPassBy = AValue then
    exit;
  FPassBy := AValue;
  NotifyObservers;
end;

procedure TSelectParam.SetSQLParamName(const AValue: string);
begin
  if FSQLParamName = AValue then
    exit;
  FSQLParamName := AValue;
  NotifyObservers;
end;

procedure TSelectParam.SetValue(const AValue: Variant);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  NotifyObservers;
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

procedure TClassMappingSelect.AssignClassProps(ASource: TtiObject);
var
  lSource: TClassMappingSelect;
begin
  inherited;

  if ASource is TClassMappingSelect then
  begin
    lSource := ASource as TClassMappingSelect;

    lSource.Params.Assign(lSource.Params);
  end;
end;

constructor TClassMappingSelect.Create;
begin
  inherited Create;
  FSQL := '';
  FParams := TSelectParamList.Create;
end;

destructor TClassMappingSelect.Destroy;
begin
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
      lParams := lPar.ParamName
    else
      lParams := lParams + ', ' + lPar.ParamName;
  end;

  result := result + lParams + ')';
end;

procedure TClassMappingSelect.SetName(const AValue: string);
begin
  if FName = AValue then
    exit;
  FName := AValue;
  NotifyObservers;
end;

procedure TClassMappingSelect.SetSQL(const Value: string);
begin
  if FSQL = Value then
    exit;

  FSQL := Value;
  NotifyObservers;
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

procedure TClassMappingSelectList.SetItems(i: Integer; AObject: TClassMappingSelect);
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
  if FClassDef = AValue then
    exit;
  FClassDef := AValue;
end;

procedure TtiMapParameterListReadVisitor.SetEnumType(const AValue: TEnumType);
begin
  if FEnumType = AValue then
    exit;
  FEnumType := AValue;
end;

procedure TtiMapParameterListReadVisitor.SetObjClass(const AValue: TtiObjectClass);
begin
  if FObjClass = AValue then
    exit;
  FObjClass := AValue;
end;

procedure TtiMapParameterListReadVisitor.SetSQL(const AValue: string);
begin
  if FSQL = AValue then
    exit;
  FSQL := AValue;
end;

procedure TtiMapParameterListReadVisitor.SetupParams;
(*
var
  lCtr: integer;
  lParam: TSelectParam;
  lList: TtiMappedFilteredObjectList;
  lProp: TMapClassProp;
*)
begin
  inherited SetupParams;

(*
  lList := TtiMappedFilteredObjectList(Visited);

  for lCtr := 0 to lList.Params.Count - 1 do
    begin
      lParam := lList.Params.Items[lCtr];

      case lParam.ParamType of
        ptString,
        ptAnsiString:
          Query.ParamAsString[lParam.SQLParamName] := lParam.Value;

        ptBoolean:
          Query.ParamAsBoolean[lParam.SQLParamName] := lParam.Value;

        ptDateTime:
          Query.ParamAsDateTime[lParam.SQLParamName] := lParam.Value;

        ptDouble,
        ptCurrency,
        ptSingle:
          Query.ParamAsFloat[lParam.SQLParamName] := lParam.Value;

        ptInt64,
        ptInteger:
          Query.ParamAsInteger[lParam.SQLParamName] := lParam.Value;

        ptEnum:
          begin
            if lList.EnumType = etString then
              Query.ParamAsString[lParam.SQLParamName] := GetEnumName(TypeInfo(TMapPropType), Integer(lParam.Value))
            else
              Query.ParamAsInteger[lParam.SQLParamName] := Integer(lParam.Value);
          end;

        ptStream:
          Query.AssignParamFromStream(lParam.SQLParamName, TStream(lParam.Value));
      end;
    end;
*)
end;

{ TtiMappedFilteredObjectList }

procedure TtiMappedFilteredObjectList.AddParam(const AName: string; const ASQLParamName: string; AParamType: TMapPropertyType; AValue: Variant);
var
  lParam: TSelectParam;
begin
  lParam := FParams.FindByName(AName);
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
  if FEnumType = AValue then
    exit;
  FEnumType := AValue;
end;

procedure TtiMappedFilteredObjectList.SetObjClass(const AValue: TtiObjectClass);
begin
  if FObjClass = AValue then
    exit;
  FObjClass := AValue;
end;

procedure TtiMappedFilteredObjectList.SetSQL(const AValue: string);
begin
  if FSQL = AValue then
    exit;
  FSQL := AValue;
end;

{ TMapValidator }

procedure TMapValidator.AssignClassProps(ASource: TtiObject);
var
  lSource: TMapValidator;
begin
  inherited;

  if ASource is TMapValidator then
  begin
    lSource := ASource as TMapValidator;

    ClassProp := lSource.ClassProp;
  end;
end;

function TMapValidator.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  result := False;

  if Assigned(AErrors) then
  begin
    if not Assigned(ClassProp) then
      AErrors.AddError('ClassProp', 'ClassProp canno''t be empty.');

    if Assigned(ClassProp) and (ClassProp.PropertyType.BaseType in [ptEnum, ptEnumSet, ptStream]) and
      (ValidatorType in [vtRegExp, vtRequired]) then
      AErrors.AddError('ValidatorType', 'Validator not available for the property "' + ClassProp.Name + '" of type "' +
        gPropTypeToStr(ClassProp.PropertyType.BaseType) + '".');

    if (ValidatorType in [vtGreater, vtGreaterEqual, vtLess, vtLessEqual, vtNotEqual, vtRegExp]) and
      (VarIsEmpty(Value) or VarIsClear(Value) or VarIsNull(Value) or (VarCompareValue(Value, Unassigned) = vrEqual)) then
      AErrors.AddError('Value', 'Value required for this validator type.');

    result := AErrors.Count = 0;
  end;
end;

procedure TMapValidator.SetClassProp(const AValue: TMapClassProp);
begin
  if FClassProp = AValue then
    exit;
  FClassProp := AValue;
  NotifyObservers;
end;

procedure TMapValidator.SetValidatorType(const AValue: TValidatorType);
begin
  if FValidatorType = AValue then
    exit;
  FValidatorType := AValue;
  NotifyObservers;
end;

procedure TMapValidator.SetValue(const AValue: variant);
begin
  if FValue = AValue then
    exit;

  FValue := AValue;
  NotifyObservers;
end;

{ TMapValidatorList }

function TMapValidatorList.Add(AObject: TMapValidator): integer;
begin
  result := inherited Add(AObject);
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

class function TValidatorStringGenerator.CreateGreaterOrEqualValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string;
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

class function TValidatorStringGenerator.CreateGreaterValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string;
const
  MSG = 'Value of %s must be greater than %s.';
begin
  result := format(MSG, [APropName, GetValueAsString(AObject, APropName, AValue)]);
end;

class function TValidatorStringGenerator.CreateLessThanOrEqualValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string;
const
  MSG = 'Value of %s must be greater than or equal to %s.';
begin
  result := format(MSG, [APropName, GetValueAsString(AObject, APropName, AValue)]);
end;

class function TValidatorStringGenerator.CreateLessThanValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string;
const
  MSG = 'Value of %s must be less than or equal to %s.';
begin
  result := format(MSG, [APropName, GetValueAsString(AObject, APropName, AValue)]);
end;

class function TValidatorStringGenerator.CreateNotEqualToValidatorMsg(AObject: TtiObject; const APropName: string; AValue: Variant): string;
const
  MSG = 'Value of %s must not equal to %s.';
begin
  result := format(MSG, [APropName, GetValueAsString(AObject, APropName, AValue)]);
end;

class function TValidatorStringGenerator.CreateRequiredValidatorMsg(AObject: TtiObject; const APropName: string): string;
const
  MSG = '%s must have a value.';
begin
  result := format(MSG, [APropName]);
end;

class function TValidatorStringGenerator.GetValueAsString(AObject: TtiObject; const APropName: string; AValue: Variant): string;
var
  lType: TtiTypeKind;
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

{ TMapPropertyTypeList }

function TMapPropertyTypeList.Add(AObject: TMapPropertyType): Integer;
begin
  result := inherited Add(AObject);
end;

function TMapPropertyTypeList.Add(const AName: string; const AType: TMapPropType): integer;
var
  lValue: TMapPropertyType;
begin
  lValue := TMapPropertyType.Create;
  lValue.BaseType := AType;
  lValue.TypeName := AName;
  result := Self.Add(lValue);
end;

function TMapPropertyTypeList.FindByTypeName(
  const ATypeName: string): TMapPropertyType;
begin
  result := TMapPropertyType(FindByProps(['TypeName'], [ATypeName], false));
end;

function TMapPropertyTypeList.GetItems(i: Integer): TMapPropertyType;
begin
  result := TMapPropertyType(inherited GetItems(I));
end;

procedure TMapPropertyTypeList.SetItems(i: Integer; AObject: TMapPropertyType);
begin
  inherited SetItems(I, AObject);
end;

{ TMapPropertyType }

procedure TMapPropertyType.SetBaseType(const Value: TMapPropType);
begin
  if FBaseType = Value then
    exit;

  FBaseType := Value;
  NotifyObservers;
end;

procedure TMapPropertyType.SetTypeName(const Value: string);
begin
  if FTypeName = Value then
    exit;

  FTypeName := Value;
  NotifyObservers;
end;

{ TMapStringProperty }

constructor TMapStringProperty.Create;
begin
  inherited Create;

  FBaseType := ptString;
  FTypeName := 'String';
end;

{ TMapIntegerProperty }

constructor TMapIntegerProperty.Create;
begin
  inherited;

  FBaseType := ptInteger;
  FTypeName := 'Integer';
end;

{ TMapAnsiStringProperty }

constructor TMapAnsiStringProperty.Create;
begin
  inherited;

  FBaseType := ptAnsiString;
  FTypeName := 'AnsiString';
end;

{ TMapDoubleProperty }

constructor TMapDoubleProperty.Create;
begin
  inherited;

  FBaseType := ptDouble;
  FTypeName := 'Double';
end;

{ TMapSingleProperty }

constructor TMapSingleProperty.Create;
begin
  inherited;

  FBaseType := ptSingle;
  FTypeName := 'Single';
end;

{ TMapCurrencyProperty }

constructor TMapCurrencyProperty.Create;
begin
  inherited;

  FBaseType := ptCurrency;
  FTypeName := 'Currency';
end;

{ TMapInt64Property }

constructor TMapInt64Property.Create;
begin
  inherited;

  FBaseType := ptInt64;
  FTypeName := 'Int64';
end;

{ TMapDateTimeProperty }

constructor TMapDateTimeProperty.Create;
begin
  inherited;

  FBaseType := ptDateTime;
  FTypeName := 'TDateTime';
end;

{ TMapBooleanProperty }

constructor TMapBooleanProperty.Create;
begin
  inherited;

  FBaseType := ptBoolean;
  FTypeName := 'Boolean';
end;

{ TBaseMapObject }

procedure TBaseMapObject.AssignClassProps(ASource: TtiObject);
begin

end;

{ TBaseMapOptions }

procedure TBaseMapOptions.SetGroupName(const Value: string);
begin
  if FGroupName = Value then
    exit;

  FGroupName := Value;
  NotifyObservers;
end;

{ TMapGeneralProjectOptions }

function TMapGeneralProjectOptions.GetOutputDirectory: string;
begin
  result := IncludeTrailingPathDelimiter(BaseDirectory) + OrigOutDirectory;
end;

procedure TMapGeneralProjectOptions.SetBaseDirectory(const Value: string);
begin
  if FBaseDirectory = Value then
    exit;

  FBaseDirectory := Value;
  NotifyObservers;
end;

procedure TMapGeneralProjectOptions.SetOrigOutDirectory(const Value: string);
begin
  if FOrigOutDirectory = Value then
    exit;

  FOrigOutDirectory := Value;
  NotifyObservers;
end;

procedure TMapGeneralProjectOptions.SetProjectName(const Value: string);
begin
  if FProjectName = Value then
    exit;

  FProjectName := Value;
  NotifyObservers;
end;

{ TMapCodeGenerationProjectOptions }

procedure TMapCodeGenerationProjectOptions.SetBeginEndTabs(
  const Value: integer);
begin
  if FBeginEndTabs = Value then
    exit;

  FBeginEndTabs := Value;
  NotifyObservers;
end;

procedure TMapCodeGenerationProjectOptions.SetMaxEditorCodeWidth(
  const Value: integer);
begin
  if FMaxEditorCodeWidth = Value then
    exit;

  FMaxEditorCodeWidth := Value;
  NotifyObservers;
end;

procedure TMapCodeGenerationProjectOptions.SetTabSpaces(const Value: integer);
begin
  if FTabSpaces = Value then
    exit;

  FTabSpaces := Value;
  NotifyObservers;
end;

procedure TMapCodeGenerationProjectOptions.SetVisibilityTabs(
  const Value: integer);
begin
  if FVisibilityTabs = Value then
    exit;

  FVisibilityTabs := Value;
  NotifyObservers;
end;

{ TMapDatabaseProjectOptions }

procedure TMapDatabaseProjectOptions.SetDoubleQuoteDBFieldNames(
  const Value: boolean);
begin
  if FDoubleQuoteDBFieldNames = Value then
    exit;

  FDoubleQuoteDBFieldNames := Value;
  NotifyObservers;
end;

procedure TMapDatabaseProjectOptions.SetEnumerationType(const Value: TEnumType);
begin
  if FEnumerationType = Value then
    exit;

  FEnumerationType := Value;
  NotifyObservers;
end;

initialization
  ValidatorStringClass := TValidatorStringGenerator;

end.

