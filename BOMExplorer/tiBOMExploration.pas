unit tiBOMExploration;

{
These classes describe and manage the information required by the
BOM Explorer (BOME) to explore a tree of BOM objects.

The BOMEDictionary is a singleton containing a list of registered ExplorableBOMs.

An ExplorableBOM is a name associated with one or more 'Aliases', 'Registrars',
'Views' and 'ViewPoints'.

An Alias is the set of information required to connect to a database.

A BOMRegistrar is responsible for registering automaps or visitors.

A BOMEView is a set of all the class-related information required to explore
a tree of objects instantiated from a particular BOM.

A BOMEViewPoint is a named root to start exploring from.
}

interface

uses
  Classes
  , tiObject
  , tiVTTreeView;

type
  TtiRegistrarKind = (rkNone, rkAutoMap, rkDBIndependant, rkHardCode, rkOther);

  TtiBOMEViewPoint = class;
  TtiBOMEViewPointList = class;
  TtiBOMEView = class;
  TtiBOMEViewList = class;
  TtiBOMERegistrar = class;
  TtiBOMERegistrarList = class;
  TtiAlias = class;
  TtiAliasList = class;
  TtiExplorableBOM = class;
  TtiExplorableBOMList = class;

  TtiBOMEDictionary = class(TtiObject)
  private
    FBOMs: TtiExplorableBOMList;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property BOMs: TtiExplorableBOMList read FBOMs;
  end;

  TtiExplorableBOMList = class(TtiObjectList)
  private
  protected
    function GetItems(I: Integer): TtiExplorableBOM; reintroduce;
    procedure SetItems(I: Integer; const AValue: TtiExplorableBOM); reintroduce;
    function GetOwner: TtiBOMEDictionary; reintroduce;
    procedure SetOwner(const AValue: TtiBOMEDictionary); reintroduce;
  public
    property Items[I: Integer]: TtiExplorableBOM read GetItems write SetItems; default;
    procedure Add(const AObject: TtiExplorableBOM); reintroduce; overload;
    function Add: TtiExplorableBOM; overload;
    property Owner: TtiBOMEDictionary read GetOwner write SetOwner;
  published
  end;

  TtiExplorableBOM = class(TtiObject)
  private
    FAliases: TtiAliasList;
    FRegistrars: TtiBOMERegistrarList;
    FViews: TtiBOMEViewList;
    FName: String;
    FViewPoints: TtiBOMEViewPointList;
  public
    function AddAlias(const AName, APerLayerName, ADatabaseName: String;
      const AUserName: String = ''; const APassword: String = '';
      AParams: String = ''): TtiAlias;
    constructor Create; override;
    destructor Destroy; override;
  published
    property Aliases: TtiAliasList read FAliases;
    property Registrars: TtiBOMERegistrarList read FRegistrars;
    property Views: TtiBOMEViewList read FViews;
    property ViewPoints: TtiBOMEViewPointList read FViewPoints;
    property Name: String read FName write FName;
  end;

  TtiAliasList = class(TtiObjectList)
  private
  protected
    function GetItems(I: Integer): TtiAlias; reintroduce;
    procedure SetItems(I: Integer; const AValue: TtiAlias); reintroduce;
    function GetOwner: TtiExplorableBOM; reintroduce;
    procedure SetOwner(const AValue: TtiExplorableBOM); reintroduce;
  public
    property Items[I: Integer]: TtiAlias read GetItems write SetItems; default;
    procedure Add(const AObject: TtiAlias); reintroduce; overload;
    function Add: TtiAlias; overload;
    property Owner: TtiExplorableBOM read GetOwner write SetOwner;
  published
  end;

  TtiAlias = class(TtiObject)
  private
    FName: String;
    FPerLayerName: String;
    FDatabaseName: String;
    FPassword: String;
    FUserName: String;
    FParams: String;
  protected
    function GetOwner: TtiAliasList; reintroduce;
    procedure SetOwner(const AValue: TtiAliasList); reintroduce;
  public
    constructor Create; override;
    property Owner: TtiAliasList read GetOwner write SetOwner;
  published
    property Name: String read FName write FName;
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property PerLayerName: String read FPerLayerName write FPerLayerName;
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
    property Params: String read FParams write FParams;
  end;

  TtiBOMERegistrarList = class(TtiObjectList)
  private
  protected
    function GetItems(I: Integer): TtiBOMERegistrar; reintroduce;
    procedure SetItems(I: Integer; const AValue: TtiBOMERegistrar); reintroduce;
    function GetOwner: TtiExplorableBOM; reintroduce;
    procedure SetOwner(const AValue: TtiExplorableBOM); reintroduce;
  public
    property Items[I: Integer]: TtiBOMERegistrar read GetItems write SetItems; default;
    procedure Add(const AObject: TtiBOMERegistrar); reintroduce; overload;
    function Add: TtiBOMERegistrar; overload;
    property Owner: TtiExplorableBOM read GetOwner write SetOwner;
  published
  end;

  TtiBOMERegistrar = class(TtiObject)
  private
    FName: String;
    FKind: TtiRegistrarKind;
    FRegistered: Boolean;
  protected
    function GetOwner: TtiBOMERegistrarList; reintroduce;
    procedure SetOwner(const AValue: TtiBOMERegistrarList); reintroduce;
    procedure DoRegisterItems; virtual;
    procedure DoUnregisterItems; virtual;
  public
    constructor Create; override;
    procedure RegisterItems;
    procedure UnregisterItems;
    property Owner: TtiBOMERegistrarList read GetOwner write SetOwner;
  published
    property Kind: TtiRegistrarKind read FKind write FKind default rkNone;
    property Name: String read FName write FName;
    property Registered: Boolean read FRegistered;
  end;

  TtiBOMEAutomapRegistrar = class(TtiBOMERegistrar)
  protected
    procedure DoUnregisterItems; override;
  public
    constructor Create; override;
  end;

  TtiBOMEVisitorRegistrar = class(TtiBOMERegistrar)
  protected
    procedure DoUnregisterItems; override;
  end;

  TtiBOMEDBIndependantRegistrar = class(TtiBOMEVisitorRegistrar)
  public
    constructor Create; override;
  end;

  TtiBOMEHardCodeRegistrar = class(TtiBOMEVisitorRegistrar)
  public
    constructor Create; override;
  end;

  TtiBOMEViewList = class(TtiObjectList)
  private
  protected
    function GetItems(I: Integer): TtiBOMEView; reintroduce;
    procedure SetItems(I: Integer; const AValue: TtiBOMEView); reintroduce;
    function GetOwner: TtiExplorableBOM; reintroduce;
    procedure SetOwner(const AValue: TtiExplorableBOM); reintroduce;
  public
    property Items[I: Integer]: TtiBOMEView read GetItems write SetItems; default;
    procedure Add(const AObject: TtiBOMEView); reintroduce; overload;
    function Add: TtiBOMEView; overload;
    property Owner: TtiExplorableBOM read GetOwner write SetOwner;
  published
  end;

  TtiBOMEView = class(TtiObject)
  private
    FName: String;
    FTreeMappings: TtiVTTVDataMappings;
  protected
    function GetOwner: TtiBOMEViewList; reintroduce;
    procedure SetOwner(const AValue: TtiBOMEViewList); reintroduce;
  public
    function AddTreeMapping(AClass: TtiClass;
      ACanEdit: Boolean = False;
      AOnInsert: TtiVTTVNodeEvent = nil;
      AOnDelete: TtiVTTVNodeEvent = nil;
      const APropertyName: String = 'Caption'): TtiVTTVDataMapping;
    constructor Create; override;
    destructor Destroy; override;
    property Owner: TtiBOMEViewList read GetOwner write SetOwner;
  published
    property TreeMappings: TtiVTTVDataMappings read FTreeMappings;
    property Name: String read FName write FName;
  end;

  TtiBOMEViewPointList = class(TtiObjectList)
  private
  protected
    function GetItems(I: Integer): TtiBOMEViewPoint; reintroduce;
    procedure SetItems(I: Integer; const AValue: TtiBOMEViewPoint); reintroduce;
    function GetOwner: TtiBOMEView; reintroduce;
    procedure SetOwner(const AValue: TtiBOMEView); reintroduce;
  public
    property Items[I: Integer]: TtiBOMEViewPoint read GetItems write SetItems; default;
    procedure Add(const AObject: TtiBOMEViewPoint); reintroduce; overload;
    function Add: TtiBOMEViewPoint; overload;
    function Add(const AName: String; ARoot: TtiObject): TtiBOMEViewPoint; overload;
    property Owner: TtiBOMEView read GetOwner write SetOwner;
  published
  end;

  TtiBOMEViewPoint = class(TtiObject)
  private
    FName: String;
    FRoot: TtiObject;
  public
    constructor Create; override;
  published
    property Name: String read FName write FName;
    property Root: TtiObject read FRoot write FRoot;
  end;

function BOMEDictionary: TtiBOMEDictionary;

implementation

uses
  tiOPFManager;

var
  uBOMEDictionary: TtiBOMEDictionary;

function BOMEDictionary: TtiBOMEDictionary;
begin
  Result := uBOMEDictionary;
end;

{ TtiBOMEDictionary }

constructor TtiBOMEDictionary.Create;
begin
  inherited;
  FBOMs := TtiExplorableBOMList.Create;
end;

destructor TtiBOMEDictionary.Destroy;
begin
  FBOMs.Free;
  inherited;
end;

{ TtiExplorableBOMList }

procedure TtiExplorableBOMList.Add(const AObject: TtiExplorableBOM);
begin
  inherited Add(AObject);
end;

function TtiExplorableBOMList.Add: TtiExplorableBOM;
begin
  Result := TtiExplorableBOM.Create;
  Add(Result);
end;

function TtiExplorableBOMList.GetItems(I: Integer): TtiExplorableBOM;
begin
  Result := TtiExplorableBOM(inherited GetItems(I));
end;

function TtiExplorableBOMList.GetOwner: TtiBOMEDictionary;
begin
  Result := TtiBOMEDictionary(inherited GetOwner);
end;

procedure TtiExplorableBOMList.SetItems(I: Integer; const AValue: TtiExplorableBOM);
begin
  inherited SetItems(I, AValue);
end;

procedure TtiExplorableBOMList.SetOwner(const AValue: TtiBOMEDictionary);
begin
  inherited SetOwner(AValue);
end;

{ TtiExploreableBOM }

function TtiExplorableBOM.AddAlias(const AName, APerLayerName,
  ADatabaseName, AUserName, APassword: String; AParams: String): TtiAlias;
begin
  Result := Aliases.Add;
  Result.Name := AName;
  Result.PerLayerName := APerLayerName;
  Result.DatabaseName := ADatabaseName;
  Result.UserName := AUserName;
  Result.Password := APassword;
  Result.Params := AParams;
end;

constructor TtiExplorableBOM.Create;
begin
  inherited;
  FAliases := TtiAliasList.Create;
  FRegistrars := TtiBOMERegistrarList.Create;
  FViews := TtiBOMEViewList.Create;
  FViewPoints := TtiBOMEViewPointList.Create;
  FName := '';
end;

destructor TtiExplorableBOM.Destroy;
begin
  FAliases.Free;
  FRegistrars.Free;
  FViews.Free;
  FViewPoints.Free;
  inherited;
end;

{ TtiAliasList }

procedure TtiAliasList.Add(const AObject: TtiAlias);
begin
  inherited Add(AObject);
end;

function TtiAliasList.Add: TtiAlias;
begin
  Result := TtiAlias.Create;
  Add(Result);
end;

function TtiAliasList.GetItems(I: Integer): TtiAlias;
begin
  Result := TtiAlias(inherited GetItems(I));
end;

function TtiAliasList.GetOwner: TtiExplorableBOM;
begin
  Result := TtiExplorableBOM(inherited GetOwner);
end;

procedure TtiAliasList.SetItems(I: Integer; const AValue: TtiAlias);
begin
  inherited SetItems(I, AValue);
end;

procedure TtiAliasList.SetOwner(const AValue: TtiExplorableBOM);
begin
  inherited SetOwner(AValue);
end;

{ TtiAlias }

constructor TtiAlias.Create;
begin
  inherited;
  FParams := '';
  FName := '';
  FPerLayerName := '';
  FDatabaseName := '';
  FPassword := '';
  FUserName := '';
end;

function TtiAlias.GetOwner: TtiAliasList;
begin
  Result := TtiAliasList(inherited GetOwner);
end;

procedure TtiAlias.SetOwner(const AValue: TtiAliasList);
begin
  inherited SetOwner(AValue);
end;

{ TiBOMERegistrarList }

procedure TtiBOMERegistrarList.Add(const AObject: TtiBOMERegistrar);
begin
  inherited Add(AObject);
end;

function TtiBOMERegistrarList.Add: TtiBOMERegistrar;
begin
  Result := TtiBOMERegistrar.Create;
  Add(Result);
end;

function TtiBOMERegistrarList.GetItems(I: Integer): TtiBOMERegistrar;
begin
  Result := TtiBOMERegistrar(inherited GetItems(I));
end;

function TtiBOMERegistrarList.GetOwner: TtiExplorableBOM;
begin
  Result := TtiExplorableBOM(inherited GetOwner);
end;

procedure TtiBOMERegistrarList.SetItems(I: Integer; const AValue: TtiBOMERegistrar);
begin
  inherited SetItems(I, AValue);
end;

procedure TtiBOMERegistrarList.SetOwner(const AValue: TtiExplorableBOM);
begin
  inherited SetOwner(AValue);
end;

{ TtiBOMERegistrar }

constructor TtiBOMERegistrar.Create;
begin
  inherited;
  FName := '';
  FKind := rkNone;
  FRegistered := False;
end;

procedure TtiBOMERegistrar.DoRegisterItems;
begin
  FRegistered := True;
end;

procedure TtiBOMERegistrar.DoUnregisterItems;
begin
  FRegistered := False;
end;

function TtiBOMERegistrar.GetOwner: TtiBOMERegistrarList;
begin
  Result := TtiBOMERegistrarList(inherited GetOwner);
end;

procedure TtiBOMERegistrar.RegisterItems;
begin
  if Registered then
    Exit;
  DoRegisterItems;
end;

procedure TtiBOMERegistrar.SetOwner(const AValue: TtiBOMERegistrarList);
begin
  inherited SetOwner(AValue);
end;

procedure TtiBOMERegistrar.UnregisterItems;
begin
  if not Registered then
    Exit;
  DoUnregisterItems;
end;

{ TtiAutomapRegistrar }

constructor TtiBOMEAutomapRegistrar.Create;
begin
  inherited;
  FKind := rkAutomap;
end;

{ TtiDBIndependent }

constructor TtiBOMEDBIndependantRegistrar.Create;
begin
  inherited;
  FKind := rkDBIndependant;
end;

procedure TtiBOMEAutomapRegistrar.DoUnregisterItems;
begin
  FRegistered := True; //remains true since AutoMaps can't be unregistered in tiOPF
end;

{ TtiHardCodeRegistrar }

constructor TtiBOMEHardCodeRegistrar.Create;
begin
  inherited;
  FKind := rkHardCode;
end;

{ TtiBOMEViewList }

procedure TtiBOMEViewList.Add(const AObject: TtiBOMEView);
begin
  inherited Add(AObject);
end;

function TtiBOMEViewList.Add: TtiBOMEView;
begin
  Result := TtiBOMEView.Create;
  Add(Result);
end;

function TtiBOMEViewList.GetItems(I: Integer): TtiBOMEView;
begin
  Result := TtiBOMEView(inherited GetItems(I));
end;

function TtiBOMEViewList.GetOwner: TtiExplorableBOM;
begin
  Result := TtiExplorableBOM(inherited GetOwner);
end;

procedure TtiBOMEViewList.SetItems(I: Integer; const AValue: TtiBOMEView);
begin
  inherited SetItems(I, AValue);
end;

procedure TtiBOMEViewList.SetOwner(const AValue: TtiExplorableBOM);
begin
  inherited SetOwner(AValue);
end;

{ TtiBOMEView }

function TtiBOMEView.AddTreeMapping(AClass: TtiClass;
  ACanEdit: Boolean;
  AOnInsert, AOnDelete: TtiVTTVNodeEvent;
  const APropertyName: String): TtiVTTVDataMapping;
begin
  Result := FTreeMappings.Add;
  Result.DataClass := AClass;
  Result.DisplayPropName := APropertyName;
  Result.CanInsert := Assigned(AOnInsert);
  Result.CanDelete := Assigned(AOnDelete);
  Result.OnInsert := AOnInsert;
  Result.OnDelete := AOnDelete;
  Result.CanView := True;
  Result.CanEdit := ACanEdit;
end;

constructor TtiBOMEView.Create;
begin
  inherited;
  FTreeMappings := TtiVTTVDataMappings.Create(nil);
  FName := '';
end;

destructor TtiBOMEView.Destroy;
begin
  FTreeMappings.Free;
  inherited;
end;

function TtiBOMEView.GetOwner: TtiBOMEViewList;
begin
  Result := TtiBOMEViewList(inherited GetOwner);
end;

procedure TtiBOMEView.SetOwner(const AValue: TtiBOMEViewList);
begin
  inherited SetOwner(AValue);
end;

{ TtiBOMEViewPointList }

procedure TtiBOMEViewPointList.Add(const AObject: TtiBOMEViewPoint);
begin
  inherited Add(AObject);
end;

function TtiBOMEViewPointList.Add: TtiBOMEViewPoint;
begin
  Result := TtiBOMEViewPoint.Create;
  Add(Result);
end;

function TtiBOMEViewPointList.Add(const AName: String;
  ARoot: TtiObject): TtiBOMEViewPoint;
begin
  Result := Add;
  Result.Name := AName;
  Result.Root := ARoot;
end;

function TtiBOMEViewPointList.GetItems(I: Integer): TtiBOMEViewPoint;
begin
  Result := TtiBOMEViewPoint(inherited GetItems(I));
end;

function TtiBOMEViewPointList.GetOwner: TtiBOMEView;
begin
  Result := TtiBOMEView(inherited GetOwner);
end;

procedure TtiBOMEViewPointList.SetItems(I: Integer; const AValue: TtiBOMEViewPoint);
begin
  inherited SetItems(I, AValue);
end;

procedure TtiBOMEViewPointList.SetOwner(const AValue: TtiBOMEView);
begin
  inherited SetOwner(AValue);
end;

{ TtiBOMEViewPoint }

constructor TtiBOMEViewPoint.Create;
begin
  inherited;
  FName := '';
end;

{ TtiVisitorRegistrar }

procedure TtiBOMEVisitorRegistrar.DoUnregisterItems;
begin
  inherited;
  {GOTCHA: tiOPF doesn't allow us to UnRegister individual visitors,
  only groups of visitors. Thus, we will unregister all 'read' and 'save'
  visitors, which should achieve the same results}

  {PH: Reasonable to add this functionality if required.}
  
  gTIOPFManager.VisitorManager.UnregisterVisitors(cuStandardTask_Read);
  gTIOPFManager.VisitorManager.UnregisterVisitors(cuStandardTask_Save);
end;

initialization
  uBOMEDictionary := TtiBOMEDictionary.Create;

finalization
  uBOMEDictionary.Free;
end.
