unit Transit_BOM;

interface

uses
  tiObject
  , tiVisitor
  , tiOID
  , tiOIDInteger
  , tiHelperClasses;

type

  TShowMethodCollection = class;
  TPeriodCollection = class;
  TMidpointListCollection = class;
  TRTSQuestionCollection = class;
  TSuperList = class;

  TTransit = class(TtiObject)
  private
    FSuperList: TSuperList;
    function GetRTSQuestions: TRTSQuestionCollection;
    function GetMidpointLists: TMidpointListCollection;
    function GetPeriods: TPeriodCollection;
    function GetShowMethods: TShowMethodCollection;
  protected
    function GetCaption: String; override;
  public
    procedure AssignClassProps(ASource: TtiObject); override;
    constructor Create; override;
    destructor Destroy; override;

    //SuperList not published, to prevent double-listing in tiGUIs
    //SuperList is a list of all lists - for convenient GUI grouping
    property SuperList: TSuperList read FSuperList;
  published
    property ShowMethods: TShowMethodCollection read GetShowMethods;
    property Periods: TPeriodCollection read GetPeriods;
    property MidpointLists: TMidpointListCollection read GetMidpointLists;
    property RTSQuestions: TRTSQuestionCollection read GetRTSQuestions;
  end;

  TSuperList = class(TtiObjectList)
  private
  protected
    function GetCaption: String; override;
    function GetItems(I: Integer): TtiObjectList; reintroduce;
    procedure SetItems(I: Integer; const AValue: TtiObjectList); reintroduce;
    function GetOwner: TTransit; reintroduce;
    procedure SetOwner(const AValue: TTransit); reintroduce;
  public
    property Items[I: Integer]: TtiObjectList read GetItems write SetItems; default;
    procedure Add(const AObject: TtiObjectList); reintroduce; overload;
    property Owner: TTransit read GetOwner write SetOwner;
  published
  end;

  TPeriod = class;
  TPeriodCollection = class(TtiSkeletonList)
  private
  protected
    function GetItems(I: Integer): TPeriod; reintroduce;
    procedure SetItems(I: Integer; const AValue: TPeriod); reintroduce;
    function GetOwner: TTransit; reintroduce;
    procedure SetOwner(const AValue: TTransit); reintroduce;
    function GetCaption: String; override;
  public
    procedure Add(const AObject: TPeriod); reintroduce; overload;
    function Add: TPeriod; overload;
    property Items[I: Integer]: TPeriod read GetItems write SetItems; default;
    property Owner: TTransit read GetOwner write SetOwner;
  published
  end;

  TPeriod = class(TtiObject)
  private
  protected
    function GetOwner: TPeriodCollection; reintroduce;
    procedure SetOwner(const AValue: TPeriodCollection); reintroduce;
  public
    property Owner: TPeriodCollection read GetOwner write SetOwner;
  published
  end;

  TShowMethod = class;
  TShowMethodCollection = class(TtiSkeletonList)
  private
  protected
    function GetCaption: String; override;
    function GetItems(I: Integer): TShowMethod; reintroduce;
    procedure SetItems(I: Integer; const AValue: TShowMethod); reintroduce;
    function GetOwner: TTransit; reintroduce;
    procedure SetOwner(const AValue: TTransit); reintroduce;
  public
    property Items[I: Integer]: TShowMethod read GetItems write SetItems; default;
    procedure Add(const AObject: TShowMethod); reintroduce; overload;
    function Add: TShowMethod; overload;
    property Owner: TTransit read GetOwner write SetOwner;
  published
  end;

  TShowMethod = class(TtiObject)
  private
    FName: String;
  protected
    function GetCaption: String; override;
    function GetOwner: TShowMethodCollection; reintroduce;
    procedure SetOwner(const AValue: TShowMethodCollection); reintroduce;
  public
    function IsValid(const AErrors: TtiObjectErrors): Boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    property Owner: TShowMethodCollection read GetOwner write SetOwner;
  published
    property Name: String read FName write FName;
  end;

  //MidpointLists and Midpoints have a One-2-Many relationship
  TMidpointList = class;
  TMidpointListCollection = class(TtiSkeletonList)
  private
  protected
    function GetCaption: String; override;
	  function GetItems(I: Integer): TMidpointList; reintroduce;
	  procedure SetItems(I: Integer; const AValue: TMidpointList); reintroduce;
	  function GetOwner: TTransit; reintroduce;
	  procedure SetOwner(const AValue: TTransit); reintroduce;
  public
	  property Items[I: Integer]: TMidpointList read GetItems write SetItems; default;
	  procedure Add(const AObject : TMidpointList); reintroduce; overload;
	  function Add: TMidpointList; overload;
	  property Owner: TTransit read GetOwner write SetOwner;
  published
  end;

  TMidpointCollection = class;
  TMidpointList = class(TtiObject)
  private
    FName: String;
    FMidpointCollection: TMidpointCollection;
  protected
    function GetCaption: String; override;
		function GetOwner: TMidpointListCollection; reintroduce;
		procedure SetOwner(const AValue: TMidpointListCollection); reintroduce;
  public
    procedure AssignClassProps(ASource: TtiObject); override;
    constructor Create; override;
    destructor Destroy; override;
    property Owner: TMidpointListCollection read GetOwner write SetOwner;
  published
    property Midpoints: TMidpointCollection read FMidpointCollection;
    property Name: String read FName write FName;
  end;

  TMidpoint = class;
  TMidpointCollection = class(TtiSkeletonList)
  private
  protected
	  function GetItems(I: Integer): TMidpoint; reintroduce;
	  procedure SetItems(I: Integer; const AValue: TMidpoint); reintroduce;
	  function GetOwner: TMidpointList; reintroduce;
	  procedure SetOwner(const AValue: TMidpointList); reintroduce;
    function GetOID: TOID; override;
  public
	  property Items[I: Integer]: TMidpoint read GetItems write SetItems; default;
	  procedure Add(const AObject : TMidpoint); reintroduce; overload;
	  function Add: TMidpoint; overload;
	  property Owner: TMidpointList read GetOwner write SetOwner;
  published
  end;

  TMidpoint = class(TtiObject)
  private
    FName: String;
    FPunch: Integer;
    FValue: Double;
  protected
    function GetCaption: String; override;
		function GetOwner: TMidpointCollection; reintroduce;
		procedure SetOwner(const AValue: TMidpointCollection); reintroduce;
  public
    function IsValid(const AErrors: TtiObjectErrors): Boolean; override;
    property Owner: TMidpointCollection read GetOwner write SetOwner;
  published
    property Punch: Integer read FPunch write FPunch;
    property Value: Double read FValue write FValue;
    property Name: String read FName write FName;
  end;

  //RTSQuestion has an association relationship with MidpointList
  TRTSQuestion = class;
  TRTSQuestionCollection = class(TtiSkeletonList)
  private
  protected
    function GetCaption: String; override;
	  function GetItems(I: Integer): TRTSQuestion; reintroduce;
	  procedure SetItems(I: Integer; const AValue: TRTSQuestion); reintroduce;
	  function GetOwner: TTransit; reintroduce;
	  procedure SetOwner(const AValue: TTransit); reintroduce;
  public
	  property Items[I: Integer]: TRTSQuestion read GetItems write SetItems; default;
	  procedure Add(const AObject : TRTSQuestion); reintroduce; overload;
	  function Add: TRTSQuestion; overload;
	  property Owner: TTransit read GetOwner write SetOwner;
  published
  end;

  TRTSQuestion = class(TtiObject)
  private
    FName: String;
    FMidpointList: TMidpointList;
    FNumber: String;  //association - not owned
  protected
    function GetCaption: String; override;
		function GetOwner: TRTSQuestionCollection; reintroduce;
		procedure SetOwner(const AValue: TRTSQuestionCollection); reintroduce;
  public
    procedure AssignClassProps(ASource: TtiObject); override;
    property Owner: TRTSQuestionCollection read GetOwner write SetOwner;
  published
    property Name: String read FName write FName;
    property Number: String read FNumber write FNumber;
    property MidpointList: TMidpointList read FMidpointList write FMidpointList;
  end;

  function MapRTSQuestionToMidpointList(RTSQuestion: TRTSQuestion;
    OID: String): TMidpointList;

implementation

uses SysUtils;

resourcestring

  SErrorZeroLengthString = '%s cannot be empty.';
  STransitCaption = 'Transit Manager';
  SShowMethodCollectionCaption = 'Show Methods';
  SPeriodCollectionCaption = 'Periods';
  SSuperListCaption = 'Transit DB';
  SMidpointListCollectionCaption = 'Midpoint Lists';
  SRTSQuestionCollectionCaption = 'RTS Questions';
  SShowMethodNameProperty = 'ShowMethod Name';
  SMidpointNameProperty = 'Midpoint Name';

function ZeroLengthError(const PropertyName: String): String;
begin
  Result := Format(SErrorZeroLengthString, [PropertyName]);
end;

function MapRTSQuestionToMidpointList(RTSQuestion: TRTSQuestion;
  OID: String): TMidpointList;
begin
    Result := nil;
    if Assigned(RTSQuestion.Owner) then
      if Assigned(RTSQuestion.Owner.Owner) then
        Result := TMidpointList(RTSQuestion.Owner.Owner.MidpointLists.Find(OID));
end;

{ TTransit }

procedure TTransit.AssignClassProps(ASource: TtiObject);
begin
  FSuperList.Assign(TTransit(ASource).FSuperList);
end;

constructor TTransit.Create;
begin
  inherited;
  FSuperList := TSuperList.Create;
  FSuperList.Owner := Self;
  FSuperList.ItemOwner := Self;
  FSuperList.Add(TPeriodCollection.Create);
  FSuperList.Add(TShowMethodCollection.Create);
  FSuperList.Add(TMidpointListCollection.Create);
  FSuperList.Add(TRTSQuestionCollection.Create);
end;

destructor TTransit.Destroy;
begin
  FSuperList.Free;
  inherited;
end;

function TTransit.GetCaption: String;
begin
  Result := STransitCaption;
end;

function TTransit.GetMidpointLists: TMidpointListCollection;
begin
  Result := TMidpointListCollection(FSuperList[2]);
end;

function TTransit.GetPeriods: TPeriodCollection;
begin
  Result := TPeriodCollection(FSuperList[1]);
end;

function TTransit.GetRTSQuestions: TRTSQuestionCollection;
begin
  Result := TRTSQuestionCollection(FSuperList[3]);
end;

function TTransit.GetShowMethods: TShowMethodCollection;
begin
  Result := TShowMethodCollection(FSuperList[0]);
end;

{ TSuperList }

procedure TSuperList.Add(const AObject: TtiObjectList);
begin
  inherited Add(AObject);
end;

function TSuperList.GetCaption: String;
begin
  Result := SSuperListCaption;
end;

function TSuperList.GetItems(I: Integer): TtiObjectList;
begin
  Result := TtiObjectList(inherited GetItems(I));
end;

function TSuperList.GetOwner: TTransit;
begin
  Result := TTransit(inherited GetOwner);
end;

procedure TSuperList.SetItems(I: Integer; const AValue: TtiObjectList);
begin
  inherited SetItems(I, AValue);
end;

procedure TSuperList.SetOwner(const AValue: TTransit);
begin
  inherited SetOwner(AValue);
end;

{ TShowMethodCollection }

procedure TShowMethodCollection.Add(const AObject: TShowMethod);
begin
  inherited Add(AObject);
end;

function TShowMethodCollection.Add: TShowMethod;
begin
  Result := TShowMethod.Create;
  Add(Result);
end;

function TShowMethodCollection.GetCaption: String;
begin
  Result := SShowMethodCollectionCaption;
end;

function TShowMethodCollection.GetItems(I: Integer): TShowMethod;
begin
  Result := TShowMethod(inherited GetItems(I));
end;

function TShowMethodCollection.GetOwner: TTransit;
begin
  Result := TTransit(inherited GetOwner);
end;

procedure TShowMethodCollection.SetItems(I: Integer; const AValue: TShowMethod);
begin
  inherited SetItems(I, AValue);
end;

procedure TShowMethodCollection.SetOwner(const AValue: TTransit);
begin
  inherited SetOwner(AValue);
end;

{ TShowMethod }

constructor TShowMethod.Create;
begin
  inherited;
end;

destructor TShowMethod.Destroy;
begin
  inherited;
end;

function TShowMethod.GetCaption: String;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetCaption;
end;

function TShowMethod.GetOwner: TShowMethodCollection;
begin
  Result := TShowMethodCollection(inherited GetOwner);
end;

function TShowMethod.IsValid(const AErrors: TtiObjectErrors): Boolean;
begin
  if FName = '' then
    AErrors.AddError('Name', ZeroLengthError(SShowMethodNameProperty));
  Result := AErrors.Count = 0;
end;

procedure TShowMethod.SetOwner(const AValue: TShowMethodCollection);
begin
  inherited SetOwner(AValue);
end;

{ TPeriodCollection }

procedure TPeriodCollection.Add(const AObject: TPeriod);
begin
  inherited Add(AObject);
end;

function TPeriodCollection.Add: TPeriod;
begin
  Result := TPeriod.Create;
  inherited Add(Result);
end;

function TPeriodCollection.GetCaption: String;
begin
  Result := SPeriodCollectionCaption;
end;

function TPeriodCollection.GetItems(I: Integer): TPeriod;
begin
  Result := TPeriod(inherited GetItems(I));
end;

function TPeriodCollection.GetOwner: TTransit;
begin
  Result := TTransit(inherited GetOwner);
end;

procedure TPeriodCollection.SetItems(I: Integer; const AValue: TPeriod);
begin
  inherited SetItems(I, AValue);
end;

procedure TPeriodCollection.SetOwner(const AValue: TTransit);
begin
  inherited SetOwner(AValue);
end;

{ TPeriod }

function TPeriod.GetOwner: TPeriodCollection;
begin
  Result := TPeriodCollection(inherited GetOwner);
end;

procedure TPeriod.SetOwner(const AValue: TPeriodCollection);
begin
  inherited SetOwner(AValue);
end;

{ TMidpointListCollection }

procedure TMidpointListCollection.Add(const AObject: TMidpointList);
begin
  inherited Add(AObject);
end;

function TMidpointListCollection.Add: TMidpointList;
begin
  Result := TMidpointList.Create;
  Add(Result);
end;

function TMidpointListCollection.GetCaption: String;
begin
  Result := SMidpointListCollectionCaption;
end;

function TMidpointListCollection.GetItems(I: Integer): TMidpointList;
begin
  Result := TMidpointList(inherited GetItems(I));
end;

function TMidpointListCollection.GetOwner: TTransit;
begin
  Result := TTransit(inherited GetOwner);
end;

procedure TMidpointListCollection.SetItems(I: Integer;
  const AValue: TMidpointList);
begin
  inherited SetItems(I, AValue);
end;

procedure TMidpointListCollection.SetOwner(const AValue: TTransit);
begin
  inherited SetOwner(AValue);
end;

{ TMidpointList }

procedure TMidpointList.AssignClassProps(ASource: TtiObject);
begin
  FMidpointCollection.Assign(TMidpointList(ASource).FMidpointCollection);
end;

constructor TMidpointList.Create;
begin
  inherited;
  FMidpointCollection := TMidpointCollection.Create;
  FMidpointCollection.Owner := Self;
end;

destructor TMidpointList.Destroy;
begin
  FMidpointCollection.Free;
  inherited;
end;

function TMidpointList.GetCaption: String;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetCaption;
end;

function TMidpointList.GetOwner: TMidpointListCollection;
begin
  Result := TMidpointListCollection(inherited GetOwner);
end;

procedure TMidpointList.SetOwner(const AValue: TMidpointListCollection);
begin
  inherited SetOwner(AValue);
end;

{ TMidpointCollection }

procedure TMidpointCollection.Add(const AObject: TMidpoint);
begin
  inherited Add(AObject);
end;

function TMidpointCollection.Add: TMidpoint;
begin
  Result := TMidpoint.Create;
  Add(Result);
end;

function TMidpointCollection.GetItems(I: Integer): TMidpoint;
begin
  Result := TMidpoint(inherited GetItems(I));
end;

function TMidpointCollection.GetOID: TOID;
begin
  Result := Owner.OID;
end;

function TMidpointCollection.GetOwner: TMidpointList;
begin
  Result := TMidpointList(inherited GetOwner);
end;

procedure TMidpointCollection.SetItems(I: Integer; const AValue: TMidpoint);
begin
  inherited SetItems(I, AValue);
end;

procedure TMidpointCollection.SetOwner(const AValue: TMidpointList);
begin
  inherited SetOwner(AValue);
end;

{ TMidpoint }

function TMidpoint.GetCaption: String;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetCaption;
end;

function TMidpoint.GetOwner: TMidpointCollection;
begin
  Result := TMidpointCollection(inherited GetOwner);
end;

function TMidpoint.IsValid(const AErrors: TtiObjectErrors): Boolean;
begin
  if FName = '' then
    AErrors.AddError('Name', ZeroLengthError(SMidpointNameProperty));
  Result := AErrors.Count = 0;
end;

procedure TMidpoint.SetOwner(const AValue: TMidpointCollection);
begin
  inherited SetOwner(AValue);
end;

{ TRTSQuestionCollection }

procedure TRTSQuestionCollection.Add(const AObject: TRTSQuestion);
begin
  inherited Add(AObject);
end;

function TRTSQuestionCollection.Add: TRTSQuestion;
begin
  Result := TRTSQuestion.Create;
  Add(Result);
end;

function TRTSQuestionCollection.GetCaption: String;
begin
  Result := SRTSQuestionCollectionCaption;
end;

function TRTSQuestionCollection.GetItems(I: Integer): TRTSQuestion;
begin
  Result := TRTSQuestion(inherited GetItems(I));
end;

function TRTSQuestionCollection.GetOwner: TTransit;
begin
  Result := TTransit(inherited GetOwner);
end;

procedure TRTSQuestionCollection.SetItems(I: Integer;
  const AValue: TRTSQuestion);
begin
  inherited SetItems(I, AValue);
end;

procedure TRTSQuestionCollection.SetOwner(const AValue: TTransit);
begin
  inherited SetOwner(AValue);
end;

{ TRTSQuestion }

procedure TRTSQuestion.AssignClassProps(ASource: TtiObject);
begin
  FMidpointList := TRTSQuestion(ASource).FMidpointList;
end;

function TRTSQuestion.GetCaption: String;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetCaption;
end;

function TRTSQuestion.GetOwner: TRTSQuestionCollection;
begin
  Result := TRTSQuestionCollection(inherited GetOwner);
end;

procedure TRTSQuestion.SetOwner(const AValue: TRTSQuestionCollection);
begin
  inherited SetOwner(AValue);
end;

end.
