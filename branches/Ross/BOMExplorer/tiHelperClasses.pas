unit tiHelperClasses;

interface

uses tiObject, tiVisitor;

type
  //TtiSkeleotonX classes implement a method of unloading persistent objects
  //i.e. allows for purges of the cache of in-memory objects loaded from disk

  //TtiSkeletonObject is used by owners of loaded singletons (0..1 relations)
  TtiSkeletonObject = class(TtiObject)
  public
    procedure Purge; virtual;
  end;

  //TtiSkeletonList is be used for 1..n relations
  TtiSkeletonList = class(TtiObjectList)
  public
    procedure Purge; virtual;
  end;

  TVisSkeletonPurge = class(TtiVisitor)
  protected
    function AcceptVisitor: Boolean; override;
  public
    procedure Execute(const AVisited: TtiVisited); override;
  end;

  TVisFreeDeleted = class(TtiVisitor)
  protected
    function AcceptVisitor: Boolean; override;
  public
    procedure Execute(const AVisited: TtiVisited); override;
  end;

  TVisDetectDeleted = class(TtiVisitor)
  private
    FDetected: boolean;
  protected
    function AcceptVisitor: Boolean; override;
  public
    procedure Execute(const AVisited: TtiVisited); override;
    property Detected: Boolean read FDetected write FDetected;
  end;





//GOTCHA: this class is made necessary because I need a skeleton into which
//tables of objects get loaded (as owned objeects of the skeleton).
//The skeleton objects, when read, get an ObjectState of posClean,
//but there's no indication that the skeleton itself is permanent/created statically
//rather than from the database

//this information is sometimes helpful in BOM explorer (e.g. when refreshing
//an object from the database)

//ideally, this function would exist in TtiObject, or be implemented as an
//interface, so it could appy to both objects and object lists

implementation

{ TtiSkeletonObject }

procedure TtiSkeletonObject.Purge;
begin
  //do nothing - only subclasses know how to purge themselves
end;

{ TtiSkeletonList }

procedure TtiSkeletonList.Purge;
begin
  Clear;
end;

{ TVisPurge }

function TVisSkeletonPurge.AcceptVisitor: Boolean;
begin
  Result := (Visited is TtiSkeletonObject) or (Visited is TtiSkeletonList);
end;

procedure TVisSkeletonPurge.Execute(const AVisited: TtiVisited);
begin
  inherited;
  if not AcceptVisitor then
    Exit;
  if Visited is TtiSkeletonObject then
    TtiSkeletonObject(Visited).Purge
  else
    TtiSkeletonList(Visited).Purge;
  TtiObject(Visited).ObjectState := posEmpty;
end;

{ TVisFreeDeleted }

function TVisFreeDeleted.AcceptVisitor: Boolean;
begin
  Result := (Visited is TtiObject)
    and (TtiObject(Visited).ObjectState = posDeleted);
end;

procedure TVisFreeDeleted.Execute(const AVisited: TtiVisited);
var
  AData: TtiObject;
begin
  inherited;
  if not AcceptVisitor then
    Exit;

  {GOTCHA: FreeDeleted assumes that Visited's class implements free
  notification, unless the owner is a TtiObjectList, in which case the issue is
  avoided by calling TtiObjectList.Remove}
  AData := TtiObject(Visited);
  if AData.Owner is TtiObjectList then
    TtiObjectLIst(AData.Owner).Remove(AData)
  else
    //GOTCHA: this will blow up if concrete class doesn't notify its owner
    AData.Free;
end;

{ TVisDetectDeleted }

function TVisDetectDeleted.AcceptVisitor: Boolean;
begin
  Result := (Visited is TtiObject) and (not Detected);
end;

procedure TVisDetectDeleted.Execute(const AVisited: TtiVisited);
begin
  inherited;
  if not AcceptVisitor then
    Exit;
  FDetected := TtiObject(Visited).ObjectState = posDeleted;
end;

end.
