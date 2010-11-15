unit app_model;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}


interface

uses
  Classes
  ,SysUtils
  ,tiObject
  ,mapper
  ,mvc_base
  ,fpc_schema_reader
  ,event_const
  ;

type

  // -----------------------------------------------------------------
  //  Enumerations
  // -----------------------------------------------------------------

  TModelProjectState = (mpsClosed, mpsLoaded, mpsChanged);

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Application Model. }
  TAppModel = class(TtiObject)
  private
    FCurrentClasses: TMapClassDefList;
    FCurrentEnums: TMapEnumList;
    FCurrentUnit: TMapUnitDef;
    FProject: TMapProject;
    FState: TModelProjectState;
    procedure SetCurrentUnit(const AValue: TMapUnitDef);
    procedure SetProject(const AValue: TMapProject);
    procedure SetState(const AValue: TModelProjectState);
  public
    property    Project: TMapProject read FProject write SetProject;
    property    State: TModelProjectState read FState write SetState;
    property    CurrentClasses: TMapClassDefList read FCurrentClasses;
    property    CurrentEnums: TMapEnumList read FCurrentEnums;
    function    LoadProject(const AFile: string): boolean;
    procedure   SaveProject;
    procedure   SaveProjectAs(const AFileName: string);
    procedure   CloseProject;
    procedure   SelectClassesForUnit(AUnit: TMapUnitDef);
    procedure   SelectEnumsForUnit(AUnit: TMapUnitDef);
    procedure   ClearUnitSelection;
    class function Instance: TAppModel;
    constructor Create; override;
    destructor  Destroy; override;
  end;

implementation

var
  mModel: TAppModel;

function gAppModel: TAppModel;
begin
  if mModel = nil then
    mModel := TAppModel.Create;
  result := mModel;
end;

{ TAppModel }

procedure TAppModel.ClearUnitSelection;
begin
  FCurrentClasses.Clear;
  FCurrentEnums.Clear;
end;

procedure TAppModel.CloseProject;
begin
  gEventManager.DispatchEvent(gCreateQuickEvent(BEFORE_PROJ_CLOSE, self));
  FProject.Units.Clear;
  FProject.Includes.Clear;
  FCurrentClasses.Clear;
  FCurrentClasses.NotifyObservers;
  FCurrentEnums.Clear;
  FCurrentEnums.NotifyObservers;
  State := mpsClosed;
  gEventManager.DispatchEvent(gCreateQuickEvent(PROJ_CLOSED, self));
end;

constructor TAppModel.Create;
begin
  inherited Create;
  if mModel <> nil then
    raise Exception.Create(ClassName + '.Create: Only one instance allowed.');

  FCurrentClasses := TMapClassDefList.Create;
  FCurrentClasses.OwnsObjects := false;
  FCurrentEnums := TMapEnumList.Create;
  FCurrentEnums.OwnsObjects := false;

  FProject := TMapProject.Create;
end;

destructor TAppModel.Destroy;
begin
  FCurrentClasses.Clear;
  FCurrentClasses.Free;
  FCurrentEnums.Free;

  if FProject <>nil then
    FProject.Free;
  inherited Destroy;
end;

class function TAppModel.Instance: TAppModel;
begin
  if mModel = nil then
    begin
      mModel := TAppModel.Create;
    end;
  result := mModel;
end;

function TAppModel.LoadProject(const AFile: string): boolean;
var
  lReader: TFPCSchemaXMLReader;
begin

  lReader := TFPCSchemaXMLReader.Create;
  try
    lReader.ReadSchema(FProject, AFile);
  finally
    lReader.free;
  end;

  // Change State
  State := mpsLoaded;
  // Send event notifcation of project loaded status
  gEventManager.DispatchEvent(gCreateQuickEvent(PROJ_LOADED, self));
end;

procedure TAppModel.SaveProject;
var
  lProjWriter: TProjectWriter;
begin
  lProjWriter := TProjectWriter.Create;
  try
    lProjWriter.WriteProject(FProject, FProject.FileName);
    gEventManager.DispatchEvent(gCreateQuickEvent(PROJ_SAVED, self));
  finally
    lProjWriter.Free;
  end;
end;

procedure TAppModel.SaveProjectAs(const AFileName: string);
var
  lProjWriter: TProjectWriter;
begin
  lProjWriter := TProjectWriter.Create;
  try
    lProjWriter.WriteProject(FProject, AFileName);
  finally
    lProjWriter.Free;
  end;
end;

procedure TAppModel.SelectClassesForUnit(AUnit: TMapUnitDef);
var
  lCtr: integer;
  lController: TMVCController;
begin

  if AUnit = nil then
    exit;

  FCurrentClasses.Clear;

  if AUnit.UnitClasses = nil then
    raise exception.Create('Unit classes not created');

  for lCtr := 0 to AUnit.UnitClasses.Count - 1 do
    begin
      FCurrentClasses.Add(AUnit.UnitClasses.Items[lCtr]);
    end;

  FCurrentClasses.SortByProps(['BaseClassName']);

end;

procedure TAppModel.SelectEnumsForUnit(AUnit: TMapUnitDef);
var
  lCtr: Integer;
begin

  if AUnit = nil then
    exit;

  FCurrentEnums.Clear;

  if AUnit.UnitEnums = nil then
    raise Exception.Create('Unit enums lsit not created');

  for lCtr := 0 to AUnit.UnitEnums.Count - 1 do
    begin
      FCurrentEnums.Add(AUnit.UnitEnums.Items[lCtr]);
    end;

  FCurrentEnums.SortByProps(['EnumName']);
end;

procedure TAppModel.SetCurrentUnit(const AValue: TMapUnitDef);
begin
  if FCurrentUnit=AValue then exit;
  FCurrentUnit:=AValue;
end;

procedure TAppModel.SetProject(const AValue: TMapProject);
begin
  if FProject=AValue then exit;
  FProject:=AValue;
end;

procedure TAppModel.SetState(const AValue: TModelProjectState);
begin
  if FState=AValue then exit;
  FState:=AValue;
end;

initialization
finalization;
  if mModel <> nil then
    mModel.free;
end.

