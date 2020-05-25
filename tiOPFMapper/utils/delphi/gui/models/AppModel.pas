unit AppModel;
{$IFDEF fpc}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, tiObject, mapper{,mvc_base, EventsConsts};

type

  // -----------------------------------------------------------------
  //  Enumerations
  // -----------------------------------------------------------------

  TModelProjectState = (mpsClosed, mpsLoaded, mpsChanged);
  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Main application controller model. }

  { TAppModel }

  TAppModel = class(TtiObject)
  private
    FProject: TMapProject;
    FState: TModelProjectState;
    FOnProjectLoaded: TNotifyEvent;
    FOnProjectUnloaded: TNotifyEvent;
    FOnBeforeAppTerminate: TNotifyEvent;
    FOnAfterProjectSaved: TNotifyEvent;
    FOnAppLoaded: TNotifyEvent;
    FCurrentUnit: TMapUnitDef;
    FCurrentEnums: TMapEnumList;
    FCurrentClasses: TMapClassDefList;
    FOnAfterAddClassDef: TNotifyEvent;
    FLastDirectoryUsed: string;
    FOnMRUClick: TNotifyEvent;
    FCurrentPropertyTypes: TMapPropertyTypeList;
    procedure InitPropertyTypes;
    procedure SetProject(const AValue: TMapProject);
    procedure SetState(const AValue: TModelProjectState);
    procedure SetOnBeforeAppTerminate(const Value: TNotifyEvent);
    procedure SetOnProjectLoaded(const Value: TNotifyEvent);
    procedure SetOnProjectUnloaded(const Value: TNotifyEvent);
    procedure SetOnAfterProjectSaved(const Value: TNotifyEvent);
    procedure SetOnAppLoaded(const Value: TNotifyEvent);
    procedure SetCurrentUnit(const Value: TMapUnitDef);
    procedure SetOnAfterAddClassDef(const Value: TNotifyEvent);
    procedure SetLastDirectoryUsed(const Value: string);
    procedure SetOnMRUClick(const Value: TNotifyEvent);
  protected
    procedure RemoveEnumsFromPropertyTypes;
  public
    class function Instance: TAppModel;

    constructor Create; override;
    destructor Destroy; override;

    procedure ClearProject;
    procedure UpdateUnitClasses;
    procedure UpdateUnitEnums;
    function LoadProject(const AFile: string): boolean;
    procedure SaveProject;
    procedure SaveProjectAs(const AFileName: string);
    procedure CreateNewProject(const AFileName: string);
    procedure CloseProject;
    procedure WriteProject(AVerbose: Boolean; AOnWriteClass: TOnWriteClassIntf;
      AOnWriteEnum: TOnWriteEnum; AOnWriteUnit: TOnWriteUnit); overload;

    property Project: TMapProject read FProject write SetProject;
    property State: TModelProjectState read FState write SetState;
    property CurrentPropertyTypes: TMapPropertyTypeList read FCurrentPropertyTypes;
    property CurrentUnit: TMapUnitDef read FCurrentUnit write SetCurrentUnit;
    property CurrentClasses: TMapClassDefList read FCurrentClasses;
    property CurrentEnums: TMapEnumList read FCurrentEnums;
    property LastDirectoryUsed: string read FLastDirectoryUsed write SetLastDirectoryUsed;
    // events
    property OnAppLoaded: TNotifyEvent read FOnAppLoaded write SetOnAppLoaded;
    property OnProjectLoaded: TNotifyEvent read FOnProjectLoaded write SetOnProjectLoaded;
    property OnProjectUnloaded: TNotifyEvent read FOnProjectUnloaded write SetOnProjectUnloaded;
    property OnBeforeAppTerminate: TNotifyEvent read FOnBeforeAppTerminate write SetOnBeforeAppTerminate;
    property OnAfterProjectSaved: TNotifyEvent read FOnAfterProjectSaved write SetOnAfterProjectSaved;
    property OnAfterAddClassDef: TNotifyEvent read FOnAfterAddClassDef write SetOnAfterAddClassDef;
    property OnMRUClick: TNotifyEvent read FOnMRUClick write SetOnMRUClick;
  end;

implementation

uses
  common_schema_reader, mapper_project_writer;

var
  mModel: TAppModel;

{ TAppModel }

procedure TAppModel.ClearProject;
begin
  // remove selections
  RemoveEnumsFromPropertyTypes;
  CurrentUnit := nil;
  CurrentClasses.Clear;
  CurrentEnums.Clear;

//  FProject.Units.Clear;
//  FProject.Includes.Clear;
  Project.ClearAll;
end;

procedure TAppModel.CloseProject;
begin
  ClearProject;

  // remove selections
  CurrentPropertyTypes.NotifyObservers;
  CurrentClasses.NotifyObservers;
  CurrentEnums.NotifyObservers;
  FProject.Units.NotifyObservers;

  State := mpsClosed;

  if Assigned(FOnProjectUnloaded) then
    FOnProjectUnloaded(Self);
end;

constructor TAppModel.Create;
begin
  inherited;

  FProject := TMapProject.Create;
  FCurrentPropertyTypes := TMapPropertyTypeList.Create;

  FCurrentClasses := TMapClassDefList.Create;
  FCurrentClasses.OwnsObjects := False;

  FCurrentEnums := TMapEnumList.Create;
  FCurrentEnums.OwnsObjects := False;
end;

procedure TAppModel.CreateNewProject(const AFileName: string);
var
  lProjWriter: TProjectWriter;
begin
  CloseProject;

  Project.GeneralOptions.ProjectName := 'My New Project';
  Project.FileName := AFileName;

  lProjWriter := TProjectWriter.Create;

  try
    lProjWriter.WriteProject(FProject, FProject.FileName);
  finally
    lProjWriter.Free;
  end;

  LoadProject(AFileName);
end;

destructor TAppModel.Destroy;
begin
  RemoveEnumsFromPropertyTypes;

  FCurrentEnums.Clear;
  FCurrentClasses.Clear;
  FCurrentUnit := nil;

  FCurrentClasses.Free;
  FCurrentEnums.Free;

  FCurrentPropertyTypes.Free;

  if FProject <> nil then
    FProject.Free;

  inherited;
end;

procedure TAppModel.InitPropertyTypes;
begin
  CurrentPropertyTypes.Clear;

  //ptString, ptAnsiString, ptDouble, ptSingle, ptCurrency, ptInteger, ptInt64,
  //ptDateTime, ptBoolean, ptEnum, ptEnumSet, ptStream

  CurrentPropertyTypes.Add('String', ptString);
  CurrentPropertyTypes.Add('AnsiString', ptAnsiString);
  CurrentPropertyTypes.Add('Double', ptDouble);
  CurrentPropertyTypes.Add('Single', ptSingle);
  CurrentPropertyTypes.Add('Currency', ptCurrency);
  CurrentPropertyTypes.Add('Integer', ptInteger);
  CurrentPropertyTypes.Add('Int64', ptInt64);
  CurrentPropertyTypes.Add('TDateTime', ptDateTime);
  CurrentPropertyTypes.Add('Boolean', ptBoolean);
  CurrentPropertyTypes.Add('Enum', ptEnum);

end;

class function TAppModel.Instance: TAppModel;
begin
  if mModel = nil then
    mModel := TAppModel.Create;

  result := mModel;
end;

function TAppModel.LoadProject(const AFile: string): boolean;
var
  lReader: TMapSchemaReader;
begin
  ClearProject;
  InitPropertyTypes;
  // remove selections
  CurrentClasses.NotifyObservers;
  CurrentEnums.NotifyObservers;
  Project.Units.NotifyObservers;

  lReader := gGetSchemaReaderClass.Create;

  try
    lReader.ReadSchema(FProject, AFile);
    FProject.FileName := AFile;
  finally
    lReader.free;
  end;

  Project.Units.NotifyObservers;

  // Change State
  State := mpsLoaded;

  if Assigned(FOnProjectLoaded) then
    FOnProjectLoaded(Self);

  // Update last directory used
  FLastDirectoryUsed := ExtractFilePath(AFile);
end;

procedure TAppModel.RemoveEnumsFromPropertyTypes;
var
  I: Integer;
begin
  if not (Assigned(CurrentEnums) and Assigned(CurrentPropertyTypes)) then
    exit;

  if not Assigned(Project) then
    exit;

  for I := 0 to Project.ProjectEnums.Count - 1 do
    CurrentPropertyTypes.Extract(Project.ProjectEnums[I]);
end;

procedure TAppModel.SaveProject;
var
  lProjWriter: TProjectWriter;
begin
  lProjWriter := TProjectWriter.Create;

  try
    lProjWriter.WriteProject(FProject, FProject.FileName);
    if Assigned(FOnAfterProjectSaved) then
      FOnAfterProjectSaved(Self);
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
    FProject.FileName := AFileName;

    if Assigned(FOnAfterProjectSaved) then
      FOnAfterProjectSaved(Self);
    if Assigned(FOnProjectLoaded) then
      FOnProjectLoaded(Self);
  finally
    lProjWriter.Free;
  end;
end;

procedure TAppModel.SetCurrentUnit(const Value: TMapUnitDef);
begin
  FCurrentUnit := Value;
end;

procedure TAppModel.SetLastDirectoryUsed(const Value: string);
begin
  FLastDirectoryUsed := Value;
end;

procedure TAppModel.SetOnAfterAddClassDef(const Value: TNotifyEvent);
begin
  FOnAfterAddClassDef := Value;
end;

procedure TAppModel.SetOnAfterProjectSaved(const Value: TNotifyEvent);
begin
  FOnAfterProjectSaved := Value;
end;

procedure TAppModel.SetOnAppLoaded(const Value: TNotifyEvent);
begin
  FOnAppLoaded := Value;
end;

procedure TAppModel.SetOnBeforeAppTerminate(const Value: TNotifyEvent);
begin
  FOnBeforeAppTerminate := Value;
end;

procedure TAppModel.SetOnMRUClick(const Value: TNotifyEvent);
begin
  FOnMRUClick := Value;
end;

procedure TAppModel.SetOnProjectLoaded(const Value: TNotifyEvent);
begin
  FOnProjectLoaded := Value;
end;

procedure TAppModel.SetOnProjectUnloaded(const Value: TNotifyEvent);
begin
  FOnProjectUnloaded := Value;
end;

procedure TAppModel.SetProject(const AValue: TMapProject);
begin
  FProject := AValue;
end;

procedure TAppModel.SetState(const AValue: TModelProjectState);
begin
  FState := AValue;
end;

procedure TAppModel.UpdateUnitClasses;
var
  lCtr: Integer;
  lClass: TMapClassDef;
begin
  CurrentClasses.Clear;
  CurrentClasses.NotifyObservers;

  // If no selected unit, then exit out.
  if CurrentUnit = nil then
    Exit;

  for lCtr := 0 to CurrentUnit.UnitClasses.Count - 1 do
  begin
    lClass := CurrentUnit.UnitClasses.Items[lCtr];
    CurrentClasses.Add(lClass);
  end;

  //CurrentClasses.SortByProps(['BaseClassName']);
  CurrentClasses.NotifyObservers;
end;

procedure TAppModel.UpdateUnitEnums;
var
  lCtr: Integer;
  lEnum: TMapEnum;
begin
  CurrentEnums.Clear;
  CurrentEnums.NotifyObservers;

  if CurrentUnit = nil then
    exit;

  for lCtr := 0 to CurrentUnit.UnitEnums.Count - 1 do
  begin
    lEnum := CurrentUnit.UnitEnums.Items[lCtr];
    CurrentEnums.Add(lEnum);
  end;

  //CurrentEnums.SortByProps(['EnumName']);
  CurrentEnums.NotifyObservers;
end;

procedure TAppModel.WriteProject(AVerbose: Boolean;
  AOnWriteClass: TOnWriteClassIntf; AOnWriteEnum: TOnWriteEnum;
  AOnWriteUnit: TOnWriteUnit);
var
  lWriter: TMapperProjectWriter;
begin
  if State = mpsClosed then
    raise Exception.Create(ClassName + '.WriteProject: No project loaded');

  SaveProject;

  lWriter := TMapperProjectWriter.Create(Project);
  lWriter.Verbose:=AVerbose;
  if AVerbose then
  begin
    lWriter.OnWriteClass := AOnWriteClass;
    lWriter.OnWriteEnum := AOnWriteEnum;
    lWriter.OnWriteUnit := AOnWriteUnit;
  end;

  try
    lWriter.WriteProject(Project.GeneralOptions.OutputDirectory);
  finally
    lWriter.Free;
  end;
end;

initialization

finalization
  if mModel <> nil then
    mModel.Free;

end.

