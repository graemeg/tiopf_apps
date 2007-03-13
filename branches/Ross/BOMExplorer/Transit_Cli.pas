unit Transit_Cli;

interface

uses
  SysUtils
  , tiObject
  , tiVirtualTrees
  , tiVTTreeView
  , Transit_BOM
  , tiBOMExploration;

type
  //this class is needed to hang some event handlers off of
  //also acts as a central factory for all BOM->GUI stuff
  TBOMETransitFactory = class(TtiExplorableBOM)
  private
    FTransit: TTransit;
  protected
    procedure Build;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InsertPeriodCollection(ptiVTTreeView: TtiVTTreeView;
      pNode: PVirtualNode; AData: TtiObject);
    procedure InsertShowMethodCollection(ptiVTTreeView: TtiVTTreeView;
      pNode: PVirtualNode; AData: TtiObject);
    procedure InsertMidpointListCollection(ptiVTTreeView: TtiVTTreeView;
      pNode: PVirtualNode; AData: TtiObject);
    procedure InsertMidpointCollection(ptiVTTreeView: TtiVTTreeView;
      pNode: PVirtualNode; AData: TtiObject);
    procedure InsertRTSQuestionCollection(ptiVTTreeView: TtiVTTreeView;
      pNode: PVirtualNode; AData: TtiObject);
    procedure MarkDeleted(ptiVTTreeView: TtiVTTreeView; pNode: PVirtualNode;
      AData: TtiObject);
    procedure InsertMidpointList(ptiVTTreeView: TtiVTTreeView;
      pNode: PVirtualNode; AData: TtiObject);
  end;

  TTransitAutoMapRegistrar = class(TtiBOMEAutomapRegistrar)
  protected
    procedure DoRegisterItems; override;
  public
  end;

  TTransitDBIndependantRegistrar = class(TtiBOMEDBIndependantRegistrar)
  protected
    procedure DoRegisterItems; override;
  public
  end;

  TTransitHardCodeRegistrar = class(TtiBOMEHardCodeRegistrar)
  protected
    procedure DoRegisterItems; override;
  public
  end;


implementation

uses
  tiConstants
  , Transit_AutoMap_Svr
  , Transit_DBIndependentVisitors_Svr
  , Transit_HardCodeVisitors_Svr
  ;

procedure RegisterBOM;
var
  BOM: TBOMETransitFactory;
begin
  BOM := TBOMETransitFactory.Create;
  BOM.Name := 'Transit';
  BOMEDictionary.BOMs.Add(BOM);
end;

{ TTransitBOM }

procedure TBOMETransitFactory.Build;
var
  Registrar: TtiBOMERegistrar;
  View: TtiBOMEView;
begin
  //configure alias(es)
  AddAlias('Local Prototype MDB',
    cTIPersistADOAccess,
    ExtractFilePath(ParamStr(0)) + 'Transit.MDB');
  AddAlias('Local XML File',
    cTIPersistXMLLight,
    ExtractFilePath(ParamStr(0)) + 'transit.xml');
  AddAlias('Local CSV File',
    cTIPersistCSV,
    ExtractFilePath(ParamStr(0)) + 'transit.csv');
  AddAlias('Local TAB File',
    cTIPersistTAB,
    ExtractFilePath(ParamStr(0)) + 'transit.tab');


  //configure registrar(s)
  Registrar := TTransitAutomapRegistrar.Create;
  Registrar.Name := 'Transit AutoMap';
  Registrars.Add(Registrar);

  Registrar := TTransitDBIndependantRegistrar.Create;
  Registrar.Name := 'Transit DB Independent Visitors';
  Registrars.Add(Registrar);

  Registrar := TTransitHardCodeRegistrar.Create;
  Registrar.Name := 'Transit DB Hard Coded Visitors';
  Registrars.Add(Registrar);

  //add viewpoints
  ViewPoints.Add('SuperList', FTransit.SuperList);
  ViewPoints.Add('Show Methods', FTransit.ShowMethods);
  ViewPoints.Add('Periods', FTransit.Periods);
  ViewPoints.Add('Midpoint Lists', FTransit.MidpointLists);
  ViewPoints.Add('RTS Questions', FTransit.RTSQuestions);
  ViewPoints.Add('Flat Root', FTransit);

  //configure first view  (allows inserting and deleting)
  View := Views.Add;
  View.Name := 'Edit All';

  View.AddTreeMapping(TSuperList, True);
  View.AddTreeMapping(TTransit, True);
  View.AddTreeMapping(TPeriodCollection, True, InsertPeriodCollection);
  View.AddTreeMapping(TShowMethodCollection, True, InsertShowMethodCollection);
  View.AddTreeMapping(TMidpointListCollection, True, InsertMidpointListCollection);
  View.AddTreeMapping(TMidpointCollection, True, InsertMidpointCollection);
  View.AddTreeMapping(TRTSQuestionCollection, True, InsertRTSQuestionCollection);
  View.AddTreeMapping(TPeriod, True, nil, MarkDeleted);
  View.AddTreeMapping(TShowMethod, True, nil, MarkDeleted);
  View.AddTreeMapping(TMidpointList, True, InsertMidpointList, MarkDeleted);
  View.AddTreeMapping(TMidpoint, True, nil, MarkDeleted);
  View.AddTreeMapping(TRTSQuestion, True, nil, MarkDeleted);

  //configure second view (read-only)
  View := Views.Add;
  View.Name := 'Read Only';
  View.AddTreeMapping(TSuperList);
  View.AddTreeMapping(TTransit);
  View.AddTreeMapping(TPeriodCollection);
  View.AddTreeMapping(TShowMethodCollection);
  View.AddTreeMapping(TMidpointListCollection);
  View.AddTreeMapping(TMidpointCollection);
  View.AddTreeMapping(TRTSQuestionCollection);
  View.AddTreeMapping(TShowMethod);
  View.AddTreeMapping(TShowMethod);
  View.AddTreeMapping(TMidpointList);
  View.AddTreeMapping(TMidpoint);
  View.AddTreeMapping(TRTSQuestion);
end;

constructor TBOMETransitFactory.Create;
begin
  inherited;
  FTransit := TTransit.Create;
  Build;
end;

destructor TBOMETransitFactory.Destroy;
begin
  FTransit.Free;
  inherited;
end;

procedure TBOMETransitFactory.InsertMidpointCollection(
  ptiVTTreeView: TtiVTTreeView; pNode: PVirtualNode; AData: TtiObject);
begin
  TMidpointCollection(AData).Add;
end;

procedure TBOMETransitFactory.InsertMidpointList(ptiVTTreeView: TtiVTTreeView;
  pNode: PVirtualNode; AData: TtiObject);
begin
  TMidpointList(AData).Midpoints.Add;
end;

procedure TBOMETransitFactory.InsertMidpointListCollection(
  ptiVTTreeView: TtiVTTreeView; pNode: PVirtualNode; AData: TtiObject);
begin
  TMidpointListCollection(AData).Add;
end;

procedure TBOMETransitFactory.InsertPeriodCollection(ptiVTTreeView: TtiVTTreeView;
  pNode: PVirtualNode; AData: TtiObject);
begin
  //GOTCHA: if we had TtiObjectList.Add:TContained_Type implemented, the
  //following line would work generically, as it does for deletion:
  //TtiObjectList(AData).Add;
  //instead we must have separate handlers, one for each type
  TPeriodCollection(AData).Add;
end;

procedure TBOMETransitFactory.InsertRTSQuestionCollection(
  ptiVTTreeView: TtiVTTreeView; pNode: PVirtualNode; AData: TtiObject);
begin
  TRTSQuestionCollection(AData).Add;
end;

procedure TBOMETransitFactory.InsertShowMethodCollection(ptiVTTreeView: TtiVTTreeView;
  pNode: PVirtualNode; AData: TtiObject);
begin
  TShowMethodCollection(AData).Add;
end;

procedure TBOMETransitFactory.MarkDeleted(ptiVTTreeView: TtiVTTreeView;
  pNode: PVirtualNode; AData: TtiObject);
begin
  AData.Deleted := True;
end;

{ TTransitAutoMapRegistrar }

procedure TTransitAutoMapRegistrar.DoRegisterItems;
begin
  inherited;
  Transit_AutoMap_Svr.RegisterMappings;
end;

{ TTransitDBIndependantRegistrar }

procedure TTransitDBIndependantRegistrar.DoRegisterItems;
begin
  inherited;
  Transit_DBIndependentVisitors_Svr.RegisterVisitors;
end;

{ TTransitHardCodeRegistrar }

procedure TTransitHardCodeRegistrar.DoRegisterItems;
begin
  inherited;
  Transit_HardCodeVisitors_Svr.RegisterVisitors;
end;

initialization
  RegisterBOM;
end.
