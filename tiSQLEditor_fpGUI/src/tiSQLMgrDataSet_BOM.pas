unit tiSQLMgrDataSet_BOM;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  tiObject,
  tiQuery,
  tiSQLMgr_BOM,
  tiDataBuffer_BOM;

type

  TtiSQLMgrDataSet = class(TtiDataBuffer)
  private
    FtiQuery: TSQLMgrQuery;
  public
    property TIQuery: TSQLMgrQuery read FtiQuery write FtiQuery;
  end;


  TtiDataBufferQueryMapping = class;


  TtiDataBufferQueryMappings = class(TtiObjectList)
  protected
    function GetItems(pIndex: integer): TtiDataBufferQueryMapping; reintroduce;
    procedure SetItems(pIndex: integer; const Value: TtiDataBufferQueryMapping); reintroduce;
  public
    property Items[pIndex: integer]: TtiDataBufferQueryMapping read GetItems write SetItems;
    function FindBySQLMgrQuery(pSQLMgrQuery: TSQLMgrQuery): TtiDataBufferQueryMapping;
  end;


  TtiDataBufferQueryMapping = class(TtiObject)
  private
    FTIDataSet: TtiDataBuffer;
    FSQLMgrQuery: TSQLMgrQuery;
    FsErrorMessage: string;
    FiTimeToRun: DWord;
    FiTimeToScan: DWord;
    procedure SetSQLMgrQuery(const AValue: TSQLMgrQuery);
  public
    constructor Create; override;
    destructor Destroy; override;
    property SQLMgrQuery: TSQLMgrQuery read FSQLMgrQuery write SetSQLMgrQuery;
    property TIDataSet: TtiDataBuffer read FTIDataSet write FTIDataSet;
    property ErrorMessage: string read FsErrorMessage write FsErrorMessage;
    property TimeToRun: DWord read FiTimeToRun write FiTimeToRun;
    property TimeToScan: DWord read FiTimeToScan write FiTimeToScan;
  end;


implementation

uses
  SysUtils;

 //* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 //*
 //* TtiDataBufferQueryMappings
 //*
 //* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiDataBufferQueryMappings.FindBySQLMgrQuery(pSQLMgrQuery: TSQLMgrQuery): TtiDataBufferQueryMapping;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].SQLMgrQuery = pSQLMgrQuery then
    begin
      Result := Items[i];
      Break; //==>
    end;
  if Result = nil then
  begin
    Result := TtiDataBufferQueryMapping.Create;
    //    result.TIDataSet := TtiDataBuffer.Create ;
    Result.SQLMgrQuery := pSQLMgrQuery;
    Add(Result);
  end;
end;

function TtiDataBufferQueryMappings.GetItems(pIndex: integer): TtiDataBufferQueryMapping;
begin
  Result := TtiDataBufferQueryMapping(inherited GetItems(pIndex));
end;

procedure TtiDataBufferQueryMappings.SetItems(pIndex: integer; const Value: TtiDataBufferQueryMapping);
begin
  inherited SetItems(pIndex, Value);
end;

{ TtiDataBufferQueryMapping }

procedure TtiDataBufferQueryMapping.SetSQLMgrQuery(const AValue: TSQLMgrQuery);
begin
  if Assigned(FSQLMgrQuery) then
    FSQLMgrQuery.Free;
  FSQLMgrQuery := AValue;
end;

constructor TtiDataBufferQueryMapping.Create;
begin
  inherited Create;
  FtiDataSet := TtiDataBuffer.Create;
end;

destructor TtiDataBufferQueryMapping.Destroy;
begin
  if Assigned(FSQLMgrQuery) then
    FSQLMgrQuery.Free;
  FtiDataSet.Free;
  inherited Destroy;
end;

end.

