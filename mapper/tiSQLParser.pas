unit tiSQLParser;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}


interface
uses
  SysUtils
  ,Classes
  ,TypInfo
  ,tiObject
  ,tiRTTI
  ,tiCriteria
  ,tiAutoMap
  ,tiVisitorDB
  ,tiVisitorCriteria
  ;

type

  // -----------------------------------------------------------------
  //  Class Of
  // -----------------------------------------------------------------
  TtiSQLParserStateClass = class of TtiSQLParserState;

  // -----------------------------------------------------------------
  //  Forward declarations
  // -----------------------------------------------------------------
  TtiSQLParser = class;

  // -----------------------------------------------------------------
  //  Enumerations
  // -----------------------------------------------------------------

  TParseState = (psParsing, psWhiteSpace, psWordStart);
  TTokenType = (ttWhiteSpace, ttSymbol, ttWordChar);

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: Parser State class base. }
  TtiSQLParserState = class(TtiObject)
  private
    FParser: TtiSQLParser;
  public
    property    Parser: TtiSQLParser read FParser;
    procedure   OnParseWord(AValue: string); virtual; abstract;
    constructor Create(AParser: TtiSQLParser); reintroduce;
  end;

  {: List of TtiSQLParserState objects. }
  TtiSQLParserStateList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiSQLParserState; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiSQLParserState); reintroduce;
  public
    property    Items[i:integer] : TtiSQLParserState read GetItems write SetItems;
    procedure   Add(AObject : TtiSQLParserState); reintroduce;
  end;


  {: The Main parser class. }
  TtiSQLParser = class(TtiObject)
  private
    FLoadStates: TtiSQLParserStateList;
    FCurrentWord: string;
    FParseState: TParseState;
    FProps: TStringList;
    FObjClass: TtiObjectClass;
    FSQLResult: string;
    procedure SetCurrentWord(const Value: string);
  protected
    function    DoGetState(const AStateClass: TtiSQLParserStateClass): TtiSQLParserState; virtual;
    procedure   DoOnChar(AChar: WideChar);
    function    DoGetCharType(AChar: WideChar): TTokenType; virtual;
    procedure   Parse(const ASQL: string);
  public
    function    FormatSQL(const ASQL: string; const AObjClass: TtiObjectClass): string;
    constructor Create; override;
    destructor  Destroy; override;
  end;

  function gFormatSQL(const ASQL: string; AObjClass: TtiObjectClass): string;

const
  cSymbols = '_x-+%=()[];,.><'':/*&#!?@';
  cWhiteSpace = ' ' + #13#10 + #9;

implementation

uses
  tiOPFManager
  ;

function gFormatSQL(const ASQL: string; AObjClass: TtiObjectClass): string;
var
  lParser: TtiSQLParser;
begin
  lParser := TtiSQLParser.Create;
  try
    Result := lParser.FormatSQL(ASQL, AObjClass);
  finally
    lParser.Free;
  end;
end;

{ TtiSQLParserStateList }

procedure TtiSQLParserStateList.Add(AObject: TtiSQLParserState);
begin
  inherited Add(AObject);
end;

function TtiSQLParserStateList.GetItems(i: integer): TtiSQLParserState;
begin
  result:= inherited GetItems(i) as TtiSQLParserState;
end;

procedure TtiSQLParserStateList.SetItems(i: integer;
  const AValue: TtiSQLParserState);
begin
  inherited SetItems(i, AValue);
end;

{ TtiSQLParser }

constructor TtiSQLParser.Create;
begin
  inherited;
  FProps := TStringList.Create;
  FProps.Delimiter := ' ';
end;

destructor TtiSQLParser.Destroy;
begin
  FProps.Free;
  inherited;
end;

function TtiSQLParser.DoGetCharType(AChar: WideChar): TTokenType;
begin
  if Pos(AChar, cSymbols) > 0 then
    Result := ttSymbol
  else if Pos(AChar, cWhiteSpace) > 0 then
    Result := ttWhiteSpace
  else
    result := ttWordChar;
end;

function TtiSQLParser.DoGetState(
  const AStateClass: TtiSQLParserStateClass): TtiSQLParserState;
var
  lCounter: Integer;
begin
  result := nil;
  for lCounter := 0 to FLoadStates.Count - 1 do
    begin
      if FLoadStates.Items[lCounter].ClassType = AStateClass then
        begin
          result := TtiSQLParserState(FLoadStates[lCounter]);
          exit;
        end;
    end;

  // Assume that the state has not been loaded if we get this far.
  Result := AStateClass.Create(Self);
  FLoadStates.Add(Result);
end;

procedure TtiSQLParser.DoOnChar(AChar: WideChar);
var
  lType: TTokenType;
  lTempStr: string;
  lColName: string;
  lIdx: Integer;
  lColAttr: TtiAttrColMap;
begin

  lType := DoGetCharType(AChar);

  if FParseState = psWordStart then
    begin
      if lType in [ttWhiteSpace, ttSymbol] then
        begin
          if AChar = '_' then
            begin
              FCurrentWord := FCurrentWord + AChar;
            end
          else
            begin
              FCurrentWord := Trim(FCurrentWord);
              lTempStr := LowerCase(FCurrentWord);
              lIdx := FProps.IndexOf(lTempStr);
              if lIdx > 0 then
                begin
                  lColAttr := GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindByClassAttrMap(FObjClass, lTempStr);
                  FSQLResult := FSQLResult + lColAttr.DBColMap.ColName;
                end
              else
                begin
                  FSQLResult := FSQLResult + FCurrentWord;
                end;
              // Add if symbol
              FSQLResult := FSQLResult + AChar;
              FCurrentWord := '';
              FParseState := psWhiteSpace;
            end;
        end
      else
        begin
          FCurrentWord := FCurrentWord + AChar;
        end;
    end
  else
    begin
      if lType = ttWordChar then
        begin
          FParseState := psWordStart;
          FCurrentWord := '';
          FCurrentWord := AChar;
        end
      else
        begin
          FSQLResult := FSQLResult + AChar;
        end;
    end;


end;

function TtiSQLParser.FormatSQL(const ASQL: string; const AObjClass: TtiObjectClass): string;
begin
  FObjClass := AObjClass;
  FSQLResult := '';
  FProps.Clear;
  tiGetPropertyNames(FObjClass, FProps, ctkAll);
  FProps.Text := LowerCase(FProps.Text);

  Parse(ASQL);

  Result := FSQLResult;
end;

procedure TtiSQLParser.Parse(const ASQL: string);
var
  lCounter: Integer;
  lLen: Integer;
begin
  lLen := Length(ASQL);

  for lCounter := 1 to lLen do
    begin
      DoOnChar(ASQL[lCounter]);
    end;

end;

procedure TtiSQLParser.SetCurrentWord(const Value: string);
begin
  FCurrentWord := Value;
end;

{ TtiSQLParserState }

constructor TtiSQLParserState.Create(AParser: TtiSQLParser);
begin
  inherited Create;
  FParser := AParser;
end;

end.
