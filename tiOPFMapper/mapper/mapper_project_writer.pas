unit mapper_project_writer;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, dateutils, mapper, tiUtils;

type
  TMapperProjectWriter = class(TMapSchemaWriter)
  private
    FBaseDir: string;
    FVerbose: boolean;
    procedure SetBaseDir(const AValue: string);
    procedure SetVerbose(AValue: Boolean);
    function DoubleQuoteFieldName(FieldName: string; DoubleQuote: boolean = false): string;
  protected
    function CreateSQLFieldList(AClassDef: TMapClassDef): string;
    procedure PrepareUnitList(AList: TStringList);
    procedure WriteUnitEnums(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure WriteSingleUnitEnum(ASL: TStringList; AEnumDef: TMapEnum);
    procedure WriteUnitClasses(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure WriteSingleUnitClass(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassProps(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteSingleClassProp(ASL: TStringList; AClassProp: TMapClassProp);
    procedure WriteClassPropAccessMethods(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WritePropPrivateVars(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WritePropGetter(ASL: TStringList; APropDef: TMapClassProp);
    procedure WritePropSetter(ASL: TStringList; APropDef: TMapClassProp);
    procedure WritePropStreamGetter(ASL: TStringList; APropMap: TPropMapping);
    procedure WritePropStreamSetter(ASL: TStringList; APropMap: TPropMapping);
    procedure WriteClassMappings(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassSelectsInf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassSelectsImps(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassMethods(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure WriteSingleClassMethods(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteORMClass(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassIntfMethods(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassImpSettersGetters(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassIntfDestructor(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassIntfReadMethod(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteIntfUses(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure WriteImpUses(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure WriteListClassIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteListClassImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassIntfSaveMethod(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassImpDestructor(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassImpReadMethod(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassImpSaveMethod(ASL: TStringList; AClassDef: TmapClassDef);
    // Visitor methods writing
    procedure WriteVisRegIntf(ASL: TStringList);
    procedure WriteVisRegImp(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure WriteClassVisitorRegistrations(ASL: TStringList; AClassMap: TMapClassDef);
    procedure WriteClassVisitorIntfs(ASL: TStringList; AClassMap: TMapClassDef);
    procedure WriteVisClassCreateIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisClassCreateImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisClassDeleteIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisClassDeleteImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisClassUpdateIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisClassUpdateImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisClassReadIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisClassReadImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisListReadIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteVisListReadImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteCustomListVisIntf(ASL: TStringList; AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
    procedure WriteCustomListVisImp(ASL: TStringList; AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
    procedure WriteAllCustomListVisIntfs(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteAllCustomListVisImps(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteExtraVarsMaybe(ASL: TStringList; AClassDef: TMapClassDef);
    // AutoMap Registration
    procedure WriteAutoMapIntf(ASL: TStringList);
    procedure WriteAllRegisterAutoMaps(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure WriteClassRegisterAutoMapImp(ASL: TStringList; AClassDef: TMapClassDef);
    // SQL construction
    procedure WriteInsertSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteUpdateSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteBaseSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteListSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteDeleteSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteSetupParams(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteMapRowToObject(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteMapRowToObjectForList(ASL: TStringList; AClassDef: TMapClassDef);
    // Custom Select Methods
    procedure WriteClassListSelectMethodIntf(ASL: TStringList; AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
    procedure WriteClassListSelectMethodImp(ASL: TStringList; AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
    // Validators
    procedure WriteClassIsValidIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure WriteClassIsValidImp(ASL: TStringList; AClassDef: TMapClassDef);
    function DoGetValidatorTestString(const APropName: string; const APropType: TMapPropType; const AValType: TValidatorType; AValue: Variant): string;
  public
    property BaseDir: string read FBaseDir write SetBaseDir;
    property Verbose: Boolean read FVerbose write SetVerbose;
    procedure WriteUnit(AUnit: TMapUnitDef; ASL: TStringList); virtual;
    {: Expects to have NO trailing path delim to ADirectory parameter string.}
    procedure WriteProject(const ADirectory: string); overload; override;
    procedure WriteProject(const ADirectory: string; ASL: TStringList); overload; override;
    constructor Create(AProject: TMapProject); override;
    destructor Destroy; override;
  end;

  TDateTimeParts = record
    Year: uint16;
    Month: uint16;
    Day: uint16;
    Hour: uint16;
    Minute: uint16;
    Second: uint16;
    MSecond: uint16;
  end;

implementation

{ TMapperProjectWriter }

constructor TMapperProjectWriter.Create(AProject: TMapProject);
begin
  inherited Create(AProject);
  FVerbose := false;
end;

function TMapperProjectWriter.CreateSQLFieldList(AClassDef: TMapClassDef): string;
var
  lCtr: integer;
  lPropMap: TPropMapping;
begin
  result := AClassDef.ClassMapping.TableName + '.OID ';

  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    lPropMap := AClassDef.ClassMapping.PropMappings.Items[lCtr];
    result := result + ', ' + AClassDef.ClassMapping.TableName + '.' + lPropMap.FieldName;
  end;

  result := UpperCase(result);
end;

destructor TMapperProjectWriter.Destroy;
begin
  inherited Destroy;
end;

function TMapperProjectWriter.DoGetValidatorTestString(const APropName: string; const APropType: TMapPropType; const AValType: TValidatorType; AValue: Variant): string;
var
  lStubIf: string;
  lDateTime: TDateTime;
  lParts: TDateTimeParts;
begin
  lStubIf := 'if ' + APropName;

  case AValType of
    vtGreater:
      begin
        case APropType of
          ptInteger, ptInt64:
            result := lStubIf + ' <= ' + IntToStr(AValue) + ' then';
          ptDateTime:
            begin
              lDateTime := AValue;
              DecodeDateTime(lDateTime, lParts.Year, lParts.Month, lParts.Day, lParts.Hour, lParts.Minute, lParts.Second, lParts.MSecond);
              lStubIf := lStubIf + ' <= EncodeDateTime(' + IntToStr(lParts.Year) + ', ' + IntToStr(lParts.Month) + ', ' + IntToStr(lParts.Day) + ', ' + IntToStr(lParts.Hour) + ', ' + IntToStr(lParts.Minute) + ', ' + IntToStr(lParts.Second) + ', ' + IntToStr(lParts.MSecond) + ' then';
            end;
          ptDouble, ptCurrency, ptSingle:
            result := lStubIf + ' <= ' + FormatFloat('#0.00', AValue) + ' then';
        else
          raise Exception.Create(ClassName + '.DoGetValidatorTestString: Operation not supported for property "' + APropName + '" of type "' + gPropTypeToStr(APropType) + '"');
        end;
      end;
    vtGreaterEqual:
      begin
        case APropType of
          ptInteger, ptInt64:
            result := lStubIf + ' < ' + IntToStr(AValue) + ' then';
          ptDateTime:
            begin
              lDateTime := AValue;
              DecodeDateTime(AValue, lParts.Year, lParts.Month, lParts.Day, lParts.Hour, lParts.Minute, lParts.Second, lParts.MSecond);
              lStubIf := lStubIf + ' < EncodeDateTime(' + IntToStr(lParts.Year) + ', ' + IntToStr(lParts.Month) + ', ' + IntToStr(lParts.Day) + ', ' + IntToStr(lParts.Hour) + ', ' + IntToStr(lParts.Minute) + ', ' + IntToStr(lParts.Second) + ', ' + IntToStr(lParts.MSecond) + ' then';
            end;
          ptDouble, ptCurrency, ptSingle:
            result := lStubIf + ' < ' + FormatFloat('#0.00', AValue) + ' then';
        else
          raise Exception.Create(ClassName + '.DoGetValidatorTestString: Operation not supported for property "' + APropName + '" of type "' + gPropTypeToStr(APropType) + '"');
        end;
      end;
    vtLess:
      begin
        case APropType of
          ptInteger, ptInt64:
            result := lStubIf + ' >= ' + IntToStr(AValue) + ' then';
          ptDateTime:
            begin
              lDateTime := AValue;
              DecodeDateTime(AValue, lParts.Year, lParts.Month, lParts.Day, lParts.Hour, lParts.Minute, lParts.Second, lParts.MSecond);
              lStubIf := lStubIf + ' >= EncodeDateTime(' + IntToStr(lParts.Year) + ', ' + IntToStr(lParts.Month) + ', ' + IntToStr(lParts.Day) + ', ' + IntToStr(lParts.Hour) + ', ' + IntToStr(lParts.Minute) + ', ' + IntToStr(lParts.Second) + ', ' + IntToStr(lParts.MSecond) + ' then';
            end;
          ptDouble, ptCurrency, ptSingle:
            result := lStubIf + ' >= ' + FormatFloat('#0.00', AValue) + ' then';
        else
          raise Exception.Create(ClassName + '.DoGetValidatorTestString: Operation not supported for property "' + APropName + '" of type "' + gPropTypeToStr(APropType) + '"');
        end;
      end;
    vtLessEqual:
      begin
        case APropType of
          ptInteger, ptInt64:
            result := lStubIf + ' > ' + IntToStr(AValue) + ' then';
          ptDateTime:
            begin
              lDateTime := AValue;
              DecodeDateTime(AValue, lParts.Year, lParts.Month, lParts.Day, lParts.Hour, lParts.Minute, lParts.Second, lParts.MSecond);
              lStubIf := lStubIf + ' > EncodeDateTime(' + IntToStr(lParts.Year) + ', ' + IntToStr(lParts.Month) + ', ' + IntToStr(lParts.Day) + ', ' + IntToStr(lParts.Hour) + ', ' + IntToStr(lParts.Minute) + ', ' + IntToStr(lParts.Second) + ', ' + IntToStr(lParts.MSecond) + ' then';
            end;
          ptDouble, ptCurrency, ptSingle:
            result := lStubIf + ' > ' + FormatFloat('#0.00', AValue) + ' then';
        else
          raise Exception.Create(ClassName + '.DoGetValidatorTestString: Operation not supported for property "' + APropName + '" of type "' + gPropTypeToStr(APropType) + '"');
        end;
      end;
    vtNotEqual:
      begin
        case APropType of
          ptInteger, ptInt64:
            result := lStubIf + ' = ' + IntToStr(AValue) + ' then';
          ptDateTime:
            begin
              lDateTime := AValue;
              DecodeDateTime(AValue, lParts.Year, lParts.Month, lParts.Day, lParts.Hour, lParts.Minute, lParts.Second, lParts.MSecond);
              lStubIf := lStubIf + ' = EncodeDateTime(' + IntToStr(lParts.Year) + ', ' + IntToStr(lParts.Month) + ', ' + IntToStr(lParts.Day) + ', ' + IntToStr(lParts.Hour) + ', ' + IntToStr(lParts.Minute) + ', ' + IntToStr(lParts.Second) + ', ' + IntToStr(lParts.MSecond) + ' then';
            end;
          ptDouble, ptCurrency, ptSingle:
            result := lStubIf + ' = ' + FormatFloat('#0.00', AValue) + ' then';
          ptString:
            result := lStubIf + ' = ' + QuotedStr(AValue) + ' then';
          ptBoolean:
            result := lStubIf + ' = ' + BoolToStr(AValue, True) + ' then';
        else
          raise Exception.Create(ClassName + '.DoGetValidatorTestString: Operation not supported for property "' + APropName + '" of type "' + gPropTypeToStr(APropType) + '"');
        end;
      end;
    vtRequired:
      begin
        case APropType of
//          ptDateTime:
//            begin
//              raise Exception.Create(ClassName + '.DoGetValidatorTestString: TDateTime not support for vtRequired')
//              //lDateTime := AValue;
//              //DecodeDateTime(AValue, lParts.Year, lParts.Month, lParts.Day, lParts.Hour, lParts.Minute,
//              //  lParts.Second, lParts.MSecond);
//              //lStubIf := lStubIf + ' EncodeDate(' + IntToStr(lParts.Year) + ', ' +
//              //  IntToStr(lParts.Month) + ', ' + IntToStr(lParts.Day) + ', ' + IntToStr(lParts.Hour) + ', ' +
//              //  IntToStr(lParts.Minute) + ', ' + IntToStr(lParts.Second) + ', ' + IntToStr(lParts.MSecond) + ' then';
//            end;
          ptString:
            result := lStubIf + ' = '''' then ';
        else
          raise Exception.Create(ClassName + '.DoGetValidatorTestString: Operation not supported for property "' + APropName + '" of type "' + gPropTypeToStr(APropType) + '"');
        end;
      end;

  end;
end;

function TMapperProjectWriter.DoubleQuoteFieldName(FieldName: string;
  DoubleQuote: boolean): string;
begin
  if DoubleQuote then
    result := '"' + FieldName + '"'
  else
    result := FieldName;
end;

procedure TMapperProjectWriter.PrepareUnitList(AList: TStringList);
begin
  AList.Clear;
end;

procedure TMapperProjectWriter.SetBaseDir(const AValue: string);
begin
  if FBaseDir = AValue then
    exit;
  FBaseDir := AValue;
end;

procedure TMapperProjectWriter.SetVerbose(AValue: Boolean);
begin
  if FVerbose = AValue then
    exit;
  FVerbose := AValue;
end;

procedure TMapperProjectWriter.WriteAllCustomListVisImps(ASL: TStringList; AClassDef: TMapClassDef);
var
  lSelect: TClassMappingSelect;
  lCtr: integer;
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  for lCtr := 0 to AClassDef.Selections.Count - 1 do
  begin
    lSelect := AClassDef.Selections.Items[lCtr];
    WriteCustomListVisImp(ASL, AClassDef, lSelect);
  end;
end;

procedure TMapperProjectWriter.WriteExtraVarsMaybe(ASL: TStringList; AClassDef: TMapClassDef);
var
  i: Integer;
  HasStream: Boolean;
begin
  HasStream := False;
  for i := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    if not HasStream and (AClassDef.ClassMapping.PropMappings.Items[i].PropertyType.BaseType = ptStream) then
    begin
      WriteLine('lStream: TStream;', ASL);
      WriteLine('lStreamFree: Boolean = True;', ASL);
      HasStream := True;
    end;
    // others...
  end;
end;

procedure TMapperProjectWriter.WriteAllRegisterAutoMaps(ASL: TStringList; AUnitDef: TMapUnitDef);
var
  lCtr: Integer;
  lClassDef: TMapClassDef;
begin

  WriteLine('procedure RegisterMappings;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  for lCtr := 0 to AUnitDef.UnitClasses.Count - 1 do
  begin
    lClassDef := AUnitDef.UnitClasses.Items[lCtr];
    WriteClassRegisterAutoMapImp(ASL, lClassDef);
    if lCtr < AUnitDef.UnitClasses.Count - 1 then // we don't want a blank line at the end
      WriteBreak(ASL);
  end;
  DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteAllCustomListVisIntfs(ASL: TStringList; AClassDef: TMapClassDef);
var
  lSelect: TClassMappingSelect;
  lCtr: integer;
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  for lCtr := 0 to AClassDef.Selections.Count - 1 do
  begin
    lSelect := AClassDef.Selections.Items[lCtr];
    WriteCustomListVisIntf(ASL, AClassDef, lSelect);
  end;
end;

procedure TMapperProjectWriter.WriteAutoMapIntf(ASL: TStringList);
begin
  WriteLine('{ Register Auto Mappings }', ASL);
  WriteLine('procedure RegisterMappings;', ASL);
end;

procedure TMapperProjectWriter.WriteBaseSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin

  lMapping := AClassDef.ClassMapping;

  WriteLine('''SELECT '' +', ASL);

  WriteLine(''' ' + AClassDef.ClassMapping.PKField + ', '' +', ASL);

  //' + lMapping.TableName + ' SET '' +', ASL);
  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
  begin
    lPropMap := lMapping.PropMappings.Items[lCtr];
    if lCtr < (lMapping.PropMappings.Count - 1) then
      WriteLine(''' ' + lPropMap.FieldName + ', '' +', ASL)
    else
      WriteLine(''' ' + lPropMap.FieldName + ' '' +', ASL)
  end;

  WriteLine('''FROM  ' + lMapping.TableName + ' ''', ASL);

end;

procedure TMapperProjectWriter.WriteClassSelectsImps(ASL: TStringList; AClassDef: TMapClassDef);
var
  lSelect: TClassMappingSelect;
  lCtr: integer;
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  for lCtr := 0 to AClassDef.Selections.Count - 1 do
  begin
    lSelect := AClassDef.Selections.Items[lCtr];
    WriteClassListSelectMethodImp(ASL, AClassDef, lSelect);
  end;
end;

procedure TMapperProjectWriter.WriteClassRegisterAutoMapImp(ASL: TStringList; AClassDef: TMapClassDef);
var
  lMapping: TClassMapping;
  lPropMap: TPropMapping;
  lCtr: Integer;
begin
  lMapping := AClassDef.ClassMapping;
  if lMapping.PropMappings.Count = 0 then
    Exit;

  WriteLine('{ Automap registrations for ' + AClassDef.BaseClassName + ' }', ASL);

  // Write out OID mapping first
  WriteLine('GTIOPFManager.ClassDBMappingMgr.RegisterMapping(' + AClassDef.BaseClassName + ', ', ASL);
  IncTab;
  WriteLine(QuotedStr(lMapping.TableName) + ', ' + QuotedStr(lMapping.PKName) + ', ' + QuotedStr(lMapping.PKField) + ', [pktDB]);', ASL);
  DecTab;

  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
  begin
    lPropMap := lMapping.PropMappings.Items[lCtr];
    WriteLine('GTIOPFManager.ClassDBMappingMgr.RegisterMapping(' + AClassDef.BaseClassName + ',', ASL);
    IncTab;
    WriteLine(QuotedStr(lMapping.TableName) + ',' + QuotedStr(lPropMap.PropName) + ', ' + QuotedStr(lPropMap.FieldName) + ');', ASL);
    DecTab;
  end;

  // Finally, register list class for class.

  if AClassDef.AutoCreateListClass then
    WriteLine('GTIOPFManager.ClassDBMappingMgr.RegisterCollection(' + AClassDef.BaseClassName + 'List, ' + AClassDef.BaseClassName + ');', ASL);
end;

procedure TMapperProjectWriter.WriteClassImpDestructor(ASL: TStringList; AClassDef: TMapClassDef);
var
  lCtr: integer;
  lMapping: TPropMapping;
  lNeedDestructor: Boolean;
begin
  lNeedDestructor := False;
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    lMapping := AClassDef.ClassMapping.PropMappings.Items[lCtr];
    if lMapping.PropertyType.BaseType = ptStream then
    begin
      lNeedDestructor := True;
      Break;
    end;
  end;

  if not lNeedDestructor then
    Exit;

  WriteLine('destructor ' + AClassDef.BaseClassName + '.Destroy;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    lMapping := AClassDef.ClassMapping.PropMappings.Items[lCtr];
    if lMapping.PropertyType.BaseType = ptStream then
    begin
      WriteLine('FreeAndNil(F' + lMapping.PropName + ');', ASL);
      Write
    end;
  end;
  WriteLine('inherited Destroy;', ASL);
  DecTab;
  WriteLine('end;', ASL);
end;

procedure TMapperProjectWriter.WriteClassImpReadMethod(ASL: TStringList; AClassDef: TMapClassDef);
var
  lBaseClassName: string;
  lBaseListClassName: string;
begin
  lBaseClassName := Copy(AClassDef.BaseClassName, 2, Length(AClassDef.BaseClassName));
  lBaseListClassName := AClassDef.BaseClassName + 'List';
  WriteLine('procedure ' + AClassDef.BaseClassName + '.Read;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  if AClassDef.AutoCreateListClass and AClassDef.ListSavesDatabaseName then
  begin
    WriteLine('if Assigned(Owner) and Owner.InheritsFrom(' + lBaseListClassName + ') then', ASL);
    IncTab;
    WriteLine('Read(' + lBaseListClassName + '(Owner).FDBConnectionName, ' + lBaseListClassName + '(Owner).FPersistenceLayerName)', ASL);
    DecTab;
    WriteLine('else', ASL);
    IncTab;
    WriteLine('Read('''', '''');', ASL);
    DecTab;
  end
  else
    WriteLine('Read('''', '''');', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '.Read(const ADBConnectionName: string; APersistenceLayerName: string);', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('GTIOPFManager.VisitorManager.Execute(''Load' + lBaseClassName + ''', self, ADBConnectionName, APersistenceLayerName);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteClassImpSaveMethod(ASL: TStringList; AClassDef: TmapClassDef);
var
  lBaseClassName: string;
  lBaseListClassName: string;
begin
  lBaseClassName := Copy(AClassDef.BaseClassName, 2, Length(AClassDef.BaseClassName));
  lBaseListClassName := AClassDef.BaseClassName + 'List';
  WriteLine('procedure ' + AClassDef.BaseClassName + '.Save;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  if AClassDef.AutoCreateListClass and AClassDef.ListSavesDatabaseName then
  begin
    WriteLine('if Assigned(Owner) and Owner.InheritsFrom(' + lBaseListClassName + ') then', ASL);
    IncTab;
    WriteLine('Save(' + lBaseListClassName + '(Owner).FDBConnectionName, ' + lBaseListClassName + '(Owner).FPersistenceLayerName)', ASL);
    DecTab;
    WriteLine('else', ASL);
    IncTab;
    WriteLine('Save('''', '''');', ASL);
    DecTab;
  end
  else
    WriteLine('Save('''', '''');', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '.Save(const ADBConnectionName: string; APersistenceLayerName: string);', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('GTIOPFManager.VisitorManager.Execute(''Save' + lBaseClassName + ''', self, ADBConnectionName, APersistenceLayerName);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteORMClass(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ Generated ORM Class: ' + AClassDef.BaseClassName + 'ORM}', ASL);
  WriteLine(AClassDef.BaseClassName + ' = class(' + AClassDef.BaseClassParent + ')', ASL);

  // protected section - includes getters and setters;
  WriteLine('protected', ASL);

  IncTab;
  WritePropPrivateVars(ASL, AClassDef);
  WriteClassPropAccessMethods(ASL, AClassDef);
  DecTab;

  WriteLine('published', ASL);
  IncTab;
  WriteClassProps(ASL, AClassDef);
  DecTab;

  WriteLine('end;', ASL);
end;

procedure TMapperProjectWriter.WriteClassMappings(ASL: TStringList; AClassDef: TMapClassDef);
begin

end;

procedure TMapperProjectWriter.WriteClassMethods(ASL: TStringList; AUnitDef: TMapUnitDef);
begin

end;

procedure TMapperProjectWriter.WriteClassIntfMethods(ASL: TStringList; AClassDef: TMapClassDef);
begin

end;

procedure TMapperProjectWriter.WriteClassIntfReadMethod(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('procedure   Read; override;', ASL);
  WriteLine('procedure   Read(const ADBConnectionName: string; APersistenceLayerName: string = ''''); override;', ASL);
end;

procedure TMapperProjectWriter.WriteClassIntfSaveMethod(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('procedure   Save; override;', ASL);
  WriteLine('procedure   Save(const ADBConnectionName: string; APersistenceLayerName: string = ''''); override;', ASL);
end;

procedure TMapperProjectWriter.WriteClassListSelectMethodImp(ASL: TStringList; AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
var
  lCtr: integer;
  lParamStr, lParamSig: string;
  lBaseSig: string;
  lParam: TSelectParam;
  lTempStr: string;
  lSL: TStringList;
  lSQL: string;
begin

  lParamSig := '';

  lTempStr := 'function ' + AClassDef.BaseClassName + 'List.' + ASelect.Name;

  for lCtr := 0 to ASelect.Params.Count - 1 do
  begin
    lParam := ASelect.Params.Items[lCtr];
    if lParamSig <> '' then
      lParamSig := lParamSig + '; '
    else
      lParamSig := '(';

//    if lParam.ParamType = ptEnum then
//      lParamSig := lParamSig + lParam.PassBy + ' ' + lParam.ParamName + ': ' + lParam.ParamTypeName
//    else
//      lParamSig := lParamSig + lParam.PassBy + ' ' + lParam.ParamName + ': ' + lParam.ParamTypeName;
      lParamSig := lParamSig + lParam.PassBy + ' ' + lParam.ParamName + ': ' + lParam.ParamType.TypeName;
  end;

  if lParamSig <> '' then
    lTempStr := lTempStr + lParamSig + ')';

  lTempStr := lTempStr + ': integer;';

  WriteLine(lTempStr, ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('if self.Count > 0 then', ASL);
  IncTab;
  WriteLine('self.Clear;', ASL);
  WriteLine('', ASL);
  DecTab;
  WriteLine('Params.Clear;', ASL);
  for lCtr := 0 to ASelect.Params.Count - 1 do
  begin
    lParam := ASelect.Params.Items[lCtr];

    case lParam.ParamType.BaseType of
      ptString, ptAnsiString:
        lParamStr := 'ptString';
      ptBoolean:
        lParamStr := 'ptBoolean';
      ptDateTime:
        lParamStr := 'ptDateTime';
      ptDouble:
        lParamStr := 'ptFloat';
      ptSingle:
        lParamStr := 'ptSingle';
      ptCurrency:
        lParamStr := 'ptCurrency';
      ptInt64, ptInteger:
        lParamStr := 'ptInteger';
      ptEnum:
        lParamStr := 'ptEnum';
      ptEnumSet:
        lParamStr := 'ptEnumSet';
      ptStream:
        lParamStr := 'ptStream';
    end;

          //AddParam('user_oid', ptString, AUser);
    WriteLine('AddParam(' + QuotedStr(lParam.ParamName) + ', ' + QuotedStr(lParam.SQLParamName) + ', ' + lParamStr + ', ' + lParam.ParamName + ');', ASL)
  end;

  lBaseSig := AClassDef.BaseClassName + 'List_' + ASelect.Name + 'Vis';

      // Write out sql to use

  WriteLine('self.SQL := ', ASL);

  IncTab;

  lSL := TStringList.create;
  try
    lSQL := ASelect.SQL;
    if POS('${field_list}', lSQL) > 0 then
      lSL.Text := WrapText(tiNormalizeStr(StringReplace(lSQL, '${field_list}', CreateSQLFieldList(AClassDef), [rfReplaceAll])), 50)
    else
      lSL.Text := WrapText(tiNormalizeStr(ASelect.SQL), 50);
    for lCtr := 0 to lSL.Count - 1 do
    begin
      if lCtr < (lSL.Count - 1) then
        WriteLine(''' ' + lSL[lCtr] + ' '' + ', ASL)
      else
        WriteLine(''' ' + lSL[lCtr] + '''; ', ASL);
    end;
  finally
    lSL.Free;
  end;
  DecTab;

  if AClassDef.ListSavesDatabaseName then
    WriteLine('GTIOPFManager.VisitorManager.Execute(' + QuotedStr(lBaseSig) + ', self, FDBConnectionName, FPersistenceLayerName);', ASL)
  else
    WriteLine('GTIOPFManager.VisitorManager.Execute(' + QuotedStr(lBaseSig) + ', self);', ASL);
  WriteLine('result := self.Count;', ASL);

  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

end;

procedure TMapperProjectWriter.WriteClassPropAccessMethods(ASL: TStringList; AClassDef: TMapClassDef);
var
  lProp: TMapClassProp;
  lCtr: Integer;
  lMapping: TPropMapping;
begin
  for lCtr := 0 to AClassDef.ClassProps.Count - 1 do
  begin
    lProp := AClassDef.ClassProps.Items[lCtr];

    if lProp.VirtualGetter then
      WritePropGetter(ASL, lProp);

    if not lProp.IsReadOnly then
      WritePropSetter(ASL, lProp);

  end;
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    lMapping := AClassDef.ClassMapping.PropMappings.Items[lCtr];
    if (lMapping.PropertyType.BaseType = ptStream) then
    begin
      if (lMapping.PropertyGetter <> '') then
        WritePropStreamGetter(ASL, lMapping);
      if not lProp.IsReadOnly and (lMapping.PropertySetter <> '') then
        WritePropStreamSetter(ASL, lMapping);
    end;
  end;
end;

procedure TMapperProjectWriter.WriteClassProps(ASL: TStringList; AClassDef: TMapClassDef);
var
  lCtr: integer;
  lProp: TMapClassProp;
begin
  for lCtr := 0 to AClassDef.ClassProps.Count - 1 do
  begin
    lProp := AClassDef.ClassProps.Items[lCtr];
    WriteSingleClassProp(ASL, lProp);
  end;
end;

procedure TMapperProjectWriter.WriteClassListSelectMethodIntf(ASL: TStringList; AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
var
  lCtr: integer;
  lParamSig: string;
  lParam: TSelectParam;
  lTempStr: string;
begin
  lParamSig := '';

  WriteLine('{ Returns Number of objects retrieved. }', ASL);
  lTempStr := 'function    ' + ASelect.Name;

  for lCtr := 0 to ASelect.Params.Count - 1 do
  begin
    lParam := ASelect.Params.Items[lCtr];
    if lParamSig <> '' then
      lParamSig := lParamSig + '; '
    else
      lParamSig := '(';

//    if lParam.ParamType in [ptEnum, ptEnumSet] then
//      lParamSig := lParamSig + lParam.PassBy + ' ' + lParam.ParamName + ': ' + lParam.ParamTypeName
//    else
//      lParamSig := lParamSig + lParam.PassBy + ' ' + lParam.ParamName + ': ' + lParam.ParamTypeName;
      lParamSig := lParamSig + lParam.PassBy + ' ' + lParam.ParamName + ': ' + lParam.ParamType.TypeName;
  end;

  if lParamSig <> '' then
    lTempStr := lTempStr + lParamSig + ')';

  lTempStr := lTempStr + ': integer;';

  WriteLine(lTempStr, ASL);

end;

procedure TMapperProjectWriter.WriteClassSelectsInf(ASL: TStringList; AClassDef: TMapClassDef);
var
  lSelect: TClassMappingSelect;
  lCtr: integer;
begin
  for lCtr := 0 to AClassDef.Selections.Count - 1 do
  begin
    lSelect := AClassDef.Selections.Items[lCtr];
    WriteClassListSelectMethodIntf(ASL, AClassDef, lSelect);
  end;
end;

procedure TMapperProjectWriter.WriteClassIsValidImp(ASL: TStringList; AClassDef: TMapClassDef);
var
  lVal: TMapValidator;
  lCtr: Integer;
  lProp: TMapClassProp;
  lIfStub: string;
begin

  if AClassDef.Validators.Count = 0 then
    exit;

  WriteLine('function ' + AClassDef.BaseClassName + '.IsValid(const AErrors: TtiObjectErrors): boolean;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lMsg: string;', ASL);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('Result := inherited IsValid(AErrors);', ASL);
  WriteLine('if not Result then', ASL);
  IncTab;
  WriteLine('Exit;', ASL);
  DecTab;

  WriteBreak(ASL);

  for lCtr := 0 to AClassDef.Validators.Count - 1 do
  begin
    lVal := AClassDef.Validators.Items[lCtr];
    lProp := lVal.ClassProp;

    if lProp = nil then
      raise Exception.Create(ClassName + '.WriteClassIsValidImp: Validator defined for property ' + lVal.ClassProp.Name + ' but no property with that name is registered to class ' + AClassDef.BaseClassName + '.');

    lIfStub := DoGetValidatorTestString(lVal.ClassProp.Name, lProp.PropertyType.BaseType, lVal.ValidatorType, lVal.Value);

    case lVal.ValidatorType of
      vtRequired:
        begin
          WriteLine(lIfStub, ASL);
          WriteLine('begin', ASL);
          IncTab;
          WriteLine('lMsg := ValidatorStringClass.CreateRequiredValidatorMsg(self, ''' + lProp.Name + ''');', ASL);
          WriteLine('AErrors.AddError(lMsg);', ASL);
          DecTab;
          WriteLine('end;', ASL);
        end;
      vtGreater:
        begin
          WriteLine(lIfStub, ASL);
          WriteLine('begin', ASL);
          IncTab;
          WriteLine('lMsg := ValidatorStringClass.CreateGreaterValidatorMsg(self, ''' + lProp.Name + ''', ' + lProp.Name + ');', ASL);
          WriteLine('AErrors.AddError(lMsg);', ASL);
          DecTab;
          WriteLine('end;', ASL);
        end;
      vtGreaterEqual:
        begin
          WriteLine(lIfStub, ASL);
          WriteLine('begin', ASL);
          IncTab;
          WriteLine('lMsg := ValidatorStringClass.CreateGreaterOrEqualValidatorMsg(self, ''' + lProp.Name + ''', ' + lProp.Name + ');', ASL);
          WriteLine('AErrors.AddError(lMsg);', ASL);
          DecTab;
          WriteLine('end;', ASL);
        end;
      vtLess:
        begin
          WriteLine(lIfStub, ASL);
          WriteLine('begin', ASL);
          IncTab;
          WriteLine('lMsg := ValidatorStringClass.CreateLessThanValidatorMsg(self, ''' + lProp.Name + ''', ' + lProp.Name + ');', ASL);
          WriteLine('AErrors.AddError(lMsg);', ASL);
          DecTab;
          WriteLine('end;', ASL);
        end;
      vtLessEqual:
        begin
          WriteLine(lIfStub, ASL);
          WriteLine('begin', ASL);
          IncTab;
          WriteLine('lMsg := ValidatorStringClass.CreateLessThanOrEqualValidatorMsg(self, ''' + lProp.Name + ''', ' + lProp.Name + ');', ASL);
          WriteLine('AErrors.AddError(lMsg);', ASL);
          DecTab;
          WriteLine('end;', ASL);
        end;
      vtNotEqual:
        begin
          WriteLine(lIfStub, ASL);
          WriteLine('begin', ASL);
          IncTab;
          WriteLine('lMsg := ValidatorStringClass.CreateNotEqualToValidatorMsg(self, ''' + lProp.Name + ''', ' + lProp.Name + ');', ASL);
          WriteLine('AErrors.AddError(lMsg);', ASL);
          DecTab;
          WriteLine('end;', ASL);
        end;
    end;
    WriteBreak(ASL);
  end;
  WriteLine('Result := AErrors.Count = 0;', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteClassIsValidIntf(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('function    IsValid(const AErrors: TtiObjectErrors): boolean; overload; override;', ASL);
end;

procedure TMapperProjectWriter.WriteClassVisitorIntfs(ASL: TStringList; AClassMap: TMapClassDef);
begin
  //WriteVisListReadIntf(ASL, AClassMap);
  WriteVisClassReadIntf(ASL, AClassMap);
  WriteVisClassCreateIntf(ASL, AClassMap);
  WriteVisClassUpdateIntf(ASL, AClassMap);
  WriteVisClassDeleteIntf(ASL, AClassMap);
end;

procedure TMapperProjectWriter.WriteClassVisitorRegistrations(ASL: TStringList; AClassMap: TMapClassDef);
var
  lCtr: Integer;
  lSelect: TClassMappingSelect;
  lBaseSig: string;
  lBaseClassName: string;
begin
  lBaseClassName := Copy(AClassMap.BaseClassName, 2, Length(AClassMap.BaseClassName));
  WriteLine('{ NOTE: The most reliable order of registering visitors are', ASL);
  WriteLine('        Read, Delete, Update, Create }', ASL);

  if AClassMap.AutoCreateListClass then
    WriteLine(Format('GTIOPFManager.VisitorManager.RegisterVisitor(''Load%sList'', %sList_Read);', [lBaseClassName, AClassMap.BaseClassName]), ASL);

  WriteLine(Format('GTIOPFManager.VisitorManager.RegisterVisitor(''Load%s'', %s_Read);', [lBaseClassName, AClassMap.BaseClassName]), ASL);
  WriteLine(Format('GTIOPFManager.VisitorManager.RegisterVisitor(''Save%s'', %s_Delete);', [lBaseClassName, AClassMap.BaseClassName]), ASL);
  WriteLine(Format('GTIOPFManager.VisitorManager.RegisterVisitor(''Save%s'', %s_Update);', [lBaseClassName, AClassMap.BaseClassName]), ASL);
  WriteLine(Format('GTIOPFManager.VisitorManager.RegisterVisitor(''Save%s'', %s_Create);', [lBaseClassName, AClassMap.BaseClassName]), ASL);

  if AClassMap.AutoCreateListClass then
  begin
    if AClassMap.Selections.Count > 0 then
    begin
      for lCtr := 0 to AClassMap.Selections.Count - 1 do
      begin
        lSelect := AClassMap.Selections.Items[lCtr];
        lBaseSig := AClassMap.BaseClassName + 'List_' + lSelect.Name + 'Vis';
        WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + lBaseSig + ''', ' + lBaseSig + ');', ASL);
      end;
    end;
  end;
end;

procedure TMapperProjectWriter.WriteCustomListVisImp(ASL: TStringList; AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
var
  lBaseSig: string;
  lProp: TMapClassProp;
  lParam: TSelectParam;
  lCtr: integer;
  lNeedsHelper: Boolean;
  lGetter: string;
  lPropIndex: Integer;
  lPropMap: TPropMapping;
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  lBaseSig := AClassDef.BaseClassName + 'List_' + ASelect.Name + 'Vis';

  WriteLine('{ ' + lBaseSig + ' }', ASL);

  WriteLine('function ' + lBaseSig + '.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('result := (Visited.ObjectState = posEmpty);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + lBaseSig + '.MapRowToObject;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
  WriteLine('lItemClass: ' + AClassDef.BaseClassName + 'Class;', ASL);
  WriteExtraVarsMaybe(ASL, AClassDef);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('lItemClass := ' + AClassDef.BaseClassName + ';', ASL);
  WriteLine('if Assigned(' + AClassDef.BaseClassName + 'List.ItemClass) then', ASL);
  IncTab;
  WriteLine('lItemClass := ' + AClassDef.BaseClassName + 'List.ItemClass;', ASL);
  DecTab;
  WriteLine('lObj := lItemClass.Create;', ASL);
  WriteLine('lObj.OID.AssignFromTIQuery(''' + AClassDef.ClassMapping.PKName + ''',Query);', ASL);
  WriteMapRowToObject(ASL, AClassDef);
  WriteLine('lObj.ObjectState := posClean;', ASL);
  WriteLine('TtiObjectList(Visited).Add(lObj);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  // SetupParams
  WriteLine('procedure ' + lBaseSig + '.SetupParams;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lCtr: integer;', ASL);
  WriteLine('lParam: TSelectParam;', ASL);
  WriteLine('lList: TtiMappedFilteredObjectList;', ASL);
  WriteExtraVarsMaybe(ASL, AClassDef);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('lList := TtiMappedFilteredObjectList(Visited);', ASL);
  WriteBreak(ASL);

  for lCtr := 0 to ASelect.Params.Count - 1 do
  begin
    lParam := ASelect.Params.Items[lCtr];
    WriteLine('lParam := TSelectParam(lList.Params.FindByProps([''ParamName''], [''' + lParam.ParamName + ''']));', ASL);

    case lParam.ParamType.BaseType of
      ptString, ptAnsiString:
        WriteLine('Query.ParamAsString[''' + lParam.SQLParamName + '''] := lParam.Value;', ASL);
      ptBoolean:
        WriteLine('Query.ParamAsBoolean[''' + lParam.SQLParamName + '''] := lParam.Value;', ASL);
      ptDateTime:
        WriteLine('Query.ParamAsDateTime[''' + lParam.SQLParamName + '''] := lParam.Value;', ASL);
      ptDouble, ptCurrency, ptSingle:
        WriteLine('Query.ParamAsFloat[''' + lParam.SQLParamName + '''] := lParam.Value;', ASL);
      ptInteger, ptInt64:
        WriteLine('Query.ParamAsInteger[''' + lParam.SQLParamName + '''] := lParam.Value;', ASL);
      ptEnum:
        begin
          if Project.DatabaseOptions.EnumerationType = etInt then
//            WriteLine('Query.ParamAsInteger[''' + lParam.SQLParamName + '''] := Integer(' + lParam.ParamTypeName + '(lParam.Value));', ASL)
            WriteLine('Query.ParamAsInteger[''' + lParam.SQLParamName + '''] := Integer(' + lParam.ParamType.TypeName + '(lParam.Value));', ASL)
          else
          begin
            WriteLine('begin', ASL);
            IncTab;
//            WriteLine('lOrdInfo := TypeInfo(' + lParam.TypeName + ');', ASL);
            WriteLine('lOrdInfo := TypeInfo(' + lParam.ParamType.TypeName + ');', ASL);
            WriteLine('Query.ParamAsString[''' + lParam.ParamName + '''] := ' + 'GetEnumName(TypeInfo(TTypeKind), Integer(lOrdInfo^.Kind));', ASL);
            DecTab;
            WriteLine('end;', ASL);
          end;

        end;
      ptEnumSet:
        begin
          WriteLine('begin', ASL);
          IncTab;
          WriteLine('Query.ParamAsString[''' + lParam.ParamName + '''] := ''' + lParam.Value + ''';', ASL);
          DecTab;
          WriteLine('end;', ASL);
        end;
      ptStream:
        begin
          //lNeedsHelper := LowerCase(lParam.ParamTypeName) <> 'tstream';
          lNeedsHelper := lParam.ParamType.BaseType <> ptStream;

          if lNeedsHelper then
          begin
            lProp := AClassDef.ClassProps.FindByName(lParam.ParamName);
            lPropIndex := lProp.Index;
            lPropMap := AClassDef.ClassMapping.PropMappings.Items[lPropIndex];
            lGetter := lPropMap.PropertyGetter;
            if lGetter = '' then
              raise Exception.CreateFmt(ClassName + '.WriteCustomListVisImp: Property mapping for %s must have accessor', [lProp.PropertyType.TypeName]);
          end;
          WriteLine('try', ASL);
          IncTab;
          if lNeedsHelper then
            WriteLine('lStream := ' + lGetter + '();', ASL)
          else
          begin
                  // WRONG!
            WriteLine('lStream := (TStream(PtrUInt(lParam.Value))));', ASL);
            WriteLine('lStreamFree := False;', ASL);
          end;

          WriteLine('Query.AssignParamFromStream(''' + lParam.SQLParamName + ''', lStream);', ASL);
          DecTab;
          WriteLine('finally', ASL);
          IncTab;
          WriteLine('if lStreamFree then', ASL);
          IncTab;
          WriteLine('lStream.Free;', ASL);
          DecTab;
          DecTab;
          WriteLine('end;', ASL);
        end;
    end;
  end;

  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

end;

procedure TMapperProjectWriter.WriteCustomListVisIntf(ASL: TStringList; AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
var
  lBaseSig: string;
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  lBaseSig := AClassDef.BaseClassName + 'List_' + ASelect.Name + 'Vis';

  WriteLine('{ ' + lBaseSig + ' }', ASL);
  WriteLine(lBaseSig + ' = class(TtiMapParameterListReadVisitor)', ASL);
  WriteLine('protected', ASL);
  IncTab;
  WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
  WriteLine('procedure   MapRowToObject; override;', ASL);
  WriteLine('procedure   SetupParams; override;', ASL);
  DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteDeleteSQL(ASL: TStringList; AClassDef: TMapClassDef);
var
  lMapping: TClassMapping;
begin

  lMapping := AClassDef.ClassMapping;

  WriteLine('Query.SQLText := ', ASL);
  IncTab;
  WriteLine('''DELETE FROM ' + lMapping.TableName + ' '' +', ASL);
  WriteLine('''WHERE ' + DoubleQuoteFieldName(AClassDef.ClassMapping.PKField,
    Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ' = :' + AClassDef.ClassMapping.PKField + ''';', ASL);
  DecTab;
end;

procedure TMapperProjectWriter.WriteInsertSQL(ASL: TStringList; AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
  lFieldName: string;
begin
  lMapping := AClassDef.ClassMapping;

  WriteLine('Query.SQLText :=', ASL);
  IncTab;
  WriteLine('''INSERT INTO ' + lMapping.TableName + ' '' +', ASL);
  WriteLine('''('' +', ASL);

  WriteLine('''  ' + DoubleQuoteFieldName(AClassDef.ClassMapping.PKField,
    Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ', '' +', ASL);

  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
  begin
    lPropMap := lMapping.PropMappings.Items[lCtr];

    lFieldName := DoubleQuoteFieldName(lPropMap.FieldName,
      Project.DatabaseOptions.DoubleQuoteDBFieldNames);

    if lCtr < (lMapping.PropMappings.Count - 1) then
      WriteLine('''  ' + lFieldName + ', '' +', ASL)
    else
      WriteLine('''  ' + lFieldName + ''' +', ASL)
  end;

  WriteLine(''') '' +', ASL);
  WriteLine('''VALUES '' +', ASL);
  WriteLine('''('' +', ASL);
  WriteLine('''  :' + AClassDef.ClassMapping.PKField + ', '' +', ASL);

  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
  begin
    lPropMap := lMapping.PropMappings.Items[lCtr];
    if lCtr < (lMapping.PropMappings.Count - 1) then
      WriteLine('''  :' + lPropMap.FieldName + ', '' +', ASL)
    else
      WriteLine('''  :' + lPropMap.FieldName + ''' +', ASL)
  end;

  WriteLine(''')'';', ASL);
  DecTab;
end;

procedure TMapperProjectWriter.WriteIntfUses(ASL: TStringList; AUnitDef: TMapUnitDef);
var
  lCtr: Integer;
begin
  WriteLine('uses', ASL);
  IncTab;
  WriteLine('SysUtils', ASL);
  WriteLine(',tiObject', ASL);

  if Project.HasCustomSelects then
    WriteLine(',typinfo', ASL);

  WriteLine(',tiAutoMap', ASL);
  WriteLine(',tiOPFManager', ASL);
  WriteLine(',tiVisitorDB', ASL);
  WriteLine(',tiVisitorCriteria', ASL);
  WriteLine(',tiCriteria', ASL);
  WriteLine(',tiSQLParser', ASL);
  WriteLine(',mapper', ASL);

  // uses added
  for lCtr := 0 to AUnitDef.References.Count - 1 do
  begin
    WriteLine(',' + AUnitDef.References[lCtr], ASL);
  end;

  WriteLine(';', ASL);

  DecTab;

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteImpUses(ASL: TStringList; AUnitDef: TMapUnitDef);
begin
  WriteLine('uses', ASL);
  IncTab;
  WriteLine('tiUtils', ASL);
  WriteLine(',tiLog', ASL);
  WriteLine(',tiRTTI', ASL);
  WriteLine(';', ASL);
  DecTab;
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteListClassIntf(ASL: TStringList; AClassDef: TMapClassDef);
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  WriteLine('{ List of ' + AClassDef.BaseClassName + '.  TtiMappedFilteredObjectList descendant. }', ASL);
  WriteLine(AClassDef.BaseClassName + 'List = class(' + AClassDef.BaseListClassParent + ')', ASL);
  WriteLine('private', ASL);
  IncTab;
  WriteLine('class var FItemClass: ' + AClassDef.BaseClassName + 'Class;', ASL);
  DecTab;
  WriteLine('protected', ASL);
  IncTab;
  if AClassDef.ListSavesDatabaseName then
  begin
    WriteLine('FDBConnectionName: string;', ASL);
    WriteLine('FPersistenceLayerName: string;', ASL);
  end;
  WriteLine('procedure   SetItems(i: integer; const AValue: ' + AClassDef.BaseClassName + '); reintroduce;', ASL);
  writeLine('function    GetItems(i: integer): ' + AClassDef.BaseClassName + '; reintroduce;', ASL);
  DecTab;
  WriteLine('public', ASL);
  IncTab;
  WriteLine('property    Items[i:integer] : ' + AClassDef.BaseClassName + ' read GetItems write SetItems;', ASL);
  WriteLine('function    Add(const AObject: ' + AClassDef.BaseClassName + '): integer; reintroduce;', ASL);
  WriteLine('procedure   Read; override;', ASL);
  WriteLine('procedure   Save; override;', ASL);
  WriteLine('procedure   Read(const ADBConnectionName: string; APersistenceLayerName: string = ''''); override;', ASL);
  WriteLine('procedure   Save(const ADBConnectionName: string; APersistenceLayerName: string = ''''); override;', ASL);
  WriteLine('class property ItemClass: ' + AClassDef.BaseClassName + 'Class read FItemClass write FItemClass;', ASL);
  if AClassDef.ListSavesDatabaseName then
  begin
    WriteLine('constructor CreateNew(const AOwner: TtiObject; const ADatabaseName: string = ''''; const APersistenceLayerName: string = ''''); override;', ASL);
    WriteLine('constructor CreateNew(const ADatabaseName: string = ''''; const APersistenceLayerName: string = ''''); override;', ASL);
  end;
  WriteLine('{ Return count (1) if successful. }', ASL);
  WriteLine('function    FindByOID(const AOID: string): integer;', ASL);
  WriteClassSelectsInf(ASL, AClassDef);
  DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteListClassImp(ASL: TStringList; AClassDef: TMapClassDef);
var
  lListName: string;
  lBaseClassName: string;
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  lBaseClassName := Copy(AClassDef.BaseClassName, 2, Length(AClassDef.BaseClassName));
  lListName := AClassDef.BaseClassName + 'List';

  WriteLine(' {' + lListName + ' }', ASL);
  WriteBreak(ASL);

  WriteLine('function ' + lListName + '.Add(const AObject: ' + AClassDef.BaseClassName + '): integer;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('result := inherited Add(AObject);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('function ' + lListName + '.GetItems(i: integer): ' + AClassDef.BaseClassName + ';', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('result := inherited GetItems(i) as ' + AClassDef.BaseClassName + ';', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + lListName + '.Read;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  if AClassDef.ListSavesDatabaseName then
    WriteLine('Read(FDBConnectionName, FPersistenceLayerName);', ASL)
  else
    WriteLine('Read('''', '''');', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + lListName + '.Read(const ADBConnectionName: string; APersistenceLayerName: string = '''');', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('GTIOPFManager.VisitorManager.Execute(''Load' + lBaseClassName + 'List'', self, ADBConnectionName, APersistenceLayerName);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + lListName + '.Save;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  if AClassDef.ListSavesDatabaseName then
    WriteLine('Save(FDBConnectionName, FPersistenceLayerName);', ASL)
  else
    WriteLine('Save('''', '''');', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + lListName + '.Save(const ADBConnectionName: string; APersistenceLayerName : string = '''');', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('GTIOPFManager.VisitorManager.Execute(''Save' + lBaseClassName + ''', self, ADBConnectionName, APersistenceLayerName);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  if AClassDef.ListSavesDatabaseName then
  begin
    WriteLine('constructor ' + lListName + '.CreateNew(const AOwner: TtiObject; const ADatabaseName: string = ''''; const APersistenceLayerName: string = '''');', ASL);
    WriteLine('begin', ASL);
    IncTab;
    WriteLine('FDBConnectionName := ADatabaseName;', ASL);
    WriteLine('FPersistenceLayerName := APersistenceLayerName;', ASL);
    WriteLine('inherited CreateNew(Owner, ADatabaseName, APersistenceLayerName);', ASL);
    DecTab;
    WriteLine('end;', ASL);
    WriteBreak(ASL);

    WriteLine('constructor ' + lListName + '.CreateNew(const ADatabaseName: string = ''''; const APersistenceLayerName: string = '''');', ASL);
    WriteLine('begin', ASL);
    IncTab;
    WriteLine('CreateNew(nil, ADatabaseName, APersistenceLayerName);', ASL);
    DecTab;
    WriteLine('end;', ASL);
    WriteBreak(ASL);
  end;

  WriteLine('procedure ' + lListName + '.SetItems(i: integer; const AValue: ' + AClassDef.BaseClassName + ');', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('inherited SetItems(i, AValue);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('function ' + lListName + '.FindByOID(const AOID: string): integer;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('if self.Count > 0 then', ASL);
  IncTab;
  WriteLine('self.Clear;', ASL);
  WriteLine('', ASL);
  DecTab;

  WriteLine('Criteria.ClearAll;', ASL);

  if AClassDef.ClassMapping.OIDType = otString then
    WriteLine('Criteria.AddEqualTo(''' + AClassDef.ClassMapping.PKField + ''', AOID);', ASL)
  else
    WriteLine('Criteria.AddEqualTo(''' + AClassDef.ClassMapping.PKField + ''', StrToInt(AOID));', ASL);

  WriteLine('Read;', ASL);
  WriteLine('result := Count;', ASL);
  DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteListSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin
  lMapping := AClassDef.ClassMapping;

  WriteLine('lSQL :=', ASL);
  IncTab;
  WriteLine('''SELECT '' +', ASL);
  WriteLine(''' ' + AClassDef.ClassMapping.PKField + ', '' +', ASL);

  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
  begin
    lPropMap := lMapping.PropMappings.Items[lCtr];
    if lCtr < (lMapping.PropMappings.Count - 1) then
      WriteLine(''' ' + lPropMap.FieldName + ', '' +', ASL)
    else
      WriteLine(''' ' + lPropMap.FieldName + ' '' +', ASL)
  end;

  WriteLine('''FROM  ' + lMapping.TableName + ' %s %s ;'';', ASL);
  DecTab;
end;

procedure TMapperProjectWriter.WriteMapRowToObject(ASL: TStringList; AClassDef: TMapClassDef);
var
  lCtr: integer;
  lPropMap: TPropMapping;
  lClassProp: TMapClassProp;
  lNeedsHelper: Boolean;
begin
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    lPropMap := AClassDef.ClassMapping.PropMappings.Items[lCtr];
    case lPropMap.PropertyType.BaseType of
      ptString, ptAnsiString:
        begin
          lClassProp := AClassDef.ClassProps.FindByName(lPropMap.PropName);

          if Assigned(lClassProp) and (lClassProp.PropertyType.BaseType = ptDateTime) then
              { DateTime stored as a ISO 8601 date/time string }
            WriteLine('lObj.' + lPropMap.PropName + ' := tiIntlDateStorAsDateTime(Query.FieldAsString[''' + lPropMap.FieldName + ''']);', ASL)
          else
            WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsString[''' + lPropMap.FieldName + '''];', ASL);
        end;
      ptBoolean:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsBoolean[''' + lPropMap.FieldName + '''];', ASL);
      ptEnum:
        if Project.DatabaseOptions.EnumerationType = etInt then
        begin
          lClassProp := AClassDef.ClassProps.FindByName(lPropMap.PropName);
          WriteLine('lObj.' + lPropMap.PropName + ' := ' + lClassProp.PropertyType.TypeName + '(Query.FieldAsInteger[''' + lPropMap.FieldName + ''']);', ASL)
        end
        else
          WriteLine('tiSetProperty(lObj, ''' + lPropMap.PropName + ''', Query.FieldAsString[''' + lPropMap.FieldName + '''];', ASL);
      ptEnumSet:
        WriteLine('SetSetProp(lObj, ''' + lPropMap.PropName + ''',  Query.FieldAsString[''' + lPropMap.FieldName + ''']);', ASL);
      ptDateTime:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsDatetime[''' + lPropMap.FieldName + '''];', ASL);
      ptDouble, ptCurrency, ptSingle:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsFloat[''' + lPropMap.FieldName + '''];', ASL);
      ptInt64, ptInteger:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsInteger[''' + lPropMap.FieldName + '''];', ASL);
      ptStream:
        begin
          lNeedsHelper := (lPropMap.PropertyType.BaseType = ptStream) and (Lowercase(AClassDef.ClassProps.FindByName(lPropMap.PropName).PropertyType.TypeName) <> 'tstream');

          if lNeedsHelper and (lPropMap.PropertySetter = '') then
            raise Exception.CreateFmt(ClassName + '.WriteMapRowToObject: Property mapping for %s.%s must have setter', [AClassDef.BaseClassName, AClassDef.ClassProps.Items[lCtr].PropertyType.TypeName]);

          WriteLine('lStream := TMemoryStream.Create;', ASL);
          WriteLine('try', ASL);
          IncTab;
          WriteLine('Query.AssignFieldAsStream(''' + lPropMap.FieldName + ''', lStream);', ASL);
          WriteLine('lStream.Position := 0;', ASL);
          if lNeedsHelper then
            WriteLine('lObj.' + lPropMap.PropertySetter + '(lStream);', ASL)
          else
          begin
            WriteLine('lObj.' + lPropMap.PropName + ' := lStream;', ASL);
            WriteLine('lStreamFree := False;', ASL);
          end;
          DecTab;
          WriteLine('finally', ASL);
          IncTab;
          WriteLine('if lStreamFree then', ASL);
          IncTab;
          WriteLine('lStream.Free', ASL);
          DecTab;
          DecTab;
          WriteLine('end;', ASL);
        end;
    end;
  end;
end;

procedure TMapperProjectWriter.WriteMapRowToObjectForList(ASL: TStringList; AClassDef: TMapClassDef);
var
  lCtr: integer;
  lPropMap: TPropMapping;
  lClassProp: TMapClassProp;
  lNeedsHelper: Boolean;
begin
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    lPropMap := AClassDef.ClassMapping.PropMappings.Items[lCtr];
    case lPropMap.PropertyType.BaseType of
      ptString, ptAnsiString:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsString[''' + lPropMap.FieldName + '''];', ASL);
      ptBoolean:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsBoolean[''' + lPropMap.FieldName + '''];', ASL);
      ptEnum:
        if Project.DatabaseOptions.EnumerationType = etInt then
        begin
          lClassProp := AClassDef.ClassProps.FindByName(lPropMap.PropName);
          WriteLine('lObj.' + lPropMap.PropName + ' := ' + lClassProp.PropertyType.TypeName + '(Query.FieldAsInteger[''' + lPropMap.FieldName + ''']);', ASL)
        end
        else
          WriteLine('tiSetProperty(lObj, ''' + lPropMap.PropName + ''', Query.FieldAsString[''' + lPropMap.FieldName + '''];', ASL);
      ptEnumSet:
        WriteLine('SetSetProp(lObj, ''' + lPropMap.PropName + ''',  Query.FieldAsString[''' + lPropMap.FieldName + ''']);', ASL);
      ptDateTime:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsDatetime[''' + lPropMap.FieldName + '''];', ASL);
      ptDouble, ptCurrency, ptSingle:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsFloat[''' + lPropMap.FieldName + '''];', ASL);
      ptInt64, ptInteger:
        WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsInteger[''' + lPropMap.FieldName + '''];', ASL);
      ptStream:
        begin
          lNeedsHelper := (lPropMap.PropertyType.BaseType = ptStream) and (Lowercase(AClassDef.ClassProps.Items[lCtr].PropertyType.TypeName) <> 'tstream');
          if lNeedsHelper and (lPropMap.PropertySetter = '') then
            raise Exception.CreateFmt(ClassName + '.WriteMapRowToObjectForList: Property mapping for %s must have accessor', [AClassDef.ClassProps.Items[lCtr].PropertyType.TypeName]);

          WriteLine('lStream := TMemoryStream.Create;', ASL);
          WriteLine('try', ASL);
          IncTab;
          WriteLine('Query.AssignFieldAsStream(''' + lPropMap.FieldName + ''', lStream);', ASL);
          WriteLine('lStream.Position := 0;', ASL);
          if lNeedsHelper then
            WriteLine('lObj.' + lPropMap.PropertySetter + '(lStream);', ASL)
          else
          begin
            WriteLine('lObj.' + lPropMap.PropName + ' := lStream;', ASL);
            WriteLine('lStreamFree := False;', ASL);
          end;
          DecTab;
          WriteLine('finally', ASL);
          IncTab;
          WriteLine('if lStreamFree then', ASL);
          IncTab;
          WriteLine('lStream.Free', ASL);
          DecTab;
          DecTab;
          WriteLine('end;', ASL);
        end;
    end;
  end;

end;

procedure TMapperProjectWriter.WriteClassImpSettersGetters(ASL: TStringList; AClassDef: TMapClassDef);
var
  lCtr: integer;
  lMap: TMapClassProp;
begin
  for lCtr := 0 to AClassDef.ClassProps.Count - 1 do
  begin
    lMap := AClassDef.ClassProps.Items[lCtr];
    if lMap.VirtualGetter then
    begin
      WriteLine('function ' + AClassDef.BaseClassName + '.Get' + lMap.Name + ': ' + lMap.PropertyType.TypeName + ';', ASL);
      WriteLine('begin', ASL);
      IncTab;
      WriteLine('Result := F' + lMap.Name + ';', ASL);
      DecTab;
      WriteLine('end;', ASL);
      WriteBreak(ASL);
    end;
    if lMap.IsReadOnly then
      Continue;
    WriteLine('procedure ' + AClassDef.BaseClassName + '.Set' + lMap.Name + '(const AValue: ' + lMap.PropertyType.TypeName + ');', ASL);
    WriteLine('begin', ASL);
    IncTab;
    WriteLine('if F' + lMap.Name + ' = AValue then', ASL);
    IncTab;
    WriteLine('Exit;', ASL);
    DecTab;
    if AClassDef.NotifyObserversOfPropertyChanges then
      WriteLine('BeginUpdate;', ASL);

    if lMap.PropertyType.BaseType = ptStream then
    begin
      WriteLine('if Assigned(F' + lMap.Name + ') then', ASL);
      WriteLine('  FreeAndNil(F' + lMap.Name + ');', ASL);
    end;
    WriteLine('F' + lMap.Name + ' := AValue;', ASL);
    if AClassDef.NotifyObserversOfPropertyChanges then
      WriteLine('EndUpdate;', ASL);
    DecTab;
    WriteLine('end;', ASL);
    WriteBreak(ASL);
  end;
end;

procedure TMapperProjectWriter.WriteClassIntfDestructor(ASL: TStringList; AClassDef: TMapClassDef);
var
  lCtr: integer;
  lMapping: TPropMapping;
begin
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    lMapping := AClassDef.ClassMapping.PropMappings.Items[lCtr];
    if lMapping.PropertyType.BaseType = ptStream then
    begin
      WriteLine('destructor  Destroy; override;', ASL);
      Exit;
    end;
  end;
end;

procedure TMapperProjectWriter.WriteProject(const ADirectory: string);
var
  lCtr: Integer;
  lSL: TStringList;
  lUnit: TMapUnitDef;
begin
  BaseDir := ADirectory;
  tiForceDirectories1(BaseDir);

  lSL := TStringList.create;
  try
    for lCtr := 0 to Project.Units.Count - 1 do
    begin
      lSL.Clear;
      lUnit := Project.Units.Items[lCtr];
      WriteUnit(lUnit, lSL);
      lSL.SaveToFile(BaseDir + PathDelim + lUnit.Name + '.pas');
    end;
  finally
    lSL.free;
  end;
end;

procedure TMapperProjectWriter.WriteProject(const ADirectory: string; ASL: TStringList);
var
  lCtr: Integer;
  lUnit: TMapUnitDef;
begin
  BaseDir := ADirectory;
  tiForceDirectories1(BaseDir);

  for lCtr := 0 to Project.Units.Count - 1 do
  begin
    lUnit := Project.Units.Items[lCtr];
    WriteBreak(ASL);
    WriteLine('// -------------------------------------------------------------', ASL);
    WriteLine('// Unit Definition: ' + lUnit.Name, ASL);
    WriteLine('// -------------------------------------------------------------', ASL);
    WriteBreak(ASL);
    WriteUnit(lUnit, ASL);
    ASL.SaveToFile(BaseDir + PathDelim + lUnit.Name + '.pas');
  end;
end;

procedure TMapperProjectWriter.WritePropGetter(ASL: TStringList; APropDef: TMapClassProp);
begin
  WriteLine('function    Get' + APropDef.Name + ': ' + APropDef.PropertyType.TypeName + '; virtual;', ASL);
end;

procedure TMapperProjectWriter.WritePropPrivateVars(ASL: TStringList; AClassDef: TMapClassDef);
var
  lCtr: integer;
  lProp: TMapClassProp;
begin
  for lCtr := 0 to AClassDef.ClassProps.Count - 1 do
  begin
    lProp := AClassDef.ClassProps.Items[lCtr];
    WriteLine('F' + lProp.Name + ': ' + lProp.PropertyType.TypeName + ';', ASL);
  end;
end;

procedure TMapperProjectWriter.WritePropSetter(ASL: TStringList; APropDef: TMapClassProp);
begin
  WriteLine('procedure   Set' + APropDef.Name + '(const AValue: ' + APropDef.PropertyType.TypeName + '); virtual;', ASL);
end;

procedure TMapperProjectWriter.WritePropStreamGetter(ASL: TStringList; APropMap: TPropMapping);
begin
  if APropMap.PropertyAccessorsAreAbstract then
    WriteLine('function    ' + APropMap.PropertyGetter + ': TStream; virtual; abstract;', ASL);
end;

procedure TMapperProjectWriter.WritePropStreamSetter(ASL: TStringList; APropMap: TPropMapping);
begin
  if APropMap.PropertyAccessorsAreAbstract then
    WriteLine('procedure   ' + APropMap.PropertySetter + '(const AValue: TStream); virtual; abstract;', ASL);
end;

procedure TMapperProjectWriter.WriteSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin
  lMapping := AClassDef.ClassMapping;

  WriteLine('Query.SQLText :=', ASL);
  IncTab;
  WriteLine('''SELECT'' +', ASL);
  WriteLine('''  ' + DoubleQuoteFieldName(AClassDef.ClassMapping.PKField,
    Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ', '' +', ASL);

  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
  begin
    lPropMap := lMapping.PropMappings.Items[lCtr];

    if lCtr < (lMapping.PropMappings.Count - 1) then
      WriteLine('''  ' + DoubleQuoteFieldName(lPropMap.FieldName, Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ','' +', ASL)
    else
      WriteLine('''  ' + DoubleQuoteFieldName(lPropMap.FieldName, Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ' '' +', ASL)
  end;

  WriteLine('''FROM'' +', ASL);
  WriteLine('''  ' + lMapping.TableName + ' '' +', ASL);
  WriteLine('''WHERE'' +', ASL);
  WriteLine('''  ' + DoubleQuoteFieldName(lMapping.PKField, Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ' = :' + lMapping.PKField + ''';', ASL);
  DecTab;
end;

procedure TMapperProjectWriter.WriteSetupParams(ASL: TStringList; AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lClassProp: TMapClassProp;
  lCtr: integer;
  lGetter: string;
  lNeedsHelper: Boolean;
begin
  WriteLine('lObj.' + AClassDef.ClassMapping.PKName + '.AssignToTIQuery(''' + AClassDef.ClassMapping.PKField + ''',Query);', ASL);
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
  begin
    lPropMap := AClassDef.ClassMapping.PropMappings.Items[lCtr];
    case lPropMap.PropertyType.BaseType of
      ptString, ptAnsiString:
        begin
          lClassProp := AClassDef.ClassProps.FindByName(lPropMap.PropName);
          if Assigned(lClassProp) and (lClassProp.PropertyType.BaseType = ptDateTime) then
              { DateTime stored as a ISO 8601 date/time string }
            WriteLine('Query.ParamAsString[''' + lPropMap.FieldName + '''] := ' + 'tiDateTimeAsIntlDateStor(lObj.' + lPropMap.PropName + ');', ASL)
          else
            WriteLine('Query.ParamAsString[''' + lPropMap.FieldName + '''] := ' + 'lObj.' + lPropMap.PropName + ';', ASL);
        end;
      ptBoolean:
        WriteLine('Query.ParamAsBoolean[''' + lPropMap.FieldName + '''] := ' + 'lObj.' + lPropMap.PropName + ';', ASL);
      ptEnum:
        if Project.DatabaseOptions.EnumerationType = etInt then
          WriteLine('Query.ParamAsInteger[''' + lPropMap.FieldName + '''] := Integer(' + 'lObj.' + lPropMap.PropName + ');', ASL)
        else
          WriteLine('Query.ParamAsString[''' + lPropMap.FieldName + '''] := ' + 'tiGetProperty(lObj, ''' + lPropMap.PropName + ''');', ASL);
      ptEnumSet:
        begin
          WriteLine('Query.ParamAsString[''' + lPropMap.FieldName + '''] := ' + 'GetSetProp(lObj, ''' + lPropMap.PropName + ''');', ASL);
        end;
      ptDateTime:
        WriteLine('Query.ParamAsDateTime[''' + lPropMap.FieldName + '''] := ' + 'lObj.' + lPropMap.PropName + ';', ASL);
      ptDouble, ptCurrency, ptSingle:
        WriteLine('Query.ParamAsFloat[''' + lPropMap.FieldName + '''] := ' + 'lObj.' + lPropMap.PropName + ';', ASL);
      ptInt64, ptInteger:
        WriteLine('Query.ParamAsInteger[''' + lPropMap.FieldName + '''] := ' + 'lObj.' + lPropMap.PropName + ';', ASL);
      ptStream:
        begin
          lNeedsHelper := LowerCase(AClassDef.ClassProps.FindByName(lPropMap.PropName).Propertytype.TypeName) <> 'tstream';
          if lNeedsHelper then
          begin
            lGetter := lPropMap.PropertyGetter;
            if lGetter = '' then
              raise Exception.CreateFmt(ClassName + '.WriteSetupParams: Property mapping for %s.%s must have getter', [AClassDef.BaseClassName, lPropMap.PropName]);
          end;
          WriteLine('try', ASL);
          IncTab;
          if lNeedsHelper then
            WriteLine('lStream := lObj.' + lGetter + '();', ASL)
          else
          begin
            WriteLine('lStreamFree := False;', ASL);
            WriteLine('lStream := ' + 'lObj.' + lPropMap.PropName + ';', ASL);
          end;
          WriteLine('if Assigned(lStream) then', ASL);
          WriteLine('begin', ASL);
          IncTab;
          WriteLine('lStream.Position := 0;', ASL);
          WriteLine('Query.AssignParamFromStream(''' + lPropMap.FieldName + ''', lStream);', ASL);
          DecTab;
          WriteLine('end;', ASL);
          DecTab;
          WriteLine('finally', ASL);
          IncTab;
          WriteLine('if lStreamFree then', ASL);
          IncTab;
          WriteLine('lStream.Free;', ASL);
          DecTab;
          Dectab;
          WriteLine('end;', ASL);

        end;

    end;
  end;
end;

procedure TMapperProjectWriter.WriteSingleClassMethods(ASL: TStringList; AClassDef: TMapClassDef);
begin

end;

procedure TMapperProjectWriter.WriteSingleClassProp(ASL: TStringList; AClassProp: TMapClassProp);
var
  lTemp: string;
begin
  lTemp := 'property    ' + AClassProp.Name + ': ' + AClassProp.PropertyType.TypeName + ' read ';
  if AClassProp.VirtualGetter then
    lTemp := lTemp + 'Get' + AClassProp.Name
  else
    lTemp := lTemp + 'F' + AClassProp.Name;

  if not AClassProp.IsReadOnly then
    lTemp := lTemp + ' write Set' + AClassProp.Name + ';'
  else
    lTemp := lTemp + ';';

  WriteLine(lTemp, ASL);
end;

procedure TMapperProjectWriter.WriteSingleUnitClass(ASL: TStringList; AClassDef: TMapClassDef);
begin

  // Event Notification
  if Assigned(FOnWriteClass) then
    FOnWriteClass(AClassDef);
  WriteLine(AClassDef.BaseClassName + 'Class = class of ' + AClassDef.BaseClassName + ';', ASL);
  WriteLine('{ Generated Class: ' + AClassDef.BaseClassName + '}', ASL);
  WriteLine(AClassDef.BaseClassName + ' = class(' + AClassDef.BaseClassParent + ')', ASL);

  // protected section - includes getters and setters;
  WriteLine('protected', ASL);

  IncTab;
  WritePropPrivateVars(ASL, AClassDef);
  WriteClassPropAccessMethods(ASL, AClassDef);
  DecTab;

  WriteLine('public', ASL);
  IncTab;
  WriteClassIntfDestructor(ASL, AClassDef);
  WriteClassIntfReadMethod(ASL, AClassDef);
  WriteClassIntfSaveMethod(ASL, AClassDef);
  if AClassDef.Validators.Count > 0 then
    WriteClassIsValidIntf(ASL, AClassDef);
  DecTab;

  WriteLine('published', ASL);
  IncTab;
  WriteClassProps(ASL, AClassDef);
  DecTab;

  WriteLine('end;', ASL);

end;

procedure TMapperProjectWriter.WriteSingleUnitEnum(ASL: TStringList; AEnumDef: TMapEnum);
var
  lTemp: string;
  lValues: string;
  lCtr: integer;
  lEnumVal: TMapEnumValue;
  lVal: string;
begin
  lValues := '';

  if AEnumDef.Values.Count = 0 then
    exit;

  // Event Notification
  if Assigned(FOnWriteUnit) then
    FOnWriteEnum(AEnumDef);

  lTemp := AEnumDef.TypeName + ' = (';

  // increase indent
  IncTab;

  try
    for lCtr := 0 to AEnumDef.Values.Count - 1 do
    begin
      lEnumVal := AEnumDef.Values.Items[lCtr];

      if lEnumVal.EnumValue >= 0 then
        lVal := lEnumVal.EnumValueName + ' = ' + IntToStr(lEnumVal.EnumValue)
      else
        lVal := lEnumVal.EnumValueName;

      if lValues = '' then
        lValues := ' ' + lVal
      else
        lValues := lValues + sLineBreak + TabToSpaces(CurrentIndent + 1) + ',' + lVal;
    end;

    lTemp := lTemp + sLineBreak + TabToSpaces(CurrentIndent + 1) + lValues;
    WriteLine(lTemp + sLineBreak + TabToSpaces(CurrentIndent) + ');' + sLineBreak, ASL);

    if AEnumDef.EnumerationSet then
      WriteLine(Format('%s = set of %s;' + sLineBreak, [AEnumDef.EnumerationSetName, AEnumDef.TypeName]), ASL);
  finally
    IncTab(-1);
  end;

end;

procedure TMapperProjectWriter.WriteUnit(AUnit: TMapUnitDef; ASL: TStringList);
var
  lCtr: Integer;
  lClassDef: TMapClassDef;
begin
  // Event Notification
  if Assigned(FOnWriteUnit) then
    FOnWriteUnit(AUnit);

  ASL.Add('unit ' + AUnit.Name + ';');

  if Verbose then
  begin
    WriteLine('// ---------------------------------------------------------', ASL);
    WriteLine('// Automatically generated on ' + FormatDateTime('yyyy-mm-dd HH:hh:ss', now), ASL);
    WriteLine('// Warning: ', ASL);
    WriteLine('//   If you rerun timap, your changes in this file will be lost', ASL);
    WriteLine('// ---------------------------------------------------------', ASL);
  end;

  ASL.Add(sLineBreak);
  ASL.Add('{$IFDEF FPC}');
  ASL.Add('{$mode objfpc}{$H+}');
  ASL.Add('{$ENDIF}');
  ASL.Add(sLineBreak);

  ASL.Add('interface');
  ASL.Add(sLineBreak);

  WriteIntfUses(ASL, AUnit);

  if ((AUnit.UnitClasses.Count > 0) or (AUnit.UnitEnums.Count > 0)) then
  begin
    ASL.Add('type');
    ASL.Add(sLineBreak);

    if AUnit.UnitEnums.Count > 0 then
    begin
      ASL.Add('// Enumerations');
      WriteUnitEnums(ASL, AUnit);
    end;

    if AUnit.UnitClasses.Count > 0 then
    begin
      IncTab;
      WriteLine('// ---------------------------------------------', ASL);
      WriteLine('// Generated Classes', ASL);
      WriteLine('// ---------------------------------------------', ASL);
      DecTab;

      ASL.Add(sLineBreak);
      WriteUnitClasses(ASL, AUnit);

      IncTab;
      for lCtr := 0 to AUnit.UnitClasses.Count - 1 do
      begin
        lClassDef := AUnit.UnitClasses.Items[lCtr];
        WriteClassVisitorIntfs(ASL, lClassDef);
        WriteVisListReadIntf(ASL, lClassDef);

                // Write out any custom list visitors
        WriteAllCustomListVisIntfs(ASL, lClassDef);
      end;
      DecTab;

      WriteBreak(ASL);
          // Write out visitior registrations declaration
      WriteVisRegIntf(ASL);

    end;
  end;

  IncTab;
  WriteAutoMapIntf(ASL);
  DecTab;

  ASL.Add(sLineBreak);
  ASL.Add('implementation');
  ASL.Add(sLineBreak);

  WriteImpUses(ASL, AUnit);

  WriteAllRegisterAutoMaps(ASL, AUnit);

  if AUnit.UnitClasses.Count > 0 then
  begin

    WriteVisRegImp(ASL, AUnit);

    for lCtr := 0 to AUnit.UnitClasses.Count - 1 do
    begin
      lClassDef := AUnit.UnitClasses.Items[lCtr];
      WriteClassImpDestructor(ASL, lClassDef);
      WriteClassImpSettersGetters(ASL, lClassDef);
      WriteClassImpReadMethod(ASL, lClassDef);
      WriteClassImpSavemethod(ASL, lClassDef);
      WriteClassIsValidImp(ASL, lClassDef);
      WriteListClassImp(ASL, lClassDef);
      WriteClassSelectsImps(ASL, lClassDef);
    end;

      // Write out Visitor implementations
    for lCtr := 0 to AUnit.UnitClasses.Count - 1 do
    begin
      lClassDef := AUnit.UnitClasses.Items[lCtr];
      WriteVisClassCreateImp(ASL, lClassDef);
      WriteVisClassUpdateImp(ASL, lClassDef);
      WriteVisClassReadImp(ASL, lClassDef);
      WriteVisClassDeleteImp(ASL, lClassDef);
      WriteVisListReadImp(ASL, lClassDef);

      WriteAllCustomListVisImps(ASL, lClassDef);
    end;
  end;

  ASL.Add('initialization');
  IncTab;
  WriteLine('RegisterVisitors;', ASL);
  WriteLine('RegisterMappings;', ASL);
  DecTab;
  //ASL.Add('// finalization');
  ASL.Add(sLineBreak);
  ASL.Add('end.');

end;

procedure TMapperProjectWriter.WriteUnitClasses(ASL: TStringList; AUnitDef: TMapUnitDef);
var
  lCtr: integer;
  lClass: TMapClassDef;
begin
  IncTab;

  for lCtr := 0 to AUnitDef.UnitClasses.Count - 1 do
  begin
    lClass := AUnitDef.UnitClasses.Items[lCtr];
    WriteSingleUnitClass(ASL, lClass);
    WriteBreak(ASL);
    WriteListClassIntf(ASL, lClass);
  end;

  DecTab;
end;

procedure TMapperProjectWriter.WriteUnitEnums(ASL: TStringList; AUnitDef: TMapUnitDef);
var
  lCtr: integer;
  lEnum: TMapEnum;
begin
  for lCtr := 0 to AUnitDef.UnitEnums.Count - 1 do
  begin
    lEnum := AUnitDef.UnitEnums.Items[lCtr];
    WriteSingleUnitEnum(ASL, lEnum);
  end;

  WriteLine(sLineBreak, ASL);
end;

procedure TMapperProjectWriter.WriteUpdateSQL(ASL: TStringList; AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin
  lMapping := AClassDef.ClassMapping;

  WriteLine('Query.SQLText :=', ASL);
  IncTab;
  WriteLine('''UPDATE ' + lMapping.TableName + ' '' +', ASL);
  WriteLine('''SET'' +', ASL);

  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
  begin
    lPropMap := lMapping.PropMappings.Items[lCtr];

    if lCtr < (lMapping.PropMappings.Count - 1) then
      WriteLine('''  ' + DoubleQuoteFieldName(lPropMap.FieldName, Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ' = :' + lPropMap.FieldName + ','' +', ASL)
    else
      WriteLine('''  ' + DoubleQuoteFieldName(lPropMap.FieldName, Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ' = :' + lPropMap.FieldName + ' '' +', ASL)
  end;

  WriteLine('''WHERE '' +', ASL);
  WriteLine('''  ' + DoubleQuoteFieldName(lMapping.PKField,
    Project.DatabaseOptions.DoubleQuoteDBFieldNames) + ' = :' + lMapping.PKField + ''';', ASL);
  DecTab;
end;

procedure TMapperProjectWriter.WriteVisClassCreateIntf(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ Create Visitor for ' + AClassDef.BaseClassName + ' }', ASL);
  WriteLine(AClassDef.BaseClassName + '_Create = class(TtiVisitorUpdate)', ASL);
  WriteLine('protected', ASL);
  IncTab;
  WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
  WriteLine('procedure   Init; override;', ASL);
  WriteLine('procedure   SetupParams; override;', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassCreateImp(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + '_Create }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + '_Create.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('result := (Visited is ' + AClassDef.BaseClassName + ') and (Visited.ObjectState = posCreate);', ASL);
  WriteLine('Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Create.Init;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteInsertSQL(ASL, AClassDef);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Create.SetupParams;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
  WriteExtraVarsMaybe(ASL, AClassDef);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
  WriteSetupParams(ASL, AClassDef);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassDeleteIntf(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ Delete Visitor for ' + AClassDef.BaseClassName + ' }', ASL);
  WriteLine(AClassDef.BaseClassName + '_Delete = class(TtiVisitorUpdate)', ASL);
  WriteLine('protected', ASL);
  IncTab;
  WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
  WriteLine('procedure   Init; override;', ASL);
  WriteLine('procedure   SetupParams; override;', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassDeleteImp(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + '_Delete }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + '_Delete.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('result := (Visited is ' + AClassDef.BaseClassName + ') and (Visited.ObjectState = posDelete);', ASL);
  WriteLine('Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Delete.Init;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteDeleteSQL(ASL, AClassDef);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Delete.SetupParams;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
  WriteLine('lObj.' + AClassDef.ClassMapping.PKName + '.AssignToTIQuery(''' + AClassDef.ClassMapping.PKField + ''',Query);', ASL);
  DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassReadIntf(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ Read Visitor for ' + AClassDef.BaseClassName + ' }', ASL);
  WriteLine(AClassDef.BaseClassName + '_Read = class(TtiVisitorSelect)', ASL);
  WriteLine('protected', ASL);
  IncTab;
  WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
  WriteLine('procedure   Init; override;', ASL);
  WriteLine('procedure   SetupParams; override;', ASL);
  WriteLine('procedure   MapRowToObject; override;', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassReadImp(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + '_Read }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + '_Read.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('result := (Visited is ' + AClassDef.BaseClassName + ') and ((Visited.ObjectState = posPK) OR (Visited.ObjectState = posEmpty));', ASL);
  WriteLine('Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Read.Init;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteSelectSQL(ASL, AClassDef);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Read.SetupParams;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
  WriteLine('lObj.' + AClassDef.ClassMapping.PKName + '.AssignToTIQuery(''' + AClassDef.ClassMapping.PKField + ''',Query);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Read.MapRowToObject;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
  WriteExtraVarsMaybe(ASL, AClassDef);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
  WriteLine('lObj.' + AClassDef.ClassMapping.PKName + '.AssignFromTIQuery(''' + AClassDef.ClassMapping.PKField + ''',Query);', ASL);
  WriteMapRowToObject(ASL, AClassDef);
  DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassUpdateIntf(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ Update Visitor for ' + AClassDef.BaseClassName + ' }', ASL);
  WriteLine(AClassDef.BaseClassName + '_Update = class(TtiVisitorUpdate)', ASL);
  WriteLine('protected', ASL);
  IncTab;
  WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
  WriteLine('procedure   Init; override;', ASL);
  WriteLine('procedure   SetupParams; override;', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassUpdateImp(ASL: TStringList; AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + '_Update }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + '_Update.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('result := (Visited is ' + AClassDef.BaseClassName + ') and (Visited.ObjectState = posUpdate);', ASL);
  WriteLine('Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Update.Init;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteUpdateSQL(ASL, AClassDef);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Update.SetupParams;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
  WriteExtraVarsMaybe(ASL, AClassDef);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
  WriteSetupParams(ASL, AClassDef);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisListReadIntf(ASL: TStringList; AClassDef: TMapClassDef);
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  WriteLine('{ List Read Visitor for ' + AClassDef.BaseClassName + 'List }', ASL);
  WriteLine(AClassDef.BaseClassName + 'List_Read = class(TtiVisitorSelect)', ASL);
  WriteLine('protected', ASL);
  IncTab;
  WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
  WriteLine('procedure   Init; override;', ASL);
      //WriteLine('procedure   SetupParams; override;', ASL);
  WriteLine('procedure   MapRowToObject; override;', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisListReadImp(ASL: TStringList; AClassDef: TMapClassDef);
begin
  if not AClassDef.AutoCreateListClass then
    exit;

  WriteLine('{ ' + AClassDef.BaseClassName + 'List_Read }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + 'List_Read.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('result := (Visited.ObjectState = posEmpty);', ASL);
  WriteLine('Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Read.Init;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lFiltered: ItiFiltered;', ASL);
  WriteLine('lWhere: string;', ASL);
  WriteLine('lOrder: string;', ASL);
  WriteLine('lSQL: string;', ASL);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;

  WriteLine('if Supports(Visited, ItiFiltered, lFiltered) then', ASL);
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('if lFiltered.GetCriteria.HasCriteria then', ASL);
  IncTab;
  WriteLine('lWhere := '' WHERE '' + tiCriteriaAsSQL(lFiltered.GetCriteria)', ASL);
  DecTab;
  WriteLine('else', ASL);
  IncTab;
  WriteLine('lWhere := '''';', ASL);
  DecTab;

  WriteBreak(ASL);
  WriteLine('if lFiltered.GetCriteria.HasOrderBy then', ASL);
  IncTab;
  WriteLine('lOrder := tiCriteriaOrderByAsSQL(lFiltered.GetCriteria)', ASL);
  DecTab;
  WriteLine('else', ASL);
  IncTab;
  WriteLine('lOrder := '''';', ASL);
  DecTab;
  DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);
  WriteListSelectSQL(ASL, AClassDef);
  WriteBreak(ASL);

  WriteLine('Query.SQLText := gFormatSQL(Format(lSQL, [lWhere, lOrder]), ' + AClassDef.BaseClassName + ');', ASL);

  DecTab;

  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Read.MapRowToObject;', ASL);
  WriteLine('var', ASL);
  IncTab;
  WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
  WriteLine('lItemClass: ' + AClassDef.BaseClassName + 'Class;', ASL);
  WriteExtraVarsMaybe(ASL, AClassDef);
  DecTab;
  WriteLine('begin', ASL);
  IncTab;
  WriteLine('lItemClass := ' + AClassDef.BaseClassName + ';', ASL);
  WriteLine('if Assigned(' + AClassDef.BaseClassName + 'List.ItemClass) then', ASL);
  IncTab;
  WriteLine('lItemClass := ' + AClassDef.BaseClassName + 'List.ItemClass;', ASL);
  DecTab;
  WriteLine('lObj := lItemClass.Create;', ASL);
  WriteLine('lObj.' + AClassDef.ClassMapping.PKName + '.AssignFromTIQuery(''' + AClassDef.ClassMapping.PKField + ''',Query);', ASL);
  WriteMapRowToObject(ASL, AClassDef);
  WriteLine('lObj.ObjectState := posClean;', ASL);
  WriteLine('TtiObjectList(Visited).Add(lObj);', ASL);
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisRegImp(ASL: TStringList; AUnitDef: TMapUnitDef);
var
  lCtr: integer;
  lMap: TMapClassDef;
begin
  WriteLine('procedure RegisterVisitors;', ASL);
  WriteLine('begin', ASL);
  IncTab;
  for lCtr := 0 to AUnitDef.UnitClasses.Count - 1 do
  begin
    lMap := AUnitDef.UnitClasses.Items[lCtr];
    WriteClassVisitorRegistrations(ASL, lMap);
    if lCtr < AUnitDef.UnitClasses.Count - 1 then // we don't want a blank line at the end
      WriteBreak(ASL);
  end;
  DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisRegIntf(ASL: TStringList);
begin
  IncTab;
  WriteLine('{ Visitor Manager Registrations }', ASL);
  WRiteLine('procedure RegisterVisitors;', ASL);
  DecTab;
  WriteBreak(ASL);
end;

end.

