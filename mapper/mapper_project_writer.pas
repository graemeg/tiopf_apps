unit mapper_project_writer;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}


interface

uses
  Classes
  ,SysUtils
  ,mapper
  ;

type

  TMapperProjectWriter = class(TMapSchemaWriter)
  private
    FBaseDir: string;
    procedure SetBaseDir(const AValue: string);
  protected
    procedure   PrepareUnitList(AList: TStringList);
//    procedure   WriteProjectUnits;
    procedure   WriteUnitEnums(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure   WriteSingleUnitEnum(ASL: TStringList; AEnumDef: TMapEnum);
    procedure   WriteUnitClasses(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure   WriteSingleUnitClass(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassProps(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteSingleClassProp(ASL: TStringList; AClassProp: TMapClassProp);
    procedure   WriteClassPropAccessMethods(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WritePropPrivateVars(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WritePropGetter(ASL: TStringList; APropDef: TMapClassProp);
    procedure   WritePropSetter(ASL: TStringList; APropDef: TMapClassProp);
    procedure   WriteClassMappings(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassSelectsInf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassSelectsImps(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassMethods(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure   WriteSingleClassMethods(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteORMClass(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassIntfMethods(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassImpSettersGetters(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassIntfReadMethod(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteIntfUses(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure   WriteListClassIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteListClassImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassIntfSaveMethod(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassImpReadMethod(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteClassImpSavemethod(ASL: TStringList; AClassDef: TmapClassDef);
    // Visitor methods writing
    procedure   WriteVisRegIntf(ASL: TStringList);
    procedure   WriteVisRegImp(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure   WriteClassVisitorRegistrations(ASL: TStringList; AClassMap: TMapClassDef);
    procedure   WriteClassVisitorIntfs(ASL: TStringList; AClassMap: TMapClassDef);
    procedure   WriteVisClassCreateIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisClassCreateImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisClassDeleteIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisClassDeleteImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisClassUpdateIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisClassUpdateImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisClassReadIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisClassReadImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisListReadIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisListReadImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisListCreateIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisListCreateImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisListUpdateIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisListUpdateImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisListDeleteIntf(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteVisListDeleteImp(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteCustomListVisIntf(ASL: TStringList; AClassDef: TMapClassDef;
      ASelect: TClassMappingSelect);
    procedure   WriteCustomListVisImp(ASL: TStringList; AClassDef: TMapClassDef;
      ASelect: TClassMappingSelect);
    procedure   WriteAllCustomListVisIntfs(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteAllCustomListVisImps(ASL: TStringList; AClassDef: TMapClassDef);
    // AutoMap Registration
    procedure   WriteAutoMapIntf(ASL: TStringList);
    procedure   WriteAllRegisterAutoMaps(ASL: TStringList; AUnitDef: TMapUnitDef);
    procedure   WriteClassRegisterAutoMapImp(ASL: TStringList; AClassDef: TMapClassDef);
    // SQL construction
    procedure   WriteInsertSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteUpdateSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteBaseSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteListSelectSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteDeleteSQL(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteSetupParams(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteMapRowToObject(ASL: TStringList; AClassDef: TMapClassDef);
    procedure   WriteMapRowToObjectForList(ASL: TStringList; AClassDef: TMapClassDef);
    // Custom Select Methods
    procedure   WriteClassListSelectMethodIntf(ASL: TStringList; AClassDef: TMapClassDef;
      ASelect: TClassMappingSelect);
    procedure   WriteClassListSelectMethodImp(ASL: TStringList; AClassDef: TMapClassDef;
      ASelect: TClassMappingSelect);
  public
    property    BaseDir: string read FBaseDir write SetBaseDir;
    procedure   WriteUnit(AUnit: TMapUnitDef; ASL: TStringList); virtual;
    {: Expects to have NO trailing path delim to ADirectory parameter string.}
    procedure   WriteProject(const ADirectory: String); overload; override;
    procedure   WriteProject(const ADirectory: string; ASL: TStringList); overload; override;
    constructor Create(AProject: TMapProject); override;
    destructor  Destroy; override;
  end;

implementation

{ TMapperProjectWriter }

constructor TMapperProjectWriter.Create(AProject: TMapProject);
begin
  inherited Create(AProject);
end;

destructor TMapperProjectWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TMapperProjectWriter.PrepareUnitList(AList: TStringList);
begin
  AList.Clear;
end;

procedure TMapperProjectWriter.SetBaseDir(const AValue: string);
begin
  if FBaseDir=AValue then exit;
  FBaseDir:=AValue;
end;

procedure TMapperProjectWriter.WriteAllCustomListVisImps(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lSelect: TClassMappingSelect;
  lCtr: integer;
begin
  for lCtr := 0 to AClassDef.Selections.Count - 1 do
    begin
      lSelect := AClassDef.Selections.Items[lCtr];
      WriteCustomListVisImp(ASL, AClassDef, lSelect);
    end;
end;

procedure TMapperProjectWriter.WriteAllRegisterAutoMaps(ASL: TStringList;
  AUnitDef: TMapUnitDef);
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
        end;
    DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);


end;

procedure TMapperProjectWriter.WriteAllCustomListVisIntfs(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lSelect: TClassMappingSelect;
  lCtr: integer;
begin
  for lCtr := 0 to AClassDef.Selections.Count - 1 do
    begin
      lSelect := AClassDef.Selections.Items[lCtr];
      WriteCustomListVisIntf(ASL, AClassDef, lSelect);
    end;
end;

procedure TMapperProjectWriter.WriteAutoMapIntf(ASL: TStringList);
begin
  WriteLine('{ Register Auto Mappings }', ASL);
  WRiteLine('procedure RegisterMappings;', ASL);
end;

procedure TMapperProjectWriter.WriteBaseSelectSQL(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin

  lMapping := AClassDef.ClassMapping;

  WriteLine('''SELECT '' + ', ASL);

   WriteLine(''' ' + AClassDef.ClassMapping.PKField + ', '' +', ASL);

  //' + lMapping.TableName + ' SET '' +', ASL);
  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
    begin
      lPropMap := lMapping.PropMappings.Items[lCtr];
      if lCtr < (lMapping.PropMappings.Count -1) then
        WriteLine(''' ' + lPropMap.FieldName + ', '' + ', ASL)
      else
        WriteLine(''' ' + lPropMap.FieldName + ' '' + ', ASL)
    end;

  WriteLine('''FROM  ' + lMapping.TableName + ' ''', ASL);

end;

procedure TMapperProjectWriter.WriteClassSelectsImps(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lSelect: TClassMappingSelect;
  lCtr: integer;
begin

  for lCtr := 0 to AClassDef.Selections.Count - 1 do
    begin
      lSelect := AClassDef.Selections.Items[lCtr];
      WriteClassListSelectMethodImp(ASL, AClassDef, lSelect);
    end;
end;

procedure TMapperProjectWriter.WriteClassRegisterAutoMapImp(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lMapping: TClassMapping;
  lPropMap: TPropMapping;
  lCtr: Integer;
begin

  lMapping := AClassDef.ClassMapping;
  if lMapping.PropMappings.Count = 0 then
    exit;

  WriteLine('{ Automap registrations for ' + AClassDef.BaseClassName + ' }', ASL);

  // Write out OID mapping first
  WriteLine('GTIOPFManager.ClassDBMappingMgr.RegisterMapping(' + AClassDef.BaseClassName + ', ', ASL);
    IncTab;
      WriteLine(QuotedStr(lMapping.TableName) + ', ' + QuotedStr(lMapping.PKName) + ', ' +
        QuotedStr(lMapping.PKField) + ', [pktDB]);', ASL);
    DecTab;

  for lCtr := 0 to lMapping.PropMappings.Count - 1 do
    begin
      lPropMap := lMapping.PropMappings.Items[lCtr];
      WriteLine('GTIOPFManager.ClassDBMappingMgr.RegisterMapping(' + AClassDef.BaseClassName + ',', ASL);
      IncTab;
        WriteLine(QuotedStr(lMapping.TableName) + ',' + QuotedStr(lPropMap.PropName) + ', ' +
          QuotedStr(lPropMap.FieldName) + ');', ASL);
      DecTab;
    end;

  // Finally, register list class for class.

  if AClassDef.AutoCreateListClass then
    WriteLine('GTIOPFManager.ClassDBMappingMgr.RegisterCollection(' +
      AClassDef.BaseClassName + 'List, ' + AClassDef.BaseClassName + ');', ASL);

  WriteBreak(ASL);

end;

procedure TMapperProjectWriter.WriteClassImpReadMethod(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('procedure ' + AClassDef.BaseClassName + '.Read;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('GTIOPFManager.VisitorManager.Execute(ClassName + ''read'', self);', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteClassImpSavemethod(ASL: TStringList;
  AClassDef: TmapClassDef);
begin
  WriteLine('procedure ' + AClassDef.BaseClassName + '.Save;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('Case ObjectState of', ASL);
      IncTab;
        WriteLine('posDelete: GTIOPFManager.VisitorManager.Execute(''' + AClassDef.BaseClassName + 'delete'', self);', ASL);
        WriteLine('posUpdate: GTIOPFManager.VisitorManager.Execute(''' + AClassDef.BaseClassName + 'save'', self);', ASL);
        WriteLine('posCreate: GTIOPFManager.VisitorManager.Execute(''' + AClassDef.BaseClassName + 'create'', self);', ASL);
      DecTab;
      WriteLine('end;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteORMClass(ASL: TStringList;
  AClassDef: TMapClassDef);
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

procedure TMapperProjectWriter.WriteClassMappings(ASL: TStringList;
  AClassDef: TMapClassDef);
begin

end;

procedure TMapperProjectWriter.WriteClassMethods(ASL: TStringList;
  AUnitDef: TMapUnitDef);
begin

end;


procedure TMapperProjectWriter.WriteClassIntfMethods(ASL: TStringList;
  AClassDef: TMapClassDef);
begin

end;

procedure TMapperProjectWriter.WriteClassIntfReadMethod(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('procedure   Read; override;', ASL);
end;

procedure TMapperProjectWriter.WriteClassIntfSaveMethod(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('procedure   Save; override;', ASL);
end;

procedure TMapperProjectWriter.WriteClassListSelectMethodImp(ASL: TStringList;
  AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
var
  lCtr: integer;
  lParamStr, lParamSig: string;
  lBaseSig: string;
  lParam: TSelectParam;
  lTempStr: string;
  lSL: TStringList;
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
      lParamSig := lParamSig + lParam.PassBy + ' ' + lParam.ParamName + ': ' + lParam.ParamTypeName;
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
          case lParam.ParamType of
            ptString, ptAnsiString: lParamStr := 'ptString';
            ptBoolean: lParamStr := 'ptBoolean';
            ptDateTime: lParamStr := 'ptDateTime';
            ptFloat: lParamStr := 'ptFloat';
            ptInt64, ptInteger: lParamStr := 'ptInteger';
          end;

          //AddParam('user_oid', ptString, AUser);
          WriteLine('AddParam('+ QuotedStr(lParam.ParamName) + ', ' + QuotedStr(lParam.SQLParamName) + ', ' +
            lParamStr + ', ' + lParam.ParamName + ');', ASL);
        end;

      lBaseSig := AClassDef.BaseClassName + 'List_' + ASelect.Name + 'Vis';

      // Write out sql to use

      WriteLine('self.SQL := ', ASL);

        IncTab;

          lSL := TStringList.create;
          try
            lSL.Text := WrapText(ASelect.SQL.Text, 50);
            for lCtr := 0 to lSL.Count -1 do
              begin
                if lCtr < (lSL.Count -1) then
                  WriteLine(''' ' + lSL[lCtr] + ' '' + ', ASL)
                else
                  WriteLine(''' ' + lSL[lCtr] + '''; ', ASL);
              end;
          finally
            lSL.Free;
          end;
        DecTab;


      WriteLine('GTIOPFManager.VisitorManager.Execute(' + QuotedStr(lBaseSig) + ', self);', ASL);
      WriteLine('result := self.Count;', ASL);

    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

end;

procedure TMapperProjectWriter.WriteClassPropAccessMethods(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lProp: TMapClassProp;
  lCtr: Integer;
begin
  for lCtr := 0 to AClassDef.ClassProps.Count - 1 do
    begin
      lProp := AClassDef.ClassProps.Items[lCtr];

      WritePropGetter(ASL, lProp);

      if not lProp.IsReadOnly then
        WritePropSetter(ASL, lProp);

    end;
end;

procedure TMapperProjectWriter.WriteClassProps(ASL: TStringList;
  AClassDef: TMapClassDef);
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

procedure TMapperProjectWriter.WriteClassListSelectMethodIntf(ASL: TStringList;
  AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
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

      lParamSig := lParamSig + lParam.PassBy + ' ' + lParam.ParamName + ': ' + lParam.ParamTypeName;
    end;

  if lParamSig <> '' then
    lTempStr := lTempStr + lParamSig + ')';

  lTempStr := lTempStr + ': integer;';

  WriteLine(lTempStr, ASL);

end;

procedure TMapperProjectWriter.WriteClassSelectsInf(ASL: TStringList;
  AClassDef: TMapClassDef);
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

procedure TMapperProjectWriter.WriteClassVisitorIntfs(ASL: TStringList;
  AClassMap: TMapClassDef);
begin
  //WriteVisListReadIntf(ASL, AClassMap);
  WriteVisClassReadIntf(ASL, AClassMap);
  WriteVisClassCreateIntf(ASL, AClassMap);
  WriteVisClassUpdateIntf(ASL, AClassMap);
  WriteVisClassDeleteIntf(ASL, AClassMap);
end;

procedure TMapperProjectWriter.WriteClassVisitorRegistrations(ASL: TStringList;
  AClassMap: TMapClassDef);
var
  lCtr: Integer;
  lSelect: TClassMappingSelect;
  lBaseSig: string;
begin

  WriteLine('{ Register Visitors for ' + AClassMap.BaseClassName + ' }', ASL);

  WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + AClassMap.BaseClassName + 'List_listread'', ' +
    AClassMap.BaseClassName + 'List_Read);', ASL);
  WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + AClassMap.BaseClassName + 'List_listsave'', ' +
    AClassMap.BaseClassName + 'List_Create);', ASL);
  WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + AClassMap.BaseClassName + 'List_listsave'', ' +
    AClassMap.BaseClassName + 'List_Save);', ASL);
  WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + AClassMap.BaseClassName + 'List_listsave'', ' +
    AClassMap.BaseClassName + 'List_Delete);', ASL);

  WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + AClassMap.BaseClassName + 'read'', ' +
    AClassMap.BaseClassName + '_Read);', ASL);
  WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + AClassMap.BaseClassName + 'save'', ' +
    AClassMap.BaseClassName + '_Save);', ASL);
  WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + AClassMap.BaseClassName + 'delete'', ' +
    AClassMap.BaseClassName + '_Delete);', ASL);
  WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + AClassMap.BaseClassName + 'create'', ' +
    AClassMap.BaseClassName + '_Create);', ASL);


  if AClassMap.Selections.Count > 0 then
    begin
      for lCtr := 0 to AClassMap.Selections.Count - 1 do
        begin
          lSelect := AClassMap.Selections.Items[lCtr];
          lBaseSig := AClassMap.BaseClassName + 'List_' + lSelect.Name + 'Vis';
          WriteLine('GTIOPFManager.VisitorManager.RegisterVisitor(''' + lBaseSig + ''', ' + lBaseSig + ');', ASL);
        end;
    end;

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteCustomListVisImp(ASL: TStringList;
  AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
var
  lBaseSig: string;
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
    DecTab;
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('lObj := ' + AClassDef.BaseClassName + '.Create;', ASL);
      WriteLine('lObj.OID.AssignFromTIQuery(''' + AClassDef.ClassMapping.PKName + ''',Query);', ASL);
      WriteMapRowToObject(ASL, AClassDef);
      WriteLine('lObj.ObjectState := posClean;', ASL);
      WriteLine('TtiObjectList(Visited).Add(lObj);', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

end;

procedure TMapperProjectWriter.WriteCustomListVisIntf(ASL: TStringList;
  AClassDef: TMapClassDef; ASelect: TClassMappingSelect);
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
    DecTab;
  WriteLine('end;', ASL);

  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteDeleteSQL(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lMapping: TClassMapping;
begin

  lMapping := AClassDef.ClassMapping;

  WriteLine('Query.SQLText := ', ASL);
    IncTab;
      WriteLine('''DELETE FROM ' + lMapping.TableName + ' '' +', ASL);
      WriteLine('''WHERE OID = :OID'';', ASL);
    DecTab;
end;

procedure TMapperProjectWriter.WriteInsertSQL(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin

  lMapping := AClassDef.ClassMapping;

  WriteLine('Query.SQLText := ', ASL);
    IncTab;
      WriteLine('''INSERT INTO ' + lMapping.TableName + '('' + ', ASL);
      WriteLine(''' OID, '' + ', ASL);
      for lCtr := 0 to lMapping.PropMappings.Count - 1 do
        begin
          lPropMap :=  lMapping.PropMappings.Items[lCtr];
          if lCtr < (lMapping.PropMappings.Count -1) then
            WriteLine(''' ' + lPropMap.FieldName + ', '' + ', ASL)
          else
            WriteLine(''' ' + lPropMap.FieldName + ''' + ', ASL)
        end;

      WriteLine(''') VALUES ('' +', ASL);
      WriteLine(''' :OID, '' +', ASL);

      for lCtr := 0 to lMapping.PropMappings.Count - 1 do
        begin
          lPropMap := lMapping.PropMappings.Items[lCtr];
          if lCtr < (lMapping.PropMappings.Count -1) then
            WriteLine(''' :' + lPropMap.FieldName + ', '' + ', ASL)
          else
            WriteLine(''' :' + lPropMap.FieldName + ''' + ', ASL)
        end;

      WriteLine(''') '';', ASL);
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
      //if Project.HasCustomSelects then
      //  WriteLine(',tiFilteredObjectList', ASL);
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

procedure TMapperProjectWriter.WriteListClassIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  if AClassDef.AutoCreateListClass then
    begin
      WriteLine('{ List of ' + AClassDef.BaseClassName + '.  TtiMappedFilteredObjectList descendant. }', ASL);
      WriteLine(AClassDef.BaseClassName + 'List = class(TtiMappedFilteredObjectList)', ASL);
      WriteLine('protected', ASL);
        IncTab;
          WriteLine('procedure   SetItems(i: integer; const AValue: ' + AClassDef.BaseClassName + '); reintroduce;', ASL);
          writeLine('function    GetItems(i: integer): ' + AClassDef.BaseClassName + '; reintroduce;', ASL);
        DecTab;
      WriteLine('public', ASL);
        IncTab;
          WriteLine('property    Items[i:integer] : ' + AClassDef.BaseClassName + ' read GetItems write SetItems;', ASL);
          WriteLine('procedure   Add(AObject: ' + AClassDef.BaseClassName + '); reintroduce;', ASL);
          WriteLine('procedure   Read; override;', ASL);
          WriteLine('procedure   Save; override;', ASL);
          WriteLine('{ Return count (1) if successful. }', ASL);
          WriteLine('function    FindByOID(const AOID: string): integer;', ASL);
          WriteClassSelectsInf(ASL, AClassDef);
        DecTab;
      WriteLine('end;', ASL);

      WriteBreak(ASL);
    end;
end;

procedure TMapperProjectWriter.WriteListClassImp(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lListName: string;
begin

  if not AClassDef.AutoCreateListClass then
    exit;

  lListName := AClassDef.BaseClassName + 'List';

  WriteLine(' {' + lListName + ' }', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + lListName + '.Add(AObject: ' + AClassDef.BaseClassName + ');', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('inherited Add(AObject);', ASL);
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
      WriteLine('GTIOPFManager.VisitorManager.Execute(''' + AClassDef.BaseClassName + 'List_listread'', self);', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + lListName + '.Save;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('GTIOPFManager.VisitorManager.Execute(''' + AClassDef.BaseClassName + 'List_listsave'', self);', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + lListName + '.SetItems(i: integer; const AValue: ' + AClassDef.BaseClassName + ');', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('inherited SetItems(i, AValue);', ASL);
    DecTab;
  WriteLine('end;', ASL);

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
        WriteLine('Criteria.AddEqualTo(' + AClassDef.ClassMapping.PKField + ', StrToInt(AOID));', ASL);

      WriteLine('Read;', ASL);
      WriteLine('result := Count;', ASL);
    DecTab;
  WriteLine('end;', ASL);



  WriteBreak(ASL);

end;

procedure TMapperProjectWriter.WriteListSelectSQL(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin

  lMapping := AClassDef.ClassMapping;

  WriteLine('lSQL := ', ASL);
    IncTab;

      WriteLine('''SELECT '' + ', ASL);

      WriteLine(''' ' + AClassDef.ClassMapping.PKField + ', '' +', ASL);

      //' + lMapping.TableName + ' SET '' +', ASL);
      for lCtr := 0 to lMapping.PropMappings.Count - 1 do
        begin
          lPropMap := lMapping.PropMappings.Items[lCtr];
          if lCtr < (lMapping.PropMappings.Count -1) then
            WriteLine(''' ' + lPropMap.FieldName + ', '' + ', ASL)
          else
            WriteLine(''' ' + lPropMap.FieldName + ' '' + ', ASL)
        end;

      WriteLine('''FROM  ' + lMapping.TableName + ' %s %s ;'';', ASL);

    DecTab;

end;

procedure TMapperProjectWriter.WriteMapRowToObject(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lCtr: integer;
  lPropMap: TPropMapping;
  lClassProp: TMapClassProp;
begin
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
    begin
      lPropMap := AClassDef.ClassMapping.PropMappings.Items[lCtr];
      case lPropMap.PropType of
        ptString, ptAnsiString:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsString[''' + lPropMap.FieldName + '''];', ASL);
        ptBoolean:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsBoolean[''' + lPropMap.FieldName + '''];', ASL);
        ptEnum:
          if Project.EnumType = etInt then
            begin
              lClassProp := AClassDef.ClassProps.FindByName(lPropMap.PropName);
              WriteLine('lObj.' + lPropMap.PropName + ' := ' + lClassProp.PropTypeName + '(Query.FieldAsInteger[''' + lPropMap.FieldName + ''']);', ASL)
            end
          else
            WriteLine('tiSetProperty(lObj, ''' + lPropMap.PropName + ''', Query.FieldAsString[''' + lPropMap.FieldName + '''];', ASL);
        ptDateTime:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsDatetime[''' + lPropMap.FieldName + '''];', ASL);
        ptFloat:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsFloat[''' + lPropMap.FieldName + '''];', ASL);
        ptInt64, ptInteger:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsInteger[''' + lPropMap.FieldName + '''];', ASL);
      end;
    end;
end;

procedure TMapperProjectWriter.WriteMapRowToObjectForList(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lCtr: integer;
  lPropMap: TPropMapping;
  lClassProp: TMapClassProp;
begin
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
    begin
      lPropMap := AClassDef.ClassMapping.PropMappings.Items[lCtr];
      case lPropMap.PropType of
        ptString, ptAnsiString:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsString[''' + lPropMap.FieldName + '''];', ASL);
        ptBoolean:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsBoolean[''' + lPropMap.FieldName + '''];', ASL);
        ptEnum:
          if Project.EnumType = etInt then
            begin
              lClassProp := AClassDef.ClassProps.FindByName(lPropMap.PropName);
              WriteLine('lObj.' + lPropMap.PropName + ' := ' + lClassProp.PropTypeName + '(Query.FieldAsInteger[''' + lPropMap.FieldName + ''']);', ASL)
            end
          else
            WriteLine('tiSetProperty(lObj, ''' + lPropMap.PropName + ''', Query.FieldAsString[''' + lPropMap.FieldName + '''];', ASL);
        ptDateTime:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsDatetime[''' + lPropMap.FieldName + '''];', ASL);
        ptFloat:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsFloat[''' + lPropMap.FieldName + '''];', ASL);
        ptInt64, ptInteger:
          WriteLine('lObj.' + lPropMap.PropName + ' := Query.FieldAsInteger[''' + lPropMap.FieldName + '''];', ASL);
      end;
    end;

end;

procedure TMapperProjectWriter.WriteClassImpSettersGetters(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lCtr: integer;
  lMap: TMapClassProp;
begin
  for lCtr := 0 to AClassDef.ClassProps.Count -1 do
    begin
      lMap := AClassDef.ClassProps.Items[lCtr];
      WriteLine('procedure ' + AClassDef.BaseClassName + '.Set' + lMap.PropName +
        '(const AValue: ' + lMap.PropTypeName + ');', ASL);
      WriteLine('begin', ASL);
      IncTab;
        WriteLine('if F' + lMap.PropName + ' <> AValue then', ASL);
          IncTab;
            WriteLine('F' + lMap.PropName + ' := AValue;', ASL);
          DecTab;
      DecTab;
      WriteLine('end;', ASL);
      WriteBreak(ASL);
    end;
end;

procedure TMapperProjectWriter.WriteProject(const ADirectory: String);
var
  lCtr: Integer;
  lSL: TStringList;
  lUnit: TMapUnitDef;
begin

  BaseDir := ADirectory;

  lSL := TStringList.create;
  try
    for lCtr := 0 to Project.Units.Count - 1 do
      begin
        lSL.Clear;
        lUnit := Project.Units.Items[lCtr];
        WriteUnit(lUnit, lSL);
        lSL.SaveToFile(BaseDir + PathDelim + lUnit.UnitName + '.pas');
      end;
  finally
    lSL.free;
  end;
end;

procedure TMapperProjectWriter.WriteProject(const ADirectory: string;
  ASL: TStringList);
var
  lCtr: Integer;
  lUnit: TMapUnitDef;
begin

  BaseDir := ADirectory;

  for lCtr := 0 to Project.Units.Count - 1 do
    begin
      lUnit := Project.Units.Items[lCtr];
      WriteBreak(ASL);
      WriteLine('// -------------------------------------------------------------', ASL);
      WriteLine('// Unit Definition: ' + lUnit.UnitName, ASL);
      WriteLine('// -------------------------------------------------------------', ASL);
      WriteBreak(ASL);
      WriteUnit(lUnit, ASL);
      ASL.SaveToFile(BaseDir + PathDelim + lUnit.UnitName + '.pas');
    end;

end;

procedure TMapperProjectWriter.WritePropGetter(ASL: TStringList;
  APropDef: TMapClassProp);
begin

end;

procedure TMapperProjectWriter.WritePropPrivateVars(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lCtr: integer;
  lProp: TMapClassProp;
begin
  for lCtr := 0 to AClassDef.ClassProps.Count - 1 do
    begin
      lProp := AClassDef.ClassProps.Items[lCtr];
      WriteLine('F' + lProp.PropName + ': ' + lProp.PropTypeName + ';', ASL);
    end;
end;

procedure TMapperProjectWriter.WritePropSetter(ASL: TStringList;
  APropDef: TMapClassProp);
begin
  WriteLine('procedure Set' + APropDef.PropName + '(const AValue: ' + APropDef.PropTypeName + '); virtual;', ASL);
end;

procedure TMapperProjectWriter.WriteSelectSQL(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin

  lMapping := AClassDef.ClassMapping;

  WriteLine('Query.SQLText := ', ASL);
    IncTab;

      WriteLine('''SELECT '' + ', ASL);

       WriteLine(''' ' + AClassDef.ClassMapping.PKField + ', '' +', ASL);

      //' + lMapping.TableName + ' SET '' +', ASL);
      for lCtr := 0 to lMapping.PropMappings.Count - 1 do
        begin
          lPropMap := lMapping.PropMappings.Items[lCtr];
          if lCtr < (lMapping.PropMappings.Count -1) then
            WriteLine(''' ' + lPropMap.FieldName + ', '' + ', ASL)
          else
            WriteLine(''' ' + lPropMap.FieldName + ' '' + ', ASL)
        end;

      WriteLine('''FROM  ' + lMapping.TableName + ' WHERE OID = :' + lMapping.PKName + ''' ;', ASL);

    DecTab;
end;

procedure TMapperProjectWriter.WriteSetupParams(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lCtr: integer;
begin

  WriteLine('lObj.OID.AssignToTIQuery(''' + AClassDef.ClassMapping.PKName + ''',Query);', ASL);
  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
    begin
      lPropMap := AClassDef.ClassMapping.PropMappings.Items[lCtr];
      case lPropMap.PropType of
        ptString, ptAnsiString:
          WriteLine('Query.ParamAsString[''' + lPropMap.FieldName + '''] := ' +
            'lObj.' + lPropMap.PropName + ';', ASL);
        ptBoolean:
          WriteLine('Query.ParamAsBoolean[''' + lPropMap.FieldName + '''] := ' +
            'lObj.' + lPropMap.PropName + ';', ASL);
        ptEnum:
          if Project.EnumType = etInt then
            WriteLine('Query.ParamAsInteger[''' + lPropMap.FieldName + '''] := Integer(' +
              'lObj.' + lPropMap.PropName + ');', ASL)
          else
            WriteLine('Query.ParamAsString[''' + lPropMap.FieldName + '''] := ' +
            'tiGetProperty(lObj, ' + lPropMap.PropName + ');', ASL);
        ptDateTime:
          WriteLine('Query.ParamAsDateTime[''' + lPropMap.FieldName + '''] := ' +
          'lObj.' + lPropMap.PropName + ';', ASL);
        ptFloat:
          WriteLine('Query.ParamAsFloat[''' + lPropMap.FieldName + '''] := ' +
            'lObj.' + lPropMap.PropName + ';', ASL);
        ptInt64, ptInteger:
          WriteLine('Query.ParamAsInteger[''' + lPropMap.FieldName + '''] := ' +
            'lObj.' + lPropMap.PropName + ';', ASL);
      end;
    end;
end;

procedure TMapperProjectWriter.WriteSingleClassMethods(ASL: TStringList;
  AClassDef: TMapClassDef);
begin

end;

procedure TMapperProjectWriter.WriteSingleClassProp(ASL: TStringList; AClassProp: TMapClassProp);
var
  lTemp: string;
begin
  lTemp := 'property    ' + AClassProp.PropName + ': ' + AClassProp.PropTypeName + ' read F' +
    AClassProp.PropName;

  if not AClassProp.IsReadOnly then
    lTemp := lTemp + ' write Set' + AClassProp.PropName + ';'
  else
    lTemp := lTemp + ';';

  WriteLine(lTemp, ASL);
end;

procedure TMapperProjectWriter.WriteSingleUnitClass(ASL: TStringList;
  AClassDef: TMapClassDef);
begin

  // Event Notification
  if Assigned(FOnWriteClass) then
    FOnWriteClass(AClassDef);

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
      WriteClassIntfReadMethod(ASL, AClassDef);
      WriteClassIntfSaveMethod(ASL, AClassDef);
    DecTab;

  WriteLine('published', ASL);
    IncTab;
      WriteClassProps(ASL, AClassDef);
    DecTab;

  WriteLine('end;', ASL);

end;

procedure TMapperProjectWriter.WriteSingleUnitEnum(ASL: TStringList;
  AEnumDef: TMapEnum);
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

  lTemp := AEnumDef.EnumName + ' = (';

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
          lValues := lVal
        else
          begin
            lValues := lValues + sLineBreak + TabToSpaces(CurrentIndent + 1) + ',' + lVal;
          end;
      end;

    lTemp := lTemp + lValues + ');';

    WriteLine(lTemp + sLineBreak, ASL);

  finally;
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


  ASL.Add(sLineBreak);
  ASL.Add('unit ' + AUnit.UnitName + ';');
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
                WriteVisListCreateIntf(ASL, lClassDef);
                WriteVisListUpdateIntf(ASL, lClassDef);
                WriteVisListDeleteIntf(ASL, lClassDef);

                // Write Out any custom list visitors
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

  WriteAllRegisterAutoMaps(ASL, AUnit);

  if AUnit.UnitClasses.Count > 0 then
    begin

      WriteVisRegImp(ASL, AUnit);

      for lCtr := 0 to AUnit.UnitClasses.Count - 1 do
        begin
          lClassDef := AUnit.UnitClasses.Items[lCtr];
          WriteClassImpSettersGetters(ASL, lClassDef);
          WriteClassImpReadMethod(ASL, lClassDef);
          WriteClassImpSavemethod(ASL, lClassDef);
          WriteListClassImp(ASL, lClassDef);
          WriteClassSelectsImps(ASL, lClassDef);
        end;

      // Write out Visitor implementations
      for lCtr := 0 to AUnit.UnitClasses.Count -1 do
        begin
          lClassDef := AUnit.UnitClasses.Items[lCtr];
          WriteVisClassCreateImp(ASL, lClassDef);
          WriteVisClassUpdateImp(ASL, lClassDef);
          WriteVisClassReadImp(ASL, lClassDef);
          WriteVisClassDeleteImp(ASL, lClassDef);
          WriteVisListReadImp(ASL, lClassDef);
          WriteVisListCreateImp(ASL, lClassDef);
          WriteVisListDeleteImp(ASL, lClassDef);
          WriteVisListUpdateImp(ASL, lClassDef);

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

procedure TMapperProjectWriter.WriteUnitClasses(ASL: TStringList;
  AUnitDef: TMapUnitDef);
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

procedure TMapperProjectWriter.WriteUnitEnums(ASL: TStringList;
  AUnitDef: TMapUnitDef);
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

procedure TMapperProjectWriter.WriteUpdateSQL(ASL: TStringList;
  AClassDef: TMapClassDef);
var
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
  lCtr: integer;
begin

  lMapping := AClassDef.ClassMapping;

  WriteLine('Query.SQLText := ', ASL);
    IncTab;

      WriteLine('''UPDATE ' + lMapping.TableName + ' SET '' +', ASL);
      for lCtr := 0 to lMapping.PropMappings.Count - 1 do
        begin
          lPropMap := lMapping.PropMappings.Items[lCtr];
          if lCtr < (lMapping.PropMappings.Count -1) then
            WriteLine(''' ' + lPropMap.FieldName + ' = :' + lPropMap.FieldName + ', '' + ', ASL)
          else
            WriteLine(''' ' + lPropMap.FieldName + ' = :' + lPropMap.FieldName + ' '' + ', ASL)
        end;

      WriteLine('''WHERE ' + lMapping.PKName + ' = :' + lMapping.PKName + ''' ;', ASL);

    DecTab;


end;

procedure TMapperProjectWriter.WriteVisClassCreateIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
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

procedure TMapperProjectWriter.WriteVisClassCreateImp(ASL: TStringList;
  AClassDef: TMapClassDef);
begin

  WriteLine('{ ' + AClassDef.BaseClassName + '_Create }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + '_Create.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('result := Visited.ObjectState = posCreate;', ASL);
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
    DecTab;
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
      WriteSetupParams(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

end;

procedure TMapperProjectWriter.WriteVisClassDeleteIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
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

procedure TMapperProjectWriter.WriteVisClassDeleteImp(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + '_Delete }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + '_Delete.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('result := Visited.ObjectState = posDelete;', ASL);
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
      WriteLine('lObj.OID.AssignToTIQuery(''' + AClassDef.ClassMapping.PKName + ''',Query);', ASL);
    DecTab;
  WriteLine('end;', ASL);


  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassReadIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
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

procedure TMapperProjectWriter.WriteVisClassReadImp(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + '_Read }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + '_Read.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('result := (Visited.ObjectState = posPK) OR (Visited.ObjectState = posClean);', ASL);
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
      WriteLine('lObj.OID.AssignToTIQuery(''' + AClassDef.ClassMapping.PKName + ''',Query);', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Read.MapRowToObject;', ASL);
  WriteLine('var', ASL);
    IncTab;
      WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
    DecTab;
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
      WriteLine('lObj.OID.AssignFromTIQuery(''' + AClassDef.ClassMapping.PKName + ''',Query);', ASL);
      WriteMapRowToObject(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);


  WriteBreak(ASL);

end;

procedure TMapperProjectWriter.WriteVisClassUpdateIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ Update Visitor for ' + AClassDef.BaseClassName + ' }', ASL);
  WriteLine(AClassDef.BaseClassName + '_Save = class(TtiVisitorUpdate)', ASL);
  WriteLine('protected', ASL);
    IncTab;
      WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
      WriteLine('procedure   Init; override;', ASL);
      WriteLine('procedure   SetupParams; override;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisListCreateImp(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + 'List_Create }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + 'List_Create.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('result := Visited.ObjectState = posCreate;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Create.Init;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteInsertSQL(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Create.SetupParams;', ASL);
  WriteLine('var', ASL);
    IncTab;
      WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
    DecTab;
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
      WriteSetupParams(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisListCreateIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ List Create Visitor for ' + AClassDef.BaseClassName + 'List }', ASL);
  WriteLine(AClassDef.BaseClassName + 'List_Create = class(TtiVisitorUpdate)', ASL);
  WriteLine('protected', ASL);
    IncTab;
      WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
      WriteLine('procedure   Init; override;', ASL);
      WriteLine('procedure   SetupParams; override;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisListDeleteImp(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + 'List_Delete }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + 'List_Delete.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('result := Visited.ObjectState = posDelete;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Delete.Init;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteDeleteSQL(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Delete.SetupParams;', ASL);
  WriteLine('var', ASL);
    IncTab;
      WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
    DecTab;
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
      WriteLine('lObj.OID.AssignToTIQuery(''' + AClassDef.ClassMapping.PKName + ''',Query);', ASL);
    DecTab;
  WriteLine('end;', ASL);
end;

procedure TMapperProjectWriter.WriteVisListDeleteIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ List Delete Visitor for ' + AClassDef.BaseClassName + 'List }', ASL);
  WriteLine(AClassDef.BaseClassName + 'List_Delete = class(TtiVisitorUpdate)', ASL);
  WriteLine('protected', ASL);
    IncTab;
      WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
      WriteLine('procedure   Init; override;', ASL);
      WriteLine('procedure   SetupParams; override;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisClassUpdateImp(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + '_Save }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + '_Save.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('result := Visited.ObjectState = posUpdate;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Save.Init;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteUpdateSQL(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + '_Save.SetupParams;', ASL);
  WriteLine('var', ASL);
    IncTab;
      WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
    DecTab;
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
      WriteSetupParams(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisListReadIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
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

procedure TMapperProjectWriter.WriteVisListUpdateImp(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ ' + AClassDef.BaseClassName + 'List_Save }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + 'List_Save.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('result := Visited.ObjectState = posUpdate;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Save.Init;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteUpdateSQL(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Save.SetupParams;', ASL);
  WriteLine('var', ASL);
    IncTab;
      WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
    DecTab;
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('lObj := ' + AClassDef.BaseClassName + '(Visited);', ASL);
      WriteSetupParams(ASL, AClassDef);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisListUpdateIntf(ASL: TStringList;
  AClassDef: TMapClassDef);
begin
  WriteLine('{ List Update Visitor for ' + AClassDef.BaseClassName + 'List }', ASL);
  WriteLine(AClassDef.BaseClassName + 'List_Save = class(TtiVisitorUpdate)', ASL);
  WriteLine('protected', ASL);
    IncTab;
      WriteLine('function    AcceptVisitor: Boolean; override;', ASL);
      WriteLine('procedure   Init; override;', ASL);
      WriteLine('procedure   SetupParams; override;', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisListReadImp(ASL: TStringList;
  AClassDef: TMapClassDef);
begin

  if not AClassDef.AutoCreateListClass then
    exit;

  WriteLine('{ ' + AClassDef.BaseClassName + 'List_Read }', ASL);

  WriteLine('function ' + AClassDef.BaseClassName + 'List_Read.AcceptVisitor: Boolean;', ASL);
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('result := (Visited.ObjectState = posEmpty);', ASL);
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

          WriteLine('if lFiltered.GetCriteria.hasOrderBy then', ASL);
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
      WriteListSelectSQL(ASL, ACLassDef);
      WriteBreak(ASL);

      WriteLine('Query.SQLText := gFormatSQL(Format(lSQL, [lWhere, lOrder]), ' +
        AClassDef.BaseClassName + ');', ASL);

      WriteBreak(ASL);
    DecTab;

  WriteLine('end;', ASL);
  WriteBreak(ASL);

  WriteLine('procedure ' + AClassDef.BaseClassName + 'List_Read.MapRowToObject;', ASL);
  WriteLine('var', ASL);
    IncTab;
      WriteLine('lObj: ' + AClassDef.BaseClassName + ';', ASL);
    DecTab;
  WriteLine('begin', ASL);
    IncTab;
      WriteLine('lObj := ' + AClassDef.BaseClassName + '.Create;', ASL);
      WriteLine('lObj.OID.AssignFromTIQuery(''' + AClassDef.ClassMapping.PKName + ''',Query);', ASL);
      WriteMapRowToObject(ASL, AClassDef);
      WriteLine('lObj.ObjectState := posClean;', ASL);
      WriteLine('TtiObjectList(Visited).Add(lObj);', ASL);
    DecTab;
  WriteLine('end;', ASL);
  WriteBreak(ASL);
end;

procedure TMapperProjectWriter.WriteVisRegImp(ASL: TStringList;
  AUnitDef: TMapUnitDef);
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

