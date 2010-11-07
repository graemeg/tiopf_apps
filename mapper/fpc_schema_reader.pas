unit fpc_schema_reader;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,mapper
  ,DOM
  ,XMLRead
  ,XMLWrite
  ,tiUtils
  ;

type

  TFPCSchemaXMLReader = class(TMapSchemaReader)
  private
    FProject: TMapProject;
    FXML: TXMLDocument;
    function    CreateSQLSelectList(AClassDef: TMapClassDef): string;
    function    ExtractBaseClassName(const AName: string): string;
    procedure   LoadXMLDoc(const AFile: string);
    procedure   ReadProjectInfo;
    procedure   ReadProjectUnits(AUnitList: TDOMNodeList);
    procedure   ReadUnitClasses(AUnit: TMapUnitDef; ANode: TDOMNode);
    procedure   ReadUnitEnums(AUnit: TMapUnitDef; ANode: TDOMNode);
    procedure   ReadClassProps(AClass: TMapClassDef; ANode: TDOMNodeList);
    procedure   ReadClassMapping(AClass: TMapClassDef; ANode: TDomNodeList);
    procedure   ReadClassSelects(AClass: TMapClassDef; ANode: TDomNode);
    procedure   ReadClassValidators(AClass: TMapClassDef; ANode: TDomNode);
  public
    procedure   ReadSchema(AProject: TMapProject; const AFileName: string = ''); overload; override;
    procedure   WriteAll; override;
    constructor Create;
    destructor  Destroy; override;
  end;

  TFPCSchemaXMLWriter = class(TMapSchemaWriter)
  protected
    FDirectory: String;
  public
    procedure   WriteProject(const ADirectory: String); override;
  end;

implementation

{ TFPCSchemaXMLReader }

constructor TFPCSchemaXMLReader.Create;
begin

end;

function TFPCSchemaXMLReader.CreateSQLSelectList(AClassDef: TMapClassDef
  ): string;
var
  lCtr: integer;
  lPropMap: TPropMapping;
  lMapping: TClassMapping;
begin
  result := AClassDef.ClassMapping.TableName + '.OID ';

  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
    begin
      lPropMap := AClassDef.ClassMapping.PropMappings.Items[lCtr];
      result := result + ', ' + AClassDef.ClassMapping.TableName + '.' + lPropMap.FieldName;
    end;

  result := UpperCase(result);
end;

destructor TFPCSchemaXMLReader.Destroy;
begin
  if FXML <>nil then
    FXML.Free;

  inherited Destroy;
end;

function TFPCSchemaXMLReader.ExtractBaseClassName(const AName: string): string;
begin
  if AnsiPos('T', AName) > 0 then
    result := Copy(AName, 2, Length(AName) -1)
  else
    result := AName;
end;

procedure TFPCSchemaXMLReader.LoadXMLDoc(const AFile: string);
begin
  ReadXMLFile(FXML, AFile);
end;

procedure TFPCSchemaXMLReader.ReadClassMapping(AClass: TMapClassDef;
  ANode: TDomNodeList);
var
  lCtr: integer;
  lNode: TDomNode;
  lMapNode: TDomNode;
  lNewMapProp: TPropMapping;
begin
  for lCtr := 0 to ANode.Length - 1 do
    begin
      lNode := ANode.Item[lCtr];
      if lNode.NodeType <> COMMENT_NODE then
        begin
          lNewMapProp := TPropMapping.create;
          lNewMapProp.FieldName := lNode.Attributes.GetNamedItem('field').NodeValue;
          lNewMapProp.PropName := lNode.Attributes.GetNamedItem('prop').NodeValue;

          lMapNode := lNode.Attributes.GetNamedItem('type');
          if lMapNode = nil then
            begin
              lNewMapProp.PropType := gStrToPropType('string');
            end
          else
            begin
              lNewMapProp.PropType := gStrToPropType(lNode.Attributes.GetNamedItem('type').NodeValue);
            end;

          AClass.ClassMapping.PropMappings.Add(lNewMapProp);
        end;
    end;
end;

procedure TFPCSchemaXMLReader.ReadClassProps(AClass: TMapClassDef; ANode: TDOMNodeList);
var
  lCtr: Integer;
  lPropNode: TDomNode;
  lPropAttr: TDomNode;
  lNewProp: TMapClassProp;
begin
  for lCtr := 0 to ANode.Length - 1 do
    begin
      lPropNode := ANode.Item[lCtr];
      if lPropNode.NodeType <> COMMENT_NODE then
        begin
          lNewProp := TMapClassProp.create;

          lNewProp.PropName := lPropNode.Attributes.GetNamedItem('name').NodeValue;

          // Read only?
          lPropAttr := lPropNode.Attributes.GetNamedItem('read-only');
          if lPropAttr <> nil then
            lNewProp.IsReadOnly := StrToBool(lPropAttr.NodeValue);

          // Property type?
          lPropAttr := lPropNode.Attributes.GetNamedItem('type');
          if lPropAttr <> nil then
            begin
              if lPropAttr.NodeValue <> '' then
                begin
                  if Copy(lPropAttr.NodeValue, 1,1) = 'T' then
                    lNewProp.PropType := ptEnum
                  else
                    lNewProp.PropType := gStrToPropType(lPropAttr.NodeValue);
                  lNewProp.PropTypeName := lPropAttr.NodeValue
                end
              else
                begin
                  lNewProp.PropType := ptString;
                  lNewProp.PropTypeName := 'String';
                end;
            end
          else
            begin
              lNewProp.PropTypeName := 'string';
              lNewProp.PropType := ptString;
            end;

          AClass.ClassProps.Add(lNewProp);
        end;
    end;
end;

procedure TFPCSchemaXMLReader.ReadClassSelects(AClass: TMapClassDef;
  ANode: TDomNode);
var
  lSelectList: TDomNodeList;
begin
  lSelectList := ANode.FindNode('enums').ChildNodes;


end;

procedure TFPCSchemaXMLReader.ReadClassValidators(AClass: TMapClassDef;
  ANode: TDomNode);
var
  lCtr: Integer;
  lVal: TMapValidator;
  lValNode: TDomNode;
  lValueNode: TDomNode;
  lProp: TMapClassProp;
  lValStr: string;
  lTempStr: string;
  lType: TMapPropType;
begin
  if not ANode.HasChildNodes then
    exit;

  for lCtr := 0 to ANode.ChildNodes.Length - 1 do
    begin
      lValNode := ANode.ChildNodes.Item[lCtr];
      if lValNode.NodeType <> COMMENT_NODE then
        begin
          lVal := TMapValidator.Create;
          lVal.ValidatorType := gStrToValType(lValNode.Attributes.GetNamedItem('type').NodeValue);
          lVal.ClassProp := lValNode.Attributes.GetNamedItem('prop').NodeValue;
          lTempStr := lVal.ClassProp;
          if lVal.ValidatorType <> vtRequired then
            begin
              lProp := TMapClassProp(AClass.ClassProps.FindByProps(['PropName'], [lVal.ClassProp]));
              lTempStr := lProp.PropName;
              lType := lProp.PropType;
              if lProp = nil then
                raise Exception.Create('No register property in class "' + AClass.BaseClassName + '" found with name ' +
                  lVal.ClassProp);

                lValueNode := lValNode.ChildNodes.Item[0];
                if lValueNode <> nil then
                  begin
                    lValStr := lValNode.ChildNodes.Item[0].TextContent;

                    case lProp.PropType of
                      ptAnsiString, ptString:
                        lVal.Value := lValStr;
                      ptBoolean:
                        lVal.Value := StrtoBool(lValStr);
                      ptInt64, ptInteger:
                        lVal.Value := StrToInt(lValStr);
                      ptDateTime:
                        lVal.Value := tiIntlDateStorAsDateTime(lValStr);
                      ptEnum:;
                      ptFloat:
                        lVal.Value := StrToFloat(lValStr);
                    end;
                  end;

            end;

          AClass.Validators.Add(lVal);
        end;
    end;

end;

procedure TFPCSchemaXMLReader.ReadProjectInfo;
begin

end;

procedure TFPCSchemaXMLReader.ReadProjectUnits(AUnitList: TDOMNodeList);
var
  lUnitsList: TDomNodeList;
  lCtr: Integer;
  lNewUnit: TMapUnitDef;
  lRefNodeList, lRefNode: TDomNode;
  lRefCtr: integer;
  lUnitNode: TDomNode;
begin

  if AUnitList = nil then exit;

  for lCtr := 0 to AUnitList.Length - 1 do
    begin
      lUnitNode := AUnitList.Item[lCtr];
      if lUnitNode.NodeType <> COMMENT_NODE then
        begin
          lNewUnit := TMapUnitDef.Create;
          lNewUnit.UnitName := lUnitNode.Attributes.GetNamedItem('name').NodeValue;
          ReadUnitEnums(lNewUnit, lUnitNode.FindNode('enums'));
          ReadUnitClasses(lNewUnit, lUnitNode.FindNode('classes'));

          // Reference (uses)
          lRefNodeList := lUnitNode.FindNode('references');
          if (lRefNodeList <> nil) and (lRefNodeList.HasChildNodes) then
            begin
              for lRefCtr := 0 to lRefNodeList.ChildNodes.Length - 1 do
                begin
                  lRefNode := lRefNodeList.ChildNodes.Item[lRefCtr];
                  lNewUnit.References.Add(lRefNode.Attributes.GetNamedItem('name').NodeValue);
                end;
            end;
          FProject.Units.Add(lNewUnit);
        end;
    end;
end;

procedure TFPCSchemaXMLReader.ReadSchema(AProject: TMapProject; const AFileName: string);
var
  lNode: TDOMNode;
  lNodeList: TDOMNodeList;
  lAttr: TDOMNode;
  lIncNode: TDOMNode;
  lCtr: Integer;
  lEnumTypeStr: string;
  lUnitList: TDOMNodeList;
  lIncProjDoc: TXMLDocument;
  lIncPath: string;
  lDirNode: TDomNode;
  lPath: string;
begin
  FProject := AProject;
  LoadXMLDoc(AFileName);

  lNode := FXML.DocumentElement;
  FProject.ProjectName := lNode.Attributes.GetNamedItem('project-name').NodeValue;

  // Establish the base directory
  lDirNode := lNode.Attributes.GetNamedItem('base-directory');
  if lDirNode <> nil then
    begin
      if lDirNode.NodeValue <> '' then
        FProject.BaseDirectory := lNode.Attributes.GetNamedItem('base-directory').NodeValue
      else
        FProject.BaseDirectory := ExtractFileDir(AFileName);
    end
  else
    begin
      FProject.BaseDirectory := ExtractFileDir(AFileName);
    end;

  // Establish the Output directory, if present.
  lDirNode := lNode.Attributes.GetNamedItem('outputdir');
  if lDirNode <> nil then
    begin
      if lDirNode.NodeValue <> '' then
        begin
          lPath := FProject.BaseDirectory;
          lPath := GetabsolutePath(FProject.BaseDirectory, lNode.Attributes.GetNamedItem('outputdir').NodeValue);
          FProject.OutputDirectory := lPath;
        end
      else
        FProject.OutputDirectory := FProject.BaseDirectory;
    end
  else
    begin
      FProject.OutputDirectory := FProject.BaseDirectory;
    end;


  lIncPath := FProject.OutputDirectory;

  lAttr := lNode.Attributes.GetNamedItem('tab-spaces');
  if lAttr <> nil then
    FProject.TabSpaces := StrToInt(lAttr.NodeValue)
  else
    FProject.TabSpaces := 2;

  lAttr := lNode.Attributes.GetNamedItem('begin-end-tabs');
  if lAttr <> nil then
    FProject.BeginEndTabs := StrtoInt(lAttr.NodeValue)
  else
    FProject.BeginEndTabs := 1;

  lAttr := lNode.Attributes.GetNamedItem('visibility-tabs');
  if lAttr <> nil then
    FProject.VisibilityTabs := StrtoInt(lAttr.NodeValue)
  else
    FProject.VisibilityTabs := 1;

  lAttr := lNode.Attributes.GetNamedItem('enum-type');
  if lAttr <> nil then
    begin
      lEnumTypeStr := lAttr.NodeValue;
      if lowercase(lEnumTypeStr) = 'string' then
        FProject.EnumType := etString
      else
        FProject.EnumType := etInt;
    end
  else
    begin
      FProject.EnumType := etInt;
    end;

  // Process Includes
  lNodeList := lNode.FindNode('includes').ChildNodes;

  if lNodeList <> nil then
    begin
      for lCtr := 0 to lNodeList.Length - 1 do
        begin
          lIncNode := lNodeList.Item[lCtr];
          if lIncNode.NodeType <> COMMENT_NODE then
            begin
              lIncPath := lIncNode.Attributes.GetNamedItem('file-name').NodeValue;
              FProject.Includes.Add(lIncPath);
              ReadXMLFile(lIncProjDoc, FProject.BaseDirectory + PathDelim + lIncPath);
              try
                lUnitList := lIncProjDoc.DocumentElement.FindNode('project-units').ChildNodes;
              finally;
                ReadProjectUnits(lUnitList);
                if lIncProjDoc <> nil then
                  lIncProjDoc.Free;
              end;
            end;
        end;
    end;



  lUnitList := FXML.DocumentElement.FindNode('project-units').ChildNodes;
  ReadProjectUnits(lUnitList);


end;

procedure TFPCSchemaXMLReader.ReadUnitClasses(AUnit: TMapUnitDef; ANode: TDOMNode);
var
  lCtr, lSelectCtr, lParamsCtr: integer;
  lClassNode: TDomNode;
  lClassListNodes: TDomNodeList;
  lClassMappings: TDOMNodeList;
  lClassMapNode: TDomNode;
  lClassProps: TDOMNodeList;
  lClassSelects: TDOMNodeList;
  lNewClass: TMapClassDef;
  lClassAttr: TDomNode;
  lSelListNode: TDOMNode;
  lSelectNode: TDOMNode;
  lParamListNode: TDOMNode;
  lParam: TDOMNode;
  lNewParam: TSelectParam;
  lNewSelect: TClassMappingSelect;
  lTemp: string;
  lValNode: TDOMNode;
begin

  lClassListNodes := ANode.ChildNodes;
  for lCtr := 0 to lClassListNodes.Length - 1 do
    begin
      lClassNode := lClassListNodes.Item[lCtr];
      if lClassNode <> nil then
        begin
          if lClassNode.NodeType <> COMMENT_NODE then
            begin
            lNewClass := TMapClassDef.Create;

            lClassAttr := lClassNode.Attributes.GetNamedItem('def-type');
            if lClassAttr <>nil then
              lNewClass.DefType := gStrToClassDefType(lClassAttr.NodeValue)
            else
              lNewClass.DefType := dtReference;

            lClassAttr := lClassNode.Attributes.GetNamedItem('base-unit');
            if lClassAttr <> nil then
              lNewClass.BaseUnitName := lClassAttr.NodeValue;

            lClassAttr := lClassNode.Attributes.GetNamedItem('base-class');
            if lClassAttr <> nil then
              lNewClass.BaseClassName := lClassAttr.NodeValue
            else
              lNewClass.BaseClassName := 'TtiObject';

            lClassAttr := lClassNode.Attributes.GetNamedItem('base-class-parent');
            if lClassAttr <> nil then
              lNewClass.BaseClassParent := lClassAttr.NodeValue
            else
              lNewClass.BaseClassParent := 'TtiObject';

            lClassAttr := lClassNode.Attributes.GetNamedItem('auto-map');
            if lClassAttr <> nil then
              lNewClass.AutoMap := StrToBool(lClassAttr.NodeValue);

            lClassAttr := lClassNode.Attributes.GetNamedItem('auto-create-base');
            if lClassAttr <> nil then
              lNewClass.AutoCreateBase := StrToBool(lClassAttr.NodeValue);

            lClassAttr := lClassNode.Attributes.GetNamedItem('crud');
            if lClassAttr <> nil then
              lNewClass.Crud := lClassAttr.NodeValue
            else
              lNewClass.Crud := 'all';

            lClassAttr := lClassNode.Attributes.GetNamedItem('auto-create-list');
            if lClassAttr <> nil then
              lNewClass.AutoCreateListClass := StrToBool(lClassAttr.NodeValue)
            else
              lNewClass.AutoCreateListClass := true;

            lClassAttr := lClassNode.Attributes.GetNamedItem('oid-type');
            if lClassAttr <> nil then
              begin
                if lClassAttr.NodeValue = 'guid' then
                  lNewClass.ClassMapping.OIDType := otString
                else
                  lNewClass.ClassMapping.OIDType := otInt;
              end;

            if lClassNode.FindNode('class-props') = nil then
              raise Exception.Create(ClassName + '.ReadUnitClasses: Class PrNode is not present.');

            lClassProps := lClassNode.FindNode('class-props').ChildNodes;
            if lClassProps <> nil then
              ReadClassProps(lNewClass, lClassProps);

            lClassMapNode := lClassNode.FindNode('mapping');
            if lClassMapNode <> nil then
              begin
                lNewClass.ClassMapping.PKName := lClassMapNode.Attributes.GetNamedItem('pk').NodeValue;
                lNewClass.ClassMapping.TableName := lClassMapNode.Attributes.GetNamedItem('table').NodeValue;
                lNewClass.ClassMapping.PKField := lClassMapNode.Attributes.GetNamedItem('pk-field').NodeValue;
                lClassMappings := lClassMapNode.ChildNodes;

                if lClassMappings <> nil then
                  ReadClassMapping(lNewClass, lClassMappings);
              end;

            lValNode := lClassNode.FindNode('validators');

            if lValNode <> nil then
              ReadClassValidators(lNewClass, lValNode);

            // Read in any selections
            lSelListNode := lClassNode.FindNode('selections');
            if lSelListNode <> nil then
              begin
                if lSelListNode.HasChildNodes then
                  begin
                    for lSelectCtr := 0 to lSelListNode.ChildNodes.Length - 1 do
                      begin
                        lSelectNode := lSelListNode.ChildNodes.Item[lSelectCtr];
                        if lSelectNode.NodeType <> COMMENT_NODE then
                          begin
                            lNewSelect := TClassMappingSelect.Create;
                            lTemp := StringReplace(lSelectNode.FindNode('sql').ChildNodes.Item[0].NodeValue, #13, '', [rfReplaceAll]);
                            lTemp := StringReplace(lTemp, #10, '', [rfReplaceAll]);
                            lTemp := tiNormalizeStr(lTemp);
                            // Change variable ${field_list} into list of field names in sql format
                            if POS('${field_list}', lTemp) > 0 then
                              lTemp := StringReplace(lTemp, '${field_list}', CreateSQLSelectList(lNewClass), [rfReplaceAll]);
                            lNewSelect.SQL.Text := lTemp;
                            lNewSelect.Name := lSelectNode.Attributes.GetNamedItem('name').NodeValue;
                            lParamListNode := lSelectNode.FindNode('params');
                            if (lParamListNode <> nil) and (lParamListNode.HasChildNodes) then
                              begin
                                for lParamsCtr := 0 to lParamListNode.ChildNodes.Length - 1 do
                                  begin
                                    lParam := lParamListNode.ChildNodes.Item[lParamsCtr];
                                    if lParam.NodeType <> COMMENT_NODE then
                                      begin
                                        lNewParam := TSelectParam.Create;
                                        lNewParam.ParamName := lParam.Attributes.GetNamedItem('name').NodeValue;
                                        lNewParam.ParamType := gStrToPropType(lParam.Attributes.GetNamedItem('type').NodeValue);
                                        lNewParam.ParamTypeName := lParam.Attributes.GetNamedItem('type').NodeValue;
                                        if lNewParam.ParamType = ptEnum then
                                          lNewParam.TypeName := lParam.Attributes.GetNamedItem('type-name').NodeValue;
                                        lNewParam.PassBy := lParam.Attributes.GetNamedItem('pass-by').NodeValue;
                                        lNewParam.SQLParamName := lParam.Attributes.GetNamedItem('sql-param').NodeValue;
                                        lNewSelect.Params.Add(lNewParam);
                                      end;
                                  end;
                              end;
                            // finally, add to list.
                            lNewClass.Selections.Add(lNewSelect);
                          end;
                      end;
                  end;
              end;

            // Add to unit classes
            AUnit.UnitClasses.Add(lNewClass);
            // Addreference to ProjectClasses
            FProject.ProjectClasses.Add(lNewClass);
          end;

        end;
    end;
end;

procedure TFPCSchemaXMLReader.ReadUnitEnums(AUnit: TMapUnitDef; ANode: TDOMNode);
var
  lEnumList: TDOMNodeList;
  lEnumValuesList: TDOMNodeList;
  lEnumValueNode: TDomNode;
  lEnum: TDOMNode;
  lAttr: TDomNode;
  lCtr: Integer;
  lValueCtr: integer;
  lNewEnum: TMapEnum;
  lNewEnumValue: TMapEnumValue;
begin

  if (ANode = nil) or (not ANode.HasChildNodes) then
    exit;

  lEnumList := ANode.ChildNodes;

  for lCtr := 0 to lEnumList.Length - 1 do
    begin
      lEnum := lEnumList.Item[lCtr];
      if lEnum.NodeType <> COMMENT_NODE then
        begin
          // Crate the Enum Class Def.
          lNewEnum := TMapEnum.Create;
          lNewEnum.EnumName := lEnum.Attributes.GetNamedItem('name').NodeValue;

          // Retrieve its values
          lEnumValuesList := lEnum.FindNode('values').ChildNodes;


          if lEnumValuesList <> nil then
            begin
              for lValueCtr := 0 to lEnumValuesList.Length - 1 do
                begin
                  lEnumValueNode := lEnumValuesList.Item[lValueCtr];
                  lNewEnumValue := TMapEnumValue.Create;
                  lNewEnumValue.EnumValueName := lEnumValueNode.Attributes.GetNamedItem('name').NodeValue;

                  lAttr := lEnumValueNode.Attributes.GetNamedItem('value');
                  if lAttr <> nil then
                    lNewEnumValue.EnumValue := StrtoInt(lAttr.NodeValue);

                  lNewEnum.Values.Add(lNewEnumValue);
                end;
            end;

          // Add it to the unit def.
          AUnit.UnitEnums.Add(lNewEnum);
          // Add reference to ProjectEnums
          FProject.ProjectEnums.Add(lNewEnum);
      end;
    end;
end;

procedure TFPCSchemaXMLReader.WriteAll;
begin

end;

{ TFPCSchemaXMLWriter }

procedure TFPCSchemaXMLWriter.WriteProject(const ADirectory: String);
begin

end;

initialization
  gSetSchemaReaderClass(TFPCSchemaXMLReader);


end.

