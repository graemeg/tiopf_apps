unit delphi_schema_reader;

interface
uses
  Classes
  ,SysUtils
  ,tiUtils
  ,mapper
  ,OmniXML
  ,OmniXMLUtils
  ;

type

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: OmniXML version of TMapSchemaReader. }
  TOmniXMLSchemaReader = class(TMapSchemaReader)
  private
    FProject: TMapProject;
    FXML: IXMLDocument;
    function    CreateSQLSelectList(AClassDef: TMapClassDef): string;
    function    ExtractBaseClassName(const AName: string): string;
    procedure   LoadXMLDoc(const AFile: string);
    procedure   ReadProjectUnits(AUnitList: IXMLNodeList);
    procedure   ReadUnitClasses(AUnit: TMapUnitDef; ANode: IXMLNode);
    procedure   ReadUnitEnums(AUnit: TMapUnitDef; ANode: IXMLNode);
    procedure   ReadClassProps(AClass: TMapClassDef; ANode: IXMLNodeList);
    procedure   ReadClassMapping(AClass: TMapClassDef; ANode: IXMLNodeList);
    procedure   ReadClassSelects(AClass: TMapClassDef; ANode: IXMLNode);
    procedure   ReadClassValidators(AClass: TMapClassDef; ANode: IXMLNode);
  public
    procedure   ReadSchema(AProject: TMapProject; const AFileName: string = ''); overload; override;
    constructor Create;
    destructor  Destroy; override;
  end;

implementation

{ TOmniXMLSchemaReader }

constructor TOmniXMLSchemaReader.Create;
begin
  inherited Create;
end;

function TOmniXMLSchemaReader.CreateSQLSelectList(
  AClassDef: TMapClassDef): string;
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

destructor TOmniXMLSchemaReader.Destroy;
begin
  FXML := nil;
  inherited;
end;

function TOmniXMLSchemaReader.ExtractBaseClassName(const AName: string): string;
begin
  if AnsiPos('T', AName) > 0 then
    result := Copy(AName, 2, Length(AName) -1)
  else
    result := AName;
end;

procedure TOmniXMLSchemaReader.LoadXMLDoc(const AFile: string);
begin
  FXML := nil;

  FXML := CreateXMLDoc;
  XMLLoadFromFile(FXML, AFile);

end;

procedure TOmniXMLSchemaReader.ReadClassMapping(AClass: TMapClassDef;
  ANode: IXMLNodeList);
var
  lCtr: integer;
  lNode: IXMLNode;
  lMapNode: IXMLNode;
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

procedure TOmniXMLSchemaReader.ReadClassProps(AClass: TMapClassDef;
  ANode: IXMLNodeList);
var
  lCtr: Integer;
  lPropNode: IXMLNode;
  lPropAttr: IXMLNode;
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

procedure TOmniXMLSchemaReader.ReadClassSelects(AClass: TMapClassDef;
  ANode: IXMLNode);
begin

end;

procedure TOmniXMLSchemaReader.ReadClassValidators(AClass: TMapClassDef;
  ANode: IXMLNode);
var
  lCtr: Integer;
  lVal: TMapValidator;
  lValNode: IXMLNode;
  lValueNode: IXMLNode;
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
                    lValStr := lValNode.ChildNodes.Item[0].Text;

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

procedure TOmniXMLSchemaReader.ReadProjectUnits(AUnitList: IXMLNodeList);
var
  lUnitsList: IXMLNodeList;
  lCtr: Integer;
  lNewUnit: TMapUnitDef;
  lRefNodeList, lRefNode: IXMLNode;
  lRefCtr: integer;
  lUnitNode: IXMLNode;
begin

  if AUnitList = nil then exit;

  for lCtr := 0 to AUnitList.Length - 1 do
    begin
      lUnitNode := AUnitList.Item[lCtr];
      if lUnitNode.NodeType <> COMMENT_NODE then
        begin
          lNewUnit := TMapUnitDef.Create;
          lNewUnit.UnitName := lUnitNode.Attributes.GetNamedItem('name').NodeValue;
          ReadUnitEnums(lNewUnit, lUnitNode.SelectSingleNode('enums'));
          ReadUnitClasses(lNewUnit, lUnitNode.SelectSingleNode('classes'));

          // Reference (uses)
          lRefNodeList := lUnitNode.SelectSingleNode('references');
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

procedure TOmniXMLSchemaReader.ReadSchema(AProject: TMapProject;
  const AFileName: string);
var
  lNode: IXMLNode;
  lNodeList: IXMLNodeList;
  lAttr: IXMLNode;
  lIncNode: IXMLNode;
  lCtr: Integer;
  lEnumTypeStr: string;
  lUnitList: IXMLNodeList;
  lIncProjDoc: IXMLDocument;
  lIncPath: string;
  lDirNode: IXMLNode;
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
  lNodeList := lNode.SelectSingleNode('includes').ChildNodes;

  if lNodeList <> nil then
    begin
      for lCtr := 0 to lNodeList.Length - 1 do
        begin
          lIncNode := lNodeList.Item[lCtr];
          if lIncNode.NodeType <> COMMENT_NODE then
            begin
              lIncPath := lIncNode.Attributes.GetNamedItem('file-name').NodeValue;
              FProject.Includes.Add(lIncPath);
              lIncProjDoc := CreateXMLDoc;
              XMLLoadFromFile(lIncProjDoc, FProject.BaseDirectory + PathDelim + lIncPath);
              try
                lUnitList := lIncProjDoc.DocumentElement.SelectSingleNode('project-units').ChildNodes;
              finally;
                ReadProjectUnits(lUnitList);
                if lIncProjDoc <> nil then
                  lIncProjDoc := nil;
              end;
            end;
        end;
    end;



  lUnitList := FXML.DocumentElement.SelectSingleNode('project-units').ChildNodes;
  ReadProjectUnits(lUnitList);


end;

procedure TOmniXMLSchemaReader.ReadUnitClasses(AUnit: TMapUnitDef;
  ANode: IXMLNode);
var
  lCtr, lSelectCtr, lParamsCtr: integer;
  lClassNode: IXMLNode;
  lClassListNodes: IXMLNodeList;
  lClassMappings: IXMLNodeList;
  lClassMapNode: IXMLNode;
  lClassProps: IXMLNodeList;
  lClassSelects: IXMLNodeList;
  lNewClass: TMapClassDef;
  lClassAttr: IXMLNode;
  lSelListNode: IXMLNode;
  lSelectNode: IXMLNode;
  lParamListNode: IXMLNode;
  lParam: IXMLNode;
  lNewParam: TSelectParam;
  lNewSelect: TClassMappingSelect;
  lTemp: string;
  lValNode: IXMLNode;
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

            if lClassNode.SelectSingleNode('class-props') = nil then
              raise Exception.Create(ClassName + '.ReadUnitClasses: Class PrNode is not present.');

            lClassProps := lClassNode.SelectSingleNode('class-props').ChildNodes;
            if lClassProps <> nil then
              ReadClassProps(lNewClass, lClassProps);

            lClassMapNode := lClassNode.SelectSingleNode('mapping');
            if lClassMapNode <> nil then
              begin
                lNewClass.ClassMapping.PKName := lClassMapNode.Attributes.GetNamedItem('pk').NodeValue;
                lNewClass.ClassMapping.TableName := lClassMapNode.Attributes.GetNamedItem('table').NodeValue;
                lNewClass.ClassMapping.PKField := lClassMapNode.Attributes.GetNamedItem('pk-field').NodeValue;
                lClassMappings := lClassMapNode.ChildNodes;

                if lClassMappings <> nil then
                  ReadClassMapping(lNewClass, lClassMappings);
              end;

            lValNode := lClassNode.SelectSingleNode('validators');

            if lValNode <> nil then
              ReadClassValidators(lNewClass, lValNode);

            // Read in any selections
            lSelListNode := lClassNode.SelectSingleNode('selections');
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
                            lTemp := StringReplace(lSelectNode.SelectSingleNode('sql').ChildNodes.Item[0].NodeValue, #13, '', [rfReplaceAll]);
                            lTemp := StringReplace(lTemp, #10, '', [rfReplaceAll]);
                            lTemp := tiNormalizeStr(lTemp);
                            // Change variable ${field_list} into list of field names in sql format
                            if POS('${field_list}', lTemp) > 0 then
                              lTemp := StringReplace(lTemp, '${field_list}', CreateSQLSelectList(lNewClass), [rfReplaceAll]);
                            lNewSelect.SQL.Text := lTemp;
                            lNewSelect.Name := lSelectNode.Attributes.GetNamedItem('name').NodeValue;
                            lParamListNode := lSelectNode.SelectSingleNode('params');
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

procedure TOmniXMLSchemaReader.ReadUnitEnums(AUnit: TMapUnitDef;
  ANode: IXMLNode);
var
  lEnumList: IXMLNodeList;
  lEnumValuesList: IXMLNodeList;
  lEnumValueNode: IXMLNode;
  lEnum: IXMLNode;
  lAttr: IXMLNode;
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
          lEnumValuesList := lEnum.SelectSingleNode('values').ChildNodes;


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

end.
