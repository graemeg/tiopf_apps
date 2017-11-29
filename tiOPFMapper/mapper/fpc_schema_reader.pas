unit fpc_schema_reader;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,variants
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
    procedure   ReadClassValidators(AClass: TMapClassDef; ANode: TDOMNode);
  public
    procedure   ReadSchema(AProject: TMapProject; const AFileName: string = ''); overload; override;
    procedure   WriteAll; override;
    constructor Create; override;
    destructor  Destroy; override;
  end;


  TProjectWriter = class(TBaseMapObject)
  protected
    FDirectory: String;
    FWriterProject: TMapProject;
    FDoc: TXMLDocument;
    procedure   WriteProjectUnits(AProject: TMapProject; ADocElem: TDOMElement);
    procedure   WriteUnit(AUnitDef: TMapUnitDef; AUnitNode: TDOMElement);
    procedure   WriteUnitEnums(AUnitDef: TMapUnitDef; AUnitNode: TDOMElement);
    procedure   WriteUnitClasses(AUnitDef: TMapUnitDef; AClassesNode: TDOMElement);
    procedure   WriteSingleUnitClass(AClassDef: TMapClassDef; AClassesNode: TDOMElement);
    procedure   WriteClassProps(AClassDef: TMapClassDef; AClassNode: TDOMElement);
    procedure   WriteClassValidators(AClassDef: TMapClassDef; AClassNode: TDOMElement);
    procedure   WriteClassMappings(AClassDef: TMapClassDef; AClassNode: TDOMElement);
    procedure   WriteClassSelections(AClassDef: TMapClassDef; AClassNode: TDOMElement);

  public
    procedure   WriteProject(AProject: TMapProject; const ADirectory: String; const AFileName: string); overload; virtual;
    procedure   WriteProject(Aproject: TMapProject; const AFilePath: string); overload; virtual;
    destructor  Destroy; override;
  end;

implementation

{ TFPCSchemaXMLReader }

constructor TFPCSchemaXMLReader.Create;
begin
  inherited Create;
end;

function TFPCSchemaXMLReader.CreateSQLSelectList(AClassDef: TMapClassDef): string;
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

procedure TFPCSchemaXMLReader.ReadClassMapping(AClass: TMapClassDef; ANode: TDomNodeList);
var
  lCtr: integer;
  lNode: TDomNode;
  lMapNode: TDomNode;
  lMapPropNode: TDOMNode;
  lNewMapProp: TPropMapping;
  lLastGood: string;
  lAbstractValue: Boolean;
  s: string;
begin
  for lCtr := 0 to ANode.Length - 1 do
    begin
      lNode := ANode.Item[lCtr];
      if lNode.NodeType = ELEMENT_NODE then
        begin
          lMapPropNOde := lNode.Attributes.GetNamedItem('field');
          if lMapPropNode = nil then
            begin
              //WriteLn('Error Node Type: ' + IntToStr(lNode.NodeType));
              raise Exception.Create(ClassName + '.ReadClassMapping: Mapping node Attribute "field" not found ' +
                'reading schema for ' + AClass.BaseClassName);
            end;

          lNewMapProp := TPropMapping.create;
          lNewMapProp.FieldName := lNode.Attributes.GetNamedItem('field').NodeValue;
          lNewMapProp.PropName := lNode.Attributes.GetNamedItem('prop').NodeValue;
          lMapPropNode := lNode.Attributes.GetNamedItem('getter');
          if Assigned(lMapPropNode) then
            lNewMapProp.PropertyGetter := lMapPropNode.NodeValue;
          lMapPropNode := lNode.Attributes.GetNamedItem('setter');
          if Assigned(lMapPropNode) then
            lNewMapProp.PropertySetter := lMapPropNode.NodeValue;
          lMapPropNode := lNode.Attributes.GetNamedItem('abstract');
          if Assigned(lMapPropNode) then
          begin
            s := LowerCase(lMapPropNode.NodeValue);
            if (s = 'false') or (s = '0') or (s = 'no') then
              lAbstractValue := False
            else
              lAbstractValue := True;
          end
          else
            lAbstractValue := True;
          lNewMapProp.PropertyAccessorsAreAbstract := lAbstractValue;

          lLastGood := lNewMapProp.PropName;

          lMapNode := lNode.Attributes.GetNamedItem('type');
          if lMapNode = nil then
            lNewMapProp.PropertyType := gStrToPropType('string')
          else
            lNewMapProp.PropertyType := gStrToPropType(lMapNode.NodeValue);

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

      lNewProp.Name := lPropNode.Attributes.GetNamedItem('name').NodeValue;

      // Read only?
      lPropAttr := lPropNode.Attributes.GetNamedItem('read-only');
      if lPropAttr <> nil then
        lNewProp.IsReadOnly := StrToBool(lPropAttr.NodeValue)
      else
        lNewProp.IsReadOnly := false;

      // virtual getter?
      lPropAttr := lPropNode.Attributes.GetNamedItem('virtual');
      if lPropAttr <> nil then
        lNewProp.VirtualGetter := StrToBool(lPropAttr.NodeValue)
      else
        lNewProp.VirtualGetter := false;

      // Property type?
      lPropAttr := lPropNode.Attributes.GetNamedItem('type');
      if lPropAttr <> nil then
      begin
        if lPropAttr.NodeValue <> '' then
        begin
          if (Copy(lPropAttr.NodeValue, 1,1) = 'T') and (LowerCase(lPropAttr.NodeValue) <> 'tdatetime') then
            lNewProp.PropertyType := ptEnum
          else
            lNewProp.PropertyType := gStrToPropType(lPropAttr.NodeValue);
          lNewProp.PropTypeName := lPropAttr.NodeValue
        end
        else
        begin
          lNewProp.PropTypeName := 'String';
          lNewProp.PropertyType := ptString;
        end;
      end
      else
      begin
        lNewProp.PropTypeName := 'String';
        lNewProp.PropertyType := ptString;
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

procedure TFPCSchemaXMLReader.ReadClassValidators(AClass: TMapClassDef; ANode: TDOMNode);
var
  lCtr: Integer;
  lVal: TMapValidator;
  lValNode: TDomNode;
  lValueNode: TDomNode;
  lTypeNode: TDOMNode;
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
          // Get validator type.  "required" is the default.
          lTypeNode := lValNode.Attributes.GetNamedItem('type');
          if lTypeNode <> nil then
            lVal.ValidatorType := gStrToValType(lTypeNode.NodeValue)
          else
            lVal.ValidatorType := vtRequired;

          lVal.ClassProp := lValNode.Attributes.GetNamedItem('prop').NodeValue;
          lTempStr := lVal.ClassProp;
          if lVal.ValidatorType <> vtRequired then
            begin
              lProp := TMapClassProp(AClass.ClassProps.FindByName(lVal.ClassProp));
              if lProp = nil then
                raise Exception.Create('No registered property in class "' + AClass.BaseClassName + '" found with name "' + lVal.ClassProp +'"');
              lTempStr := lProp.Name;
              lType := lProp.PropertyType;

              lValueNode := lValNode.ChildNodes.Item[0];
              if lValueNode <> nil then
                begin
                  lValStr := lValueNode.TextContent;
                  case lProp.PropertyType of
                    ptAnsiString, ptString:
                      lVal.Value := lValStr;
                    ptBoolean:
                      lVal.Value := StrtoBool(lValStr);
                    ptInt64, ptInteger:
                      lVal.Value := StrToInt(lValStr);
                    ptDateTime:
                      lVal.Value := tiIntlDateStorAsDateTime(lValStr);
                    ptEnum:;
                    ptDouble, ptCurrency, ptSingle:
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
  lUnit: TMapUnitDef;
  lRefNodeList, lRefNode: TDomNode;
  lRefCtr: integer;
  lUnitNode: TDomNode;
  lName: string;
begin
  if AUnitList = nil then exit;

  for lCtr := 0 to AUnitList.Length - 1 do
    begin
      lUnitNode := AUnitList.Item[lCtr];
      if lUnitNode.NodeType = ELEMENT_NODE then
        begin
          lName := lUnitNode.Attributes.GetNamedItem('name').NodeValue;
          lUnit := TMapUnitDef(FProject.Units.FindByProps(['Name'], [lName]));
          if lUnit = nil then
            begin
              lUnit := TMapUnitDef.Create;
              lUnit.Name := lName;
              FProject.Units.Add(lUnit);
            end;

          ReadUnitEnums(lUnit, lUnitNode.FindNode('enums'));
          ReadUnitClasses(lUnit, lUnitNode.FindNode('classes'));

          // Reference (uses)
          lRefNodeList := lUnitNode.FindNode('references');
          if (lRefNodeList <> nil) and (lRefNodeList.HasChildNodes) then
            begin
              for lRefCtr := 0 to lRefNodeList.ChildNodes.Length - 1 do
                begin
                  lRefNode := lRefNodeList.ChildNodes.Item[lRefCtr];
                  if lRefNode.NodeType = ELEMENT_NODE then
                    lUnit.References.Add(lRefNode.Attributes.GetNamedItem('name').NodeValue);
                end;
            end;
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
  FProject.ClearAll;

  LoadXMLDoc(AFileName);

  lNode := FXML.DocumentElement;
  if lNode.Attributes.GetNamedItem('project-name') = nil then
    raise Exception.Create(ClassName + '.ReadSchema: Missing <project-name> attribute.');

  FProject.ProjectName := lNode.Attributes.GetNamedItem('project-name').NodeValue;

  // Establish the base directory
  lDirNode := lNode.Attributes.GetNamedItem('base-directory');
  if lDirNode <> nil then
    begin
      if lDirNode.NodeValue <> '' then
        FProject.BaseDirectory := lNode.Attributes.GetNamedItem('base-directory').NodeValue
      else
      begin
        lPath := ExtractFileDir(AFileName);
        if lPath = '' then  // means only the filename was passed in, without any path details
          lPath := GetCurrentDir;
        FProject.BaseDirectory := lPath;
      end;
    end
  else
    begin
      lPath := ExtractFileDir(AFileName);
      if lPath = '' then  // means only the filename was passed in, without any path details
        lPath := GetCurrentDir;
      FProject.BaseDirectory := lPath;
    end;

  lDirNode := lNode.Attributes.GetNamedItem('outputdir');
  if lDirNode = nil then
    FProject.OrigOutDirectory := FProject.BaseDirectory
  else
    FProject.OrigOutDirectory := lDirNode.NodeValue;

  // Establish the Output directory, if present.
  if lDirNode <> nil then
    begin
      if lDirNode.NodeValue <> '' then
        begin
          lPath := GetAbsolutePath(FProject.BaseDirectory, lDirNode.NodeValue);
          FProject.OutputDirectory := lPath;
        end
      else
        FProject.OutputDirectory := FProject.BaseDirectory;
    end
  else
    begin
      FProject.OutputDirectory := FProject.BaseDirectory;
    end;

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
  if lNode.FindNode('includes') <> nil then
    begin
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
    end;



  lUnitList := FXML.DocumentElement.FindNode('project-units').ChildNodes;
  ReadProjectUnits(lUnitList);

  FProject.FileName := AFileName;

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
            if lClassAttr <> nil then
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

            lClassAttr := lClassNode.Attributes.GetNamedItem('list-saves-database-name');
            if lClassAttr <> nil then
              lNewClass.ListSavesDatabaseName := StrToBool(lClassAttr.NodeValue)
            else
              lNewClass.ListSavesDatabaseName := true;

            lClassAttr := lClassNode.Attributes.GetNamedItem('notify-observers');
            if lClassAttr <> nil then
              lNewClass.NotifyObserversOfPropertyChanges := StrToBool(lClassAttr.NodeValue);

            if lClassNode.FindNode('class-props') = nil then
              raise Exception.Create(ClassName + '.ReadUnitClasses: "class-props" node is not present.');

            lClassProps := lClassNode.FindNode('class-props').ChildNodes;
            if lClassProps <> nil then
              ReadClassProps(lNewClass, lClassProps);

            lClassMapNode := lClassNode.FindNode('mapping');
            if lClassMapNode <> nil then
              begin
                lNewClass.ClassMapping.PKName := lClassMapNode.Attributes.GetNamedItem('pk').NodeValue;
                lNewClass.ClassMapping.TableName := lClassMapNode.Attributes.GetNamedItem('table').NodeValue;
                lNewClass.ClassMapping.PKField := lClassMapNode.Attributes.GetNamedItem('pk-field').NodeValue;
                lNewClass.ClassMapping.OIDType := gStrToOIDType(lClassMapNode.Attributes.GetNamedItem('oid-type').NodeValue);
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
                            lTemp := StringReplace(lSelectNode.FindNode('sql').ChildNodes.Item[0].NodeValue, #13, ' ', [rfReplaceAll]);
                            lTemp := StringReplace(lTemp, #10, ' ', [rfReplaceAll]);
                            lTemp := tiNormalizeStr(lTemp);
                            // Change variable ${field_list} into list of field names in sql format
                            if POS('${field_list}', lTemp) > 0 then
                              lTemp := StringReplace(lTemp, '${field_list}', CreateSQLSelectList(lNewClass), [rfReplaceAll]);
                            lNewSelect.SQL := lTemp;
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
  lValuesNode: TDomNode;
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
          lNewEnum.EnumSetName := lEnum.Attributes.GetNamedItem('set').NodeValue;

          // Retrieve its values
          lValuesNode := lEnum.FindNode('values');
          if lValuesNode <> nil then
            lEnumValuesList := lValuesNode.ChildNodes;


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

{ TProjectWriter }

destructor TProjectWriter.Destroy;
begin
  if FDoc <> nil then
    FDoc.Free;
  inherited Destroy;
end;

procedure TProjectWriter.WriteClassMappings(AClassDef: TMapClassDef;
  AClassNode: TDOMElement);
var
  lCtr: integer;
  lMapProp: TPropMapping;
  lNewMapNode: TDOMElement;
  lNewMapPropNode: TDOMElement;
begin

  lNewMapNode := FDoc.CreateElement('mapping');
  AClassNode.AppendChild(lNewMapNode);

  lNewMapNode.SetAttribute('table', AClassDef.ClassMapping.TableName);
  lNewMapNode.SetAttribute('pk', AClassDef.ClassMapping.PKName);
  lNewMapNode.SetAttribute('pk-field', AClassDef.ClassMapping.PKField);

  case AClassDef.ClassMapping.OIDType of
    otString: lNewMapNode.SetAttribute('oid-type', 'string');
    otInt: lNewMapNode.SetAttribute('oid-type', 'int');
  end;

  for lCtr := 0 to AClassDef.ClassMapping.PropMappings.Count - 1 do
    begin
      lMapProp := AClassDef.ClassMapping.PropMappings.Items[lCtr];
      lNewMapPropNode := FDoc.CreateElement('prop-map');
      lNewMapPropNode.SetAttribute('prop', lMapProp.PropName);
      lNewMapPropNode.SetAttribute('field', lMapProp.FieldName);
      lNewMapPropNode.SetAttribute('type', gPropTypeToStr(lMapProp.PropertyType));
      lNewMapNode.AppendChild(lNewMapPropNode);
    end;

end;

procedure TProjectWriter.WriteClassProps(AClassDef: TMapClassDef;
  AClassNode: TDOMElement);
var
  lNewPropNode: TDOMElement;
  lClassPropsNode: TDOMElement;
  lCtr: integer;
  lProp: TMapClassProp;
begin

  lClassPropsNode := FDoc.CreateElement('class-props');
  AClassNode.AppendChild(lClassPropsNode);

  for lCtr := 0 to AClassDef.ClassProps.Count - 1 do
    begin
      lProp := AClassDef.ClassProps.Items[lCtr];
      lNewPropNode := FDoc.CreateElement('prop');
      lNewPropNode.SetAttribute('name', lProp.Name);
      if lProp.PropertyType = ptEnum then
        lNewPropNode.SetAttribute('type', lProp.PropTypeName)
      else
        lNewPropNode.SetAttribute('type', gPropTypeToStr(lProp.PropertyType));

      lClassPropsNode.AppendChild(lNewPropNode);
    end;
end;

procedure TProjectWriter.WriteClassSelections(AClassDef: TMapClassDef;
  AClassNode: TDOMElement);
var
  lCtr, lParamCtr: integer;
  lSelect: TClassMappingSelect;
  lParam: TSelectParam;
  lNewSelNode: TDOMElement;
  lNewSelectionsNode: TDOMElement;
  lNewParamsNode: TDOMElement;
  lNewParam: TDOMElement;
  lNewCDATA: TDOMCDATASection;
  lNewSQLNode: TDOMElement;
begin

  lNewSelectionsNode := FDoc.CreateElement('selections');
  AClassNode.AppendChild(lNewSelectionsNode);

  for lCtr := 0 to AClassDef.Selections.Count - 1 do
    begin
      lSelect := AClassDef.Selections.Items[lCtr];
      lNewSelNode := FDoc.CreateElement('select');
      lNewSelNode.SetAttribute('name', lSelect.Name);
      // SQL
      lNewSQLNode := FDoc.CreateElement('sql');
      lNewCDATA := FDoc.CreateCDATASection(WrapText(lSelect.SQL, 40));
      lNewSQLNode.AppendChild(lNewCDATA);
      lNewSelNode.AppendChild(lNewSQLNode);

      // Params Node
      lNewParamsNode := FDoc.CreateElement('params');
      lNewSelNode.AppendChild(lNewParamsNode);

      // Add params to Params node
      for lParamCtr := 0 to lSelect.Params.Count - 1 do
        begin
          lParam := lSelect.Params.Items[lParamCtr];
          lNewParam := FDoc.CreateElement('param');
          lNewParam.SetAttribute('name', lParam.ParamName);
          lNewParam.SetAttribute('pass-by', lParam.PassBy);
          lNewParam.SetAttribute('sql-param', lParam.SQLParamName);
          if lParam.ParamType = ptEnum then
            begin
              lNewParam.SetAttribute('type', 'enum');
              lNewParam.SetAttribute('type-name', lParam.ParamTypeName);
            end
          else
            begin
              lNewParam.SetAttribute('type', gPropTypeToStr(lParam.ParamType));
            end;
          lNewParamsNode.AppendChild(lNewParam);
        end;
      // finally add the selection node to the <selections> node.
      lNewSelectionsNode.AppendChild(lNewSelNode);
    end;
end;

procedure TProjectWriter.WriteClassValidators(AClassDef: TMapClassDef;
  AClassNode: TDOMElement);
var
  lVal: TMapValidator;
  lNewValidatorsNode: TDOMElement;
  lNewValNode: TDOMElement;
  lNewValItemNode: TDOMElement;
  lNewValueNode: TDOMElement;
  lCtr, lItemCtr: integer;
begin

  lNewValidatorsNode := FDoc.CreateElement('validators');
  AClassNode.AppendChild(lNewValidatorsNode);

  for lCtr := 0 to AClassDef.Validators.Count - 1 do
    begin
      lVal := AClassDef.Validators.Items[lCtr];
      lNewValNode := FDoc.CreateElement('item');
      lNewValNode.SetAttribute('prop', lVal.ClassProp);
      lNewValNode.SetAttribute('type', gValTypeToStr(lVal.ValidatorType));
      if not VarIsNull(lVal.Value) then
        begin
          lNewValueNode := FDoc.CreateElement('value');
          lNewValueNode.TextContent := lVal.Value;
          lNewValNode.AppendChild(lNewValueNode);
        end;
      lNewValidatorsNode.AppendChild(lNewValNode);
    end;

end;

procedure TProjectWriter.WriteProject(Aproject: TMapProject;
  const AFilePath: string);
var
  lDocElem: TDOMElement;
  lNewElem: TDOMElement;
  lDir: string;
begin
  if FDoc <> nil then
    begin
      FreeAndNil(FDoc);
    end;

  FWriterProject := AProject;

  FDoc := TXMLDocument.Create;

  // Setup the <project> root node
  lDocElem := FDoc.CreateElement('project');
  lDocElem.SetAttribute('tab-spaces', IntToStr(FWriterProject.TabSpaces));
  lDocElem.SetAttribute('begin-end-tabs', IntToStr(FWriterProject.BeginEndTabs));
  lDocElem.SetAttribute('visibility-tabs', IntToStr(FWriterProject.VisibilityTabs));
  lDocElem.SetAttribute('project-name', FWriterProject.ProjectName);
  lDocElem.SetAttribute('outputdir', FWriterProject.OrigOutDirectory);
  lDocElem.SetAttribute('enum-type', 'int');
  FDoc.AppendChild(lDocElem);



  WriteProjectUnits(FWriterProject, lDocElem);

  WriteXMLFile(FDoc, AFilePath);

end;

procedure TProjectWriter.WriteProject(AProject: TMapProject; const ADirectory: String; const AFileName: string);
begin
  if FDoc <> nil then
    begin
      FreeAndNil(FDoc);
    end;

  FWriterProject := AProject;

  FDoc := TXMLDocument.Create;

  FDirectory := ExcludeTrailingPathDelimiter(ADirectory);

  FDirectory := ExcludeTrailingPathDelimiter(ExtractFileDir(AFileName));

  if AFileName <> '' then
    WriteProject(AProject, FDirectory + PathDelim + AFileName)
  else
    WriteProject(AProject, FDirectory + PathDelim + FWriterProject.ProjectName + '.xml');
end;

procedure TProjectWriter.WriteProjectUnits(AProject: TMapProject;
  ADocElem: TDOMElement);
var
  lCtr: integer;
  lUnit: TMapUnitDef;
  lUnitsElem: TDOMElement;
  lNewUnitNode: TDOMElement;
begin
  lUnitsElem := FDoc.CreateElement('project-units');
  FDoc.DocumentElement.AppendChild(lUnitsElem);

  for lCtr := 0 to FWriterProject.Units.Count - 1 do
    begin
      lUnit := FWriterProject.Units.Items[lCtr];
      lNewUnitNode := FDoc.CreateElement('unit');
      lNewUnitNode.SetAttribute('name', lUnit.Name);
      WriteUnit(lUnit, lNewUnitNode);
      lUnitsElem.AppendChild(lNewUnitNode);
    end;
end;

procedure TProjectWriter.WriteSingleUnitClass(AClassDef: TMapClassDef;
  AClassesNode: TDOMElement);
var
  lNewClassNode: TDOMElement;
begin
  lNewClassNode := FDoc.CreateElement('class');
  AClassesNode.AppendChild(lNewClassNode);

  lNewClassNode.SetAttribute('base-class', AClassDef.BaseClassName);
  lNewClassNode.SetAttribute('base-class-parent', AClassDef.BaseClassParent);
  lNewClassNode.SetAttribute('auto-map', LowerCase(BoolToStr(AClassDef.AutoMap, true)));
  lNewClassNode.SetAttribute('auto-create-list', LowerCase(BoolToStr(AClassDef.AutoCreateListClass, true)));
  lNewClassNode.SetAttribute('list-saves-database-name', LowerCase(BoolToStr(AClassDef.ListSavesDatabaseName, true)));

  WriteClassProps(AClassDef, lNewClassNode);
  WriteClassValidators(AClassDef, lNewClassNode);
  WriteClassMappings(AClassDef, lNewClassNode);
  WriteClassSelections(AClassDef, lNewClassNode);

end;

procedure TProjectWriter.WriteUnit(AUnitDef: TMapUnitDef;
  AUnitNode: TDOMElement);
var
  lCtr: integer;
  lEnumNode: TDOMElement;
  lClassesNode: TDOMElement;
  lEnum: TMapEnum;
  lClass: TMapClassDef;
begin
  lEnumNode := FDoc.CreateElement('enums');
  AUnitNode.AppendChild(lEnumNode);

  lClassesNode := FDoc.CreateElement('classes');
  AUnitNode.AppendChild(lClassesNode);

  WriteUnitEnums(AUnitDef, AUnitNode);

  WriteUnitClasses(AUnitDef, lClassesNode);

end;

procedure TProjectWriter.WriteUnitClasses(AUnitDef: TMapUnitDef;
  AClassesNode: TDOMElement);
var
  lCtr: integer;
  lClassDef: TMapClassDef;
  lClassesNode: TDOMNode;
begin
  for lCtr := 0 to AUnitDef.UnitClasses.Count - 1 do
    begin
      lClassDef := AUnitDef.UnitClasses.Items[lCtr];
      WriteSingleUnitClass(lClassDef, AClassesNode);
    end;
end;

procedure TProjectWriter.WriteUnitEnums(AUnitDef: TMapUnitDef;
  AUnitNode: TDOMElement);
var
  lEnumsNode: TDOMNode;
  lEnumEl: TDOMElement;
  lValuesEl: TDOMElement;
  lSingleValNode: TDOMElement;
  lCtr: integer;
  lItemCtr: integer;
  lEnum: TMapEnum;
  lEnumVal: TMapEnumValue;
begin
  lEnumsNode := AUnitNode.FindNode('enums');
  for lCtr := 0 to AUnitDef.UnitEnums.Count - 1 do
    begin
      lEnum := AUnitDef.UnitEnums.Items[lCtr];
      lEnumEl := FDoc.CreateElement('enum');
      lValuesEl := FDoc.CreateElement('values');
      lEnumEl.AppendChild(lValuesEl);
      lEnumEl.SetAttribute('name', lEnum.EnumName);
      // items of enum
      for lItemCtr := 0 to lEnum.Values.Count - 1 do
        begin
          lEnumVal := lEnum.Values.Items[lItemCtr];
          lSingleValNode := FDoc.CreateElement('item');
          lSingleValNode.SetAttribute('name', lEnumVal.EnumValueName);
          if lEnumVal.EnumValue >= 0 then
            lSingleValNode.SetAttribute('value', IntToStr(lEnumVal.EnumValue));
          // Append to <values> node
          lValuesEl.AppendChild(lSingleValNode);
        end;
      lEnumsNode.AppendChild(lEnumEl);
    end;
end;

initialization
  gSetSchemaReaderClass(TFPCSchemaXMLReader);


end.

