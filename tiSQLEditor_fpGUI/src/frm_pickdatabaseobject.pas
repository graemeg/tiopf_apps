unit frm_pickdatabaseobject;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, 
  Classes, 
  fpg_base, 
  fpg_main, 
  fpg_form,
  fpg_button,
  fpg_combobox,
  fpg_panel,
  fpg_tree,
  fpg_popupwindow,
  fpg_imagelist,
  tiQuery, 
  tiObject{,
  tiTreeBuildVisitor},
  tiMediators,
  tiListMediators;

type

  TSQLFormatters   = class ;
  TSQLFormatterAbs = class ;


  TSQLFormatters = class( TtiObjectList )
  protected
    function    GetItems(i: integer): TSQLFormatterAbs ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TSQLFormatterAbs); reintroduce ;
  public
    constructor Create ; override ;
    property    Items[i:integer] : TSQLFormatterAbs read GetItems write SetItems ;
    function    Add( pObject : TSQLFormatterAbs): integer ; reintroduce ;
  end ;


  TSQLFormatterAbs = class( TtiObject )
  private
    FData : TtiObject ;
  protected
    function    GetOwner: TSQLFormatters; reintroduce ;
    procedure   SetOwner(const Value: TSQLFormatters); reintroduce ;
    property    Data : TtiObject read FData write FData ;
    function    GetCurrentTable : TtiDBMetaDataTable ;
    function    GetCurrentField : TtiDBMetaDataField ;
    function    GetCurrentTableName : string ;
    function    GetCurrentFieldName : string ;
    function    GetAllFields: string;
  public
    function    GetSQL( pData : TtiObject ) : String ; virtual ;
    property    Owner       : TSQLFormatters             read GetOwner      write SetOwner ;
  end ;


  TSQLFormatterCurrentObject = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TtiObject ) : String ; override ;
  end ;


  TSQLFormatterAllFields = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TtiObject ) : String ; override ;
  end ;


  TSQLFormatterSelectSQL = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TtiObject ) : String ; override ;
  end ;


  TSQLFormatterCreateSQL = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TtiObject ) : String ; override ;
  end ;


  TSQLFormatterUpdateSQL = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TtiObject ) : String ; override ;
  end ;


  TSQLFormatterDeleteSQL = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TtiObject ) : String ; override ;
  end ;


  TSQLFormatterDropTable = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TtiObject ) : String ; override ;
  end ;


  TSQLFormatterCreateTableSQL = class( TSQLFormatterAbs )
  protected
    function    GetCaption: string; override ;
  public
    function    GetSQL( pData : TtiObject ) : String ; override ;
  end ;


  TPickDatabaseObjectForm = class(TfpgPopupWindow)
  private
    {@VFD_HEAD_BEGIN: PickDatabaseObjectForm}
    Bevel1: TfpgBevel;
    btnRefresh: TfpgButton;
    cbAction: TfpgComboBox;
    btnInsertObject: TfpgButton;
    btnInsertSql: TfpgButton;
    btnClose: TfpgButton;
    TV: TfpgTreeView;
    {@VFD_HEAD_END: PickDatabaseObjectForm}
    FText: string;
    FData: TtiDBMetaData;
    FSQLFormatters: TSQLFormatters;
//    FDataMappings: TtiTVDataMappings;
    FImageList: TfpgImageList;
    FFormatterMediator: TtiItemComboBoxMediatorView;
    procedure   btnCloseClicked(Sender: TObject);
    procedure   FormPaint(Sender: TObject);
    procedure   FormShow(Sender: TObject);
    procedure   BuildTheTree;
    procedure   InitializeTreeview;
    procedure   SetupDataMappings;
    function    GetComboboxSelection: integer;
    procedure   InsertStatementExecute(Sender: TObject);
    procedure   InsertObjectExecute(Sender: TObject);
    procedure   TVKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   ClearImageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; {override;}
    property    ComboboxSelection: integer read GetComboboxSelection;
    property    Text: string read FText;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiUtils,
  tiOPFManager,
  tiConstants,
  Math,
  tiINI;


const
  cusPad = '    ' ;
  cusCR  = cLineEnding ;


{@VFD_NEWFORM_IMPL}

procedure TPickDatabaseObjectForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: PickDatabaseObjectForm}
  Name := 'PickDatabaseObjectForm';
  SetPosition(379, 195, 330, 308);
  Hint := '';
  ShowHint := True;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(4, 4, 322, 32);
    Hint := '';
  end;

  btnRefresh := TfpgButton.Create(Bevel1);
  with btnRefresh do
  begin
    Name := 'btnRefresh';
    SetPosition(4, 4, 24, 24);
    Text := 'R';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 1;
  end;

  cbAction := TfpgComboBox.Create(Bevel1);
  with cbAction do
  begin
    Name := 'cbAction';
    SetPosition(32, 4, 200, 24);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 2;
  end;

  btnInsertObject := TfpgButton.Create(Bevel1);
  with btnInsertObject do
  begin
    Name := 'btnInsertObject';
    SetPosition(236, 4, 24, 24);
    Text := 'Obj';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Insert current object (Ctrl+Enter)';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 3;
    OnClick := @InsertObjectExecute;
  end;

  btnInsertSql := TfpgButton.Create(Bevel1);
  with btnInsertSql do
  begin
    Name := 'btnInsertSql';
    SetPosition(264, 4, 24, 24);
    Text := 'Sql';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Insert SQL statement (Alt+Enter)';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 4;
    OnClick := @InsertStatementExecute;
  end;

  btnClose := TfpgButton.Create(Bevel1);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(292, 4, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.close';
    TabOrder := 5;
    OnClick := @btnCloseClicked;
  end;

  TV := TfpgTreeView.Create(self);
  with TV do
  begin
    Name := 'TV';
    SetPosition(4, 36, 322, 268);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 0;
    ImageList := FImageList;
    ShowImages := True;
    OnKeyPress := @TVKeyPressed;
  end;

  {@VFD_BODY_END: PickDatabaseObjectForm}
  {%endregion}
end;

constructor TPickDatabaseObjectForm.Create(AOwner: TComponent);
var
  i: integer;
  lDBTable: TtiDBMetaDataTable;
begin
  inherited Create(AOwner);
  
  FImageList := TfpgImageList.Create;
  FImageList.AddImage(fpgImages.GetImage('sqlmgr.database'), 0);
  FImageList.AddImage(fpgImages.GetImage('sqlmgr.table'), 1);
  FImageList.AddImage(fpgImages.GetImage('sqlmgr.sql'), 2);
  
  OnPaint := @FormPaint;
  OnShow := @FormShow;
  
  AfterCreate;
//  FDataMappings := TtiTVDataMappings.Create(nil);
//  SetupDataMappings;
  FSQLFormatters := TSQLFormatters.Create;
  
  FData := TtiDBMetaData.Create;
  FData.Owner := TtiObject(gTIOPFManager.DefaultDBConnectionPool);
  gTIOPFManager.ReadMetaDataTables(FData);

  for i := 0 to FData.Count-1 do
  begin
    lDBTable := FData.Items[i];
    gTIOPFManager.ReadMetaDataFields(lDBTable);
  end;
  BuildTheTree;
  
  cbAction.Items.Clear;
  for i := 0 to FSQLFormatters.Count-1 do
    cbAction.Items.AddObject(FSQLFormatters.Items[i].Caption, FSQLFormatters.Items[i]);
  cbAction.FocusItem := 0; // default to the first item. If used before, a new value will be assigned shortly.

  i := Min( FSQLFormatters.Count-1, gINI.ReadInteger(Name, 'Action', -1));
  if i <> -1 then
    cbAction.FocusItem := i;
end;
    
destructor TPickDatabaseObjectForm.Destroy;
begin
  FFormatterMediator.Free;
  FSQLFormatters.Free;

  ClearImageList;
  FImageList.Free;

  FData.Free;
  inherited Destroy;
end;

procedure TPickDatabaseObjectForm.btnCloseClicked(Sender: TObject);
begin
  Close;
end;

procedure TPickDatabaseObjectForm.FormPaint(Sender: TObject);
begin
//  Canvas.SetColor(clBlack);
//  Canvas.DrawRectangle(0, 0, Width, Height);  // black border
  Canvas.DrawButtonFace(0, 0, Width, Height, []);  // 3d rectangle inside black border
end;

procedure TPickDatabaseObjectForm.BuildTheTree;
var
//  lVisitor: TtiVisObjToTree;
  i, j: integer;
  lDB: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  r, n1, n2: TfpgTreeNode;
begin
(*
  lVisitor := TtiVisObjToTree.Create(TV);
  try
    lVisitor.DataMappings := FDataMappings;
    lVisitor.IncludeDeleted := False;
    FData.Iterate(lVisitor);
  finally
    lVisitor.Free;
  end;
*)  
  r := TV.RootNode.AppendText(FData.Caption);
  r.ImageIndex := 0;
  for i := 0 to FData.Count-1 do
  begin 
    lTable := FData.Items[i];
    n1 := r.AppendText(lTable.Caption);
    n1.Data := lTable;
    n1.ImageIndex := 1;
    for j := 0 to lTable.Count-1 do
    begin
      lField := lTable.Items[j];
      n2 := n1.AppendText(lField.Caption);
      n2.Data := lField;
      n2.ImageIndex := 2;
    end;
  end;
  r.Expand;
  TV.Selection := r;
end;

procedure TPickDatabaseObjectForm.SetupDataMappings;
//var
//  lMap: TtiTVDataMapping;
begin
{
  lMap := TtiTVDataMapping(FDataMappings.Add(TtiDBMetaData, 'Caption', 0));
  lMap.CanInsert        := False;
  lMap.CanEdit          := False;
  lMap.CanDelete        := False;

  lMap := TtiTVDataMapping(FDataMappings.Add(TtiDBMetaDataTable, 'Caption', 1));
  lMap.CanInsert        := False;
  lMap.CanEdit          := False;
  lMap.CanDelete        := False;

  lMap := TtiTVDataMapping(FDataMappings.Add(TtiDBMetaDataField, 'Caption', 2));
  lMap.CanInsert        := False;
  lMap.CanEdit          := False;
  lMap.CanDelete        := False;
}
end;

procedure TPickDatabaseObjectForm.InitializeTreeview;
begin
(*
  TV.BeginUpdate;
  try
    TV.Items.Clear;
    if Data = nil then
      Exit; //==>
    BuildTreeView;
    if TV.Items.Count > 0 then
    begin
      TV.Items.GetFirstNode.Expanded := True;
      TV.Items.GetFirstNode.Selected := True;
      TV.Items.GetFirstNode.Focused  := True;
    end;
  finally
    TV.EndUpdate;
  end;
*)
end;

procedure TPickDatabaseObjectForm.FormShow(Sender: TObject);
begin
  TV.SetFocus;
end;

function TPickDatabaseObjectForm.GetComboboxSelection: integer;
begin
  Result := cbAction.FocusItem;
end;

procedure TPickDatabaseObjectForm.InsertStatementExecute(Sender: TObject);
begin
  FText := TSQLFormatterAbs(cbAction.Items.Objects[cbAction.FocusItem]).GetSQL(TtiObject(TV.Selection.Data)) ;
  Close;
end;

procedure TPickDatabaseObjectForm.InsertObjectExecute(Sender: TObject);
var
  lSQLFormatter: TSQLFormatterAbs;
begin
  lSQLFormatter := TSQLFormatterCurrentObject.Create;
  try
    FText := lSQLFormatter.GetSQL(TtiObject(TV.Selection.Data));
  finally
    lSQLFormatter.Free;
  end;
  Close;
end;

procedure TPickDatabaseObjectForm.TVKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  if KeyCode = keyEnter then
  begin
    if ShiftState = [ssCtrl] then
    begin
      // Ctrl+Enter pressed
      InsertObjectExecute(Sender);
      Consumed := True;
    end
    else if ShiftState = [ssAlt] then
    begin
      // Alt+Enter pressed
      InsertStatementExecute(Sender);
      Consumed := True;
    end;
  end;
end;

procedure TPickDatabaseObjectForm.ClearImageList;
var
  itm: TfpgImageItem;
  i: integer;
begin
  for i := FImageList.Count-1 downto 0 do
  begin
    itm := FImageList.Items[i];
    itm.Image := nil;   // the actual image was just reference anyway to fpgImages
  end;
end;

{ TSQLFormatterAbs }

function TSQLFormatterAbs.GetAllFields : string ;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
begin

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  for i := 0 to lTable.Count - 1 do
  begin
    result := tiAddTrailingValue( result, cusCR + '    ,' ) ;
    result := result + lTable.Name + '.' + lTable.Items[i].Name ;
  end ;

  if result <> '' then
    result := cusPad + result ;

end;

function TSQLFormatterAbs.GetCurrentField: TtiDBMetaDataField;
begin
  if ( FData is TtiDBMetaDataField ) then
    result := TtiDBMetaDataField( FData )
  else
    result := nil ;
end;

function TSQLFormatterAbs.GetCurrentFieldName: string;
var
  lField : TtiDBMetaDataField ;
begin
  lField := GetCurrentField ;
  if lField <> nil then
    result := lField.Name
  else
    result := '' ;
end;

function TSQLFormatterAbs.GetCurrentTable: TtiDBMetaDataTable;
begin
  if ( FData is TtiDBMetaDataTable ) then
    result := TtiDBMetaDataTable( FData )
  else
  if ( FData is TtiDBMetaDataField ) then
    result := TtiDBMetaDataField( FData ).Owner
  else
    result := nil ;
end;

function TSQLFormatterAbs.GetCurrentTableName: string;
var
  lTable : TtiDBMetaDataTable ;
begin
  lTable := GetCurrentTable ;
  if lTable <> nil then
    result := lTable.Name
  else
    result := '' ;
end;

{ TSQLFormatterInsertAllFields }

function TSQLFormatterAllFields.GetSQL( pData : TtiObject ) : String ;
begin
  inherited GetSQL( pData ) ;
  result := GetAllFields ;
end;

function TSQLFormatterAllFields.GetCaption: string;
begin
  result := 'Insert all fields' ;
end;

{ TSQLFormatterSelectSQL }

function TSQLFormatterSelectSQL.GetSQL(pData: TtiObject):String;
begin
  inherited GetSQL( pData ) ;
  result := GetAllFields ;
  if Result <> '' then
    result :=
      'select' + cusCR +
      result + cusCR +
      'from' + cusCR +
      cusPad +
      GetCurrentTableName ;
end;

function TSQLFormatterSelectSQL.GetCaption: string;
begin
  result := 'Insert SELECT SQL' ;
end;

{ TSQLFormatters }

function TSQLFormatters.Add(pObject: TSQLFormatterAbs): integer;
begin
  Result := inherited Add( pObject) ;
end;

constructor TSQLFormatters.Create;
begin
  inherited;
  Add( TSQLFormatterAllFields.Create ) ;
  Add( TSQLFormatterSelectSQL.Create ) ;
  Add( TSQLFormatterCreateSQL.Create ) ;
  Add( TSQLFormatterUpdateSQL.Create ) ;
  Add( TSQLFormatterDeleteSQL.Create ) ;
  Add( TSQLFormatterDropTable.Create ) ;
  Add( TSQLFormatterCreateTableSQL.Create ) ;
end;

function TSQLFormatters.GetItems(i: integer): TSQLFormatterAbs;
begin
  result := TSQLFormatterAbs( inherited GetItems( i )) ;
end;

procedure TSQLFormatters.SetItems(i: integer; const Value: TSQLFormatterAbs);
begin
  inherited SetItems( i, Value ) ;
end;

{ TSQLFormatterAbs }

function TSQLFormatterAbs.GetOwner: TSQLFormatters;
begin
  result := TSQLFormatters( inherited GetOwner ) ;
end;

function TSQLFormatterAbs.GetSQL(pData: TtiObject): String;
begin
  FData := pData ;
end;

procedure TSQLFormatterAbs.SetOwner(const Value: TSQLFormatters);
begin
  inherited SetOwner( Value ) ;
end;

{ TSQLFormatterCurrentObject }

function TSQLFormatterCurrentObject.GetCaption: string;
begin
  result := 'Insert current object' ;
end;

function TSQLFormatterCurrentObject.GetSQL(pData: TtiObject): String;
begin
  inherited GetSQL( pData ) ;
  if GetCurrentFieldName <> '' then
    result := GetCurrentTableName + '.' +
              GetCurrentFieldName
  else
  if GetCurrentTableName <> '' then
    result := GetCurrentTableName
  else
    result := '' ;
end;

{ TSQLFormatterCreateSQL }

function TSQLFormatterCreateSQL.GetCaption: string;
begin
  result := 'Insert INSERT SQL' ;
end;

function TSQLFormatterCreateSQL.GetSQL(pData: TtiObject): String;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
  lFields : string ;
  lParams : string ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  lFields := '' ;
  lParams := '' ;
  for i := 0 to lTable.Count - 1 do
  begin
    lFields := tiAddTrailingValue( lFields, cusCR + '    ,' ) ;
    lFields := lFields + lTable.Items[i].Name ;

    lParams := tiAddTrailingValue( lParams, cusCR + '    ,' ) ;
    lParams := lParams + ':' + lTable.Items[i].Name ;

  end ;

  if ( lFields <> '' ) and
     ( lParams <> '' ) then
    result := 'insert into ' + lTable.Name + cusCR +
              '(' + cusCR +
              '     ' + lFields + cusCR +
              ')' + cusCR +
              'values' + cusCR +
              '(' + cusCR +
              '     ' + lParams +
              ')' ;
end;

{ TSQLFormatterUpdateSQL }

function TSQLFormatterUpdateSQL.GetCaption: string;
begin
  result := 'Insert UPDATE SQL' ;
end;

function TSQLFormatterUpdateSQL.GetSQL(pData: TtiObject): String;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
  lFields : string ;
  lParams : string ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  lFields := '' ;
  lParams := '' ;
  for i := 0 to lTable.Count - 1 do
  begin
    lFields := tiAddTrailingValue( lFields, cusCR + '    ,' ) ;
    lFields := lFields +
               lTable.Items[i].RPadName + ' = :' +
               lTable.Items[i].Name ;

    lParams := tiAddTrailingValue( lParams, cusCR + '    ,' ) ;
    lParams := lParams +
               lTable.Items[i].RPadName + ' = :Old_' +
               lTable.Items[i].Name ;

  end ;

  if ( lFields <> '' ) and
     ( lParams <> '' ) then
    result := 'update ' + lTable.Name + cusCR +
              'set' + cusCR +
              '     ' + lFields + cusCR +
              'where' + cusCR +
              '     ' + lParams ;
end;

{ TSQLFormatterDeleteSQL }

function TSQLFormatterDeleteSQL.GetCaption: string;
begin
  result := 'Insert DELETE SQL' ;
end;

function TSQLFormatterDeleteSQL.GetSQL(pData: TtiObject): String;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
  lParams : string ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  lParams := '' ;
  for i := 0 to lTable.Count - 1 do
  begin
    lParams := tiAddTrailingValue( lParams, cusCR + 'and ' ) ;
    lParams := lParams +
               lTable.Items[i].RPadName + ' = :Old_' +
               lTable.Items[i].RPadName ;
  end ;

  if ( lParams <> '' ) then
    result := 'delete from ' + lTable.Name + cusCR +
              'where' + cusCR +
              '    ' + lParams ;
end;

{ TSQLFormatterDropTable }

function TSQLFormatterDropTable.GetCaption: string;
begin
  result := 'Drop table' ;
end;

function TSQLFormatterDropTable.GetSQL(pData: TtiObject): String;
var
  lTable : TtiDBMetaDataTable ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  result := 'drop table ' + lTable.Name ;

end;

{ TSQLFormatterCreateTableSQL }

function TSQLFormatterCreateTableSQL.GetCaption: string;
begin
  result := 'Create table SQL' ;
end;

function TSQLFormatterCreateTableSQL.GetSQL(pData: TtiObject): String;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
  lFields : string ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  lFields := '' ;
  for i := 0 to lTable.Count - 1 do
  begin
    lFields := tiAddTrailingValue( lFields, cusCR + '    ,' ) ;
    lFields := lFields +
               lTable.Items[i].RPadName + ' ' +
               'DataTypeUnderConstruction' ;
  end ;

  if ( lFields <> '' ) then
    result := 'create table ' + lTable.Name + cusCR +
              '(' +
              '    ' + lFields + cusCR +
              ')' ;

end;


end.
