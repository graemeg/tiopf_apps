unit frm_browseresult;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_basegrid,
  fpg_grid,
  fpg_panel,
  fpg_menu,
  tiThread,
  tiSQLMgrDataSet_BOM,
  tiSQLMgr_BOM;

type

  TBrowseResultForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: BrowseResultForm}
    SB: TfpgPanel;
    MenuBar: TfpgMenuBar;
    Grid1: TfpgStringGrid;
    mnuFile: TfpgPopupMenu;
    mnuSQL: TfpgPopupMenu;
    mnuGrid: TfpgPopupMenu;
    {@VFD_HEAD_END: BrowseResultForm}
    pmGrid: TfpgPopupMenu;
    FtiDataSetQueryMapping: TtiDataBufferQueryMapping;
    FCols: TStringList;
    FiColWidth: integer;
    procedure SetTIDataSetQueryMapping(const AValue: TtiDataBufferQueryMapping);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure CommandHandler(Sender: TObject);
    procedure SQLReadExecute(Sender: TObject);
    procedure SQLCreateExecute(Sender: TObject);
    procedure SQLUpdateExecute(Sender: TObject);
    procedure SQLDeleteExecute(Sender: TObject);
    procedure SQLAsClassInterfaceExecute(Sender: TObject);
    procedure SQLAsSetupParamsExecute(Sender: TObject);
    procedure SQLAsMapRowToObjectExecute(Sender: TObject);
    procedure CopyCellToClipExecute(Sender: TObject);
    procedure StructureExecute(Sender: TObject);
    function  ColNameText(pIndex: integer): string;
    function  ColNameDelimText(pIndex: integer): string;
    function  ColDataType(pIndex: integer): string;
    function  ColAsProperty(pIndex: integer):string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterCreate; override;
    property TIDataSetQueryMapping: TtiDataBufferQueryMapping read FtiDataSetQueryMapping write SetTIDataSetQueryMapping;
  end;


  // Modify to take a list of SQLMgrQuery objects as well as a single instance
  TthrdSQLMgrAbs = class(TtiThread)
  private
    FtiDataSetQueryMapping: TtiDataBufferQueryMapping;
    FsErrorText: string;
    FForm: TBrowseResultForm;
    procedure SetSQLMgrQuery(const AValue: TSQLMgrQuery);
    function GetSQLMgrQuery: TSQLMgrQuery;
  protected
    procedure DoOnTerminate(Sender: TObject); override;
    procedure DoShowResultSet(Sender: TObject); virtual; abstract;
    procedure DoShowNoResultSet(Sender: TObject);
    procedure DoShowException(Sender: TObject);
  public
    constructor Create(CreateSuspended: Boolean); override;
    property SQLMgrQuery: TSQLMgrQuery read GetSQLMgrQuery write SetSQLMgrQuery;
    procedure Execute; override;
  end;

  TthrdSQLMgrBrowse = class(TthrdSQLMgrAbs)
  protected
    procedure DoShowResultSet(Sender: TObject); override;
  end;

  TthrdSQLMgrSave = class(TthrdSQLMgrAbs)
  protected
    procedure DoShowResultSet(Sender: TObject); override;
  end;


implementation

uses
  tiOPFManager,
  tiDialogs,
  tiINI,
  tiGUIINI,
  tiGUIUtils,
  tiDataBuffer_Cli,
  tiSQLMgrDataSet_Srv, // This has to be used somewhere in the app, and this is a good a place as any
  tiConstants,
  tiUtils,
  tiQuery,
  fpg_dialogs,
  fpg_command_intf,
  Math,
  commands;

var
  uSaveFileName: TFileName;


function GetSQLSaveFileName(pFileName: TFileName): TFileName;
var
  lSD: TfpgFileDialog;
begin
  Result := '';
  lSD    := TfpgFileDialog.Create(nil);
  try
    lSD.FileName := pFileName;
    lSD.Filter := 'CSV files|*.csv|Text files|*.txt|All files|'+AllFilesMask;
    //    lSD.DefaultExt := '.CSV' ;
    //    if SameText( tiExtractExtension( pFileName ), 'TXT' ) then
    //      lSD.FilterIndex := 1 ;
    if lSD.RunSaveFile then
      Result := lSD.FileName;
  finally
    lSD.Free;
  end;
end;

{ TBrowseResultForm }

procedure TBrowseResultForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: BrowseResultForm}
  Name := 'BrowseResultForm';
  SetPosition(358, 262, 587, 261);
  WindowTitle := 'BrowseResultForm';
  Hint := '';

  SB := TfpgPanel.Create(self);
  with SB do
  begin
    Name := 'SB';
    SetPosition(0, 239, 587, 22);
    Align := alBottom;
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := 'Panel';
  end;

  MenuBar := TfpgMenuBar.Create(self);
  with MenuBar do
  begin
    Name := 'MenuBar';
    SetPosition(0, 0, 587, 23);
    Align := alTop;
  end;

  Grid1 := TfpgStringGrid.Create(self);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(0, 24, 587, 215);
    Align := alClient;
    BackgroundColor := TfpgColor($80000002);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 3;
    Options := [go_AlternativeColor, go_SmoothScroll];
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(376, 20, 120, 20);
    AddMenuItem('Close', 'Esc', @CommandHandler).SetCommand(TSQLCloseCommand.Create(self));
  end;

  mnuSQL := TfpgPopupMenu.Create(self);
  with mnuSQL do
  begin
    Name := 'mnuSQL';
    SetPosition(376, 40, 120, 20);
    AddMenuItem('Read', '', @SQLReadExecute);
    AddMenuItem('-', '', nil);
    AddMenuItem('Create', '', @SQLCreateExecute);
    AddMenuItem('Update', '', @SQLUpdateExecute);
    AddMenuItem('Delete', '', @SQLDeleteExecute);
    AddMenuItem('-', '', nil);
    AddMenuItem('As class interface', '', @SQLAsClassInterfaceExecute);
    AddMenuItem('SetupParams', '', @SQLAsSetupParamsExecute);
    AddMenuItem('MapRowToObject', '', @SQLAsMapRowToObjectExecute);
  end;

  mnuGrid := TfpgPopupMenu.Create(self);
  with mnuGrid do
  begin
    Name := 'mnuGrid';
    SetPosition(376, 60, 120, 20);
    AddMenuItem('Show Structure', '', @StructureExecute);
  end;

  {@VFD_BODY_END: BrowseResultForm}
  {%endregion}
  
  pmGrid := TfpgPopupMenu.Create(self);
  with pmGrid do
  begin
    Name := 'pmGrid';
    AddMenuItem('Copy cell to clipboard', '', @CopyCellToClipExecute);
  end;
  Grid1.PopupMenu := pmGrid;

  MenuBar.AddMenuItem('&File', nil).SubMenu  := mnuFile;
  MenuBar.AddMenuItem('&SQL', nil).SubMenu  := mnuSQL;
  MenuBar.AddMenuItem('&Grid', nil).SubMenu := mnuGrid;
end;

procedure TBrowseResultForm.SetTIDataSetQueryMapping(const AValue: TtiDataBufferQueryMapping);
const
  cStatusBarMessage = 'Record count: %d   Time to execute on server: %dms   Time to download data: %dms';
var
  i: integer;
begin
  FtiDataSetQueryMapping := AValue;
  WindowTitle := ' Results for query: ' + FtiDataSetQueryMapping.SQLMgrQuery.Caption;

  Grid1.BeginUpdate;
  try
    // Create the Columns in the StringGrid
    tiDataSetToGridColumns(FtiDataSetQueryMapping.TIDataSet, Grid1);
    SB.Text := Format(cStatusBarMessage, [FtiDataSetQueryMapping.TIDataSet.Count,
      FtiDataSetQueryMapping.TimeToRun, FtiDataSetQueryMapping.TimeToScan]);
    FCols.Clear;
    for i := 0 to FtiDataSetQueryMapping.TIDataSet.Fields.Count - 1 do
      FCols.Add(FtiDataSetQueryMapping.TIDataSet.Fields.Items[i].Name);
    FiColWidth := 0;
    for i := 0 to FCols.Count - 1 do
      FiColWidth := Max(FiColWidth, Length(FCols.Strings[i]));

    // Populate the StringGrid with data (items)
    tiDataSetToGridContents(FtiDataSetQueryMapping.TIDataSet, Grid1);
  finally
    Grid1.EndUpdate;
  end;
end;

procedure TBrowseResultForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  gGUIINI.WriteFormState(self);
  Action := caFree;
end;

procedure TBrowseResultForm.FormShow(Sender: TObject);
begin
  gGUIINI.ReadFormState(self);
end;

constructor TBrowseResultForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCols      := TStringList.Create;
  FiColWidth := 0;
  OnClose    := @FormClose;
  OnShow := @FormShow;
end;

destructor TBrowseResultForm.Destroy;
begin
  FCols.Free;
  FtiDataSetQueryMapping.Free;
  inherited Destroy;
end;

procedure TBrowseResultForm.CommandHandler(Sender: TObject);
var
  cmd: ICommand;
  holder: ICommandHolder;
begin
  if Supports(Sender, ICommandHolder, holder) then
  begin
    cmd := holder.GetCommand;
    if Assigned(cmd) then
      cmd.Execute;
  end;
end;

procedure TBrowseResultForm.SQLReadExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := 'SELECT' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + cLineEnding;
  end ;

  ls := ls + 'FROM' + cLineEnding +
        'ENTER_TABLE_NAME' + cLineEnding;

  ls := ls + 'WHERE' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'AND  ' ;
    ls := ls + ColNameText(i) + ' = :' + ColNameText(i) + cLineEnding;
  end ;

  fpgClipboard.Text := ls ;
end;

procedure TBrowseResultForm.SQLCreateExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := 'insert into ENTER_TABLE_NAME' + cLineEnding +
    '(' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + cLineEnding;
  end ;

  ls := ls + ')' + cLineEnding + 'VALUES' + cLineEnding + '(' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     :'
    else
      ls := ls + '    ,:' ;
    ls := ls + ColNameText(i) + cLineEnding;
  end ;

  ls := ls + ')' ;

  fpgClipboard.Text := ls;
end;

procedure TBrowseResultForm.SQLUpdateExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := 'UPDATE ENTER_TABLE_NAME' + cLineEnding +
    'SET' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + ' = :' + ColNameText(i) + cLineEnding;
  end ;

  ls := ls + 'WHERE' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'AND  ' ;
    ls := ls + ColNameText(i) + ' = :OLD_' + ColNameText(i) + cLineEnding;
  end ;

  fpgClipboard.Text := ls ;
end;

procedure TBrowseResultForm.SQLDeleteExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := 'DELETE FROM ENTER_TABLE_NAME' + cLineEnding +
    'WHERE' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'AND  ' ;
    ls := ls +
      ColNameText( i ) + ' = :' + ColNameText(i) + cLineEnding;
  end ;

  fpgClipboard.Text := ls ;
end;

procedure TBrowseResultForm.SQLAsClassInterfaceExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := '  EnterClassType = class(TtiObject)' + cLineEnding +
    '  private' + cLineEnding +
    '  protected' + cLineEnding +
    '  public' + cLineEnding +
    '  published' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    ls := ls +
      '    property ' + ColAsProperty(i) +
      ' : ' + ColDataType(i) + ' read F' +
      ColAsProperty(i) +
      ' write F' +
      ColAsProperty(i) +
      ';' + cLineEnding;
  end ;

  ls := ls + '  end;' + tiLineEnd(2);

  fpgClipboard.Text := ls ;
end;

procedure TBrowseResultForm.SQLAsSetupParamsExecute(Sender: TObject);
var
  ls: string;
  i: integer;
begin
  ls :=
    'var'                            + cLineEnding +
    '  lData: EnterClassType;'         + cLineEnding +
    'begin'                          + cLineEnding +
    '  lData := (Visited as EnterClassType);' + cLineEnding +
    '  lData.OID.AssignToTIQuery(Query);' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    ls := ls + '  Query.ParamAs[ ' +
      ColNameDelimText(i) + ' ] := lData.' + ColAsProperty(i) + ';' + cLineEnding;
  end ;

  fpgClipboard.Text := ls;
end;

procedure TBrowseResultForm.SQLAsMapRowToObjectExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls :=
    'var'                               + cLineEnding +
    '  lData: EnterClassName;'          + cLineEnding +
    'begin'                             + cLineEnding +
    '  lData := EnterClassName.Create;' + cLineEnding +
    '  lData.OID.AssignFromTIQuery(Query);' + cLineEnding;

  for i := 0 to FCols.Count - 1 do
  begin
    ls := ls +
      '  ' +
      'lData.' + ColAsProperty(i) + ' := Query.FieldAs' +
      tiPadR( ColDataType(i) + '[ ', 10 ) +
      ColNameDelimText(i) + ' ];' + cLineEnding;
  end ;

  ls := ls + '  lData.ObjectState := posClean;' + cLineEnding;
  ls := ls + '  (Visited as TtiObjectList).Add(lData);' + cLineEnding;

  fpgClipboard.Text := ls;
end;

procedure TBrowseResultForm.CopyCellToClipExecute(Sender: TObject);
begin
  fpgClipboard.Text := Grid1.Cells[Grid1.FocusCol, Grid1.FocusRow];
end;

procedure TBrowseResultForm.StructureExecute(Sender: TObject);
var
  i : integer ;
  ls : string ;
begin
  ls := '' ;
  for i := 0 to Grid1.ColumnCount - 1 do
    ls := ls + Grid1.Columns[i].Title + cLineEnding;
  tiShowString(ls);
end;

function TBrowseResultForm.ColNameText(pIndex: integer): string;
begin
  result := tiPadR( FCols.Strings[pIndex], FiColWidth ) ;
end;

function TBrowseResultForm.ColNameDelimText(pIndex: integer): string;
begin
  result := tiPadR( QuotedStr( FCols.Strings[pIndex]), FiColWidth+2 ) ;
end;

function TBrowseResultForm.ColDataType(pIndex: integer): string;
begin
  result := QueryFieldKindToString(FtiDataSetQueryMapping.TIDataSet.Fields.Items[pIndex].Kind);
  if SameText( result, 'date' ) then
    result := result + 'Time' ;
end;

function TBrowseResultForm.ColAsProperty(pIndex: integer): string;
begin
  result := SQLParamAsProp(FCols.Strings[pIndex], FiColWidth)
end;

{ TthrdSQLMgrAbs }

constructor TthrdSQLMgrAbs.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FsErrorText     := '';
  FtiDataSetQueryMapping := TtiDataBufferQueryMapping.Create;
end;

procedure TthrdSQLMgrAbs.DoOnTerminate(Sender: TObject);
begin
  inherited DoOnTerminate(Sender);
  // An error
  if FsErrorText <> '' then
    DoShowException(Sender)
  // An empty result set
  else if FtiDataSetQueryMapping.TIDataSet.Count = 0 then
  begin
    DoShowNoResultSet(Sender);
  end
  // A valid result set
  else
  begin
    DoShowResultSet(Sender);
  end;
end;

procedure TthrdSQLMgrAbs.DoShowException(Sender: TObject);
begin
  tiAppError(FsErrorText, 'Error running query');
end;

procedure TthrdSQLMgrAbs.Execute;
begin
  gTIOPFManager.Read(FtiDataSetQueryMapping);
end;

procedure TthrdSQLMgrAbs.SetSQLMgrQuery(const AValue: TSQLMgrQuery);
begin
  { FtiDataSetQueryMapping is now taking ownership of AValue }
  FtiDataSetQueryMapping.SQLMgrQuery := AValue;
  Text := 'Running: ' + FtiDataSetQueryMapping.SQLMgrQuery.Caption;
end;

function TthrdSQLMgrAbs.GetSQLMgrQuery: TSQLMgrQuery;
begin
  Result := FtiDataSetQueryMapping.SQLMgrQuery;
end;

procedure TthrdSQLMgrAbs.DoShowNoResultSet(Sender: TObject);
begin
  tiAppMessage('Query executed. There is no result set.');
  FtiDataSetQueryMapping.Free;
end;

{ TthrdSQLMgrBrowse }

procedure TthrdSQLMgrBrowse.DoShowResultSet(Sender: TObject);
begin
  FForm := TBrowseResultForm.Create(nil);
  { The form now takes ownership }
  FForm.TIDataSetQueryMapping := FtiDataSetQueryMapping;
  FForm.Show;
end;

{ TthrdSQLMgrSave }

procedure TthrdSQLMgrSave.DoShowResultSet(Sender: TObject);
var
  lFileName: TFileName;
begin
  lFileName := uSaveFileName;
  if lFileName = '' then
    lFileName := gINI.ReadString('SQLResultAsFile', 'FileName', '');
  lFileName := GetSQLSaveFileName(lFileName);
  if lFileName <> '' then
  begin
    tiDataSetToTextFile(FtiDataSetQueryMapping.TIDataSet, lFileName);
    gINI.ReadString('SQLResultAsFile', 'FileName', lFileName);
    uSaveFileName := lFileName;
  end;
  FtiDataSetQueryMapping.Free;
end;

initialization
  uSaveFileName := '';

end.

