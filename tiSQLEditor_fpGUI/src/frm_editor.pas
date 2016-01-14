unit frm_editor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_memo,
  fpg_panel,
  fpg_menu,
  tiSQLMgr_BOM,
  frm_pickdatabaseobject;

type

  TEditorForm = class(TfpgBevel)
  private
    {@VFD_HEAD_BEGIN: EditorForm}
    FMemoSQL: TfpgMemo;
    {@VFD_HEAD_END: EditorForm}
    FReadOnly: Boolean;
    FDirty: Boolean;
    FData: TSQLMgrQuery;
    FFormPickDatabaseObject: TPickDatabaseObjectForm;
    procedure SetReadOnly(const AValue: Boolean);
    function GetSQL: string;
    procedure GUItoBOM;
    procedure CloneData(const pList: TList);
    procedure MemoSQLKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure CopyToClipAsQuotedExecute(Sender: TObject);
    procedure PasteAsQuotedStringExecute(Sender: TObject);
    procedure CopyToClipExecute(Sender: TObject);
    procedure PasteFromClipExecute(Sender: TObject);
    procedure DoShowPickDatabaseObjectWindow;
    procedure FormPickDatabaseObjectOnClose(Sender: TObject);
  public
    pmEdit: TfpgPopupMenu; // public so it can be shared with Main Form
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure AfterCreate; {override;}
    procedure Execute;
    procedure ExecuteThenSave;
    //    property  Data: TSQLMgrQuery read FData write SetData;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    procedure LoadFromFile(pFileName: TFileName);
    procedure SaveToFile(pFileName: TFileName);
    procedure Clear;
    property SQL: string read GetSQL;
    //    procedure SetFocus; override;
    property Dirty: Boolean read FDirty write FDirty;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiUtils,
  tiINI,
  tiLog,
  frm_browseresult;

{@VFD_NEWFORM_IMPL}

procedure TEditorForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: EditorForm}
  Name := 'EditorForm';
  SetPosition(372, 189, 300, 250);
  //  WindowTitle := 'EditorForm';
  Hint := '';

  FMemoSQL := TfpgMemo.Create(self);
  with FMemoSQL do
  begin
    Name     := 'FMemoSQL';
    SetPosition(4, 4, 292, 244);
    FontDesc := '#Edit2';
    Hint     := '';
    TabOrder := 1;
    Anchors  := [anTop, anLeft, anBottom, anRight];
    OnKeyPress := @MemoSQLKeyPressed;
  end;

  {@VFD_BODY_END: EditorForm}
  {%endregion}
  
  pmEdit := TfpgPopupMenu.Create(self);
  with pmEdit do
  begin
    Name := 'pmEdit';
    AddMenuItem('Copy to clipboard', '', @CopyToClipExecute);
    AddMenuItem('Paste from clipboard', '', @PasteFromClipExecute);
    AddMenuItem('-', '', nil);
    AddMenuItem('Copy query to clipboard as quoted string', '', @CopyToClipAsQuotedExecute);
    AddMenuItem('Paste quoted string as unquoted string', '', @PasteAsQuotedStringExecute);
  end;
  
  FMemoSQL.PopupMenu := pmEdit;
end;

procedure TEditorForm.SetReadOnly(const AValue: Boolean);
var
  SQLColor: TfpgColor;
begin
  FReadOnly         := AValue;
  FMemoSQL.ReadOnly := AValue;
end;

function TEditorForm.GetSQL: string;
begin
  if FMemoSQL.SelectionText <> '' then
    Result := FMemoSQL.SelectionText
  else
    Result := FMemoSQL.Lines.Text;
  Result := Trim(Result);
end;

procedure TEditorForm.Execute;
var
  lThrd: TthrdSQLMgrBrowse;
  lData: TSQLMgrQuery;
  lList: TList;
begin
  lList := TList.Create;
  try
    CloneData(lList);
    lData := TSQLMgrQuery(lList.Items[0]);
    lThrd := TthrdSQLMgrBrowse.Create(True);
    { the thread now takes ownership of lData }
    lThrd.SQLMgrQuery := lData;
    lThrd.Resume;
  finally
    lList.Free;
  end;
end;

procedure TEditorForm.ExecuteThenSave;
var
  lThrd: TthrdSQLMgrSave;
  lData: TSQLMgrQuery;
  lList: TList;
begin
  lList := TList.Create;
  try
    CloneData(lList);
    lData := TSQLMgrQuery(lList.Items[0]);
    lThrd := TthrdSQLMgrSave.Create(True);
    lThrd.SQLMgrQuery := lData;
    lThrd.Resume;
  finally
    lList.Free;
  end;
end;

procedure TEditorForm.LoadFromFile(pFileName: TFileName);
begin
  FMemoSQL.Lines.LoadFromFile(pFileName);
end;

procedure TEditorForm.SaveToFile(pFileName: TFileName);
begin
  FMemoSQL.Lines.SaveToFile(pFileName);
end;

procedure TEditorForm.Clear;
begin
  FMemoSQL.Lines.Clear;
  FDirty := False;
end;

constructor TEditorForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AfterCreate;
end;

procedure TEditorForm.GUItoBOM;
var
  ls: string;
begin
  ls := FMemoSQL.Lines.Text;

  if (FData <> nil) and
    (FData.SQL <> ls) then
  begin
    FData.SQL   := ls;
    FData.Dirty := True;
  end;

  FDirty := True;
end;

procedure TEditorForm.CloneData(const pList: TList);
var
  lData: TSQLMgrQuery;
begin
  pList.Clear;

  GUIToBOM;
  if FData <> nil then
    lData     := FData.Clone
  else
  begin
    lData     := TSQLMgrQuery.Create;
    lData.SQL := FMemoSQL.Text;
  end;

  if FMemoSQL.SelectionText <> '' then
    lData.SQL := FMemoSQL.SelectionText;

  pList.Add(lData);
end;

procedure TEditorForm.MemoSQLKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  case KeyCode of
    keyF8: 
    begin
      if ShiftState = [] then
      begin
        Execute;
        Consumed := True;
      end
      else if ShiftState = [ssAlt] then
      begin
        ExecuteThenSave;
        Consumed := True;
      end;
    end;

    else
    begin
      if (ShiftState = [ssCtrl]) and (KeyCode = keySpace) then
      begin
        DoShowPickDatabaseObjectWindow;
      end
      else
        GUItoBOM;
    end;
  end;
end;

procedure TEditorForm.CopyToClipAsQuotedExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
  lsl : TStringList ;
begin
  lsl := TStringList.Create ;
  try
    if FMemoSQL.SelectionText <> '' then
      lsl.Text := FMemoSQL.SelectionText
    else
      lsl.Assign( FMemoSQL.Lines ) ;

    ls := '' ;
    for i := 0 to lsl.Count - 1 do begin
      ls := ls + '    ' + QuotedStr( lsl.Strings[i] + ' '  ) + ' ' ;
      if i < lsl.Count - 1 then
        ls := ls + '+' + CrLf
      else
        ls := ls + ';' ;
    end ;
  finally
    lsl.Free ;
  end ;
  fpgClipboard.Text := ls ;
end;

procedure TEditorForm.PasteAsQuotedStringExecute(Sender: TObject);

  //----------------
  // Remove any quotions marks, end + signs or ending ; signs
  function _ParseLine(const ALine: string): string;
  const
    cApos = '&apos;';
  begin
    Result := Trim(ALine);
    if Copy(Result, Length(Result), 1) = '+' then
      Result := Copy(Result, 1, Length(Result)-1);
    if Copy(Result, Length(Result), 1) = ';' then
      Result := Copy(Result, 1, Length(Result)-1);
    Result := tiStrTran(Result, '''''', cApos);
    Result := tiStrTran(Result, '''', '');
    Result := tiStrTran(Result, cApos, '''');
  end;
  
var
  lsl : TStringList;
  i: Integer;
  BackupString: String;
begin
  lsl := TStringList.Create;
  BackupString := fpgClipboard.Text;  // save copy of original text
  try
    lsl.Text := fpgClipboard.Text;
    for i := 0 to lsl.Count - 1 do
      lsl.Strings[i] := _ParseLine(lsl.Strings[i]);
    Log(lsl.Text, lsDebug);
    fpgClipboard.Text := lsl.Text;
    FMemoSQL.PasteFromClipboard;
    GUItoBOM;
  finally
    fpgClipboard.Text := BackupString; // restore original text
    lsl.Free;
  end;
end;

procedure TEditorForm.CopyToClipExecute(Sender: TObject);
var
  ls : string ;
begin
  if FMemoSQL.SelectionText <> '' then
    ls := FMemoSQL.SelectionText
  else
    ls := FMemoSQL.Lines.Text ;
  fpgClipboard.Text := ls ;
end;

procedure TEditorForm.PasteFromClipExecute(Sender: TObject);
begin
  FMemoSQL.PasteFromClipboard;
end;

destructor TEditorForm.Destroy;
begin
  FFormPickDatabaseObject.Free;
  inherited Destroy;
end;

procedure TEditorForm.DoShowPickDatabaseObjectWindow;
var
  pt: TPoint;
  x, y: integer;
begin
  if Assigned(FFormPickDatabaseObject) then
    FFormPickDatabaseObject.Free;
  FFormPickDatabaseObject := TPickDatabaseObjectForm.Create(nil);
  { calculate screen position }
  x := FMemoSQL.Left + fpgStyle.GetControlFrameBorders.Left + (FMemoSQL.CursorPos * FMemoSQL.Font.TextWidth('o'));
  y := FMemoSQL.Top + fpgStyle.GetControlFrameBorders.Top + ((FMemoSQL.CursorLine+1) * FMemoSQL.LineHeight);
  pt := WindowToScreen(FMemoSQL, Point(Left, Bottom));
  
  FFormPickDatabaseObject.OnClose := @FormPickDatabaseObjectOnClose;
  { now finalyl show the window }
  FFormPickDatabaseObject.ShowAt(FMemoSQL, x, y);
end;

procedure TEditorForm.FormPickDatabaseObjectOnClose(Sender: TObject);
var
  lBackup: string;
begin
  gINI.WriteInteger( FFormPickDatabaseObject.Name, 'Action',  FFormPickDatabaseObject.ComboboxSelection) ;
  if FFormPickDatabaseObject.Text <> '' then
  begin
    lBackup := fpgClipboard.Text;
    fpgClipboard.Text := FFormPickDatabaseObject.Text;
    FMemoSQL.PasteFromClipboard;
    fpgClipboard.Text := lBackup;
    GUItoBOM;
  end;
end;

end.

