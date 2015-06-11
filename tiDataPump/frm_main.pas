unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_panel,
  fpg_label,
  fpg_memo,
  fpg_edit,
  fpg_combobox
  ;

type

  TMainForm = class(TfpgForm)
    procedure FormDestroy(Sender: TObject);
  private
    {@VFD_HEAD_BEGIN: MainForm}
    GroupBox1: TfpgGroupBox;
    GroupBox2: TfpgGroupBox;
    btnCopyData: TfpgButton;
    memoLog: TfpgMemo;
    memoTables: TfpgMemo;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    Label4: TfpgLabel;
    cbSourcePL: TfpgComboBox;
    edtSourceDatabaseName: TfpgEdit;
    edtSourceUsername: TfpgEdit;
    edtSourcePassword: TfpgEdit;
    Label21: TfpgLabel;
    Label22: TfpgLabel;
    Label23: TfpgLabel;
    Label24: TfpgLabel;
    cbTargetPL: TfpgComboBox;
    edtTargetDatabaseName: TfpgEdit;
    edtTargetUsername: TfpgEdit;
    edtTargetPassword: TfpgEdit;
    Label5: TfpgLabel;
    Bevel1: TfpgBevel;
    lblStatus: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    procedure FormCreate(Sender: TObject);
    procedure btnCopyDataClicked(Sender: TObject);
    procedure UpdateProgress(const pMessage: string);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiINI
  ,tiDataPump_BOM
  ,tiOPFManager
  ,tiLog
  ;


{@VFD_NEWFORM_IMPL}

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  gINI.WriteInteger('Source', 'perlayer', cbSourcePL.FocusItem);
  gINI.WriteString('Source', 'dbname', edtSourceDatabaseName.Text);
  gINI.WriteString('Source', 'username', edtSourceUsername.Text);
  gINI.WriteString('Source', 'password', edtSourcePassword.Text);

  gINI.WriteInteger('Target', 'perlayer', cbTargetPL.FocusItem);
  gINI.WriteString('Target', 'dbname', edtTargetDatabaseName.Text);
  gINI.WriteString('Target', 'username', edtTargetUsername.Text);
  gINI.WriteString('Target', 'password', edtTargetPassword.Text);

  gINI.EraseSection('SourceTables');  // clear any previous data in case count differs
  gINI.WriteInteger('SourceTables', 'count', memoTables.Lines.Count);
  for i := 1 to memoTables.Lines.Count do
    gINI.WriteString('SourceTables', Format('item%2.2d', [i]), memoTables.Lines[i-1]);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  c, i: integer;
begin
  gTIOPFManager.PersistenceLayers.AssignCaptions(cbSourcePL.Items);
  gTIOPFManager.PersistenceLayers.AssignCaptions(cbTargetPL.Items);

  gINI('tidatapump.ini');
  cbSourcePL.FocusItem := gINI.ReadInteger('Source', 'perlayer', -1);
  edtSourceDatabaseName.Text := gINI.ReadString('Source', 'dbname', '');
  edtSourceUsername.Text := gINI.ReadString('Source', 'username', '');
  edtSourcePassword.Text := gINI.ReadString('Source', 'password', '');

  cbTargetPL.FocusItem := gINI.ReadInteger('Target', 'perlayer', -1);
  edtTargetDatabaseName.Text := gINI.ReadString('Target', 'dbname', '');
  edtTargetUsername.Text := gINI.ReadString('Target', 'username', '');
  edtTargetPassword.Text := gINI.ReadString('Target', 'password', '');

  memoTables.Clear;
  c := gINI.ReadInteger('SourceTables', 'count', 0);
  for i := 1 to c do
    memoTables.Lines.Add(gINI.ReadString('SourceTables', Format('item%2.2d', [i]), ''));
end;

procedure TMainForm.btnCopyDataClicked(Sender: TObject);
var
  lDP : TtiDataPump ;
begin
  lDP := TtiDataPump.Create ;
  try
    lDP.SourcePerLayerName := cbSourcePL.Text;
    lDP.SourceDatabaseName := edtSourceDatabaseName.Text;
    lDP.SourceUserName := edtSourceUserName.Text;
    lDP.SourcePassword := edtSourcePassword.Text;

    lDP.TargetPerLayerName := cbTargetPL.Text;
    lDP.TargetDatabaseName := edtTargetDatabaseName.Text;
    lDP.TargetUserName := edtTargetUserName.Text;
    lDP.TargetPassword := edtTargetPassword.Text;

    lDP.Tables.Assign(memoTables.Lines);

    lDP.OnUpdateProgress := @UpdateProgress;

    lDP.Execute;
  finally
    lDP.Free;
  end;
end;

procedure TMainForm.UpdateProgress(const pMessage: string);
begin
//  memoLog.Lines.Add(pMessage);
  Log(pMessage);
  lblStatus.Text := pMessage;
  fpgApplication.ProcessMessages;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(1583, 169, 649, 507);
  WindowTitle := 'tiOPF Data Pump';
  Hint := '';
  WindowPosition := wpScreenCenter;
  OnCreate := @FormCreate;
  OnDestroy := @FormDestroy;

  GroupBox1 := TfpgGroupBox.Create(self);
  with GroupBox1 do
  begin
    Name := 'GroupBox1';
    SetPosition(12, 12, 256, 245);
    FontDesc := '#Label1';
    Hint := '';
    Margin := 4;
    Text := 'Source';
  end;

  GroupBox2 := TfpgGroupBox.Create(self);
  with GroupBox2 do
  begin
    Name := 'GroupBox2';
    SetPosition(384, 12, 256, 245);
    FontDesc := '#Label1';
    Hint := '';
    Margin := 4;
    Text := 'Target';
  end;

  btnCopyData := TfpgButton.Create(self);
  with btnCopyData do
  begin
    Name := 'btnCopyData';
    SetPosition(276, 116, 100, 23);
    Text := 'Copy Data >>';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnCopyDataClicked;
  end;

  memoLog := TfpgMemo.Create(self);
  with memoLog do
  begin
    Name := 'memoLog';
    SetPosition(12, 268, 628, 213);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 4;
  end;

  memoTables := TfpgMemo.Create(GroupBox1);
  with memoTables do
  begin
    Name := 'memoTables';
    SetPosition(8, 148, 241, 90);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
  end;

  Label1 := TfpgLabel.Create(GroupBox1);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 24, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Persistence Layer';
  end;

  Label2 := TfpgLabel.Create(GroupBox1);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 52, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Database Name';
  end;

  Label3 := TfpgLabel.Create(GroupBox1);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 80, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'User Name';
  end;

  Label4 := TfpgLabel.Create(GroupBox1);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(8, 108, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Password';
  end;

  cbSourcePL := TfpgComboBox.Create(GroupBox1);
  with cbSourcePL do
  begin
    Name := 'cbSourcePL';
    SetPosition(128, 20, 120, 24);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 6;
  end;

  edtSourceDatabaseName := TfpgEdit.Create(GroupBox1);
  with edtSourceDatabaseName do
  begin
    Name := 'edtSourceDatabaseName';
    SetPosition(128, 48, 120, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 7;
    Text := '';
  end;

  edtSourceUsername := TfpgEdit.Create(GroupBox1);
  with edtSourceUsername do
  begin
    Name := 'edtSourceUsername';
    SetPosition(128, 76, 120, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 8;
    Text := '';
  end;

  edtSourcePassword := TfpgEdit.Create(GroupBox1);
  with edtSourcePassword do
  begin
    Name := 'edtSourcePassword';
    SetPosition(128, 104, 120, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 9;
    Text := '';
  end;

  Label21 := TfpgLabel.Create(GroupBox2);
  with Label21 do
  begin
    Name := 'Label21';
    SetPosition(8, 24, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Persistence Layer';
  end;

  Label22 := TfpgLabel.Create(GroupBox2);
  with Label22 do
  begin
    Name := 'Label22';
    SetPosition(8, 52, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Database Name';
  end;

  Label23 := TfpgLabel.Create(GroupBox2);
  with Label23 do
  begin
    Name := 'Label23';
    SetPosition(8, 80, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'User Name';
  end;

  Label24 := TfpgLabel.Create(GroupBox2);
  with Label24 do
  begin
    Name := 'Label24';
    SetPosition(8, 108, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Password';
  end;

  cbTargetPL := TfpgComboBox.Create(GroupBox2);
  with cbTargetPL do
  begin
    Name := 'cbTargetPL';
    SetPosition(128, 20, 120, 24);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 6;
  end;

  edtTargetDatabaseName := TfpgEdit.Create(GroupBox2);
  with edtTargetDatabaseName do
  begin
    Name := 'edtTargetDatabaseName';
    SetPosition(128, 48, 120, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 7;
    Text := '';
  end;

  edtTargetUsername := TfpgEdit.Create(GroupBox2);
  with edtTargetUsername do
  begin
    Name := 'edtTargetUsername';
    SetPosition(128, 76, 120, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 8;
    Text := '';
  end;

  edtTargetPassword := TfpgEdit.Create(GroupBox2);
  with edtTargetPassword do
  begin
    Name := 'edtTargetPassword';
    SetPosition(128, 104, 120, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 9;
    Text := '';
  end;

  Label5 := TfpgLabel.Create(GroupBox1);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(8, 132, 80, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Tables:';
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(0, 487, 648, 19);
    Anchors := [anLeft,anRight,anBottom];
    Hint := '';
    Style := bsLowered;
  end;

  lblStatus := TfpgLabel.Create(Bevel1);
  with lblStatus do
  begin
    Name := 'lblStatus';
    SetPosition(5, 2, 636, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.
