unit frm_scriptsequence;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, 
  Classes, 
  fpg_base, 
  fpg_main, 
  fpg_form,
  fpg_label,
  fpg_button,
  fpg_edit,
  fpg_memo,
  fpg_editbtn;

type

  TRunScriptSequenceForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: RunScriptSequenceForm}
    Label1: TfpgLabel;
    edtDirectory: TfpgDirectoryEdit;
    edtStart: TfpgEditInteger;
    lblStart: TfpgLabel;
    lblEnd: TfpgLabel;
    edtEnd: TfpgEditInteger;
    Memo1: TfpgMemo;
    btnCheck: TfpgButton;
    btnRun: TfpgButton;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: RunScriptSequenceForm}
    function    GetScripts: TStrings;
    procedure   btnCheckClicked(Sender: TObject);
    procedure   btnRunClicked(Sender: TObject);
    procedure   FormShow(Sender: TObject);
    procedure   FormClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;  
    procedure   AfterCreate; override;
    procedure   GenerateScriptList;
    property    Scripts: TStrings read GetScripts;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiConstants,
  tiUtils,
  tiINI,
  tiGUIINI,
  tiDialogs;
  
  
{@VFD_NEWFORM_IMPL}

procedure TRunScriptSequenceForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: RunScriptSequenceForm}
  Name := 'RunScriptSequenceForm';
  SetPosition(344, 149, 550, 401);
  WindowTitle := 'Script Sequence';
  Hint := '';

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 8, 312, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Script Directory';
  end;

  edtDirectory := TfpgDirectoryEdit.Create(self);
  with edtDirectory do
  begin
    Name := 'edtDirectory';
    SetPosition(8, 24, 536, 24);
    Anchors := [anLeft,anRight,anTop];
    Directory := '';
    ExtraHint := '';
    RootDirectory := '';
    TabOrder := 2;
  end;

  edtStart := TfpgEditInteger.Create(self);
  with edtStart do
  begin
    Name := 'edtStart';
    SetPosition(8, 72, 120, 24);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 3;
    Value := 0;
  end;

  lblStart := TfpgLabel.Create(self);
  with lblStart do
  begin
    Name := 'lblStart';
    SetPosition(8, 56, 196, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Starting sequence';
  end;

  lblEnd := TfpgLabel.Create(self);
  with lblEnd do
  begin
    Name := 'lblEnd';
    SetPosition(284, 60, 156, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Ending Sequence';
  end;

  edtEnd := TfpgEditInteger.Create(self);
  with edtEnd do
  begin
    Name := 'edtEnd';
    SetPosition(284, 76, 120, 24);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 6;
    Value := 0;
  end;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(8, 144, 388, 204);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 7;
  end;

  btnCheck := TfpgButton.Create(self);
  with btnCheck do
  begin
    Name := 'btnCheck';
    SetPosition(408, 144, 80, 24);
    Text := 'Check';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 8;
    OnClick := @btnCheckClicked;
  end;

  btnRun := TfpgButton.Create(self);
  with btnRun do
  begin
    Name := 'btnRun';
    SetPosition(356, 372, 92, 24);
    Text := 'Run';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 9;
    OnClick := @btnRunClicked;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(452, 372, 92, 24);
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 10;
  end;

  {@VFD_BODY_END: RunScriptSequenceForm}
  {%endregion}
end;

function TRunScriptSequenceForm.GetScripts: TStrings;
begin
  Result := Memo1.Lines;
end;

procedure TRunScriptSequenceForm.btnCheckClicked(Sender: TObject);
begin
  GenerateScriptList;
end;

procedure TRunScriptSequenceForm.btnRunClicked(Sender: TObject);
begin
  if edtEnd.Value < edtStart.Value then
  begin
    tiAppError('Ending sequence must be equal or larger than starting sequence number');
    Exit;
  end;
  GenerateScriptList;
  ModalResult := mrOK;
end;

procedure TRunScriptSequenceForm.GenerateScriptList;
var
  sl: TStringList;
  i: integer;
  s: string;
begin
  s := '';
  sl := TStringList.Create;
  Memo1.Clear;
  for i := edtStart.Value to edtEnd.Value do
  begin
    tiFilesToStringList(edtDirectory.Directory + Format('%3.3d', [i]), '*.sql', sl, True);
    Memo1.Text := Memo1.Text + s + sl.Text;
    s := cLineEnding;
  end;
  sl.Free;
end;

procedure TRunScriptSequenceForm.FormShow(Sender: TObject);
begin
  gGUIINI.ReadFormState(self);
  edtDirectory.Directory := gGUIINI.ReadString(name, 'ScriptDirectory', '');
  edtStart.Value := gGUIINI.ReadInteger(name, 'StartSequence', 1);
  edtEnd.Value := gGUIINI.ReadInteger(name, 'StopSequence', 1);
end;

procedure TRunScriptSequenceForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  gGUIINI.WriteFormState(self);
  gGUIINI.WriteString(name, 'ScriptDirectory', edtDirectory.Directory);
  gGUIINI.WriteInteger(name, 'StartSequence', edtStart.Value);
  gGUIINI.WriteInteger(name, 'StopSequence', edtEnd.Value);
end;

constructor TRunScriptSequenceForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow := @FormShow;
  OnClose := @FormClose;
end;


end.
