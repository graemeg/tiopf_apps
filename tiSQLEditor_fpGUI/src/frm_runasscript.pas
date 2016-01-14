{
  Run an adhoc query as a script - good for multiple queries, separated by ;s
  Bit of a hack though to get multiple queries running.

  Classes:
    TFormRunAsScript - The form

  ToDo:
  Remove the need to shell out to a script engine. Better to run the quesies
  from within the SQLManager process.
}
unit frm_runasscript;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, 
  Classes, 
  fpg_base, 
  fpg_main, 
  fpg_form, 
  fpg_edit,
  fpg_editbtn,
  fpg_label,
  fpg_button,
  fpg_memo;

type

  TRunAsScriptForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: RunAsScriptForm}
    edtProgram: TfpgFileNameEdit;
    edtParams: TfpgEdit;
    lblProgram: TfpgLabel;
    lblParams: TfpgLabel;
    btnOk: TfpgButton;
    btnCancel: TfpgButton;
    Memo1: TfpgMemo;
    {@VFD_HEAD_END: RunAsScriptForm}
    function GetAppToRun: string;
    function GetParams: string;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
    property AppToRun : string read GetAppToRun ;
    property Params   : string read GetParams ;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiGUIINI;

const
  cDefaultExe = '/path/to/isql';
  cDefaultParams = '-i "%ScriptFileName%" %DatabaseName% -u %UserName% -p %Password%';

{@VFD_NEWFORM_IMPL}

procedure TRunAsScriptForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: RunAsScriptForm}
  Name := 'RunAsScriptForm';
  SetPosition(356, 186, 472, 235);
  WindowTitle := 'Run As Script';
  Hint := '';
  MinWidth := 250;
  MinHeight := 170;

  edtProgram := TfpgFileNameEdit.Create(self);
  with edtProgram do
  begin
    Name := 'edtProgram';
    SetPosition(8, 24, 452, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FileName := '';
    Filter := '';
    InitialDir := '';
    TabOrder := 1;
  end;

  edtParams := TfpgEdit.Create(self);
  with edtParams do
  begin
    Name := 'edtParams';
    SetPosition(8, 72, 452, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := '';
  end;

  lblProgram := TfpgLabel.Create(self);
  with lblProgram do
  begin
    Name := 'lblProgram';
    SetPosition(8, 8, 324, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Program';
  end;

  lblParams := TfpgLabel.Create(self);
  with lblParams do
  begin
    Name := 'lblParams';
    SetPosition(8, 56, 400, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Parameters';
  end;

  btnOk := TfpgButton.Create(self);
  with btnOk do
  begin
    Name := 'btnOk';
    SetPosition(276, 205, 92, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 5;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(372, 205, 92, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 6;
  end;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(8, 104, 452, 89);
    FontDesc := '#Edit1';
    Hint := '';
    Lines.Add('Available parameters are:');
    Lines.Add('  %ScriptFileName%');
    Lines.Add('  %UserName%');
    Lines.Add('  %Password%');
    Lines.Add('  %DatabaseName%');
    Lines.Add('');
    Lines.Add('It seems ISQL is fussy about the order of parameters. This is what');
    Lines.Add('works here:');
    Lines.Add('    isql -i <script> <database> -u <user> -p <passwd>');
    TabOrder := 7;
    ReadOnly := True;
  end;

  {@VFD_BODY_END: RunAsScriptForm}
  {%endregion}
end;

function TRunAsScriptForm.GetAppToRun: string;
begin
  result := edtProgram.Filename;
end;

function TRunAsScriptForm.GetParams: string;
begin
  result := edtParams.Text;
end;

procedure TRunAsScriptForm.FormShow(Sender: TObject);
begin
  gGUIINI.ReadFormState(self);
  edtProgram.Filename := gGUIINI.ReadString(name, 'AppToRun', cDefaultExe);
  edtParams.Text := gGUIINI.ReadString(name, 'Params', cDefaultParams);
end;

constructor TRunAsScriptForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow := @FormShow;
  OnClose := @FormClose;
end;

procedure TRunAsScriptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  gGUIINI.WriteFormState(self);
  gGUIINI.WriteString(name, 'AppToRun', edtProgram.Filename);
  gGUIINI.WriteString(name, 'Params',   edtParams.Text);
end;


end.
