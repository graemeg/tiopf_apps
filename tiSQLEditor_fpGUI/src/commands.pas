{
  Here we define some commands that can be reused throughout a application.
  Command actions are kept separate from the UI code (Forms).
}
unit commands;

{$mode objfpc}{$H+}

interface

uses
  fpg_command_intf,
  fpg_memo,
  fpg_form,
  frm_editor;

type
  // non reference counted interface
  TNullInterfacedObject = class(TObject)
  protected
    function QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
    function _AddRef: longint; stdcall;
    function _Release: longint; stdcall;
  end;


  TExitCommand = class(TInterfacedObject, ICommand)
  public
    procedure Execute;
  end;


  TAboutfpGUICommand = class(TInterfacedObject, ICommand)
  public
    procedure Execute;
  end;


  TAboutCommand = class(TInterfacedObject, ICommand)
  public
    procedure Execute;
  end;


  TRunSQLCommand = class(TInterfacedObject, ICommand)
  private
    FForm: TEditorForm;
  public
    constructor Create(AEditorForm: TEditorForm); reintroduce;
    procedure Execute;
  end;


  TRunSQLAndSaveCommand = class(TInterfacedObject, ICommand)
  private
    FForm: TEditorForm;
  public
    constructor Create(AEditorForm: TEditorForm); reintroduce;
    procedure Execute;
  end;


  TDatabaseConnectionDetailsCommand = class(TInterfacedObject, ICommand)
  public
    procedure Execute;
  end;
  
  
  TSQLCloseCommand = class(TInterfacedObject, ICommand)
  private
    FForm: TfpgForm;
  public
    constructor Create(AForm: TfpgForm); reintroduce;
    procedure Execute;
  end;


implementation

uses
  SysUtils,
  fpg_main,
  fpg_dialogs,
  tiOPFManager,
  tiDialogs;

{ TNullInterfacedObject }

function TNullInterfacedObject.QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := integer(e_nointerface);
end;

function TNullInterfacedObject._AddRef: longint; stdcall;
begin
  Result := -1;
end;

function TNullInterfacedObject._Release: longint; stdcall;
begin
  Result := -1;
end;

{ TExitCommand }

procedure TExitCommand.Execute;
begin
  fpgApplication.Terminated := True;
end;

{ TAboutfpGUICommand }

procedure TAboutfpGUICommand.Execute;
begin
  TfpgMessageDialog.AboutFPGUI;
end;

{ TAboutCommand }

procedure TAboutCommand.Execute;
begin
  TfpgMessageDialog.Information('About tiSQLEditor', 
      'Originally written by Peter Hinrichsen - 2000' + LineEnding +
      'Ported to fpGUI and tiOPF2 by Graeme Geldenhuys - 2011');
end;

{ TRunSQLCommand }

constructor TRunSQLCommand.Create(AEditorForm: TEditorForm);
begin
  inherited Create;
  FForm := AEditorForm;
end;

procedure TRunSQLCommand.Execute;
begin
  FForm.Execute;
end;

{ TDatabaseConnectionDetailsCommand }

procedure TDatabaseConnectionDetailsCommand.Execute;
var
  ls: string;
begin
  ls := gTIOPFManager.DefaultPerLayer.DBConnectionPools.DetailsAsString;
  tiShowMessage(ls);
end;

{ TRunSQLAndSaveCommand }

constructor TRunSQLAndSaveCommand.Create(AEditorForm: TEditorForm);
begin
  inherited Create;
  FForm := AEditorForm;
end;

procedure TRunSQLAndSaveCommand.Execute;
begin
  FForm.ExecuteThenSave;
end;

{ TSQLCloseCommand }

constructor TSQLCloseCommand.Create(AForm: TfpgForm);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TSQLCloseCommand.Execute;
begin
  FForm.Close;
end;

end.

