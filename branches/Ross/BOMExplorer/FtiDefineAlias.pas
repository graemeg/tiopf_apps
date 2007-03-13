unit FtiDefineAlias;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FPickDatabase, ActnList, Menus, ExtCtrls, tiFocusPanel,
  tiPerAwareCtrls, Buttons, tiSpeedButton, StdCtrls;

type
  TtiDefineAlias = class(TFormPickDatabase)
    btnCancel: TButton;
    btnOK: TButton;
    actOK: TAction;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
  private
    procedure ApplyResourceStrings;
  public
    function Validate: Boolean;
    function Execute: Boolean;
  end;

var
  tiDefineAlias: TtiDefineAlias;

implementation

resourcestring
  SactOKCaption = 'OK';
  SbtnCancelCaption = 'Cancel';

{$R *.dfm}

procedure TtiDefineAlias.actOKExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrOk;
end;

procedure TtiDefineAlias.actOKUpdate(Sender: TObject);
begin
  inherited;
  actOK.Enabled := Validate;
end;

procedure TtiDefineAlias.ApplyResourceStrings;
begin
  actOK.Caption := SactOKCaption;
  btnCancel.Caption := SbtnCancelCaption;
end;

procedure TtiDefineAlias.btnCancelClick(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
end;

function TtiDefineAlias.Execute: Boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TtiDefineAlias.FormCreate(Sender: TObject);
begin
  inherited;
  ApplyResourceStrings;
end;

function TtiDefineAlias.Validate: Boolean;
begin
  Result := (paePersistenceLayer.Value <> '') and (paeDatabaseName.Value <> '') and
    (paeUserName.Value <> '') and (paePassword.Value <> '');
end;

end.
