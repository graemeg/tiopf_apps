unit FtiSelectBOMDlg;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,
  FtiSelectBOMFrame, tiBOMExploration, ActnList;

type
  TtiSelectBOMDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    pnlFrameHolder: TPanel;
    ActionList: TActionList;
    actOK: TAction;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Frame: TtiSelectBOMFrame;
    procedure AssignResourceStrings;
  public
    function Execute(var ABOM: TtiExplorableBOM; var AAlias: TtiAlias;
      var ARegistrar: TtiBOMERegistrar): Boolean;
  end;

var
  tiSelectBOMDlg: TtiSelectBOMDlg;

implementation

{$R *.dfm}

resourcestring
  SCaption = 'Select a Business Object Model';
  SactOKCaption = 'OK';
  SCancelBtnCaption = 'Cancel';

procedure TtiSelectBOMDlg.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TtiSelectBOMDlg.actOKUpdate(Sender: TObject);
begin
  actOK.Enabled := Frame.Validate;
end;

procedure TtiSelectBOMDlg.AssignResourceStrings;
begin
  Caption := SCaption;
  actOK.Caption := SactOKCaption;
  CancelBtn.Caption := SCancelBtnCaption;
end;

function TtiSelectBOMDlg.Execute(var ABOM: TtiExplorableBOM;
  var AAlias: TtiAlias; var ARegistrar: TtiBOMERegistrar): Boolean;
begin
  Frame.Init;
  if Assigned(ABOM) then
  begin
    Frame.BOM := ABOM;
    if Assigned(AAlias) then
      Frame.Alias := AAlias;
    if Assigned(ARegistrar) then
      Frame.Registrar := ARegistrar;
  end;
  Result := ShowModal = mrOk;
  if Result then
  begin
    ABOM := Frame.BOM;
    AAlias := Frame.Alias;
    ARegistrar := Frame.Registrar;
  end;
end;

procedure TtiSelectBOMDlg.FormCreate(Sender: TObject);
begin
  AssignResourceStrings;
  Frame := TtiSelectBOMFrame.Create(Self);
  Frame.Parent := pnlFrameHolder;
  Frame.Align := alClient;
  Frame.Show;
end;

end.
