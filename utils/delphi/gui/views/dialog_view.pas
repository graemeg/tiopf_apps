unit dialog_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_dialog_view, StdCtrls, ComCtrls, Spin;

type
  TDialogView = class(TBaseDialogView)
    eValue: TEdit;
    btnCancel: TButton;
    dtpValue: TDateTimePicker;
    lblMsg: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    class function GetInteger(var AIntValue: Int64; const AMsg: string): Boolean;
    class function GetString(var AStringValue: string; const AMsg: string): Boolean;
    class function GetDouble(var ADoubleValue: Double; const AMsg: string): Boolean;
    class function GetDateTime(var ADataTime: TDateTime; const AMsg: string): Boolean;
    class function GetDate(var ADataTime: TDateTime; const AMsg: string): Boolean;
    class function GetTime(var ADataTime: TDateTime; const AMsg: string): Boolean;
  end;

var
  DialogView: TDialogView;

implementation

{$R *.dfm}

{ TDialogView }

procedure TDialogView.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDialogView.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

class function TDialogView.GetDate(var ADataTime: TDateTime;
  const AMsg: string): Boolean;
var
  lFrm: TDialogView;
begin
  lFrm := TDialogView.Create(nil);
  try
    lFrm.lblMsg.Caption := AMsg;
    lFrm.dtpValue.Visible := True;
    lFrm.dtpValue.Kind := dtkDate;
    lFrm.dtpValue.Date := ADataTime;
    Result := lFrm.ShowModal = mrOk;
    if Result then
      ADataTime := lFrm.dtpValue.Date;
  finally
    lFrm.Free;
  end;
end;

class function TDialogView.GetDateTime(var ADataTime: TDateTime;
  const AMsg: string): Boolean;
begin

end;

class function TDialogView.GetDouble(var ADoubleValue: Double;
  const AMsg: string): Boolean;
var
  lFrm: TDialogView;
begin
  lFrm := TDialogView.Create(nil);
  try
    lFrm.lblMsg.Caption := AMsg;
    lFrm.eValue.Visible := True;
    lFrm.eValue.Text := FloatToStr(ADoubleValue);
    Result := lFrm.ShowModal = mrOk;
    if Result then
      ADoubleValue := StrToFloat(lFrm.eValue.Text);
  finally
    lFrm.Free;
  end;
end;

class function TDialogView.GetInteger(var AIntValue: Int64;
  const AMsg: string): Boolean;
var
  lFrm: TDialogView;
begin
  lFrm := TDialogView.Create(nil);
  try
    lFrm.lblMsg.Caption := AMsg;
    lFrm.eValue.Visible := True;
    lFrm.eValue.Text := IntToStr(AIntValue);
    Result := lFrm.ShowModal = mrOk;
    if Result then
      AIntValue:= StrToInt64(lFrm.eValue.Text);
  finally
    lFrm.Free;
  end;

end;

class function TDialogView.GetString(var AStringValue: string;
  const AMsg: string): Boolean;
var
  lFrm: TDialogView;
begin
  lFrm := TDialogView.Create(nil);
  try
    lFrm.lblMsg.Caption := AMsg;
    lFrm.eValue.Visible := True;
    lFrm.eValue.Text := AStringValue;
    Result := lFrm.ShowModal = mrOk;
    if Result then
      AStringValue := lFrm.eValue.Text;
  finally
    lFrm.Free;
  end;

end;

class function TDialogView.GetTime(var ADataTime: TDateTime;
  const AMsg: string): Boolean;
begin

end;

end.
