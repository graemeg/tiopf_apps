unit input_query;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, EditBtn, ZVDateTimePicker;

type

  {: type of dialog. }
  TDialogType = (dtInput, dtList, dtDateTime, dtFloat);


  TInputQuery = class(TForm)
    ButtonPanel: TButtonPanel;
    CalcEdit: TCalcEdit;
    cboInput: TComboBox;
    eInput: TEdit;
    MessageLabel: TLabel;
    DateTimePicker: TZVDateTimePicker;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FDialogType: TDialogType;
    function IsValid: boolean;
    procedure SetDialogType(const AValue: TDialogType);
  public
    property    DialogType: TDialogType read FDialogType write SetDialogType;
    class function    GetInputAsString(var AValue: string; const AMessage: string; ADilalogType: TDialogType): boolean;
    class function    GetInputAsDouble(var AValue: Double; const AMessage: string): Boolean;
    class function    GetInputAsDateTime(var AValue: TDateTime; const AMessage: string): Boolean;
    class function    GetInputAsBoolean(const AMessage: String): boolean;
  end; 

var
  InputQuery: TInputQuery;

implementation

{$R *.lfm}

{ TInputQuery }

procedure TInputQuery.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

class function TInputQuery.GetInputAsBoolean(const AMessage: String): boolean;
var
  lForm: TInputQuery;
begin
  lForm := TInputQuery.Create(nil);
  try
    lForm.DialogType := dtInput;
    lForm.Height := lForm.Height - 30;
    lForm.MessageLabel.Caption := AMessage;
    result := lForm.ShowModal = mrOK;
  finally
    lForm.Free;
  end;

end;

class function TInputQuery.GetInputAsDateTime(var AValue: TDateTime;
  const AMessage: string): Boolean;
var
  lForm: TInputQuery;
begin
  lForm := TInputQuery.Create(nil);
  try
    lForm.DialogType := dtDateTime;
    lForm.MessageLabel.Caption := AMessage;
    lForm.DateTimePicker.Visible := true;
    result := lForm.ShowModal = mrOK;
    if result then
      AValue := lForm.DateTimePicker.DateTime;
  finally
    lForm.Free;
  end;
end;

class function TInputQuery.GetInputAsDouble(var AValue: Double; const AMessage: string): Boolean;
var
  lForm: TInputQuery;
begin
  lForm := TInputQuery.Create(nil);
  try
    lForm.DialogType := dtFloat;
    lForm.MessageLabel.Caption := AMessage;
    lForm.CalcEdit.Visible := true;
    result := lForm.ShowModal = mrOK;
    if result then
      AValue := lForm.CalcEdit.AsFloat;
  finally
    lForm.Free;
  end;

end;

class function TInputQuery.GetInputAsString(var AValue: string; const AMessage: string;
  ADilalogType: TDialogType): boolean;
var
  lForm: TInputQuery;
begin
  lForm := TInputQuery.Create(nil);
  try
    lForm.DialogType := dtInput;
    lForm.MessageLabel.Caption := AMessage;
    lForm.eInput.Visible := true;
    result := lForm.ShowModal = mrOK;
    if result then
      AValue := lForm.eInput.Text;
  finally
    lForm.Free;
  end;
end;

function TInputQuery.IsValid: boolean;
begin
  if DialogType = dtInput then
    result := eInput.Text <> ''
  else if DialogType = dtInput then
    result := cboInput.ItemIndex >= 0
  else if (DialogType = dtDateTime) or (DialogType = dtFloat) then
    result := true;
end;

procedure TInputQuery.OKButtonClick(Sender: TObject);
begin
  if IsValid then
    ModalResult := mrOK;
end;

procedure TInputQuery.SetDialogType(const AValue: TDialogType);
begin
  if FDialogType=AValue then exit;
  FDialogType:=AValue;
end;

end.

