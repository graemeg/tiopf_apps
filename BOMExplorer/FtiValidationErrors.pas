unit FtiValidationErrors;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,
  tiObject;

type
  TtiValidationErrorsDlg = class(TForm)
    OKBtn: TButton;
    Bevel1: TBevel;
    lblMessage: TLabel;
    mmErrors: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    procedure ApplyStrings;
  public
    class procedure Execute(AData: TtiObject; ErrorMsgs: TStrings);
  end;

var
  tiValidationErrorsDlg: TtiValidationErrorsDlg;

implementation

{$R *.dfm}

resourcestring
  SCaption = 'Validation Errors';
  SlblMessageCaption = 'The following errors occurred when validating "%s":';
  SOKBtnCaption = 'OK';

procedure TtiValidationErrorsDlg.ApplyStrings;
begin
  Caption := SCaption;
  lblMessage.Caption := SlblMessageCaption;
  OKBtn.Caption := SOKBtnCaption;
end;

class procedure TtiValidationErrorsDlg.Execute(AData: TtiObject; ErrorMsgs: TStrings);
var
  Dialog: TtiValidationErrorsDlg;
begin
  Dialog := Create(nil);
  try
    Dialog.lblMessage.Caption := Format(SlblMessageCaption, [AData.Caption]);
    Dialog.mmErrors.Lines := ErrorMsgs;
    Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;

procedure TtiValidationErrorsDlg.FormCreate(Sender: TObject);
begin
  ApplyStrings;
end;

end.
