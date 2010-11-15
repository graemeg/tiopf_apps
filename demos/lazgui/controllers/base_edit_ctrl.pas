unit base_edit_ctrl;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses
  SysUtils
  ,Classes
  ,Dialogs
  ,mvc_base
  ,tiObject
  ,base_edit_view
  ;

type

  TEditKind = (ekExisting, ekNew, ekSavedNew);

  {: Base controller for editing objects. }
  TBaseEditController = class(TMVCController)
  private
    FClone: TtiObject;
    FCloseAfterSave: Boolean;
    FEditState: TEditKind;
    procedure SetCloseAfterSave(const Value: Boolean);
    procedure SetEditState(const Value: TEditKind);
  protected
    procedure   CopyCloneToData;
    procedure   DoCreateCommands; override;
    procedure   DoCreateView; override;
    procedure   DoSaveObject(AObject: TtiObject); virtual; abstract;
    procedure   DoAfterModelSaved; virtual;
    procedure   DoCheckBeforeSave(var ACancel: Boolean; var AMsg: string); virtual;
    procedure   HandleSave(Sender: TObject); virtual;
    procedure   HandleCancel(Sender: TObject); virtual;
    procedure   HandleViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure   SetActive(const AValue: Boolean); override;
  public
    property    EditState: TEditKind read FEditState write SetEditState;
    property    CloseAfterSave: Boolean read FCloseAfterSave write SetCloseAfterSave;
    constructor Create(AModel: TObject; AView: TObject); override;
    destructor  Destroy; override;
  end;

  // -----------------------------------------------------------------
  //  Event names
  // -----------------------------------------------------------------

const
  DO_COMMIT_CHANGE = 'DoCommitChange';
  DO_CANCEL_CHANGE = 'DoCancelChange';
  DO_CANCEL_OP = 'DoCancelOp';

implementation

uses
  Forms
  ,Controls
  ,language_factory
  ,language_strings
  ;



{ TBaseEditController }

procedure TBaseEditController.CopyCloneToData;
begin
  Controllers.ChangeAllActive(false);
  try
    FModel.Free;
    FModel := FClone.Clone;
  finally
    if Active then
      Controllers.ChangeAllActive(True);
  end;
end;

constructor TBaseEditController.Create(AModel, AView: TObject);
begin
  inherited;
  if FClone <> nil then
    FreeAndNil(FClone);
  FClone := TtiObject(Model).Clone;

  case TtiObject(Model).ObjectState of
    posCreate: EditState := ekNew;
    posUpdate: EditState := ekExisting;
  end;
end;

destructor TBaseEditController.Destroy;
var
  lView: TBaseEditView;
begin
  if FClone <> nil then
    FClone.Free;

  // Manually detach native events
  lView := TBaseEditView(View);
  if Assigned(lView) then
    begin

      lView.Visible := False;
    end;

  inherited;
end;


procedure TBaseEditController.DoAfterModelSaved;
begin
  // nothing.  dummy method
end;

procedure TBaseEditController.DoCheckBeforeSave(var ACancel: Boolean;
  var AMsg: string);
begin
  // dummy method, fall through
end;

procedure TBaseEditController.DoCreateCommands;
begin
  inherited;
  // create registered commands
  gCmdManager.CreateCommands(Self);
end;

procedure TBaseEditController.DoCreateView;
begin
  inherited;
  with TBaseEditView(View) do
    begin
      Visible := True;
      btnCancel.OnClick := @self.HandleCancel;
      btnOK.OnClick := @self.HandleSave;
      btnSaveNew.OnClick := @self.HandleSave;

      OnKeyDown := @self.HandleViewKeyDown;
    end;

end;

procedure TBaseEditController.HandleCancel(Sender: TObject);
begin

  if EditState = ekSavedNew then
    gEventManager.DispatchEvent(gCreateQuickEvent(DO_COMMIT_CHANGE, Self))
  else
    begin
      TtiObject(FModel).Assign(FClone);
      gEventManager.DispatchEvent(gCreateQuickEvent(DO_CANCEL_OP, Self))
    end;

end;

procedure TBaseEditController.HandleSave(Sender: TObject);
var
  lCtrl: TMVCController;
  lMsg: string;
  lObj: TtiObject;
  lContinue: Boolean;
begin


  lContinue := True;
  DoCheckBeforeSave(lContinue, lMsg);
  if not lContinue then
    begin
      MessageDlg(lMsg, mtError, [mbOK], 0);
      exit;
    end;

  lObj := TtiObject(Model);

  if not lObj.IsValid(lMsg) then
    begin
      lMsg := GLang.GetLang(rsSaveChangesError);
      MessageDlg(lMsg, mtError, [mbOK], 0);
      exit;
    end;

  lObj.Dirty := True;

  try
    DoSaveObject(lObj);
    DoAfterModelSaved;
  except on e: Exception do
    begin
      MessageDlg(e.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;

  if (Sender = TBaseEditView(View).btnSaveNew) then
    begin
      gEventManager.DispatchEvent(gCreateQuickEvent(DO_COMMIT_CHANGE, self))
    end
  else
    begin
      EditState := ekSavedNew;
    end;

end;

procedure TBaseEditController.HandleViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //ShowMessage(IntToStr(Key));
  if (ssCtrl in Shift) then
    begin
      case Key of
        27: // close
        begin
          TBaseEditView(View).btnCancel.Click;
        end;
        69: // edit
        begin
          TBaseEditView(View).btnOK.Click;
        end;
        83: // save new
        begin
          TBaseEditView(View).btnSaveNew.Click;
        end;
      end;
    end;

 // key := 0;
end;

procedure TBaseEditController.SetActive(const AValue: Boolean);
begin
  inherited;
  TBaseEditView(View).Visible := Active;
end;

procedure TBaseEditController.SetCloseAfterSave(const Value: Boolean);
begin
  FCloseAfterSave := Value;
end;

procedure TBaseEditController.SetEditState(const Value: TEditKind);
begin
  FEditState := Value;
end;

end.
