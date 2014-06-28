unit FtiEditFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  tiObject, tiReadOnly;

type
  TValidateEvent = procedure(Sender: TObject; Buffer: TtiObject;
    var Valid: Boolean) of object;
  TEditNotifyEvent = procedure(Sender: TObject; Buffer: TtiObject) of object;

  TtiEditFrame = class(TFrame)
  private
    { Private declarations }
    FData: TtiObject; //pointer
    FDataBuffer: TtiObject; //Owned object
    FReadOnly: Boolean;
    FOnValidate: TValidateEvent;
    FOnRollback: TEditNotifyEvent;
    FOnCommit: TEditNotifyEvent;
  protected
    property DataBuffer: TtiObject read FDataBuffer write FDataBuffer;
    procedure SetData(const AValue: TtiObject); virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
  public
    { Public declarations }
    function DataModified: Boolean; virtual;
    procedure CommitBuffer; virtual;
    procedure RollbackBuffer; virtual;
    destructor Destroy; override;
    function Validate: Boolean; virtual;
    property Data: TtiObject read FData write SetData;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnCommit: TEditNotifyEvent read FOnCommit write FOnCommit;
    property OnRollback: TEditNotifyEvent read FOnRollback write FOnRollback;
  end;

implementation

{$R *.dfm}

{ TtiEditFrame }

procedure TtiEditFrame.CommitBuffer;
begin
  FData.Assign(FDataBuffer);

  //GOTCHA: this can cause an Assert 'under construction' failure in TtiObject.SetDirty if ObjectState = posEmpty
  FData.Dirty := True;

  //setting Dirty:=True can alter the ObjectState and OID values, so these must be re-read into the buffer
  FDataBuffer.ObjectState := FData.ObjectState;
{$IFDEF OID_AS_INT64}
  DataBuffer.OID := FData.OID;
{$ELSE}

  //GOTCHA: cannot call the following, because it isn't guaranteed to be implemented
  //FDataBuffer.OID.Assign(FData.OID);
  FDataBuffer.OID.AsString := FData.OID.AsString;
{$ENDIF}
  if Assigned(FOnCommit) then
    FOnCommit(Self, FDataBuffer);
end;

function TtiEditFrame.DataModified: Boolean;
begin
  if Assigned(FData) then
    Result := (not (FData.Equals(FDataBuffer) and
      (FData.ObjectState = FDataBuffer.ObjectState)))
  else
    Result := False;
end;

function TtiEditFrame.Validate: Boolean;
begin
  if Assigned(FOnValidate) then
    FOnValidate(Self, FDataBuffer, Result)
  else
    Result := True;
end;

destructor TtiEditFrame.Destroy;
begin
  FDataBuffer.Free;
  inherited;
end;

procedure TtiEditFrame.RollbackBuffer;
begin
  if Assigned(FData) and Assigned(FDataBuffer) then
    FDatabuffer.Assign(FData);
  if Assigned(FOnRollback) then
    FOnRollback(Self, FDataBuffer);
end;

procedure TtiEditFrame.SetData(const AValue: TtiObject);
begin
  FData := AValue;
  FreeAndNil(FDataBuffer);
  if FData <> nil then
    FDataBuffer := FData.Clone;
end;

procedure TtiEditFrame.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

end.
