unit FtiSelectBOMFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ActnList,
  tiObject, tiFocusPanel, tiPerAwareCtrls, tiVTListView,
  tiBOMExploration, tiVTAbstract;

type
  TtiSelectBOMFrame = class(TFrame)
    gbBOM: TGroupBox;
    cbBOM: TComboBox;
    lblBOM: TLabel;
    imgBOMIcon: TImage;
    lvAliases: TtiVTListView;
    imgAliasIcon: TImage;
    ilRegistrar: TImage;
    lblAliases: TLabel;
    LVRegistrars: TtiVTListView;
    lblRegistrars: TLabel;
    btnAddAlias: TButton;
    ActionList: TActionList;
    actAddAlias: TAction;
    procedure actAddAliasExecute(Sender: TObject);
    procedure actAddAliasUpdate(Sender: TObject);
    procedure cbBOMChange(Sender: TObject);
  private
    procedure FindAliasName(AObject: TtiObject; var AFound: Boolean;
      AUserContext: Pointer);
    function GetAlias: TtiAlias;
    function GetBOM: TtiExplorableBOM;
    function GetRegistrar: TtiBOMERegistrar;
    procedure SetSelectedAlias(const Value: TtiAlias);
    procedure SetSelectedBOM(const Value: TtiExplorableBOM);
    procedure SetSelectedRegistrar(const Value: TtiBOMERegistrar);
    procedure ClearAll;
    procedure ApplyResourceStrings;
    procedure ReadBOMData;
    procedure ReadBOMs;
  public
    procedure Init;
    function Validate: Boolean;
    constructor Create(AOwner: TComponent); override;
    property BOM: TtiExplorableBOM read GetBOM write SetSelectedBOM;
    property Alias: TtiAlias read GetAlias write SetSelectedAlias;
    property Registrar: TtiBOMERegistrar read GetRegistrar write SetSelectedRegistrar;
  end;

implementation

uses
  FtiDefineAlias;

resourcestring
  SlblBOMCaption = 'BOMs:';
  SlblAliasCaption = 'Aliases:';
  SlblRegistrarsCaption = 'Registrars:';
  SgbBOMCaption = 'Business Object Models';
  SaddAliasCaption = 'Add Alias...';
  SCustom = 'Custom';

{$R *.dfm}

{ TtiSelectBOMFrame }

procedure TtiSelectBOMFrame.actAddAliasExecute(Sender: TObject);
var
  Dialog: TtiDefineAlias;
  NewAlias: TtiAlias;
  CustomCount: Integer;
  Found: Boolean;
begin
  Dialog := TtiDefineAlias.Create(Self);
  try
    if Dialog.Execute then
    begin
      NewAlias := TtiAlias.Create;
      CustomCount := 0;
      repeat
        Inc(CustomCount);
        NewAlias.Name := SCustom + IntToStr(CustomCount);
        Found := Assigned(BOM.Aliases.Find(FindAliasName, NewAlias));
      until not Found;
      NewAlias.DatabaseName := Dialog.DatabaseName;
      NewAlias.PerLayerName := Dialog.PersistenceLayerName;
      NewAlias.UserName := Dialog.UserName;
      NewAlias.Password := Dialog.Password;
      BOM.Aliases.Add(NewAlias);

      //select just-created Alias
      LVAliases.Data := nil;
      LVAliases.Data := BOM.Aliases;
      LVAliases.SelectedData := NewAlias;

    end;
  finally
    Dialog.Free;
  end;
end;

procedure TtiSelectBOMFrame.actAddAliasUpdate(Sender: TObject);
begin
  actAddAlias.Enabled := Assigned(BOM);
end;

procedure TtiSelectBOMFrame.ApplyResourceStrings;
begin
  lblBOM.Caption := SlblBOMCaption;
  gbBOM.Caption := SgbBOMCaption;
  lblAliases.Caption := SlblAliasCaption;
  lblRegistrars.Caption := SlblRegistrarsCaption;
  actAddAlias.Caption := SaddAliasCaption;
end;

procedure TtiSelectBOMFrame.cbBOMChange(Sender: TObject);
begin
  ReadBOMData;
end;

procedure TtiSelectBOMFrame.ClearAll;
begin
  cbBOM.Items.Clear;
  LVAliases.Data := nil;
  LVRegistrars.Data := nil;
end;

constructor TtiSelectBOMFrame.Create(AOwner: TComponent);
begin
  inherited;
  ApplyResourceStrings;
  LVAliases.AddColumn('Name', vttkString, 'Name', 130);
  LVAliases.AddColumn('DatabaseName', vttkString, 'Database Name', 200);
  LVAliases.AddColumn('PerLayerName', vttkString, 'Persistence Layer', 80);
  LVAliases.AddColumn('UserName', vttkString, 'User Name', 80);
  LVAliases.AddColumn('Password', vttkString, 'Password', 80);
  LVAliases.AddColumn('Params', vttkString, 'Params', 200);
  LVRegistrars.AddColumn('Name', vttkString, 'Name', 200);
  LVRegistrars.AddColumn('Kind', vttkString, 'Kind', 100);
  LVRegistrars.AddColumn('Registered', vttkString, 'Registered', 80);
end;

procedure TtiSelectBOMFrame.FindAliasName(AObject: TtiObject;
  var AFound: Boolean; AUserContext: Pointer);
begin
  AFound := TtiAlias(AObject).Name = TtiAlias(AUserContext).Name;
end;

function TtiSelectBOMFrame.GetAlias: TtiAlias;
begin
  Result := TtiAlias(LVAliases.SelectedData);
end;

function TtiSelectBOMFrame.GetBOM: TtiExplorableBOM;
begin
  if (cbBOM.ItemIndex >= 0) and (cbBOM.ItemIndex <= BOMEDictionary.BOMs.Count - 1) then
    Result := BOMEDictionary.BOMs[cbBOM.ItemIndex]
  else
    Result := nil;
end;

function TtiSelectBOMFrame.GetRegistrar: TtiBOMERegistrar;
begin
  Result := TtiBOMERegistrar(LVRegistrars.SelectedData);
end;

procedure TtiSelectBOMFrame.Init;
begin
  ReadBOMs;
end;

procedure TtiSelectBOMFrame.ReadBOMData;
begin
  if Assigned(BOM) then
  begin
    LVAliases.Data := BOM.Aliases;
    if BOM.Aliases.Count > 0 then
      LVAliases.SelectedData := BOM.Aliases[0];
    LVRegistrars.Data := BOM.Registrars;
    if BOM.Registrars.Count > 0 then
      LVRegistrars.SelectedData := BOM.Registrars[0];
  end;
end;

procedure TtiSelectBOMFrame.ReadBOMs;
var
  I: Integer;
begin
  ClearAll;
  if BOMEDictionary.BOMs.Count < 1 then
    Exit;
  for I := 0 to BOMEDictionary.BOMs.Count - 1 do
    cbBOM.Items.AddObject(BOMEDictionary.BOMs[I].Name, BOMEDictionary.BOMs[I]);
  cbBOM.ItemIndex := 0;
  ReadBOMData;
end;

procedure TtiSelectBOMFrame.SetSelectedAlias(const Value: TtiAlias);
begin
  LVAliases.SelectedData := Value;
end;

procedure TtiSelectBOMFrame.SetSelectedBOM(const Value: TtiExplorableBOM);
var
  Index: Integer;
begin
  Index := cbBOM.Items.IndexOfObject(Value);
  if (Index >= 0) and (Index <> cbBOM.ItemIndex) then
  begin
    cbBOM.ItemIndex := Index;
    ReadBOMData;
  end;
end;

procedure TtiSelectBOMFrame.SetSelectedRegistrar(const Value: TtiBOMERegistrar);
begin
  LVRegistrars.SelectedData := Value;
end;

function TtiSelectBOMFrame.Validate: Boolean;
begin
  Result := (cbBOM.ItemIndex >= 0) and Assigned(LVAliases.SelectedData) and
    Assigned(LVRegistrars.SelectedData);
end;

end.
