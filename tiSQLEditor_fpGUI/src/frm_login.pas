unit frm_login;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
interface

uses
  Classes,
  fpg_base,
  fpg_dialogs;

type
  TLoginForm = class(TfpgPromptUserDbDialog)
  private
    procedure   GetAvailableConnections(out ANames: TStringList; out AConnections: TStringList);
          procedure   PasswordKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
  protected
    function    GetSelectedDatabase: TfpgString; override;
    procedure   PopulateComboDb; override;
  public
    constructor Create(AOwner: TComponent); override;
    function    Authenticate: boolean; override;
  end;
   
implementation

uses
  SysUtils,
  inifiles,
  tiGUIINI,
  tiOPFManager,
  tiDialogs,
  tiUtils;
  
const
  cConnections = 'connections.ini';  
  
{ TLoginForm }

procedure TLoginForm.PopulateComboDb;
var
  lNames: TStringList;
begin
  inherited PopulateComboDb;
  try
    aStringList.Free; // The next function will create a new instance for us
    GetAvailableConnections(lNames, aStringList);
    cbDatabases.Items.Assign(lNames);
    cbDatabases.FocusItem := gGUIINI.ReadInteger('login', 'database', 0);
  finally
    lNames.Free;
  end;

  { also setup the last used username }
  UserID := gGUIINI.ReadString( 'login', 'username', '');
  if UserID <> '' then
    edPassword.SetFocus;
end;

constructor TLoginForm.Create(AOwner: TComponent);
begin
        inherited Create(AOwner);
        edPassword.OnKeyPress  := @PasswordKeyPressed;
end;

function TLoginForm.Authenticate: boolean;
begin
  // Connect to the appropriate database using -d -u and -p command line params.
  // persistence layer linked in via a compiler directive. eg: LINK_FBL = Firebird FBLib
  try
//    FConnections.ValueFromIndex[cbDatabase.FocusItem],  { Database }
    gTIOPFManager.ConnectDatabase(cbDatabases.Text, Database, UserID, Password, '', '');
    Result := True;
    gGUIINI.WriteInteger('login', 'database', cbDatabases.FocusItem);
    gGUIINI.WriteString('login', 'username', UserID);
  except
    on E: Exception do
      begin
        Result := False;
        tiAppError(E.Message);
      end;
  end;
end;

function TLoginForm.GetSelectedDatabase: TfpgString;
begin
  Result := aStringList.ValueFromIndex[cbDatabases.FocusItem];
end;

procedure TLoginForm.PasswordKeyPressed(Sender: TObject; var KeyCode: word;
        var ShiftState: TShiftState; var Consumed: boolean);
begin
        if KeyCode = keyEnter then
          btnOKClick(nil)
end;

procedure TLoginForm.GetAvailableConnections(out ANames: TStringList; out AConnections: TStringList);
var
  ini: TIniFile;
  lFilename: string;
begin
  lFilename := tiGetEXEPath + PathDelim + cConnections;
  if not FileExists(lFilename) then
    raise Exception.Create(cConnections + ' files is not available. Create it and try again.');
  ini := TIniFile.Create(lFilename);
  try
    ANames := TStringList.Create;
    AConnections := TStringList.Create;
    ini.ReadSection('Connections', ANames);
    ini.ReadSectionValues('Connections', AConnections);
  finally
    ini.Free;
  end;
end;


end.
