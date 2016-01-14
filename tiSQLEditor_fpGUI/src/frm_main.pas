unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_menu,
  fpg_panel,
  frm_editor;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    MainMenu: TfpgMenuBar;
    SB: TfpgPanel;
    FSQLEditor: TEditorForm;
    mnuFile: TfpgPopupMenu;
    mnuQuery: TfpgPopupMenu;
    mnuHelp: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    FFileName: TfpgString;
    FDatabase: string;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CommandHandler(Sender: TObject);
    procedure RunAsScriptClicked(Sender: TObject);
    procedure RunScriptSequence(Sender: TObject);
    procedure Save;
    procedure Load(const AFileName: TfpgString);
    function GetSQL: string;
    procedure RunAsScript(const AAppName: string; const AParams: string);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    function  DoLogin: Boolean;
    procedure LoadImages;
    procedure StatusUpdate(const AMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_command_intf,
  fpg_utils,
  fpg_dialogs,
  tiINI,
  tiGUIINI,
  tiCommandLineParams,
  tiQuery,
  tiUtils,
  tiOPFManager,
  tiLog,
  tiOPFSQLScript,
  commands,
  constants,
  frm_runasscript,
  frm_scriptsequence,
  frm_login;


const
  cDatabaseName   = '%DatabaseName%';
  cUserName       = '%UserName%';
  cPassword       = '%Password%';
  cScriptFileName = '%ScriptFileName%' ;
  
  {$I images.inc}

 {@VFD_NEWFORM_IMPL}

 { A single event handler that handles all Command based events. }
procedure TMainForm.CommandHandler(Sender: TObject);
var
  cmd: ICommand;
  holder: ICommandHolder;
begin
  if Supports(Sender, ICommandHolder, holder) then
  begin
    cmd := holder.GetCommand;
    if Assigned(cmd) then
      cmd.Execute;
  end;
end;

procedure TMainForm.RunAsScriptClicked(Sender: TObject);
var
  lForm: TRunAsScriptForm;
begin
  lForm := TRunAsScriptForm.Create( nil ) ;
  try
    if lForm.ShowModal = mrOK then
      RunAsScript( lForm.AppToRun, lForm.Params ) ;
  finally
    lForm.Free;
  end;
end;

procedure TMainForm.RunScriptSequence(Sender: TObject);
var
  lForm: TRunScriptSequenceForm;
  scriptrunner: TtiOPFSQLscript;
  i: integer;
  f: string;
  db: TtiDatabase;
begin
  lForm := TRunScriptSequenceForm.Create( nil ) ;
  try
    if lForm.ShowModal = mrOK then
    begin
      scriptrunner := TtiOPFSQLscript.Create(nil);
      db := gTIOPFManager.DefaultDBConnectionPool.Lock;
      try
        scriptrunner.Database := db;
        scriptrunner.AutoCommit := True;
        for i := 0 to lForm.Scripts.Count-1 do
        begin
          f := lForm.Scripts[i];
          StatusUpdate('Processing... ' + f);
          scriptrunner.Script.LoadFromFile(f);
          scriptrunner.Execute;
        end;
        StatusUpdate('Done processing script sequence');
      finally
        scriptrunner.Free;
        gTIOPFManager.DefaultDBConnectionPool.Unlock(db);        
      end;
//      tiShowString(lForm.Scripts);
    end;
  finally
    lForm.Free;
  end;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(329, 182, 650, 346);
  WindowTitle := 'tiSQLEditor';
  Hint := '';

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 650, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  SB := TfpgPanel.Create(self);
  with SB do
  begin
    Name := 'SB';
    SetPosition(0, 326, 650, 20);
    Anchors := [anLeft,anRight,anBottom];
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := 'Panel';
  end;

  FSQLEditor := TEditorForm.Create(self);
  with FSQLEditor do
  begin
    Name := 'FSQLEditor';
    SetPosition(4, 28, 642, 292);
    Anchors := [anLeft,anRight,anTop,anBottom];
    TabOrder := 0;
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(512, 52, 120, 20);
    AddMenuItem('Open...', 'Ctrl+O', @FileOpenExecute);
    AddMenuItem('Save', 'Ctrl+S', @FileSaveExecute);
    AddMenuItem('Save As...', 'Ctrl+Shift+S', @FileSaveAsExecute);
    AddMenuItem('New', 'Ctrl+N', @FileNewExecute);
    AddMenuItem('-', '', nil);
    AddMenuItem('Quit', 'Ctrl+Q', @CommandHandler).SetCommand(TExitCommand.Create);
  end;

  mnuQuery := TfpgPopupMenu.Create(self);
  with mnuQuery do
  begin
    Name := 'mnuQuery';
    SetPosition(512, 92, 120, 20);
    AddMenuItem('Run', 'F8', @CommandHandler).SetCommand(TRunSQLCommand.Create(FSQLEditor));
    AddMenuItem('Run and save', 'Alt+F8', @CommandHandler).SetCommand(TRunSQLAndSaveCommand.Create(FSQLEditor));
    AddMenuItem('Run as script', 'Alt+Shift+F8', @RunAsScriptClicked);
    AddMenuItem('-', '', nil);
    AddMenuItem('Run a script sequence', '', @RunScriptSequence);
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(512, 110, 120, 20);
    AddMenuItem('About tiSQLEditor...', 'F1', @CommandHandler).SetCommand(TAboutCommand.Create);
    AddMenuItem('About fpGUI Toolkit...', '', @CommandHandler).SetCommand(TAboutfpGUICommand.Create);
    AddMenuItem('-', '', nil);
    AddMenuItem('About Database Connection...', 'Alt+F1', @CommandHandler).SetCommand(TDatabaseConnectionDetailsCommand.Create);
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  MainMenu.AddMenuItem('&File', nil).SubMenu  := mnuFile;
  MainMenu.AddMenuItem('&Edit', nil).SubMenu  := FSQLEditor.pmEdit;
  MainMenu.AddMenuItem('&Query', nil).SubMenu := mnuQuery;
  MainMenu.AddMenuItem('&Help', nil).SubMenu  := mnuHelp;

  // instantiate the Command classes
  //  mnuFile.MenuItemByName('Quit').SetCommand(TExitCommand.Create);

  //  mnuQuery.MenuItemByName('Run').SetCommand(TRunSQLCommand.Create(FSQLEditor));

end;

procedure TMainForm.Save;
begin
  FSQLEditor.SaveToFile(FFileName);
  WindowTitle      := rsFormCaption + ' - ' + FFileName;
  FSQLEditor.Dirty := False;
  StatusUpdate('Saved ' + FFilename);
end;

procedure TMainForm.Load(const AFileName: TfpgString);
begin
  FFileName := AFileName ;
  GINI.WriteString( 'FileName', FDatabase, AFileName);
  FSQLEditor.LoadFromFile( AFileName ) ;
  WindowTitle := rsFormCaption + ' - ' + AFileName ;
  StatusUpdate('Loaded ' + FFilename);
end;

function TMainForm.GetSQL: string;
begin
  result := FSQLEditor.SQL ;
end;

procedure TMainForm.RunAsScript(const AAppName: string; const AParams: string);
var
  lFileName : string ;
  lParams   : string ;
  lDBParams: TtiDBConnectionParams;
begin
  lFileName := tiGetTempFile( 'SQL' ) ;
  lFileName := tiAddTrailingSlash(fpgExtractFilePath(lFileName)) + 'tiSQLEditor' + PathDelim + fpgExtractFileName(lFileName);
  tiForceDirectories(ExtractFilePath(lFileName));
  tiStringToFile( GetSQL, lFileName ) ;
  lDBParams := gTIOPFManager.DefaultDBConnectionPool.DBConnectParams;
//  Assert( lDBParams.TestValid, cErrorTIPerObjAbsTestValid );
  lParams := tiStrTran( AParams, cDatabaseName,   lDBParams.DatabaseName ) ;
  lParams := tiStrTran( lParams,  cUserName,       lDBParams.UserName ) ;
  lParams := tiStrTran( lParams,  cPassword,       lDBParams.Password ) ;
  lParams := tiStrTran( lParams,  cScriptFileName, lFileName ) ;
  Log('About to run: ' + AAppName + ' ' + lParams);
//  tiShowString('About to run: ' + AAppName + ' ' + lParams);
  tiRunEXEAndWait(AAppName, lParams);
  StatusUpdate('Done running as script');
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GGUIINI.ReadFormState(self);
  OnShow := @FormShow;
  OnCloseQuery := @FormCloseQuery;
  LoadImages;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  { Show a login screen. More user friendly for a GUI app }
  if not DoLogin then
  begin
    Close;
    Exit;
  end;

  WindowTitle := 'tiSQLEditor - ' + GTIOPFManager.DefaultDBConnectionPool.DBConnectParams.DatabaseName + ' [' +GTIOPFManager.DefaultDBConnectionPool.DatabaseAlias + ']';
  FFileName := GGUIINI.ReadString('FileName', FDatabase, '');
  if (FFileName <> '') and fpgFileExists(FFileName) then
    Load(FFileName)
  else
    FFileName := '';
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  lAction: TfpgMsgDlgBtn;
begin
  if not FSQLEditor.Dirty then
  begin
    CanClose := true ;
    Exit ; //==>
  end ;

  lAction := TfpgMessageDialog.Question( 'Save changes', rsDoYouWantToSave, [mbYes, mbNo, mbCancel] ) ;

  case lAction of
    mbYes:
    begin
      FileSaveExecute( nil ) ;
      CanClose := true;
    end;
    mbNo:
      CanClose := true;

    mbCancel:
      CanClose := false;
  end ;
end;

procedure TMainForm.FileOpenExecute(Sender: TObject);
var
  lFile: TfpgString;
  lDir: TfpgString;
begin
  if FFilename <> '' then
    lDir := fpgExtractFileDir(FFileName);
  lFile := SelectFileDialog(sfdOpen, 'SQL files|*.sql', lDir);
  if lFile <> '' then
    Load(lFile);
end;

procedure TMainForm.FileSaveExecute(Sender: TObject);
begin
  if FFileName = '' then
    FileSaveAsExecute(nil)
  else
    Save;
end;

procedure TMainForm.FileSaveAsExecute(Sender: TObject);
var
  lFile: TfpgString;
  lDir: TfpgString;
begin
  if FFilename <> '' then
    lDir := fpgExtractFileDir(FFileName);
  lFile := SelectFileDialog(sfdSave, 'SQL files|*.sql', lDir);
  if lFile <> '' then
  begin
    FFileName := lFile;
    GINI.WriteString('FileName', FDatabase, FFileName);
    Save;
  end;
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
  //--------------
  procedure _DoNew;
  begin
    FSQLEditor.Clear;
    FFileName := '';
    WindowTitle := rsFormCaption;
  end ;
var
  lAction: TfpgMsgDlgBtn;
begin
  if not FSQLEditor.Dirty then
  begin
    _DoNew;
    exit; //==>
  end;

  lAction := TfpgMessageDialog.Question( 'Save changes', rsDoYouWantToSave, [mbYes, mbNo, mbCancel] ) ;

  case lAction of
    mbYes: 
    begin
      FileSaveExecute(nil);
      _DoNew;
    end;

    mbNo:
      _DoNew;
  end;
end;

function TMainForm.DoLogin: Boolean;
var
  login: TLoginForm;
begin
  login := TLoginForm.Create(nil);
  try
    Result := login.ShowModal = mrOK;
    if Result then
      FDatabase := login.Database;
  finally
    login.Free;
  end;
end;

procedure TMainForm.LoadImages;
begin
  fpgImages.AddMaskedBMP('sqlmgr.database', @cImg_sqlMgr_DB,
    sizeof(cImg_sqlMgr_DB), 0,0);

  fpgImages.AddBMP('sqlmgr.sql', @cImg_sqlMgr_SQL,
    sizeof(cImg_sqlMgr_SQL));

  fpgImages.AddBMP('sqlmgr.table', @cImg_sqlMgr_Table,
    sizeof(cImg_sqlMgr_Table));
end;

procedure TMainForm.StatusUpdate(const AMessage: string);
begin
        SB.Text := AMessage;
end;


end.

