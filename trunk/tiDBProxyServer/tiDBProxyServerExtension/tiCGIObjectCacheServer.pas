unit tiCGIObjectCacheServer;

interface
uses
  // tiOPF
  tiCGIObjectCacheAbs
  ,tiDBConnectionPool
  ,tiQuery
  // Delphi
  ,SysUtils
  ,Classes
  ;

type

  TtiCGIOjectCacheServer = class( TtiCGIOjectCacheAbs )
  private
    procedure   ReturnDataFromCache;
  protected
    function    GetDataAsXML: string ; virtual ; abstract ;
    procedure   Init; override ;
    procedure   RefreshCacheFromDB; override;
    procedure   ConnectToDatabase; virtual;
    procedure   DoExecute;

    // You MAY override these
    //function    GetDBFileDataSQL: string ; override ;
    //Result := 'select max(Field_Name) as file_date from Table_Name ' ;
    function    CacheDirectory: string; override;
    procedure   GetDBConnectionDetails(var ADatabaseName, AUserName, APassword: string); virtual;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    class procedure Execute;
  end ;

function  tiGetCGIAppINIFileName: string;

implementation
uses
  // tiOPF
  tiUtils
  ,tiCGIDBConnection
  ,tiOPFManager
  ,tiConstants
  ,tiXML
  ,tiCommandLineParams
  ,tiRegINI
  ,tiLog
  ,tiWebServerConstants
  ,tiDBProxyServerConfig
  // Delphi
  ;

const
  cErrorReadingDBConnectionFromINI = 'Database connection details not found in <%s> Looking for [%s] %s, %s, %s';

function tiGetCGIAppINIFileName: string;
var
  LEXEName: string;
begin
  LEXEName:= tiSwapExt(ParamStr(0), 'ini');
  Result :=
    ExpandFileName(
      tiAddTrailingSlash(ExtractFilePath(LEXEName)) +
      '..\' +
      ExtractFileName(LEXEName));
end;

{ TtiCGIOjectCacheServer }

constructor TtiCGIOjectCacheServer.Create;
begin
  gTIOPFManager.VisMgr.BreakOnException := True ;
  inherited ;
  Params.AsCompressedEncodedString := gCommandLineParams.AsString ;
end;

destructor TtiCGIOjectCacheServer.Destroy;
begin
  inherited;
end;

procedure TtiCGIOjectCacheServer.DoExecute;
var
  lCachedFileDate : TDateTime ;
  lDatabaseFileDate : TDateTime ;
begin
  ExitCode:= cTICGIExitCodeOK;
  try
    Init ;
    ConnectToDatabase;
    gTIOPFManager.DefaultDBConnectionPool.MinPoolSize := 1 ;
    lCachedFileDate := GetCachedFileDate;
    lDatabaseFileDate := GetDBFileDate ;
    if ( lCachedFileDate <> lDatabaseFileDate ) or
       ( not FileExists( GetCachedFileDirAndName )) then
    begin
      RefreshCacheFromDB;
      SetCachedFileDate(lDatabaseFileDate);
    end ;
    ReturnDataFromCache;
  except
    on E:Exception do
    begin
      if E is EtiCGIException then
        ExitCode := (e as EtiCGIException).ExitCode
      else
        ExitCode:= cTICGIExitCodeUnknownException;
      WriteLn(e.message);
    end;
  end ;
end;

class procedure TtiCGIOjectCacheServer.Execute;
var
  L: TtiCGIOjectCacheServer;
begin
  L:= Create;
  try
    L.DoExecute;
  finally
    L.Free;
  end;
end;

procedure TtiCGIOjectCacheServer.ReturnDataFromCache;
var
  lFileName : string ;
begin
  lFileName := tiAddTrailingSlash(CacheDirectory) + CachedFileName ;
  Assert(FileExists(lFileName), 'File not found in cache: ' + lFileName );
  Write(tiFileToString(lFileName))
end;

procedure TtiCGIOjectCacheServer.RefreshCacheFromDB;
var
  lResult: string ;
  lResultEncode: string;
  lFileName: string;
begin
  Assert(gTIOPFManager.DefaultDBConnectionName <> '', 'No database connection');
  lFileName := GetCachedFileDirAndName;
  lResult := GetDataAsXML;
  lResultEncode := tiCompressEncode(lResult);
  tiStringToFile(lResultEncode,lFileName);
end;

procedure TtiCGIOjectCacheServer.Init;
var
  LPath: string;
begin
  LPath:= CacheDirectory;
  if not DirectoryExists(LPath) then
    ForceDirectories(LPath);
  if not DirectoryExists(LPath) then
    raise EtiCGIException.Create(cTICGIExitCodeCanNotCreateCacheDirectory);
  inherited;
end;

function TtiCGIOjectCacheServer.CacheDirectory: string;
begin
  Result:= ExpandFileName( tiAddTrailingSlash(tiGetEXEPath) + '\..\CachedData' );
end;

procedure TtiCGIOjectCacheServer.ConnectToDatabase;
var
  lPerLayerName   : string ;
  lDatabaseName   : string ;
  lUserName       : string ;
  lPassword       : string ;
  lError          : string ;
begin
  lPerLayerName := gTIOPFManager.DefaultPerLayerName ;
  Assert( lPerLayerName <> cTIPersistXMLLight, 'Default PerLayerName is ' +
          cTIPersistXMLLight + '. You must include tiQuery??? ahead of tiQueryXMLLight' ) ;

  gTIOPFManager.TerminateOnFailedDBConnection := False ;
  gTIOPFManager.VisMgr.BreakOnException := True ;

  GetDBConnectionDetails(lDatabaseName, lUserName, lPassword);

  if gTIOPFManager.DefaultDBConnectionName = lDatabaseName then
    Exit ; //==>

  try
    gTIOPFManager.ConnectDatabase( lDatabaseName, lUserName, lPassword, '', lPerLayerName ) ;
    gTIOPFManager.DefaultDBConnectionName := lDatabaseName ;
    gTIOPFManager.DefaultPerLayerName := lPerLayerName;
  except
    on e:Exception do
    begin
      lError := e.message ;
      lError := tiStrTran(lError, #10, '');
      lError := tiStrTran(lError, #13, ' ');
      lError := tiStrTran(lError, '  ', ' ');
      lError := tiStrTran(lError, '  ', ' ');
      lError := tiStrTran(lError, '  ', ' ');
      raise Exception.Create(lError);
    end;
  end;
end;

procedure TtiCGIOjectCacheServer.GetDBConnectionDetails(var ADatabaseName, AUserName, APassword: string);
var
  LINI: TtiDBProxyServerConfig;
begin
  LINI:= TtiDBProxyServerConfig.Create;
  try
    ADatabaseName := LINI.DatabaseName;
    AUserName     := LINI.UserName;
    APassword     := LINI.Password;
  finally
    LINI.Free;
  end;
end;

initialization
  // Not a good use of Assert in an initialization, better as a ShowMessage followed
  // by Abort, but that would link in Dialogs, which we want to avoid in the
  // long term.
  Assert( IsConsole, 'tiCGIObjectCache.pas has been included in an Application that is not a Console App' );

end.
