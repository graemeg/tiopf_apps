unit connection_util;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes
  ,SysUtils
  ,tiQuerySqldbIB
  ,tiOPFManager
  ,tiConstants
  ,tiOIDGUID
  ;


function ConnectToDatabase(const ADataSource: string; const AServer: string;
  const AUser: string; const APass: string): boolean;

implementation

var
  mConnected: boolean;

function ConnectToDatabase(const ADataSource: string; const AServer: string;
  const AUser: string; const APass: string): boolean;
var
  lResource: string;
begin

  result := mConnected;

  if not result then
    begin
      GTIOPFManager.DefaultPersistenceLayerName := cTIPersistSqldbIB;

      if AServer <> '' then
        lResource := AServer + ':' + ADataSource
      else
        lResource := ADataSource;

      result := GTIOPFManager.TestThenConnectDatabase(lResource, AUser, APass);
      mConnected := result;

    end;

end;

end.

