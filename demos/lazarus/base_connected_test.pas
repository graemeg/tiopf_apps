unit base_connected_test;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, fpcunit, testutils, testregistry, connection_util;

type

  TConnectRec = record
    DBHost: string;
    DBPath: string;
    UserName: string;
    Password: string;
  end;

  TBaseConnectedTest = class(TTestCase)
  protected
    procedure SetUp; override;
  end;

var
  mDBConnect: TConnectRec;

implementation

{ TTestUserRoles }

procedure TBaseConnectedTest.SetUp;
var
  lConnected: boolean;
begin
  lConnected := ConnectToDatabase(mDBConnect.DBPath, mDBConnect.DBHost,
    mDBConnect.UserName, mDBConnect.Password);

  AssertTrue('Could not connect to database', lConnected);

end;

end.

