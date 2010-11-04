unit test_cleanup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  TTestCleanup= class(TTestCase)
  published
    procedure   CleanUpDatabase;
  end; 

implementation
uses
  tiOPFManager
  ;

{ TTestCleanup }

procedure TTestCleanup.CleanUpDatabase;
begin
  GTIOPFManager.ExecSQL('delete from person');
  GTIOPFManager.ExecSQL('delete from jobs');
  GTIOPFManager.ExecSQL('delete from user_job_relation');
end;

initialization

  RegisterTest(TTestCleanup); 
end.

