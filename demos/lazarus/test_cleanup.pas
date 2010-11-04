unit test_cleanup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit
  ,testutils
  ,testregistry
  ,base_connected_test
  ;

type

  TTestCleanUp = class(TBaseConnectedTest)
  protected
    procedure SetUp; override;
  published
    procedure   CleanUp;
  end;

implementation
uses
  tiOPFManager
  ;

{ TTestUser }

procedure TTestCleanUp.CleanUp;
begin
  GTIOPFManager.ExecSQL('delete from user_permission');
  GTIOPFManager.ExecSQL('delete from contact_info');
  GTIOPFManager.ExecSQL('delete from user_group_relation');
  GTIOPFManager.ExecSQL('delete from user_group');
  GTIOPFManager.ExecSQL('delete from system_user');
  GTIOPFManager.ExecSQL('delete from system_audit');
  GTIOPFManager.ExecSQL('delete from service_charge_item');
  GTIOPFManager.ExecSQL('delete from sale_tax_item');
  GTIOPFManager.ExecSQL('delete from sale_item');
  GTIOPFManager.ExecSQL('delete from sale');
end;

procedure TTestCleanUp.SetUp;
begin
  inherited SetUp;
end;

initialization
  RegisterTest(TTestCleanUp);

end.

