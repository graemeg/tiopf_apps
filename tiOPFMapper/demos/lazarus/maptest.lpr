program maptest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_mapper, base_connected_test,
  connection_util, jobs_bom, person_bom, laz_timapper, test_cleanup, SysUtils;

{$R *.res}

begin
  Application.Initialize;
  with base_connected_test.mDBConnect do
    begin
      DBPath := ExtractFileDir(Application.ExeName)+'..\database\jobs.fdb';
      DBHost := 'localhost';
      UserName := 'sysdba';
      Password := 'masterkey';
    end;

  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

