program maptest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_mapper, base_connected_test,
  connection_util, jobs_bom, person_bom, laz_timapper;

{$R *.res}

begin
  Application.Initialize;
  with base_connected_test.mDBConnect do
    begin
      DBPath := 'M:\LazarusProjects\custom_comps\tiOPFMapper\demos\database\jobs.fdb';
      DBHost := 'localhost';
      UserName := 'sysdba';
      Password := 'masterkey';
    end;

  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

