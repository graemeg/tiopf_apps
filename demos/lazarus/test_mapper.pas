unit test_mapper; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, base_connected_test,
  person_bom, jobs_bom;

type

  TTestMapper= class(TBaseConnectedTest)
  published
    procedure   TestUserCrud;
  end; 

implementation

procedure TTestMapper.TestUserCrud;
var
  lPersons: TPersonList;
  lPer: TPerson;
  lPersonOID: string;
begin
  lPersons := TPersonList.Create;
  try
    lPer := TPerson.CreateNew;
    lPersonOID := lPer.OID.AsString;
    lPer.FirstName := 'Abby';
    lPer.LastName := 'Johnson';
    lPer.Age := 28;
    lPer.Gender := gtFemale;
    lPersons.Add(lPer);

    lPer.Save;

    // All created object lists get the FindByOID function.
    lPersons.Clear;
    lPersons.FindByOID(lPersonOID);
    AssertTrue('FindByOID did not work', lPersons.Count > 0);

    // This is a custom method made defined for TPerson in the xml schema.
    // It causes the methods defined to be created on the TPersonList object.
    lPersons.Clear;
    lPersons.FindByFirstName('Abby');
    AssertTrue('Custom method FindByFirstName did not work', lPersons.Count > 0);

  finally
    lPersons.Free;
  end;
end;

initialization

  RegisterTest(TTestMapper); 
end.

