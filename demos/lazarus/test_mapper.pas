unit test_mapper; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, base_connected_test,
  person_bom, jobs_bom;

type

  TTestMapper= class(TBaseConnectedTest)
  private
  published
    procedure   TestUserCrud;
    procedure   TestJobCrud;
    procedure   TestCreateUserJobRelations;
  end; 


const
  JOB_OID = 'E2385E40-70F4-4ECE-A752-3A242EE84165';
  JOB2_OID = '28DEF125-B498-4814-B410-F915681EBC55';
  USER_OID = '8AC4AAEE-F6EC-4FF9-89CD-FBEF375C0259';

implementation

procedure TTestMapper.TestCreateUserJobRelations;
var
  lRelList: TUserJobRelationList;
  lRel: TUserJobRelation;
  lJobList: TJobList;
begin
  lRelList := TUserJobRelationList.Create;
  try
    lRel := TUserJobRelation.CreateNew;
    lRel.JobOID := JOB_OID;
    lRel.UserOID := USER_OID;
    lRelList.Add(lRel);

    lRel := TUserJobRelation.CreateNew;
    lRel.JobOID := JOB2_OID;
    lRel.UserOID := USER_OID;
    lRelList.Add(lRel);

    lRelList.Save;

  finally
    lRelList.Free;
  end;

  lJobList := TJobList.Create;
  try
    lJobList.FindJobsForUser(USER_OID);
    AssertTrue('Failed Custom method FindJobsForUser', lJobList.Count > 0);

    lJobList.Clear;
    lJobList.FindByStatus(Integer(jsFinished));
    AssertTrue('Failed Custom method FindByStatus', lJobList.Count = 1);
  finally
    lJobList.free;
  end;
end;

procedure TTestMapper.TestJobCrud;
var
  lJobList: TJobList;
  lJob: TJob;
begin
  lJobList := TJobList.Create;
  try
    lJob := TJob.CreateNew;
    lJob.OID.AsString := JOB_OID;
    lJob.JobName := 'Pooper Scooper';
    lJob.JobDesc := 'Zoo animal cage clean up';
    lJob.Status := jsStarted;
    lJobList.Add(lJob);

    lJob := TJob.CreateNew;
    lJob.OID.AsString := JOB2_OID;
    lJob.Status := jsFinished;
    lJob.JobName := 'Package Assumbly';
    lJob.JobDesc := 'Factory Line Packaging';
    lJobList.Add(lJob);

    lJobList.Save;

  finally
    lJobList.Free;
  end;
end;

procedure TTestMapper.TestUserCrud;
var
  lPersons: TPersonList;
  lPer: TPerson;
begin
  lPersons := TPersonList.Create;
  try
    lPer := TPerson.CreateNew;
    lPer.OID.AsString := USER_OID;
    lPer.FirstName := 'Abby';
    lPer.LastName := 'Johnson';
    lPer.Age := 28;
    lPer.Gender := gtFemale;
    lPersons.Add(lPer);

    lPer.Save;

    // All created object lists get the FindByOID function.
    lPersons.Clear;
    lPersons.FindByOID(USER_OID);
    AssertTrue('FindByOID did not work', lPersons.Count > 0);

    {
      This is a custom method defined for TPerson in the xml schema.
      It causes the methods defined to be created on the TPersonList object.
      See /demos/bom/sample.xml.
    }
    lPersons.Clear;
    lPersons.FindByFirstName('Abby');
    AssertTrue('Custom method FindByFirstName did not work', lPersons.Count > 0);

    { Because we hook into the automapping registrations, we leverage the tiCriteriaAsSQL
    methods to create simple filter using tiCriteria but with hard coded visitors.  Allows to use
    only object's properties for attributes.  See /demos/bom/person_bom.pas -> TPersonList_Read.init method. }
    lPersons.Clear;
    lPersons.Criteria.AddEqualTo('FirstName', 'Abby');
    lPersons.Read;
    AssertTrue('Custom method Critera Map did not work', lPersons.Count > 0);

  finally
    lPersons.Free;
  end;
end;

initialization

  RegisterTest(TTestMapper); 
end.

