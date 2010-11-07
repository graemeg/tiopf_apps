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
    procedure   AllTogetherNow;
  end; 


const
  JOB_OID = 'E2385E40-70F4-4ECE-A752-3A242EE84165';
  JOB2_OID = '28DEF125-B498-4814-B410-F915681EBC55';
  USER_OID = '8AC4AAEE-F6EC-4FF9-89CD-FBEF375C0259';
  USER2_OID = 'C2FA8BA3-1CEF-4BAB-8C31-22B53FA52AB6';
implementation

procedure TTestMapper.AllTogetherNow;
var
  lPersons: TPersonList;
  lPerson: TPerson;
  lJobList: TJobList;
  lJobRelList: TUserJobRelationList;
  lRel: TUserJobRelation;
  lJob: TJob;
  lCtr: integer;
begin
  lPersons := TPersonList.create;
  try
    // First, change ownership of all jobs from user 1 to user 2
    lJobRelList := TUserJobRelationList.Create;
    try
      // Convenience method defined in xml schema
      lJobRelList.FindByUser(USER_OID);

      for lCtr := 0 to lJobRelList.Count - 1 do
        begin
          lRel := lJobRelList.Items[lCtr];
          lRel.UserOID := USER2_OID;
        end;

      lJobRelList.Save;
    finally
      lJobRelList.free;
    end;

    lJob := TJob.CreateNew;
    try
      lJob.JobName := 'Bikini Judge';
      lJob.JobDesc := 'Enjoying lifes wonders';
      lJob.Save;

      lRel := TUserJobRelation.CreateNew;
      try
        lRel.UserOID := USER_OID;
        lRel.JobOID := lJob.OID.AsString;
        lRel.Save;
      finally
        lRel.Free;
      end;

    finally
      lJob.Free;
    end;

  finally
    lpersons.free;
  end;

  // Finally, find all jobs with status of started and changed to finished
  lJobList := TJobList.Create;
  try
    lJobList.FindByStatus(jsStarted);
    for lCtr := 0 to lJobList.Count - 1 do
      begin
        lJob := lJobList.Items[lCtr];
        lJob.Status := jsFinished;
      end;
    lJobList.Save;
  finally
    lJobList.free;
  end;

  // One last thing, lets give the ultimate gift to our older persons
  // and show how we can use the tiAutomapping registrations along with the
  // tiCriteriaAsSQL methods to query objects.
  lPersons := TPersonList.Create;
  try
    lPersons.Criteria.AddGreaterOrEqualThan('Age', 40);
    lPersons.Read;
    for lCtr := 0 to lPersons.Count - 1 do
      begin
        lPerson := lPersons.Items[lCtr];
        lPerson.Age := lPerson.Age - 10;
      end;
    lPersons.Save;
  finally
    lPersons.Free;
  end;
end;

procedure TTestMapper.TestCreateUserJobRelations;
var
  lRelList: TUserJobRelationList;
  lRel: TUserJobRelation;
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
    lPer.PersonType := ptEmployee;
    lPersons.Add(lPer);

    lPer := TPerson.CreateNew;
    lPer.FirstName := 'Alexandra';
    lPer.LastName := 'Harrington';
    lPer.Age := 38;
    lPer.Gender := gtFemale;
    lPer.PersonType := ptEmployee;
    lPersons.Add(lPer);

    lPer := TPerson.CreateNew;
    lPer.FirstName := 'Bill';
    lPer.LastName := 'Smith';
    lPer.Age := 49;
    lPer.PersonType := ptCustomer;
    lPer.Gender := gtMale;
    lPersons.Add(lPer);

    lPer := TPerson.CreateNew;
    lPer.FirstName := 'Janet';
    lPer.LastName := 'Corning';
    lPer.Age := 23;
    lPer.PersonType := ptCustomer;
    lPer.Gender := gtFemale;
    lPersons.Add(lPer);

    lPersons.Save;

    // All created object lists get the FindByOID function.
    lPersons.Clear;
    lPersons.FindByOID(USER_OID);
    AssertTrue('FindByOID did not work', lPersons.Count > 0);

    {This is a custom method defined for TPerson in the xml schema.
    It causes the methods defined to be created on the TPersonList object.
    See /demos/bom/sample.xml.}

    lPersons.Clear;
    lPersons.FindByFirstNameMatch('B');
    AssertTrue('Custom method FindByFirstNameMatch did not work', lPersons.Count > 0);

    lPersons.Clear;
    lPersons.FindByGender(gtFemale);
    AssertTrue('Custom method FindByGender did not work', lPersons.Count > 0);

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

