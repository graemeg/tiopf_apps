program timap;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, mapper, fpc_schema_writer;

type

  { TMapperCmd }

  TMapperCmd = class(TCustomApplication)
  protected
    procedure WriteProjectStats(AProject: TMapProject);
    procedure WriteProject(AProject: TMapProject);
    procedure DoRun; override;
    Procedure WriteError(const AError: string);
    procedure OnWriteUnit(AUnitDef: TMapUnitDef);
    procedure OnWriteClass(AClassDef: TMapClassDef);
    procedure OnWriteEnum(AEnum: TMapEnum);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

// -f "m:\lazarusProjects\simplepos\bo\main_schema.xml"

{ TMapperCmd }

procedure TMapperCmd.DoRun;
var
  lRead: TMapSchemaReader;
  lWriter: TMapperProjectWriter;
  lProj: TMapProject;
  lPath: string;
  lSL: TStringList;
  lProjWriter: TProjectWriter;
begin

  if not HasOption('f','file') then
    begin
      WriteError('File to process was not specified');
      Terminate;
      exit;
    end;


  lProj := TMapProject.Create;
  lRead := gGetSchemaReaderClass.Create;
  try

    lPath := GetOptionValue('f', 'file');
    lRead.ReadSchema(lProj, lPath);
    lWriter := TMapperProjectWriter.Create(lProj);
    lSL := TStringList.create;
    try

      WriteLn('');

      if HasOption('v', '-verbose') then
        begin
          lWriter.OnWriteClass := {$IFDEF FPC}@{$ENDIF}Self.OnWriteClass;
          lWriter.OnWriteEnum := {$IFDEF FPC}@{$ENDIF}self.OnWriteEnum;
          lWriter.OnWriteUnit := {$IFDEF FPC}@{$ENDIF}self.OnWriteUnit;
        end;

      lWriter.WriteProject(lProj.OutputDirectory);

      //lProjWriter := TProjectWriter.Create;
      //try
      //  lProjWriter.WriteProject(lProj, lProj.OutputDirectory, 'backup.xml');
      //finally
      //  lProjWriter.Free;
      //end;

      WriteLn('Operation was successful');
      WriteProjectStats(lProj);
      WriteLn;
      WriteLn;
    finally
      lWriter.free;
      lSL.free;
    end;
  finally
    lRead.Free;
    lProj.free;
  end;

  Terminate;
end;

procedure TMapperCmd.OnWriteClass(AClassDef: TMapClassDef);
begin
  WriteLn('  Writing Class: ' + AClassDef.BaseClassName + '...');
end;

procedure TMapperCmd.OnWriteEnum(AEnum: TMapEnum);
begin
  WriteLn('  Writing Enum: ' + AEnum.EnumName + '...');
end;

procedure TMapperCmd.OnWriteUnit(AUnitDef: TMapUnitDef);
begin
  WriteLn('Writing Unit: ' + AUnitDef.UnitName);
end;

procedure TMapperCmd.WriteError(const AError: string);
begin
  WriteLn(sLineBreak);
  WriteLn('*** Error ***');
  WriteLn(AError);
  WriteLn(sLineBreak);
end;

constructor TMapperCmd.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMapperCmd.Destroy;
begin
  inherited Destroy;
end;

procedure TMapperCmd.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

procedure TMapperCmd.WriteProject(AProject: TMapProject);
begin
  WriteLn('---------------------------------');
  WriteLn('|Project: ' + AProject.ProjectName);
end;

procedure TMapperCmd.WriteProjectStats(AProject: TMapProject);
var
  lCtr: integer;
  lCount: integer;
  lUnit: TMapUnitDef;
begin
  lCount := 0;

  WriteLn(sLineBreak);
  WriteLn('------- Project Statistics --------------------');
  WriteLn('# Includes:                ' + IntToStr(AProject.Includes.Count));
  WriteLn('# Units:                   ' + IntToStr(AProject.Units.Count));
  for lCtr := 0 to AProject.Units.Count - 1 do
    begin
      lUnit := AProject.Units.Items[lCtr];
      lCount := lCount + lUnit.UnitClasses.Count;
    end;
  WriteLn('# Classes:                 ' + IntToStr(lCount));

end;

var
  Application: TMapperCmd;

{$R *.res}

begin
  Application:=TMapperCmd.Create(nil);
  Application.Title:='TtiMapper';
  Application.Run;
  Application.Free;
end.

