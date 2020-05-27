program timap;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$apptype console}
{$ENDIF}

uses
  Classes,
  SysUtils,
  mapper,
  {$ifdef FPC}
  custapp,
  {$endif}
  {$ifndef FPC}
  delphi_custom_app,
  {$endif}
  AppModel,
  common_schema_reader;

type

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
  lPath: string;
//  lProjWriter: TProjectWriter;
begin
  if (ParamCount = 0) or HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not HasOption('f','file') then
    begin
      WriteError('File to process was not specified');
      Terminate;
      exit;
    end;

  try
    lPath := GetOptionValue('f', 'file');

    TAppModel.Instance.LoadProject(lPath);
    TAppModel.Instance.WriteProject(HasOption('r','resave'),
                               HasOption('v', 'verbose'),
                               {$IFDEF FPC}@{$ENDIF}OnWriteClass,
                               {$IFDEF FPC}@{$ENDIF}OnWriteEnum,
                               {$IFDEF FPC}@{$ENDIF}OnWriteUnit
                               );

    WriteLn('Operation was successful');
    WriteProjectStats(TAppModel.Instance.Project);
    WriteLn;
    WriteLn;
  finally
    TAppModel.Instance.CloseProject;
  end;

  Terminate;
end;

procedure TMapperCmd.OnWriteClass(AClassDef: TMapClassDef);
begin
  WriteLn('  Writing Class: ' + AClassDef.BaseClassName + '...');
end;

procedure TMapperCmd.OnWriteEnum(AEnum: TMapEnum);
begin
  WriteLn('  Writing Enum: ' + AEnum.TypeName + '...');
end;

procedure TMapperCmd.OnWriteUnit(AUnitDef: TMapUnitDef);
begin
  WriteLn('Writing Unit: ' + AUnitDef.Name);
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
  writeln('');
  writeln('   -h  or --help         Displays this help');
  writeln('   -v  or --verbose      Verbose output');
  writeln('   -r  or --resave       Resave the XML schema file');
  writeln('   -f <file>   or        Where <file> is the XML schema file');
  writeln('   --file=<file>         ');
  writeln('');
end;

procedure TMapperCmd.WriteProject(AProject: TMapProject);
begin
  WriteLn('---------------------------------');
  WriteLn('|Project: ' + AProject.GeneralOptions.ProjectName);
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
begin
  Application:=TMapperCmd.Create(nil);
  Application.Title:='TtiMapper';
  Application.Run;
  Application.Free;
end.
