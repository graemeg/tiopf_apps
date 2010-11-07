{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_timapper; 

interface

uses
    tiSQLParser, mapper, fpc_schema_reader, mapper_project_writer, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('laz_timapper', @Register); 
end.
