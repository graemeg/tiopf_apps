@echo off
SET THEFILE=M:\LazarusProjects\custom_comps\tiOPFMapper\demos\lazarus\maptest.exe
echo Linking %THEFILE%
M:\lazarus_current\lazarus\fpc\2.4.0\bin\i386-win32\ld.exe -b pe-i386 -m i386pe  --gc-sections    --entry=_mainCRTStartup    -o M:\LazarusProjects\custom_comps\tiOPFMapper\demos\lazarus\maptest.exe M:\LazarusProjects\custom_comps\tiOPFMapper\demos\lazarus\link.res
if errorlevel 1 goto linkend
M:\lazarus_current\lazarus\fpc\2.4.0\bin\i386-win32\postw32.exe --subsystem console --input M:\LazarusProjects\custom_comps\tiOPFMapper\demos\lazarus\maptest.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
