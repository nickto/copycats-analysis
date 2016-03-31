@ECHO OFF
SET username=%1
SET postgreBin=%2

::%postgreBin%psql.exe -a -f "%~dp0\03-create-schema-tr.sql" copycats %username%
%postgreBin%psql.exe -a -f "%~dp0\03-create-schema-crsp.sql" copycats %username%

ECHO If no error messages were shown, schemas "tr" and "crsp" were created succesfully.