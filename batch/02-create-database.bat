@ECHO OFF
SET username=%1
SET postgreBin=%2

%postgreBin%psql.exe -a -f "%~dp0\02-create-db-copycats.sql" postgres %username%

ECHO If no error messages were shown, database "copycats" created succesfully.