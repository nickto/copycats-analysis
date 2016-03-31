@ECHO OFF
SET username=%1
SET postgreBin=%2

::%postgreBin%psql.exe -a -f "%~dp0\04-create-table-tr.sql" copycats %username%
%postgreBin%psql.exe -a -f "%~dp0\04-create-table-crsp.sql" copycats %username%

ECHO Tables created succesfully.

