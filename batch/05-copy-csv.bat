@ECHO OFF
SET username=%1
SET postgreBin=%2

%postgreBin%psql.exe -a -f "%~dp0\05-copy-csv-tr.sql" copycats %username%

ECHO Tables imported succesfully.
