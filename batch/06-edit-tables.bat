@ECHO OFF
SET username=%1
SET postgreBin=%2

::%postgreBin%psql.exe -a -f "%~dp0\06-edit-tables-tr.sql" copycats %username%

ECHO Tables edited succesfully.
