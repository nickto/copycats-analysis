@ECHO OFF
SET username=%1
SET postgreBin=%2

%postgreBin%psql.exe -a -f "%~dp0\03-create-schema-tr.sql" copycats %username%

ECHO Schema "tr" for Thomson Reuters created succesfully.