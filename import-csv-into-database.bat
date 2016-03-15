:: Usage: 
:: This batch file accepts 3
:: %~1 - step at which to start (create-user, create-database, create-table, create-nothing)
:: %~2 - username of user to create-database (recommended to leave default)
:: %~3 - password of user to create (recommended to leave default)
:: %~4 - location of the bin folder of PostgreSQL (recommended to change in the file)



:: Add 7zip to PATH, so that we can use it from command line (this needs to be done just once)
set PATH=%PATH%;C:\Program Files\7-Zip\
:: default path to postgre bin/ folder
SET postgreBin="C:\Program Files\PostgreSQL\9.5\bin\"


@ECHO OFF


:: setting defaults for missed parameters
IF [%1]==[] GOTO:TRUE-1
GOTO:ELSE-1
:TRUE-1
:: set default value for parameter
SET stepToStart=create-user
GOTO:END-IF-1
:: set custom (passed) value for parameter
:ELSE-1
SET stepToStart=%1
GOTO:END-IF-1
:END-IF-1

IF [%2]==[] GOTO:TRUE-2
GOTO:ELSE-2
:TRUE-2
:: set default value for parameter
SET username=copycat
GOTO:END-IF-2
:: set custom (passed) value for parameter
:ELSE-2
SET username=%2
GOTO:END-IF-2
:END-IF-2

IF [%3]==[] GOTO:TRUE-3
GOTO:ELSE-3
:TRUE-3
:: set default value for parameter
SET password=default-password-for-copycats
GOTO:END-IF-3
:: set custom (passed) value for parameter
:ELSE-3
SET password=%3
GOTO:END-IF-3
:END-IF-3

IF [%4]==[] GOTO:TRUE-4
GOTO:ELSE-4
:TRUE-4
:: set default value for parameter
:: is already set
GOTO:END-IF-4
:: set custom (passed) value for parameter
:ELSE-4
SET postgreBin=%4
GOTO:END-IF-4
:END-IF-4


:: Start main batch
GOTO:%stepToStart%


:create-user
ECHO Start start from creating a user (from scratch)
:1
CALL batch\01-create-user.bat %username% %password% %postgreBin%
GOTO:2

:create-database
ECHO Start from creating a database
:2
CALL batch\02-create-database.bat %username% %postgreBin%
GOTO:3

:create-schemas
ECHO: Start from creating a table
:3
CALL batch\03-create-schemas.bat %username% %postgreBin%
GOTO:4

:create-tables
ECHO: Start from creating a table
:4
CALL batch\04-create-tables.bat %username% %postgreBin%
GOTO:5

:copy-csv
ECHO: Just import from csv
:5
CALL batch\05-copy-csv.bat %username% %postgreBin%

:edit-tables
ECHO: Edit imported tables
:6
CALL batch\06-edit-tables.bat %username% %postgreBin%





GOTO:pause-in-the-end



:pause-in-the-end
pause