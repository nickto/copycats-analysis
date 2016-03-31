@ECHO OFF
SET username=%1
SET password=%2
SET postgreBin=%3

:: create user without a password
%postgreBin%createuser.exe --createdb --username postgres --no-createrole  --unencrypted %username%
:: set password
%postgreBin%psql.exe -c "ALTER ROLE %username% WITH PASSWORD '%password%';" postgres postgres
:: update pgpass.conf file with the password
ECHO localhost:5432:*:%username%:%password%>>%APPDATA%\postgresql\pgpass.conf

ECHO If no error messages were shown, user "%username%" created succesfully, the password is set to be "%password%".