#!/bin/bash

# This shell script creates a database and sets it up for further convenient
# analysis in R.

# It accepts the following arguments
# placeholder for arguments


# Set up directories and variable values
# Set up directory locations
psqlDir=/cygdrive/c/Program\ Files/PostgreSQL/9.5/bin/
copycatsDir=$(pwd)

# Set up usernames and passwords (not recommended to change)
username=copycattest
dbname=copycats
password=default-password-for-copycats


# Create a user
# Change directory to PostgreSQL/bin directory
cd "$psqlDir"

# Check if user exists
# The following query returns nothing (is empty) if the user does not exist.
sqlOutput=`./psql.exe -U postgres -tAc "SELECT 1 FROM pg_user WHERE usename =\
    '$username';"`

if [[ ${#sqlOutput} -gt 0 || $sqlOutput == "1" ]]
then
    # User exists. No need to create
    echo User $username already exists. No need to create a new one.
else
    # User does no exist
    echo User $username does not exist. Creating a new one.
    ./createuser.exe --createdb --username postgres --no-createrole \
	--unencrypted $username
    # Check for error codes after creating a user
    if [ $? -ne 0 ] ; then
	echo Creating user exited with errors. 
	exit 2
    fi
    
    echo User $username created succesfuly.


    echo Changing $username\'s password.
    ./psql.exe -c "ALTER ROLE $username WITH PASSWORD '$password';" postgres postgres
    # Check for error codes after changing the password of the user
    if [  $? -ne 0 ] ; then
	echo Changing password exited with errors.
	exit 2
    fi
    echo Password for user $username changed succesfully.

    # Append password to password management file (to avoid typing password for
    # this user again
    echo "localhost:5432:*:$username:$password">>$APPDATA/postgresql/pgpass.conf
    if [ $? -ne 0 ] ; then
	echo Storing password in pgpass.conf exited with errors.
	exit 
    fi
    echo Password stored in pgpass.conf succesfully.

    

fi

# Create user
# ./createuser.exe --createdb --username postgres --no-createrole --unencrypted $username
# Change password for the created user












exit 0
