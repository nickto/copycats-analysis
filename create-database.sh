#!/bin/bash

# TODO
# experiment with psql variable name so that it points directly to psql.exe
# create a table

# This shell script creates a database and sets it up for further convenient
# analysis in R.

# It accepts the following arguments
# placeholder for arguments


# Set up directories and variable values
# Set up directory locations
psql=/cygdrive/c/Program\ Files/PostgreSQL/9.5/bin/psql.exe
createuser=/cygdrive/c/Program\ Files/PostgreSQL/9.5/bin/createuser.exe
zipDir="/cygdrive/c/Program Files/7-zip/"
copycatsDir=$(pwd)
copycatsDirWin="D:/Cloud\ Storages/GitHub/copycats-analysis/"

# Set up usernames and passwords (not recommended to change)
username=copycat
dbname=copycats
password=default-password-for-copycats
database=copycats

# default username and databse (username was provided during installation 
# process; database is the one that already exist)
postgresUser=postgres
postgresDb=postgres


# Change directory to PostgreSQL/bin directory
# cd "$psqlDir"

# Create a user
echo ----------------------------------------------------------------------------

# Check if user exists
# The following query returns nothing (is empty) if the user does not exist.
sqlOutput=`"$psql" -U $postgresUser -tAc \
    "SELECT 1 FROM pg_user WHERE usename = '$username';"`

if [[ ${#sqlOutput} -gt 0 || $sqlOutput == "1" ]]
then
    # User exists. No need to create
    echo User $username already exists. No need to create a new one.
else
    # User does no exist
    echo User $username does not exist. Creating a new one.
    "$createuser" --createdb --username $postgresUser --no-createrole \
	--unencrypted $username
    # Check for error codes after creating a user
    if [ $? -ne 0 ] ; then
	echo Creating user exited with errors. 
	exit 2
    fi
    
    echo User $username created succesfuly.


    echo Changing $username\'s password.
    "$psql" -c "ALTER ROLE $username WITH PASSWORD '$password';" $postgresDb \
	$postgresUser
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
	exit 2
    fi
    echo Password stored in pgpass.conf succesfully.
fi


# Create copycats database
echo ----------------------------------------------------------------------------

# Check if database exists
# The following query returns nothing (is empty) if the databse exist.
sqlOutput=`"$psql" -U $postgresUser -tAc \
    "SELECT 1 FROM pg_database WHERE datname = '$database';"`

if [[ ${#sqlOutput} -gt 0 || $sqlOutput == "1" ]]
then
    # Databse exists. No need to create
    echo Databse $database already exists. No need to create a new one.
else
    # Databse does no exist
    echo Databse $database does not exist. Creating a new one.
    "$psql" -c "CREATE DATABASE $database" $postgresDb $username
    
    if [ $? -ne 0 ] ; then
	echo Creating a databse $database exited with errors.
	exit 2
    fi 
    echo Database $databse created succesfully.
fi


# Create schemas
echo ----------------------------------------------------------------------------

# Check if schemas exist
# tr schema
# The following query returns nothing (is empty) if the databse exist.
schema=tr
sqlOutput=`"$psql" -U $username -d $database -tAc \
    "SELECT 1 FROM information_schema.schemata WHERE schema_name = '$schema';"`

if [[ ${#sqlOutput} -gt 0 || $sqlOutput == "1" ]]
then
    # Schema exists. No need to create
    echo Schema $schema already exists. No need to create a new one.
else
    #Schema does no exist
    echo Schema $schema does not exist. Creating a new one.
    "$psql" -U $username -d $database -c \
	"CREATE SCHEMA $schema"
    if [ $? -ne 0 ] ; then
	echo Creating a schema $schema exited with errors.
	exit 2
    fi
    echo Schema $schema created succesfully.
fi

# crsp schema
# The following query returns nothing (is empty) if the databse exist.
schema=crsp
sqlOutput=`"$psql" -U $username -d $database -tAc \
    "SELECT 1 FROM information_schema.schemata WHERE schema_name = '$schema';"`

if [[ ${#sqlOutput} -gt 0 || $sqlOutput == "1" ]]
then
    # Schema exists. No need to create
    echo Schema $schema already exists. No need to create a new one.
else
    # Schema does no exist
    echo Schema $schema does not exist. Creating a new one.
    "$psql" -U $username -d $database -c \
	"CREATE SCHEMA $schema"
    if [ $? -ne 0 ] ; then
	echo Creating a schema $schema exited with errors.
	exit 2
    fi
    echo Schema $schema created succesfully.
fi


# Create tables
echo ----------------------------------------------------------------------------

# Create tr tables
# Check if tables already exist. (Check only one table and assume that if one
# table exist, then the other ones too, by the nature of the script: if it has
# been run once, then all tables should already exist)
schema=tr
table=holdings_change # last table in the script

sqlOutput=`"$psql" -U copycat -d copycats -tAc \
    "SELECT 1 FROM information_schema.tables \
     WHERE table_schema = '$schema' \
     AND tables.table_name = '$table';"`

if [[ ${#sqlOutput} -gt 0 || $sqlOutput == "1" ]]
then
    # Table exists. No need to create
    echo Table $schema.$table already exists. No need to create new $schema \
	tables.
else
    # Table does no exist
    echo Schema $schema.$table does not exist. Creating new tables.
    "$psql" -U $username -d $database -f "./sql/create-tables-$schema.sql"
    if [ $? -ne 0 ] ; then
	echo Creating tables in $schema exited with errors.
	exit 2
    fi
    echo Tables in $schema created succesfully. 
fi

# Create crsp tables
# Check if tables already exist. (Check only one table and assume that if one
# table exist, then the other ones too, by the nature of the script: if it has
# been run once, then all tables should already exist)
schema=crsp
table=portfolio_holdings # last table in the script

sqlOutput=`"$psql" -U copycat -d copycats -tAc \
    "SELECT 1 FROM information_schema.tables \
     WHERE table_schema = '$schema' \
     AND tables.table_name = '$table';"`

if [[ ${#sqlOutput} -gt 0 || $sqlOutput == "1" ]]
then
    # Table exists. No need to create
    echo Table $schema.$table already exists. No need to create new $schema \
	tables.
else
    # Table does no exist
    echo Schema $schema.$table does not exist. Creating new tables.
    "$psql" -U $username -d $database -f "./sql/create-tables-$schema.sql"
    if [ $? -ne 0 ] ; then
	echo Creating tables in $schema exited with errors.
	exit 2
    fi
    echo Tables in $schema created succesfully. 
fi


# Populate tables from csv files
echo ----------------------------------------------------------------------------
export PATH=$PATH:"$zipDir"

# tr
# Check if tables are already populated. (Again, only check one table)
schema=tr # last table in the script
table=holdings_change # last table in the script

sqlOutput=`"$psql" -U copycat -d copycats -tAc \
    "SELECT * FROM $schema.$table LIMIT 1;"`


if [ ${#sqlOutput} -ne 0 ] 
then
    # Table is populated. Not need to populate again
    echo $schema.$table contains at least one entry. No need to import data into\
	$schema tables.
else
    # Table is not populated. Need to populate it
    echo $schema.$table contains no entries. Populating $schema tables.    
    "$psql" -U $username -d $database -f \
	"./sql/copy-csv-$schema.sql"
    if [ $? -ne 0 ] ; then
	echo Populating $schema tables exited with errrors.
	exit 2
    fi
    echo Tables $schema populated succesfully.
fi

# crsp
# Check if tables are already populated. (Again, only check one table)
schema=crsp # last table in the script
table=portfolio_holdings # last table in the script 

sqlOutput=`"$psql" -U copycat -d copycats -tAc \
    "SELECT * FROM $schema.$table LIMIT 1;"`


if [ ${#sqlOutput} -ne 0 ] 
then
    # Table is populated. Not need to populate again
    echo $schema.$table contains at least one entry. No need to import data into\
	$schema tables.
else
    # Table is not populated. Need to populate it
    echo $schema.$table contains no entries. Populating $schema tables.    

    # Some of the tables need to be cleaned before they can be imported into 
    # Postgres

    echo Create temporary files.
#    zcat ./data/raw/wrds-crsp-mfdb-daily-returns-19980901-20151231.csv.gz | \
#	awk -F, -f ./awk/crsp-daily-return.awk | \
#	gzip > ./data/clean/wrds-crsp-mfdb-daily-returns-19980901-20151231.csv.gz

    echo Temporary daily returns files created succesfully.




#    "$psql" -U $username -d $database -f \
#	"\copy crsp.daily_returns FROM PROGRAM \
#	'7z x `$copycatsDirWin`data/raw/wrds-crsp-mfdb-daily-returns-19980901-20151231.csv.gz -so'
#	DELIMITER ',' 
#	CSV HEADER ;"


    "$psql" -U $username -d $database -f \
	"./sql/copy-csv-$schema.sql"
    if [ $? -ne 0 ] ; then
	echo Populating $schema tables exited with errrors.
	exit 2
    fi
    echo Tables $schema populated succesfully.
    
    echo Remove temporaty files.
    a=0
#    rm ./data/clean/wrds-crsp-mfdb-daily-returns-19980901-20151231.csv.gz
    a=$(($a + $?))

    if [ $? -ne 0 ] ; then
	echo Removing temporary files exited with errors.
	exit 2
    fi
    echo Temporary files removed succesfully.
fi






exit 0
