#!/bin/bash

# This shell script creates a database and sets it up for further convenient
# analysis in R.

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
echo ---------------------------------------------------------------------------

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
echo ---------------------------------------------------------------------------

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
echo ---------------------------------------------------------------------------

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

# mflinks schema
# The following query returns nothing (is empty) if the databse exist.
schema=mflinks
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

# stocks schema
# The following query returns nothing (is empty) if the databse exist.
schema=stocks
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
echo ---------------------------------------------------------------------------

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
    echo Table $schema.$table does not exist. Creating new tables.
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
    echo Table $schema.$table does not exist. Creating new tables.
    "$psql" -U $username -d $database -f "./sql/create-tables-$schema.sql"
    if [ $? -ne 0 ] ; then
	echo Creating tables in $schema exited with errors.
	exit 2
    fi
    echo Tables in $schema created succesfully. 
fi

# Create mflinks tables
# Check if tables already exist. (Check only one table and assume that if one
# table exist, then the other ones too, by the nature of the script: if it has
# been run once, then all tables should already exist)
schema=mflinks
table=link2 # last table in the script

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
    echo Table $schema.$table does not exist. Creating new tables.
    "$psql" -U $username -d $database -f "./sql/create-tables-$schema.sql"
    if [ $? -ne 0 ] ; then
        echo Creating tables in $schema exited with errors.
        exit 2
    fi
    echo Tables in $schema created succesfully.
fi

# Create mflinks tables
# Check if tables already exist. (Check only one table and assume that if one
# table exist, then the other ones too, by the nature of the script: if it has
# been run once, then all tables should already exist)
schema=stocks
table=daily # last table in the script

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
    echo Table $schema.$table does not exist. Creating new tables.
    "$psql" -U $username -d $database -f "./sql/create-tables-$schema.sql"
    if [ $? -ne 0 ] ; then
        echo Creating tables in $schema exited with errors.
        exit 2
    fi
    echo Tables in $schema created succesfully.
fi


# Populate tables from csv files
echo ---------------------------------------------------------------------------
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

    # Some of the tables need to be cleaned before they can be imported into 
    # Postgres
    echo Create temporary files.
    zcat ./data/raw/wrds-tr-s12-type1-197901-201509.csv.gz | tr -d \
	'\200-\377' | gzip > \
 	./data/clean/wrds-tr-s12-type1-197901-201509.csv.gz
    echo Temporary type 1 data files created succesfully.


    "$psql" -U $username -d $database -f \
	"./sql/copy-csv-$schema.sql"
    if [ $? -ne 0 ] ; then
	echo Populating $schema tables exited with errrors.
	exit 2
    fi
    echo Tables $schema populated succesfully.

    echo Remove temporary files
    rm ./data/clean/*

    if [ $? -ne 0 ] ; then
	echo Removing temporary files exited with errors.
	exit 2
    fi
    echo Temporary tr files removed succesfully.
fi

# crsp
# Check if tables are already populated. (Again, only check one table)
schema=crsp # last table in the script
table=rear_load # last table in the script 

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
    # Daily returns
    # Replace incorrect missing value characters with nothing
    zcat ./data/raw/wrds-crsp-mfdb-daily-returns-19980901-20151231.csv.gz | \
	awk -F, -f ./awk/crsp-daily-return.awk | \
	gzip > ./data/clean/wrds-crsp-mfdb-daily-returns-19980901-20151231.csv.gz
    echo Temporary clean daily returns file created succesfully.

    # Fund portfolio map
    # Clean file from non-ASCII characters
    zcat ./data/raw/wrds-crsp-mfdb-fund-portfolio-map.csv.gz | tr -d \
	'\200-\377' | gzip > \
 	./data/clean/wrds-crsp-mfdb-fund-portfolio-map.csv.gz
    echo Temporary clean fund portfolio map file created succesfully.
    
    # Fund summary
    # Change three quotes to one quote and clean from non-ASCII characters
    TEMP=$(mktemp)
    zcat ./data/raw/wrds-crsp-mfdb-fund-summary-196112-2015-12.csv.gz | \
	sed -e 's/\"\"\"/\"/g' | tr -d '\200-\377' | gzip > $TEMP
    echo Temporary file with removed excess quotation created.
    # Replace incorrect missing values characters with nothing
    zcat $TEMP | \
	awk -vFPAT='[^,]*|"[^"]*"' -f./awk/crsp-fund-summary.awk | \
	gzip > ./data/clean/wrds-crsp-mfdb-fund-summary-196112-2015-12.csv.gz
    echo Temporary fund summary files created succesfully.
    rm $TEMP
    echo Temporary file with removed excess quotation removed.
    echo Temporary clean fund summary file created succesfully.

    # Monthly returns
    # Replace incorrect missing values characters with nothin
    zcat ./data/raw/wrds-crsp-mfdb-monthly-returns-196112-2015-12.csv.gz | \
	awk -F, -f ./awk/crsp-monthly-return.awk | \
	gzip > ./data/clean/wrds-crsp-mfdb-monthly-returns-196112-2015-12.csv.gz
    echo Temporary daily returns file created succesfully.

    # Portfolio holdings
    # Clean from non-ASCII characters
    zcat ./data/raw/wrds-crsp-mfdb-portfolio-holdings-200101-2015-12.csv.gz | \
	tr -d '\200-\377' | gzip > \
	./data/clean/wrds-crsp-mfdb-portfolio-holdings-200101-2015-12.csv.gz 
    echo Temporary portfolio holdings file created succesfuly.

    echo Import data
    "$psql" -U $username -d $database -f \
    "./sql/copy-csv-$schema.sql"
    if [ $? -ne 0 ] ; then
	echo Populating $schema tables exited with errrors.
	exit 2
    fi
    echo Tables $schema populated succesfully.
    
    echo Remove temporary files
    rm ./data/clean/*

    if [ $? -ne 0 ] ; then
	echo Removing temporary files exited with errors.
	exit 2
    fi
    echo Temporary crsp files removed succesfully.
fi

# mflinks
# Check if tables are already populated. (Again, only check one table)
schema=mflinks # last table in the script
table=link2 # last table in the script

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

    # Clean file from non-ASCII characters
    cat ./data/mflinks/link1.csv | tr -d '\200-\377' | \
        sed 's/,"/,QUOTEPLACEHOLDER/g' | \
        sed 's/",/QUOTEPLACEHOLDER,/g' | \
        sed 's/"$/QUOTEPLACEHOLDER/g' | \
        sed 's/"//g'        | \
        sed 's/QUOTEPLACEHOLDER/"/g' > \
        ./data/clean/link1.csv
    echo Temporary clean link1 file created succesfully.
    cat ./data/mflinks/link2.csv | tr -d '\200-\377' > \
        ./data/clean/link2.csv
    echo Temporary clean link2 file created succesfully.

    "$psql" -U $username -d $database -f \
        "./sql/copy-csv-$schema.sql"
    if [ $? -ne 0 ] ; then
        echo Populating $schema tables exited with errrors.
        exit 2
    fi
    echo Tables $schema populated succesfully.

    echo Remove temporary files
    rm ./data/clean/*
    if [ $? -ne 0 ] ; then
	echo Removing temporary files exitted with errors.
	exit 2;    
    fi
    echo Temporary files removed succesfully.
fi

# stocks
# Check if tables are already populated. (Again, only check one table)
schema=stocks # last table in the script
table=daily # last table in the script

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

    # Replace incorrect missing values characters with nothing
    echo Create temporary daily returns file.
    zcat ./data/raw/wrds-crsp-stocks-daily-19900101-20153112.csv.gz | \
	awk -F, -f ./awk/crsp-stocks-daily.awk | \
	gzip > ./data/clean/wrds-crsp-stocks-daily-19900101-20153112.csv.gz
    echo Temporary daily returns file created succesfully.
   
     "$psql" -U $username -d $database -f \
        "./sql/copy-csv-$schema.sql"
    if [ $? -ne 0 ] ; then
        echo Populating $schema tables exited with errrors.
        exit 2
    fi
    echo Tables $schema populated succesfully.

    echo Remove temporary files
    rm ./data/clean/*
    if [ $? -ne 0 ] ; then
	echo Removing temporary files exitted with errors.
	exit 2;    
    fi
    echo Temporary files removed succesfully.

fi
# Index tables
echo ---------------------------------------------------------------------------

# Create table indeces
echo Create indeces

"$psql" -U $username -d $database -f './sql/index-tables.sql'

# Import cash and other data
bash import-cash-other-returns.sh

# Create clean dataset
bash create-clean-dataset.sh



exit 0

