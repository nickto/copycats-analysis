# Set up directories and variable values
# Set up directory locations
psql=/cygdrive/c/Program\ Files/PostgreSQL/9.5/bin/psql.exe
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


# Call SQL
"$psql" -U $username -d $database -f  "./sql/create-clean-dataset.sql"
