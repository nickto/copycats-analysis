# Analysis of Copycat Mutual Fund Startegies
This repo contains code to perform analysis of performance of strategies that attempt to copy other mutual funds based on reported holdings.

## Structure
This repo contains scripts needed to set up a data base and perform analysis. Data base in PostgreSQL is set up using bash script files. Analysis is performed using R scripts.

## Getting data

1. Download data
2. Set up data base
3. Manipulate data in database
4. Manipulate data in R

### Download 
Data is downloaded from [WRDS](https://wrds-web.wharton.upenn.edu/) web-site. The process is described in [getting-data.html](http://htmlpreview.github.com/?https://github.com/nickto/copycats-analysis/blob/master/getting-data.html) in details.

Data can be also found [here](https://mega.nz/#F!EhpUlTyR), however, this link is encrypted, because it is probably prohibited to share this data :)

### Set up database
Data base is set up in [PostgreSQL 9.5.1](http://www.enterprisedb.com/products-services-training/pgdownload#windows) under Windows 10 operating system. The bash script is inteded to run using the cygwin environment setting the working directory to the project directory. The bash script is called [create-databse.sh](./create-database.sh). It performs the following tasks:

1. Creates a user
2. Creates a data base
3. Creates schemas in the data base
4. Creates tables
5. Populates tables with data from .csv files downloaded from WRDS.
6. Manipulates databases (currently this includes only indexing)

The script will skip already accomplished steps automatically.

Batch files, might require additional manual configuration before running them:

- Set PostgreSQL bin folder
- Add [7-zip](http://www.7-zip.org/download.html) folder 
- Set usernames, passwords, etc for database.

So overall, the minimum input from a user to set up a database is to install PostgreSQL, 7-zip and change paths described above.

### Perform analysis in R
R scripts are divided into logical chunks that are stored in separate numbered files and are intended to run in that order:

- `00-load-packages.R` loads required packages. Should run before any other file.
- `01-code-definitions.R` loads descriptions of codes used in data bases. (Note: currently used only in deprecated files.)
- ~~`02-read-tr-data-(depreacated).R`: reads data into ff objects and stored them on disk.~~ 
- ~~`03-create-db-(depreacated).R`: creates PostgreSQL data base from ff-objects stored on disk.~~ 
- `04-extract-data.R`: extracts data from database (Currently just prints a few sample graphs using data from database). 

## Analysis
Analysis is yet to be done

## TO-DO:

- ~~Rewrite BATCHs as bash scipts!!!~~
- Import CRSP data into databse
- Perform analysis :)
