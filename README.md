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

Data can be also found [here](https://mega.nz/#F!EhpUlTyR), however, this link is encrypted, because it is probably prohibited to share this data. [Here](https://mega.nz/#F!9hARSITJ) is the PostgreSQL dump, which contains enough data to run analysis that is alrady performed in R (not in shell or SQL scripts).

### Set up database
Data base is set up in [PostgreSQL 9.5.1](http://www.enterprisedb.com/products-services-training/pgdownload#windows) under Windows 10 operating system. The bash script is intended to run using the cygwin environment setting the working directory to the project directory. The bash script is called [create-databse.sh](./create-database.sh). It performs the following tasks:

1. Creates a user
2. Creates a data base
3. Creates schemas in the data base
4. Creates tables
5. Populates tables with data from .csv files downloaded from WRDS.
6. Manipulates databases including creation of a "clean" schema which contains data for more convenient linking of CRSP and Thomson Reuters data bases.

The script will skip already accomplished steps automatically.

Batch files, might require additional manual configuration before running them:

- Set PostgreSQL bin folder
- Add [7-zip](http://www.7-zip.org/download.html) folder.
- Set usernames, passwords, etc for database.
- SQL files in `~/sql/` directory that start with words `copy` or `import` require changing the directory of raw data.

So overall, the minimum input from a user to set up a database is to install PostgreSQL, 7-zip and change paths described above.

### Perform analysis in R
R scripts are divided into logical chunks that are stored in separate numbered files and are intended to run in that order (but one might not want to run them all in one run):

1. `01-load-packages.R`: this file loads (and downloads if necessary) packages that are required for analysis.
2. `02-connect-to-db.R`: this file sets up connections to the data base.
3. `03-export-cusips.R`: this file creates a file with the list of stocks information about which should be downloaded from WRDS. This might be tricky as it should be run somewhere in the middle of `create-database.sh` script, therefore, the simplest way is to just download it from the repo and the `create-database.sh` will work. 
4. `04-copy-performance-functions.R`: this file contains functions used in the `04-copy-performance.R`.
5. `04-copy-performance.R`: this file creates the performance of copycat funds. 
6. `05-analysis-functions.R`: this file contains functions used `05-analysis.R`
7. `05-analysis.R`: this files performs analysis of copycat performance.

## Analysis

The following analysis is performed (in the `05-analysis.R`):

- We calculate means of various indicators such as gross, after trading costs and net returns of original (primitive) funds and copycats. Respective t-statistics and p-values are also reported.
- We calculate means of various indicators such as gross, after trading costs and net returns of original (primitive) funds and copycats for each year. Respective t-statistics and p-values are also **not** reported.
- We perform decile sorting of funds based on various indicators using data for the last 12 months. We perform this sorting once in every 3 months. Then we compare various performance indicators for each decile and for bottom-minus-top decile.
- We perform decile sorting of funds based on various indicators using data for the last 12 months. We perform this sorting once in every 3 months. Then we calculate Carhart's alphas of various performance indicators for each decile and for bottom-minus-top decile.

## TO-DO:

- Decile sorting base on Carhart's alphas.
- Describe the analysis part.
