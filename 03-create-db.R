source("./00-load-packages.R")
# Commong settings
dataDir <- "data"
rawDataDir <- paste(dataDir, "raw", sep = "/")
ffdfDataDir <- paste(dataDir, "ffdf", sep = "/")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
    "thesis-password"
}
#
# # loads the PostgreSQL driver
# drv <- dbDriver("PostgreSQL")
# # creates a connection to the postgres database
# # note that "con" will be used later in each connection to the database
# con <- dbConnect(drv, dbname = "postgres",
#                  host = "localhost", port = 5432,
#                  user = "openpg", password = pw)
# rm(pw) # removes the password
#
# # check for the cartable
# dbExistsTable(con, "test")
# # TRUE



load.ffdf(dir = paste(ffdfDataDir, "trFundCharac", sep = "/"))

# ffapply(X = unique(trFundCharac$country),
#         AFUN = function(x){
#             x <- as.character(x)
#             print(max(nchar(x)))
#             # return nchar(x)
#             })
#
# max(trFundCharac$assets[!is.na.ff(trFundCharac$assets),])


# connect to Postgre
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "template1",
                 host = "localhost", port = 5432,
                 user = "openpg", password = pw)

# create database
sql_command <- "CREATE DATABASE master_thesis"
dbGetQuery(con, sql_command)

# close this connection
dbDisconnect(con)

# connect to this database
con <- dbConnect(drv, dbname = "master_thesis",
                 host = "localhost", port = 5432,
                 user = "openpg", password = pw)

# create schema
sql_command <- "CREATE SCHEMA tr"
dbGetQuery(con, sql_command)

# create table (TEST TABLE AT THIS POINT IN TIME)
sql_command <- "
CREATE TABLE
tr.holdings_charac (
    fdate date,
    fundno integer,
    fundname varchar(32),
    mgrcoab varchar(8),
    rdate date,
    assets integer,
    ioc varchar(32),
    prdate date,
    country varchar(32),
    PRIMARY KEY(fdate, fundno)
)"
dbGetQuery(con, sql_command)

sql_command <- "
INSERT INTO tr.test
VALUES
    (1, 2, 'aaaa')
"
dbGetQuery(con, sql_command)

sql_command <- "
INSERT INTO tr.test
VALUES
    (1, 1, 'aaaa')
"
dbGetQuery(con, sql_command)

sql_command <- "
SELECT *
FROM tr.test
"
a <- dbGetQuery(con, sql_command)


dbDisconnect(con)




colClassVector <- c("Date",
                    "integer",
                    "factor",
                    "factor",
                    "Date",
                    "integer",
                    "factor",
                    "Date",
                    "factor")



sql_command <- "
CREATE TABLE
testtable (
    column1 integer,
    column2 smallint,
    column3 varchar(255),
    PRIMARY KEY(column1, column2)
)"
dbGetQuery(con, sql_command)