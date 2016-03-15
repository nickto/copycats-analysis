source("./00-load-packages.R")
# Commong settings
dataDir <- "data"
rawDataDir <- paste(dataDir, "raw", sep = "/")
ffdfDataDir <- paste(dataDir, "ffdf", sep = "/")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
    "default-password-for-copycats"
}

# # check for the cartable
# dbExistsTable(con, "test")

# connect to Postgre
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "copycats",
                 host = "localhost", port = 5432,
                 user = "copycat", password = pw)

# create schema
sql_command <- "
SELECT *
FROM tr.stock_characteristics
LIMIT 100
OFFSET 900000
"
dbGetQuery(con, sql_command)






load.ffdf(dir = paste(ffdfDataDir, "trHoldingsChg", sep = "/"))

ffapply(X = unique(trStockCharac$stkcdesc),
        AFUN = function(x){
            x <- as.character(x)
            print(max(nchar(x)))
            })

min(trHoldingsChg$change[!is.na.ff(trHoldingsChg$change),])
