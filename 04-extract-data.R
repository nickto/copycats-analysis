source("./00-load-packages.R")

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

# # template
# sql_command <- "
# "
# dbGetQuery(con, sql_command)


# get list of funds
sql_command <- "
SELECT *
    FROM tr.fund_characteristics
WHERE ioc = 3
OFFSET 90000
LIMIT 100
"
fundList <- dbGetQuery(con, sql_command)

for (i in 1:length(fundList$fundno)) {
    fno <- fundList$fundno[i]
    sql_command <- paste0("
    SELECT *
        FROM tr.detailed_holdings
        WHERE fundno = ", fno, "
    ")
    fundHoldingsDF <- dbGetQuery(con, sql_command)


    sql_command <- paste0("
    SELECT
        fundno,
        fdate,
        SUM(prc * shares)
        FROM tr.detailed_holdings
    WHERE fundno = ", fno, "
    GROUP BY fundno, fdate
    ORDER BY fdate ASC
    ")
    fundDetailedHoldingsDF <- dbGetQuery(con, sql_command)

    sql_command <- paste0("
    SELECT
        *
        FROM tr.fund_characteristics
    WHERE fundno = ", fno, "
    ")
    rawDf <- dbGetQuery(con, sql_command)

    # common data frame
    commonDF <- merge(x = fundDetailedHoldingsDF, y = rawDf, by = "fdate", all = TRUE)
    commonDF <- commonDF[, c("fdate", "assets", "sum")]
    commonDF$assets <- commonDF$assets * 10000



    if (!dir.exists("export")) dir.create("export")
    if (!dir.exists("export/images")) dir.create("export/images")
    png(paste0("export/images/", fno, ".png"))
    plot(commonDF$fdate, commonDF$assets)
    lines(commonDF$fdate, commonDF$assets, col = "blue")
    lines(commonDF$fdate, commonDF$sum, col = "red")
    dev.off()
}


# template
sql_command <- "
SELECT *
    FROM tr.fund_characteristics
    WHERE ioc = 3
    OFFSET 10012
    LIMIT 1
"
dbGetQuery(con, sql_command)

