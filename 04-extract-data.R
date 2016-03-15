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
    #fno <- fundList$fundno[i]
    fno <- 1389

    # obtain holdings of a fund
    sql_command <- paste0("
        SELECT
            tr.holdings.*,
            tr.stock_characteristics.stkname,
            tr.stock_characteristics.ticker,
            tr.stock_characteristics.ticker2,
            tr.stock_characteristics.exchcd,
            tr.stock_characteristics.stkcd,
            tr.stock_characteristics.stkcdesc,
            tr.stock_characteristics.shrout1,
            tr.stock_characteristics.prc,
            tr.stock_characteristics.shrout2,
            tr.stock_characteristics.indcode
        FROM tr.holdings
        LEFT OUTER JOIN tr.stock_characteristics ON
            (
                tr.holdings.cusip = tr.stock_characteristics.cusip AND
                tr.holdings.fdate = tr.stock_characteristics.fdate
            )
        WHERE fundno = ", fno, "
    ")
    holdingsDF <- dbGetQuery(con, sql_command)

    # calculate derived value (shares time their price)
    holdingsDF$value <- with(holdingsDF, {value <- prc * shares})

    # store these values
    assets <- aggregate(holdingsDF$value,
                        by = list(holdingsDF$fdate),
                        FUN = sum,
                        na.rm = TRUE)

    names(assets) <- c("fdate", "derived")

    # obtain reported assets value
    sql_command <- paste0("
        SELECT
            fdate,
            assets,
            rdate
        FROM tr.fund_characteristics
            WHERE fundno = ", fno, "
    ")
    reportedDF <- dbGetQuery(con, sql_command)
    # adjust b ya factor of 10000
    with(reportedDF, assets <- assets * 10000)

    # merge two tables
    commonDF <- merge(assets, reportedDF, by = "fdate", ALL = TRUE)
    names(commonDF) <- c("fdate", "derived", "reported", "rdate")


    if (!dir.exists("export")) dir.create("export")
    if (!dir.exists("export/images")) dir.create("export/images")
    png(paste0("export/images/", fno, ".png"))
    with(commonDF, {
        plot(fdate, reported)
        lines(fdate, reported, col = "blue")
        lines(fdate, derived, col = "red")
    })

    dev.off()
}




