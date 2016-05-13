# Overall outline of the procedure in this file.
# First we load packages, function, and connect to the database.
# Then we extract information that is common for all funds from the database and
# store it in the memory.
# Then we set up databses: create schemas (tmp schema is a legacy. Can be
# removed).
#

source("./r/02-connect-to-db.R")
source("./r/04-copy-performance-functions.R")

s <- Sys.time()
write(paste("\nStarted at:", s), file = "log.txt",
          append = TRUE, sep = " ")

#-------------------------------------------------------------------------------
# things common for all funds

# get database query results
# get list of of unique fund identifiers (wfcin)
wfcinList <- sort(getWfcinList())
# get average cash-TNA ratio
averageCash <- getAverageCash()
# get list of stock dates
stockDatesAll <-getStockDates()
# get daily cash returns
cashReturn <- getCashReturn()
# get daily other returns (note that after a change, this is not from db already)
otherReturn <- getOtherReturn()
# get average monthly expense ratio
averageExpenseRatio <- getAverageExpenseRatio()


# set up database for saving results
# create schema for temporay tables (there is at least one in the code)
sql_command <- paste0("create schema if not exists tmp")
dbSendQuery(con, sql_command)
# create schema for resulting tables
sql_command <- paste0("create schema if not exists performance")
dbSendQuery(con, sql_command)
#-------------------------------------------------------------------------------

# Loop through all funds
# is needed for random sampling. Does not affect anything if run on whole sample
set.seed(1036)
sampleSize <- 300

fundNo <- 0
# for whole sample:
# for (wfcin in wfcinList) {
# for random sample:
for (wfcin in (sample_n(as.data.frame(wfcinList), sampleSize))[,1]) {
    # start log message for this fund
    logMessage <- paste(
            paste0(inc(fundNo), ":"),
            wfcin,
            sep = "\t"
        )

    # add timing information
    t <- system.time({

    # run whole code through tryCatch to avoid stopping the program due to errors
    # in a fund. Errors are mostly when fund holdings containg stocks that
    # are not present in the CRSP database.
    errorInCalculations <- FALSE
    errorInWriteToDb <- FALSE
    tryCatch(suppressWarnings({

        # get holdings of this fund
        holdings <- getHoldings(wfcin, stockDatesAll, startEndDates)

        # get start and end dates for analysis
        startEndDates <- getStartEndDates(holdings)
        # check whether there is a period to analyze
        if(startEndDates[1,start] >= startEndDates[1,end]) {
            # start date is greater than end date: skip this fund, add log
            # message
            logMessage <- paste(
                    logMessage,
                    "skip",
                    "skip",
                    "-",
                    sep = "\t"
                )
            if(fundNo == 1) {
                addLogEntry(logMessage, "log.txt", TRUE)
            } else {
                addLogEntry(logMessage, "log.txt")
            }
            next()
        }

        # subset holdings from start to end date
        holdings <- subsetHoldings(holdings, startEndDates)

        # get stock information about all stocks in the holdings of the fund
        stocks <- getStockData(holdings, startEndDates)

        # get periods with key dates. Period is a period of time from the
        # first working day of the month to the last. If there is a rebalancing
        # in the middle of the month, then the month gets splitted into two
        # periods, where first is from the beginning of the month till the
        # rebalancing date, and the second is from the rebalancing date to the
        # end of the month
        periods <- getPeriodDates(startEndDates,
                                  stockDatesAll,
                                  holdings)

        # get returns at the stock level
        setkey(stocks, cusip, date) # ensure that DT is sorted in a right way
        stockLevelReturnsList <- getStockLevelGrossReturns(periods,
                                                           holdings,
                                                           stocks)

        # get changes in weights at the stock level on rebalancing dates
        stockLevelChangesList <- getStockLevelChanges(periods,
                                                      stockLevelReturnsList)

        # get proprtion of asset classes for each period
        periodProportions <- getPeriodProportions(wfcin,
                                                  periods,
                                                  averageCash)

        # get stock level trading costs
        stockLevelTradingCosts <- getStockLevelTradingCosts(periodProportions,
                                                            stockLevelChangesList,
                                                            stocks)

        # get gross returns and cost terms for each period
        periodReturnsAndCosts <- getPeriodReturnsAndCosts(stockLevelReturnsList,
                                                          stockLevelTradingCosts)

        # get period returns of all asset classes
        periodReturns <- getPeriodReturns(periodReturnsAndCosts,
                                          periods,
                                          cashReturn,
                                          otherReturn)

        # get monthly returns of copycat
        monthlyCopycatReturns <- getMonthlyCopycatReturns(periodReturns)

        # add data on actual (primitive funds) returns
        monthlyReturns <- getActualReturns(monthlyCopycatReturns,
                                           wficn,
                                           averageExpenseRatio)


    }), error=function(cond) {
        message(paste("Error in fund calculations; wfcin = ", wfcin))
        errorInCalculations <<- TRUE
    #}, warning=function(cond) {
    #   warning is commented, because in this case we are interested only in
    #   errors
    },finally=function(cond){
        # we do not need to take any actions
    })


    # write to db only if there were no errors in calculations
    if(!errorInCalculations) {
        # write table to the database
        tryCatch(suppressWarnings({
            writeTableToCopycats(monthlyReturns, wfcin)
        }),error=function(cond) {
            message(paste("Error in fund writing to db; wfcin = ", wfcin))
            errorInWriteToDb <<- TRUE
        #}, warning=function(cond) {
        #   warning is commented, because in this case we are interested only in
        #   errors
        },finally=function(cond){
            # we do not need to take any actions
        })

    }

    }) # this bracket ends system.time() function


    #  add error information to log
    if(errorInCalculations) {
        logMessage <- paste(logMessage,
                            "ERROR",
                            "skip",
                            sep = "\t")
    } else {
        logMessage <- paste(logMessage,
                            "OK",
                            sep = "\t")
        if(!errorInWriteToDb) {
            logMessage <- paste(logMessage,
                                "OK",
                                sep = "\t")
        } else {
            logMessage <- paste(logMessage,
                                "ERROR",
                                sep = "\t")
        }
    }

    # add time information to log
    logMessage <- paste(logMessage,
                        round(t["elapsed"], 2),
                        sep = "\t")

    # print to log files
    if(fundNo == 1) {
        addLogEntry(logMessage, "log.txt", TRUE)
    } else {
        addLogEntry(logMessage, "log.txt")
    }

    # print log message to the screen
    cat(paste(logMessage, "\n"))

    # force garbage collector
    gc()
}

print(paste("Finished in:", Sys.time() - s))


# disconnect from database and remove all vairbales. This is needed for testing
# purposes to make a clean run.
# dbDisconnect(con); rm(list = ls())