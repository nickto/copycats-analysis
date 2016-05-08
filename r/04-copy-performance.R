source("./r/02-connect-to-db.R")
source("./r/04-copy-performance-functions.R")

s <- Sys.time()
write(paste("\nStarted at:", s), file = "log.txt",
          append = TRUE, sep = " ")

#-------------------------------------------------------------------------------
# things common for all funds

# get database query results
# get list of of unique fund identifiers (wfcin)
# wfcinList <- sort(getWfcinList())    # wfcinList <- sort(getWfcinList(), decreasing = TRUE)
# wfcin list for funds with errors
wfcinErrors <- fread("error-wfcin-from-log.txt")
wfcinList <- wfcinErrors[Copy == "ERROR", wfcin]
print(paste("Total number of funds in this run:", length(wfcinList)))
# get average cash-TNA ratio
averageCash <- getAverageCash()
# get list of stock dates
stockDatesAll <-getStockDates()
# get daily cash returns
cashReturn <- getCashReturn()
# get daily other returns
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
set.seed(1200)
fundNo <- 0
for (wfcin in wfcinList) { # for (wfcin in wfcinList) { # for (wfcin in (sample_n(as.data.frame(wfcinList), 3))[,1]) {
    # wfcin <- 106067
    logMessage <- paste(
            paste0(inc(fundNo), ":"),
            wfcin,
            sep = "\t"
        )



    # add timing information
    t <- system.time({

    # run whole code through tryCatch to avoid stopping the program due to errors
    # in a fund
    errorInCalculations <- FALSE
    errorInWriteToDb <- FALSE
    tryCatch(suppressWarnings({



        # get holdings of this fund
        holdings <- getHoldings(wfcin, stockDatesAll, startEndDates)

        # get start and end dates for analysis
        startEndDates <- getStartEndDates(holdings)
        # check whether there is a period to analyze
        if(startEndDates[1,start] >= startEndDates[1,end]) {
            # start date is greater than end date: skip this fund
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

        # subset holdings
        holdings <- subsetHoldings(holdings, startEndDates)

        # get stock information
        stocks <- getStockData(holdings, startEndDates)

        # get periods with key dates
        periods <- getPeriodDates(startEndDates, stockDatesAll, holdings)

        # get returns at the stock level
        setkey(stocks, cusip, date)
        stockLevelReturnsList <- getStockLevelGrossReturns(periods, holdings, stocks)

        # get changes in weights at the stock level
        stockLevelChangesList <- getStockLevelChanges(periods, stockLevelReturnsList)

        # get proprtion of asset classes for each period
        periodProportions <- getPeriodProportions(wfcin, periods, averageCash)

        # get stock level trading costs
        stockLevelTradingCosts <- getStockLevelTradingCosts(periodProportions, stockLevelChangesList, stocks)

        # get gross returns and cost terms for each period
        periodReturnsAndCosts <- getPeriodReturnsAndCosts(stockLevelReturnsList, stockLevelTradingCosts)

        # get period returns of all asset classes
        periodReturns <- getPeriodReturns(periodReturnsAndCosts, periods, cashReturn, otherReturn)

        # get monthly returns of copycat
        monthlyCopycatReturns <- getMonthlyCopycatReturns(periodReturns)

        # add data on actual returns
        monthlyReturns <- getActualReturns(monthlyCopycatReturns, wficn, averageExpenseRatio)


    }), error=function(cond) {
        message(paste("Error in fund calculations; wfcin = ", wfcin))
        errorInCalculations <<- TRUE
    #}, warning=function(cond) {
    #    print("warning")
    #    print(cond)
    },finally=function(cond){
        # maybe we should write something here... but later!
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
        #    print("warning")
        #    print(cond)
        },finally=function(cond){
            # maybe we should write something here... but later!
        })

    }


    }) # this bracket ends system.time() function


    #  add information to log
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

    if(fundNo == 1) {
        addLogEntry(logMessage, "log.txt", TRUE)
    } else {
        addLogEntry(logMessage, "log.txt")
    }

    # print message
    cat(paste(logMessage, "\n"))



    # graphs
    # ind <- monthlyReturns
    # ind[, index_net_original := cumprod(1 + net_ret_act)]
    # ind[, index_net_copied := cumprod(1 + net_ret_10m_cop)]
    # ind[, index_gross_original := cumprod(1 + gross_ret_act)]
    # ind[, index_gross_copied := cumprod(1 + gross_ret_cop)]
    #
    # indN <- as.xts.data.table(ind[, list(caldt,index_net_original,index_net_copied)])
    # indG <- as.xts.data.table(ind[, list(caldt,index_gross_original,index_gross_copied)])
    #
    # autoplot(indG) +
    #     ggtitle("Comparison of gross return indeces") +
    #     xlab("Date") +
    #     ylab("Index value")
    # g$panel$layout$variable <- c("Copycat", "Original")
    # grid.draw(ggplot_gtable(g))
    # autoplot(indN) +
    #     ggtitle("Comparison of net return indeces") +
    #     xlab("Date") +
    #     ylab("Index value")
    #
    # retN <- as.xts.data.table(ind[, list(caldt,net_ret_act,net_ret_10m_cop)])
    # retG <- as.xts.data.table(ind[, list(caldt,gross_ret_act,gross_ret_cop)])
    #
    # autoplot(retN)
    # autoplot(retG)



    #print(paste0(iWfcin,"/",nWfcin))
    #iWfcin = iWfcin + 1
    gc()

}

print(paste("Finished in:", Sys.time() - s))



#dbDisconnect(con); rm(list = ls())