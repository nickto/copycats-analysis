source("./r/02-connect-to-db.R")
source("./r/04-copy-performance-functions.R")

s <- Sys.time()
write(paste("\nStarted at:", s), file = "log.txt",
          append = TRUE, sep = " ")

# get list of of unique fund identifiers (wfcin)
wfcinList <- getWfcinList()
# get average cash-TNA ratio
averageCash <- getAverageCash()

# create schema for temporay tables (there is at least one in the code)
sql_command <- paste0("create schema if not exists tmp")
dbSendQuery(con, sql_command)
# create schema for resulting tables
sql_command <- paste0("create schema if not exists performance")
dbSendQuery(con, sql_command)

# Loop through all funds
set.seed(1200)
for (wfcin in (sample_n(as.data.frame(wfcinList), 1))[,1]) {
    # add timing information
    t <- system.time({

    # run whole code through tryCatsh to avoid stopping the program due to errors
    # in a fund
    errorInCalculations <- FALSE
    errorInWriteToDb <- FALSE
    tryCatch(suppressWarnings({
        # get start and end dates of fund for analysis
        startEndDates <- getStartEndDates(wfcin)
        # check whether there is a period to analyze
        if(startEndDates[1,start_date] > startEndDates[1,end_date]) {
            # start date is greater than end date: skip this fund
            next()
        }


        # get periods with key dates
        # The returned table has the following columns:
        # - wfcin: unique fund identifier
        # - period_no:             number of period
        # - new_holdings:          flag whether there should be a rebalancing
        # - returns_from_date:     date (including) from which to calculate returns
        # - returns_to_date:       date (including) till which to calculate returns
        # - stock_data_date:       date at which to look share-level information of
        #                          holdings (used to calculate initial weights)
        # - holdings_fdate:        fdate of current holdings (used for linking)
        # - holdings_returns_from: date from which current holdings started to
        #                          accumulate returns (used for calculating current
        #                          weights)
        periodKeyDates <- getPeriodDates(wfcin, startEndDates)

        # get holdings data, weights as of the first date of holdings information
        holdingsData <- getHoldingsData(wfcin, periodKeyDates)

        # get all stock data
        stockData <- getAllStockData(holdingsData, startEndDates)


        # add cusips to period key dates and extract each period return
        # also add buy and sell price:
        # - buy price is the price on the day before the beginning of the period
        #   (the day on which the stock is actually bought)
        # - sell price is the price on the last day of the period
        periodStockData <- getPeriodStockData(periodKeyDates, holdingsData, stockData)


        # get gross returns of stocks in the portfolio for the period
        periodStockReturns <- getPeriodStockReturns(periodStockData, periodKeyDates)
        periodCashReturns <- getPeriodCashReturns(periodKeyDates)
        periodOtherReturns <- getPeriodOtherReturns(periodKeyDates)

        periodReturns <- mergeReturns(periodStockReturns, periodCashReturns, periodOtherReturns)

        # get weights and returns of each class
        returnsWeightsOfClasses <- getCategoriesData(periodKeyDates, periodReturns, averageCash)


        # add stock trading costs
        costComponents <- getTradingCosts(periodKeyDates, returnsWeightsOfClasses,
                                          stockData)
        setkey(costComponents, period_no)

        # get fund returns
        periodReturns <- getPeriodReturns(returnsWeightsOfClasses, costComponents,
                                          periodKeyDates)

        # get monthly returns
        monthlyReturns <- getMonthlyReturns(periodReturns)

        # get actual monthly returns of a fund
        actualReturns <- getActualReturns(wfcin, startEndDates)

        # merge copied and actual returns
        monthlyReturns <- merge(
            monthlyReturns,
            actualReturns,
            by = c("year", "month"),
            all = TRUE
        )

        # add wfcin for identification purposes
        monthlyReturns[, wfcin := wfcin]


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


    # add information to log
    messageToLog <- addLogEntry(wfcin, t, errorInCalculations, errorInWriteToDb)

    # print message
    print(messageToLog)



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