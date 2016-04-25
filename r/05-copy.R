source("./r/02-connect-to-db.R")
source("./r/05-functions.R")

s <- Sys.time()

# get list of of unique fund identifiers (wfcin)
wfcinList <- getWfcinList()
# get average cash-TNA ratio
averageCash <- getAverageCash()

sql_command <- paste0("create schema if not exists tmp")
dbSendQuery(con, sql_command)

# Loop through all funds
set.seed(548)
for (wfcin in (sample_n(as.data.frame(wfcinList), 2))[,1]) {
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

    # tmpN <- as.xts.data.table(monthlyReturns[, list(caldt, net_ret_50m, net_ret)])
    # tmpG <- as.xts.data.table(monthlyReturns[, list(caldt, gross_return, gross_ret)])
    #
    #
    # autoplot(tmpG)
    # autoplot(tmpN)





    #print(paste0(iWfcin,"/",nWfcin))
    #iWfcin = iWfcin + 1
    gc()

    print("one fund done")
}

print(Sys.time() - s)



#dbDisconnect(con); rm(list = ls())