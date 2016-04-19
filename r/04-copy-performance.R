s <- Sys.time()
source("./r/02-connect-to-db.R")
source("./r/functions.R")

# Move everything to data base

# Date at which to start the analysis (if you change it to earlier, it won't
# work anyway, because there is not data downloaded for earlier points in time)
analysisStartDate <- as.Date('1990-01-01')


# get list of of unique fund identifiers
wfcinList <- getWfcinList()


# Count fund numbrs do display progress
nWfcin <- length(wfcinList)
iWfcin <- 1

# Loop through all funds
set.seed(182)
for (wfcin in (sample_n(as.data.frame(wfcinList), 5))[,1]) {
    #wfcin <- 102441 # problematic
    # wfcin <- 107167 # working
    #wfcin <- 106730 # long
    #wfcin <- 410706 # another problematic fund (straight line)


    # Process outline:
    # 1. select monthtly fund returns
    # 2. select all information about fund holdings
    # 3. work with daily copycat returns on the following timeline:
    #   all dates from stock return data base that are between the first
    #   holding disclosure date and the last fund return date

    # Select all monthly returns for the current funds
    fundReturns <- getFundReturns(wfcin, frequency = "monthly")
    if(isEmptyDataFrame(fundReturns)) {next()}
    # Select fund holdings information
    holdings <- getFundHoldings(wfcin)
    if(isEmptyDataFrame(holdings)) {next()}
    holdingsDates <- getHoldingsDates(holdings)
    holdingsDates <- holdingsDates[holdingsDates >= analysisStartDate]
    # Obtain start and end date of our analysis
    startDate <- min(holdingsDates)
    endDate <- min(
        max(fundReturns$caldt),
        max(holdingsDates) + 183
    )


    # Check if the fund has some info in the analysis period
    if(startDate >= endDate) {next()}

    # Select all information about holdings that we will need later
    stockData <- getStockData(wfcin, startDate, endDate)
    if(isEmptyDataFrame(stockData)) {next()}
    # Select dates on which holdings have changed
    stockDataDates <- getStockDataDates(stockData)


    # List of dates to work with
    dateList <- sort(stockDataDates[stockDataDates >= startDate &&
                                   stockDataDates <= endDate])

    # This data frame stores returns
    copycatReturns <- data.table(
        date = dateList,
        stockR = as.numeric(NA),    # domestic equity return
        stockW = as.numeric(NA),    # domestic equity weight
        stockTc = as.numeric(NA),   # trading costs expressed as negative returns
        cashR = as.numeric(NA),     # cash return
        cashW = as.numeric(NA),     # cash weight
        bondR = as.numeric(NA),     # bond returns
        bondW = as.numeric(NA),     # bond weight
        fStockR = as.numeric(NA),   # foreign stock return
        fStockW = as.numeric(NA))   # foreign stock weight

    portfolio <- data.table()
    for (today in dateList) {
        # Calculate returns (this is done always, unless this is the first day,
        # in which case portfolio variable is an empty data frame)
        if(!isEmptyDataFrame(portfolio)) {
            # Domestic equity
            portfolioData <- getPortfolioData(portfolio,
                                              stockData,
                                              dateList,
                                              today)

            portfolioReturn <- getPortfolioReturn(portfolioData)

            # store domestic equity return
            copycatReturns[date == today, stockR := portfolioReturn]
            # adjust for splots
            portfolio <- splitAdjust(portfolioData)
        }

        # Check if we need to update our portfolio
        # Appropriate holdings date. NULL if there is no need to update holdings
        hDate <- updateHoldingsDate(today, holdingsDates, dateList)

        if (!is.null(hDate) && hDate != -Inf) {
            print(paste(as.Date(hDate), as.Date(dateList[length(dateList)])))
            portfolio <- getPortfolio(holdings, hDate)
        }


    }


    # cR <- select(copycatReturns, date, stockR) %>%
    #     mutate(year = year(date), month = month(date)) %>%
    #     mutate(ym = paste(year, month, sep = "-")) %>%
    #     select(ym, stockR)
    # cR <- summarize(group_by(cR, ym),
    #                 ret = exp(sum(log(stockR + 1), na.rm = TRUE)) - 1)
    #
    # fR <- select(fundReturns, caldt, mret) %>%
    #     mutate(year = year(caldt), month = month(caldt)) %>%
    #     mutate(ym = paste(year, month, sep = "-")) %>%
    #     select(caldt, ym, mret)
    #
    # tmp <- full_join(cR, fR, by = 'ym') %>%
    #     rename(date = caldt, copied = ret, actual = mret) %>%
    #     select(date, actual, copied)
    #
    # tmp <- xts(select(tmp, actual, copied), order.by = tmp$date)
    # png(paste0('./export/images/', wfcin, ".png"))
    # print(autoplot(tmp))
    # dev.off()

    print(paste0(iWfcin,"/",nWfcin))
    iWfcin = iWfcin + 1
    gc()
}


print(Sys.time() - s)