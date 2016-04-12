source("./r/02-connect-to-db.R")
source("./r/functions.R")


# get list of of unique fund identifiers
sql_command <- "
SELECT DISTINCT
    wfcin
FROM
    clean.deoni_wfcin
"
wfcinList <- dbGetQuery(con, sql_command)


# Count fund numbrs do disaply progress
nWfcin <- dim(wfcinList)[1]
iWfcin <- 1

# Loop through all funds
for (wfcin in wfcinList[3598,1]) {
    # Select all daily returns for the current funds
    fundReturns <- getFundReturns(wfcin, frequency = "daily")

    # replicate performance
    # Create a data frame with cusips that the funds ever had and store there
    # current share number. Update share numbers whenever there is a change in
    # portfolio holdings.
    # For every date, find TNA of portfolio. Calculate return for that date.
    # Add this return to grossReturnsDf

    # TODO:
    # - tradings costs
    # - amount of cash (not forgetting that it also bears some interest)
    # - fix the NAs problem in returns


    holdings <- getFundHoldings(wfcin)
    holdingsChange <- getFundHoldingsChange(wfcin)


    startDate <- min(holdings$fdate)
    endDate <- max(fundReturns$caldt)
    dateList <- fundReturns$caldt[fundReturns$caldt >= startDate &
                                     fundReturns$caldt <= endDate]



    oldPortfolio <-portfolioTemplate(wfcin)
    # For every day that returns are available for
    for (date in dateList) {
        portfolio <- getPortfolio(wfcin, date)
        if(date == startDate) {
            next()
        }
        #print(portfolio[1,])

        portfolio <- fillPortfolioData(portfolio)
        stockReturn <- sum(portfolio$weight * portfolio$return, na.rm = TRUE)
        print(stockReturn)

    }
    print(portfolio)

    source("./r/functions.R")
    portfolio <- getPortfolio(wfcin, '1998-10-30')
    fillPortfolioData(portfolio)



    # pritn p
    # print(paste0(iWfcin,"/",nWfcin))
    # iWfcin = iWfcin + 1

}

