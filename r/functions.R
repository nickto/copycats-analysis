# This file contains functions.


source("./r/02-connect-to-db.R")

isEmptyDataFrame <- function(df) {
    if (dim(df)[1] == 0 && dim(df)[2] == 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

getWfcinList <- function() {
# This function returns the vector of all wfcin -- unique fund identifiers from
# MFLINKS
    sql_command <- "
    SELECT DISTINCT
        wfcin
    FROM
        clean.deoni_wfcin
    "
    wfcinList <- dbGetQuery(con, sql_command)

    return(as.vector(wfcinList[,1]))
}

getStockData <- function(wfcin, startDate, endDate) {
# This function return data frame with information about all stocks (that are
# present in the database) that have ever been in funds holdings. This includes
# cusip, date, various prices, returns, exchange, shares outstanding

    sql_command <- paste0("
        with cusips as (
          select DISTINCT
            cusip
          from clean.holdings_wfcin
          where
            wfcin = ", wfcin, "
        )

        select
          c.cusip,
          s.date,
          s.openprc,
          s.prc,
          s.bid,
          s.ask,
          s.ret,
          s.dlret,
          s.shrout,
          s.exchcd
        from cusips as c
        left join stocks.daily as s on
          c.cusip = s.ncusip
        where
          s.date >= '", as.Date(startDate), "' AND
          s.date <= '", as.Date(endDate), "'
        order by
          date,
          cusip
    ")
    stockData <- dbGetQuery(con, sql_command)

    # repalce missing values indicated by -66, -77, -88, -99 with proper NA
    stockData$ret[stockData$ret %in% c(-66,-77,-88,-99)] <- NA
    stockData$dlret[is.na(stockData$dlret)] <- 0

    return(stockData)
}

getHoldingsDates <- function(holdings) {
# This function returns a vector of dates on which holdings have changed
    dates <- select(holdings, fdate) %>% distinct()
    return(dates[,1])
}

getStockDataDates <- function (stockData) {
    dates <- select(stockData, date) %>% distinct()
    return(dates[,1])
}



getPortfolioChanges <- function(oldPortfolio, newPortfolio) {
# This function returns differences between two portfolios.

    # Check if old portfolio is empty and then basically return the new
    # portfolio
    if (dim(oldPortfolio)[1] == 0 && dim(oldPortfolio)[2] == 0) {
        both <- mutate(newPortfolio, sharesOld = 0,
                       sharesNew = shares,
                       change = shares) %>%
                    select(cusip, sharesOld, sharesNew, change)
        return(both)
    }

    # Both portfolios contain somethind, therefore actually calculate the
    # difference
    both <- full_join(oldPortfolio, newPortfolio, by = 'cusip') %>%
        rename(sharesOld = shares.x, sharesNew = shares.y) %>%
        select(cusip, sharesOld, sharesNew) %>%
        mutate(sharesOld = replace(sharesOld, is.na(sharesOld), 0),
               sharesNew = replace(sharesNew, is.na(sharesNew), 0)) %>%
        mutate(change = sharesNew - sharesOld)

    return(both)
}


getFundReturns <- function(wfcin, frequency = "daily") {
# This function extracts all available returns at a specified frequency (daily/
# monthly) for a given fund.
#
# It returns the data frame with the following columns
# - caldt
# - crsp_fundno
# - tna
# - ret
# - nav
# - wfcin

    if (frequency == "daily") {
        sql_command <- paste0("
        select
            *
        from
            clean.dret_wfcin
        where
            wfcin = ", wfcin ,"
        ")
        fundReturns <- dbGetQuery(con, sql_command)
    } else if (frequency == "monthly") {
        sql_command <- paste0("
        select
            *
        from
            clean.mret_wfcin
        where
            wfcin = ", wfcin ,"
        ")
        fundReturns <- dbGetQuery(con, sql_command)
    } else {
        stop("Wrong argument value.")
    }

    return(fundReturns)
}

getFundHoldings <- function(wfcin) {
# This function returns holdings of a fund.
#
# it returns data frame with 4 columns:
# - fdate
# - cusip
# - shares
# - wfcin

    sql_command <- paste0("
        select
            fdate,
            cusip,
            shares,
            wfcin
        from
          clean.holdings_wfcin
        where
          wfcin = ", wfcin, "
    ")
    holdings <- dbGetQuery(con, sql_command)

    return(holdings)
}

getPortfolioData <- function(portfolio, stockData, dateList, today) {
# This function returns the portfolio together with the neccesaary infromation
# to calculate return
    prevDay <- max(dateList[dateList < today])

    currStockData <- filter(stockData, date == today)
    prevStockData <- filter(stockData, date == prevDay)

    portfolioData <-
    left_join(portfolio, currStockData, by = 'cusip') %>%
        left_join(select(prevStockData, cusip, prc), by = 'cusip') %>%
        rename(prc = prc.x, prcPrev = prc.y) %>%
        mutate(weight = prcPrev * shares /
                   sum (prcPrev * shares, na.rm = TRUE))
    return(portfolioData)
}

getPortfolio <- function(holdings, date) {
# This function returns portfolio at a specified date. (It finds the most
# recent information in holdings)

    # date from which to get holdings. It can be different from supplied date
    hDate <- max(distinct(filter(select(holdings, fdate), fdate <= today))[,1])

    newHoldings <- select(holdings, fdate, cusip, shares) %>%
        filter(fdate == hDate )

    return(newHoldings)
}

getPortfolioReturn <- function(portfolioData) {
    return(with(portfolioData, sum(weight * ret, na.rm = TRUE)))
}

updateHoldingsDate <- function(today, holdingsDates, dateList) {
    if (today %in% holdingsDates) {
        return(today)
    } else {
        hDate <- max(holdingsDates[holdingsDates <= today])
        prevDate <- max(dateList[dateList < today], na.rm = TRUE)
        if (hDate > prevDate) {
            return(hDate)
        } else {
            return(NULL)
        }
    }
}


createIndeces <- function(returns) {
    indeces <- data.frame()
    indeces[1,"date"] <- returns[1,"date"]
    indeces[1,"actual"] <- 1
    indeces[1,"copy"] <- 1
    for (i in 2:nrow(returns)) {
        indeces[i,"date"] <- returns[i,"date"]

        if (!is.na(returns[i,"actualRet"])) {
            indeces[i,"actual"] <- indeces[i - 1,"actual"] *
                (1 + returns[i,"actualRet"] )
        } else {
            indeces[i,"actual"] <- indeces[i - 1,"actual"]
        }

        if (!is.na(returns[i,"stockRet"])) {
            indeces[i,"copy"] <- indeces[i - 1,"copy"] *
                (1 + returns[i,"stockRet"] )
        } else {
            indeces[i,"copy"] <- indeces[i - 1,"copy"]
        }

    }
    indeces$date <- as.Date(indeces$date)
    return(indeces)
}




