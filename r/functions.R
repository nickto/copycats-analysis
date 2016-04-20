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

getAverageCash <- function(){
    sql_command <- "
    with avg_by_year as (
      select
        date_part('year', asset_dt),
        avg(per_cash)
      from clean.crsp_fs_wfcin
      group by
        date_part('year', asset_dt)
      order by
        date_part('year', asset_dt)
    )
    select
      avg(avg)
    from avg_by_year
    "
    avg <- dbGetQuery(con, sql_command)

    return(avg[1,1]/100)
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
          --s.openprc,
          s.prc,
          --s.bid,
          --s.ask,
          s.ret,
          s.dlret,
          s.shrout,
          s.exchcd,
          s.cfacshr
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
    stockData <- as.data.table(dbGetQuery(con, sql_command))

    # repalce missing values indicated by -66, -77, -88, -99 with proper NA
    stockData[ret %in% c(-66,-77,-88,-99), ret := NA]

    # replace dlret with zeroes
    stockData[is.na(dlret), dlret := 0]

    # remove duplicates, which for some reason is present
    stockData <- unique(stockData, by = c("cusip", "date"))

    return(stockData)
}

getCategoryData <- function(holdingsDates, wfcin) {
    sql_command <- paste0("
        select distinct
          wfcin,
          caldt,
          asset_dt,
          tna_latest * 1000000 as tna_latest,
          per_com / 100 as per_com,
          per_cash / 100 as per_cash,
          per_bond / 100 as per_bond
        from
          clean.crsp_fs_wfcin
        where wfcin = ", wfcin,"
    ")
    cDataQuery <- as.data.table(dbGetQuery(con, sql_command))
    cDataQuery[, stockW := per_com]
    cDataQuery[, cashW := per_cash]

    # now combine it with holdings
    if(!isEmptyDataFrame(cDataQuery)) {
        # there is at least some information about categories
        cData <- data.table(
            fdate = as.Date(holdingsDates),
            caldt = as.Date(NA))

        for(i in 1:nrow(cData)) {
            curFdate <- cData[i, fdate]
            curCaldt <- max(cDataQuery[caldt <= curFdate, caldt])
            cData[i, caldt := curCaldt]
        }

        cData <- merge(cData,
                       cDataQuery[,c("caldt","asset_dt","tna_latest","stockW","cashW"),with=FALSE],
                       by = 'caldt')
        cData[,otherW := as.numeric(NA)]
    } else {
        # there is no information
        cData <- data.table(
            fdate = as.Date(holdingsDates),
            caldt = as.Date(NA),
            asset_dt = as.Date(NA),
            tna_latest = as.numeric(NA),
            stockW = as.numeric(NA),
            cashW = as.numeric(NA),
            otherW = as.numeric(NA))
    }
    return(cData)
}

getHoldingsDates <- function(holdings) {
# This function returns a vector of dates on which holdings have changed
    dateKey <- setkey(holdings, fdate)
    dates <- unique(holdings, by = key(dateKey))[,fdate]

    return(dates)
}

getStockDataDates <- function (stockData) {
    dateKey <- setkey(stockData, date)
    dates <- unique(stockData, by = key(dateKey))[,date]

    return(dates)
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

    return(as.data.table(fundReturns))
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
            wfcin,
            fdate,
            cusip,
            shares
        from
          clean.holdings_wfcin
        where
          wfcin = ", wfcin, "
    ")
    holdings <- dbGetQuery(con, sql_command)

    return(as.data.table(holdings))
}

getPortfolio <- function(holdings, hDate) {
# This function returns portfolio at a specified date. (It finds the most
# recent information in holdings)
    newHoldings <- holdings[fdate == hDate, .(fdate, cusip, shares)]
    return(newHoldings)
}

getPortfolioData <- function(portfolio, stockData, dateList, today) {
# This function returns the portfolio together with the neccesaary infromation
# to calculate return
    prevDay <- max(dateList[dateList < today])

    currStockData <- stockData[date == today]
    prevStockData <- stockData[date == prevDay, .(cusip, prc, cfacshr)]

    setkey(currStockData, cusip)
    setkey(prevStockData, cusip)

    portfolioData <- merge(portfolio, currStockData, by = 'cusip',
                           all.x = TRUE, all.y = FALSE)

    portfolioData <- merge(portfolioData,
                           prevStockData,
                           by = 'cusip', all.x = TRUE, all.y = FALSE)
    setnames(portfolioData,
             c("prc.x", "prc.y"),
             c("prc", "prcPrev"))
    portfolioData[, split := cfacshr.y / cfacshr.x]
    portfolioData[, cfacshr.y := NULL]
    portfolioData[, cfacshr.x := NULL]
    portfolioData[, weight := prcPrev * shares /
                      sum (prcPrev * shares, na.rm = TRUE)]

    return(portfolioData)
}

getPortfolioReturn <- function(portfolioData) {
    return(with(portfolioData, sum(weight * ret, na.rm = TRUE)))
}

splitAdjust <- function(portfolioData) {
    portfolioData[!is.na(split) & split != 1, shares := shares * split]
    return(portfolioData[, .(fdate, cusip, shares)])
}

updateHoldingsDate <- function(today, holdingsDates, dateList) {
# Returns the date at which to look for holding information. If there is no new
# holding information, returns NULL
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

getNewWeights <- function(copycatReturns, today, yesterday) {
    # Stocks
    wS <- copycatReturns[date == yesterday, stockW]

    # Cash
    wC <- copycatReturns[date == yesterday, cashW]

    # Check if cash/stock data is missing
    if(is.na(wS) && is.na(wC)) {
        wC <- averageCash
        wS <- 1 - averageCash
    } else if (is.na(wS)) {
        wC <- 1 - wS
    } else if (is.na(wC)){
        wS <- 1 - wC
    }

    # Other (bonds and foreign equity)
    wO <- copycatReturns[date == yesterday, otherW]
    # Check this type of data is also missing
    if (is.na(wO)) {
        wO <- 1 - wS - wC
    }


    # Returns
    rS <- copycatReturns[date == today, stockR]
    rS <- ifelse(is.na(rS), 0, rS)
    rC <- copycatReturns[date == today, cashR]
    rC <- ifelse(is.na(rC), 0, rC)
    rO <- copycatReturns[date == today, otherR]
    rO <- ifelse(is.na(rO), 0, rO)


    # Total weight
    wT <- wS * (1 + rS) + wC * (1 + rC) + wO * (1 + rO)

    # Write data to copycatReturns
    copycatReturns[date == today, stockW := (1 + rS) * wS / wT]
    copycatReturns[date == today, cashW := (1 + rC) * wC / wT]
    copycatReturns[date == today, otherW := (1 + rO) * wO / wT]

    return(copycatReturns)
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





