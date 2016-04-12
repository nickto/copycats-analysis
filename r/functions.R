# This file contains functions.


source("./r/02-connect-to-db.R")

portfolioTemplate <- function(wfcin) {
# This function creates empty portfolio for a fund, given its wfcin.
# It populates it with rows that have cusip for all possible portfolio
# cusip values.
#
# It returns the data frame that is return have following colmns:
# - cusip
# - wfcin
# - fdate
# - date
# - shares
# - price
# - weight
# - value
# - return

    # extract all cusips of of stocks that the mutual fund have ever had
    sql_command <- paste0("
    select distinct
        cusip
    from
        tr.holdings as h
        where
    fdate in (
        select
        fdate
        from
        clean.deoni_wfcin
        where
        wfcin = ", wfcin, "
    ) and
    fundno in (
        select
        fundno
        from
        clean.deoni_wfcin
        where
        wfcin = ", wfcin, "
    )")

    portfolio <- data.frame(cusip <- dbGetQuery(con, sql_command))
    portfolio$wfcin <- wfcin
    portfolio$fdate <- NA
    portfolio$date <- NA
    portfolio$shares <- 0
    portfolio$price <- NA
    portfolio$weight <- NA
    portfolio$value <- NA
    portfolio$return <- NA

    portfolio <- arrange(portfolio, wfcin, fdate,cusip)
    return(portfolio)
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
            crsp.daily_returns
        where
        crsp_fundno in (
            select
                crsp_fundno
            from
                clean.deoni_wfcin
            where
                wfcin = ", wfcin ,"
            limit 1
        )
        ")
        fundReturns <- dbGetQuery(con, sql_command)
    } else if (frequency == "monthly") {
        sql_command <- paste0("
        select
            *
        from
            crsp.monthly_returns
        where
        crsp_fundno in (
            select
                crsp_fundno
            from
                clean.deoni_wfcin
            where
                wfcin = ", wfcin ,"
            limit 1
        )
        ")
        fundReturns <- dbGetQuery(con, sql_command)
    } else {
        stop("Wrong argument value.")
    }

    fundReturns$wfcin <- wfcin

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
    select distinct
        fdate,
        cusip,
        shares
    from
        tr.holdings as h
    where
        fdate in (
            select
                fdate
            from
                clean.deoni_wfcin
            where
                wfcin = ", wfcin, "
        ) AND
        fundno in (
            select
                fundno
            from
                clean.deoni_wfcin
            where
                wfcin = ", wfcin, "
        )
        order by
            fdate")
    holdings <- dbGetQuery(con, sql_command)
    holdings$wfcin <- wfcin

    return(holdings)
}


getFundHoldingsChange <- function(wfcin) {
# This function returns holdings change of a fund.
#
# it returns data frame with 4 columns:
# - fdate
# - cusip
# - change
# - wfcin


    sql_command <- paste0("
    select
        fdate,
        cusip,
        change
    from
        tr.holdings_change as hc
    where
        fdate in (
            select
                fdate
            from
                clean.deoni_wfcin
            where
                wfcin = ", wfcin, "
        ) AND
        fundno in (
            select
                fundno
            from
                clean.deoni_wfcin
            where
                wfcin = ", wfcin, "
        )
        order by
            fdate")
    holdingsChange <- dbGetQuery(con, sql_command)
    holdingsChange$wfcin <- wfcin

    return(holdingsChange)
}

getPortfolio <- function(wfcin, date) {
    sql_command <- paste0("
    select
        fdate,
        cusip,
        shares
    from
        tr.holdings as h
    where
        fdate in (
            select
                fdate
            from
                clean.deoni_wfcin
            where
                wfcin = ", wfcin, "
        ) AND
        fundno in (
            select
                fundno
            from
                clean.deoni_wfcin
            where
                wfcin = ", wfcin, "
        ) AND
        fdate = (
            select
                max(fdate)
            from
                tr.holdings as h
            where
                fdate in (
                    select
                        fdate
                    from
                        clean.deoni_wfcin
                    where
                        wfcin = ", wfcin, "
                ) AND
                    fundno in (
                        select
                            fundno
                        from
                            clean.deoni_wfcin
                        where
                            wfcin = ", wfcin, "
                ) AND
                fdate <= '", as.Date(date), "'
        )

    ")

    holdings <- dbGetQuery(con, sql_command)
    portfolio <- portfolioTemplate(wfcin)
    portfolio <- fillPortfolioWithHoldings(portfolio, holdings)
    portfolio$date <- as.Date(date)
    portfolio$fdate <- as.Date(holdings$fdate[1])

    portfolio <- arrange(portfolio,wfcin,fdate,cusip)
    return(portfolio)
}

fillPortfolioWithHoldings <- function(portfolio, holdings) {
    for (i in 1:nrow(holdings)) {
        currentStock <- holdings[i,]
        cusip <- currentStock$cusip

        portfolio$shares[portfolio$cusip == cusip] <- currentStock$shares
    }

    return(portfolio)
}


getPortfolioChanges <- function(oldPortfolio, newPortfolio) {
    portfolio1 <- arrange(oldPortfolio,wfcin,fdate,cusip)
    portfolio2 <- arrange(newPortfolio,wfcin,fdate,cusip)


    changes <- portfolio1
    changes$change <- portfolio2$shares - portfolio1$shares
    changes$shares <- NULL
    changes$price <- NULL
    changes$wight <- NULL
    changes$value <- NULL
    changes$return <- NULL
    changes$fdate <- NULL
    changes$date <- NULL
    changes$fdateOld <- portfolio1$fdate
    changes$fdateNew <- portfolio2$fdate
    changes$dateOld <- portfolio1$date
    changes$dateNew <- portfolio2$date

    return(changes)
}

fillPortfolioData <- function(portfolio) {
    for(i in 1:nrow(portfolio)) {
        if(portfolio$shares[i]==0) {
            next()
        }

        cusip <- portfolio$cusip[i]
        date <- portfolio$date[i]

        sql_command <- paste0("
            select
                cusip,
                date,
                prc,
                ret
            from
                stocks.daily
            where
                cusip = '", cusip, "'
                and
                date = '", date, "'
        ")

        stockData <- dbGetQuery(con, sql_command)

        if(dim(stockData)[1] > 0) {
            portfolio$price[i] <- stockData[1,"prc"]
            portfolio$return[i] <- stockData[1,"ret"]

            if (portfolio$return[i] %in% c(-66,-77,-88,-99)) {
                portfolio$return[i] <- NA
            }
        } else {
            portfolio$price[i] <- NA
            portfolio$return[i] <- NA
        }

    }

    portfolio$value <- portfolio$shares * portfolio$price
    totalValue <- sum(portfolio$value, na.rm = TRUE)

    for(i in 1:nrow(portfolio)) {
        portfolio$weight[i] <- portfolio$value[i] / totalValue
    }

    return(portfolio)
}



