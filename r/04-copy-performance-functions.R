inc <- function(x, n = 1){
  # This function is analogoues to x++
  eval.parent(substitute(x <- x + n))
}

isEmptyDataTable <- function(dt) {
    # This function checks if data table is empty. An empty data table is
    # defined as a data table where either the number of rows or columns is
    # zero.

    if (dim(dt)[1] == 0 && dim(dt)[2] == 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

getWfcinList <- function() {
    # This function returns the vector of all wfcin -- unique fund identifiers from
    # MFLINKS.

    # Also exclude funds that have more than 120% equity in at least one period,
    # or have less than 0% equity in at least one period.

    sql_command <- "
        with
          min_max_per_com as (
            select
              wfcin,
              min(per_com),
              max(per_com)
            from clean.crsp_fs_wfcin
            group by wfcin
          )

        SELECT DISTINCT
          w.wfcin,
          mm.min,
          mm.max
        FROM clean.deoni_wfcin as w
        left join min_max_per_com as mm on
          w.wfcin = mm.wfcin
        where
          mm.min > 0 and
          mm.max < 120 AND
          w.wfcin is not null
        ORDER BY mm.max desc
    "

    wfcinList <- dbGetQuery(con, sql_command)

    return(as.vector(wfcinList[,1]))
}

getAverageCash <- function(){
    # This function calculates average cash proportion held by mutual funds.
    # It calculates it equally-weighted average across funds for each year and
    # then averages it across years in our analaysis.

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

    return(avg[1,1]/100) # /100 becayse original units are percents
}

getStockDates <- function() {
    # This function returns a data frame with all the dates at which the
    # infomation about stocks is available.
    #
    # Tabke clean.stock_dates contains all unique dates from stocks.daily
    # for which information about at laest 100 stocks is available. This is
    # done because for some dates, information ony about 5-6 stocks is available
    # and then we should exclude this day from analysis to avoid errros.

    sql_command <- "
        select
          *
        from clean.stock_dates
        order by date
    "
    stockDates <- as.data.table(dbGetQuery(con, sql_command))
    setnames(stockDates,
             c("lag_date", "lead_date"),
             c("lagDate", "leadDate"))
    return(stockDates)
}

getCashReturn <- function() {
    sql_command <- paste0("
        select
          caldt as date,
          ret
        from cash.daily
    ")
    return(as.data.table(dbGetQuery(con, sql_command)))
}

getOtherReturn <- function() {
    bondIndex <- fread(
        "data/raw/fred-bond-index-1989-12-2016-05.csv",
        skip = 10,
        na.strings = "#N/A"
    )
    setnames(bondIndex,
             c("observation_date", "BAMLCC2A035YTRIV"),
             c("date", "index"))
    bondIndex[, lagIndex := shift(
        bondIndex[, index],
        n = 1,
        type = "lag")]
    bondIndex[, ret := index / lagIndex - 1]
    bondIndex[is.na(ret), ret := 0]

    bondIndex[, list(date, ret)]

    return(bondIndex[, list(date, ret)])
}

getAverageExpenseRatio <- function() {
    # This function calculates average cash proportion held by mutual funds.
    # It calculates it equally-weighted average across funds for each year and
    # then averages it across years in our analaysis.

    sql_command <- "
      with avg_by_year as (
      select
        date_part('year', asset_dt),
        avg(exp_ratio)
      from clean.crsp_fs_wfcin
      where date_part('year', asset_dt) >= '1990'
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

    return(avg[1,1]/12) # /12 because we need monthly expense ratio
}


getHoldings <- function(wfcin, stockDatesAll, startEndDates) {
    # This function return holdings information, which includes dates (rdate,
    # fdate, rebalance date, stock date) and share number. Where stock data date
    # is the day before fdate or fdate, and rebalance date is the rdate + 60
    # or fdate (if it is lower), because if the data enetered db, it means that
    # the was already a disclosure date.
    # Return since is the date since which the return started to accumulate on
    # the current portfolio composition.

    sql_command <- paste0("
        with
          rdate_fdate as (
            select
              max(wfcin) as wfcin,
              rdate,
              min(fdate) as fdate
            from clean.tr_fc_wfcin
            where wfcin = ", wfcin, "
            group by rdate
          )
          select
            r.rdate,
            r.fdate,
            h.cusip,
            h.shares
          from rdate_fdate as r
          inner join clean.holdings_wfcin as h on
            r.wfcin = h.wfcin and
            r.fdate = h.fdate
    ")
    holdings <- as.data.table(dbGetQuery(con, sql_command))

    # Add rebalance date
    holdings[, rebalanceDate := as.Date(NA) ]
    holdings[, effectiveSince := as.Date(NA) ]
    for(curRdate in unique(holdings[, rdate])) {
        # select the day before rdate + 60 (or rdate + 60)
        rBased <- min(stockDatesAll[date >= curRdate + 60, date])

        # get approprate fdate
        curFdate <- min(holdings[rdate == curRdate, fdate])
        # get fdate or next working date
        fBased <- min(stockDatesAll[date >= curFdate, date])


        curRebalanceDate <- min(rBased, fBased)
        holdings[rdate == curRdate, rebalanceDate := curRebalanceDate]

        # returns since is the next day after rebalancing date
        holdings[rdate == curRdate,
                 effectiveSince := min(stockDatesAll[date > curRebalanceDate, date])]
    }

    # Add stock data date
    holdings[, stockDataDate := as.Date(NA) ]
    for(curFdate in unique(holdings[, fdate])) {
        # select the next date after cu
        curStockDataDate <- max(stockDatesAll[date <= curFdate, date])
        holdings[fdate == curFdate, stockDataDate := curStockDataDate]
        #print(curRdate)
    }

    setkey(holdings, rebalanceDate, stockDataDate, cusip)
    return(holdings)
}

getStartEndDates <- function(holdings) {
    # This function returns start and end dates of analysis for a given fund.
    # A start date is the maximum between the first day of holdings disclosure
    # and the first day of returns history. And end date is the minimum of
    # the last disclosure day plus 92 (last disclosure day plus one quarter) and
    # the last returns history date.

    disclosureDates <- data.table(
        min = min(holdings[, rebalanceDate]),
        max = max(holdings[, rebalanceDate]) + 92
    )

    sql_command <- paste0("
        select
          min(caldt) as min,
          max(caldt) as max
        from clean.mret_wfcin
        where wfcin = ", wfcin, "
    ")
    returnDates <- as.data.table(dbGetQuery(con, sql_command))

    if(disclosureDates[1, min] <= returnDates[1, min]) {
        # first rebalance date is before the first return date

        # first rebalance date where rebalance date is bigger than the
        # lowest return date
        startDate <- min(
            holdings[rebalanceDate >= returnDates[1, min], rebalanceDate]
        )
    } else {
        # first return date is before the first rebalance date
        startDate <- disclosureDates[, min]
    }

    endDate <- min(disclosureDates[1, max] + 92, returnDates[1, max])

    startEndDates <- data.table(start = startDate, end = endDate)

    return(startEndDates)
}

subsetHoldings <- function(holdings, startEndDates) {
    # This function subsets holdings according to start and end dates of
    # analsyis.

    holdings <- subset(holdings, rebalanceDate >= startEndDates[, start] &
                           rebalanceDate <= startEndDates[, end])
    return(holdings)
}

getStockData <- function(holdings, startEndDates) {
    # This function returns stock data for all stocks that ever appear in the
    # fund holdings from the start to the end date of analysis.

    cusipsStr <- paste(
        unique(holdings[!is.na(cusip), cusip]),
        collapse = "', '"
    )



    # stockDataDate > 0 is a stupid workaround, because na.rm = TRUE for
    # some reason does not work
    startDate <- min(startEndDates[, start], min(holdings[stockDataDate > 0, stockDataDate], na.rm = TRUE))
    # max of fdate and actual analysis end date, because fdate potentially can
    # outisde the analysis date range, but the stock data for that rebalancing
    # period should be retrieved from that date
    endDate <- max(startEndDates[, end], max(holdings[stockDataDate > 0, stockDataDate], na.rm = TRUE))

    sql_command <- paste0("
        select
          s.ncusip as cusip,
          s.date,
          abs(s.prc) as prc,
          abs(s.prc) * s.shrout * 1000 as mcap,
          case when s.ret < -1 then NULL else s.ret end as ret,
          coalesce(case when exchcd = 3 then 1 else 0 end, 0) as nasdaq
        from stocks.daily as s
        where
          s.date >= '", startDate, "' and
          s.date <= '", endDate, "' and
          s.ncusip in ('", cusipsStr, "')
            ")
    stockData <- as.data.table(dbGetQuery(con, sql_command))
    setkey(stockData, cusip, date)

    return(stockData)
}

getPeriodDates <- function(startEndDates, stockDatesAll, holdings) {
    # This function returns start and end dates for each periods. It also
    # returns boolean variables that indicate whether there is rebalancing at
    # the end of this period or if this is the first period with these holdings.

    # get end of month dates
    stocksEndOfMonths <- stockDatesAll[
        date >= startEndDates[, start] & date <= startEndDates[, end],
        max(date),
        by = list(year(date), month(date))
    ][, V1]

    # same as rebalance dates, because if we rebalance, this is also the last
    # day we collect returns on previous version of portfolio
    lastReturnDates <- holdings[, unique(rebalanceDate)]

    periodEndDates <- sort(unique(c(stocksEndOfMonths, lastReturnDates)))

    periods <- data.table(end = periodEndDates)
    periods <- merge(periods,
                     stockDatesAll[, list(date, leadDate)],
                     by.x = 'end',
                     by.y = 'date')
    periods[, start := shift(periods[, leadDate], 1, type = "lag")]
    periods[1, start := end]
    periods <- periods[, list(start, end)]

    periods[, newHoldings := FALSE]
    periods[, rebalance := FALSE]

    for(i in 1:nrow(periods)) {
        if(periods[i, start] %in% holdings[, effectiveSince]) {
            periods[i, newHoldings := TRUE]
        }
        if(periods[i, end] %in% holdings[, rebalanceDate]) {
            periods[i, rebalance := TRUE]
        }
    }

    periods[1, rebalance := TRUE]

    periods <- subset(periods, start >= startEndDates[, start] &
               end <= startEndDates[, end])

    return(periods)
}

getStockLevelGrossReturns <- function(periods, holdings, stocks) {
    # This function finds appropriate begining and end of the period weights,
    # returns for each stock held in the portfolio in each period. It returns
    # a list of data frame, where each element of the list corresponds to a
    # period.
    periodsDataReturns <- list()


    # for each period
    for(i in 1:nrow(periods)) {
        period <- periods[i]

        if(period[,newHoldings]) {
            # find new holdings
            #setkey(stocks, date, cusip)
            currentHoldings <- holdings[effectiveSince == period[,start]]
            #setkey(stocks, cusip, date)

            # add weights
            weights <- merge(
                currentHoldings[, list(cusip, shares, stockDataDate)],
                stocks[, list(cusip, date, prc)],
                by.x = c("cusip","stockDataDate"),
                by.y = c("cusip", "date"),
                all.x = TRUE
            )
            weights[, weightStart := prc * shares / sum(prc * shares, na.rm = TRUE)]
            weights[is.na(weightStart), weightStart := 0]
            weights[, prc := NULL]

            # calculate stock returns for this date
            returns <- stocks[
                date >= period[, start] &
                    date <= period[, end],
                exp(sum(log(1 + ret), na.rm = TRUE)) - 1,
                by = cusip
            ]
            setnames(returns, "V1", "ret")

            # merge weights with returns
            weightsAndReturns <- merge(
                weights,
                returns,
                by = "cusip"
            )

            # calculate end weights
            weightsAndReturns[, weightEnd := weightStart * (1 + ret)]
            # rescale
            weightsAndReturns[, weightEnd := weightEnd / sum(weightEnd)]

            # add data from periods
            #weightsAndReturnsS <- weightsAndReturns
            #weightsAndReturns <- weightsAndReturnsS
            weightsAndReturns[, `:=` (
                start = period[, start],
                end = period[, end]
            )]

            # store it to list
            periodsDataReturns[[i]] <- weightsAndReturns[, list(
                start, end, cusip, ret, weightStart, weightEnd
            )]
        } else if(i > 1) {
            # use previous holdings if there is information about them
            previousHoldings <- periodsDataReturns[[i - 1]]

            # check if there is something in previous holdings
            if(isEmptyDataTable(previousHoldings)) {next}

            # weights
            weights <- previousHoldings[,
                list(cusip = cusip, weightStart = weightEnd)
            ]

            # calculate stock returns for this date
            returns <- stocks[
                date >= period[, start] &
                    date <= period[, end],
                exp(sum(log(1 + ret), na.rm = TRUE)) - 1,
                by = cusip
            ]
            setnames(returns, "V1", "ret")

            # merge weights with returns
            weightsAndReturns <- merge(
                weights,
                returns,
                by = "cusip"
            )

            # calculate end weights
            weightsAndReturns[, weightEnd := weightStart * (1 + ret)]
            # rescale
            weightsAndReturns[, weightEnd := weightEnd / sum(weightEnd)]

            # add data from periods
            #weightsAndReturnsS <- weightsAndReturns
            #weightsAndReturns <- weightsAndReturnsS
            weightsAndReturns[, `:=` (
                start = period[, start],
                end = period[, end]
            )]

            # store it to list
            periodsDataReturns[[i]] <- weightsAndReturns[, list(
                start, end, cusip, ret, weightStart, weightEnd
            )]

        } else {
            # first entry. Just add one row
            periodsDataReturns[[i]] <-
                data.table(
                    start = period[, start],
                    end = period[, end],
                    cusip = NA,
                    ret = 0,
                    weightStart = 0,
                    weightEnd = 0
                )
        }
        #print(paste(i, period[, start]))
    }
    return(periodsDataReturns)
}

getStockLevelChanges <- function(periods, stockLevelReturnsList) {
    # This function calculates changes in weights in periods when there is a
    # need for rebalancing. It returns a list of these periods. If there is no
    # rebalancing, then the element in the list is NULL.

    weightChanges <- list()
    # loop through each period
    for(i in 1:nrow(periods)) {
        period <- periods[i]
        #print(i)

        if(period[, rebalance]) {
            # check if this is the last period. if last, no need to rebalance
            if(i == nrow(periods)) {
                weightChanges[[i]] <- NULL
                next
            }

            if(i == 1) {
                # check if this is the first period, then changes are just weights
                changes <- stockLevelReturnsList[[i + 1]][, list(cusip, weightStart)]
                setnames(changes, "weightStart", "change")
            } else {
                # else calculate changes
                changes <- merge(
                    # start weights of the next period
                    stockLevelReturnsList[[i + 1]][, list(cusip, weightStart)],
                    # end weights of current period
                    stockLevelReturnsList[[i]][, list(cusip, weightEnd)],
                    by = "cusip",
                    all = TRUE
                )

                # set missing values to 0
                changes[is.na(weightStart), weightStart := 0]
                changes[is.na(weightEnd), weightEnd := 0]

                # find changes
                changes[, change := weightStart - weightEnd]
                changes[, weightStart := NULL]
                changes[, weightEnd := NULL]
            }

             # add data from periods
            changes[, start := period[, start]]
            changes[, end := period[, end]]

            # add it to the list
            weightChanges[[i]] <- changes
        } else {
            # no rebalancing in this period
            weightChanges[[i]] <- NULL
        }
    }

    return(weightChanges)
}

getPeriodProportions <- function(wfcin, periods, averageCash) {

    # get information about asset classes proportion
    sql_command <- paste0("
        select
          caldt,
          per_com / 100 as per_com,
          per_cash / 100 as per_cash
        from clean.crsp_fs_wfcin
        where wfcin = ", wfcin, " and per_com is not null
    ")
    classesProportion <- as.data.table(dbGetQuery(con, sql_command))

    # add proportion information to periods
    periodsProportions <- periods
    for(i in 1:nrow(periodsProportions)) {
        periodStartDate <- periodsProportions[i, start]
        dateInClasses <- max(classesProportion[caldt <= periodStartDate, caldt])

        # check if there is info in class proportions
        if(!is.na(dateInClasses)) {
            # info is present
            periodsProportions[i,
                perCash := max(
                    classesProportion[caldt == dateInClasses, per_cash],
                    0
                )
            ]
            periodsProportions[i,
                perCom := min(
                    classesProportion[caldt == dateInClasses, per_com],
                    1
                )
            ]
        } else {
            # info is mssing => assign averages
            periodsProportions[i,
                perCash := averageCash
            ]
            periodsProportions[i,
                perCom := 1 - averageCash
            ]
        }
    }

    # calculate proportion of other assets
    periodsProportions[, perOther := 1 - perCash - perCom]

    return(periodsProportions)
}

getStockLevelTradingCosts <- function(periodProportions, stockLevelChangesList, stocks) {
    # This function calculates trading costs at a stock level and returns
    # a list where each element corresponds to a period. if there was no
    # rebalancing, then the element is null.

    stockLevelTradingCosts <- list()

    # for each periods that has rebalancing in it
    for(i in 1:nrow(periodProportions)) {
        period <- periodProportions[i]

        if(i > length(stockLevelChangesList)) {
            # skip if there is no data in stockLevelChangeList
            next
        }

        if(period[, rebalance]) {
            perCom <- period[, perCom]

            currentChanges <- stockLevelChangesList[[i]]
            currentChanges[, scaledChange := change * perCom]

            # add stock information
            currentChanges <- merge(
                currentChanges,
                stocks[, list(cusip, date, prc, mcap, nasdaq)],
                by.x = c("cusip", "end"),
                by.y = c("cusip", "date"),
                all.x = TRUE
            )

            # calculate term a
            currentChanges[scaledChange > 0, costsA := abs(scaledChange) *
                            (1.098
                             + 0.336 * nasdaq
                             - 0.084 * log(mcap / 1000)
                             + 13.807 * 1 / prc) ]
            currentChanges[scaledChange < 0, costsA := abs(scaledChange) *
                            (0.979
                             + 0.058 * nasdaq
                             - 0.059 * log(mcap / 1000)
                             + 6.537 * 1 / prc) ]
            currentChanges[scaledChange == 0, costsA := 0]

            # calculate term b
            currentChanges[scaledChange > 0, costsB := abs(scaledChange^2) *
                            (0.092 * 1 / mcap) ]
            currentChanges[scaledChange < 0, costsB := abs(scaledChange^2) *
                            (0.214 * 1 / mcap) ]
            currentChanges[scaledChange == 0, costsB := 0 ]

            # rescale to decimals instead of percentages
            currentChanges[, costsA := costsA / 100]
            currentChanges[, costsB := costsB / 100]

            stockLevelTradingCosts[[i]] <- currentChanges
        } else {
            stockLevelTradingCosts[[i]] <- NULL
        }
    }

    return(stockLevelTradingCosts)
}

getPeriodReturnsAndCosts <- function(stockLevelReturnsList, stockLevelTradingCosts) {
    # Create data tables from lists
    returns <- rbindlist(stockLevelReturnsList)
    setkey(returns, start, cusip)
    costs <- rbindlist(stockLevelTradingCosts)[,list(cusip, start, costsA, costsB)]
    setkey(costs, start, cusip)

    # calculate costs on a stock level and control whether it is less than 0
    costs[, costs10 := costsA + costsB * 10 * 10^6]
    costs[, costs50 := costsA + costsB * 50 * 10^6]
    costs[, costs250 := costsA + costsB * 250 * 10^6]

    # correct for negative costs
    costs[costs10 < 0, costs10 := 0]
    costs[costs50 < 0, costs50 := 0]
    costs[costs250 < 0, costs250 := 0]


    # summarize by periods
    returns <- returns[,
        list(stockGrossRet = sum(ret * weightStart, na.rm = TRUE)),
        by = list(start, end)
    ]
    costs <- costs[,
        list(costs10 = sum(costs10, na.rm = TRUE),
             costs50 = sum(costs50, na.rm = TRUE),
             costs250 = sum(costs250, na.rm = TRUE)),
        by = list(start)
    ]


    # merge them costs and gross stock returns
    costsAndReturns <- merge(
        returns,
        costs,
        by = c("start"),
        all.x = TRUE
    )

    # substitute NAs with zeros
    costsAndReturns[is.na(costs10), costs10 := 0]
    costsAndReturns[is.na(costs50), costs50 := 0]
    costsAndReturns[is.na(costs250), costs250 := 0]

    return(costsAndReturns)
}

getPeriodReturns <- function(periodReturnsAndCosts, periods, cashReturn, otherReturn) {

    # combine period data with stock returns
    periodReturns <- merge(
        periods,
        periodReturnsAndCosts,
        by = c("start", "end"),
        all.x = TRUE
    )

    # add cash and other returns
    for(i in 1:nrow(periodReturns)) {
        period <- periodReturns[i]

        periodReturns[i,
            cashRet := cashReturn[
                date >= period[, start] & date <= period[, end],
                exp(sum(log(1 + ret), na.rm = TRUE)) - 1
            ]
        ]
        periodReturns[i,
            otherRet := otherReturn[
                date >= period[, start] & date <= period[, end],
                exp(sum(log(1 + ret), na.rm = TRUE)) - 1
            ]
        ]
    }

    # calculate appropriate weights of asset classes based on differences in
    # their returns. Start with 2, because first period is rebalancing only, no
    # returns yet.
    for(i in 2:nrow(periodReturns)) {
        period <- periodReturns[i]

        if(period[, newHoldings]) {
            # there is no need to change weights, because they have just started
            next
        }

        # calculated unscaled weights
        perCom <- periodReturns[i - 1, perCom] * (1 + periodReturns[i - 1, stockGrossRet])
        perCash <- periodReturns[i - 1, perCash] * (1 + periodReturns[i - 1, cashRet])
        perOther <- periodReturns[i - 1, perOther] * (1 + periodReturns[i - 1, otherRet])

        # scale them
        perComS <- perCom / (perCom + perCash + perOther)
        perCashS <- perCash / (perCom + perCash + perOther)
        perOtherS <- perOther / (perCom + perCash + perOther)

        # write them back
        periodReturns[i, perCom := perComS]
        periodReturns[i, perCash := perCashS]
        periodReturns[i, perOther := perOtherS]
    }


    # calculate stock after trade returns
    periodReturns[, stockAtcRet10 := stockGrossRet - costs10]
    periodReturns[, stockAtcRet50 := stockGrossRet - costs50]
    periodReturns[, stockAtcRet250 := stockGrossRet - costs250]

    # calculate gross fund return
    periodReturns[,
        copycatGrossRet :=
            stockGrossRet * perCom +
            cashRet * perCash +
            otherRet * perOther
    ]

    return(periodReturns)
}

getMonthlyCopycatReturns <- function(periodReturns) {
    periodReturns[, month := month(end)]
    periodReturns[, year := year(end)]

    # aggregate function
    logsum <- function(x) {
        return(exp(sum(log(1 + x), na.rm = TRUE)) - 1)
    }

    monthlyReturns <-
        periodReturns[, list(
            stock_ret = logsum(stockGrossRet),
            cash_ret = logsum(cashRet),
            other_ret = logsum(otherRet),
            gross_ret_cop = logsum(copycatGrossRet),
            costs_10m_cop = logsum(-costs10),
            costs_50m_cop = logsum(-costs50),
            costs_250m_cop = logsum(-costs250),
            mgmt_fees_cop = 0.2 * 0.01 / 12,
            atc_ret_10m_cop =  logsum(stockAtcRet10), # atc_ : after trading cost
            atc_ret_50m_cop =  logsum(stockAtcRet50),
            atc_ret_250m_cop = logsum(stockAtcRet250),
            net_ret_10m_cop =  logsum(stockAtcRet10) - 0.2 * 0.01 / 12,
            net_ret_50m_cop =  logsum(stockAtcRet50) - 0.2 * 0.01 / 12,
            net_ret_250m_cop = logsum(stockAtcRet250) - 0.2 * 0.01 / 12
            ), by = list(year, month)]

    setkey(monthlyReturns, year, month)

    return(monthlyReturns)
}

getActualReturns <- function(monthlyCopycatReturns, wficn, averageExpenseRatio) {
    sql_command <- paste0("
        with
        mret as (
          select
            caldt,
            date_part('year', caldt) as year,
            date_part('month', caldt) as month,
            mret
          from clean.mret_wfcin
          where wfcin = ", wfcin, "
          order by caldt
        ),
        exp as (
          select
            caldt,
            exp_ratio
          from clean.crsp_fs_wfcin
          where wfcin = ", wfcin, "
        )
        select distinct on (year, month)
          r.*,
          e.exp_ratio / 12 as m_exp
        from mret as r
        left join exp as e on
          e.caldt = e.caldt
    ")
    returnsAndExpenses <- as.data.table(dbGetQuery(con, sql_command))

    # add expense and return data to monthly copycat returns
    monthlyReturns <- merge(
        monthlyCopycatReturns,
        returnsAndExpenses,
        by = c("year", "month"),
        all.x = TRUE
    )

    # fill missing expense ratios
    for(i in 1:nrow(monthlyReturns)) {
        if(is.na(monthlyReturns[i, m_exp])) {
            if(i > 1) {
                monthlyReturns[i,
                    m_exp :=
                        monthlyReturns[i - 1, m_exp]
                ]
            } else {
                monthlyReturns[i,
                    m_exp := averageExpenseRatio
                ]
            }
        }
    }

    # calculate actual funds gross returns
    setnames(monthlyReturns, "mret", "net_ret_act")
    monthlyReturns[, gross_ret_act := net_ret_act + m_exp]

    # add wfcin identificator
    monthlyReturns[, wfcin := wfcin]

    return(monthlyReturns)

}

writeTableToCopycats <- function(monthlyReturns, wfcin) {
    # check if this fund is already in db
    sql_command <- paste0("
       select
         wfcin
       from performance.monthly
       where wfcin = '", wfcin, "'
       limit 1
   ")
    tryCatch(
       empty <- isEmptyDataTable(as.data.table(dbGetQuery(con, sql_command))),
       error = function(cond) {
           empty <- TRUE
       })
    2+2


    if(!empty) {
        # database already there
        return(FALSE)
    }

    # write table
    dbWriteTable(con, c("performance","monthly"),
                 value = monthlyReturns,
                 append = TRUE,
                 row.names = FALSE)
    return(TRUE)
}

addLogEntry <- function(message, file, first = FALSE) {
    if(first) {
        header <- paste(
            "No",
            "wfcin",
            "Copy",
            "DB",
            "Elapsed",
            sep = "\t")

        write(header, file = file,
           append = TRUE, sep = " ")
    }

    write(message, file = file,
           append = TRUE, sep = " ")

    return(TRUE)
}