isEmptyDataTable <- function(dt) {
    if (dim(dt)[1] == 0 && dim(dt)[2] == 0) {
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

getStartEndDates <- function(wfcin) {
    sql_command <- paste0("
with
  -----------------------------------------------------------------------------
  -- wfcin list
  -----------------------------------------------------------------------------
  wfcin_list as (
    select distinct
      wfcin,
      1 as join_column
    from clean.deoni_wfcin
    where wfcin = '", wfcin, "'
  ),
  -----------------------------------------------------------------------------
  -- fund return dates
  -----------------------------------------------------------------------------
   fund_mret_dates_setup as (
    select distinct
      w.wfcin,
      r.caldt
    from wfcin_list as w
    left join clean.mret_wfcin as r on
      w.wfcin = r.wfcin
    where r.caldt >= '1990-01-01'
  ),
  fund_mret_dates as (
    select
      r.wfcin,
      r.caldt,
      lag(r.caldt) over (partition by r.wfcin order by r.caldt) as lag_caldt,
      lead(r.caldt) over (partition by r.wfcin order by r.caldt) as lead_caldt,
      min(r.caldt) over (partition by r.wfcin) as min_caldt,
      max(r.caldt) over (partition by r.wfcin) as max_caldt
    from fund_mret_dates_setup as r
  ),
  -----------------------------------------------------------------------------
  -- fdates
  -----------------------------------------------------------------------------
  fdates_setup as (
    select distinct on (w.wfcin, h.fdate)
      w.wfcin,
      h.fdate
    from wfcin_list as w
    left join clean.holdings_wfcin as h on
      w.wfcin = h.wfcin
    where h.fdate >= '1990-01-01'

  ),
  fdates as (
    select
      f.wfcin,
      f.fdate,
      lag(f.fdate) over (partition by f.wfcin order by f.fdate) as lag_fdate,
      coalesce(
        lead(f.fdate) over (partition by f.wfcin order by f.fdate),
        f.fdate + 365) as lead_fdate,
      min(f.fdate) over (partition by f.wfcin) as min_fdate,
      max(f.fdate) over (partition by f.wfcin) as max_fdate
    from fdates_setup as f
  ),
  -----------------------------------------------------------------------------
  -- start, end dates
  -----------------------------------------------------------------------------
  min_max_fdate as (
    select
      f.wfcin,
      min(f.fdate) as min_fdate,
      max(f.fdate) as max_fdate
    from fdates as f
    group by f.wfcin
  ),
  min_max_return_dates as (
    select
      r.wfcin,
      min(r.caldt) as min_caldt,
      max(r.caldt) as max_caldt
    from fund_mret_dates as r
    group by r.wfcin
  ),
  start_end_dates as (
    select
      f.wfcin,
      greatest(f.min_fdate, r.min_caldt) as start_date,
      least(f.max_fdate + 183, r.max_caldt) as end_date
    from min_max_fdate as f
    left join min_max_return_dates as r on
      f.wfcin = r.wfcin
  )
select * from start_end_dates
    ")

    query_result <- as.data.table(dbGetQuery(con, sql_command))
    return(query_result)
}

getPeriodDates <- function(wfcin, startEndDates) {
    sql_command <- paste0("
with
  -----------------------------------------------------------------------------
  -- wfcin list
  -----------------------------------------------------------------------------
  wfcin_list as (
    select distinct
      wfcin,
      1 as join_column
    from clean.deoni_wfcin
    where wfcin = '", wfcin,"'
  ),
  -----------------------------------------------------------------------------
  -- fund return dates
  -----------------------------------------------------------------------------
   fund_mret_dates_setup as (
    select distinct
      w.wfcin,
      r.caldt
    from wfcin_list as w
    left join clean.mret_wfcin as r on
      w.wfcin = r.wfcin
    where r.caldt >= '1990-01-01'
  ),
  fund_mret_dates as (
    select
      r.wfcin,
      r.caldt,
      lag(r.caldt) over (partition by r.wfcin order by r.caldt) as lag_caldt,
      lead(r.caldt) over (partition by r.wfcin order by r.caldt) as lead_caldt,
      min(r.caldt) over (partition by r.wfcin) as min_caldt,
      max(r.caldt) over (partition by r.wfcin) as max_caldt
    from fund_mret_dates_setup as r
  ),
  -----------------------------------------------------------------------------
  -- fdates
  -----------------------------------------------------------------------------
  fdates_setup as (
    select distinct on (w.wfcin, h.fdate)
      w.wfcin,
      h.fdate
    from wfcin_list as w
    left join clean.holdings_wfcin as h on
      w.wfcin = h.wfcin
    where h.fdate >= '1990-01-01'

  ),
  fdates_rdates as (
    select distinct on (tfw.wfcin, tfw.fdate)
       tfw.wfcin,
       tfw.fdate,
       tfw.rdate
     from clean.tr_fc_wfcin as tfw
     where
       tfw.wfcin in (select distinct wfcin from wfcin_list)
  ),
  fdates as (
    select
      f.wfcin,
      f.fdate,
      fc.rdate,
      lag(f.fdate) over (partition by f.wfcin order by f.fdate) as lag_fdate,
      coalesce(
        lead(f.fdate) over (partition by f.wfcin order by f.fdate),
        f.fdate + 365) as lead_fdate,
      min(f.fdate) over (partition by f.wfcin) as min_fdate,
      max(f.fdate) over (partition by f.wfcin) as max_fdate
    from fdates_setup as f
    left join fdates_rdates as fc on
      fc.wfcin = f.wfcin and
      fc.fdate = f.fdate
  ),
  -----------------------------------------------------------------------------
  -- start, end dates
  -----------------------------------------------------------------------------
  min_max_fdate as (
    select
      f.wfcin,
      min(f.fdate) as min_fdate,
      max(f.fdate) as max_fdate
    from fdates as f
    group by f.wfcin
  ),
  min_max_return_dates as (
    select
      r.wfcin,
      min(r.caldt) as min_caldt,
      max(r.caldt) as max_caldt
    from fund_mret_dates as r
    group by r.wfcin
  ),
  start_end_dates as (
    select
      f.wfcin,
      greatest(f.min_fdate, r.min_caldt) as start_date,
      least(f.max_fdate + 365, r.max_caldt) as end_date
    from min_max_fdate as f
    left join min_max_return_dates as r on
      f.wfcin = r.wfcin
  ),
  -----------------------------------------------------------------------------
  -- Calculate holdings dates: at which to look for information, at which date
  -- we start receiving returns
  -----------------------------------------------------------------------------
  holdings_multiple_stock_dates_le_fdate as (
    select
      f.wfcin,
      f.fdate,
      f.rdate,
      s.date
    from fdates as f
    left join clean.stock_dates as s on
      s.date <= f.rdate
    left join start_end_dates as sed on
      f.wfcin = sed.wfcin
    where f.fdate >= sed.start_date and f.fdate <= sed.end_date
  ),
  holdings_fdate_stock_data_date as (
    select distinct
      h.wfcin,
      h.fdate,
      max(h.date) latest_stock_data_date
    from holdings_multiple_stock_dates_le_fdate as h
    group by h.wfcin, h.fdate
  ),
  holdings_multiple_stock_dates_ge_fdate as (
    select
      f.wfcin,
      f.fdate,
      s.date
    from fdates as f
    left join clean.stock_dates as s on
      s.lag_date >= f.fdate
    left join start_end_dates as sed on
      f.wfcin = sed.wfcin
    where f.fdate >= sed.start_date and f.fdate <= sed.end_date
  ),
  holdings_fdate_first_holdings_date as (
    select
      h.wfcin,
      h.fdate,
      min(h.date) first_holdings_date
    from holdings_multiple_stock_dates_ge_fdate as h
    group by h.wfcin, h.fdate
  ),
  holdings_dates as (
    select
      le.wfcin,
      le.fdate,
      le.latest_stock_data_date as stock_data_date,
      ge.first_holdings_date as returns_from_date
    from holdings_fdate_stock_data_date as le
    left join holdings_fdate_first_holdings_date as ge on
      le.wfcin = ge.wfcin and
      le.fdate = ge.fdate
  ),
  -----------------------------------------------------------------------------
  -- period dates
  -----------------------------------------------------------------------------
  period_start_dates as (
    select
      mr.wfcin,
      min(s.date) as returns_from_date,
      false as new_holdings,
      null as fdate,
      null as stock_data_date
    from fund_mret_dates as mr
    left join clean.stock_dates as s on
      date_part('year', mr.caldt) = date_part('year', s.date) AND
      date_part('month', mr.caldt) = date_part('month', s.date)
    left join start_end_dates as sed on
      mr.wfcin = sed.wfcin
    where
      mr.caldt >= sed.start_date and mr.caldt <= sed.end_date
    group by
      mr.wfcin,
      mr.caldt
      --max(sed.end_date) as end_date
    union ALL
    select
      hd.wfcin,
      hd.returns_from_date,
      true as new_holdings,
      hd.fdate,
      hd.stock_data_date
    from holdings_dates as hd
  ),
  period_start_distinct as (
    select distinct on (ps.wfcin, ps.returns_from_date)
      ps.wfcin,
      ps.new_holdings,
      ps.returns_from_date,
      --lead(ps.returns_from_date) over (partition by ps.wfcin order by ps.returns_from_date) as lead_returns_from_date,
      sed.end_date,
      ps.fdate,
      ps.stock_data_date
    from period_start_dates as ps
    left join start_end_dates as sed on
      ps.wfcin = sed.wfcin
    order by ps.wfcin, ps.returns_from_date, ps.new_holdings desc
  ),
  period_start_lead_start_date as (
    select
      p.*,
      lead(p.returns_from_date) over (partition by p.wfcin order by p.returns_from_date) as lead_returns_from_date
    from period_start_distinct as p
  ),
  period_start_end_dates as (
    select
      psl.wfcin,
      row_number() over (partition by psl.wfcin order by psl.returns_from_date asc) period_no,
      psl.new_holdings,
      psl.fdate,
      --psl.lead_returns_from_date,
      psl.stock_data_date,
      psl.returns_from_date,
      coalesce(s.lag_date, psl.end_date) as returns_to_date
    from period_start_lead_start_date as psl
    left join clean.stock_dates as s on
      psl.lead_returns_from_date = s.date
  ),
  -----------------------------------------------------------------------------
  -- period key dates. Table contains following colunmns:
  -- - wfcin
  -- - period_no
  -- - new_holdings: true if this is a rebalancing date)
  -- - returns_from_date: first day at which to look for returns in this period
  -- - returns_to_date: last day at which to look for returns in this period
  -- - holdings_fdate: fdate of current holdings
  -- - holdings_returns_from: date from which to look for returns to adjust
  --   weights
  -----------------------------------------------------------------------------
  period_key_dates as (
    select distinct
      p.wfcin,
      p.period_no,
      p.new_holdings,
      --p.fdate,
      p.returns_from_date,
      p.returns_to_date,
      p.stock_data_date,
      max(pf.fdate) over (partition by p.wfcin, p.period_no) holdings_fdate,
      max(pf.returns_from_date) over (partition by p.wfcin, p.period_no) holdings_returns_from,
      sb.lag_date as buy_date,
      p.returns_to_date as sell_date
    from period_start_end_dates as p
    left join (select
                 wfcin, fdate, returns_from_date
               from period_start_end_dates
               where new_holdings) as pf on
      pf.returns_from_date <= p.returns_from_date
    left join clean.stock_dates as sb on
      sb.date = p.returns_from_date
  )
select * from period_key_dates where returns_from_date <= returns_to_date order by wfcin, period_no
    ")

    query_result <- as.data.table(dbGetQuery(con, sql_command))
    return(query_result)
}

getHoldingsData <- function(wfcin, periodKeyDates) {
    # get holdings in terms of number os shares
    fdates <- periodKeyDates[new_holdings == TRUE, holdings_fdate]
    fdates <- fdates[!is.na(fdates)]

    fdatesStr <- paste(fdates, collapse = "', '")
    sql_command <- paste0("
        select
        *
        from clean.holdings_wfcin as h
        where
          h.wfcin = '", wfcin, "' and
          h.fdate in ('", fdatesStr,"')
    ")

    query_result <- as.data.table(dbGetQuery(con, sql_command))
    sharesData <- merge(
        query_result,
        periodKeyDates[new_holdings == TRUE,
                       c("holdings_fdate",
                       "stock_data_date",
                       "holdings_returns_from"), with = FALSE],
        by.x = 'fdate',
        by.y = 'holdings_fdate',
        all = TRUE

    )

    # obtain prices
    cusipsStr <- paste(unique(sharesData[!is.na(cusip), cusip]), collapse = "', '")
    stockDatesStr <- paste(unique(sharesData[!is.na(stock_data_date), stock_data_date]), collapse = "', '")

    sql_command <- paste0("
        select
          s.ncusip as cusip,
          s.date,
          abs(s.prc) as prc
        from stocks.daily as s
        where
          s.ncusip in ('", cusipsStr, "') and
          s.date in ('", stockDatesStr,"')
    ")
    query_result <- as.data.table(dbGetQuery(con, sql_command))

    # join numbers of shares with prices
    sharesData <- merge(
        sharesData,
        query_result,
        by.x = c("cusip", "stock_data_date"),
        by.y = c("cusip", "date"),
        all.x = TRUE
    )

    sharesData[, value := ifelse(is.na(prc * shares), 0, prc * shares)]
    sharesData[,fdate_sum_value := sum(value), by = fdate]
    sharesData[,weight := value / fdate_sum_value]

    setkey(sharesData, fdate, cusip)

    return(sharesData)
}

getAllStockData <- function(holdingsData, startEndDates) {
    cusipsStr <- paste(unique(holdingsData[!is.na(cusip), cusip]), collapse = "', '")

    sql_command <- paste0("
        select
          *
        from clean.stock_dates
        where
          date >= '", startEndDates[,start_date], "' and
          date <= '", startEndDates[,end_date], "'
    ")
    stockDates <- as.data.table(dbGetQuery(con, sql_command))
    stockDatesStr <- paste(unique(stockDates[!is.na(date),date]), collapse = "', '")

    sql_command <- paste0("
        select
          s.ncusip as cusip,
          s.date,
          abs(s.prc) as prc,
          abs(s.prc) * s.shrout as mcap,
          case when s.ret < -1 then NULL else s.ret end as ret,
          coalesce(case when exchcd = 3 then 1 else 0 end, 0) as nasdaq
        from stocks.daily as s
        where
          s.date >= '", startEndDates[,start_date], "' and
          s.date <= '", startEndDates[,end_date], "' and
          s.ncusip in ('", cusipsStr, "')
            ")
    stockData <- as.data.table(dbGetQuery(con, sql_command))
    setkey(stockData, cusip, date)

    return(stockData)
}

getPeriodStockData <- function(periodKeyDates, holdingsData, stockData) {
    # add cusips to each period
    periodsWithCusips <- merge(
        periodKeyDates[, c("wfcin", "period_no", "holdings_fdate",
                           "holdings_returns_from", "returns_from_date",
                           "returns_to_date"), with = FALSE],
        holdingsData[, c("fdate", "cusip", "weight"), with = FALSE],
        by.x = "holdings_fdate",
        by.y = "fdate",
        all.x = TRUE,
        allow.cartesian = TRUE
    )
    setnames(periodsWithCusips, "weight", "weight_on_fdate")
    setkey(periodsWithCusips, period_no, cusip)

    # for each period from periods, calculate returns of each stock for that
    # period. then add them into original table
    stockReturnsAll <- list()
    for(i in 1:nrow(periodKeyDates)) {
        # subset stocks
        subsetStockData <-
            stockData[date >= periodKeyDates[i, returns_from_date] &
                      date <= periodKeyDates[i, returns_to_date], ]
        # calculate returns
        stockReturns <- subsetStockData[, exp(sum(log(1 + ret), na.rm = TRUE)) - 1, by = cusip]
        setnames(stockReturns, "V1", "period_ret")
        # add current period number for joining purposes
        stockReturns[, period_no := i]

        stockReturnsAll[[i]] <- stockReturns
    }
    # create a single data table from returns for all periods
    stockReturns <- rbindlist(stockReturnsAll)
    setkey(stockReturns, period_no, cusip)

    # merge it with data base that conatins holdings
    periodsWithCusips <- merge(
        periodsWithCusips,
        stockReturns,
        by = c("period_no", "cusip"),
        all.x = TRUE
    )

    # for each period from periods, calculate returns of each stock for since
    # holdings return start to the day before period starts. this is needed for
    # calculation of weights as of beginning of the period
    stockReturnsAll <- list()
    for(i in 1:nrow(periodKeyDates)) {
        # subset stocks
        subsetStockData <-
            stockData[date >= periodKeyDates[i, holdings_returns_from] &
                      date < periodKeyDates[i, returns_from_date], ]
        # calculate returns
        stockReturns <- subsetStockData[, exp(sum(log(1 + ret), na.rm = TRUE)) - 1, by = cusip]
        setnames(stockReturns, "V1", "since_fdate_ret")
        # add current period number for joining purposes
        stockReturns[, period_no := i]

        stockReturnsAll[[i]] <- stockReturns
    }
    # create a single data table from returns for all periods
    stockReturns <- rbindlist(stockReturnsAll)
    setkey(stockReturns, period_no, cusip)

    # merge it with data base that conatins holdings
    periodsWithCusips <- merge(
        periodsWithCusips,
        stockReturns,
        by = c("period_no", "cusip"),
        all.x = TRUE
    )


    # calculate weight as of the beginning of period
    # first, calculate not scaled weight
    periodsWithCusips[, weight_ns :=
                          weight_on_fdate * ifelse(is.na(since_fdate_ret), 1,
                                               1 + since_fdate_ret)]
    # scale weights
    periodsWithCusips[, sum_weight_ns := sum(weight_ns), by = period_no]
    periodsWithCusips[, weight := weight_ns / sum_weight_ns]

    periodsWithCusips <- periodsWithCusips[, c("wfcin", "period_no",
                                               "returns_from_date",
                                               "returns_to_date",
                                               "cusip",
                                               "weight","period_ret"),
                                           with = FALSE]

    return(periodsWithCusips)
}

getPeriodStockReturns <- function(periodStockData, periodKeyDates) {
    periodReturns <- periodStockData[, sum(weight * period_ret, na.rm = TRUE),
                                     by = period_no]
    setnames(periodReturns, "V1", "stock_period_ret")
    setkey(periodReturns, period_no)
    return(periodReturns)
}

getPeriodCashReturns <- function(periodKeyDates) {
    dbWriteTable(con, c("tmp","period_key_dates"),
                 value = periodKeyDates,
                 overwrite=TRUE,
                 row.names=FALSE)

    sql_command <- paste0("
        select
          p.period_no,
          exp(sum(ln(coalesce(c.ret, 0) + 1))) - 1 as cash_period_ret
        from tmp.period_key_dates as p
        left join cash.daily as c on
          c.caldt >= p.returns_from_date and
          c.caldt <= p.returns_to_date
        group by
          p.period_no
        order by
          p.period_no
    ")
    cashReturns <- as.data.table(dbGetQuery(con, sql_command))
    setkey(cashReturns, period_no)
    return(cashReturns)
}

getPeriodOtherReturns <- function(periodKeyDates) {
    dbWriteTable(con, c("tmp","period_key_dates"),
                 value = periodKeyDates,
                 overwrite=TRUE,
                 row.names=FALSE)

    sql_command <- paste0("
        select
          p.period_no,
          exp(sum(ln(coalesce(g.ret, 0) + 1))) - 1 as other_period_ret
        from tmp.period_key_dates as p
        left join global.daily as g on
          g.date >= p.returns_from_date and
          g.date <= p.returns_to_date
        group by
          p.period_no
        order by
          p.period_no
    ")
    globalReturns <- as.data.table(dbGetQuery(con, sql_command))
    setkey(globalReturns, period_no)
    return(globalReturns)
}

mergeReturns <- function(periodStockReturns, periodCashReturns,
                        periodOtherReturns) {
    periodReturns <- merge(
        periodStockReturns,
        periodCashReturns
    )
    periodReturns <- merge(
        periodReturns,
        periodOtherReturns
    )
    setkey(periodReturns, period_no)
    return(periodReturns)
}

getCategoriesData <- function(periodKeyDates, periodReturns, averageCash) {
    # get dates on which portfolio composition changes (fdates)
    newHoldingDates <- periodKeyDates[new_holdings == TRUE, ]

    # get data on asset classes proportions from CRSP
    sql_command <- paste0("
        select
          fs.caldt,
          fs.per_com / 100 as per_com,
          fs.per_cash / 100 as per_cash,
          (100 - fs.per_com - fs.per_cash) / 100 as per_other
        from clean.crsp_fs_wfcin as fs
        where wfcin = '", wfcin, "'
        ")
    proportions <- as.data.table(dbGetQuery(con, sql_command))

    categoriesData <- merge(
        newHoldingDates,
        proportions,
        by.x = 'holdings_fdate',
        by.y = 'caldt',
        all.x = TRUE
    )

    categoriesDataOnFdates <- categoriesData[, c("period_no",
                                                 "per_com", "per_cash",
                                                 "per_other"), with = FALSE]
    # fill missing values with previous values if there is no data on allocation\
    for(i in 1:nrow(categoriesDataOnFdates)) {
        if(categoriesDataOnFdates[i, is.na(per_com)]) {
            if(i == 1) {
                categoriesDataOnFdates[i, per_com := 1 - averageCash]
                categoriesDataOnFdates[i, per_cash := averageCash]
                categoriesDataOnFdates[i, per_other := 0]
            } else {
                categoriesDataOnFdates[i, per_com := categoriesDataOnFdates[i - 1, per_com]]
                categoriesDataOnFdates[i, per_cash := categoriesDataOnFdates[i - 1, per_cash]]
                categoriesDataOnFdates[i, per_other := categoriesDataOnFdates[i - 1, per_other]]
            }
        }
    }
    setkey(categoriesDataOnFdates, period_no)


    # merge category data with returns data
    categoryAndReturns <- merge(
        periodReturns,
        categoriesDataOnFdates,
        all.x = TRUE
    )

    # find the period_no of the first date at which holdings information is a
    # available
    firstHoldingsPeriodNo <- min(categoriesData[, period_no])

    # add columns to categoryAndReturns. _ns stands for non-scaled
    categoryAndReturns[, w_stock_ns := as.numeric(NA)]
    categoryAndReturns[, w_cash_ns := as.numeric(NA)]
    categoryAndReturns[, w_other_ns := as.numeric(NA)]
    categoryAndReturns[, w_stock := as.numeric(NA)]
    categoryAndReturns[, w_cash := as.numeric(NA)]
    categoryAndReturns[, w_other := as.numeric(NA)]

    # add averages as the first values

    # loop through all periods and fill in non-scaled weights
    for(i in firstHoldingsPeriodNo:nrow(categoryAndReturns)) {
        # check if there are reported stock weights
        if(categoryAndReturns[i, is.na(per_com)]) {
            categoryAndReturns[i, w_stock_ns :=
                max(categoryAndReturns[i - 1, w_stock_ns * (1 + stock_period_ret)],0)]
        } else {
            categoryAndReturns[i, w_stock_ns := max(per_com, 0)]
        }
        # check if there are reported cash weights
        if(categoryAndReturns[i, is.na(per_cash)]) {
            categoryAndReturns[i, w_cash_ns :=
                max(categoryAndReturns[i - 1, w_cash_ns * (1 + cash_period_ret)],0)]
        } else {
            categoryAndReturns[i, w_cash_ns := max(per_cash, 0)]
        }
        # check if there are reported other weights
        if(categoryAndReturns[i, is.na(per_other)]) {
            categoryAndReturns[i, w_other_ns :=
                max(categoryAndReturns[i - 1, w_other_ns * (1 + other_period_ret)],0)]
        } else {
            categoryAndReturns[i, w_other_ns := max(per_other, 0)]
        }
    }

    # rescale weights
    categoryAndReturns[, sum_w_ns := w_stock_ns + w_cash_ns + w_other_ns]
    categoryAndReturns[, w_stock := w_stock_ns / sum_w_ns]
    categoryAndReturns[, w_cash := w_cash_ns / sum_w_ns]
    categoryAndReturns[, w_other := w_other_ns / sum_w_ns]

    categoryAndReturns <- categoryAndReturns[, c("period_no",
                                                 "stock_period_ret",
                                                 "cash_period_ret",
                                                 "other_period_ret",
                                                 "w_stock",
                                                 "w_cash",
                                                 "w_other"), with = FALSE]

    setkey(categoryAndReturns, period_no)

    return(categoryAndReturns)
}

getTradingCosts <- function(periodKeyDates, returnsWeightsOfClasses, stockData) {
    rebalanceDates <- merge(
        periodKeyDates[new_holdings == TRUE, c("period_no", "buy_date",
                                               "sell_date"), with = FALSE],
        returnsWeightsOfClasses[, c("period_no", "w_stock", "w_cash", "w_other"),
                      with = FALSE],
        by = "period_no",
        all.x = TRUE
    )

    costsList <- list()
    for(i in 1:nrow(rebalanceDates)) {
        periodNo <- rebalanceDates[i, period_no]
        buySellDate <- rebalanceDates[i, buy_date]
        oldStockWeight <- returnsWeightsOfClasses[period_no == periodNo - 1, w_stock]
        oldStockWeight <- ifelse(is.na(oldStockWeight), 0, oldStockWeight)

        newStockWeight <- returnsWeightsOfClasses[period_no == periodNo, w_stock]
        newStockWeight <- ifelse(is.na(newStockWeight), 0, newStockWeight)

        oldHoldings <- periodStockData[period_no == periodNo - 1,
                                       c("cusip", "weight"), with = FALSE]
        newHoldings <- periodStockData[period_no == periodNo,
                                       c("cusip", "weight"), with = FALSE]

        holdingsChange <- merge(
            newHoldings,
            oldHoldings,
            by = "cusip",
            all = TRUE
        )

        setnames(holdingsChange, "weight.y", "oldWeight")
        setnames(holdingsChange, "weight.x", "newWeight")

        holdingsChange[is.na(oldWeight), oldWeight := 0]
        holdingsChange[is.na(newWeight), newWeight := 0]

        holdingsChange[, oldWeight := oldWeight * oldStockWeight]
        holdingsChange[, newWeight := newWeight * newStockWeight]

        holdingsChange[, change := newWeight - oldWeight]

        # add price and market cap infromation
        holdingsChange <- merge(
            holdingsChange[, c("cusip", "change"), with = FALSE],
            stockData[date == buySellDate, c("cusip", "prc", "mcap", "nasdaq"), with = FALSE],
            by = "cusip",
            all.x = TRUE
        )

        # add trading cost terms
        holdingsChange[change > 0, costs_a := abs(change) *
                        (1.098
                         + 0.336 * nasdaq
                         - 0.084 * log(mcap / 1000)
                         + 13.807 * 1 / prc) ]
        holdingsChange[change < 0, costs_a := abs(change) *
                        (0.979
                         + 0.058 * nasdaq
                         - 0.059 * log(mcap / 1000)
                         + 6.537 * 1 / prc) ]
        holdingsChange[change == 0, costs_a := 0]

        holdingsChange[change > 0, costs_b := abs(change * change) *
                        (0.092 * 1 / mcap) ]
        holdingsChange[change < 0, costs_b := abs(change * change) *
                        (0.214 * 1 / mcap) ]
        holdingsChange[change == 0, costs_b := 0 ]

        costs_a <- holdingsChange[, sum(costs_a, na.rm = TRUE)]
        costs_b <- holdingsChange[, sum(costs_b, na.rm = TRUE)]

        costsList[[i]] <- data.table(period_no = periodNo,
                costs_a = costs_a / 100,
                costs_b = costs_b / 100)
    }

    costComponents <- rbindlist(costsList)
    return(costComponents)
}

getPeriodReturns <- function(returnsWeightsOfClasses, costComponents,
                             periodKeyDates) {
    returnsWeightsOfClasses[, gross_return := stock_period_ret * w_stock +
                                cash_period_ret * w_cash +
                                other_period_ret * w_other]
    returns <- returnsWeightsOfClasses
    setkey(returns, period_no)

    returns <- merge(returns,
          costComponents,
          by = "period_no",
          all.x = TRUE)

    returns[is.na(gross_return), gross_return := 0]
    returns[is.na(costs_a), costs_a := 0]
    returns[is.na(costs_b), costs_b := 0]

    returns[, costs_10m := costs_a + costs_b * 10 * 10^6]
    returns[, costs_50m := costs_a + costs_b * 50 * 10^6]
    returns[, costs_250m := costs_a + costs_b * 250 * 10^6]


    returns <- merge(
        periodKeyDates[, c("period_no", "returns_from_date", "returns_to_date"),
                       with = FALSE],
        returns,
        by = "period_no"
    )

    returns[, net_ret_10m := gross_return - costs_10m]
    returns[, net_ret_50m := gross_return - costs_50m]
    returns[, net_ret_250m := gross_return - costs_250m]

    return(returns)
}

getMonthlyReturns <- function(periodReturns) {
    periodReturns[, month := month(returns_to_date)]
    periodReturns[, year := year(returns_to_date)]

    # aggregate function
    logsum <- function(x) {
        return(exp(sum(log(1 + x), na.rm = TRUE)) - 1)
    }

    columnList <- list()
    monthlyReturns <-
        periodReturns[, list(
            stock_ret = logsum(stock_period_ret),
            cash_ret = logsum(cash_period_ret),
            other_ret = logsum(other_period_ret),
            gross_ret_cop = logsum(gross_return),
            costs_10m_cop = logsum(-costs_10m),
            costs_50m_cop = logsum(-costs_50m),
            costs_250m_cop = logsum(-costs_250m),
            mgmt_fees_cop = 0.2 * 0.01 / 12,
            atc_ret_10m_cop = logsum(net_ret_10m), # ac_ : after cost
            atc_ret_50m_cop = logsum(net_ret_50m),
            atc_ret_250m_cop = logsum(net_ret_250m),
            net_ret_10m_cop = logsum(net_ret_10m) - 0.2 * 0.01 / 12,
            net_ret_50m_cop = logsum(net_ret_50m) - 0.2 * 0.01 / 12,
            net_ret_250m_cop = logsum(net_ret_250m) - 0.2 * 0.01 / 12
            ), by = list(year, month)]

    setkey(monthlyReturns, year, month)

    return(monthlyReturns)
}

getActualReturns <- function(wfcin, startEndDates) {
    sql_command <- paste0("
        select distinct on (r.wfcin, r.caldt)
          r.wfcin,
          r.caldt,
          r.mret as net_ret_act,
          coalesce((first_value(s.exp_ratio) over (partition by r.wfcin, r.caldt order by s.caldt desc)) / 12,
            avg(s.exp_ratio) over(),
            (select avg(exp_ratio) from clean.crsp_fs_wfcin) ) as m_exp
        from clean.mret_wfcin as r
        left join clean.crsp_fs_wfcin as s on
          s.wfcin = r.wfcin AND
          s.caldt <= r.caldt
        where r.wfcin = '", wfcin, "'
        order by caldt
        ")
    mret <- as.data.table(dbGetQuery(con, sql_command))
    mret <- mret[caldt >= startEndDates[, start_date] &
                     caldt <= startEndDates[, end_date], ]
    mret[, gross_ret_act := net_ret_act + m_exp]
    mret[, year := year(caldt)]
    mret[, month := month(caldt)]

    setkey(mret, year, month)

    return(mret)
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

addLogEntry <- function(wfcin, t, errorInCalculations, errorInWriteToDb) {
    timeElapsed <- t[["elapsed"]]
    messageToLog <- ""
    messageToLog <- paste("wfcin:", wfcin, ". Time elapsed:", timeElapsed, ".")
    messageToLog <- paste(messageToLog, "Calculations:")
    if(!errorInCalculations) {
        messageToLog <- paste(messageToLog, "OK.")

        messageToLog <- paste(messageToLog, "Write to DB:")
        if(!errorInWriteToDb) {
            messageToLog <- paste(messageToLog, "OK.")
        } else {
            messageToLog <- paste(messageToLog, "ERROR.")
        }
    } else {
        messageToLog <- paste(messageToLog, "ERROR.", "Write to DB: skipped.")
    }
    write(messageToLog, file = "log.txt",
          append = TRUE, sep = " ")

    return(messageToLog)
}