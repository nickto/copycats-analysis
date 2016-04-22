with 
  holdings_data_dates as (
    -- Select dates at which holdings change: fdate and lead(fdate) for every fund (wfcin)
    select 
      dhw.wfcin,
      dhw.fdate,
      lead(dhw.fdate, 1, dhw.fdate + 365) over 
        (partition by dhw.wfcin order by dhw.fdate asc) as lead_fdate
    from (select distinct on (wfcin, fdate)
            wfcin,
            fdate
          from clean.holdings_wfcin
          where wfcin in (500576) ) as dhw --105658 500576
  ),
  holdings_data as (
    select
      hdd.*,
      hw.cusip,
      hw.shares
    from holdings_data_dates as hdd
    left join clean.holdings_wfcin as hw on
      hdd.wfcin = hw.wfcin AND
      hdd.fdate = hw.fdate
  ),
  stock_data_extra as (
    -- Select data for stocks that are part of holdings.
    -- Select slightly more data to ensure that leads and lags for the dates in the actual analysis are always present
    select
      hd.wfcin,
      hd.fdate,
      hd.lead_fdate,
      hd.cusip,
      hd.shares,
      hd.shares * s.cfacshr as shares_adj,
      s.date as stock_date,
      lag(s.date) over (partition by hd.wfcin, s.ncusip, hd.fdate order by s.date) as lag_stock_date,
      lead(s.date) over (partition by hd.wfcin, s.ncusip, hd.fdate order by s.date) as lead_stock_date,
      abs(s.prc) as prc, --there are some errors in db, e.g. cusip = "29100510" June 3, 2000
      abs(lag(s.prc) over (partition by hd.wfcin, s.ncusip, hd.fdate order by s.date)) as lag_prc,
      abs(lead(s.prc) over (partition by hd.wfcin, s.ncusip, hd.fdate order by s.date)) as lead_prc,
      abs(s.prc / s.cfacpr) as prc_adj,
      lag(s.prc / s.cfacpr) over (partition by hd.wfcin, s.ncusip, hd.fdate order by s.date) as lag_prc_adj,
      lead(s.prc / s.cfacpr) over (partition by hd.wfcin, s.ncusip, hd.fdate order by s.date) as lead_prc_adj,
      s.cfacshr,
      lag(s.cfacshr) over (partition by hd.wfcin, s.ncusip, hd.fdate order by s.date) as lag_cfacshr,
      s.ret,
      s.exchcd,
      s.shrout,
      s.prc * s.shrout * 1000 as market_cap
    from holdings_data as hd
    left join stocks.daily as s ON
      s.ncusip = hd.cusip AND
      s.date >= hd.fdate - 10 AND
      s.date <= hd.lead_fdate + 10
  ),
  stock_data as (
    -- select data for stocks excluding extra days.
    -- add init_cfachshr for split adjusting shares
    select 
      sda.*,
      -- here we take lag, because we store new holdings only on the second day,
      -- thus the proper init_cfacshr is the lag of the second day
      first_value(sda.lag_cfacshr) OVER (partition by sda.wfcin, sda.cusip, sda.fdate order by sda.stock_date ASC) as init_cfacshr
    from stock_data_extra as sda
    where 
      sda.stock_date > sda.fdate AND 
      --sda.stock_date <= sda.lead_fdate AND
      sda.lag_stock_date >= sda.fdate AND 
      sda.lag_stock_date < sda.lead_fdate
      
  ),
  stock_data_lag_val as (
    -- calculate lagged value of a position in stock
    select
      sd.*,
      sd.shares * sd.init_cfacshr / sd.lag_cfacshr * sd.lag_prc as lag_val
    from stock_data as sd
  ),
  stock_data_lag_weight as (
    -- calculate lagged weight of position in stock
    select
      sdlv.*,
      sdlv.lag_val / (sum(sdlv.lag_val) over (partition by sdlv.wfcin, sdlv.stock_date)) as lag_weight
    from stock_data_lag_val as sdlv
  ),
  stock_returns as (
    select
      sdlw.*,
      ln(1 + sdlw.lag_weight * sdlw.ret) as ln_ret_weighted
    from stock_data_lag_weight as sdlw
  ),
  daily_returns as (
    select
      sr.wfcin,
      sr.fdate,
      sr.lead_fdate,
      sr.stock_date,
      exp(sum(sr.ln_ret_weighted)) - 1 as dret
    from stock_returns as sr
    group by 
      sr.wfcin,
      sr.fdate,
      sr.lead_fdate,
      sr.stock_date
  )
 
  

  select * from stock_returns
  order by
  wfcin,
  fdate,
  stock_date,
  cusip