with 
-------------------------------------------------------------------------------
-- Copy returns
-------------------------------------------------------------------------------
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
          where wfcin in (105658) ) as dhw --105658 500576
  ),
  categories as (
    select distinct on (hdd.wfcin, hdd.fdate)
      hdd.*,
      cfw.caldt,
      cfw.per_com
    from holdings_data_dates as hdd
    left join clean.crsp_fs_wfcin as cfw ON
      hdd.wfcin = cfw.wfcin AND
      hdd.fdate >= cfw.caldt
    order by
      hdd.wfcin,
      hdd.fdate,
      cfw.caldt DESC
  ),
  holdings_data as (
    -- add holdings data for each of the selected days in holdings_data_dates
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
    -- Calculate weighted returns
    select
      sdlw.*,
      sdlw.lag_weight * sdlw.ret as ret_weighted
    from stock_data_lag_weight as sdlw
  ),
  daily_returns as (
    -- claculate daily returns based on returns of each stock
    select
      sr.wfcin,
      sr.fdate,
      sr.lead_fdate,
      sr.stock_date,
      sum(sr.ret_weighted) - 1 as dret
    from stock_returns as sr
    group by 
      sr.wfcin,
      sr.fdate,
      sr.lead_fdate,
      sr.stock_date
  ),
  -------------------------------------------------------------------------------------------------
  -------------------------------------------------------------------------------------------------
  first_holdings_day as (
    -- select first day at which we already store new holdings (this is usually the next day after fdate)
    select distinct on (sd.wfcin, sd.fdate, sd.cusip)
      sd.wfcin,
      sd.fdate,
      sd.cusip,
      sd.stock_date,
      sd.lag_stock_date,
      sd.shares_adj * sd.prc_adj / (sum(sd.shares_adj * sd.prc_adj) over (partition by sd.fdate, sd.stock_date)) as weight
    from stock_data as sd
    order by sd.wfcin, sd.fdate, sd.cusip, sd.stock_date ASC  
  ),
  last_holdings_day as (
    -- select the last day at which the holdings are stored (this is usueally leaf_fdate)
    select distinct on (sd.wfcin, sd.fdate, sd.cusip)
      sd.wfcin,
      sd.lead_fdate,
      sd.cusip,
      sd.stock_date,
      sd.shares_adj * sd.prc_adj / (sum(sd.shares_adj * sd.prc_adj) over (partition by sd.fdate, sd.stock_date)) as weight
    from stock_data as sd
    order by sd.wfcin, sd.fdate, sd.cusip, sd.stock_date DESC
  ),
  weight_chg as (
    -- find change in weight of each stock
    select 
      coalesce(f.wfcin, l.wfcin) as wfcin,
      coalesce(f.fdate, l.lead_fdate) as fdate,
      coalesce(f.lag_stock_date, l.stock_date) as chg_date,
      coalesce(f.cusip, l.cusip) as cusip,
      coalesce(f.weight, 0) - coalesce (l.weight, 0) as weight_chg
    from first_holdings_day as f
    full join last_holdings_day as l ON
      f.wfcin = l.wfcin AND
      f.fdate = l.lead_fdate AND
      f.cusip = l.cusip
  ),
  costs_preset as (
    select
      wc.*,
      s.prc,
      case when wc.weight_chg >= 0 then
        true 
      else 
        false
      end as buy,
      abs(s.prc * s.shrout * 1000) as market_cap,
      abs(s.prc * s.shrout) as market_cap_thsds,
      case when s.exchcd = 3 then 
        1
      else 
        0
      end as nasdaq
    from weight_chg as wc
    left join stocks.daily as s on
      s.ncusip = wc.cusip AND
      s.date = wc.chg_date
  ),
  costs_by_stock as (
    select 
      cp.*,
      case when cp.buy then
        abs((1.098 + 0.336 * cp.nasdaq - 0.084 * ln(cp.market_cap_thsds) + 13.807 * (1 / cp.prc)) * cp.weight_chg)
      else 
        abs((0.979 + 0.058 * cp.nasdaq - 0.059 * ln(cp.market_cap_thsds) +  6.537 * (1 / cp.prc)) * cp.weight_chg)
      end as costs_term_a,
      case when cp.buy then
        abs(0.092 / cp.market_cap * cp.weight_chg * cp.weight_chg)
      else
        abs(0.214 / cp.market_cap * cp.weight_chg * cp.weight_chg)
      end as costs_term_b
    from costs_preset as cp
  ),
  fdate_chg_date_link as (
    SELECT
      wfcin,
      fdate,
      mode()  WITHIN GROUP (ORDER BY chg_date) as chg_date
    FROM costs_preset
    group by 
      wfcin,
      fdate
  ),
  costs as (
    select
      cbs.wfcin,
      cbs.fdate,
      fcdl.chg_date,
      sum(cbs.costs_term_a) as costs_term_a, -- do nothing, just add a + b
      sum(cbs.costs_term_b) as costs_term_b -- should be multiplied by tna * per_com
    from costs_by_stock as cbs
    left join fdate_chg_date_link as fcdl on
      fcdl.wfcin = cbs.wfcin AND
      fcdl.fdate = cbs.fdate
    group by 
      cbs.wfcin,
      cbs.fdate,
      fcdl.chg_date
  )
 
  
select
* 
from categories
order by wfcin, fdate, caldt