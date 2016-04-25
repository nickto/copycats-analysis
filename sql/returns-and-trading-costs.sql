with 
-------------------------------------------------------------------------------
-- Copy returns
-------------------------------------------------------------------------------
-- 
  holdings_dates as (
    -- Select dates at which holdings change: fdate and lead(fdate) for every fund (wfcin)
    -- Also select lead and lag fdate
    -- If lead_fdate is missing, then just select a date that is one year in the future compared to the last fdate
    select 
      dhw.wfcin,
      dhw.fdate,
      lead(dhw.fdate, 1, dhw.fdate + 365) over 
        (partition by dhw.wfcin order by dhw.fdate asc) as lead_fdate,
      lag(dhw.fdate) over 
        (partition by dhw.wfcin order by dhw.fdate asc) as lag_fdate 
    from (select distinct on (wfcin, fdate)
            wfcin,
            fdate
          from clean.holdings_wfcin
          where wfcin in (101139) ) as dhw --105658 500576
     where dhw.fdate >= '1990-01-01'
  ),
  holdings_data as (
    -- add holdings data for each of the selected days in holdings_dates
    select
      hd.wfcin,
      hd.fdate,
      hd.lead_fdate,
      hw.cusip,
      hw.shares
    from holdings_dates as hd
    left join clean.holdings_wfcin as hw on
      hd.wfcin = hw.wfcin AND
      hd.fdate = hw.fdate
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
      sum(sr.ret_weighted) as dret
    from stock_returns as sr
    group by 
      sr.wfcin,
      sr.fdate,
      sr.lead_fdate,
      sr.stock_date
  ),
  -------------------------------------------------------------------------------------------------
  -------------------------------------------------------------------------------------------------
  fdate_to_fdate_returns as (
    select
      hd.wfcin,
      hd.fdate,
      exp(sum(ln(1 + dr.dret))) - 1 as fdate_stock_ret
    from holdings_dates as hd
    left join daily_returns as dr on
      hd.wfcin = dr.wfcin AND
      hd.fdate = dr.fdate
    group by 
      hd.wfcin,
      hd.fdate
  ),
  categories_dates_extra as (
    -- Select category  multiple dates per each fdate
    select
      hdd.*,
      cfw.caldt
    from holdings_dates as hdd
    left join clean.crsp_fs_wfcin as cfw ON
      hdd.wfcin = cfw.wfcin AND
      hdd.fdate >= cfw.caldt
    where cfw.per_cash is not null
    order by
      hdd.wfcin,
      hdd.fdate,
      cfw.caldt DESC
  ),
  category_dates as (
    select distinct on (cda.wfcin, cda.fdate)
      -- Select only one caldt (date for category information) per one fdate
      cda.wfcin,
      cda.fdate,
      cda.lead_fdate,
      cda.lag_fdate,
      min(cda.fdate) over (partition by cda.wfcin) as min_fdate,
      case when coalesce(caldt < lag_fdate, FALSE) then
        -- caldt is already stored, therefore no need to update it to outdated value
        NULL
      else
        caldt
      end as caldt
    from categories_dates_extra as cda
  ),
  category_data_raw as (
    select distinct on (cd.wfcin, cd.fdate, cd.caldt)
      -- Select data for each category date. There should be no duplcates per each
      -- caldt, but to ensure this select only one caldt per fdate
      cd.wfcin,
      cd.fdate,
      cd.lead_fdate,
      cd.lag_fdate,
      cd.min_fdate,
      cfw.tna_latest * 1000000 tna_latest,
      case when (cd.fdate = cd.min_fdate)
                and ((cfw.per_com is null) OR  (cfw.per_cash = 0 and cfw.per_com = 0)) 
                then
        100 - 
        (select avg(avg) from (select
                        avg(per_cash)
                      from clean.crsp_fs_wfcin
                      where caldt >= '1990-01-01'
                      group by date_part('year', caldt)) as avg_per_year ) 
      else
        cfw.per_com
      end  as per_com
      ,
      case when (cd.fdate = cd.min_fdate)
                and ((cfw.per_cash is null) OR  (cfw.per_cash = 0 and cfw.per_com = 0)) 
                then
        (select avg(avg) from (select
                        avg(per_cash)
                      from clean.crsp_fs_wfcin
                      where caldt >= '1990-01-01'
                      group by date_part('year', caldt)) as avg_per_year )
      else
        cfw.per_cash
      end  as per_cash,
      lag(per_com) over (partition by cd.wfcin order by cd.fdate) as lag_per_com,
      ffr.fdate_stock_ret,
      lag(ffr.fdate_stock_ret) over (partition by ffr.wfcin order by ffr.fdate) as lag_stock_ret
    from category_dates as cd
    left join clean.crsp_fs_wfcin as cfw on
      cd.wfcin = cfw.wfcin AND
      cd.caldt = cfw.caldt
    left join fdate_to_fdate_returns as ffr on
      cd.wfcin = ffr.wfcin AND
      cd.lag_fdate = ffr.fdate
  ),
    
  category_data_inferred as (
    select
      cd.wfcin,
      cd.fdate,
      case when per_com is null then
        (lag(cd.per_com) over (partition by cd.wfcin order by cd.fdate)) * cd.lag_stock_ret
      else
        cd.per_com
      end
    from category_data_raw as cd
  ),
  
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
      coalesce(f.weight, 0) as weight_prev,
      coalesce (l.weight, 0) as weight_curr,
      --cd.tna_latest,
      cd.per_com--,
      --cd.per_cash
    from first_holdings_day as f
    full join last_holdings_day as l ON
      f.wfcin = l.wfcin AND
      f.fdate = l.lead_fdate AND
      f.cusip = l.cusip
    left join category_data_inferred as cd on
      f.wfcin = cd.wfcin AND
      f.fdate = cd.fdate
  )

  
 
  
select
  *
from category_data_inferred
