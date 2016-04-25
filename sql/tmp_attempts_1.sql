with 
  -----------------------------------------------------------------------------
  -- Set up: 
  -- - list of funds (wfcin)
  -- - list of dates in the whole sample
  -- - list of stock dates
  -----------------------------------------------------------------------------  
  whole_sample_dates as (
    SELECT 
      date_trunc('day', dd)::date as date--,
      --1 as join_column
    FROM generate_series
            ( '1990-01-01'::timestamp 
            , '2015-12-31'::timestamp
            , '1 day'::interval) dd
  ),
  wfcin_list as (
    select distinct 
      wfcin,
      1 as join_column
    from clean.deoni_wfcin
    offset 1141
    limit 1
    --where wfcin in (105658, 500576, 102387)
    --where wfcin in (240477, 500714, 500717, 100320, 109110, 500718, 105475, 501067, 101724, 101723, 106375, 500716, 102105, 102654, 109275, 101055, 109111, 105780, 102417, 105779)

  ),
  stock_dates as (
    select 
      date,
      lag(date) over () as lag_date,
      lead(date) over () as lead_date
    from (select
            date,
            count(date)
          from stocks.daily
          group by date) as date_count
    where count > 1000
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
      least(f.max_fdate + 365, r.max_caldt) as end_date
    from min_max_fdate as f
    left join min_max_return_dates as r on
      f.wfcin = r.wfcin
  ),
  -----------------------------------------------------------------------------
  -- For each fdate, find dates (from stock dates) starting from which the 
  -- holdings are actually held, and the last date at which we hold them (sell
  -- in the end of that day)
  -----------------------------------------------------------------------------
  fdate_stock_dates_setup as (
    select --distinct on (f.wfcin, f.fdate)
      f.wfcin,
      f.fdate,
      f.lead_fdate,
      s.date,
      s.lag_date,
      s.lead_date
    from fdates as f
    left join stock_dates as s on
      s.date > f.fdate AND
      s.lag_date <= f.lead_fdate
    order by 
      f.wfcin,
      f.fdate,
      s.date
  ),
  fdate_stock_dates as (
    select distinct on (fs.wfcin, fs.fdate)
      fs.wfcin,
      fs.fdate,
      fs.lead_date,
      fs.date,
      min(fs.lead_date) over (partition by fs.wfcin, fs.fdate) as first_holdings_date,
      max(fs.date) over (partition by fs.wfcin, fs.fdate) as last_holdings_date
    from fdate_stock_dates_setup as fs
  ),
  -----------------------------------------------------------------------------
  -- Find periods for which to calculate returns. These periods include either
  -- a calendar month, or a part of the month if there is a change in holdings.
  -- For each period, the beginning and the end of date is selected.
  -----------------------------------------------------------------------------
  month_beginning_dates as (
    -- find all last days from returns. Then add lead of that day to get the 
    -- first day of the period (currently we do not take into account any
    -- periods that arise due to rebalancing in the middle of the month)
    select distinct on (sed.wfcin, r.caldt)
      sed.wfcin,
      min(s.date) over (partition by date_part('year', s.date), date_part('month', s.date)) as month_beginning,
      --max(s.date) over (partition by date_part('year', s.date), date_part('month', s.date)) as month_end
      1 as join_column
    from start_end_dates as sed
    left join fund_mret_dates as r on
      sed.wfcin = r.wfcin AND
      r.caldt >= sed.start_date AND
      r.caldt <= sed.end_date
    left join stock_dates as s on
      date_part('year', s.date) = date_part('year', r.caldt) AND
      date_part('month', s.date) = date_part('month', r.caldt) AND
      s.date >= sed.start_date AND
      s.date <= sed.end_date
  ),
  holdings_beginning_dates as (
    select
      f.wfcin,
      f.first_holdings_date,
      --f.last_holdings_date,
      1 as join_column
    from fdate_stock_dates as f
  ),
  period_beginning_dates as (
    select
      m.wfcin,
      m.month_beginning as period_beginning
    from month_beginning_dates as m
    union
    select 
      h.wfcin,
      h.first_holdings_date as period_beginning
    from holdings_beginning_dates as h
  ),
  period_beginning_end_dates_setup as (
    select 
      b.wfcin,
      b.period_beginning,
      lead(b.period_beginning) over (partition by b.wfcin order by b.period_beginning) as lead_period_beginning
    from period_beginning_dates as b
  ),
  period_beginning_end_dates as (
    select
      ps.wfcin,
      ps.period_beginning,
      --ps.lead_period_beginning,
      coalesce(s.lag_date, sed.end_date) as period_end,
      row_number() over (partition by ps.wfcin order by ps.period_beginning) as period_no
    from period_beginning_end_dates_setup as ps
    left join stock_dates as s on
      ps.lead_period_beginning = s.date
    left join start_end_dates as sed on
      ps.wfcin = sed.wfcin
    where 
      ps.period_beginning != coalesce(s.lag_date, sed.end_date)
  ),
  -----------------------------------------------------------------------------
  -- Add the most recent holdings information (share number) for each period 
  -- beginning date 
  -----------------------------------------------------------------------------  
  holdings_fdates_setup as (
    select distinct on (p.wfcin, p.period_beginning, p.period_end, s.fdate)
      p.wfcin,
      p.period_no,
      p.period_beginning,
      p.period_end,
      --s.fdate,
      first_value(s.fdate) over (partition by p.wfcin, p.period_beginning order by s.fdate desc) as latest_fdate
    from period_beginning_end_dates as p
    left join fdate_stock_dates as s on
      p.wfcin = s.wfcin AND
      p.period_beginning >= s.first_holdings_date

  ),
  holdings_fdates_wo_fdate_stock_date as (
    select distinct on (hf.wfcin, hf.period_beginning, hf.period_end, hf.latest_fdate)
      hf.wfcin,
      hf.period_no,
--      hf.fdate,
      hf.period_beginning,
      hf.period_end,
      hf.latest_fdate,
      min(hf.period_beginning) over (partition by hf.wfcin, hf.latest_fdate) as latest_fdate_period_beginning
      --hf.latest_fdate_stock_date
    from holdings_fdates_setup as hf
  ),
  holdings_fdates as (
    select
      d.wfcin,
      d.period_no,
      d.latest_fdate,
      d.period_beginning,
      d.period_end,
      s.lag_date as latest_fdate_stock_date
    from holdings_fdates_wo_fdate_stock_date as d 
    left join stock_dates as s on 
      d.latest_fdate_period_beginning = s.date
  ),
  subset_holdings as (
  -- subsetting holdings increases performance singnificantly for small
  -- amounts of fu
    select
     *
    from clean.holdings_wfcin
    where wfcin in (select distinct wfcin from wfcin_list)
    --where wfcin in (105658, 500576, 102387)
    --where wfcin in (240477, 500714, 500717, 100320, 109110, 500718, 105475, 501067, 101724, 101723, 106375, 500716, 102105, 102654, 109275, 101055, 109111, 105780, 102417, 105779)
  ),
  holdings as (
    select
      hf.wfcin,
      hf.period_no,
      hf.latest_fdate,
      hf.latest_fdate_stock_date,
      hf.period_beginning,
      hf.period_end,
      h.cusip,
      h.shares
    from holdings_fdates as hf
    left join subset_holdings as h on 
    --left join clean.holdings_wfcin as h on 
      hf.wfcin = h.wfcin and
      hf.latest_fdate = h.fdate
  ),
  subset_stocks as (
    select
      s.date,
      s.ncusip as cusip,
      case when s.exchcd = 3 then true else false end as nasdaq,
      abs(s.prc) * s.shrout * 1000000 as market_cap,
      abs(s.prc),
      abs(s.prc / s.cfacshr) as prc_adj,
      s.cfacshr,
      s.ret
    from stocks.daily as s where
      s.ncusip in (select distinct 
                     cusip 
                   from holdings)
      AND
      s.date in (select distinct 
                   period_beginning
                 from holdings
                 UNION
                 select distinct
                   period_end
                 from holdings)
  ),
  holdings_stocks as (
    select 
      h.wfcin,
      h.period_no,
      h.latest_fdate,
      h.latest_fdate_stock_date,
      h.period_beginning,
      h.period_end,
      h.shares,
      h.shares * s.cfacshr as shares_adj,
      s.*
    from holdings as h 
    left join subset_stocks as s on
      h.cusip = s.cusip and
      h.period_beginning = s.date
  ),
  -----------------------------------------------------------------------------
  -- Add category data
  -----------------------------------------------------------------------------  
  known_assets_value as (
    select
      fs.wfcin,
      fs.fdate,
      fs.first_holdings_date,
      h.cusip,
      h.shares,
      s.prc
    from fdate_stock_dates as fs
    left join subset_holdings as h on
      fs.wfcin = h.wfcin AND
      fs.fdate = h.fdate
    left join stocks.daily as s on 
      h.cusip = s.ncusip AND
      fs.first_holdings_date = s.date
    
  ),
  category_lead_lag_period as (
    select
      d.*,
      lag(d.period_beginning) over (partition by d.wfcin order by d.period_beginning) as lag_period_beginning,
      lag(d.period_end) over (partition by d.wfcin order by d.period_beginning) as lag_period_end
      
    from period_beginning_end_dates as d
--    left join clean.crsp_fs_wfcin as c on
--      d.wfcin = c.wfcin AND
--      d.caldt 
  ),
  cash as (
    select 
      d.*,
--      case when period_no = 1 AND c.per_com is null then
--        coalesce(c.per_com,
--                 (select avg(avg) from (select
--                    avg(per_cash)
--                  from clean.crsp_fs_wfcin
--                  where caldt >= '1990-01-01'
--                  group by date_part('year', caldt)) as avg_per_year ) ) 
--      else
--        c.per_cash
--      end as per_cash,
      c.per_com,
      c.per_cash,
      fs.fdate,
      t.assets * 10000 as assets
    from category_lead_lag_period as d
    left join clean.crsp_fs_wfcin as c on
      d.wfcin = c.wfcin AND
      -- the following works, because it checks whether there is new information
      -- about category weights during the period when there was no rebalancing.
      -- in most cases new category data has the same date as new holdings data
      d.lag_period_beginning <= c.caldt AND
      d.period_beginning > c.caldt
    left join fdate_stock_dates as fs on
      d.wfcin = fs.wfcin and
      d.period_beginning = fs.first_holdings_date
    left join clean.tr_fc_wfcin as t on 
      d.wfcin = t.wfcin AND
      fs.fdate = t.fdate
  )
  

  

--select * from fdates order by wfcin, fdate
--select * from wfcin_list order by wfcin
--select * from fund_mret_dates order by wfcin, caldt
--select distinct wfcin, max_caldt from fund_mret_dates
--select * from start_end_dates order by wfcin
--select * from sample_dates_per_wfcin order by wfcin, date
--select * from fdate_stock_dates_setup order by wfcin, fdate, date
--select * from fdate_stock_dates order by wfcin, fdate
--select * from stock_dates
--select * from month_beginning_dates order by wfcin, month_beginning
--select * from holdings_beginning_dates order by wfcin, first_holdings_date
--select * from period_beginning_dates order by wfcin, period_beginning
select * from period_beginning_end_dates_setup order by wfcin, period_beginning
--select * from period_beginning_end_dates order by wfcin, period_beginning
--select * from holdings_fdates_setup order by wfcin, period_no
--select * from holdings_fdates order by wfcin, period_beginning
--select * from holdings order by wfcin, period_beginning, cusip
--select * from cash order by wfcin, period_beginning--, cusip
--select * from known_assets_value order by wfcin, fdate, cusip
--select * from subset_stocks
--select * from holdings_stocks order by wfcin, period_no, cusip
