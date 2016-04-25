with
  -----------------------------------------------------------------------------
  -- wfcin list 
  -----------------------------------------------------------------------------
  wfcin_list as (
    select distinct 
      wfcin,
      1 as join_column
    from clean.deoni_wfcin
    --offset 1800 --1141
    --limit 1
    where wfcin = '501335'
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
  -- Calculate holdings dates: at which to look for information, at which date 
  -- we start receiving returns
  -----------------------------------------------------------------------------
  holdings_multiple_stock_dates_le_fdate as (
    select
      f.wfcin,
      f.fdate,
      s.date
    from fdates as f
    left join clean.stock_dates as s on 
      s.date <= f.fdate
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
      ps.fdate
      --ps.stock_data_date
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
      --psl.stock_data_date,
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
      max(pf.fdate) over (partition by p.wfcin, p.period_no) holdings_fdate,
      max(pf.returns_from_date) over (partition by p.wfcin, p.period_no) holdings_returns_from
    from period_start_end_dates as p
    left join (select
                 wfcin, fdate, returns_from_date
               from period_start_end_dates
               where new_holdings) as pf on
      pf.returns_from_date <= p.returns_from_date
  ),
  -----------------------------------------------------------------------------
  -- holdings and weights of each stock (also value for future calculations)
  -----------------------------------------------------------------------------
  subset_holdings as (
    select
      h.wfcin,
      h.fdate,
      h.cusip,
      h.shares
    from clean.holdings_wfcin as h
    where wfcin in (select distinct wfcin from wfcin_list)
  ),
  holdings_shares as (
    select
      hd.wfcin,
      hd.fdate,
      hd.stock_data_date,
      h.cusip,
      h.shares
    from holdings_dates as hd
    left join subset_holdings as h on
      hd.wfcin = h.wfcin and
      hd.fdate = h.fdate
  ),
    subset_stock_data as (
    select
      s.date,
      s.ncusip as cusip,
      s.prc,
      s.ret,
      s.prc * s.shrout as market_cap,
      s.cfacshr
    from stocks.daily as s 
    where
      s.ncusip in (select distinct cusip from holdings_shares) and
      s.date in (select distinct stock_data_date from holdings_shares)
  ),
  holdings_weight as (
    select
      h.wfcin,
      h.fdate,
      h.stock_data_date,
      h.cusip,
      h.shares * coalesce(s.prc, 0) as val,
      (h.shares * coalesce(s.prc, 0)) / (sum((h.shares * coalesce(s.prc, 0))) over (partition by h.wfcin, h.fdate)) as weight
    from holdings_shares as h
    left join subset_stock_data as s on
      h.cusip = s.cusip and
      h.stock_data_date = s.date
  ),
  -----------------------------------------------------------------------------
  -- holdings and weights of each stock (also value for future calculations)
  -----------------------------------------------------------------------------
  weight_setup as (
    select
      p.wfcin,
      p.period_no,
      p.new_holdings,
      p.returns_from_date,
      p.returns_to_date,
      p.holdings_fdate,
      p.holdings_returns_from,
      w.cusip,
      w.weight as w_init,
      case when new_holdings then 
        returns_from_date 
      else
        s.lag_date
      end as weight_to_date
    from period_key_dates as p
    left join holdings_weight as w on
      p.wfcin = w.wfcin and
      p.holdings_fdate = w.fdate
    left join clean.stock_dates as s on
      p.returns_from_date = s.date
  ),
  tmp as (
    select
      s.date,
      s.ncusip,
      s.prc
    from start_end_dates as sed
    left join stocks.daily as s on
      s.date >= sed.start_date and
      s.date <= sed.end_date and
      s.ncusip in (select distinct cusip from holdings_shares)
  )

--select * from holdings_dates order by wfcin, fdate
--select * from period_start_dates order by wfcin, returns_from_date
--select *, case when date_part('dow', fdate) = 0 then 7 else date_part('dow', fdate) end as dow from period_start_end_dates order by wfcin, period_no
--select *, case when date_part('dow', fdate) = 0 then 7 else date_part('dow', fdate) end as dow from holdings_fdate_first_holdings_date order by wfcin, fdate
--select * from period_key_dates order by wfcin, period_no
--select * from tmp order by wfcin, fdate, cusip
--select * from subset_stock_data
--select * from holdings_weight
--select * from tmp order by wfcin, period_no, cusip
--select * from tmp
--select * from period_key_dates order by wfcin, period_no
--select * from period_start_lead_start_date order by wfcin, returns_from_date
select * from period_key_dates order by wfcin, period_no
--select * from start_end_dates