-- Select the list of dates, where we will have:
-- - all fdate (the date at which new holdings information is obtained) with
--   corresponding date at which the trade occurs.
-- - all end-of-month dates for which to calculate monthly returns


with
  stocks_eom_dates as (
  -- List of dates that are latest day of month at which the stock information is present
    select distinct 
      max(date) over (partition by date_part('year', date), date_part('month', date)) as eom
    from (select
            date,
            count(date)
          from stocks.daily
          group by date
         ) as dates_count
     where dates_count.count > 100
  ), 
  fdates_lead_lag as (
  -- select list of fdates for each fund (including lead and lag fdate)
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
          where wfcin in (105658, 500576) ) as dhw --105658 500576
  ),
  fdates as (
    select
      f.*,
      min(f.fdate) over (partition by f.wfcin) as min_fdate,
      max(f.lead_fdate) over (partition by f.wfcin) as max_fdate
    from fdates_lead_lag as f
  ),

  
  cusips as (
  -- select list of cusips for each fund
    select distinct on (f.wfcin, h.cusip)
      f.wfcin,
      h.cusip
    from fdates as f
    left join clean.holdings_wfcin as h on
      f.wfcin = h.wfcin
  ),
  wfcin_tmp as (
    select distinct 
      wfcin,
      1 as tmp
    from fdates
  ),
  eom_tmp as (
    select 
      *,
      1 as tmp
    from stocks_eom_dates
  ),
  wfcin_eom_tmp as (
    select
     w.fdate,
     e.eom 
    from wfcin_tmp as w
    full join eom_tmp as e on 
      w.tmp = e.tmp
    order by wfcin, eom

  )

  select
    
  from
  wfcin_eom_tmp
  order by 
  
  
