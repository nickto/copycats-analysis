with wfcin_fdate as (
  -- select a list of fdates per every fund (wfcin)
  select distinct
    h.wfcin,
    h.fdate
  from clean.holdings_wfcin as h
  where h.wfcin in (240388)
  --(240388, 500576, 200211)
  order by wfcin, fdate
),
lead_fdate as (
  -- find lead fdate for every fdate in a fund. If there is no lead,
  -- then add 183 days (half a year)
  select 
    wf.wfcin,
    wf.fdate,
    lead(wf.fdate) over (partition by wfcin order by wf.fdate) as lead_fdate
  from wfcin_fdate as wf
),
h_lead as (
  -- select holdings at every fdate 
  select 
    lf.wfcin,
    lf.fdate,
    COALESCE(lf.lead_fdate, lf.fdate + 183) as lead_fdate,
    h.cusip,
    h.shares
  from lead_fdate as lf
  left join clean.holdings_wfcin as h on 
    lf.wfcin = h.wfcin AND
    lf.fdate = h.fdate
),
share_data as (
  -- select daily share data for time period between fdate and lead_fdate
  select 
    hl.wfcin,
    hl.fdate,
    hl.lead_fdate,
    hl.cusip,
    hl.shares,
    s.date as stock_date,
    s.prc,
    lag(s.prc) over (partition by hl.cusip order by s.date) as lag_prc,
    s.ret,
    s.shrout,
    s.cfacshr,
    lag(s.cfacshr) over (partition by hl.cusip order by s.date) as lag_cfacshr,
    s.exchcd,
    first_value(s.cfacshr) OVER (partition by hl.wfcin, hl.cusip, hl.fdate order by hl.fdate) as init_cfacshr
  from h_lead as hl
  left join stocks.daily as s on
    hl.cusip = s.ncusip AND
    hl.lead_fdate >= s.date AND
    hl.fdate < s.date
  order by 
    hl.wfcin,
    hl.fdate,
    s.date,
    hl.cusip
    
),
share_data_adj as (
  -- calculate split-adjusted share number
  select
    sd.*, 
    sd.init_cfacshr / sd.cfacshr * sd.shares as shares_adj
  from share_data as sd
),
share_data_val as (
  -- calculate value of each share type
  select
    sd.*,
    sd.shares * sd.init_cfacshr / sd.lag_cfacshr * sd.lag_prc as lag_val
  from share_data_adj as sd
  where sd.prc is not null
),
share_data_tot_daily_val as (
  -- calculate daily portfolio value
  select
    sd.*,
    COALESCE(sum(lag_val) over (partition by wfcin, stock_date), 0) as lag_total_val
  from share_data_val as sd
),
share_data_weight as (
  -- calculate weight of each share type
  select
    sd.*,
    COALESCE(val / total_val, 0) as weight
  from share_data_tot_daily_val as sd
),
share_data_returns as (
  -- calculate weighted returns (and log-returns for easier summation)
  select 
    *,
    COALESCE(ret * weight, 0) as ret_weighted,
    COALESCE(ln(1 + ret * weight), 0) ln_ret_weighted
  from share_data_weight
),
daily as (
  select
    r.wfcin,
    r.fdate,
    r.lead_fdate,
    r.stock_date,
    exp(sum(r.ln_ret_weighted)) - 1 as return_daily
  from share_data_returns as r
  group by
    r.wfcin,
    r.fdate,
    r.lead_fdate,
    r.stock_date
)
select 
  *
from share_data_returns
order by wfcin, cusip, stock_date