-- Create clean dataset from existing data in PostgreSQL

-------------------------------------------------------------------------------
-- Create schema
CREATE SCHEMA IF NOT EXISTS clean;


-------------------------------------------------------------------------------
-- Create table that links (CRSP) crsp_fundno to wfcin
--
-- This table links crsp_fundno (fund identification in CRSP tables) to wfcin
-- that is a unique fund identifier. Since there are multiple crsp_fundno per
-- single wfcin (due to different share classes), we select the share class
-- that has the longest history. If there share classes with the same history
-- length, we select the one that has lower crsp_fundno (which is an arbitrary 
-- choice).
--
-- The resulting table consists of crsp_fundno and corresponding wfcin. Only 
-- funds that have this correspondence are included.

CREATE TABLE IF NOT EXISTS clean.crsp_wfcin AS (
  WITH 
  -- count the length of time series for each fund and share type (crsp_fundno)
  crsp_ts_length AS (
    SELECT
      link2.wfcin,
      fs.crsp_fundno,
      COUNT(fs.crsp_fundno) ts_length
    FROM 
      crsp.fund_summary AS fs
    INNER JOIN 
      mflinks.link2 ON
      link2.crsp_fundno = fs.crsp_fundno
    GROUP BY
      link2.wfcin,
      fs.crsp_fundno
  ),
  -- count max time-series length per fund (mfcin)
  wfcin_ts_longest AS (
    SELECT 
      wfcin,
      MAX(ts_length) AS ts_max
    FROM 
      crsp_ts_length
    GROUP BY 
      wfcin
  ), 
  -- select fund and share type that has the same ts and max ts length
  crsp_ts_longest AS (
    SELECT
      wl.*,
      cl.crsp_fundno,
      cl.ts_length
    FROM 
      wfcin_ts_longest AS wl
    LEFT JOIN
      crsp_ts_length AS cl ON
      wl.wfcin = cl.wfcin AND 
      wl.ts_max = cl.ts_length
  ),
  -- select lowest fundno if there are multiple per wfcin
  wfcin_crisp AS (
    SELECT DISTINCT ON (wfcin)
      *
    FROM 
      crsp_ts_longest
    ORDER BY
      wfcin,
      crsp_fundno
  )
  -- table that contains only wfcin - crsp_fundno pairs
  SELECT
    wfcin,
    crsp_fundno
  FROM 
    wfcin_crisp
);
-- Create indeces for this new table
CREATE INDEX IF NOT EXISTS 
  crsp_wfcin_wfcin_idx 
ON 
  clean.crsp_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS 
  crsp_wfcin_crsp_fundno_idx
ON 
  clean.crsp_wfcin(crsp_fundno);


-------------------------------------------------------------------------------
-- Create table that links (TR) fundno to wfcin
--
-- This table links fundno (fund identification from TR database) to wfcin 
-- (unique fund identifier). Since fundno is not unique, linking is based 
-- also on fdate which is the date when TR obtained information.
--
-- The resulting table consists of wfcin, fdate. This fields allow unique
-- identification of a fund in TR datbase. The table also has rdate and
-- max_fdate, because there are often multiple fdate per single rdate, because
-- of data stalleness. Depending on application, one might be interested in
-- the most complete information about an rdate (fdate = max_fdate) or in 
-- information that is available as soon as possible even if it is not complete
-- (then one would look at all fdate per rdate).
CREATE TABLE IF NOT EXISTS clean.tr_wfcin AS (
  -- find max fdate for each fundno-rdate pair
  WITH tr_max_fdate as (
    SELECT 
      fc.fundno,
      fc.rdate,
      mf.wfcin,
      MAX(fc.fdate) as max_fdate
    from 
      tr.fund_characteristics as fc
    inner join 
      mflinks.link1 as mf on
      fc.fundno = mf.fundno and
      fc.fdate = mf.fdate
    group by
      fc.fundno,
      fc.rdate,
      mf.wfcin
  ),
  -- link all TR funds to wfcin if it exists
  tr_wfcin_link_all AS (
  SELECT 
    fc.fundno,
    fc.rdate,
    fc.fdate,
    mf.wfcin
  from 
    tr.fund_characteristics as fc
  inner join 
    mflinks.link1 as mf on
    fc.fundno = mf.fundno and
    fc.fdate = mf.fdate
  ORDER BY 
    fc.fundno,
    fc.rdate,
    fc.fdate
  )
  -- add max fdate (and rdate) to the final table
  select 
    tra.*,
    trm.max_fdate
  from 
    tr_wfcin_link_all as tra
  inner join tr_max_fdate as trm on
    tra.fundno = trm.fundno AND
    tra.rdate  = trm.rdate AND
    tra.wfcin  = trm.wfcin
);
-- Create indeces for this new table
CREATE INDEX IF NOT EXISTS 
  tr_wfcin_wfcin_idx 
ON 
  clean.tr_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS 
  tr_wfcin_fundno_idx 
ON 
  clean.tr_wfcin(fundno);
CREATE INDEX IF NOT EXISTS 
  tr_wfcin_fdate_idx 
ON 
  clean.tr_wfcin(fdate);
  CREATE INDEX IF NOT EXISTS 
  tr_wfcin_rdate_idx 
ON 
  clean.tr_wfcin(rdate);


-- Select only the funds that are domestic equity only, non-index, using the 
-- methodology approach Wang and Verbeek (2009). 
--
-- The name prefix stands for "Domestic equity-only"
CREATE TABle if not exists clean.deo_wfcin as (
  -- TR funds that fit into required category by IOC
  with tr_deo as (
    select Distinct on (trw.wfcin, trw.fundno, trw.fdate)
      trw.*,
      trc.ioc,
      trc.fundname
    from clean.tr_wfcin as trw
    inner join tr.fund_characteristics as trc on
      trw.fundno = trc.fundno and
      trw.fdate  = trc.fdate
    where 
      trc.ioc in (2,3,4,9) or 
      trc.ioc is null
  ),
  -- CRSP funds that fit into required category by IO and have more than 90% of
  -- cash plus equity, or this information is missing. Or policy is explicitly 
  -- common stock (CS).
  crsp_deo as (
    select distinct on (crspw.wfcin, crspw.crsp_fundno)
      crspw.*,
      crsps.fund_name,
      crsps.index_fund_flag,
      crsps.lipper_obj_cd,
      crsps.wbrger_obj_cd,
      crsps.si_obj_cd,
      (crsps.per_com + crsps.per_cash) as eq_plus_cash
    from clean.crsp_wfcin as crspw
    inner join crsp.fund_summary as crsps on
      crspw.crsp_fundno = crsps.crsp_fundno
    where 
      (
      crsps.lipper_obj_cd in ('EI', 'EIEI', 'EMN', 'FLX', 'G', 'GI', 'I', 'LCCE',
        'LCGE', 'LCVE', 'LSE', 'MC', 'MCCE', 'MCGE', 'MCVE', 'MLCE', 'MLGE', 
        'MLVE', 'SCCE', 'SCGE', 'SCVE', 'SESE', 'SG') OR 
      crsps.wbrger_obj_cd in ('SCG', 'AGG', 'G', 'G-S', 'S-G', 'GRO', 'LTG', 'I',
        'I-S', 'IEQ', 'ING', 'GCI', 'G-I', 'G-I-S', 'G-S-I', 'I-G', 'I-G-S', 
        'I-S-G', 'S-G-I', 'S-I-G', 'GRI', 'MCG') OR
      crsps.si_obj_cd in ('SCG', 'GRO', 'AGG', 'ING', 'GRI', 'GMC') or 
      (crsps.lipper_obj_cd is null and
       crsps.wbrger_obj_cd is null and
       crsps.si_obj_cd     is null) 
      ) AND (
      (crsps.per_com + crsps.per_cash) > 90 OR
      (crsps.per_com is null and crsps.per_cash is null)
      ) OR (
      crsps.policy = 'CS'
      )
  )
  select 
    tr_deo.max_fdate,
    tr_deo.fdate,
    tr_deo.rdate,
    tr_deo.fundno,
    tr_deo.fundname,
    crsp_deo.*
  from tr_deo
  inner join crsp_deo on
    tr_deo.wfcin = crsp_deo.wfcin
);
CREATE INDEX IF NOT EXISTS 
  deo_wfcin_wfcin_idx 
ON 
  clean.deo_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS 
  deo_wfcin_fdate_idx 
ON 
  clean.deo_wfcin(fdate);
CREATE INDEX IF NOT EXISTS 
  deo_wfcin_rdate_idx 
ON 
  clean.deo_wfcin(rdate);
CREATE INDEX IF NOT EXISTS 
  deo_wfcin_fundno_idx 
ON 
  clean.deo_wfcin(fundno);
CREATE INDEX IF NOT EXISTS 
  deo_wfcin_crsp_fundno_idx 
ON 
  clean.deo_wfcin(crsp_fundno);
CREATE INDEX IF NOT EXISTS 
  deo_wfcin_wfcin_idx 
ON 
  clean.deo_wfcin(wfcin);


-- Select non-index funds as suggested by Wang and Verbeek (2009): partially 
-- matching string that contain word "index" in some form. But additionaly
-- exlcude the ones that have index fund flag
-- 
-- The name prefix stands for "Domestic equity-only non-index"
create table if not exists clean.deoni_wfcin as (
  select 
    *
  from clean.deo_wfcin
  WHERE 
    (lower(fundname) not similar to '%index|inde|indx|inx|idx|s\&p|msci%') AND
    (lower(fund_name) not similar to '%index|inde|indx|inx|idx|s\&p|msci%') AND
    index_fund_flag is null
);
CREATE INDEX IF NOT EXISTS 
  deoni_wfcin_wfcin_idx 
ON 
  clean.deoni_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS 
  deoni_wfcin_fdate_idx 
ON 
  clean.deoni_wfcin(fdate);
CREATE INDEX IF NOT EXISTS 
  deoni_wfcin_rdate_idx 
ON 
  clean.deoni_wfcin(rdate);
CREATE INDEX IF NOT EXISTS 
  deoni_wfcin_fundno_idx 
ON 
  clean.deoni_wfcin(fundno);
CREATE INDEX IF NOT EXISTS 
  deoni_wfcin_crsp_fundno_idx 
ON 
  clean.deoni_wfcin(crsp_fundno);




-------------------------------------------------------------------------------
-- Create tr.holdings table with wfcin indeces
CREATE TABLE IF NOT EXISTS clean.holdings_wfcin as (
  SELECT 
    h.*,
    w.wfcin
  FROM
    tr.holdings as h
  LEFT JOIN 
    clean.tr_wfcin as w ON
    w.fundno = h.fundno AND
    w.fdate = h.fdate
);

CREATE INDEX IF NOT EXISTS
  holdings_wfcin_wfcin_idx
ON
  clean.holdings_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS
  holdings_wfcin_fdate_idx
ON
  clean.holdings_wfcin(fdate);
CREATE INDEX IF NOT EXISTS
  holdings_wfcin_fundno_idx
ON
  clean.holdings_wfcin(fundno);

--------------------------------------------------------------------------------
-- Crate crsp.monthly_returns table with wfcin indeces
CREATE TABLE IF NOT EXISTS clean.mret_wfcin as (
  SELECT 
    r.*,
    w.wfcin
  FROM
    crsp.monthly_returns as r
  LEFT JOIN 
    clean.crsp_wfcin as w ON
    w.crsp_fundno = r.crsp_fundno    
);
CREATE INDEX IF NOT EXISTS
  mret_wfcin_wfcin_idx
ON
  clean.mret_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS
  mret_wfcin_caldt_idx
ON
  clean.mret_wfcin(caldt);
CREATE INDEX IF NOT EXISTS
  mret_wfcin_crsp_fundno_idx
ON
  clean.mret_wfcin(crsp_fundno);

--------------------------------------------------------------------------------
-- Crate crsp.monthly_returns table with wfcin indeces
CREATE TABLE IF NOT EXISTS clean.dret_wfcin as (
  SELECT 
    r.*,
    w.wfcin
  FROM
    crsp.daily_returns as r
  LEFT JOIN 
    clean.crsp_wfcin as w ON
    w.crsp_fundno = r.crsp_fundno    
);
CREATE INDEX IF NOT EXISTS
  dret_wfcin_wfcin_idx
ON
  clean.dret_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS
  dret_wfcin_caldt_idx
ON
  clean.dret_wfcin(caldt);
CREATE INDEX IF NOT EXISTS
  dret_wfcin_crsp_fundno_idx
ON
  clean.dret_wfcin(crsp_fundno);

--------------------------------------------------------------------------------
-- Crate crsp.monthly_returns table with wfcin indeces
CREATE TABLE IF NOT EXISTS clean.crsp_fs_wfcin as (
  SELECT 
    s.*,
    w.wfcin
  FROM
    crsp.fund_summary as s
  LEFT JOIN 
    clean.crsp_wfcin as w ON
    w.crsp_fundno = s.crsp_fundno    
);

UPDATE clean.crsp_fs_wfcin SET exp_ratio = NULL WHERE exp_ratio = -99; 

CREATE INDEX IF NOT EXISTS
  crsp_fs_wfcin_wfcin_idx
ON
  clean.crsp_fs_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS
  crsp_fs_wfcin_caldt_idx
ON
  clean.crsp_fs_wfcin(caldt);
CREATE INDEX IF NOT EXISTS
  crsp_fs_wfcin_asset_dt_idx
ON
  clean.crsp_fs_wfcin(asset_dt);

--------------------------------------------------------------------------------
-- create daily t-bill returns
CREATE TABLE IF NOT EXISTS clean.tbill_dret as (
  select 
    caldt,
    avg(case tdretnua when -99 then NULL ELSE tdretnua END) as ret
  from cash.daily
  group by caldt
  order by caldt
);
CREATE INDEX IF NOT EXISTS
  tbill_dret_caldt_idx
ON
  clean.tbill_dret(caldt);
-------------------------------------------------------------------------------
-- link TR fund characteristics with wfcin
CREATE TABLE IF NOT EXISTS clean.tr_fc_wfcin AS (
  select
    c.*,
    w.wfcin
  from tr.fund_characteristics as c
  left join clean.deoni_wfcin as w on
    w.fundno = c.fundno AND
    w.fdate = c.fdate
);
CREATE INDEX IF NOT EXISTS
  tr_fc_wfcin_wfcin
on
  clean.tr_fc_wfcin(wfcin);
CREATE INDEX IF NOT EXISTS 
  tr_fc_wfcin_fdate
on
  clean.tr_fc_wfcin(fdate);
-------------------------------------------------------------------------------
-- stock dates
CREATE TABLE IF NOT EXISTS clean.stock_dates as (
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
);
CREATE INDEX IF NOT EXISTS stock_dates_date_idx
ON clean.stock_dates(date);
