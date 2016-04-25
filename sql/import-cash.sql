-------------------------------------------------------------------------------
-- Create schema
CREATE SCHEMA IF NOT EXISTS cash;

CREATE TABLE IF NOT EXISTS cash.tbills (
    kytreasno bigint,
    kyccrspid float,
    tcusip varchar(10),
    caldt date,
    tdretnua float
);

SET datestyle = 'ISO,MDY';
\copy cash.tbills FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-crsp-t-bills-daily-19900101-20153112.csv.gz" -so' DELIMITER ','  CSV HEADER ;
SET datestyle = 'ISO,DMY';

CREATE TABLE cash.daily AS (
  select
    caldt,
    avg(tdretnua) as ret
  from cash.tbills
  where tdretnua > -1 -- missing values are indicated by -99
  group by caldt
  order by caldt
);

CREATE INDEX IF NOT EXISTS
    daily_caldt_idx
ON
    cash.daily(caldt);
