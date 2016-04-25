-------------------------------------------------------------------------------
-- Create schema
CREATE SCHEMA IF NOT EXISTS global;

CREATE TABLE IF NOT EXISTS global.index (
    date date,
    index numeric(10, 3)
);

\copy global.index FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/bloomberg-sp1200-daily-19900101-20160419.csv.gz" -so' DELIMITER ','  CSV HEADER ;

CREATE TABLE global.daily AS (
  SELECT 
    date,
    index,
    index / lag(index) OVER (ORDER BY date) - 1 as ret,
    lag(date) OVER (ORDER BY date) lag_date,
    lag(index) OVER (ORDER BY date) lead_date
  FROM global.index
);


CREATE INDEX IF NOT EXISTS
    daily_date_idx
ON
    global.daily(date);
