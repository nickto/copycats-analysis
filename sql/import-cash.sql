-------------------------------------------------------------------------------
-- Create schema
CREATE SCHEMA IF NOT EXISTS cash;

CREATE TABLE IF NOT EXISTS cash.daily (
    kytreasno bigint,
    kyccrspid float,
    tcusip varchar(10),
    caldt date,
    tdretnua float
);

SET datestyle = 'ISO,MDY';
\copy cash.daily FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-crsp-t-bills-daily-19900101-20153112.csv.gz" -so' DELIMITER ','  CSV HEADER ;
SET datestyle = 'ISO,DMY';

CREATE INDEX IF NOT EXISTS
    daily_caldt_idx
ON
    cash.daily(caldt);
