-------------------------------------------------------------------------------
-- Create schema
CREATE SCHEMA IF NOT EXISTS factors;

CREATE TABLE IF NOT EXISTS factors.monthly (
    dateff date,
    mktrf numeric(10, 8),
    smb numeric(10, 8),
    hml numeric(10, 8),
    rf numeric(10, 8),
    umd numeric(10,8)
);

\copy factors.monthly FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-ff-monthly-199001-201601.csv.gz" -so' DELIMITER ','  CSV HEADER ;

CREATE INDEX IF NOT EXISTS
    monthly_dateff_idx
ON
    factors.monthly(dateff);
