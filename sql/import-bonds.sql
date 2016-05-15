-------------------------------------------------------------------------------
-- Create schema
CREATE SCHEMA IF NOT EXISTS bonds;

CREATE TABLE IF NOT EXISTS bonds.monthly (
    date date,
    ind float
);

\copy bonds.monthly FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/fred-bond-index-1989-12-2016-05.csv.gz" -so' DELIMITER ','  CSV HEADER NULL '#N/A';

CREATE INDEX IF NOT EXISTS
    monthly_date_idx
ON
    bonds.monthly(date);
