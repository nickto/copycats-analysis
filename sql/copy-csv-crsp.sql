--\copy crsp.contact_info FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-crsp-mfdb-contact-infromation.csv.gz" -so' DELIMITER ','  CSV HEADER ;

--\copy crsp.daily_returns FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/clean/wrds-crsp-mfdb-daily-returns-19980901-20151231.csv.gz" -so' DELIMITER ',' CSV HEADER ;

\copy crsp.front_load FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-crsp-mfdb-front-loads.csv.gz" -so' DELIMITER ',' CSV HEADER ;
