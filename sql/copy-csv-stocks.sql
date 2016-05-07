\copy stocks.daily FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/clean/wrds-crsp-stocks-daily-19900101-20153112.csv.gz" -so' DELIMITER ','  CSV HEADER ;

UPDATE stocks.daily SET ret = NULL WHERE ret IN (-77, -66);
