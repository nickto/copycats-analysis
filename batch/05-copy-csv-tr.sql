\copy tr.fund_characteristics FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-tr-s12-type1-197901-201509.csv.gz" -so' DELIMITER ','  CSV HEADER ;

\copy tr.stock_characteristics FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-tr-s12-type2-197901-201509.csv.gz" -so' DELIMITER ','  CSV HEADER ;

\copy tr.holdings FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-tr-s12-type3-197901-201509.csv.gz" -so' DELIMITER ','  CSV HEADER ;

\copy tr.holdings_change FROM PROGRAM '7z x "D:/Cloud Storages/GitHub/copycats-analysis/data/raw/wrds-tr-s12-type4-197901-201509.csv.gz" -so' DELIMITER ','  CSV HEADER ;