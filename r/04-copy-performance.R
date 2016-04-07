source("./r/02-connect-to-db.R")


sql_command <- "
SELECT
    fund_characteristics.fdate,
    fund_characteristics.fundno,
    link1.wfcin,
    link2.wfcin
FROM tr.fund_characteristics
LEFT JOIN mflinks.link1 ON
    fund_characteristics.fdate = link1.fdate AND
    fund_characteristics.fundno = link1.fundno
LEFT JOIN mflinks.link2 ON
    link1.wfcin = link2.wfcin
OFFSET 54621
LIMIT 1000
"
dbGetQuery(con, sql_command)


sql_command <- "
SELECT
    fund_summary.crsp_fundno,
    wfcin
FROM crsp.fund_summary
LEFT JOIN mflinks.link2 ON
    link2.crsp_fundno = fund_summary.crsp_fundno
LIMIT 100
OFFSET 9854
"
dbGetQuery(con, sql_command)




sql_command <- "
WITH
crsp_data AS (
    SELECT
        fund_summary.crsp_fundno,
        wfcin
    FROM crsp.fund_summary
    LEFT JOIN mflinks.link2 ON
        link2.crsp_fundno = fund_summary.crsp_fundno
),
tr_data AS (
    SELECT
        fund_characteristics.fdate,
        fund_characteristics.fundno,
        link1.wfcin
    FROM tr.fund_characteristics
    LEFT JOIN mflinks.link1 ON
        fund_characteristics.fdate = link1.fdate AND
        fund_characteristics.fundno = link1.fundno
)
SELECT
    *
FROM crsp_data
INNER JOIN tr_data ON
    crsp_data.wfcin = tr_data.wfcin
OFFSET 5456
LIMIT 100
;
"
dbGetQuery(con, sql_command)
