source("02-connect-to-db.R")

sql_command <- "
SELECT DISTINCT
    cusip
FROM tr.holdings
"
cusipList <- dbGetQuery(con, sql_command)
