source("02-connect-to-db.R")

sql_command <- "
SELECT DISTINCT
    cusip
FROM tr.holdings
"
cusipList <- dbGetQuery(con, sql_command)
write.table(cusipList, file = "cusip-list.txt",
            quote = FALSE, sep = " ",
            row.names = FALSE,
            col.names = FALSE)
