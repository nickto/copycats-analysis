source("./r/01-load-packages.R")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
    "default-password-for-copycats"
}

# # check for the cartable
# dbExistsTable(con, "test")

# connect to Postgre
drv <- dbDriver("PostgreSQL")
tryCatch(dbDisconnect(con))

con <- dbConnect(drv, dbname = "copycats",
                 host = "localhost", port = 5432,
                 user = "copycat", password = pw)

# # template
# sql_command <- "
# "
# dbGetQuery(con, sql_command)


