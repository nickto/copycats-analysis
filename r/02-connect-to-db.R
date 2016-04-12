source("./r/00-load-packages.R")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
    "default-password-for-copycats"
}

# # check for the cartable
# dbExistsTable(con, "test")

# connect to Postgre
drv <- dbDriver("PostgreSQL")
if (!exists("con")) {
    con <- dbConnect(drv, dbname = "copycats",
                     host = "localhost", port = 5432,
                     user = "copycat", password = pw)
} else {
    warning("Connection with this name exists, did not create a new one.")
}

# # template
# sql_command <- "
# "
# dbGetQuery(con, sql_command)


