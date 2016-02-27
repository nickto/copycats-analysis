source("./00-load-packages.R")

# Commong settings
dataDir <- "data"
setDefaults('as.Date.character', format = '%Y/%m/%d')


# Thomson Reuters Mutual Funds Type 1
fileName <- "wrds-tr-s12-type1-197901-201509.csv.gz"
filePath <- paste(dataDir, fileName, sep = "/")

colClassVector <- c("Date", "integer", "factor", "factor", "Date", "integer",
                    "integer", "Date", "factor")
start.time <- Sys.time()
data <- read.csv.ffdf(file = filePath,
                      header=TRUE,
                      VERBOSE=TRUE,
                      colClasses = colClassVector,
                      na.strings="")
end.time <- Sys.time()
end.time - start.time







# data[data$crsp_fundno == 1, ]

# head(data$crsp_fundno == 1)

# max <- 6130202
# subset(data[,], data$crsp_fundno[] == 86585)


data2 <- read.csv("data/test.csv", colClasses = c("Date", "integer", "character",
                                 "character","Date", "integer", "integer",
                                 "Date", "character"))