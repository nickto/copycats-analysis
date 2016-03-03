# store the start time
ptm <- proc.time()

source("./01-code-definitions.R")

# Commong settings
dataDir <- "data"
rawDataDir <- paste(dataDir, "raw", sep = "/")
ffdfDataDir <- paste(dataDir, "ffdf", sep = "/")
if (!dir.exists(ffdfDataDir)) dir.create(ffdfDataDir)
# setDefaults('as.Date.character', format = '%Y/%m/%d')
nRows <- -1 # negative values are ignored, thus, whole file is read

# Thomson Reuters Mutual Funds Type 1
fileName <- "wrds-tr-s12-type1-197901-201509.csv.gz"
filePath <- paste(rawDataDir, fileName, sep = "/")

# Specify column types
colClassVector <- c("Date",
                    "integer",
                    "factor",
                    "factor",
                    "Date",
                    "integer",
                    "factor",
                    "Date",
                    "factor")

# Read teh data from csv
data <- read.csv.ffdf(file = filePath,
                      header=TRUE,
                      VERBOSE = TRUE,
                      colClasses = colClassVector,
                      na.strings = "",
                      nrow = nRows)

# Assign code definitions
levels(data$ioc) <- plyr::revalue(levels(data$ioc),
                                  codeDefinitions$ioc)

# Rename data variable
trFundCharac <- data

# save ffdf
save.ffdf(trFundCharac,
          dir = paste(ffdfDataDir, "trFundCharac", sep = "/"),
          overwrite = TRUE)

# remove ffdf from memory
rm(trFundCharac)


# Thomson Reuters Mutual Funds Type 2
fileName <- "wrds-tr-s12-type2-197901-201509.csv.gz"
filePath <- paste(rawDataDir, fileName, sep = "/")

# Specify column types
colClassVector <- c("Date",
                    "factor",
                    "factor",
                    "factor",
                    "factor",
                    "factor",
                    "factor",
                    "factor",
                    "integer",
                    "numeric",
                    "integer",
                    "factor")

# Read teh data from csv
data <- read.csv.ffdf(file = filePath,
                      header=TRUE,
                      VERBOSE = TRUE,
                      colClasses = colClassVector,
                      na.strings = "",
                      nrow = nRows)

# Assign code definitions
levels(data$exchcd) <- plyr::revalue(levels(data$exchcd),
                                     codeDefinitions$exchcd)
levels(data$stkcd) <- plyr::revalue(levels(data$stkcd),
                                    codeDefinitions$stkcd)
levels(data$stkcdesc) <- plyr::revalue(levels(data$stkcdesc),
                                       codeDefinitions$stkcdesc)
levels(data$indcode) <- plyr::revalue(levels(data$indcode),
                                      codeDefinitions$indcode)

# Rename data variable
trStockCharac <- data

# save ffdf
save.ffdf(trStockCharac,
          dir = paste(ffdfDataDir, "trStockCharac", sep = "/"),
          overwrite = TRUE)

# remove ffdf from memory
rm(trStockCharac)


# Thomson Reuters Mutual Funds Type 3
fileName <- "wrds-tr-s12-type3-197901-201509.csv.gz"
filePath <- paste(rawDataDir, fileName, sep = "/")

# Specify column types
colClassVector <- c("Date",
                    "factor",
                    "integer",
                    "double")

# Read teh data from csv
data <- read.csv.ffdf(file = filePath,
                      header=TRUE,
                      VERBOSE = TRUE,
                      colClasses = colClassVector,
                      na.strings = "",
                      nrow = nRows)

# Assign code definitions

# Rename data variable
trHoldings <- data

# save ffdf
save.ffdf(trHoldings,
          dir = paste(ffdfDataDir, "trHoldings", sep = "/"),
          overwrite = TRUE)

# remove ffdf from memory
rm(trHoldings)

# Thomson Reuters Mutual Funds Type 4
fileName <- "wrds-tr-s12-type4-197901-201509.csv.gz"
filePath <- paste(rawDataDir, fileName, sep = "/")

# Specify column types
colClassVector <- c("Date",
                    "factor",
                    "integer",
                    "double")

# Read teh data from csv
data <- read.csv.ffdf(file = filePath,
                      header=TRUE,
                      VERBOSE = TRUE,
                      colClasses = colClassVector,
                      na.strings = "",
                      nrow = nRows)

# Assign code definitions

# Rename data variable
trHoldingsChg <- data

# save ffdf
save.ffdf(trHoldingsChg,
          dir = paste(ffdfDataDir, "trHoldingsChg", sep = "/"),
          overwrite = TRUE)

# remove ffdf from memory
rm(trHoldingsChg)


# count ellapsed time
proc.time() - ptm


# con <- gzfile(filePath, open = "r")
# readLines(con, n = 10)

# data[data$crsp_fundno == 1, ]

# head(data$crsp_fundno == 1)

# max <- 6130202
# subset(data[,], data$crsp_fundno[] == 86585)

