source("./00-load-packages.R")
source("./code-definitions.R")

# Commong settings
dataDir <- "data"
setDefaults('as.Date.character', format = '%Y/%m/%d')
nRows <- 0

# Thomson Reuters Mutual Funds Type 1
fileName <- "wrds-tr-s12-type1-197901-201509.csv.gz"
filePath <- paste(dataDir, fileName, sep = "/")

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

# Assign code definitions to numeric variables to create factors
levels(data$ioc) <- plyr::revalue(levels(data$ioc),
                                  codeDefinitions$ioc)

# Rename data variable
trS12Type1 <- data


# Thomson Reuters Mutual Funds Type 2
fileName <- "wrds-tr-s12-type2-197901-201509.csv.gz"
filePath <- paste(dataDir, fileName, sep = "/")

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

# Assign code definitions to numeric variables to create factors
levels(data$exchcd) <- plyr::revalue(levels(data$exchcd),
                                     codeDefinitions$exchcd)
levels(data$stkcd) <- plyr::revalue(levels(data$stkcd),
                                    codeDefinitions$stkcd)
levels(data$stkcdesc) <- plyr::revalue(levels(data$stkcdesc),
                                       codeDefinitions$stkcdesc)
levels(data$indcode) <- plyr::revalue(levels(data$indcode),
                                      codeDefinitions$indcode)

# Rename data variable
trS12Type2 <- data







# con <- gzfile(filePath, open = "r")
# readLines(con, n = 10)

# data[data$crsp_fundno == 1, ]

# head(data$crsp_fundno == 1)

# max <- 6130202
# subset(data[,], data$crsp_fundno[] == 86585)

