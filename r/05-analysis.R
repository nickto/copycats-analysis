source("./r/01-load-packages.R")
source("./r/02-connect-to-db.R")
source("./r/05-analysis-functions.R")

# Average copycat performance comparison
# Whole sample performance comparison with t-test
performanceComparison <- getCopycatPerformanceWholeSample()
# Performance comparison by years (without t-test)
performanceComparisonByYear <- getCopycatPerformanceByYear()


# Get analysis by deciles
# Means
decileMeans <- getDecileMeans()
# Carhart alphas
decileAlphas <- getDecileAlphas()

# means by funds
ssf <- getFundLevelStatistics()
# means by months
ssm <- getMonthLevelStatistics()

# write analysis resuts to file not to lose it
save.image(file = "analysis-results.RData")

# # clear cache
# clearDecileCache()
# clearFundAndFactorsCache()
# clearAlphaDecilesCache()

# create plots by funds
createPlotsAcrossFundsFirst(ssf, jitter = TRUE)
# create plots by monhts
createPlotsAcrossMonthsFirst(ssm)

# create table by funds
dff <- getStatsAcrossFundsFirst(ssf)
createLatexTablesOfWholeSampleResults(dff)

# create table by months
dfm <- getStatsAcrossMonthsFirst(ssm)
createLatexTablesOfWholeSampleResults(dfm)

# create plot of deciles by size
plotSizeDecileAnalysis(ssf)

# create decile tables
createTables(decileMeans, decileAlphas)

# create sample summary statistics
sss <- getSampleSummaryStatistics()

# create performance comparison by year
getYearlyPerformance()




# disconnect from database and remove all vairbales. This is needed for testing
# purposes to make a clean run.
# dbDisconnect(con); rm(list = ls())

