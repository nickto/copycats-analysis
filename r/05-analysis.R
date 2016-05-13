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


# write analysis resuts to file not to lose it
save.image(file = "analysis-results.RData")



# # clear cache
# clearDecileCache()
# clearFundAndFactorsCache()
# clearAlphaDecilesCache()




# create parts of latex tables
createTable(decileMeans, decileAlphas)






# disconnect from database and remove all vairbales. This is needed for testing
# purposes to make a clean run.
# dbDisconnect(con); rm(list = ls())


ss <- getFundLevelStatistics()
createPlots(ss)






source("./r/05-analysis-functions.R")
tmp <- getMonthlyAverages()




