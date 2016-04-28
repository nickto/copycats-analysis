# to-do:
# - carhart regressions
# - carhart alpha sorting (?)

source("./r/02-connect-to-db.R")
source("./r/05-analysis-functions.R")

# Average copycat performance comparison
# Whole sample performance comparison with t-test
performanceComparison <- getCopycatPerformanceWholeSample()
# Performance comparison by years (without t-test)
performanceComparisonByYear <- getCopycatPerformanceByYear()


# Decile analysis
decileAnalysis <- getCompleteDecileAnalyis()

d <- getCompleteDecileAnalyis()
dTmp <- getDecileAnalyisTmp()


# Carhart regressions
alphas <- getAlphas()

















clearDecileCache()







#dbDisconnect(con); rm(list = ls())




