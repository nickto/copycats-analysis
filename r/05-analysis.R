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


# Get analysis by deciles
# Means
decileMeans <- getDecileMeans()
# Carhart alphas
decileAlphas <- getDecileAlphas()


# write analysis resuts to file
save.image(file = "analysis-results.RData")



# # clear cache
# clearDecileCache()
# clearFundAndFactorsCache()
# clearAlphaDecilesCache()
#
#
#
#
#
#
# # create tables
# createTable(value, tstat, pvalue)
#
# value <- decileMeans$`10`$primitive_return_decile$mean
# tstat <- decileMeans$`10`$primitive_return_decile$tstat
# pvalue <- decileMeans$`10`$primitive_return_decile$pvalue











#dbDisconnect(con); rm(list = ls())




