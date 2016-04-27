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


# Carhart regressions
decileNames <- c("primitive_return_decile",
                 "net_return_decile",
                 "exp_ratio_decile",
                 "sr_decile")

for(size in c(10,50,250)) {
    decileData <- getDecileData(size)

    for(decileName in decileNames) {
        # decileName <- "net_return_decile"


        for(decile in 1:10) {
            # decile <- 2

            # get data for the current decile (averages)
            dependentVariable <- "net_ret_diff"

            data <-
                decileData[get(decileName) == decile, list(
                        monthMean = mean(get(dependentVariable), na.rm =TRUE),
                        mktrf = mean(mktrf),
                        smb = mean(smb),
                        hml = mean(hml),
                        rf = mean(rf),
                        umd = mean(umd)
                    ), by = list(year, month)]
            setkey(data, year, month)
        }
    }



}



clearDecileCache()







#dbDisconnect(con); rm(list = ls())




