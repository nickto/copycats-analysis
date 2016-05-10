inc <- function(x, n = 1){
  eval.parent(substitute(x <- x + n))
}

getCopycatPerformanceWholeSample <- function() {
# This function returns a data table that compares means of primitive funds and
# copycats. It also contains t-statistics and p-values.

    sql_command <- paste0("
    select
      --net_ret_act as primitive_gross,
      --gross_ret_act as primitive_net,
      --gross_ret_cop as copycat_gross,
      --net_ret_10m_cop as copycat_net_10m,
      --net_ret_50m_cop as copycat_net_50m,
      --net_ret_250m_cop as copycat_net_250m,
      --atc_ret_10m_cop as copycat_net_10m,
      --atc_ret_50m_cop as copycat_net_50m,
      --atc_ret_250m_cop as copycat_net_250m,
      gross_ret_cop - net_ret_act as gross_diff,
      atc_ret_10m_cop - net_ret_act as atc_diff_10m,
      atc_ret_50m_cop - net_ret_act as atc_diff_50m,
      atc_ret_250m_cop - net_ret_act as atc_diff_250m,
      net_ret_10m_cop - net_ret_act as net_diff_10m,
      net_ret_50m_cop - net_ret_act as net_diff_50m,
      net_ret_250m_cop - net_ret_act as net_diff_250m
    from performance.monthly
    where wfcin is not null
    -- legacy. should be removed later!
    and m_exp >= 0
    ")
    wholeSampleDiffs <- as.data.table(dbGetQuery(con, sql_command))

    diffList <- list()
    for(i in 1:dim(wholeSampleDiffs)[2]) {
        t <- t.test(wholeSampleDiffs[, i, with = FALSE], alternative = "two.sided")
        diffList[[i]] <- data.table(
            diff = names(wholeSampleDiffs)[i],
            estimate = t$estimate,
            statistic = t$statistic,
            p.value = t$p.value
        )
    }
    performanceComparison <- rbindlist(diffList)

    return(performanceComparison)
}

getCopycatPerformanceByYear <- function() {
# This function returns a data table that compares means of primitive funds and
# copycats by year. Unlike getCopycatPerformanceWholeSample it does not return
# t-statistics and p-values.

    sql_command <- paste0("
    select
      year,
      avg(net_ret_act) as primitive_gross,
      avg(gross_ret_act) as primitive_net,
      avg(gross_ret_cop) as copycat_gross,
      avg(net_ret_10m_cop) as copycat_net_10m,
      avg(net_ret_50m_cop) as copycat_net_50m,
      avg(net_ret_250m_cop) as copycat_net_250m,
      avg(atc_ret_10m_cop) as copycat_net_10m,
      avg(atc_ret_50m_cop) as copycat_net_50m,
      avg(atc_ret_250m_cop) as copycat_net_250m,
      avg(gross_ret_cop) - avg(gross_ret_act) as gross_diff,
      avg(atc_ret_10m_cop) - avg(net_ret_act) as atc_diff_10m,
      avg(atc_ret_50m_cop) - avg(net_ret_act) as atc_diff_50m,
      avg(atc_ret_250m_cop) - avg(net_ret_act) as atc_diff_250m,
      avg(net_ret_10m_cop) - avg(net_ret_act) as net_diff_10m,
      avg(net_ret_50m_cop) - avg(net_ret_act) as net_diff_50m,
      avg(net_ret_250m_cop) - avg(net_ret_act) as net_diff_250m
    from performance.monthly
    where wfcin is not null
    -- legacy. should be removed later!
    and m_exp >= 0
    group by year
    order by year
    ")

    byYear <- as.data.table(dbGetQuery(con, sql_command))

    byYear <- byYear[, list(year,
                            gross_diff,
                            atc_diff_10m,
                            atc_diff_50m,
                            atc_diff_250m,
                            net_diff_10m,
                            net_diff_50m,
                            net_diff_250m)]

    return(byYear)

}


extractDecileData <- function(size) {
# This function extracts decile data from the database. It retursn a data table
# object.

    sql_command <- paste0("
    with
      performance_previous_month_count as (
        select
          p.*,
          f.rf,
          count(month) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding) as months_before
        from performance.monthly as p
        left join factors.monthly as f on
          date_part('year', p.caldt) = date_part('year', f.dateff) AND
          date_part('month', p.caldt) = date_part('month', f.dateff)
        where wfcin is not null
        -- legacy. should be removed later!
        and m_exp >= 0
      ),
      performance_w_excess_returns as (
        select
          *,
          gross_ret_act - rf as gross_excess_ret_act,
          gross_ret_cop - rf as gross_excess_ret_cop,
          net_ret_act - rf as net_excess_ret_act,
          net_ret_", size, "m_cop - rf as net_excess_ret_cop
        from performance_previous_month_count
      ),
      performance_w_sr as (
        select
          *,
          -- this cases are needed, because when there are less than 12 months, stddev sometimes is 0 (e.g. when it's the 1sd or 2nd observation)
          -- also we calculate more SRs than needed, just to avoid writing them again when (if) needed. I guess PostgreSQL should optimize and not
          -- calculate them if we do not use them.
          case when months_before = 12 then
            (exp(sum(ln(1 + gross_excess_ret_act)) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding)  ) - 1) /
                (stddev_samp(gross_excess_ret_act) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding) )
          else
            Null
          end as last_12m_gross_sr_act,
          case when months_before = 12 then
            (exp(sum(ln(1 + gross_excess_ret_cop)) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding)  ) - 1) /
              (stddev_samp(gross_excess_ret_cop) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding) )
          else
            Null
          end as last_12m_gross_sr_cop,
          case when months_before = 12 then
            (exp(sum(ln(1 + net_excess_ret_act)) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding)  ) - 1) /
              (stddev_samp(net_excess_ret_act) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding) )
          else
            Null
          end as last_12m_net_sr_act,
          case when months_before = 12 then
            (exp(sum(ln(1 + net_excess_ret_cop)) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding)  ) - 1) /
              (stddev_samp(net_excess_ret_cop) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding) )
          else
            Null
          end as last_12m_net_sr_cop

        from performance_w_excess_returns
      ),
      last_12_months_values_w_incomplete as (
          select
            wfcin,
            caldt,
            year,
            month,
            --count(month) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding) as months_before,
            months_before,
            exp(sum(ln(1 + net_ret_act)) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding)  ) - 1  as last_12m_net_ret_act,
            exp(sum(ln(1 + (net_ret_", size, "m_cop - net_ret_act))) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding)  ) - 1  as last_12m_net_ret_diff,
            avg(m_exp) over (partition by wfcin order by year, month rows between 12 preceding and 1 preceding)  as last_12m_avg_exp_rat,
            gross_ret_act,
            net_ret_act,
            gross_ret_cop,
            atc_ret_", size, "m_cop as atc_ret_cop,
            net_ret_", size, "m_cop as net_ret_cop,
            gross_ret_cop - gross_ret_act as gross_diff,
            atc_ret_", size, "m_cop - net_ret_act as atc_ret_diff,
            net_ret_", size, "m_cop - net_ret_act as net_ret_diff,
            last_12m_gross_sr_act as gross_sr_act,
            last_12m_net_sr_act as net_sr_act,
            last_12m_net_sr_cop as net_sr_cop,
            last_12m_net_sr_cop - last_12m_net_sr_act as net_sr_diff
          from performance_w_sr
          order by year, month
        ),
          last_12_months_values as (
            select
            *
            from last_12_months_values_w_incomplete
            where months_before = 12
        ),
        monthly_deciles as (
          SELECT
            wfcin,
            caldt,
            year,
            month,
            ntile(10) over (partition by month, year order by last_12m_net_ret_act asc) as primitive_return_decile,
            ntile(10) over (partition by month, year order by last_12m_net_ret_diff asc) as net_return_decile,
            ntile(10) over (partition by month, year order by last_12m_avg_exp_rat asc) as exp_ratio_decile,
            ntile(10) over (partition by month, year order by gross_sr_act asc) as sr_decile,
            gross_ret_act,
            net_ret_act,
            gross_ret_cop,
            atc_ret_cop,
            net_ret_cop,
            gross_diff,
            atc_ret_diff,
            net_ret_diff,
            gross_sr_act,
            net_sr_act,
            net_sr_cop,
            net_sr_diff
          FROM last_12_months_values
        ),
        quarterly_decile as (
          select
            wfcin,
            caldt,
            year,
            month,
            lag(primitive_return_decile, (cast(month as int) - 1) % 3) over (partition by wfcin order by year asc, month asc) as primitive_return_decile,
            lag(net_return_decile, (cast(month as int) - 1) % 3) over (partition by wfcin order by year asc, month asc) as net_return_decile,
            lag(exp_ratio_decile, (cast(month as int) - 1) % 3) over (partition by wfcin order by year asc, month asc) as exp_ratio_decile,
            lag(sr_decile, (cast(month as int) - 1) % 3) over (partition by wfcin order by year asc, month asc) as sr_decile,
            gross_ret_act,
            net_ret_act,
            gross_ret_cop,
            atc_ret_cop,
            net_ret_cop,
            gross_diff,
            atc_ret_diff,
            net_ret_diff,
            gross_sr_act,
            net_sr_act,
            net_sr_cop,
            net_sr_diff
          from monthly_deciles
        )
        select
            wfcin,
            caldt,
            year,
            month,
            primitive_return_decile,
            net_return_decile,
            exp_ratio_decile,
            sr_decile,
            gross_ret_act,
            net_ret_act,
            gross_ret_cop,
            atc_ret_cop,
            net_ret_cop,
            gross_diff,
            atc_ret_diff,
            net_ret_diff,
            gross_sr_act,
            net_sr_act,
            net_sr_cop,
            net_sr_diff,
            f.mktrf,
            f.smb,
            f.hml,
            f.rf,
            f.umd
        from quarterly_decile as d
        left join factors.monthly as f on
          date_part('year', d.caldt) = date_part('year', f.dateff) AND
          date_part('month', d.caldt) = date_part('month', f.dateff)

    ")

    decileData <- as.data.table(dbGetQuery(con, sql_command))

    return(decileData)
}

getDecileData <- function(size) {
# This functio returns a data table with decile data. It uses cached data if
# available. Cache is not cleaned between sessions, therefore one might
# consider using clearDecileCache function from time to time.

    # 1. Try to load cached data, if already extracted
    key <- list("deciles", size)
    data <- loadCache(key)

    if (!is.null(data)) {
        cat("Loaded cached decile data\n")
        return(data);
    }

    # 2. If not available, extract it.
    cat("Extracting decile data from DB...")
    data <- extractDecileData(size)
    cat("ok\n")
    saveCache(data, key = key, comment="getDecileData()")
    return(data)
}

clearDecileCache <- function() {
# This function clear cache of getDecileData() function.

    for(size in c(10, 50, 250)) {
        tryCatch( file.remove(findCache(key=list("deciles", size))) ,
                  error = function(e) {})
    }

    print("Cached decile data removed")
}


extractFundAndFactorsData <- function(size) {
    sql_command <- paste0("
        select
          p.wfcin,
          p.year,
          p.month,
          p.gross_ret_cop,
          p.atc_ret_", size, "m_cop as atc_ret_cop,
          p.net_ret_", size, "m_cop as net_ret_cop,
          p.net_ret_act,
          p.gross_ret_act,
          p.m_exp,
          f.rf,
          f.mktrf,
          f.smb,
          f.hml,
          f.umd
        from performance.monthly as p
        left join factors.monthly as f on
          date_part('year', p.caldt) = date_part('year', f.dateff) AND
          date_part('month', p.caldt) = date_part('month', f.dateff)
        where wfcin is not null
        -- bodge
        and m_exp >= 0
    ")
    return(as.data.table(dbGetQuery(con, sql_command)))
}

getFundAndFactorsData <- function(size) {
    # 1. Try to load cached data, if already extracted
    key <- list("factors", size)
    data <- loadCache(key)

    if (!is.null(data)) {
        cat("Loaded cached factors data\n")
        return(data);
    }

    # 2. If not available, extract it.
    cat("Extracting factors data from DB...")
    data <- extractDecileData(size)
    cat("ok\n")
    saveCache(data, key = key, comment="getDecileData()")
    return(data)
}

clearFundAndFactorsCache <- function() {
# This function clear cache of getDecileData() function.

    for(size in c(10, 50, 250)) {
        tryCatch( file.remove(findCache(key=list("factors", size))) ,
                  error = function(e) {})
    }

    print("Cached decile data removed")
}


getMonthlyAlphaDeciles <- function(size) {
# This function returns monthly deciles base on Carharts alpha. (The problem is
# that rebalancing should be done only quarterly).

    data <- getFundAndFactorsData(size)
    setkey(data, wfcin, year, month)

    wfcinList <- unique(data[,wfcin])

    alphasList <- list(); iAlpha <- 0; iWfcin <- 0

    for(curWfcin in wfcinList) {
        wfcinData <- data[wfcin == curWfcin]
        setkey(wfcinData, year, month)

        if(nrow(wfcinData) > 13) {
            for(i in 13:nrow(wfcinData)) {
                curData <- wfcinData[(i-12):(i-1)]
                curYear <- wfcinData[i,year]
                curMonth <- wfcinData[i,month]

                tryCatch({
                    fm <- lm(gross_ret_act ~ rf + mktrf + smb + hml + umd,
                         data = curData)
                    se <- coeftest(fm, NeweyWest(fm))

                    alphasList[[inc(iAlpha)]] <- data.table(
                        wfcin = curWfcin,
                        year = curYear,
                        month = curMonth,
                        alpha = se[1,1])
                },
                error = function(e) {
                    alphasList[[inc(iAlpha)]] <- data.table(
                        wfcin = curWfcin,
                        year = curYear,
                        month = curMonth,
                        alpha = NA)
                })
            }
        }
        cat(paste(inc(iWfcin), curWfcin, "\n", sep = "\t"))
    }


    alphas <- rbindlist(alphasList)
    alphas[, alphas_decile := ntile(alpha, 10), by = list(year, month)]

    return(alphas)
}

getQuarterlyDeciles <- function(monthlyDeciles) {
    # This functions accepts monthly deciles as inputs, but rebalances only
    # once in a quarter: 1, 4, 7, 10

    # sort
    setkey(monthlyDeciles, wfcin, year, month)

    # loop through all
    for(i in 1:nrow(monthlyDeciles)) {
        month <- monthlyDeciles[i, month]

        # months 1, 4, 7, 10: current month values
        if(month %in% c(1, 4, 7, 10)) {
            # no need to change deciles in these months
            next
        }

        # months 2, 5, 8, 11: previous month values
        if(month %in% c(2, 5, 8, 11)) {
            # use values from previous month
            if(i > 2 && monthlyDeciles[i - 1, wfcin] == monthlyDeciles[i, wfcin]) {
                # if there are previous values to use
                monthlyDeciles[i,
                    alphas_decile := monthlyDeciles[i - 1, alphas_decile]
                ]
            } else {
                # there are no previous values to use
                monthlyDeciles[i,
                    alphas_decile := NA
                ]
            }
            next
        }

        # months 3, 6, 9, 12: previous (-2) month values
        if(month %in% c(3, 6, 9, 12)) {
            # use values from previous month
            if(i > 3 && monthlyDeciles[i - 2, wfcin] == monthlyDeciles[i, wfcin]) {
                # if there are previous values to use
                monthlyDeciles[i,
                    alphas_decile := monthlyDeciles[i - 2, alphas_decile]
                ]
            } else {
                # there are no previous values to use
                monthlyDeciles[i,
                    alphas_decile := NA
                ]
            }
            next
        }
    }

    # remove rows where there if no information from previous decile assugnment
    # month
    monthlyDeciles <- filter(monthlyDeciles, !is.na(alphas_decile))

    return(monthlyDeciles)

}

computeAlphaDeciles <- function(size) {
    # This function computes deciles based on Carhart alpha of the primitive
    # fund with quarterly changes in deciles.

    monthly <- getMonthlyAlphaDeciles(size)
    quarterly <- getQuarterlyDeciles(monthly)

    return(quarterly)
}

getAlphaDeciles <- function(size) {
    # This function returns deciles based on Carhart alpha, but it returns
    # it from cache if it already there.

    # size is hardcoded to 10, because alphas and therefore deciles are
    # the same regardless of the copycat fund size (because we compute them
    # based on primitive fund size), but we store data that is used to calculate
    # deciles by copycat fund size.

    # 1. Try to load cached data, if already extracted
    key <- list("alpha_deciles", 10)
    data <- loadCache(key)

    if (!is.null(data)) {
        cat("Loaded cached decile data\n")
        return(data);
    }

    # 2. If not available, extract it.
    cat("Calculating...", "\n")
    data <- computeAlphaDeciles(10)
    cat("ok\n")
    saveCache(data, key = key, comment="computeAlphaDeciles()")
    return(data)
}

clearAlphaDecilesCache <- function() {
# This function clear cache of getDecileData() function.

    for(size in c(10, 50, 250)) {
        tryCatch( file.remove(findCache(key=list("alpha_deciles", size))) ,
                  error = function(e) {})
    }

    print("Cached alpha deciles data removed")
}



getDecileMeans <- function() {
# This function return mean comparisons (with t-statistic and p-values) for
# different decile sorting and different fund sizes. Note that decile 11
# means decile 1 minus decile 10.
#
# The returned object is a list of lists... where the first level indicates
# the fund size, the second level indicates which variable the deciles where
# sorted, the third level contains difference statistics (means, t-stat etc),
# each of this statistics is represented using a data table.

    decileNames <- c("primitive_return_decile",
                     "net_return_decile",
                     "exp_ratio_decile",
                     "sr_decile",
                     "alphas_decile")
    dependentVariables <- c("gross_ret_act",
                            "net_ret_act",
                            "gross_ret_cop",
                            "atc_ret_cop",
                            "net_ret_cop",
                            "gross_diff",
                            "atc_ret_diff",
                            "net_ret_diff",
                            "gross_sr_act",
                            "net_sr_act",
                            "net_sr_cop",
                            "net_sr_diff")

    means <- list()
    for(size in c(10,50,250)) {
        #size <- 50
        decileData <- getDecileData(size)
        alphaDeciles <- getAlphaDeciles(size)

        decileData <- merge(
            decileData,
            alphaDeciles[, list(wfcin, year, month, alphas_decile)],
            by = c("wfcin", "year", "month")
        )

        means[[as.character(size)]] <- list()
        for(decileName in decileNames) {
            # decileName <- "net_return_decile"

            means[[as.character(size)]][[decileName]] <- list()

            means[[as.character(size)]][[decileName]][["mean"]]  <- data.table(decile = 1:11)
            means[[as.character(size)]][[decileName]][["tstat"]]  <- data.table(decile = 1:11)
            means[[as.character(size)]][[decileName]][["pvalue"]] <- data.table(decile = 1:11)


            for(dependentVariable in dependentVariables) {
                # dependentVariable <- "net_ret_diff"

                # means for deciles 1 to 10
                data <- list()
                for(decile in 1:10) {
                    # decile <- 2

                    # get data for the current decile (averages)
                    data[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(dependentVariable), na.rm =TRUE)
                        ), by = list(year, month)]
                    setkey(data[[decile]], year, month)

                    tTest <- t.test(data[[decile]][, monthMean])

                    means[[as.character(size)]][[decileName]][["mean"]][decile, (dependentVariable) := tTest$estimate]
                    means[[as.character(size)]][[decileName]][["tstat"]][decile, (dependentVariable) := tTest$statistic]
                    means[[as.character(size)]][[decileName]][["pvalue"]][decile, (dependentVariable) := tTest$p.value]
                }

                # means fore decile 1 minus decile 10
                dataD1MinusD10 <- data[[1]]
                dataD1MinusD10[, monthMean := data[[1]][, monthMean] - data[[10]][, monthMean]]

                tTest <- t.test(dataD1MinusD10[, monthMean])

                means[[as.character(size)]][[decileName]][["mean"]][11, (dependentVariable) := tTest$estimate]
                means[[as.character(size)]][[decileName]][["tstat"]][11, (dependentVariable) := tTest$statistic]
                means[[as.character(size)]][[decileName]][["pvalue"]][11, (dependentVariable) := tTest$p.value]
            }
        }
    }

    return(means)
}

getDecileAlphas <- function() {
# This function return carhart alphas (with t-statistic and p-values) for
# different decile sorting and different fund sizes. Note that decile 11
# means decile 1 minus decile 10.
#
# The returned object is a list of lists of lists, where the first level indicates
# the fund size, the second level indicates which variable the deciles where
# sorted, the third level contains difference statistics (alphas, t-stat etc),
# each of this statistics is represented using a data table.

    decileNames <- c("primitive_return_decile",
                     "net_return_decile",
                     "exp_ratio_decile",
                     "sr_decile",
                     "alphas_decile")
    dependentVariables <- c("gross_ret_act",
                            "net_ret_act",
                            "gross_ret_cop",
                            "atc_ret_cop",
                            "net_ret_cop",
                            "gross_diff",
                            "atc_ret_diff",
                            "net_ret_diff",
                            "gross_sr_act",
                            "net_sr_act",
                            "net_sr_cop",
                            "net_sr_diff")

    alphas <- list()
    for(size in c(10,50,250)) {
        #size <- 50
        decileData <- getDecileData(size)
        alphaDeciles <- getAlphaDeciles(size)

        decileData <- merge(
            decileData,
            alphaDeciles[, list(wfcin, year, month, alphas_decile)],
            by = c("wfcin", "year", "month")
        )

        alphas[[as.character(size)]] <- list()
        for(decileName in decileNames) {
            # decileName <- "net_return_decile"

            alphas[[as.character(size)]][[decileName]] <- list()

            alphas[[as.character(size)]][[decileName]][["alpha"]]  <- data.table(decile = 1:11)
            alphas[[as.character(size)]][[decileName]][["tstat"]]  <- data.table(decile = 1:11)
            alphas[[as.character(size)]][[decileName]][["pvalue"]] <- data.table(decile = 1:11)


            for(dependentVariable in dependentVariables) {
                # dependentVariable <- "net_ret_diff"

                # alphas for deciles 1 to 10
                data <- list()
                for(decile in 1:10) {
                    # decile <- 2

                    # get data for the current decile (averages)
                    data[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(dependentVariable), na.rm =TRUE),
                            mktrf = mean(mktrf),
                            smb = mean(smb),
                            hml = mean(hml),
                            rf = mean(rf),
                            umd = mean(umd)
                        ), by = list(year, month)]
                    setkey(data[[decile]], year, month)

                    fm <- lm(monthMean ~ rf + mktrf + smb + hml + umd, data = data[[decile]])
                    se <- coeftest(fm, NeweyWest(fm, lag = 2))

                    alphas[[as.character(size)]][[decileName]][["alpha"]][decile, (dependentVariable) := se[1,1]]
                    alphas[[as.character(size)]][[decileName]][["tstat"]][decile, (dependentVariable) := se[1,3]]
                    alphas[[as.character(size)]][[decileName]][["pvalue"]][decile, (dependentVariable) := se[1,4]]
                }

                # alphas fore decile 1 minus decile 10
                dataD1MinusD10 <- data[[1]]
                dataD1MinusD10[, monthMean := data[[1]][, monthMean] - data[[10]][, monthMean]]

                fm <- lm(monthMean ~ rf + mktrf + smb + hml + umd, data = dataD1MinusD10)
                se <- coeftest(fm, NeweyWest(fm, lag = 2))

                alphas[[as.character(size)]][[decileName]][["alpha"]][11, (dependentVariable) := se[1,1]]
                alphas[[as.character(size)]][[decileName]][["tstat"]][11, (dependentVariable) := se[1,3]]
                alphas[[as.character(size)]][[decileName]][["pvalue"]][11, (dependentVariable) := se[1,4]]

            }
        }
    }

    return(alphas)

}



createTable <- function(decileMeans, decileAlphas) {

    # --------------------------------------------------------------------------
    # Define local functions
    # Function for creating a panle
    createPanel <- function(decileBy, decileData, measure) {
        # This function accepts the variable name by which deciles are created,
        # a list with decile statistics (means or alphas) and which statistics
        # where calculated ("mean" or "alpha")
        #
        # It returns a list of three elements: measure, tstat, pvalue, where
        # each element corresponds to the layout of the table in latex.

        getPanel <- function(measure) {
            panel <- data.table(
                decile        = decileData$`10`[[decileBy]][[measure]]$decile,
                grossRetPrim  = decileData$`10`[[decileBy]][[measure]]$gross_ret_act,
                grossRetCopy  = decileData$`10`[[decileBy]][[measure]]$gross_ret_cop,
                grossRetDiff  = decileData$`10`[[decileBy]][[measure]]$gross_diff,
                netRetPrim    = decileData$`10`[[decileBy]][[measure]]$net_ret_act,
                netRetCopy10  = decileData$`10`[[decileBy]][[measure]]$net_ret_cop,
                netRetCopy50  = decileData$`50`[[decileBy]][[measure]]$net_ret_cop,
                netRetCopy250 = decileData$`250`[[decileBy]][[measure]]$net_ret_cop,
                netRetDiff10  = decileData$`10`[[decileBy]][[measure]]$net_ret_diff,
                netRetDiff50  = decileData$`50`[[decileBy]][[measure]]$net_ret_diff,
                netRetDiff250 = decileData$`250`[[decileBy]][[measure]]$net_ret_diff
            )
            return(panel)
        }

        panel <- list()
        panel$measure <- getPanel(measure)
        panel$tstat   <- getPanel("tstat")
        panel$pvalue  <- getPanel("pvalue")

        return(panel)
    }

    getFormattedNumber <- function(stat,
                                   pvalue = NULL,
                                   percent = FALSE,
                                   tstat = FALSE,
                                   roundN = 2) {
        # this function accepts statistics, it's pvalue (if it is not t-stat)
        # and returns formatted version of it. tstat indicates whether this is a
        # t-statitcs (formatted differently). If percent = TRUE, the number is
        # multiplied by 100 before conversion.

        if(!tstat) {
            # convert decimal to percentage if nccessary
            if(percent) {
                stat <- stat * 100
            }

            statStr <- format(round(stat, roundN), nsmall = roundN)
            statStr <- paste0("\\num{", statStr ,"}")
            # add significance asterisks
            if(pvalue <= 0.1) {
                statStr <- paste0(statStr, "*\\phantom{**)}")
            } else if(pvalue <= 0.5) {
                statStr <- paste0(statStr, "**\\phantom{*)}")
            } else if(pvalue <= 0.01) {
                statStr <- paste0(statStr, "***\\phantom{)}")
            } else {
                statStr <- paste0(statStr, "\\phantom{***)}")
            }
        } else {
            statStr <- format(round(stat, roundN), nsmall = roundN)
            statStr <- paste0("(\\num{", statStr, "})\\phantom{***}")
        }

        return(statStr)
    }

    writePanel <- function(outputFileName, decileData, panelCaption, measure) {
        # Add panel name to a file
        panelNameString <- paste0(
            "\t\t\\midrule\n\t\t\\mc{11}{l}{\\scriptsize{\\textit{\\quad ",
            panelCaption,
            "}}} \\\\[\\panelspacing]",
            sep = ""
        )
        write(panelNameString, file = outputFileName, append = TRUE)


        # get panel
        cat(decileBy, measure, "\n")
        panel <- createPanel(decileBy, decileData, measure)

        for(row in 1:nrow(panel$measure)) {
            for(col in 1:ncol(panel$measure)) {
                # check if this is the first column (it is special for both
                # statistics and t-statistics)
                if(col == 1) {
                    # statistics: decile name
                    if(panel$measure[row, col] == 11) {
                        # This is a D1-D10 decile
                        statRow <- paste("\t\t\\decilename{D1$-$D10}")
                    } else {
                        # This is a usual decile
                        statRow <- paste0(
                            "\t\t\\decilename{D",
                            panel$measure[row, col, with = FALSE],
                            "}"
                        )
                    }
                    # t-statistics: empty column
                    tStatRow <- "\t\t"
                } else {
                    # statistics row
                    statRow <- paste0(
                        statRow,
                        " & ",
                        getFormattedNumber(
                            panel$measure[row, col, with = FALSE],
                            panel$pvalue[row, col, with = FALSE],
                            percent = TRUE,
                            tstat = FALSE
                        )
                    )

                    # t-statistics row
                    tStatRow <- paste0(
                        tStatRow,
                        " & ",
                        getFormattedNumber(
                            panel$tstat[row, col, with = FALSE],
                            percent = TRUE,
                            tstat = TRUE
                        )
                    )
                }
            }

            # write statistics row
            statRow <- paste(statRow, "\\\\")
            write(statRow, file = outputFileName, append = TRUE)
            # write t-statistics row
            tStatRow <- paste(tStatRow, "\\\\ [\\dspacing]")
            write(tStatRow, file = outputFileName, append = TRUE)
        }
    }
    # --------------------------------------------------------------------------


    decileNames <- c("primitive_return_decile",
                     "net_return_decile",
                     "exp_ratio_decile",
                     "sr_decile",
                     "alphas_decile")

    for(decileBy in decileNames) {
        # setup ouput file name
        outputFileName <- paste0("latex/tables_deciles_by_", decileBy,".tex")

        # add text that goes before panels
        fileName1 <- "latex/part-01-before-panels.tex"
        file.copy(fileName1, outputFileName, overwrite = TRUE)


        # add means panel
        panelCaption <- "Panel A: simple averages"
        writePanel(outputFileName, decileMeans, panelCaption, "mean")

        # add alphas panel
        panelCaption <- "Panel B: Carhart alphas"
        writePanel(outputFileName, decileAlphas, panelCaption, "alpha")


        # add text that goes after panels
        fileName2 <- "latex/part-02-after-panels.tex"
        file.append(outputFileName, fileName2)
    }
}
