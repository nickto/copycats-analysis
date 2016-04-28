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
      net_ret_250m_cop - gross_ret_act as net_diff_250m
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
            lag(primitive_return_decile, (month - 1) % 3) over (partition by wfcin order by year asc, month asc) as primitive_return_decile,
            lag(net_return_decile, (month - 1) % 3) over (partition by wfcin order by year asc, month asc) as net_return_decile,
            lag(exp_ratio_decile, (month - 1) % 3) over (partition by wfcin order by year asc, month asc) as exp_ratio_decile,
            lag(sr_decile, (month - 1) % 3) over (partition by wfcin order by year asc, month asc) as sr_decile,
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
    key <- list(size)
    data <- loadCache(key)

    if (!is.null(data)) {
        cat("Loaded cached decile data\n")
        return(data);
    }

    # 2. If not available, extract it.
    cat("Extracting decile data from DB...")
    data <- extractDecileData(size)
    cat("ok\n")
    saveCache(data, key=key, comment="getDecileData()")
    return(data)
}

clearDecileCache <- function() {
# This function clear cache of getDecileData() function.

    for(size in c(10, 50, 250)) {
        tryCatch( file.remove(findCache(key=list(size))) ,
                  error = function(e) {})
    }

    print("Cached decile data removed")
}

getDecileAnalyis <- function() {
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
                     "sr_decile")
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

                tTest <- t.test(data[[decile]][, monthMean])

                means[[as.character(size)]][[decileName]][["mean"]][11, (dependentVariable) := tTest$estimate]
                means[[as.character(size)]][[decileName]][["tstat"]][11, (dependentVariable) := tTest$statistic]
                means[[as.character(size)]][[decileName]][["pvalue"]][11, (dependentVariable) := tTest$p.value]
            }
        }
    }

    return(means)

}

getAlphas <- function() {
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
                     "sr_decile")
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
                    se <- coeftest(fm, NeweyWest(fm, lag=2))

                    alphas[[as.character(size)]][[decileName]][["alpha"]][decile, (dependentVariable) := se[1,1]]
                    alphas[[as.character(size)]][[decileName]][["tstat"]][decile, (dependentVariable) := se[1,3]]
                    alphas[[as.character(size)]][[decileName]][["pvalue"]][decile, (dependentVariable) := se[1,4]]
                }

                # alphas fore decile 1 minus decile 10
                dataD1MinusD10 <- data[[1]]
                dataD1MinusD10[, monthMean := data[[1]][, monthMean] - data[[10]][, monthMean]]

                fm <- lm(monthMean ~ rf + mktrf + smb + hml + umd, data = dataD1MinusD10)
                se <- coeftest(fm, NeweyWest(fm, lag=2))

                alphas[[as.character(size)]][[decileName]][["alpha"]][11, (dependentVariable) := se[1,1]]
                alphas[[as.character(size)]][[decileName]][["tstat"]][11, (dependentVariable) := se[1,3]]
                alphas[[as.character(size)]][[decileName]][["pvalue"]][11, (dependentVariable) := se[1,4]]

            }
        }
    }

    return(alphas)

}