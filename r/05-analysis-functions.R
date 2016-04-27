getCopycatPerformanceWholeSample <- function() {
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
          -- this should be removed! it is the legacy of the version where we had this problem
          AND m_exp > -8.25
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
    for(size in c(10, 50, 250)) {
        tryCatch( file.remove(findCache(key=list(size))) ,
                  error = function(e) {})
    }

    print("Cached decile data removed")
}

getDecileAnalyis <- function(size, decileName) {
    # This function returns a list with the following elements: estimate,
    # statistics and p.value. Each element is a data table with deciles in the
    # rows and performance measure in the columns. Decile 0 means is a whole
    # sample result (deicle 1--10).
    #
    # - size is either 10, 50 or 250
    # - decileName is eitehr primitive_return_decile, net_return_decile,
    #   exp_ratio_decile, or sr_decile

    #decileName <- "primitive_return_decile"
    #size <- 250

    # get data from the SQL database
    decileData <- getDecileData(size)

    # set new column names
    newNames <- c("gross_ret_act",
                  "net_ret_act",
                  "gross_ret_cop",
                  "atc_ret_cop",
                  "net_ret_cop",
                  "net_sr_act",
                  "net_sr_cop",
                  "gross_diff",
                  "atc_ret_diff",
                  "net_ret_diff",
                  "net_sr_diff")

    decileStatistics <- list()
    for(parameter in c("estimate", "statistic", "p.value")) {
        decileStatistics[[parameter]] <- list()
        decileStatistics[[parameter]][[1]] <- decileData[!is.na(get(decileName)), list(
            t.test(gross_ret_act, alternative = "two.sided")[[parameter]],
            t.test(net_ret_act, alternative = "two.sided")[[parameter]],
            t.test(gross_ret_cop, alternative = "two.sided")[[parameter]],
            t.test(atc_ret_cop, alternative = "two.sided")[[parameter]],
            t.test(net_ret_cop, alternative = "two.sided")[[parameter]],
            t.test(net_sr_act, alternative = "two.sided")[[parameter]],
            t.test(net_sr_cop, alternative = "two.sided")[[parameter]],
            t.test(gross_diff, alternative = "two.sided")[[parameter]],
            t.test(atc_ret_diff, alternative = "two.sided")[[parameter]],
            t.test(net_ret_diff, alternative = "two.sided")[[parameter]],
            t.test(net_sr_diff, alternative = "two.sided")[[parameter]]),
            by = decileName]
        setnames(decileStatistics[[parameter]][[1]],
                 c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"),
                 newNames)

        # now the same for all deciles
        decileStatistics[[parameter]][[2]] <- decileData[!is.na(get(decileName)), list(
            0,
            t.test(gross_ret_act, alternative = "two.sided")[[parameter]],
            t.test(net_ret_act, alternative = "two.sided")[[parameter]],
            t.test(gross_ret_cop, alternative = "two.sided")[[parameter]],
            t.test(atc_ret_cop, alternative = "two.sided")[[parameter]],
            t.test(net_ret_cop, alternative = "two.sided")[[parameter]],
            t.test(net_sr_act, alternative = "two.sided")[[parameter]],
            t.test(net_sr_cop, alternative = "two.sided")[[parameter]],
            t.test(gross_diff, alternative = "two.sided")[[parameter]],
            t.test(atc_ret_diff, alternative = "two.sided")[[parameter]],
            t.test(net_ret_diff, alternative = "two.sided")[[parameter]],
            t.test(net_sr_diff, alternative = "two.sided")[[parameter]])]
        setnames(decileStatistics[[parameter]][[2]],
                 c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12"),
                 names(decileStatistics[[parameter]][[1]]))


        # bind deciles and wholesample analysis together
        decileStatistics[[parameter]] <- rbindlist(decileStatistics[[parameter]])
        # sort on decile
        setkeyv(decileStatistics[[parameter]], decileName)
    }


    return(decileStatistics)
}

getCompleteDecileAnalyis <- function() {
# This function returns as list of lists of lists, where
# - the first element is either means the size of the fund:
#   10, 50 or 250
# - the second is decile name by which the sorting is performed
#   primitive_return_decile, net_return_decile, exp_ratio_decile or sr_decile
# - the thirs element is type of statistics
#   mean, tStat or pValue
#
# To access t-statistics for a 50M size fund of deciles by net return difference
# (assuming the you save the result of the function into variable result) you
# should write decileAnalysis$`50`$net_return_decile$tStat.
# (If you do it in command line, then wait after writing $, and RStudio will show
# you the options).
#
    decileAnalysis <- list()
    for(size in c(10,50,250)) {
        decileAnalysis[[as.character(size)]] <- list()
        for(decileName in c("primitive_return_decile",
                            "net_return_decile",
                            "exp_ratio_decile",
                            "sr_decile")) {
            decileAnalysis[[as.character(size)]][[decileName]] <-
                getDecileAnalyis(size, decileName)
        }
    }
    return(decileAnalysis)
}