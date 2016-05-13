inc <- function(x, n = 1){
  eval.parent(substitute(x <- x + n))
}

getFamaFactors <- function() {
    # Get monthly Fama French factors.

    sql_command <- paste0("
    select
      date_part('year', dateff) as year,
      date_part('month', dateff) as month,
      mktrf,
      smb,
      hml,
      rf,
      umd
    from factors.monthly
    ")

    ff <- as.data.table(dbGetQuery(con, sql_command))

    return(ff)
}

trimLongDataFrame <- function(df, trim = c(1,100)) {
    df <- tmp

    df[, percentile := ntile(value, 100), by = variable]
    df[percentile %in% trim, value := NA]
    df[, percentile := NULL]

    return(df)
}

getFundLevelAlphas <- function(returns, ff) {

    # --------------------------------------------------------------------------
    # function for regression
        getAlphaStatitstic <- function(col, ff) {
            fm <- lm(col ~ rf + mktrf + smb + hml + umd, data = ff)
            se <- coeftest(fm, NeweyWest(fm))

            result <- list(
                alpha <- se[1,1],
                tstat <- se[1,3],
                pvalue <- se[1,4]
            )

            return(result)
        }
    # --------------------------------------------------------------------------

    wfcinList <- unique(returns[, wfcin])

    statList <- list()
    iWfcin <- 0
    for(wfcinCur in wfcinList) {
        # get data on current fund
        wfcinReturns <- returns[wfcin == wfcinCur,]
        wfcinFF <- ff[
            paste(year, month) %in% unique(wfcinReturns[, paste(year, month)])
        ]
        setkey(wfcinReturns, year, month)
        setkey(wfcinFF, year, month)

        # regress each column in the data table for current fund
        statistics <- try(
            wfcinReturns[,
                lapply(.SD, getAlphaStatitstic, wfcinFF),
                .SDcols = -c("year",
                             "month",
                             "caldt",
                             "wfcin",
                             "m_exp",
                             "costs_10m_cop",
                             "costs_50m_cop",
                             "costs_250m_cop")
            ],
            silent = TRUE
        )

        if(is.data.table(statistics)) {
            # exitted with no errors
            # add names
            statistics[, type := c("alpha", "tstat", "pvalue")]
            statistics[, wfcin := wfcinCur]

            statList[[inc(iWfcin)]] <- statistics
        }
    }

    alphas <- rbindlist(statList)

    # make columns vectors instead of lists
    for(i in 1:ncol(alphas)) {
        alphas[, i] <- unlist(alphas[, i, with = FALSE])
    }

    return(alphas)
}

addDiffs <- function(returns) {
    returns[, gross_ret_diff := gross_ret_cop - gross_ret_act]
    returns[, atc_ret_10m_diff := atc_ret_10m_cop - gross_ret_act]
    returns[, atc_ret_50m_diff := atc_ret_50m_cop - gross_ret_act]
    returns[, atc_ret_250m_diff := atc_ret_250m_cop - gross_ret_act]
    returns[, net_ret_10m_diff := net_ret_10m_cop - net_ret_act]
    returns[, net_ret_50m_diff := net_ret_50m_cop - net_ret_act]
    returns[, net_ret_250m_diff := net_ret_250m_cop - net_ret_act]

    return(returns)
}

getFundLevelStatistics <- function() {
    sql_command <- paste0("
    select
      *
    from performance.monthly
    ")
    wholeSample <- as.data.table(dbGetQuery(con, sql_command))
    wholeSample <- addDiffs(wholeSample)

    ss <- list()
    # mean
    ss$mean <- wholeSample[,
        lapply(.SD, mean, na.rm = TRUE),
        .SDcols = -c("year", "month", "caldt"),
        by = "wfcin"
    ]
    # median
    ss$median <- wholeSample[,
        lapply(.SD, median, na.rm = TRUE),
        .SDcols = -c("year", "month", "caldt"),
        by = "wfcin"
    ]

    # alphas
    ff <- getFamaFactors()
    ss$alpha <- getFundLevelAlphas(wholeSample, ff)

    ss$mean <- addDiffs(ss$mean)
    ss$alpha <- addDiffs(ss$alpha)

    return(ss)
}

getStatsAcrossFundsFirst <- function(ss) {

    resultList <- list()
    resultRow <- 0
    # Returns
    # Returns
    for(varName in c("gross_ret_act",
                     "gross_ret_cop",
                     "atc_ret_10m_cop",
                     "atc_ret_50m_cop",
                     "atc_ret_250m_cop",
                     "net_ret_act",
                     "net_ret_10m_cop",
                     "net_ret_50m_cop",
                     "net_ret_250m_cop")) {
        resultList[[inc(resultRow)]] <- data.table(
            variable =  varName,
            mean =      mean(ss$mean[, get(varName)], na.rm = TRUE),
            std =       sd(ss$mean[, get(varName)], na.rm = TRUE),
            tstat =     t.test(ss$mean[, get(varName)], na.rm = TRUE)$statistic,
            tstatP =    t.test(ss$mean[, get(varName)], na.rm = TRUE)$p.value,
            q1 =        fivenum(ss$mean[, get(varName)], na.rm = TRUE)[2],
            q2 =        fivenum(ss$mean[, get(varName)], na.rm = TRUE)[3],
            q3 =        fivenum(ss$mean[, get(varName)], na.rm = TRUE)[4],
            wilcoxonP = as.numeric(NA)
        )
    }

    # differences
    varNames <- cbind(
        c("gross_ret_diff",
          "atc_ret_10m_diff",
          "atc_ret_50m_diff",
          "atc_ret_250m_diff",
          "net_ret_10m_diff",
          "net_ret_50m_diff",
          "net_ret_250m_diff"),
        c("gross_ret_cop",
          "atc_ret_10m_cop",
          "atc_ret_50m_cop",
          "atc_ret_250m_cop",
          "net_ret_10m_cop",
          "net_ret_50m_cop",
          "net_ret_250m_cop"),
        c("gross_ret_act",
          "gross_ret_act",
          "gross_ret_act",
          "gross_ret_act",
          "net_ret_act",
          "net_ret_act",
          "net_ret_act")
    )
    for(i in 1:nrow(varNames)) {
        curRow <- varNames[i,]
        resultList[[inc(resultRow)]] <- data.table(
            variable =  curRow[1],
            mean = mean(
                ss$mean[, get(curRow[2])] - ss$mean[, get(curRow[3])],
                na.rm = TRUE),
            std = sd(
                ss$mean[, get(curRow[2])] - ss$mean[, get(curRow[3])],
                na.rm = TRUE),
            tstat = t.test(
                ss$mean[, get(curRow[2])], ss$mean[, get(curRow[3])],
                na.rm = TRUE)$statistic,
            tstatP = t.test(
                ss$mean[, get(curRow[2])], ss$mean[, get(curRow[3])],
                na.rm = TRUE)$p.value,
            q1 = fivenum(
                ss$mean[, get(curRow[2])] - ss$mean[, get(curRow[3])],
                na.rm = TRUE)[2],
            q2 = fivenum(
                ss$mean[, get(curRow[2])] - ss$mean[, get(curRow[3])],
                na.rm = TRUE)[3],
            q3 = fivenum(
                ss$mean[, get(curRow[2])] - ss$mean[, get(curRow[3])],
                na.rm = TRUE)[4],
            wilcoxonP = wilcox.test(
                ss$mean[, get(curRow[2])], ss$mean[, get(curRow[3])],
                na.rm = TRUE)$p.value
        )
    }

    # Alphas
    # Returns
    for(varName in c("gross_ret_act",
                     "gross_ret_cop",
                     "atc_ret_10m_cop",
                     "atc_ret_50m_cop",
                     "atc_ret_250m_cop",
                     "net_ret_act",
                     "net_ret_10m_cop",
                     "net_ret_50m_cop",
                     "net_ret_250m_cop")) {
        resultList[[inc(resultRow)]] <- data.table(
            variable =  varName,
            mean =      mean(ss$alpha[type == "alpha", get(varName)], na.rm = TRUE),
            std =       sd(ss$alpha[type == "alpha", get(varName)], na.rm = TRUE),
            tstat =     t.test(ss$alpha[type == "alpha", get(varName)], na.rm = TRUE)$statistic,
            tstatP =    t.test(ss$alpha[type == "alpha", get(varName)], na.rm = TRUE)$p.value,
            q1 =        fivenum(ss$alpha[type == "alpha", get(varName)], na.rm = TRUE)[2],
            q2 =        fivenum(ss$alpha[type == "alpha", get(varName)], na.rm = TRUE)[3],
            q3 =        fivenum(ss$alpha[type == "alpha", get(varName)], na.rm = TRUE)[4],
            wilcoxonP = as.numeric(NA)
        )
    }

    # differences
    varNames <- cbind(
        c("gross_ret_diff",
          "atc_ret_10m_diff",
          "atc_ret_50m_diff",
          "atc_ret_250m_diff",
          "net_ret_10m_diff",
          "net_ret_50m_diff",
          "net_ret_250m_diff"),
        c("gross_ret_cop",
          "atc_ret_10m_cop",
          "atc_ret_50m_cop",
          "atc_ret_250m_cop",
          "net_ret_10m_cop",
          "net_ret_50m_cop",
          "net_ret_250m_cop"),
        c("gross_ret_act",
          "gross_ret_act",
          "gross_ret_act",
          "gross_ret_act",
          "net_ret_act",
          "net_ret_act",
          "net_ret_act")
    )
    for(i in 1:nrow(varNames)) {
        curRow <- varNames[i,]
        resultList[[inc(resultRow)]] <- data.table(
            variable =  curRow[1],
            mean = mean(
                ss$alpha[type == "alpha", get(curRow[2])] - ss$alpha[type == "alpha", get(curRow[3])],
                na.rm = TRUE),
            std = sd(
                ss$alpha[type == "alpha", get(curRow[2])] - ss$alpha[type == "alpha", get(curRow[3])],
                na.rm = TRUE),
            tstat = t.test(
                ss$alpha[type == "alpha", get(curRow[2])], ss$alpha[type == "alpha", get(curRow[3])],
                na.rm = TRUE)$statistic,
            tstatP = t.test(
                ss$alpha[type == "alpha", get(curRow[2])], ss$alpha[type == "alpha", get(curRow[3])],
                na.rm = TRUE)$p.value,
            q1 = fivenum(
                ss$alpha[type == "alpha", get(curRow[2])] - ss$alpha[type == "alpha", get(curRow[3])],
                na.rm = TRUE)[2],
            q2 = fivenum(
                ss$alpha[type == "alpha", get(curRow[2])] - ss$alpha[type == "alpha", get(curRow[3])],
                na.rm = TRUE)[3],
            q3 = fivenum(
                ss$alpha[type == "alpha", get(curRow[2])] - ss$alpha[type == "alpha", get(curRow[3])],
                na.rm = TRUE)[4],
            wilcoxonP = wilcox.test(
                ss$alpha[type == "alpha", get(curRow[2])], ss$alpha[type == "alpha", get(curRow[3])],
                na.rm = TRUE)$p.value
        )
    }










    df <- (rbindlist(resultList))
    latex(
        df,
        file = "test.tex",
        label = "tab:test-label",
        rgroup = c("Return", "Return difference", "Carhart Alpha", "Carhart Alpha difference"),
        n.rgroup = c(9, 7, 9, 7),
        na.blank = TRUE,
        rowname = NULL,
        colheads = c("", "Mean", "St. dev.","T-stat", "p-value", "25", "50", "75", "Wilcoxon"),
        booktabs = TRUE,
        #dcolumn = TRUE,
        dec = 3


    )




    xtable(
        x = df,
        digits = 3
    )


}

createPlots <- function(ss, widthCm = 15, heightCm = 7.75) {
    ciAbove <- function(x, na.rm = TRUE, ...) {
        t <- t.test(x, na.rm = na.rm, ...)
        return(t$conf.int[2])
    }
    ciBelow <- function(x, na.rm = TRUE, ...) {
        t <- t.test(x, na.rm = na.rm, ...)
        return(t$conf.int[1])
    }

    # Average returns
    meanReturnsLong <- melt(
        ss$mean,
        id.vars = c("wfcin"),
        measure.vars = c("gross_ret_act",
                         "atc_ret_10m_cop",
                         "atc_ret_50m_cop",
                         "atc_ret_250m_cop",
                         "net_ret_act",
                         "net_ret_10m_cop",
                         "net_ret_50m_cop",
                         "net_ret_250m_cop")
    )
    gMeans <- ggplot(meanReturnsLong, aes (x = variable, y = value)) +
        scale_x_discrete(
            breaks=c("gross_ret_act",
                     "atc_ret_10m_cop",
                     "atc_ret_50m_cop",
                     "atc_ret_250m_cop",
                     "net_ret_act",
                     "net_ret_10m_cop",
                     "net_ret_50m_cop",
                     "net_ret_250m_cop"),
            labels=c("ATC\nprimitive",
                     "ATC\ncopycat\n(10M)",
                     "ATC\ncopycat\n(50M)",
                     "ATC\ncopycat\n(250M)",
                     "Net\nprimitive",
                     "Net\ncopycat\n(10M)",
                     "Net\ncopycat\n(50M)",
                     "Net\ncopycat\n(250M)")
        ) +
        ylab("Average return") +
        #xlab("Return type") +
        theme(axis.title.x = element_blank()) +
        scale_y_continuous(
            labels = percent,
            limits = c(-0.005, + 0.015)) +
        # geom_point(
        #     position = position_jitter(width = 0.9),
        #     size = 0.1,
        #     alpha = 0.3) +
        geom_boxplot(
            na.rm = TRUE,
            notch = TRUE,
            outlier.shape = NA,
            alpha = 0.75) +
        geom_boxplot(
            na.rm = TRUE,
            notch = TRUE,
            outlier.shape = NA,
            fill = NA) +
        stat_boxplot(geom = "errorbar") +
        stat_summary(
            fun.y = mean,
            fun.ymin = ciBelow,
            fun.ymax = ciAbove,
            colour = "darkred",
            geom = "errorbar",
            width = 0.95,
            linetype = "dashed",
            show.legend = FALSE,
            na.rm = TRUE
        ) +
        stat_summary(
            fun.y = mean,
            fun.ymin = mean,
            fun.ymax = mean,
            colour = "darkred",
            geom = "errorbar",
            width = 0.95,
            #size = 1,
            linetype = "solid",
            show.legend = FALSE,
            na.rm = TRUE
        )

    tikz(file = 'latex/figures_means_boxplots.tikz',
         sanitize = TRUE,
         width = widthCm / 2.54,
         height=  heightCm / 2.54)
    plot(gMeans)
    dev.off()

    # Alphas of returns
    alphasReturnsLong <- melt(
        ss$alpha[type == "alpha", ],
        id.vars = c("wfcin"),
        measure.vars = c("gross_ret_act",
                         "atc_ret_10m_cop",
                         "atc_ret_50m_cop",
                         "atc_ret_250m_cop",
                         "net_ret_act",
                         "net_ret_10m_cop",
                         "net_ret_50m_cop",
                         "net_ret_250m_cop")
    )

    gAlphas <- ggplot(alphasReturnsLong, aes (x = variable, y = value)) +
        scale_x_discrete(
            breaks=c("gross_ret_act",
                     "atc_ret_10m_cop",
                     "atc_ret_50m_cop",
                     "atc_ret_250m_cop",
                     "net_ret_act",
                     "net_ret_10m_cop",
                     "net_ret_50m_cop",
                     "net_ret_250m_cop"),
            labels=c("ATC\nprimitive",
                     "ATC\ncopycat\n(10M)",
                     "ATC\ncopycat\n(50M)",
                     "ATC\ncopycat\n(250M)",
                     "Net\nprimitive",
                     "Net\ncopycat\n(10M)",
                     "Net\ncopycat\n(50M)",
                     "Net\ncopycat\n(250M)")
        ) +
        scale_y_continuous(
            labels = percent,
            limits = c(-0.0075, + 0.0075)) +
        ylab("Carhart alpha") +
        #xlab("Return type") +
        theme(axis.title.x = element_blank()) +
        # geom_point(
        #     position = position_jitter(width = 0.9),
        #     size = 0.1,
        #     alpha = 0.3) +
        geom_boxplot(
            na.rm = TRUE,
            notch = TRUE,
            outlier.shape = NA,
            alpha = 0.75) +
        geom_boxplot(
            na.rm = TRUE,
            notch = TRUE,
            outlier.shape = NA,
            fill = NA) +
        stat_boxplot(geom = "errorbar") +
        stat_summary(
            fun.y = mean,
            fun.ymin = ciBelow,
            fun.ymax = ciAbove,
            colour = "darkred",
            geom = "errorbar",
            width = 0.95,
            linetype = "dashed",
            show.legend = FALSE,
            na.rm = TRUE
        ) +
        stat_summary(
            fun.y = mean,
            fun.ymin = mean,
            fun.ymax = mean,
            colour = "darkred",
            geom = "errorbar",
            width = 0.95,
            #size = 1,
            linetype = "solid",
            show.legend = FALSE,
            na.rm = TRUE
        )

    tikz(file = 'latex/figures_alphas_boxplots.tikz',
         sanitize = TRUE,
         width = widthCm / 2.54,
         height=  heightCm / 2.54)
    plot(gAlphas)
    dev.off()



    # Differences in average returns
    meanDiffsLong <- melt(
        ss$mean,
        id.vars = c("wfcin"),
        measure.vars = c("atc_ret_10m_diff",
                         "atc_ret_50m_diff",
                         "atc_ret_250m_diff",
                         "net_ret_10m_diff",
                         "net_ret_50m_diff",
                         "net_ret_250m_diff")
    )

    gMeanDiffs <- ggplot(meanDiffsLong, aes (x = variable, y = value)) +
        scale_x_discrete(
            breaks=c("atc_ret_10m_diff",
                     "atc_ret_50m_diff",
                     "atc_ret_250m_diff",
                     "net_ret_10m_diff",
                     "net_ret_50m_diff",
                     "net_ret_250m_diff"),
            labels=c("ATC\n(10M)",
                     "ATC\n(50M)",
                     "ATC\n(250M)",
                     "Net\n(10M)",
                     "Net\n(50M)",
                     "Net\n(250M)")
        ) +
        scale_y_continuous(
            labels = percent,
            limits = c(-0.004, + 0.006)) +
        ylab("Average return difference") +
        #xlab("Return type") +
        theme(axis.title.x = element_blank()) +
        # geom_point(
        #     position = position_jitter(width = 0.9),
        #     size = 0.1,
        #     alpha = 0.3) +
        geom_boxplot(
            na.rm = TRUE,
            notch = TRUE,
            outlier.shape = NA,
            alpha = 0.75) +
        geom_boxplot(
            na.rm = TRUE,
            notch = TRUE,
            outlier.shape = NA,
            fill = NA) +
        stat_boxplot(geom = "errorbar") +
        stat_summary(
            fun.y = mean,
            fun.ymin = ciBelow,
            fun.ymax = ciAbove,
            colour = "darkred",
            geom = "errorbar",
            width = 0.95,
            linetype = "dashed",
            show.legend = FALSE,
            na.rm = TRUE
        ) +
        stat_summary(
            fun.y = mean,
            fun.ymin = mean,
            fun.ymax = mean,
            colour = "darkred",
            geom = "errorbar",
            width = 0.95,
            #size = 1,
            linetype = "solid",
            show.legend = FALSE,
            na.rm = TRUE
        )

    tikz(file = 'latex/figures_means_diff_boxplots.tikz',
         sanitize = TRUE,
         width = widthCm / 2.54,
         height=  heightCm / 2.54)
    plot(gMeanDiffs)
    dev.off()


    # Differences in alphas
    alphaDiffsLong <- melt(
        ss$alpha[type == "alpha"],
        id.vars = c("wfcin"),
        measure.vars = c("atc_ret_10m_diff",
                         "atc_ret_50m_diff",
                         "atc_ret_250m_diff",
                         "net_ret_10m_diff",
                         "net_ret_50m_diff",
                         "net_ret_250m_diff")
    )

    gAlphaDiffs <- ggplot(alphaDiffsLong, aes (x = variable, y = value)) +
        scale_x_discrete(
            breaks=c("atc_ret_10m_diff",
                     "atc_ret_50m_diff",
                     "atc_ret_250m_diff",
                     "net_ret_10m_diff",
                     "net_ret_50m_diff",
                     "net_ret_250m_diff"),
            labels=c("ATC\n(10M)",
                     "ATC\n(50M)",
                     "ATC\n(250M)",
                     "Net\n(10M)",
                     "Net\n(50M)",
                     "Net\n(250M)")
        ) +
        scale_y_continuous(
            labels = percent,
            limits = c(-0.0065, + 0.007)) +
        ylab("Average alpha difference") +
        #xlab("Return type") +
        theme(axis.title.x = element_blank()) +
        # geom_point(
        #     position = position_jitter(width = 0.9),
        #     size = 0.1,
        #     alpha = 0.3) +
        geom_boxplot(
            na.rm = TRUE,
            notch = TRUE,
            outlier.shape = NA,
            alpha = 0.75) +
        geom_boxplot(
            na.rm = TRUE,
            notch = TRUE,
            outlier.shape = NA,
            fill = NA) +
        stat_boxplot(geom = "errorbar") +
        stat_summary(
            fun.y = mean,
            fun.ymin = ciBelow,
            fun.ymax = ciAbove,
            colour = "darkred",
            geom = "errorbar",
            width = 0.95,
            linetype = "dashed",
            show.legend = FALSE,
            na.rm = TRUE
        ) +
        stat_summary(
            fun.y = mean,
            fun.ymin = mean,
            fun.ymax = mean,
            colour = "darkred",
            geom = "errorbar",
            width = 0.95,
            #size = 1,
            linetype = "solid",
            show.legend = FALSE,
            na.rm = TRUE
        )

    tikz(file = 'latex/figures_alphas_diff_boxplots.tikz',
         sanitize = TRUE,
         width = widthCm / 2.54,
         height=  heightCm / 2.54)
    plot(gAlphaDiffs)
    dev.off()

    return(TRUE)
}



getCopycatPerformanceWholeSample <- function() {
# This function returns a data table that compares means of primitive funds and
# copycats. It also contains t-statistics and p-values.

    sql_command <- paste0("
    select
      gross_ret_act as primitive_gross,
      gross_ret_cop as copycat_gross,
      gross_ret_cop - gross_ret_act as gross_diff,
      net_ret_act as primitive_net,
      atc_ret_10m_cop as copycat_atc_10m,
      atc_ret_50m_cop as copycat_atc_50m,
      atc_ret_250m_cop as copycat_atc_250m,
      atc_ret_10m_cop - gross_ret_act as atc_diff_10m,
      atc_ret_50m_cop - gross_ret_act as atc_diff_50m,
      atc_ret_250m_cop - gross_ret_act as atc_diff_250m,
      net_ret_10m_cop as copycat_net_10m,
      net_ret_50m_cop as copycat_net_50m,
      net_ret_250m_cop as copycat_net_250m,
      net_ret_10m_cop - net_ret_act as net_diff_10m,
      net_ret_50m_cop - net_ret_act as net_diff_50m,
      net_ret_250m_cop - net_ret_act as net_diff_250m
    from performance.monthly
    where wfcin is not null
    -- legacy. should be removed later!
    --and m_exp >= 0
    ")
    wholeSampleDiffs <- as.data.table(dbGetQuery(con, sql_command))

    diffList <- list()
    for(i in 1:dim(wholeSampleDiffs)[2]) {
        t <- t.test(wholeSampleDiffs[, i, with = FALSE], alternative = "two.sided")

        diffList[[i]] <- data.table(
            perf_measure = names(wholeSampleDiffs)[i],
            estimate = t$estimate,
            statistic = t$statistic,
            p.value = t$p.value
        )
    }
    performanceComparison <- rbindlist(diffList)

    return(performanceComparison)
}

getMonthlyAverages <- function(conf = 0.95) {
# This function returns a data table that compares means of primitive funds and
# copycats. It also contains t-statistics and p-values.

    sql_command <- paste0("
    select
      year,
      month,
      gross_ret_act as primitive_gross,
      gross_ret_cop as copycat_gross,
      gross_ret_cop - gross_ret_act as gross_diff,
      net_ret_act as primitive_net,
      atc_ret_10m_cop as copycat_atc_10m,
      atc_ret_50m_cop as copycat_atc_50m,
      atc_ret_250m_cop as copycat_atc_250m,
      atc_ret_10m_cop - gross_ret_act as atc_diff_10m,
      atc_ret_50m_cop - gross_ret_act as atc_diff_50m,
      atc_ret_250m_cop - gross_ret_act as atc_diff_250m,
      net_ret_10m_cop as copycat_net_10m,
      net_ret_50m_cop as copycat_net_50m,
      net_ret_250m_cop as copycat_net_250m,
      net_ret_10m_cop - net_ret_act as net_diff_10m,
      net_ret_50m_cop - net_ret_act as net_diff_50m,
      net_ret_250m_cop - net_ret_act as net_diff_250m
    from performance.monthly
    where wfcin is not null
    -- legacy. should be removed later!
    --and m_exp >= 0
    ")
    fundLevel <- as.data.table(dbGetQuery(con, sql_command))

    #monthly <- monthly[, lapply(.SD, mean, na.rm = TRUE), by = list(year, month)]

    setkey(fundLevel, year, month)

    ts <- list()
    for(i in 3:dim(fundLevel)[2]) {
        cur <- fundLevel[,c(1,2,i), with = FALSE]
        curName <- names(cur)[3]

        print(i)

        ts[[i-2]] <- cur[,
            t.test(get(curName), conf.level = conf),
            by = list(year, month)
        ]
    }

    return(ts)
}

getCopycatPerformanceByYear <- function() {
# This function returns a data table that compares means of primitive funds and
# copycats by year. Unlike getCopycatPerformanceWholeSample it does not return
# t-statistics and p-values.

    sql_command <- paste0("
    select
      year,
      avg(gross_ret_act) as primitive_gross,
      avg(net_ret_act) as primitive_net,
      avg(gross_ret_cop) as copycat_gross,
      avg(net_ret_10m_cop) as copycat_net_10m,
      avg(net_ret_50m_cop) as copycat_net_50m,
      avg(net_ret_250m_cop) as copycat_net_250m,
      avg(atc_ret_10m_cop) as copycat_atc_10m,
      avg(atc_ret_50m_cop) as copycat_atc_50m,
      avg(atc_ret_250m_cop) as copycat_atc_250m,
      avg(gross_ret_cop) - avg(gross_ret_act) as gross_diff,
      avg(atc_ret_10m_cop) - avg(gross_ret_act) as atc_diff_10m,
      avg(atc_ret_50m_cop) - avg(gross_ret_act) as atc_diff_50m,
      avg(atc_ret_250m_cop) - avg(gross_ret_act) as atc_diff_250m,
      avg(net_ret_10m_cop) - avg(net_ret_act) as net_diff_10m,
      avg(net_ret_50m_cop) - avg(net_ret_act) as net_diff_50m,
      avg(net_ret_250m_cop) - avg(net_ret_act) as net_diff_250m
    from performance.monthly
    where wfcin is not null
    -- legacy. should be removed later!
    -- and m_exp >= 0
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
        -- and m_exp >= 0
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
            atc_ret_", size, "m_cop - gtoss_ret_act as atc_ret_diff,
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
        -- and m_exp >= 0
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
                            "net_ret_cop")
    dependentVariablesDiffs <- cbind(
        c("gross_diff",
          "atc_ret_diff",
          "net_ret_diff"),
        c("gross_ret_cop",
          "atc_ret_cop",
          "net_ret_cop"),
        c("gross_ret_act",
          "gross_ret_act",
          "net_ret_act")
    )

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
                dataCop <- list()
                dataAct <- list()
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
                dataD1MinusD10 <- merge(
                    data[[1]],
                    data[[10]],
                    by = c("year", "month"),
                    suffixes = c("1", "10")
                )

                t1Test <- t.test(dataD1MinusD10[, monthMean1 - monthMean10], na.rm = TRUE)
                t2Test <- t.test(dataD1MinusD10[, monthMean1], dataD1MinusD10[, monthMean10], na.rm = TRUE)

                means[[as.character(size)]][[decileName]][["mean"]][11, (dependentVariable) := t1Test$estimate]
                means[[as.character(size)]][[decileName]][["tstat"]][11, (dependentVariable) := t2Test$statistic]
                means[[as.character(size)]][[decileName]][["pvalue"]][11, (dependentVariable) := t2Test$p.value]
            }

            for(i in 1:nrow(dependentVariablesDiffs)) {
                # i <- 1
                varName <- dependentVariablesDiffs[i,1]
                copName <- dependentVariablesDiffs[i,2]
                actName <- dependentVariablesDiffs[i,3]

                # means for deciles 1 to 10
                data <- list()
                for(decile in 1:10) {
                    # decile <- 2

                    # get data for the current decile (averages)
                    data[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(copName) - get(actName), na.rm =TRUE)
                        ), by = list(year, month)]
                    setkey(data[[decile]], year, month)
                    dataCop[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(copName), na.rm =TRUE)
                        ), by = list(year, month)]
                    setkey(dataCop[[decile]], year, month)
                    dataAct[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(actName), na.rm =TRUE)
                        ), by = list(year, month)]
                    setkey(dataAct[[decile]], year, month)

                    t1Test <- t.test(data[[decile]][, monthMean])
                    t2Test <- t.test(dataCop[[decile]][, monthMean], dataAct[[decile]][, monthMean])

                    means[[as.character(size)]][[decileName]][["mean"]][decile, (varName) := t1Test$estimate]
                    means[[as.character(size)]][[decileName]][["tstat"]][decile, (varName) := t2Test$statistic]
                    means[[as.character(size)]][[decileName]][["pvalue"]][decile, (varName) := t2Test$p.value]
                }

                # means fore decile 1 minus decile 10
                dataD1MinusD10 <- merge(
                    data[[1]],
                    data[[10]],
                    by = c("year", "month"),
                    suffixes = c("1", "10")
                )

                t1Test <- t.test(dataD1MinusD10[, monthMean1 - monthMean10], na.rm = TRUE)
                t2Test <- t.test(dataD1MinusD10[, monthMean1], dataD1MinusD10[, monthMean10], na.rm = TRUE)

                means[[as.character(size)]][[decileName]][["mean"]][11, (varName) := t1Test$estimate]
                means[[as.character(size)]][[decileName]][["tstat"]][11, (varName) := t2Test$statistic]
                means[[as.character(size)]][[decileName]][["pvalue"]][11, (varName) := t2Test$p.value]
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
                            "net_ret_cop")
    dependentVariablesDiffs <- cbind(
        c("gross_diff",
          "atc_ret_diff",
          "net_ret_diff"),
        c("gross_ret_cop",
          "atc_ret_cop",
          "net_ret_cop"),
        c("gross_ret_act",
          "gross_ret_act",
          "net_ret_act")
    )

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
                dataCop <- list()
                dataAct <- list()
                for(decile in 1:10) {
                    # decile <- 2

                    # get data for the current decile (averages)
                    data[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(dependentVariable), na.rm =TRUE),
                            mktrf = mean(mktrf), # the number is the same, so it is not actually mean
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
                dataD1MinusD10 <- merge(
                    data[[1]],
                    data[[10]][, list(month, year, monthMean)],
                    by = c("year", "month"),
                    suffixes = c("1", "10")
                )

                fm <- lm((monthMean1 - monthMean10) ~ rf + mktrf + smb + hml + umd, data = dataD1MinusD10)
                se <- coeftest(fm, NeweyWest(fm, lag = 2))

                alphas[[as.character(size)]][[decileName]][["alpha"]][11, (dependentVariable) := se[1,1]]
                alphas[[as.character(size)]][[decileName]][["tstat"]][11, (dependentVariable) := se[1,3]]
                alphas[[as.character(size)]][[decileName]][["pvalue"]][11, (dependentVariable) := se[1,4]]
            }

            for(i in 1:nrow(dependentVariablesDiffs)) {
                # dependentVariable <- "net_ret_diff"
                varName <- dependentVariablesDiffs[i,1]
                copName <- dependentVariablesDiffs[i,2]
                actName <- dependentVariablesDiffs[i,3]

                # alphas for deciles 1 to 10
                data <- list()
                for(decile in 1:10) {
                    # decile <- 2

                    # get data for the current decile (averages)
                    data[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(copName) - get(actName), na.rm =TRUE),
                            mktrf = mean(mktrf), # the number is the same, so it is not actually mean
                            smb = mean(smb),
                            hml = mean(hml),
                            rf = mean(rf),
                            umd = mean(umd)
                        ), by = list(year, month)]
                    setkey(data[[decile]], year, month)
                    dataCop[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(copName), na.rm =TRUE),
                            mktrf = mean(mktrf), # the number is the same, so it is not actually mean
                            smb = mean(smb),
                            hml = mean(hml),
                            rf = mean(rf),
                            umd = mean(umd)
                        ), by = list(year, month)]
                    setkey(dataCop[[decile]], year, month)
                    dataAct[[decile]] <- decileData[get(decileName) == decile, list(
                            monthMean = mean(get(actName), na.rm =TRUE),
                            mktrf = mean(mktrf), # the number is the same, so it is not actually mean
                            smb = mean(smb),
                            hml = mean(hml),
                            rf = mean(rf),
                            umd = mean(umd)
                        ), by = list(year, month)]
                    setkey(dataAct[[decile]], year, month)

                    t1Test <- t.test(data[[decile]][, monthMean])
                    t2Test <- t.test(dataCop[[decile]][, monthMean], dataAct[[decile]][, monthMean])

                    alphas[[as.character(size)]][[decileName]][["alpha"]][decile, (varName) := t1Test$estimate]
                    alphas[[as.character(size)]][[decileName]][["tstat"]][decile, (varName) := t2Test$statistic]
                    alphas[[as.character(size)]][[decileName]][["pvalue"]][decile, (varName) := t2Test$p.value]
                }

                 # alphas fore decile 1 minus decile 10
                dataD1MinusD10 <- merge(
                    data[[1]],
                    data[[10]][, list(month, year, monthMean)],
                    by = c("year", "month"),
                    suffixes = c("1", "10")
                )

                fm <- lm((monthMean1 - monthMean10) ~ rf + mktrf + smb + hml + umd, data = dataD1MinusD10)
                se <- coeftest(fm, NeweyWest(fm, lag = 2))

                alphas[[as.character(size)]][[decileName]][["alpha"]][11, (varName) := se[1,1]]
                alphas[[as.character(size)]][[decileName]][["tstat"]][11, (varName) := se[1,3]]
                alphas[[as.character(size)]][[decileName]][["pvalue"]][11, (varName) := se[1,4]]
            }
        }

    }

    return(alphas)
}



createTable <- function(decileMeans, decileAlphas) {
    # This function creates tabularx tables for latex.

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
            # This function creates data tabels with the same layout as
            # desired in tables in latex.

            panel <- data.table(
                decile        = decileData$`10`[[decileBy]][[measure]]$decile,
                atcRetPrim    = decileData$`10`[[decileBy]][[measure]]$gross_ret_act,
                atcRetCopy10  = decileData$`10`[[decileBy]][[measure]]$atc_ret_cop,
                atcRetCopy50  = decileData$`50`[[decileBy]][[measure]]$atc_ret_cop,
                atcRetCopy250  = decileData$`250`[[decileBy]][[measure]]$atc_ret_cop,
                atcRetDiff10  = decileData$`10`[[decileBy]][[measure]]$atc_ret_diff,
                atcRetDiff50  = decileData$`50`[[decileBy]][[measure]]$atc_ret_diff,
                atcRetDiff250  = decileData$`250`[[decileBy]][[measure]]$atc_ret_diff,
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
                stat <- as.numeric(stat * 100)
            }
            statStr <- formatC(x = stat, format='f', digits=roundN )
            statStr <- paste0("\\num{", statStr, "}")
            # add significance asterisks
            if(pvalue <= 0.01) {
                statStr <- paste0(statStr, "***\\phantom{)}")
            } else if(pvalue <= 0.05) {
                statStr <- paste0(statStr, "**\\phantom{*)}")
            } else if(pvalue <= 0.1) {
                statStr <- paste0(statStr, "*\\phantom{**)}")
            } else {
                statStr <- paste0(statStr, "\\phantom{***)}")
            }
        } else {
            stat <- as.numeric(stat)
            statStr <- formatC(x = stat, format='f', digits=roundN )
            statStr <- paste0("(\\num{", statStr, "})\\phantom{***}")
        }


        return(statStr)
    }

    writePanel <- function(outputFileName, decileData, panelCaption, measure) {
        # This function writes a panles to the tex file.

        # Add panel name to a file
        panelNameString <- paste0(
            "\n\t\t\\midrule\n\t\t\\mc{15}{l}{\\scriptsize{\\textit{\\quad ",
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
                    if(panel$measure[row, col, with = FALSE] == 11) {
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























#
# getDecileMeans <- function() {
# # This function return mean comparisons (with t-statistic and p-values) for
# # different decile sorting and different fund sizes. Note that decile 11
# # means decile 1 minus decile 10.
# #
# # The returned object is a list of lists... where the first level indicates
# # the fund size, the second level indicates which variable the deciles where
# # sorted, the third level contains difference statistics (means, t-stat etc),
# # each of this statistics is represented using a data table.
#
#     decileNames <- c("primitive_return_decile",
#                      "net_return_decile",
#                      "exp_ratio_decile",
#                      "sr_decile",
#                      "alphas_decile")
#     dependentVariables <- c("gross_ret_act",
#                             "net_ret_act",
#                             "gross_ret_cop",
#                             "atc_ret_cop",
#                             "net_ret_cop",
#                             "gross_diff",
#                             "atc_ret_diff",
#                             "net_ret_diff",
#                             "gross_sr_act",
#                             "net_sr_act",
#                             "net_sr_cop",
#                             "net_sr_diff")
#
#     means <- list()
#     for(size in c(10,50,250)) {
#         #size <- 50
#         decileData <- getDecileData(size)
#         alphaDeciles <- getAlphaDeciles(size)
#
#         decileData <- merge(
#             decileData,
#             alphaDeciles[, list(wfcin, year, month, alphas_decile)],
#             by = c("wfcin", "year", "month")
#         )
#
#         means[[as.character(size)]] <- list()
#         for(decileName in decileNames) {
#             # decileName <- "net_return_decile"
#
#             means[[as.character(size)]][[decileName]] <- list()
#
#             means[[as.character(size)]][[decileName]][["mean"]]  <- data.table(decile = 1:11)
#             means[[as.character(size)]][[decileName]][["tstat"]]  <- data.table(decile = 1:11)
#             means[[as.character(size)]][[decileName]][["pvalue"]] <- data.table(decile = 1:11)
#
#
#             for(dependentVariable in dependentVariables) {
#                 # dependentVariable <- "net_ret_diff"
#
#                 # means for deciles 1 to 10
#                 data <- list()
#                 for(decile in 1:10) {
#                     # decile <- 2
#
#                     # get data for the current decile (averages)
#                     data[[decile]] <- decileData[get(decileName) == decile, list(
#                             monthMean = mean(get(dependentVariable), na.rm =TRUE)
#                         ), by = list(year, month)]
#                     setkey(data[[decile]], year, month)
#
#                     tTest <- t.test(data[[decile]][, monthMean])
#
#                     means[[as.character(size)]][[decileName]][["mean"]][decile, (dependentVariable) := tTest$estimate]
#                     means[[as.character(size)]][[decileName]][["tstat"]][decile, (dependentVariable) := tTest$statistic]
#                     means[[as.character(size)]][[decileName]][["pvalue"]][decile, (dependentVariable) := tTest$p.value]
#                 }
#
#                 # means fore decile 1 minus decile 10
#                 dataD1MinusD10 <- data[[1]]
#                 dataD1MinusD10[, monthMean := data[[1]][, monthMean] - data[[10]][, monthMean]]
#
#                 tTest <- t.test(dataD1MinusD10[, monthMean])
#
#                 means[[as.character(size)]][[decileName]][["mean"]][11, (dependentVariable) := tTest$estimate]
#                 means[[as.character(size)]][[decileName]][["tstat"]][11, (dependentVariable) := tTest$statistic]
#                 means[[as.character(size)]][[decileName]][["pvalue"]][11, (dependentVariable) := tTest$p.value]
#             }
#         }
#     }
#
#     return(means)
# }
#
# getDecileAlphas <- function() {
# # This function return carhart alphas (with t-statistic and p-values) for
# # different decile sorting and different fund sizes. Note that decile 11
# # means decile 1 minus decile 10.
# #
# # The returned object is a list of lists of lists, where the first level indicates
# # the fund size, the second level indicates which variable the deciles where
# # sorted, the third level contains difference statistics (alphas, t-stat etc),
# # each of this statistics is represented using a data table.
#
#     decileNames <- c("primitive_return_decile",
#                      "net_return_decile",
#                      "exp_ratio_decile",
#                      "sr_decile",
#                      "alphas_decile")
#     dependentVariables <- c("gross_ret_act",
#                             "net_ret_act",
#                             "gross_ret_cop",
#                             "atc_ret_cop",
#                             "net_ret_cop",
#                             "gross_diff",
#                             "atc_ret_diff",
#                             "net_ret_diff",
#                             "gross_sr_act",
#                             "net_sr_act",
#                             "net_sr_cop",
#                             "net_sr_diff")
#
#     alphas <- list()
#     for(size in c(10,50,250)) {
#         #size <- 50
#         decileData <- getDecileData(size)
#         alphaDeciles <- getAlphaDeciles(size)
#
#         decileData <- merge(
#             decileData,
#             alphaDeciles[, list(wfcin, year, month, alphas_decile)],
#             by = c("wfcin", "year", "month")
#         )
#
#         alphas[[as.character(size)]] <- list()
#         for(decileName in decileNames) {
#             # decileName <- "net_return_decile"
#
#             alphas[[as.character(size)]][[decileName]] <- list()
#
#             alphas[[as.character(size)]][[decileName]][["alpha"]]  <- data.table(decile = 1:11)
#             alphas[[as.character(size)]][[decileName]][["tstat"]]  <- data.table(decile = 1:11)
#             alphas[[as.character(size)]][[decileName]][["pvalue"]] <- data.table(decile = 1:11)
#
#
#             for(dependentVariable in dependentVariables) {
#                 # dependentVariable <- "net_ret_diff"
#
#                 # alphas for deciles 1 to 10
#                 data <- list()
#                 for(decile in 1:10) {
#                     # decile <- 2
#
#                     # get data for the current decile (averages)
#                     data[[decile]] <- decileData[get(decileName) == decile, list(
#                             monthMean = mean(get(dependentVariable), na.rm =TRUE),
#                             mktrf = mean(mktrf),
#                             smb = mean(smb),
#                             hml = mean(hml),
#                             rf = mean(rf),
#                             umd = mean(umd)
#                         ), by = list(year, month)]
#                     setkey(data[[decile]], year, month)
#
#                     fm <- lm(monthMean ~ rf + mktrf + smb + hml + umd, data = data[[decile]])
#                     se <- coeftest(fm, NeweyWest(fm, lag = 2))
#
#                     alphas[[as.character(size)]][[decileName]][["alpha"]][decile, (dependentVariable) := se[1,1]]
#                     alphas[[as.character(size)]][[decileName]][["tstat"]][decile, (dependentVariable) := se[1,3]]
#                     alphas[[as.character(size)]][[decileName]][["pvalue"]][decile, (dependentVariable) := se[1,4]]
#                 }
#
#                 # alphas fore decile 1 minus decile 10
#                 dataD1MinusD10 <- data[[1]]
#                 dataD1MinusD10[, monthMean := data[[1]][, monthMean] - data[[10]][, monthMean]]
#
#                 fm <- lm(monthMean ~ rf + mktrf + smb + hml + umd, data = dataD1MinusD10)
#                 se <- coeftest(fm, NeweyWest(fm, lag = 2))
#
#                 alphas[[as.character(size)]][[decileName]][["alpha"]][11, (dependentVariable) := se[1,1]]
#                 alphas[[as.character(size)]][[decileName]][["tstat"]][11, (dependentVariable) := se[1,3]]
#                 alphas[[as.character(size)]][[decileName]][["pvalue"]][11, (dependentVariable) := se[1,4]]
#
#             }
#         }
#     }
#
#     return(alphas)
#
# }








