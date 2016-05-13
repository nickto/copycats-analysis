if (!require("dplyr")) install.packages("dplyr")
if (!require("RPostgreSQL")) install.packages("RPostgreSQL")
if (!require("lubridate")) install.packages("lubridate")
if (!require("data.table")) install.packages("data.table")
if (!require("R.cache")) install.packages("R.cache")
n
if (!require("sandwich")) install.packages("sandwich")
if (!require("lmtest")) install.packages("lmtest")

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("xts")) install.packages("xts")
if (!require("ggfortify")) install.packages("ggfortify")
if (!require("scales")) install.packages("scales")
if (!require("tikzDevice")) install.packages("tikzDevice")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("xtable")) install.packages("xtable")

# If the above command does not work, then install from another repo:
# install.packages('ff', repo='http://nbcgib.uesc.br/mirrors/cran/')
library(dplyr)       # data frame manipulations
library(RPostgreSQL) # connection to Postgre
library(lubridate)   # usefule date function
library(data.table)  # quick and slightly different version of data tables
library(R.cache)     # caching

library(sandwich)    # Newey-west var-covar matrix
library(lmtest)      # display coefficients of regression

library(ggplot2)     # graph package
library(xts)         # is needed for time series in ggfortify
library(ggfortify)   # another graph package, useful for time-sries
library(scales)      # for scaling variables in ggplots
# For this to work correcly you need to have a LaTeX compiler
library(tikzDevice)  # for outputing graphs to LaTeX
#library(Hmisc)
library(xtable)

