#if (!require("ff")) install.packages("ff")
#if (!require("ffbase")) install.packages("ffbase")
if (!require("plyr")) install.packages("plyr")
if (!require("RPostgreSQL")) install.packages("RPostgreSQL")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("xts")) install.packages("xts")
if (!require("ggfortify")) install.packages("ggfortify")

# If the above command does not work, then install from another repo:
# install.packages('ff', repo='http://nbcgib.uesc.br/mirrors/cran/')
#library(ff)
#library(ffbase)
library(plyr)
library(RPostgreSQL)
library(ggplot2)
library(xts)
library(ggfortify)


