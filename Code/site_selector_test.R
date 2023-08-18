# This script is used to easily test the site_selector.R file without knitting.
# Do not use for real applications.
# AJ Brown 13 Aug 2023


library(dplyr)
library(ggplot2)
library(tidyr)
#library(GGally)
#library(plotly)
library(PerformanceAnalytics)

loc = 'AVRC STAR'

# Uploading file-merge code to create data frame
source('./Code/file-merger.R')
# create data frame
dat <- returnAllFiles(d = directory, export = FALSE)
# make columns lowercase so they are all the same case
colnames(dat) <- tolower(names(dat))
















