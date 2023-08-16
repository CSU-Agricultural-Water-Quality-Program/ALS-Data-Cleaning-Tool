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

filtered_data <- dat %>%
  dplyr::filter(location.name == loc) %>%
  dplyr::mutate(sample.id = sub("-[1-5]$", "", sample.id),  # Remove "-1", "-2", "-3", "-4", "-5" from sample.id
                sample.id = sub("-[1-5]-D$", "-D", sample.id)) %>%  # Replace "-1-D", "-2-D", "-3-D", "-4-D", "-5-D" with "-D"
  dplyr::select(sample.id, result, analyte, event.type)

# Pivot the data wider
pivoted_data <- filtered_data %>%
  pivot_wider(names_from = analyte, values_from = result)

# drop ID column?
pd2 <- subset(pivoted_data, select = -c(sample.id, event.type))

# Create scatter matrix
pairs(pd2,
      col = 'blue',
      pch = 8,
      main = 'Scatterplot Matrix of All Water Analytes'
      )

# works in console, but not in HTML when knitting
chart.Correlation(pd2,
                  histogram = T,
                  method = 'pearson',
                  pch = 8
                  )
