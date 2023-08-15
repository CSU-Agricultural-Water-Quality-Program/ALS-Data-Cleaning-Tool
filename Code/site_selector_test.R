library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)

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

create_scatterplot_matrix(dat, loc)


