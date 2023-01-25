# ALS to AWQP Data Clean and Merge Tool
# AJ Brown and Caz Bell
# 7/13/2022
# Updated 22 Jan 2023

# TODO: finish process_data function
# TODO: finish listFiles function
# TODO: test script on multiple files

# Tool to clean and merge multiple htm files directly downloaded
# from the ALS global portal into one and create categories for 
# future analysis by AWQP staff.

# Script process flow:
# 1) Import data
# 2) Clean data
# 3) Process data
  # this will involve the creation of dictionaries to convert codes to names
  # it will also create new identifying columns based on the dictionaries
# 4) Repeat for each htm file desired and merge into one df
# 5) Graph and analyze data as desired

# Import libraries
package.list <- c("magrittr", "dplyr", "readr", "readxl", "ggplot2", "lattice", "rvest", "xml2")
packageLoad <- function(packages){
  for (i in packages) {
    if (!require(i, character.only = TRUE)) {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}
packageLoad(package.list)

# Global Variables
directory <- file.choose()

import_data <- function(directory) {
  # ALS exports data as xls, but it is actually htm, so it requires some cleaning here
  df <- read_html(directory) %>% # read in html file
    html_table() %>% # convert to table
    data.frame() # convert to dataframe
  return(df) # return dataframe
}

clean_data <- function(df) {
  # Clean imported dataframe for merging, graphing, etc.
  df <-df[!grepl("Sample:", df$Name),] %>% # Drop unnecessary rows containing the word "sample:"
    mutate_at(c('RESULT','MDL','RL','PERCENT.MOISTURE','PERCENT.SOLID'), as.numeric) # convert numeric columns to numeric
  return(df)
}

process_data <- function(df) {
  location.dict = c(
    'BT' = 'Berthoud',
    'ASO' = 'ARDEC South - Org',
    'ASC' = 'ARDEC South -  Conv',
    'A2' = 'ARDEC 2200',
    'MOL' = 'Molina',
    'GU' = 'Gunnison',
    'K' = 'Kerbel',
    'YJ' = 'Yellow Jacket ',
    'UYM' = 'Yampa',
    'LG' = 'Legacy',
    'AV' = 'AVRC Star',
    'BAR' = 'Barley',
    'HOL' = 'Big Hollow',
    'SCI' = 'Stage Coach In',
    'SCA' = 'Stage Coach Above',
    'SB' = 'Stagecoach',
    'TR' = 'Todds Ranch ',
    'UYM' = 'Upper Yampa')
  trt.dict = c(
    'ST1' = 'ST1',
    'ST2' = 'ST2',
    'CT1' = 'CT1',
    'CT2' = 'CT2',
    'MT1' = 'MT1',
    'MT2' = 'MT2',
    'INF' = 'Inflow',
    'RVA' = 'River A',
    'RVB' = 'River B',
    'RVMID' = 'River Middle',
    'PZE' = 'Piezometer East',
    'PZW' = 'Piezometer West',
    'TDR' = 'Tile Drainage River',
    'TDL' = 'Tile Drainage Lake',
    'CON' = 'Confluence',
    'UP' = 'Upstream of Bridge',
    'DOWN' = 'Downstream of Bridge',
    'MID' = 'Middle at Bridge',
    'ANF' = 'Arapahoe Natl. Forest',
    'WC' = 'Willow Creek',
    'DP' = 'Duck Pond',
    'CUL' = 'Upper willow at @ culvert (swale)',
    'FP' = 'Fish Pond',
    'FR2' = 'Fire 2')
  method.dict = c(
    'ISC' = 'ISCO',
    'LC' = 'Low-Cost Sampler',
    'GB' = 'Grab Sample')
  eventType.dict = c(
    'IN' = 'Inflow',
    'OUT' = 'Outflow')
}

listFiles <- function(directory) {
  # import htm files and merge into single df
  df <- list.files(path=directory) %>%
    lapply(read_xls) %>%
    bind_rows
  return(df)
}
