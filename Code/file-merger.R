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
# Working directory
directory <- file.choose()
# Dictionaries for converting ID codes to names
location.dict = c(
  "BT" = "Berthoud",
  "ASO" = "ARDEC South - Org",
  "ASC" = "ARDEC South -  Conv",
  "A2" = "ARDEC 2200",
  "MOL" = "Molina",
  "GU" = "Gunnison",
  "K" = "Kerbel",
  "ST1" = "Kerbel",
  "ST2" = "Kerbel",
  "CT1" = "Kerbel",
  "CT2" = "Kerbel",
  "MT1" = "Kerbel",
  "MT2" = "Kerbel",
  "INF" = "Kerbel",
  "YJ" = "Yellow Jacket ",
  "UYM" = "Yampa",
  "LG" = "Legacy",
  "AV" = "AVRC STAR",
  "AVST1" = "AVRC STAR",
  "AVST2" = "AVRC STAR",
  "AVCT1" = "AVRC STAR",
  "AVST2" = "AVRC STAR",
  "BAR" = "Barley",
  "HOL" = "Big Hollow",
  "SCI" = "Stage Coach In",
  "SCA" = "Stage Coach Above",
  "SB" = "Stagecoach",
  "TR" = "Todds Ranch ",
  "UYM" = "Upper Yampa")
trt.dict = c(
  "ST1" = "ST1",
  "ST2" = "ST2",
  "CT1" = "CT1",
  "CT2" = "CT2",
  "MT1" = "MT1",
  "MT2" = "MT2",
  "INF" = "Inflow",
  "RVA" = "River A",
  "RVB" = "River B",
  "RVMID" = "River Middle",
  "PZE" = "Piezometer East",
  "PZW" = "Piezometer West",
  "TDR" = "Tile Drainage River",
  "TDL" = "Tile Drainage Lake",
  "CON" = "Confluence",
  "UP" = "Upstream of Bridge",
  "DOWN" = "Downstream of Bridge",
  "MID" = "Middle at Bridge",
  "ANF" = "Arapahoe Natl. Forest",
  "WC" = "Willow Creek",
  "DP" = "Duck Pond",
  "CUL" = "Upper willow at @ culvert (swale)",
  "FP" = "Fish Pond",
  "FR2" = "Fire 2")
method.dict = c(
  "ISC" = "ISCO",
  "LC" = "Low-Cost Sampler",
  "GB" = "Grab Sample")
eventType_dict = c(
  "IN" = "Inflow",
  "OUT" = "Outflow")

importData <- function(directory) {
  # ALS exports data as xls, but it is actually htm, so it requires some cleaning here
  df <- read_html(directory) %>% # read in html file
    html_table() %>% # convert to table
    data.frame() # convert to dataframe
  return(df) # return dataframe
}

cleanData <- function(df) {
  # Clean imported dataframe for merging, graphing, etc.
  df <-df[!grepl("Sample:", df$SAMPLE.ID),] %>% # Drop unnecessary rows containing the word "sample:"
    # mutate_at(c("RESULT","MDL","RL","PERCENT.MOISTURE","PERCENT.SOLID"), as.numeric) # convert numeric columns to numeric if needed
  return(df)
}

processData <- function(df) {
  # Process data to create new columns for analysis based on ID codes
  df$duplicate <- ifelse(grepl("-D", df$SAMPLE.ID, fixed=F), TRUE, FALSE) # create duplicate column
  df$location <- gsub("-.*", "", df$SAMPLE.ID) %>% # create location column
    # TODO: convert location codes to names; see below links 
    # https://stackoverflow.com/questions/7547597/dictionary-style-replace-multiple-items
    # https://stackoverflow.com/questions/1105659/how-to-add-variable-key-value-pair-to-list-object
    mutate(location = recode(location.dict)) # convert ID codes to names
  return(df)
}

listFiles <- function(directory) {
  # import htm files and merge into single df
  df <- list.files(path=directory) %>%
    lapply(read_xls) %>%
    bind_rows
  return(df)
}
