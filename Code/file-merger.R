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
package.list <- c("magrittr",
                  "dplyr",
                  "readr",
                  "readxl",
                  "ggplot2",
                  "lattice",
                  "rvest",
                  "xml2",
                  "purrr")
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
location.dict <- list(
  "Berthoud" = "BT",
  "ARDEC South - Org" = "ASO",
  "ARDEC South -  Conv" = "ASC",
  "ARDEC 2200" = "A2",
  "Molina" = "MOL",
  "Gunnison" = "GU",
  "Kerbel" = c("K", "ST1", "ST2", "CT1", "CT2", "MT1", "MT2", "INF"),
  "Yellow Jacket " = "YJ",
  "Yampa" = "UYM",
  "Legacy" = "LG",
  "AVRC STAR" = c("AV", "AVST1", "AVST2", "AVCT1", "AVCT2"),
  "Barley" = "BAR",
  "Big Hollow" = "HOL",
  "Stage Coach In" = "SCI",
  "Stage Coach Above" = "SCA",
  "Stagecoach" = "SB",
  "Todds Ranch" = "TR",
  "Upper Yampa" = "UYM")
trt.dict <- c(
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
method.dict <- c(
  "ISC" = "ISCO",
  "LC" = "Low-Cost Sampler",
  "GB" = "Grab Sample")
eventType.dict <- c(
  "IN" = "Inflow",
  "OUT" = "Outflow")

reverseList <- function(list) {
  # Reverse the key pairs order of a list
  rev.list <- with(stack(location.dict), split(as.character(ind), values))
  return(rev.list)
}

importData <- function(directory) {
  # ALS exports data as xls, but it is actually htm,
   # so it requires some cleaning here.
  df <- read_html(directory) %>% # read in html file
    html_table() %>% # convert to table
    data.frame() # convert to dataframe
  return(df) # return dataframe
}

cleanData <- function(df) {
  # Clean imported dataframe for merging, graphing, etc.
   # Drop unnecessary rows containing the word "sample:"
  df <- df[!grepl("Sample:", df$SAMPLE.ID),] %>% 
    # convert numeric columns to numeric if needed
    mutate_at(c("RESULT",
                "DILUTION",
                "MDL",
                "RL",
                "PERCENT.MOISTURE",
                "PERCENT.SOLID"),
               as.numeric)
  return(df)
}

processData <- function(df) {
  # Process data to create new columns for analysis based on ID codes
   # create duplicate column
  df$duplicate <- ifelse(grepl("-D", df$SAMPLE.ID, fixed = FALSE), TRUE, FALSE)
   # create location ID column
  df$location.id <- gsub("-.*", "", df$SAMPLE.ID)
   # convert ID codes to names
  vec <- data.frame(location.name = unlist(lapply(df2$location.id, 
                    FUN = function(x){reverseList(location.dict)[[x]]})))
  df <- cbind(df, vec)
  #TODO: finish this function with the rest of the dictionaries
  return(df)
}

executeFxns <- function(directory) {
  # execute all previous functions and return final dataframe
  df <- importData(directory) %>%
    cleanData() %>%
    processData()
  return(df)
}

listFiles <- function(directory) {
  # import htm files and merge into single df
  df <- list.files(path=directory) %>%
    lapply(read_xls) %>%
    bind_rows
  return(df)
}
