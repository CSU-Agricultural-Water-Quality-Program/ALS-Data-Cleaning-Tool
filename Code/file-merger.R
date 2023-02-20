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
                  "xml2")
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
  "Inflow" = "INF",
  "River A" = "RVA",
  "River B" = "RVB",
  "River Middle" = "RVMID",
  "Piezometer East" = "PZE",
  "Piezometer West" = "PZW",
  "Tile Drainage River" = "TDR",
  "Tile Drainage Lake" = "TDL",
  "Confluence" = "CON",
  "Upstream of Bridge" = "UP",
  "Downstream of Bridge" = "DOWN",
  "Middle at Bridge" = "MID",
  "Arapahoe Natl. Forest" = "ANF",
  "Willow Creek" = "WC",
  "Duck Pond" = "DP",
  "Upper willow at @ culvert (swale)" = "CUL",
  "Fish Pond" = "FP",
  "Fire 2" = "FR2")
method.dict <- c(
  "ISCO" = "ISC",
  "Low-Cost Sampler" = "LC",
  "Grab Sample" = "GB")
eventType.dict <- c(
  "Inflow" = "IN",
  "Outflow" = "OUT")

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
    # convert values containing "<" to 0
    mutate(RESULT = ifelse(grepl("<", RESULT), 0, RESULT),
         # create column to indicate if a result value was a non-detect
         non_detect = ifelse(RESULT == 0, TRUE, FALSE),
         # change "N/A" to NA in any column
         across(everything(), ~ ifelse(. == "N/A", NA, .))) %>%
    # convert select columns to numeric if needed
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

# temporary code for testing
df3 <- importData(directory)
df2 <- cleanData(df3)
df <- processData(df2)
df_final <- executeFxns(directory)


listFiles <- function(directory) {
  # import htm files and merge into single df
  df <- list.files(path=directory) %>%
    lapply(read_xls) %>%
    bind_rows
  return(df)
}
