# ALS to AWQP Data Clean and Merge Tool
# AJ Brown and Caz Bell
# Started: 7/13/2022
# First working version: 20 Feb 2023

# Tool to clean and merge multiple htm files directly downloaded
# from the ALS global portal into one and create categories for
# future analysis by AWQP staff.

# Script process flow:
# 1) Define global variables
 # 1a) Working file_path
 # 1b) Set the default file directory
 # 1c) Define dictionaries for interpreting sample ID codes
# 2) Import data
# 3) Clean data
# 4) Process data
 # this will involve the creation of dictionaries to convert codes to names
 # it will also create new identifying columns based on the dictionaries
# 5) Repeat for each htm file desired and merge into one df
# 6) Export data as csv

# Import libraries
package.list <- c("magrittr",
                  "dplyr",
                  "readr",
                  "readxl",
                  "ggplot2",
                  "lattice",
                  "rvest",
                  "xml2",
                  "stringr")
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
 # Working file_path
file_path <- file.choose()
 # Set the default file directory to the directory containing the selected file
directory <- dirname(file_path)
 # Dictionaries for interpreting sample ID codes
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
  "ST1" = C("ST1", "AVST1"),
  "ST2" = C("ST2", "AVST2"),
  "CT1" = C("CT1", "AVCT1"),
  "CT2" = C("CT2", "AVCT2"),
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
  "ISCO" = c("ISC", "IN", "OT"),
  "Low-Cost Sampler" = c("LC", "LCIN", "LCOT"),
  "Grab Sample" = c("GB", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9"))
eventType.dict <- c(
  "Inflow" = "IN",
  "Outflow" = c("OUT", "OT"))

# Functions
map_values <- function(text, dict) {
 # function to map sample ID text to dictionary values
  # Split the text by spaces
  text_values <- unlist(strsplit(text, "-"))

  # Map each value to the corresponding dictionary value
  mapped_values <- lapply(text_values, function(x) {
    for (key in names(dict)) {
      if (x %in% dict[[key]]) {
        return(key)
      }
    }
    return(NA)
  })

  # Combine the mapped values into a single vector
  combined_values <- unlist(mapped_values)

  # Remove any NAs
  combined_values <- combined_values[!is.na(combined_values)]

  # Return the first combined value (or NA if there are no values)
  if (length(combined_values) > 0) {
    return(combined_values[1])
  } else {
    return(NA)
  }
}

importData <- function(file_path) {
  # ALS exports data as xls, but it is actually htm,
   # so it requires some cleaning here.
  df <- read_html(file_path) %>% # read in html file
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
  df %>%
    mutate(
      # create duplicate column
      duplicate = ifelse(grepl("-D", SAMPLE.ID, fixed = FALSE), TRUE, FALSE),
      # create location name column based on Sample ID
      location.name = sapply(SAMPLE.ID, 
                             function(x) map_values(x, location.dict)),
      # create treatment name column based on Sample ID
      treatment.name = sapply(SAMPLE.ID, function(x) map_values(x, trt.dict)),
      # create method name column based on Sample ID
      method.name = sapply(SAMPLE.ID, function(x) map_values(x, method.dict)),
      # create event type name column based on Sample ID
      event.type = sapply(SAMPLE.ID, function(x) map_values(x, eventType.dict))
    ) %>%
    # if event type is NA, use "Point Sample" as default
    mutate(y = if_else(is.na(y), "Point Sample", y))
}

executeFxns <- function(file_path) {
  # execute all previous functions and return final dataframe
  df <- importData(file_path) %>%
    cleanData() %>%
    processData()
  return(df)
}

listFiles <- function(directory) {
  # import htm files and merge into single df
  print(list.files(path = directory, pattern = "*.xls", full.names = TRUE))
  df <- list.files(path = directory, pattern = "*.xls", full.names = TRUE) %>%
    lapply(executeFxns) %>%
    bind_rows
  return(df)
}

# Execute functions and view resulting dataframe:
 # For a single file
df_final <- executeFxns(directory) # works
View(df_final)
 # For multiple files in a given directory
df_final_merged <- listFiles(directory)
View(df_final_merged)

# Export the combined data frame as a CSV file
write.csv(df_final_merged, file = "combined_data.csv", row.names = FALSE)
 # Print a confirmation message
cat("Combined data exported as 'combined_data.csv'.\n")
