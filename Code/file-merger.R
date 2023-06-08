# ALS to AWQP Data Clean and Merge Tool
# AJ Brown, ansley.brown@colostate.edu
# Started: 13 July 2022
# Updated: 21 Feb 2023

# Tool to clean and merge multiple .htm files directly downloaded
# from the ALS global portal into one master file and create categories for
# further analysis.

# TODO:
# make this script a package for use in future analysis scripts

# Script work flow:
  # 0) Import libraries
  # 1) Define global variables
  # 1a) Working file_path
  # 1b) Set the default file directory
  # 1c) Define dictionaries for interpreting sample ID codes
  # 2) Define functions
  # 2a) map_values <-- a function to map sample ID text to dictionary values
  # 2b) import data
  # 2c) clean data
  # 2d) process data
  # 2e) execute previous functions
  # 2f) repeat for all files in a directory
  # 4) View resulting dataframe(s) for QA/QC
  # 3) Export data as csv

# Example code execution for users
# df_test <- returnSingleFile(path=file_path, export=FALSE)
# df_all <- returnAllFiles(d=directory, export=FALSE)
 

# Then take df_test or df_all and do whatever you want with it (e.g., graph)


# Import libraries
package.list <- c("magrittr",
                  "dplyr",
                  "readr",
                  "readxl",
                  "ggplot2",
                  "lattice",
                  "rvest",
                  "xml2",
                  "stringr",
                  'tidyverse',
                  'lubridate'
                  )
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
 # Working file paths
  # for ALS data
file_path <- "./Data/Webtrieve-10-HS22090451.xls"
tss_file_path <- './TSS/TSS_Master_2023.xlsx'
# file_path <- file.choose()
 # Set the default file directory to the directory containing the selected file
directory <- dirname(file_path)
tss_directory<- dirname(tss_file_path)
 # Dictionaries for interpreting sample ID codes
  # Add to these at needed for new locations, treatments, methods, etc.
location.dict <- c(
  "Berthoud" = "BT",
  "ARDEC South - Org" = "ASO",
  "ARDEC South -  Conv" = "ASC",
  "ARDEC 2200" = "A2",
  "Molina" = "MOL",
  "Gunnison" = "GU",
  "Kerbel" = c("K", "KB", "ST1", "ST2", "CT1", "CT2", "MT1", "MT2", "INF"),
  "Yellow Jacket " = "YJ",
  "Legacy" = "LG",
  "AVRC STAR" = c("AV", "AVST1", "AVST2", "AVCT1", "AVCT2"),
  "Barley" = "BAR",
  "Big Hollow" = "HOL",
  "Stage Coach In" = "SCI",
  "Stage Coach Above" = "SCA",
  "Stagecoach" = "SB",
  "Todds Ranch" = "TR",
  "Upper Yampa" = "UYM",
  "Boulder Lake" = "BOL",
  "Morrison Creek" = "MOR",
  "Below Stagecoach Dam" = "SCO"
  )
trt.dict <- c(
  "ST1" = c("ST1", "AVST1"),
  "ST2" = c("ST2", "AVST2"),
  "CT1" = c("CT1", "AVCT1"),
  "CT2" = c("CT2", "AVCT2"),
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
  "Fire 2" = "FR2"
  )
method.dict <- c(
  "ISCO" = c("ISC", "IN", "OT"),
  "Low-Cost Sampler" = c("LC", "INLC", "OTLC"),
  "Grab Sample" = c("GB", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9")
  )
eventType.dict <- c(
  "Inflow" = c("IN", "INLC", "IN1", "IN2", "IN3", "IN4", "IN5", "IN6", "IN7", 
    "IN8", "IN9"),
  "Outflow" = c("OUT", "OT", "OTLC")
  )

# Define Private Functions (i.e., do not call them directly)
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

importDataXls <- function(file_path) {
  # ALS exports meta data as xls. Very confusing.
   # so we have to import it differently
  df <- read_excel(file_path) %>% # read in html file
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
         non.detect = ifelse(RESULT == 0, TRUE, FALSE),
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
    # create a list of columns for post-processing
  text_cols <- c("location.name",
                   "treatment.name",
                   "method.name",
                   "event.type")
    # create new columns based on ID codes
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
    # remove numbers from new columns due to dict mapping
     # caution: if there are more than 10 dict keys, this will not work
     # note: avoid naming future locations with numbers in the name
    mutate_at(c("location.name",
                "method.name",
                "event.type"), ~ gsub("[0-9]", "", .)) %>%
     # treatment.name needs special treament because of CT/MT/ST 1/2 having #'s
     # TODO: detect number first, then do this, else leave it alone
    mutate_at("treatment.name", ~ substr(., 1, nchar(.) - 1)) %>%
    # if event.type is NA, use "Point Sample" as default
    mutate(event.type = if_else(is.na(event.type), "Point Sample", event.type))
}

flagData <- function(df){
  # function to flag data for QA/QC after merging both htm and xls files
  # check water data for flags such as:
    # H = past hold time
    # J = minimum detection limit (MDL) > value > reporting limit (RL)
    # N = non-EPA method used
    # P = Ortho-P > Total P
    # more?
  # create flag column
  df$flag <- NA
  df %>%
    # search for J values
    mutate(flag = ifelse(RESULT > MDL & RESULT < RL, "J", NA)) %>%
    # identify samples past hold time, based on ALS "HOLD" column
    mutate(flag = ifelse(HOLD == 'Yes', paste0(flag, "H"), flag))
  return(df)
}

executeFxns <- function(file_path) {
  # execute all previous functions and return final dataframe
  df <- importData(file_path) %>%
    cleanData() %>%
    processData()
  return(df)
}
dfTss <- function(tss_fp) {
  df <- read_excel(tss_fp, sheet = "MasterData") %>%
    select(c('Sample_ID', 'Collection_date', 'TSS_mg/L', 'pH', 'EC_mS/cm')) %>%
    rename("SAMPLE.ID" = "Sample_ID",
           "COLLECTED" = "Collection_date",
           "TSS" = "TSS_mg/L",
           "EC" = "EC_mS/cm") %>%
    filter(!(SAMPLE.ID %in% c("Stock Solution", "DI"))) %>%
    na.omit() %>%
    mutate(
      duplicate = ifelse(grepl("-D", SAMPLE.ID, fixed = FALSE), TRUE, FALSE),
      location.name = sapply(SAMPLE.ID, function(x) map_values(x, location.dict)),
      treatment.name = sapply(SAMPLE.ID, function(x) map_values(x, trt.dict)),
      method.name = sapply(SAMPLE.ID, function(x) map_values(x, method.dict)),
      event.type = sapply(SAMPLE.ID, function(x) map_values(x, eventType.dict))
    ) %>%
    gather(key = "ANALYTE", value = "RESULT", c(pH, TSS, EC )) %>%
    mutate(UNITS = ifelse(ANALYTE == "TSS", "mg/L",
                          ifelse(ANALYTE == "pH", "pH",
                                 ifelse(ANALYTE == "EC", "mS/cm", "unknown")))) %>%
    mutate_at(c("location.name", "method.name", "event.type"), ~ gsub("[0-9]", "", .)) %>%
    mutate_at("treatment.name", ~ substr(., 1, nchar(.) - 1)) %>%
    mutate(event.type = if_else(is.na(event.type), "Point Sample", event.type)) %>%
    mutate(COLLECTED = as.character(COLLECTED)) %>%
     mutate(RESULT = as.numeric(RESULT))
  
  return(df)
}

mergeFiles <- function(directory, tss_fp) {
  # import all htm files in the directory, merge, and return df
  print("Merging files...")
  file_list <- list.files(path = directory,
                          pattern = "*.xls", 
                          full.names = TRUE)
  print("Data files to be merged:")
  data_files <- file_list[!grepl("-Samples", file_list)]
  print(data_files)
  print("Metadata files to be merged:")
  meta_files <- file_list[grepl("-Samples", file_list)]
  print(meta_files)
  # merge data files
  df_data <- data_files %>%
    # pair and merge files here
    lapply(executeFxns) %>%
    bind_rows
  # merge metadata files
  df_meta <- meta_files %>%
    # pair and merge files here
    lapply(importDataXls) %>%
    bind_rows
  # merge data and metadata
  df_merge <- df_data %>%
    left_join(df_meta, by = "SAMPLE.ID") %>%
    flagData()
  # import TSS data to df w/ metadata
  df_tss <- dfTss(tss_fp)
  # merge tss data with als data
  df <- bind_rows(df_merge, df_tss)
  #convert to date
  df$COLLECTED <- parse_date_time(df$COLLECTED, orders = c("ymd", "dby HM"))
  #remove time so TSS matches with ALS
  df$COLLECTED <- as.Date(df$COLLECTED)
  return(df)
  
}

# Define public functions (i.e., to be called by user)
returnSingleFile <- function(path = file_path, export = FALSE) {
  # return and optionally export a single file for QA/QC
  df <- executeFxns(path)
  if (export == TRUE) {
    write.csv(df, file = "single_file.csv", row.names = FALSE)
  }
  return(df)
}

returnAllFiles <- function(d = directory, tss_fp = tss_file_path, export = TRUE) {
  # return and optionally export all files for QA/QC
  df <- mergeFiles(d, tss_fp)
  # for debugging only; uncomment as necessary
  #View(df)
  if (export == TRUE) {
    write.csv(df, file = "all_files.csv", row.names = FALSE)
  }
  return(df)
}



