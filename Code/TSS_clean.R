#TSS Data cleaning to merge
#Caz Bell
#Created 6/2/2023

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

file_path <- "./TSS/TSS_Master_2023.xlsx"


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
  "Yampa" = "UYM",
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

#read in the TSS Excel file

TSS <- read_excel(file_path, sheet = "MasterData") %>%
  #select what rows are needed
  select(c('Sample_ID', 'Collection_date', 'TSS_mg/L')) %>%
  #add units column
  mutate(units = rep('mg/L')) %>%
  #ad the analyte column
  mutate(analyte = rep('TSS')) %>% 
  #rename so it matches with the ALS data
  rename("sample.id" = "Sample_ID",
         "collected" = "Collection_date",
         "result" = "TSS_mg/L") %>%
  #filter out standards
  filter(!(sample.id %in% c("Stock Solution", "DI"))) %>%
  na.omit() %>%
  mutate(
    # create duplicate column
    duplicate = ifelse(grepl("-D", sample.id, fixed = FALSE), TRUE, FALSE),
    # create location name column based on Sample ID
    location.name = sapply(sample.id,
                           function(x) map_values(x, location.dict)),
    # create treatment name column based on Sample ID
    treatment.name = sapply(sample.id, function(x) map_values(x, trt.dict)),
    # create method name column based on Sample ID
    method.name = sapply(sample.id, function(x) map_values(x, method.dict)),
    # create event type name column based on Sample ID
    event.type = sapply(sample.id, function(x) map_values(x, eventType.dict))
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
