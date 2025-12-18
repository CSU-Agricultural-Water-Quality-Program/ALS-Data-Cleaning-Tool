# ALS to AWQP Data Clean and Merge Tool
# AJ Brown, ansley.brown@colostate.edu
# Started: 13 July 2022
# Updated: 21 Feb 2023
#test
# Tool to clean and merge multiple .htm files directly downloaded
# from the ALS global portal into one master file and create categories for
# further analysis.

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

# for checking one site/trt only:
# filtered_df <- df_all %>% filter(location.name == 'Stagecoach')
# Then take df_test, df_all, or filtered_df and do whatever you want with it (e.g., graph)


# Import libraries
package.list <- c("magrittr",
                  "dplyr",
                  "readr",
                  "readxl",
                  "ggplot2",
                  "lattice",
                  "rvest",
                  "xml2",
                  'tidyverse',
                  'lubridate',
                  'stringr'
)
packageLoad <- function(packages){
  for (i in packages) {
    if (!require(i, character.only = TRUE)) {
      install.packages(i, repos = "http://cran.us.r-project.org")
      library(i, character.only = TRUE)
    }
  }
}
packageLoad(package.list)

# Global Variables
# source file paths from config.r
source('./Code/config.R')


# Dictionaries for interpreting sample ID codes
# Add to these at needed for new locations, treatments, methods, etc.
location.dict <- list(
  "ARDEC" = "A2", #TODO: fix 2200 labeling in process data fxn; 2200 is removed
  "ARDEC South - Conv" = "ASC",
  "ARDEC South - Org" = "ASO",
  "AVRC STAR" = c("AV", "AVST1", "AVST2", "AVCT1", "AVCT2"),
  "Barley" = "BAR",
  "Berthoud" = "BT",
  "Big Hollow" = "HOL",
  "Boulder Lake" = "BOL",
  "Gunnison" = "GU",
  "Kerbel" = c("K", "KBI", "ST1", "ST2", "CT1", "CT2", "MT1", "MT2", "INF"),
  "Legacy" = "LG",
  "Molina" = "MOL",
  "Stagecoach" = c("SC", "SB", "SCA", "SB-SCA", "SCI", "SB-SCI", "SCO", "SB-SCO", "MOR", "SB-MOR", "TR", "SB-TR"),
  # "Morrison Creek" = c("MOR","SB-MOR"),
  # "Stage Coach Above" = c("SCA","SB-SCA"),
  # "Stage Coach In" = c("SCI", "SB-SCI"),
  # "Stage Coach Dam Outflow" = c("SCO","SB-SCO"),
  # "The Ranch" = c("TR","SB-TR"), # Formerly, "Todd's Ranch"
  "Upper Yampa" = "UYM",
  "Yellow Jacket " = "YJ",
  "Fruita W" = c("W1", "W2", "FW", "FW1", "FW2"),
  "Fruita B" = c("FB", "FBR", "F-BR", "F-B", "BR"),
  "Fruita NT" = "FNT",
  "Fruita A" = c("FA", "FALF", "F-ALF", "ALF"),
  "Fruita C" = c("FC", "FC1", "FC2", "C1", "C2"),
  "Fruita F" = c("FF", "FF1", "FF2", "FF3", "FF4", "F1", "F2", "F3", "F4"),
  "AVRC Cowpea" = c("COW", "T1", "T2", "T3", "T4"),
  "North Sand Creek" = c("NSC", "J", "G", "F"),
  "Lab Blank" = "BK",
  "Method Blank" = "Method Blank",
  "Lab Control Sample" = "Lab Control Sample"
)

trt.dict <- list(
  #note that leaving AVRC and Kerbel as CT/MT/ST breaks the GPS coordinate locator b/c it needs the block# in the trt name to find it (e.g., CT2)
  #I've opted to let this be the case for now, but it could be fixed by adding the block# to the treatment.name here if needed.
  "ST" = c("ST1", "AVST1", "ST2", "AVST2"),
  "CT" = c("CT1", "AVCT1", "CT2", "AVCT2"),
  "MT" = c("MT1", "MT2"),
  "River A" = "RVA",
  "River B" = "RVB",
  "River Middle" = "RVMID",
  "Piezometer East" = "PZE",
  "Piezometer West" = "PZW",
  "Piezometer North" = "PZN",
  "Piezometer South" = "PZS",
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
  "Fire 2" = "FR2",
  "Stagecoach ISCO" = c("SC", "SB"),
  "Morrison Creek" = c("MOR","SB-MOR"),
  "Stagecoach Above" = c("SCA","SB-SCA"),
  "Stagecoach In" = c("SCI", "SB-SCI"),
  "Stagecoach Dam Outflow" = c("SCO","SB-SCO"),
  "The Ranch" = c("TR","SB-TR"), # Formerly, "Todd's Ranch"
  "W1" = c("W1", "FW1"),
  "W2" = c("W2", "FW2"),
  "C1" = c("C1", "FC1"),
  "C2" = c("C2", "FC2"),
  "Fertilizer Treatment 1" =c("F1", "FF1"),
  "Fertilizer Treatment 2" =c("F2", "FF2"),
  "Fertilizer Treatment 3" =c("F3", "FF3"),
  "Fertilizer Treatment 4" =c("F4", "FF4"),
  "Cowpea Treatment 1" = c("T1"),
  "Cowpea Treatment 2" = c("T2"),
  "Cowpea Treatment 3" = c("T3"),
  "Cowpea Treatment 4" = c("T4"),
  "NSC Site J" = c("J"),
  "NSC Site G" = c("G"),
  "NSC Site F" = c("F")
)

method.dict <- list(
  "ISCO" = c("ISC"), # formerly had "IN", "OT", "0T" here, but I think it messed up labels
  "Low-Cost Sampler" = c("LC", "INLC", "OTLC"),
  "Grab Sample" = c("GB", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9"),
  "Hourly Grab" = c("GBH"),
  "Lab Blank" = c("BK"),
  "Method Blank" = c("Method Blank"),
  "Lab Control Sample" = c("Lab Control Sample")
)

eventType.dict <- list(
  "Inflow" = c("IN", "INF", "INLC", "IN1", "IN2", "IN3", "IN4", "IN5", "IN6", 
               "IN7", "IN8", "IN9", "Inflow"),
  "Outflow" = c("OUT", "OT", "OTLC", "ST1", "ST2", "CT1", "CT2", "MT1", "MT2",
                "0T"),
  "Lab Blank" = c("BK"),
  "Method Blank" = c("Method Blank"),
  "Lab Control Sample" = c("Lab Control Sample")
)

eventCount.dict <- list(
  "Irrigation 1" = c("01"),
  "Irrigation 2" = c("02"),
  "Irrigation 3" = c("03"),
  "Irrigation 4" = c("04"),
  "Irrigation 5" = c("05"),
  "Irrigation 6" = c("06"),
  "Irrigation 7" = c("07"),
  "Irrigation 8" = c("08"),
  "Storm 1" = c("S1"),
  "Storm 2" = c("S2"),
  "Storm 3" = c("S3"),
  "Storm 4" = c("S4"),
  "Storm 5" = c("S5")
)

tssUnits.dict <- c(
  "TSS" = "mg/L",
  "Suspended Solids (Residue, Non-Filterable)" = "mg/L",
  "EC" = "dS/cm",
  "EC25" = "dS/cm",
  "Specific Conductance" = "dS/cm",
  "pH" = "pH"
)

#untested analyte dict update - but worked in the load-calc-experiment script
analyteAbbr.dict <- list(
  "TKN"   = c("Nitrogen, Total Kjeldahl"),
  "NO2_N" = c("Nitrogen, Nitrite  (As N)", "NITRITE AS N"),
  "PO4_P" = c("Phosphorus, Total Orthophosphate (as P)", "ORTHOPHOSPHATE AS P"),
  "TP"    = c("Phosphorus, Total (As P)", "TOTAL PHOSPHORUS"),
  "TDS"   = c("Total Dissolved Solids (Residue, Filterable)"),
  "NO3_N" = c("Nitrogen, Nitrate (As N)", "NITRATE AS N"),
  "TSS"   = c("Suspended Solids (Residue, Non-Filterable)", "TSS"),
  "Fe"    = c("Iron, Total"),
  "Se"    = c("Selenium, Total", "SELENIUM"),
  "pH"    = c("pH", "POTENTIAL HYDROGEN"),
  "EC25"  = c("Specific Conductance", "ELECTRICAL CONDUCTIVITY")
)



# copy/paste excel data below to create geodata dataframe (i.e., separated by tabs, \t)
geo_key <- read.csv(text = "
location.name	treatment.name	event.type	long	lat
ARDEC 2200		Outflow	-104.9869329	40.65356941
ARDEC 2200		Inflow	-104.9888983	40.65361825
AVRC STAR	CT1	Point Sample	-103.689906	38.03957397
AVRC STAR	ST1	Point Sample	-103.6898805	38.03982154
AVRC STAR	ST2	Point Sample	-103.68987	38.04005803
AVRC STAR	CT2	Point Sample	-103.6898893	38.04030463
Barley		Outflow	-105.0167433	40.35369119
Below Stagecoach Dam		Point Sample	-106.8290859	40.28655773
Berthoud		Outflow	-105.08495	40.2818
Berthoud		Inflow	-105.0855686	40.28104111
Berthoud	River A	Point Sample	-105.09267	40.27863
Berthoud	River Middle	Point Sample	-105.08875	40.27962
Berthoud	River B	Point Sample	-105.08402	40.28317
Berthoud	Tile Drainage Lake	Point Sample	-105.089151	40.27926715
Berthoud	Tile Drainage River	Point Sample	-105.089	40.27928
Berthoud	Piezometer East	Point Sample	-105.0847844	40.28175382
Berthoud 	Piezometer West	Point Sample	-105.0849372	40.28170676
Big Hollow	Confluence	Point Sample	-105.018	40.29267
Big Hollow	Downstream of Bridge	Point Sample	-105.02235	40.28622
Big Hollow	Middle at Bridge	Point Sample	-105.02345	40.28388
Big Hollow	Upstream of Bridge	Point Sample	-105.03335	40.27663
Fire 1		Point Sample	-105.995761	40.152369
Fire 2		Point Sample	-105.987369	40.157551
Fire 3		Point Sample	-105.990499	40.154385
Gunnison		Outflow	-106.8269323	38.52626188
Gunnison		Inflow	-106.8160882	38.52338862
Kerbel	CT	Outflow	-104.99828	40.67641
Kerbel	CT	Outflow	-104.99666	40.67637
Kerbel	ST	Outflow	-104.99764	40.67641
Kerbel	ST	Outflow	-104.99701	40.67636
Kerbel	MT	Outflow	-104.99798	40.67641
Kerbel	MT	Outflow	-104.99735	40.67638
Kerbel	Inflow	Inflow	-104.997502	40.679262
Legacy		Outflow	-106.8205175	40.43192773
Molina		Inflow	-108.04037	39.163825
Molina		Outflow	-108.04204	39.17067
Stagecoach	Morrison Creek	Point Sample	-106.8158776	40.28942095
Stagecoach	Stage Coach Above	Point Sample	-106.8812387	40.2691647
Stagecoach	Stage Coach In	Point Sample	-106.8495245	40.28580534
Stagecoach	Stagecoach ISCO	Outflow	-106.825602	40.29226711
Stagecoach	Stage Coach Dam Outflow	Point Sample	-106.8290859	40.28655773
The Ranch		Point Sample	-106.8150173	40.29174667
Upper Yampa		Outflow	-106.922182	40.20191997
Upper Yampa		Inflow	-106.91579	40.19042
Upper Yampa	Piezometer North	Point Sample	-106.92215	40.20168
Upper Yampa	Piezometer South 	Point Sample	-106.92193	40.20131
Fruita Water		Inflow	-108.6919781	39.18870659
Fruita Water	W1	Outflow	-108.6945949	39.18915138
Fruita Water	W2	Outflow	-108.6946281	39.18913084
Fruita Beer		Inflow	-108.6592228	39.17582245
Fruita Beer		Outflow	-108.663471	39.17578835
Fruita No Till		Inflow	-108.7285248	39.24897763
Fruita No Till		Outflow	-108.7266562	39.24607514
Fruita Alfalfa		Inflow	-108.7801719	39.22423003
Fruita Alfalfa		Outflow	-108.7800218	39.22234458
Fruita Chicken		Inflow	-108.673517	39.167938
Fruita Chicken	C1	Outflow	-108.677814	39.165480
Fruita Chicken	C2	Outflow	-108.677830	39.164432
Fruita Fertilizer		Inflow	-108.67325	39.19308
Fruita Fertilizer   F1  Outflow	-108.68052	39.18974
Fruita Fertilizer   F2  Outflow	-108.68052	39.18974
Fruita Fertilizer   F3  Outflow	-108.68052	39.18974
Fruita Fertilizer   F4  Outflow	-108.68052	39.18974
AVRC Cowpea		Inflow	TBD	TBD	
AVRC Cowpea	  T1	Outflow TBD TBD	
AVRC Cowpea	  T2	Outflow	TBD	TBD
AVRC Cowpea	  T3	Outflow	TBD	TBD	
AVRC Cowpea	  T4	Outflow	TBD	TBD	
", sep = '\t', header = TRUE, stringsAsFactors = FALSE)

# ---- Helper Functions ----

map_values_analyte <- function(text, dict) {
  # Ensure text is treated as a character string
  text <- as.character(text)
  
  # Loop through each key in the dictionary
  for (key in names(dict)) {
    # Check each value associated with the current key
    for (value in dict[[key]]) {
      # Use fixed = TRUE for exact substring matching (without regex)
      # TODO: fix grepl below; identifies 'ISC' as Stageocach erroneously
      if (grepl(value, text, fixed = TRUE)) {
        return(key)  # Return the key if a match is found
      }
    }
  }
  
  return(NA)  # Return NA if no match is found
}

map_values <- function(text, dict) {
  # Ensure text is treated as a character string
  text <- as.character(text)
  
  # Loop through each key in the dictionary
  for (key in names(dict)) {
    # Check each value associated with the current key
    for (value in dict[[key]]) {
      # Use grepl with word boundaries to match whole words
      if (grepl(paste0("\\b", value, "\\b"), text)) {
        return(key)  # Return the key if a match is found
      }
    }
  }
  
  return(NA)  # Return NA if no match is found
}

standardize_als_column_names <- function(df) {
  # Upper-case and replace non-alnum with dots to normalize
  std_names <- toupper(gsub("[^A-Za-z0-9]+", ".", names(df)))
  names(df) <- std_names
  
  # Known synonym map -> canonical
  alias_map <- c(
    "SAMPLE.ID"          = "SAMPLE.ID",
    "SAMPLE.ID."         = "SAMPLE.ID",
    "SAMPLEID"           = "SAMPLE.ID",
    "SAMPLE"             = "SAMPLE.ID",
    "SAMPLE.ID.NUMBER"   = "SAMPLE.ID",
    "SAMPLE.ID.CODE"     = "SAMPLE.ID",
    "SAMPLE.ID1"         = "SAMPLE.ID",
    "SAMPLE.ID2"         = "SAMPLE.ID",
    "SAMPLE.ID3"         = "SAMPLE.ID",
    "SAMPLE.ID4"         = "SAMPLE.ID",
    "SAMPLE.ID5"         = "SAMPLE.ID",
    "SAMPLE.ID6"         = "SAMPLE.ID",
    "SAMPLE.ID7"         = "SAMPLE.ID",
    "SAMPLE.ID8"         = "SAMPLE.ID",
    "SAMPLE.ID9"         = "SAMPLE.ID",
    "SAMPLE.ID10"        = "SAMPLE.ID",
    "SAMPLE.ID11"        = "SAMPLE.ID",
    "SAMPLE.ID12"        = "SAMPLE.ID",
    "SAMPLE.ID13"        = "SAMPLE.ID",
    "SAMPLE.ID14"        = "SAMPLE.ID",
    "SAMPLE.ID15"        = "SAMPLE.ID",
    "SAMPLE.ID16"        = "SAMPLE.ID",
    "SAMPLE.ID17"        = "SAMPLE.ID",
    "SAMPLE.ID18"        = "SAMPLE.ID",
    "SAMPLE.ID19"        = "SAMPLE.ID",
    "SAMPLE.ID20"        = "SAMPLE.ID",
    "SAMPLE.ID21"        = "SAMPLE.ID",
    "SAMPLE.ID22"        = "SAMPLE.ID",
    "SAMPLE.ID23"        = "SAMPLE.ID",
    "SAMPLE.ID24"        = "SAMPLE.ID",
    "SAMPLE.ID25"        = "SAMPLE.ID",
    "SAMPLE.ID26"        = "SAMPLE.ID",
    "SAMPLE.ID27"        = "SAMPLE.ID",
    "SAMPLE.ID28"        = "SAMPLE.ID",
    "SAMPLE.ID29"        = "SAMPLE.ID",
    "SAMPLE.ID30"        = "SAMPLE.ID",
    
    "SAMPLE.ID."         = "SAMPLE.ID",
    "SAMPLE.ID.."        = "SAMPLE.ID",
    "SAMPLE.ID..."       = "SAMPLE.ID",
    "SAMPLE.ID...."      = "SAMPLE.ID",
    
    "RESULT"             = "RESULT",
    "RESULT.VALUE"       = "RESULT",
    "VALUE"              = "RESULT",
    "CONCENTRATION"      = "RESULT",
    "MEASURED.RESULT"    = "RESULT",
    
    "RESULT.REPORTED.TO" = "RESULT.REPORTED.TO",
    "REPORTING.LIMIT"    = "RL",
    "RL"                 = "RL",
    "DETECTION.LIMIT"    = "MDL",
    "MDL"                = "MDL",
    "DILUTION.FACTOR"    = "DILUTION",
    "DILUTION"           = "DILUTION",
    "UNITS"              = "UNITS",
    "UNIT"               = "UNITS",
    "ANALYTE"            = "ANALYTE",
    "COMPONENT"          = "ANALYTE"
  )
  
  # Apply alias remapping where present
  names(df) <- dplyr::recode(names(df), !!!alias_map, .default = names(df))
  
  df
}

importData <- function(file_path) {
  # ALS exports results data as htm. Very confusing.
  tabs <- xml2::read_html(file_path) %>% rvest::html_table(fill = TRUE)
  if (length(tabs) == 0) stop("No tables found in HTML/XLS: ", file_path)
  
  # Standardize names per table, then pick a table that has both SAMPLE.ID & RESULT
  std_tabs <- lapply(tabs, standardize_als_column_names)
  
  # 1) prefer tables with BOTH SAMPLE.ID and RESULT
  has_both <- purrr::keep(std_tabs, ~ all(c("SAMPLE.ID","RESULT") %in% names(.x)))
  if (length(has_both) > 0) return(as.data.frame(has_both[[1]]))
  
  # 2) next-best: has RESULT (we will fail later if SAMPLE.ID truly absent)
  has_result <- purrr::keep(std_tabs, ~ "RESULT" %in% names(.x))
  if (length(has_result) > 0) return(as.data.frame(has_result[[1]]))
  
  # 3) fallback: widest table
  pick <- which.max(purrr::map_int(std_tabs, ncol))
  as.data.frame(std_tabs[[pick]])
}

importDataXls <- function(file_path) {
  # ALS exports meta data as xls. Very confusing.
  # so we have to import it differently
  df <- readxl::read_excel(file_path) %>% data.frame()
  df <- standardize_als_column_names(df)
  return(df) # return dataframe
}

importDataKelso <- function(file_path) {
  # Selenium testing is done by the Kelso lab and is a csv file
  # read in csv file
  df <- read_csv(file_path, show_col_types = FALSE) %>% 
    # create columns to be congruent with houston data
    # TODO: check if we actually need to add these NA cols to make it work, b/c we remove them later anyway in executeFxns
    mutate(METHOD =paste0(`Extraction Method`, sep = "-", Method),
           LAB.ID = NA,
           CAS.NUMBER = NA,
           RESULT.REPORTED.TO = NA,
           REPORT.BASIS = NA,
           PERCENT.MOISTURE = NA,
           PERCENT.SOLID = NA
    ) %>%  
    # Drop any rows where the Sample Type column is not 'SMPL'
    filter(`Sample Type` == 'SMPL') %>%
    # Select desired columns, dropping the rest
    select(Sample, Result, Units, Component, 'Dilution Factor', 'Reporting Limit', 
           'Detection Limit', METHOD, LAB.ID, CAS.NUMBER, RESULT.REPORTED.TO,
           REPORT.BASIS, PERCENT.MOISTURE, PERCENT.SOLID) %>%  
    # Rename columns to match Houston data
    rename("SAMPLE ID" = "Sample", "RESULT" = "Result", "UNITS" = "Units", 
           "ANALYTE"="Component", "DILUTION"="Dilution Factor",
           "RL"="Reporting Limit", "MDL"="Detection Limit" 
    ) %>%
    # convert to data frame type
    data.frame() 
  # return dataframe
  return(df) 
}

cleanData <- function(df, file_path = NULL) {
  # takes imported data and cleans it for processing
  
  # normalize column names early (handles SAMPLE ID/RESULT variants)
  df <- standardize_als_column_names(df)
  
  # --- sanity check on required columns ---
  required_cols <- c("SAMPLE.ID", "RESULT")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "❌ Missing columns in file: ", file_path,
      "\nRequired column(s) missing: ", paste(missing_cols, collapse = ", ")
    ))
  }
  
  # --- main cleaning pipeline (keep RESULT as text until final coercion) ---
  df <- df[!grepl("Sample:", df$SAMPLE.ID), ] %>%
    mutate(
      flag   = NA_character_,
      RESULT = as.character(RESULT),
      
      # censored values (“<…”) -> explicit 0 (as text for now)
      RESULT = ifelse(grepl("<", RESULT, fixed = TRUE), "0", RESULT),
      
      # “H” hold-time -> save flag, then strip
      flag   = ifelse(stringr::str_detect(RESULT, "H"),
                      if_else(is.na(flag), "H", paste0(flag, ", H")),
                      flag),
      RESULT = gsub("H", "", RESULT, fixed = TRUE),
      
      # “See Attached” sentinel -> 9999 (text for now; we’ll flag + drop later)
      RESULT = gsub("See Attached", "9999", RESULT,  fixed = TRUE),
      
      # normalize empties
      RESULT = ifelse(RESULT %in% c("N/A", "NA", "", " "), NA, RESULT),
      
      # trim + keep sci-notation characters only
      RESULT = ifelse(is.na(RESULT), NA_character_, trimws(RESULT)),
      RESULT = ifelse(
        is.na(RESULT), NA_character_,
        gsub("[^0-9eE+\\-\\.]", "", RESULT)
      )
    ) %>%
    # Numeric coercion (robust sci-notation)
    mutate(
      RESULT           = readr::parse_double(RESULT, na = c("", "NA"),
                                             locale = readr::locale(decimal_mark = ".", grouping_mark = ",")),
      DILUTION         = suppressWarnings(as.numeric(DILUTION)),
      MDL              = suppressWarnings(as.numeric(MDL)),
      RL               = suppressWarnings(as.numeric(RL)),
      PERCENT.MOISTURE = suppressWarnings(as.numeric(PERCENT.MOISTURE)),
      PERCENT.SOLID    = suppressWarnings(as.numeric(PERCENT.SOLID))
    ) %>%
    # Convert 9999 (“See Attached”) to a separate flag, then remove from RESULT
    mutate(
      see_attached = dplyr::if_else(!is.na(RESULT) & RESULT == 9999, TRUE, FALSE, missing = NA),
      RESULT       = dplyr::if_else(see_attached, NA_real_, RESULT)
    ) %>%
    # Non-detect after numeric: prefer MDL rule; fallback to exact 0 when MDL missing
    mutate(
      non.detect = dplyr::case_when(
        !is.na(MDL)  & !is.na(RESULT) & RESULT <= MDL ~ TRUE,
        is.na(MDL)   & !is.na(RESULT) & RESULT == 0   ~ TRUE,
        TRUE                                          ~ FALSE
      )
    )
  
  return(df)
}

# TODO: add the calculation of total N and Mineral P here
dict_to_regex <- function(dict) {
  tibble::tibble(
    key = names(dict),
    rx  = purrr::map_chr(dict, ~ paste0("\\b(", paste(sort(.x, decreasing = TRUE), collapse="|"), ")\\b"))
  )
}

map_by_regex <- function(x, rx_tbl) {
  out <- rep(NA_character_, length(x))
  for (i in seq_len(nrow(rx_tbl))) {
    hits <- stringr::str_detect(x, rx_tbl$rx[i])
    out[is.na(out) & hits] <- rx_tbl$key[i]
  }
  out
}

# --- ADD once (after dicts are defined) so they’re in scope globally ---
.location_rx <- dict_to_regex(location.dict)
.method_rx   <- dict_to_regex(method.dict)
.event_rx    <- dict_to_regex(eventType.dict)
.trt_rx      <- dict_to_regex(trt.dict)
.eventcnt_rx <- dict_to_regex(eventCount.dict)

processData <- function(df) {
  df %>%
    mutate(
      duplicate      = stringr::str_detect(SAMPLE.ID, "-D"),
      location.name  = map_by_regex(SAMPLE.ID, .location_rx),
      method.name    = map_by_regex(SAMPLE.ID, .method_rx),
      event.type     = map_by_regex(SAMPLE.ID, .event_rx),
      treatment.name = map_by_regex(SAMPLE.ID, .trt_rx),
      event.count    = map_by_regex(SAMPLE.ID, .eventcnt_rx),
      treatment.name = dplyr::if_else(is.na(treatment.name), event.type, treatment.name),
      event.type     = dplyr::coalesce(event.type, "Point Sample")
    ) %>%
    mutate(
      across(c(location.name, method.name, event.type),
             ~ gsub("[0-9]", "", .)),
      treatment.name = dplyr::if_else(
        stringr::str_detect(treatment.name, "[0-9][0-9]$"),
        substr(treatment.name, 1, nchar(treatment.name) - 1),
        treatment.name
      ),
      treatment.name = dplyr::if_else(
        stringr::str_detect(treatment.name, "Inflow"),
        "Inflow", treatment.name
      ),
      event.count = as.factor(event.count)
    )
}

# TODO: fix and start using the flag data function
flagData <- function(df){
  # function to flag data and perform QA/QC after merging both htm and xls files
  # check water data for flags such as:
  # H = past hold time (declared in cleanData function)
  # J = minimum detection limit (MDL) > value > reporting limit (RL)
  # N = non-EPA method used
  # P = Ortho-P > Total P
  # more?
  
  # Other, more simple flag conditions...
  df %>%
    mutate(
      # J flag: RESULT in [MDL, RL) with both limits present (vectorized)
      flag = dplyr::if_else(
        !is.na(RESULT) & !is.na(MDL) & !is.na(RL) & RESULT >= MDL & RESULT < RL,
        dplyr::if_else(is.na(flag), "J", paste0(flag, ", J")),
        flag
      ),
      # See Attached: use vectorized condition; do NOT use isTRUE() on a vector
      flag = dplyr::if_else(
        dplyr::coalesce(see_attached, FALSE),
        dplyr::if_else(is.na(flag), "See Attached", paste0(flag, ", See Attached")),
        flag
      )
    )
}

# TODO: investigate addCoord; it doesn't work reliably
addCoord <- function(df, geo_key) {
  dup_keys <- geo_key %>%
    dplyr::count(location.name, treatment.name, event.type) %>%
    dplyr::filter(n > 1)
  if (nrow(dup_keys) > 0) {
    warning("Duplicate rows in geo_key for some (location, treatment, event) keys; using first match.")
    geo_key <- geo_key %>%
      dplyr::distinct(location.name, treatment.name, event.type, .keep_all = TRUE)
  }
  
  out <- df %>%
    dplyr::left_join(geo_key, by = c("location.name","treatment.name","event.type"))
  
  n_missing <- sum(is.na(out$long) | is.na(out$lat))
  if (n_missing > 0) {
    warning(sprintf("Missing coordinates for %d rows after join.", n_missing))
  }
  out
}

normalize_ec <- function(df) {
  df %>%
    mutate(
      RESULT = dplyr::if_else(
        ANALYTE %in% c("Specific Conductance","EC25","EC") &
          !is.na(RESULT) & RESULT > 50 & RESULT < 200000,  # crude heuristic for µS/cm
        RESULT / 1000,  # µS/cm -> mS/cm
        RESULT
      ),
      UNITS  = dplyr::if_else(
        ANALYTE %in% c("Specific Conductance","EC25","EC"),
        "mS/cm", UNITS
      )
    )
}

qa_scan <- function(df){
  list(
    negative_results = df %>% dplyr::filter(!is.na(RESULT) & RESULT < 0) %>% nrow(),
    rl_less_than_mdl = df %>% dplyr::filter(!is.na(RL) & !is.na(MDL) & RL < MDL) %>% nrow(),
    zero_not_flagged = df %>% dplyr::filter(!is.na(RESULT) & RESULT == 0 & non.detect != TRUE) %>% nrow(),
    missing_coords   = df %>% dplyr::filter(is.na(long) | is.na(lat)) %>% nrow(),
    see_attached_kept= df %>% dplyr::filter(see_attached %in% TRUE, !is.na(RESULT)) %>% nrow()
  )
}

dfTss <- function(tss_fp) {
  df <- read_excel(tss_fp, sheet = "MasterData") %>%
    # Standardize the BAD_GOOD column to lowercase
    mutate(BAD_GOOD = tolower(BAD_GOOD)) %>%
    # Consolidate filtering steps
    filter(
      # Keep rows where BAD_GOOD is not 'bad' or is NA
      is.na(BAD_GOOD) | BAD_GOOD != 'bad',
      # Remove 'Stock Solution' and 'DI' from SAMPLE.ID
      !(Sample_ID %in% c("Stock Solution", "DI")),
      # Remove rows where 'TSS_mg/L' is NA
      !is.na(`TSS_mg/L`)
    ) %>%
    # Collect relevant columns
    select(Sample_ID, Collection_date, `TSS_mg/L`, pH, `EC_mS/cm`) %>%
    # Rename to be congruent with ALS data
    rename(SAMPLE.ID = Sample_ID,
           COLLECTED = Collection_date,
           `Suspended Solids (Residue, Non-Filterable)` = `TSS_mg/L`,
           `Specific Conductance` = `EC_mS/cm`) %>%
    # Omit NA values (if still needed, this would omit rows with NA in any remaining column)
    #na.omit() %>%
    # processData here to get other columns correctly designated
    processData() %>%
    # TSS-specific column filling/data cleaning
    mutate(
      non.detect = NA, #make this a placeholder column for later when we detect non-detects
      `Suspended Solids (Residue, Non-Filterable)` = as.numeric(
        `Suspended Solids (Residue, Non-Filterable)`),
      `Specific Conductance` = as.numeric(`Specific Conductance`),
      `pH` = as.numeric(`pH`)
    ) %>%
    pivot_longer(cols = c(
      'pH', 'Suspended Solids (Residue, Non-Filterable)', 
      'Specific Conductance'),
      names_to = "ANALYTE", 
      values_to = "RESULT") %>%
    mutate(
      DILUTION = 1,
      `RESULT.REPORTED.TO` = "RL",
      MDL = case_when(
        ANALYTE == "pH" ~ 0.1,
        ANALYTE == "Suspended Solids (Residue, Non-Filterable)" ~ 2.5,
        ANALYTE == "Specific Conductance" ~ 5.0,
        TRUE ~ NA_real_
      ),
      RL = case_when(
        ANALYTE == "pH" ~ 0.1,
        ANALYTE == "Suspended Solids (Residue, Non-Filterable)" ~ 2.5,
        ANALYTE == "Specific Conductance" ~ 5.0,
        TRUE ~ NA_real_
      ),
      METHOD = case_when(
        ANALYTE == "pH" ~ "EPA150.1",
        ANALYTE == "Suspended Solids (Residue, Non-Filterable)" ~ "E160.2 - TSS_W_160.2",
        ANALYTE == "Specific Conductance" ~ "E120.1 - COND_W",
        TRUE ~ NA_character_
      ),
      # Add logic to replace negative values in RESULT with 0 and mark non.detect
      non.detect = RESULT <= 0,
      RESULT = ifelse(RESULT < 0, 0, RESULT),
      UNITS = tssUnits.dict[ANALYTE],
      COLLECTED = as.POSIXct(COLLECTED, format = '%Y-%m-%d %H:%M:%S'),
      treatment.name = as.character(treatment.name),
      event.count = as.character(event.count)
    ) 
  return(df)
}
# ---- End of Helper Functions ----

# ---- Main Functions ----
executeFxns <- function(file_path, kelso=FALSE, geo_key) {
  # Conditionally use importData or importDataKelso based on kelso variable
  if (kelso) {
    df <- importDataKelso(file_path)
  } else {
    df <- importData(file_path)
  }
  
  df <- df %>%
    cleanData(file_path = file_path) %>%   # clean ALS/Kelso format
    processData() %>%                      # create new columns using IDs
    normalize_ec() %>%                     # <-- optional EC unit normalization
    addCoord(geo_key) %>%                  # add spatial data
    flagData() %>%                         # flag and QA/QC data
    # TODO: Add epa cols fxn
    { select(., -all_of(
      c("REPORT.BASIS","PERCENT.MOISTURE","PERCENT.SOLID","LAB.ID.y","MATRIX","HOLD")
      [c("REPORT.BASIS","PERCENT.MOISTURE","PERCENT.SOLID","LAB.ID.y","MATRIX","HOLD") %in% names(.)]
    )) }
  
  return(df)
}

mergeFiles <- function(directory, tss_fp) {
  # import all data files in the directory, merge, and return df
  print("Merging files...")
  file_list <- list.files(path = directory,
                          pattern = "\\.(xls|csv)$", 
                          full.names = TRUE,
                          ignore.case = TRUE)
  # import houston data files
  data_files <- file_list[!grepl("-Samples|Kelso|kelso|kelos|Kelos", file_list)]
  print("Houston data files to be merged:")
  print(data_files)
  # import kelso data files
  kelso_files <- file_list[grepl("Kelso|kelso|kelos|Kelos", file_list)]
  print("Kelso data files to be merged:")
  print(kelso_files)
  # import meta data files
  meta_files <- file_list[grepl("-Samples", file_list)]
  print("Metadata files to be merged:")
  print(meta_files)
  
  # merge houston data files
  if (length(data_files) > 0) {
    df_data <- data_files %>%
      lapply(executeFxns, kelso = FALSE, geo_key = geo_key) %>%
      purrr::compact() %>%  # remove NULLs if any executeFxns failed
      bind_rows()
  } else {
    warning("⚠️ No Houston data files found. Proceeding with Kelso data\n")
    df_data <- data.frame()
  }
  
  # merge kelso data files
  if (length(kelso_files) > 0) {
    df_kelso <- kelso_files %>%
      lapply(executeFxns, kelso = TRUE, geo_key = geo_key) %>%
      purrr::compact() %>%
      bind_rows()
  } else {
    warning("⚠️ No Kelso data files found. Proceeding with metadata\n")
    df_kelso <- data.frame()
  }
  
  # merge metadata files
  df_meta <- if (length(meta_files) > 0) {
    meta_files %>% 
      lapply(importDataXls) %>%
      bind_rows()
  } else {
    warning("⚠️ No metadata files found. Error below is likely due to this.\n")
    data.frame()
  }
  
  # Merge Houston and Kelso data
  df_combined <- bind_rows(df_data, df_kelso)
  
  # Merge combined data with metadata
  df_merge <- df_combined %>%
    left_join(df_meta, by = 'SAMPLE.ID')
  
  # Change to POSIXct
  df_merge$COLLECTED <- lubridate::parse_date_time(
    df_merge$COLLECTED,
    orders = c("d b Y H:M", "Y-m-d H:M:S", "m/d/Y H:M", "Y-m-d"),
    tz = "America/Denver"
  )
  
  
  # Import TSS data to df with metadata
  if (!is.null(tss_fp) && file.exists(tss_fp)) {
    df_tss <- tryCatch({
      tss_data <- dfTss(tss_fp)
      message("✅ TSS data imported: ", nrow(tss_data), " rows.")
      tss_data
    }, error = function(e) {
      warning("⚠️ Failed to read TSS file at: ", tss_fp, "\nError: ", e$message)
      data.frame()
    })
  } else {
    warning("⚠️ TSS file not found or path is NULL: ", tss_fp)
    df_tss <- data.frame()
  }
  
  # Merge TSS data with the combined data
  df <- bind_rows(df_merge, df_tss) %>%
    filter(!grepl("Analysis", ANALYTE, ignore.case = TRUE)) %>%
    mutate(
      # create analyte abbreviation column based on ANALYTE
      analyte.abbr = sapply(ANALYTE, function(x) map_values_analyte(x, analyteAbbr.dict)),
      analyte.abbr = gsub("1", "", analyte.abbr)
    )
  
  # Drop unnecessary columns that get re-created during merge
  # List of columns to drop
  cols_to_drop <- c("LAB.ID.y", 
                    "MATRIX", 
                    "HOLD")
  # Drop only if the column exists
  cols_to_drop <- cols_to_drop[cols_to_drop %in% names(df)]
  df <- select(df, -all_of(cols_to_drop))
  
  return(df)
  
}

# Define public functions (i.e., to be called by user)
returnSingleFile <- function(path = file_path, export = FALSE) {
  # return and optionally export a single file for QA/QC
  df <- executeFxns(path, kelso = FALSE, geo_key = geo_key)
  if (export == TRUE) {
    # Get the current date in YYYY-MM-DD format
    currentDate <- format(Sys.Date(), "%Y-%m-%d")
    # Create a file name with the current date
    fileName <- paste0("./Report/single_file_", currentDate, ".csv")
    # Export the file with the new file name
    write.csv(df, file = fileName, row.names = FALSE)
  }
  return(df)
}

returnAllFiles <- function(d = directory, tss_fp = tss_file_path, export = TRUE) {
  # return and optionally export all files for QA/QC
  df <- mergeFiles(d, tss_fp)
  # for debugging only; uncomment as necessary
  #View(df)
  if (export == TRUE) {
    # Get the current date in YYYY-MM-DD format
    currentDate <- format(Sys.Date(), "%Y-%m-%d")
    # Create a file name with the current date
    fileName <- paste0("./Report/all_files_", currentDate, ".csv")
    # Export the file with the new file name
    write.csv(df, file = fileName, row.names = FALSE)
  }
  return(df)
}
