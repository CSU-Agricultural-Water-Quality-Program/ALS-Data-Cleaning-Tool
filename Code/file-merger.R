# Steamboat Data Clean and Merge 2022
# AJ Brown
# 7/13/2022

# Tool to merge multiple xcel files into one and create categories for analysis

library(magrittr)
library(dplyr)
library(readr)
library(readxl)

#Change directory to where ever needed, make sure to replace "\" with "/"
directory = 'C:/Users/ansleybr/OneDrive - Colostate/AWQP/Caz Steamboat Data'

setwd(directory)

# import xls files and merge into single df
df <- list.files(path=directory) %>% 
  lapply(read_xls) %>% 
  bind_rows

df$IndTestName = gsub(" ", "_", df$IndTestName)

# Create Inflow/outflow/other source column based on FieldID
df$source = ifelse(grepl('IN', df$FieldID), 'Inflow', 
                   ifelse(grepl('OT|UYM', df$FieldID), 'Outflow',
                           ifelse(grepl('LABQC', df$FieldID),'LABQC', 'Other')))

# Create location column based on FieldID
df$location = ifelse(grepl('SCI',df$FieldID), 'Stagecoach_In',
                     ifelse(grepl('SCA', df$FieldID),'Stagecoach_Above',
                            ifelse(grepl('00', df$FieldID),'Stagecoach_Install',
                                   ifelse(grepl('TR', df$FieldID),'Todds_Ranch',
                                          ifelse(grepl('SB-L', df$FieldID),'Legacy_Ranch',
                                                 ifelse(grepl('Y', df$FieldID), 'Upper_Yampa',
                                                        ifelse(grepl('SCO', df$FieldID), 'Stagecoach_Out', NA)))))))
# Create sample type column base on FieldID
df$type = ifelse(grepl('G|GB|DB',df$FieldID), 'Grab',
                     ifelse(grepl('LC', df$FieldID),'Low_Cost', 
                            ifelse(grepl('ISC!LC!GB', df$FieldID), 'ISCO', NA)))

dfID = df[c('FieldID','type')]

# Important Graphs
# Goal 1: Get rid of unnecessary columns
# Goal 2: Make important graphs for each location illustrating inflow v. outflow
  # Nitrate, Total P, Ortho-P on same graph for example, selenium on separate due to units
        # Pro-tip: if ggplot is sucking b/c your df is too big, then consider dropping unneeded rows to simplify
             # e.g., drop all rows with locations other than the ones you're using


