# Steamboat Data Clean and Merge 2022
# AJ Brown and Caz Bell
# 7/13/2022

# TODO create fxns: 1) processing df, 2) clean data for graphing, 3) graphing
# TODO create dictionary instead of ifelse() e.g., foo = c(a=1, b=2, etc.)

# Tool to merge multiple excel files into one and create categories for analysis

library(magrittr)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(lattice)

#Change directory to where ever needed, make sure to replace "\" with "/"
directory = 'C:/Users/ansleybr/OneDrive - Colostate/AWQP/Caz Steamboat Data'

setwd(directory)

# import xls files and merge into single df
df <- list.files(path=directory) %>%
  lapply(read_xls) %>%
  bind_rows

# Create Inflow/outflow/other source column based on FieldID
df$source = ifelse(grepl('IN', df$FieldID), 'Inflow',
                   ifelse(grepl('OT|UYM', df$FieldID), 'Outflow',
                           ifelse(grepl('LABQC', df$FieldID),'LABQC', 'Point_Sample')))

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
                            ifelse(grepl('LABQC', df$FieldID), 'LABQC', 'ISCO')))
# Create subset dataframe to check categorization algorithm
dfID = df[c('FieldID','type')]


# DATA CLEANING
# Drop LABQC columns
df = subset(df, df$type != 'LABQC')

# drop columns containing only NAs
df = Filter(function(x)!all(is.na(x)), df)

# replace spaces in test name column for graphing
df$IndTestName = gsub(" ", "_", df$IndTestName)

# convert results to numeric values
df$FinalResult = as.numeric(df$FinalResult)


# Important Graphs

# Generic bar plot tool

df_a = subset(df, df$IndTestName == 'TOTAL_PHOSPHORUS')
#df_P = subset(df_a, df_a$location == 'Legacy_Ranch' | df_a$location == 'Upper_Yampa')

p = ggplot(df_P, aes(x = source, y = FinalResult, fill=source)) +
           geom_bar(stat='identity') +
           facet_wrap(~location) +
           xlab('') +
           ylab('Total Phosphorus, mg/L') +
           ggtitle('Water Quality Analysis for Yampa River Valley') +
           theme_bw() +
           theme(axis.title.y = element_text(margin=margin(t = 0, r = 20, b = 0, l = 0)))
p
