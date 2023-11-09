# configuration file for ALS data cleaning tool
# AJ Brown
# Ansley.Brown@colostate.edu
# 3 Nov 2023

# How to:
# Define your file paths below to be used in this tool.  Please note that these
# variables are found in the file-merger.r script, which ultimately controls the
# file path for all other scripts.

# Definitions:
# directory = where ALS raw data is stored (xls and htm)
# tss_file_path = where TSS data excel file is stored (xlsx)

# For GitHub  Repo
  # directory <- "./Data"
  # tss_file_path <- './TSS/TSS_Master_2023.xlsx'
# for real data in file that won't be uploaded to github repo:
  # directory <- "{your file path to ALS data FOLDER here}"
  # tss_file_path <- '{your file path to TSS data FILE here}'
# Example
  directory <- "./Confidential Data"
  tss_file_path <- './Confidential TSS/TSS_Master_2023.xlsx'

tss_directory<- dirname(tss_file_path)