## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(toc = TRUE, echo = FALSE, warning = FALSE, message = FALSE)




## ---- include=FALSE-----------------------------------------------------------------------------------------------------------------------------------
package.list <- c("ggplot2",
                  "dplyr",
                  "DT",
                  "plotly",
                  "knitr",
                  'taskscheduleR'
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
#clean data source
source("file-merger.R", local = knitr::knit_global())
#gitignore file source 
source('config.R',local = knitr::knit_global())

# Set directories
report_path <- paste(dirname(getwd()), "/Example Report", sep = "")
data_path <- paste(dirname(getwd()), "/Data", sep = "")


# import data
dat <- returnAllFiles(d = data_path, export = FALSE)
#dat <- read.csv('all_files.csv', header = TRUE, stringsAsFactors = FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
#create grouped data frame
df_grouped <- dat %>%
    group_by(location.name, ANALYTE) %>%
    arrange(location.name)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
colnames(df_grouped) <- tolower(names(df_grouped))


## -----------------------------------------------------------------------------------------------------------------------------------------------------
#create data table based on counts
counts <- dat %>% 
    group_by(location.name, ANALYTE) %>% 
    summarise(count = n())
DT::datatable(counts, options = list(pageLength = length(counts$location.name)))


## -----------------------------------------------------------------------------------------------------------------------------------------------------
# Define the scatter plot function
make_scatter_plots <- function(data) {
  
  # Get unique locations and analytes
  unique_locations <- unique(data$location.name)
  unique_analytes <- unique(data$analyte)
  
  # Generate scatter plots for each unique location and analyte
  for (location in unique_locations) {
    for (analyte in unique_analytes) {
      # Filter the data for the current location and analyte
      filtered_data <- data %>%
        filter(location == location, analyte == analyte) %>%
        ungroup()
      
      # Create the scatter plot
      scatter_plot <- ggplot(filtered_data, aes(x = collected, y = result, color = event.type)) +
        geom_point() +
        labs(x = "Date", y = "Result (mg/L)", title = paste( analyte, "Results for", location))
      
      # Print the scatter plot
      print(scatter_plot)
      
      # Add a new line after each plot
      cat("\n")
    }
  }
}



## -----------------------------------------------------------------------------------------------------------------------------------------------------
#filter to site
ARDEC2200 <- df_grouped %>% 
  filter(location.name == "ARDEC 2200")


# Call the scatter plot function for the site
make_scatter_plots(ARDEC2200)


## -----------------------------------------------------------------------------------------------------------------------------------------------------

ARDECSouth <- df_grouped %>% 
  filter(location.name == "ARDEC South -  Conv")



make_scatter_plots(ARDECSouth)


## -----------------------------------------------------------------------------------------------------------------------------------------------------

ARDECSouthOrg <- df_grouped %>% 
  filter(location.name == "ARDEC South - Org")



make_scatter_plots(ARDECSouthOrg)


## -----------------------------------------------------------------------------------------------------------------------------------------------------

AVRC <- df_grouped %>% 
  filter(location.name == "AVRC STAR")


make_scatter_plots(AVRC)


## -----------------------------------------------------------------------------------------------------------------------------------------------------

Barley <- df_grouped %>% 
  filter(location.name == "Barley")

make_scatter_plots(Barley)


## -----------------------------------------------------------------------------------------------------------------------------------------------------

BelowStage <- df_grouped %>% 
  filter(location.name == "Below Stagecoach Dam")


# Call the scatter plot function with your dataframe
make_scatter_plots(BelowStage)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
Berthoud <- df_grouped %>% 
  filter(location.name == "Berthoud")


make_scatter_plots(Berthoud)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
BigHollow <- df_grouped %>% 
  filter(location.name == "Big Hollow")


make_scatter_plots(BigHollow)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
BoulderLake <- df_grouped %>% 
  filter(location.name == "Boulder Lake")


make_scatter_plots(BoulderLake)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
Gunnison <- df_grouped %>% 
  filter(location.name == "Gunnison")


make_scatter_plots(Gunnison)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
Kerbel <- df_grouped %>% 
  filter(location.name == "Kerbel")


make_scatter_plots(Kerbel)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
Legacy <- df_grouped %>% 
  filter(location.name == "Legacy")


make_scatter_plots(Legacy)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
Molina <- df_grouped %>% 
  filter(location.name == "Molina")


make_scatter_plots(Molina)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
MorrisonCreek <- df_grouped %>% 
  filter(location.name == "Morrison Creek")


make_scatter_plots(MorrisonCreek)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
SCA <- df_grouped %>% 
  filter(location.name == "Stage Coach Above")


make_scatter_plots(SCA)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
SCI <- df_grouped %>% 
  filter(location.name == "Stage Coach In")


make_scatter_plots(SCI)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
Stagecoach <- df_grouped %>% 
  filter(location.name == "Stagecoach")


make_scatter_plots(Stagecoach)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
TR <- df_grouped %>% 
  filter(location.name == "Todds Ranch")


make_scatter_plots(TR)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
UY <- df_grouped %>% 
  filter(location.name == "Upper Yampa")


make_scatter_plots(UY)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
Yampa <- df_grouped %>% 
  filter(location.name == "Yampa")


make_scatter_plots(Yampa)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
YJ <- df_grouped %>% 
  filter(location.name == "Yellow Jacket")


make_scatter_plots(YJ)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
datatable(df_grouped, colnames = c('Sample ID', 'Lab ID', 'Method', 'CAS Number', 'Analyte', 'Result', 'Units', 'Dilution', "Results Reported To", 'MDL', 'RL', 'Report Basis', 'Moisture (%)', 'Solid (%)', 'Non detect', 'Duplicate', 'Location', 'Treatment', 'Method Name', 'Event Type', 'Lab ID', 'Matrix', 'Collection Date', 'Recieved Date', 'Hold Time', 'Flag'))

