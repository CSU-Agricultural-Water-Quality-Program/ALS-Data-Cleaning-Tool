## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------
# excludes warnings, messages, and echo from all code chunks
knitr::opts_chunk$set(toc = TRUE, echo = FALSE, warning = FALSE, message = FALSE)
#set entire file to the main working directory
knitr::opts_knit$set(root.dir = '~/GitHub/ALS-Data-Cleaning-Tool')




## ---- include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
# load in packages
package.list <- c("ggplot2",
                  "dplyr",
                  "DT",
                  "plotly",
                  "knitr"
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
# clean data source
source("./Code/file-merger.R", local = knitr::knit_global())

# import data
dat <- returnAllFiles(d = directory, export = FALSE) 
# dat <- read.csv('all_files.csv', header = TRUE, stringsAsFactors = FALSE)
# make everything lowercase
colnames(dat) <- tolower(names(dat))





## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
# create grouped data frame
df_grouped <- dat %>%
    group_by(location.name, analyte) %>%
    arrange(location.name)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

# create scatterplot function
make_scatter_plots <- function(dataframe) {
  # Get unique analytes
  analytes <- sort(unique(dataframe$analyte))
  
  event_colors <- c("Inflow" = "blue", "Outflow" = "green4", "Point Sample" = "orange")
  
  for (i in 1:length(analytes)) {
    # Subset data for the current analyte
    subset_analyte <- subset(dataframe, analyte == analytes[i])
    
    # Check if there are no results for the current analyte
    if (nrow(subset_analyte) == 0) {
      next  # Skip creating the plot for this analyte
    }
    
    # Get unique units for the current analyte
    units <- unique(subset_analyte$units)
    
    # Identify highest and lowest values
    if (nrow(subset_analyte) >= 2) {
      highest_value <- subset_analyte %>% arrange(desc(result)) %>% slice(1)
      lowest_value <- subset_analyte %>% arrange(result) %>% slice(1)
      
      #round to 2 decimal places to make data call outs cleaner
      highest_value$result <- round(highest_value$result, 2)
      lowest_value$result <- round(lowest_value$result, 2)
      
      # Create scatter plot for the current analyte
      plot <- ggplot(subset_analyte, aes(x = collected, y = result, color = event.type)) +
        geom_point() +
        geom_text(data = rbind(highest_value, lowest_value),
                  aes(label = result), hjust = 0, vjust = 0, size = 3, color = "black") + # Add data callouts for highest and lowest values
        scale_color_manual(values = event_colors) +
        labs(x = "Date", y = paste0("Result (", units, ")"), title = paste(analytes[i], "Results"))
      
      # Print the plot
      print(plot)
      
      # Add a new line after each plot
      cat("\n")
    }
  }
}



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
#filter to site
ARDEC2200 <- df_grouped %>% 
  filter(location.name == "ARDEC 2200")


# Call the scatter plot function for the site
make_scatter_plots(ARDEC2200)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

ARDECSouth <- df_grouped %>% 
  filter(location.name == "ARDEC South -  Conv")



make_scatter_plots(ARDECSouth)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

ARDECSouthOrg <- df_grouped %>% 
  filter(location.name == "ARDEC South - Org")



make_scatter_plots(ARDECSouthOrg)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

AVRC <- df_grouped %>% 
  filter(location.name == "AVRC STAR")


make_scatter_plots(AVRC)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

Barley <- df_grouped %>% 
  filter(location.name == "Barley")

make_scatter_plots(Barley)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

BelowStage <- df_grouped %>% 
  filter(location.name == "Below Stagecoach Dam")


# Call the scatter plot function with your dataframe
make_scatter_plots(BelowStage)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
Berthoud <- df_grouped %>% 
  filter(location.name == "Berthoud")


make_scatter_plots(Berthoud)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
BigHollow <- df_grouped %>% 
  filter(location.name == "Big Hollow")


make_scatter_plots(BigHollow)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
BoulderLake <- df_grouped %>% 
  filter(location.name == "Boulder Lake")


make_scatter_plots(BoulderLake)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
Gunnison <- df_grouped %>% 
  filter(location.name == "Gunnison")


make_scatter_plots(Gunnison)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
Kerbel <- df_grouped %>% 
  filter(location.name == "Kerbel")


make_scatter_plots(Kerbel)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
Legacy <- df_grouped %>% 
  filter(location.name == "Legacy")


make_scatter_plots(Legacy)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
Molina <- df_grouped %>% 
  filter(location.name == "Molina")


make_scatter_plots(Molina)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
MorrisonCreek <- df_grouped %>% 
  filter(location.name == "Morrison Creek")


make_scatter_plots(MorrisonCreek)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
SCA <- df_grouped %>% 
  filter(location.name == "Stage Coach Above")


make_scatter_plots(SCA)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
SCI <- df_grouped %>% 
  filter(location.name == "Stage Coach In")


make_scatter_plots(SCI)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
Stagecoach <- df_grouped %>% 
  filter(location.name == "Stagecoach")


make_scatter_plots(Stagecoach)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
TR <- df_grouped %>% 
  filter(location.name == "Todds Ranch")


make_scatter_plots(TR)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
UY <- df_grouped %>% 
  filter(location.name == "Upper Yampa")


make_scatter_plots(UY)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
YJ <- df_grouped %>% 
  filter(location.name == "Yellow Jacket")


make_scatter_plots(YJ)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
#create data table based on counts
counts <- dat %>% 
    group_by(location.name, analyte) %>% 
    summarise(count = n())
DT::datatable(counts, options = list(pageLength = length(counts$location.name)))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
datatable(df_grouped, colnames = c('Sample ID', 'Lab ID', 'Method', 'CAS Number', 'Analyte', 'Result', 'Units', 'Dilution', "Results Reported To", 'MDL', 'RL', 'Report Basis', 'Moisture (%)', 'Solid (%)', 'Non detect', 'Duplicate', 'Location', 'Treatment', 'Method Name', 'Event Type', 'Lab ID', 'Matrix', 'Collection Date', 'Recieved Date', 'Hold Time', 'Flag'))

