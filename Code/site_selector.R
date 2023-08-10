# AWQP Site Selector and Data Download Tool
# Caz Bell, molly.bell@colostate.edu

# TODO:


# Instructions: run the code, fill in the location name that is in quotes with the site of interest

# Available Sites

# ARDEC 2200
# AVRC STAR
# Barley
# Berthoud
# Big Hollow
# Boulder Lake
# Below Stagecoach Dam
# Gunnison
# Kerbel
# Legacy
# Molina
# Morrison Creek
# Stage Coach Above
# Stage Coach In
# Stagecoach
# Todds Ranch
# Upper Yampa
# Yellow Jacket


# load required packages
package.list <- c('plotly',
                  'dplyr',
                  'htmltools',
                  'shiny'
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

# source the file-merger for clean data
source('./Code/file-merger.R')

# create data frame
dat <- returnAllFiles(d = directory, export = FALSE)
# make columns lowercase so they are all the same case
colnames(dat) <- tolower(names(dat))

# Make folder name
output_folder <- "site-selector-temporary-results"
# Site selector csv data

filter_and_save_data <- function(dat, location_name) {
  location_data <- dat %>%
    filter(location.name == location_name)
  
  
  file_name <- paste0(folder_name, "/", location_name, "_data_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
  write.csv(location_data, file = file_name, row.names = FALSE)
}

filter_and_save_data(dat, "Boulder Lake")

library(plotly)
library(dplyr)

create_location_violin_plot <- function(df, selected_location, event_colors) {
  y_axis_var_names <- unique(df$analyte)
  
  create_buttons <- function(df, y_axis_var_name, selected_location) {
    list(
      method = 'restyle',
      args = list('y', list(df[df$analyte == y_axis_var_name & df$location.name == selected_location, "result"])),
      label = sprintf('Show %s', y_axis_var_name)
    )
  }
  
  location_violin_plot <- plot_ly(df, x = ~collected,
                                  y = ~result,
                                  color = ~event.type,
                                  colors = event_colors,
                                  type = 'violin', box = list(
                                    visible = T
                                  ),
                                  meanline = list(
                                    visible = T
                                  )) %>%
    layout(
      title = paste("Violin plots for Location:", selected_location),
      xaxis = list(title = "Collected", tickformat = "%y-%m-%d"),
      yaxis = list(title = "Result"),
      showlegend = TRUE,
      updatemenus = list(
        list(
          buttons = lapply(y_axis_var_names, create_buttons, df = df, selected_location = selected_location)
        )
      ))
  
  return(location_violin_plot)
}



# Define the color scheme
event_colors <- c("Inflow" = "#E69F00", "Outflow" = "#56B4E9", "Point Sample" = "#009E73")

# Replace "AVRC STAR" with your desired selected location
selected_location <- "AVRC STAR"

# Call the function to create the location-specific violin plot
location_plot <- create_location_violin_plot(dat, selected_location, event_colors)

# Display the location-specific violin plot
location_plot
 



