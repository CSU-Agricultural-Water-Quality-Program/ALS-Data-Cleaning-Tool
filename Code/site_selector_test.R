# This script is used to easily test the site_selector.R file without knitting.
# Do not use for real applications.
# AJ Brown 13 Aug 2023


library(dplyr)
library(ggplot2)
library(tidyr)
#library(GGally)
#library(plotly)
library(PerformanceAnalytics)

loc = 'AVRC STAR'

# Uploading file-merge code to create data frame
source('./Code/file-merger.R')
# create data frame
dat <- returnAllFiles(d = directory, export = FALSE)
# make columns lowercase so they are all the same case
colnames(dat) <- tolower(names(dat))

# Load required libraries
library(plotly)

# Load required libraries
library(plotly)

# Load required libraries
library(plotly)

# Function to create a scatter plot with dropdown
# Function to create a scatter plot with dropdown
# Function to create a scatter plot with dropdown
create_scatterplot <- function(df, selected_location) {
  # Filter data to exclude analytes with no data
  filtered_df <- df[df$location.name == selected_location, ]
  analyte_counts <- table(filtered_df$analyte)
  valid_analytes <- names(analyte_counts)[analyte_counts > 0]
  filtered_df <- filtered_df[filtered_df$analyte %in% valid_analytes, ]
  
  y_axis_var_names <- sort(unique(filtered_df$analyte))
  
  # Specify event colors and types
  event_colors <- c("Inflow" = "#E69F00", "Outflow" = "#56B4E9", "Point Sample" = "#009E73")
  event_types <- c("Inflow", "Outflow", "Point Sample")
  
  create_buttons <- function(y_axis_var_name) {
    analyte_data <- filtered_df[filtered_df$analyte == y_axis_var_name, ]
    y_data <- analyte_data$result
    
    y_data <- y_data[!is.na(y_data)]  # Exclude NAs from y_data
    
    if (length(y_data) == 0) {
      return(NULL)  # Exclude analytes with no data from the dropdown
    }
    
    y_range <- range(y_data)
    buffer_factor <- 0.1  # Set your desired buffer factor
    y_range_with_buffer <- y_range + diff(y_range) * c(-buffer_factor, buffer_factor)
    
    list(
      method = 'restyle',
      args = list('y', list(y_data)),
      label = y_axis_var_name,
      args2 = list('yaxis.range', y_range_with_buffer)  # Update y-axis range
    )
  }
  
  # Create a scatter plot with the first analyte's data
  first_analyte <- y_axis_var_names[1]
  first_analyte_data <- filtered_df[filtered_df$analyte == first_analyte, ]
  first_analyte_event_types <- unique(first_analyte_data$event.type)
  
  location_scatter_plot <- plot_ly(data = first_analyte_data, x = ~collected, y = ~result,
                                   color = ~event.type, colors = event_colors,
                                   type = 'scatter', mode = 'markers',
                                   marker = list(size = 8, opacity = 0.6),
                                   legendgroup = ~event.type,
                                   showlegend = TRUE) %>%
    layout(
      title = paste("Scatter plot for Location:", selected_location),
      xaxis = list(title = "Collected", tickformat = "%m/%d/%y"),  # Format the date as MM/DD/YY
      yaxis = list(title = "Result", rangeslider = list()),  # Add rangeslider
      showlegend = TRUE,
      updatemenus = list(
        list(
          buttons = lapply(y_axis_var_names, create_buttons)
        )
      )
    )
  
  return(location_scatter_plot)
}

create_scatterplot(dat, loc)

