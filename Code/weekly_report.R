today <- function() {
  today <- Sys.Date()
  return(today)
}


#Source the Rmd file
library(knitr)

rmd_file <- "water_report.Rmd"
r_script <- knitr::purl(rmd_file)
source(r_script)
#create function for todays date
today <- function() {
  return(Sys.Date())
}
# render rmd file and output into report folder
rmarkdown::render(input = 'water_report.Rmd', output_file = sprintf('~/GitHub/ALS-Data-Cleaning-Tool/Report/water_report_%s.html', 
                                                                    format(today, format = "%m-%d-%y")))
library(later)
# Set the path to the R script
script_path <- "weekly_report.R"

# Define the function to execute the R script

execute_script <- function() {
  source(script_path)
}



execution_time <- as.POSIXct(paste(today(), "10:45:00"))

# Calculate the delay in seconds
delay_seconds <- as.numeric(execution_time - Sys.time())

# Schedule the execution of the R script
later::later(execute_script, delay = delay_seconds)









