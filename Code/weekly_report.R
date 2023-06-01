today <- function() {
  today <- Sys.Date()
  return(today)
}


#Source the Rmd file
library(knitr)

rmd_file <- "water_report.Rmd"
r_script <- knitr::purl(rmd_file)
source(r_script)

library(rmarkdown)

# Generate the output file name with today's date
output_file <- sprintf('~/GitHub/ALS-Data-Cleaning-Tool/Report/water_report_%s.html',
                       format(today(), format = "%m-%d-%y"))

# Render the R Markdown document
rmarkdown::render(input = 'water_report.Rmd', output_file = output_file)


# Set the path to the R script

script_path <- "weekly_report.R"

# Define the function to execute the R script

execute_script <- function(){
  source(script_path)}



execution_time <- as.POSIXct(paste(today(), "11:32:00"))


delay_seconds <- as.numeric(execution_time - Sys.time())


later::later(execute_script, delay = delay_seconds)













