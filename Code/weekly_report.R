#create function that shows todays date for output

today <- function() {
  today <- Sys.Date()
  return(today)
}




library(knitr)

rmd_file <- "water_report.Rmd"
r_script <- knitr::purl(rmd_file)
source(r_script)

library(rmarkdown)

library(rmarkdown)

# Generate the output file name with today's date
output_file <- sprintf('~/GitHub/ALS-Data-Cleaning-Tool/water_report_%s.html',
                       format(Sys.Date(), format = "%m-%d-%y"))

# Render the R Markdown document
rmarkdown::render(input = 'water_report.Rmd', output_file = output_file)

# Suppress the warning messages
warnings()


library(later)

# Set the path to the R script

script_path <- "weekly_report.R"

# Define the function to execute the R script

execute_script <- function(){
  source(script_path)}



execution_time <- as.POSIXct(paste(today(), "10:00:00"))


delay_seconds <- as.numeric(execution_time - Sys.time())


later::later(execute_script, delay = delay_seconds)


# Calculate the delay in seconds for next Friday at 8 AM

next_friday <- Sys.Date() + ((5 - as.integer(format(Sys.Date(), "%w"))) %% 7)
execution <- as.POSIXct(paste(next_friday, "08:00:00"))

# Calculate the delay in seconds

delay <- as.numeric(execution - Sys.time())

# Schedule the execution of the R script

later::later(execute_script, delay = delay, period = "week")








