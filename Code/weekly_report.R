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



today <- weekdays(Sys.Date())
if (today == "Friday") {
  # If today is already Friday, calculate the delay for the following week
  delay_seconds <- 86400 * (7 - 1) + 60 * 60 * (8 - as.numeric(format(Sys.time(), "%H")))
} else {
  # If today is not Friday, calculate the delay until the next Friday
  delay_seconds <- 86400 * ((5 - as.integer(format(Sys.Date(), "%w"))) %% 7) + 60 * 60 * (8 - as.numeric(format(Sys.time(), "%H")))
}

# Schedule the execution of the R script
later::later(execute_script, delay = delay_seconds)














