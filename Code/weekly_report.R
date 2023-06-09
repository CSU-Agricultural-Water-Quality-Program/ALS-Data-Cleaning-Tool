today <- function() {
  today <- Sys.Date()
  return(today)
}


#Source the Rmd file

library(knitr)
library(taskscheduleR)

rmd_file <- "water_report.Rmd"
r_script <- knitr::purl(rmd_file)
source(r_script)

library(rmarkdown)

# Generate the output file name with today's date

output_file <- sprintf('~/GitHub/ALS-Data-Cleaning-Tool/Report/water_report_%s.html',
                       format(today(), format = "%m-%d-%y"))


rmarkdown::render(input = 'water_report.Rmd', output_file = output_file)


# Set the path to the R script

script_path <- "./Code/weekly_report.R"

# Define the function to execute the R script

taskscheduler_create(taskname = "myfancyscriptsunsat", rscript = script_path, 
                     schedule = "WEEKLY", starttime = "8:00", days = 'FRI')

















