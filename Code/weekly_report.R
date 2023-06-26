
# TODO:
# make relative pathway work
# get taskscheduler to work

# Import libraries
package.list <- c('lubridate',
                  'rmarkdown'
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

# create tody function, this will be used to schedule tasks
today <- function() {
  today <- Sys.Date()
  return(today)
}




# Set the path to the R Markdown file
rmd_file <- "./Code/water-report.Rmd"

# Set the path to the output directory
output_dir <- "~/GitHub/ALS-Data-Cleaning-Tool/Report"

# Set the output filename including the date the report was made
output_filename <- paste0("water_report_", format(Sys.Date(), "%Y%m%d"))

# Render the R Markdown file
render(rmd_file, output_format = "html_document",
       output_file = file.path(output_dir, paste0(output_filename, ".html")))






library(taskscheduleR)

# Define the command to execute (path to Rscript and your script)
command <- "./Code/weekly_report.R"

# Create a weekly task
task <- taskscheduler_create(
  taskname = "WeeklyTask",
  rscript = command,
  schedule = "WEEKLY",
  starttime = "01:30",
  days = "FRI"
)

# Register the task
registerTask(task)










