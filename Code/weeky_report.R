#load taskscheduler, this is the only package needed so far, more will when we do the email
library(taskscheduleR)

#create function that shows todays date for output
today <- function() {
  today <- Sys.Date()
  return(today)
}


#Source the Rmd file
library(knitr)

rmd_file <- "water_report.Rmd"
r_script <- knitr::purl(rmd_file)
source(r_script)

myscript

taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
                     schedule = "ONCE", starttime = format(Sys.time() + 62, "%H:%M"))



