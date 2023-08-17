# ALS Data Cleaning Tool
A coding tool developed in R to take water analysis results exported from the [ALS WEBTRIEVE™ data portal](https://webtrieveus.alsenviro.com/Login.aspx). Exported data are cleaned, merged, and exported into archiving (e.g., CSV) or visual (e.g., HTML) formats. The tool also merges data produced in-house (i.e., pH, Total Suspended Soilds, and Specific Electrical Conductivity) with the final exported data.

*Created By: A.J. Brown, Agricultural Data Scientist, 
ansley.brown@colostate.edu*

## <u>Contents:</u>
* **file-merger.R** - a script that takes the raw files from ALS, cleans the data, and exports as a dataframe and/or CSV file for other uses
* **site_selector.Rmd** - a markdown script that produces an HTML report with interactive data visualization and optional data export for a specific research site location as chosen by the user
* **water_report.Rmd** - a markdown script that produces a summary of all water quality data for all sites on a YTD basis

## <u>Some Helpful Troubleshooting Notes:</u>

* ALS exports data as ".xls" file format in name only.  The actual result files are in ".htm" format.  The sample metadata, however is actually in ".xls" format, as found on the "samples" tab.

* If you edit the .htm file from ALS in excel and save it, it corrupts the file and cannot then be imported.  This occurred for us once when we tried to edit an erroneous date.

* When running file-merger.R, which occurs in all the markdown scripts, you CANNOT have the TSS excel file open, else an error will occur

## <u>Future Developments:</u>
* Automate the emailing of the report after generation

* Use config file to easily change file paths

* Fix mutate error in scatterplot matrix

* Set the plots to start at the correct y range

* Fix scatter plot points for Point Sample on the violin plot

* Potentially create a way to run the functions for multiple sites

* Add histograms and correlations to scatterplot matrix

* Put analyte labels outside of the scatterplot matrix and shorten their names with a analyte dict. 



