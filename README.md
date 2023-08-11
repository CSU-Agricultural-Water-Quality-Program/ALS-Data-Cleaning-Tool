# ALS Data Cleaning Tool
R script to take water analysis results exported from the [ALS WEBTRIEVE™ data portal](https://webtrieveus.alsenviro.com/Login.aspx) and then clean, merge, and export the data into a more useable format for the Colorado State University Agricultural Water Quality Program. The script can be used as a library for other scripts (e.g., for analysis and graphing), or simply used to export data for archiving.

Cheers,
A.J. Brown
Agricultural Data Scientist
ansley.brown@colostate.edu

Some Helpful Troubleshooting Notes: 

**ALS exports data as ".xls" file format in name only.  The actual files are in ".htm" format.  This is just a helpful tip for users working with these data

**If you edit the .htm file from ALS in excel and save it, it corrupts the file and cannot then be imported.  This occurred for us once when we tried to edit an erroneous date.

**When running file-merger.R, which occurs in all the markdown scripts, you CANNOT have the TSS excel file open, else an error will occur

Future developments:
**Automate the emailing of the report after generation

**Use config file to easily change file paths


