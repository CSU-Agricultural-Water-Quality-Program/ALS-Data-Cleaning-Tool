---
---
---

# How To Download Data From ALS Portal

#### Created by: Lauryn Howlett

#### Last updated: 07/29/2025

------------------------------------------------------------------------

## Go to the ALS Website

-   Navigate to <https://webtrieveus.alsenviro.com/Login.aspx>

-   Enter your UserID and Password and select the appropriate Laboratory

    -   AWQP uses the Houston, TX, laboratory

## Work Orders

-   Once you are logged in, you will see a list of the submitted Work Orders

-   You can search between specific time periods or by Project Name at the top

-   The columns for the Work Orders are as follows:

    -   DETAILS - This will open up details for the work order, including the results

    -   STATUS - This indicates whether the work order is complete or incomplete

    -   WORK ORDER - The Work Order ID assigned by ALS

    -   SAMPLES - The number of samples received by ALS

    -   RECEIVED - The date the samples were received by ALS

    -   PROJECT NAME - The project name associated with the work order

    -   PURCHASE ORDER - The organization responsible for payment

    -   REPORT TO - Who the results are sent to

## View and Download Data

-   To view and download the results for a work order click the "OPEN" button under the "DETAILS" column
-   Navigate to the "Samples" tab for the sample metadata
    -   Click the small "Export to Excel" icon to download the metadata (.xls file)
-   Navigate to the "Results" tab for the analyte results
    -   Navigate to the "Export" sub-tab, click the "Export" icon to download the analyte results (.xls file)
-   Navigate to the "Documents" tab for additional information
    -   Click the "OPEN" button to download any subcontracted results, the pdf report and invoice
        -   Selenium is subcontracted to the ALS Kelso laboratory and is in a .csv format, this will need to be downloaded
        -   The Final.pdf file contains the list of samples and results in a pdf format, this will need to be downloaded but is not required for the ALS Data Cleaning Tool
        -   The Invoice.pdf file is not required, but may be needed
-   For each work order, at least three files (sample .xls, results .xls, and Final.pdf) should be downloaded, as well as the subcontracted (.csv) files, if applicable

```{=html}
<div class="alert alert-warning">
<strong>Warning!</strong> The ALS Data Cleaning Tool requires the metadata .xls file (under the "Samples" tab), the results .xls file (under the "Results tab") and the Kelso .csv file (if applicable, under the "Documents" tab). There is another .xlsx file with results under the "Documents" tab, THIS FILE WILL NOT WORK WITH THE ALS DATA CLEANING TOOL. 
</div>
```

## Uploading Data Files for the ALS Data Cleaning Tool

-   After downloading the data from the ALS portal, copy and paste the files into the "Confidential Data" folder within the "ALS-Data-Cleaning-Tool" folder

    -   Example: /Users/lauryn/Desktop/GitHub/ALS-Data-Cleaning-Tool/Confidential Data/2025

-   You are now ready to go!
