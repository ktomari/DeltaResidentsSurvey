# Delta Residents Survey 2023

Thank you for your interest in the *source code repository* for the 2023 Delta Residents Survey. For more information about the survey itself (including reports and documentation), visit the (separate) [DRS GitHub Page](https://ktomari.github.io/DeltaResidentsSurvey/).

## Quickstart

For a more complete guide, please read the [Core Functions Documentation](https://ktomari.github.io/DeltaResidentsSurvey/doc_drs_functions.html).

1. Download drs_functions.R, or clone this repository using git.

2. Begin writing your script for analyzing the DRS data. At the top of this script, write `source('drs_functions.R')`. Please note that the argument inside `source()` is a string representing the path to the script which you downloaded in step 1. By running this `source` command, you will store the core data loading function, `drs_read()` to your R Environment.

3. Download a copy of the [DRS 2023 data set](https://www.openicpsr.org/openicpsr/project/195447/version/V1/view?path=/openicpsr/195447/fcr:versions/V1/DRS-public-data_2023_12_01.zip&type=file) onto your local machine. If you cloned the repository, create a `/data` directory in the clone directory, and place the unzipped folder in `/data`. 

4. Copy the path to the unzipped directory (eg. "User/Documents/DeltaResidentsSurvey/data/public_data"). While this directory should contain three files (see below), the path you need is to the directory, not the files within. If you're working from an R Project of a clone of this repository, your can use a relative path, eg "/data/public_data".

    A. Data Dictionary (xlsx)
    B. Data (csv)
    C. Hash (txt)

5. Finally, execute `data <- drs_read('YOUR/PATH/HERE')` to read the data into the R environment. 

### Quickstart Notes

Please review the documentation for further information describing the different "missingness" levels present in our data set. As a quick summary, please note that various columns contain responses that convey an absence of data using angle brackets, ie. `<` and `>`. You may quickly convert these to `NA` values by including the argument `convert_to_NA = T` in `drs_read()`.

**Caution:** While you may load the data by simply running `read.csv`, we encourage you to use the functions provided in the file "drs_functions.R". This readily provides a reproducible approach that properly structures the DRS data set. The csv file, *as is*, fails to reproduce the complete data set. The metadata included in the data dictionary provides important information regarding the variable types, and wherever relevant, ordinal levels.

## Navigation

* /admin - This directory contains miscellaneous code for administering this repository. It does not contain DRS data relevant code.

## Versions & Dependencies

R version 4.3.2 (2023-10-31)

|package|loadedversion|date      |source        |
|-------|-------------|----------|--------------|
|digest |0.6.33       |2023-07-07|CRAN (R 4.3.1)|
|dplyr  |1.1.3        |2023-09-03|CRAN (R 4.3.1)|
|forcats|1.0.0        |2023-01-29|CRAN (R 4.3.1)|
|purrr  |1.0.2        |2023-08-10|CRAN (R 4.3.1)|
|readr  |2.1.4        |2023-02-10|CRAN (R 4.3.1)|
|readxl |1.4.3        |2023-07-06|CRAN (R 4.3.1)|
|stringr|1.5.0        |2022-12-02|CRAN (R 4.3.1)|
|tibble |3.2.1        |2023-03-20|CRAN (R 4.3.1)|
|tidyr  |1.3.0        |2023-01-24|CRAN (R 4.3.1)|

## License

Forthcoming. In the intervening period, for information about licensing this source code, please contact Dr. Jessica Rudnick at UCSD/SeaGrant.

