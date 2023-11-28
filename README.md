# Delta Residents Survey 2023

Thank you for your interest in the source code *repository* for the 2023 Delta Residents Survey. For more information about the survey itself (including reports and documentation on methods), visit the (separate) [DRS GitHub Page](https://ktomari.github.io/DeltaResidentsSurvey/).

## Quickstart

First, download a copy of the DRS 2023 data set onto your local machine and begin writing *your* script with the line `source('drs_functions.R')`. This command loads the script with functions relevant to the DRS data. (At a future date, these functions will be incorporated into an R package.)

Next, determine the path of your local copy of the DRS data set. Importantly, the path we desire is the path to the **directory** containing the:

1. Data Dictionary (xlsx)
2. Data (csv)
3. Hash (txt)

While each of these files is needed to load the data, we do not need their individual file paths. While you are free to set the name of the directory as you please, do not change the names of the files themselves.

Finally, execute `data <- drs_read('YOUR/PATH/HERE')` to read the data into the R environment. 

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

