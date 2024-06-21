# Delta Residents Survey 2023

Thank you for your interest in the the 2023 Delta Residents Survey. For more information about the survey itself (including reports and documentation), visit the (separate) [DRS GitHub Page](https://ktomari.github.io/DeltaResidentsSurvey/).

## Source Code

This repository largely contains scripts and files used to create the [DRS GitHub Page](https://ktomari.github.io/DeltaResidentsSurvey/), a home page for our research project (eg. the directory `/docs`). While we encourage researchers to utilize the [package {cdrs}](https://github.com/ktomari/cdrs) for analyzing the DRS data set, this repository also offers the directory `/code` which contains R scripts. These scripts exist here primarily for two reasons:

1. As an archive for an earlier version of the source code used to read the DRS data set (primarily, `drs_functions.R`). This code is now largely integrated into the package {cdrs} and has been superseded. See [Core Functions Doc](https://ktomari.github.io/DeltaResidentsSurvey/doc_drs_functions.html).
2. Scripts that utilize the {cdrs} package to produce analyses or visualizations used in other documents.
