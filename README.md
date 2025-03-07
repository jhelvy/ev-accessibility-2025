# Mapping Electric Vehicle Accessibility in the United States

Replication code and data for paper *Mapping Electric Vehicle Accessibility in the United States*, by Lujin Zhao, Michael Mann, and John Paul Helveston

All content is licensed under a [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/) (CC BY-SA 4.0)

## Installation

To replicate our results in this study, ensure that you have properly installed the required software. First, install the latest version of R and RStudio (RStudio is not required, but highly recommended):
- [Download and install R](https://cloud.r-project.org) (version 4.4.0 or higher recommended)
- [Download and install RStudio](https://rstudio.com/products/rstudio/download/) (Desktop version)

### Setting up the environment using renv

This repository uses the `renv` package to create a reproducible environment with the exact package versions used in our analysis. To set up this environment:

1. Clone this repository to your local machine.
2. Open the project in RStudio by clicking on the `.Rproj` file (or open R in the project directory).
3. The `renv` package should automatically be installed when you open the project.
4. Run the following command to install all required packages with their correct versions:

```r
renv::restore()
```

This may take some time as it installs all necessary packages. Once completed, you'll have an identical R environment to the one used for our analysis.

## Running the Analysis

### Data setup

To replicate the results of this study, first download the ["data_local.zip"](https://filedn.com/lYURdAnVcCykBHec07i0c6j/ev-accessibility-2025/data_local.zip) file, unzip it, and put the resulting "data_local" folder in the root folder for this repository. This is a large folder (1.57GB) that contains the vehicle listings data and US Census data needed to replicate the results of this study and is too large to put in this public repository. A detailed description of each file in the folder is available in the "data_local/README.md" file.

### Data preparation calculations

Once the "data_local" folder is in the project root folder, the code in the "code/1-data-prep" folder should be run one file at a time in sequential order to make the primary calculations needed to replicate the results in this study. Each of these have already been run and the resulting data files are stored in the "data" or "data_local" folders. However, if you would like replicate these files from the original data sources, run each file in sequential order. Some of these calculations take a long time to run, please read the comments in each .R file carefully. Some data files end in "_all" and others end in "_25" - the difference is a reflection of the vehicle prices, with "_all" referring to all vehicles regardless of price, and "_25" referring to only vehicles under $25,000 in price. This table summarizes each data prep file:

File name | Expected run time (min) | Expected outcome
----------|-------------------|--------------------
1-census.R | 1.2 | Formats census tract demographic and shape data at "data/tract_dt.parquet"
2-tesla-locations.R | 4.6 | Geocodes Tesla stores stored in "data/tesla_dealer.csv" and makes Tesla BEV counts in "data/counts/tesla.parquet"
3-counts.R | 0.5 | Creates all vehicle summary counts data files in the "data/counts" folder except the "tesla.parquet" file.
4-distances.R | 16.1 | Computes linear distances from census tract centroids to nearby dealerships, with results stored in the "data_local" folder as "dealer_distances_all.parquet" and "dealer_distances_25.parquet"
5-distance-to-time.R | 75 | This file computes the driving time between all pairs of census tract centroids and dealerships in the "dealer_distances_all.parquet" and "dealer_distances_25.parquet" files. These travel times are stored in the  "data_local/dealer_times.parquet" file.
6-min-times.R | 0.2 | Creates the "min_times_all.parquet" and "min_times_25.parquet" files in the "data_local" folder. These contain the driving times to the closest BEV and CV from each census tract centroid.
7-time-burden.R | 0.05 | Creates the "burden_time_all.parquet" and "burden_time_25.parquet" files in the "data_local" folder. These take the minimum times computed in the last step and calculate the "burden", which is the difference between the BEV and CV times.
8-map-data.R | 0.09 | Retrieves and saves the census tract shape file data from the {tigris} package and saves the files in "data/states_sf.qs" and "data/tracts.qs".

### Analysis calculations

To replicate all analysis figures created in the study, run each .R file in sequential order in the "code/2-analysis" folder. This table summarizes each file:

File name | Expected run time (sec) | Expected outcome
----------|-------------------|--------------------
1-bev-percent-dealer.R | 5.0 | Creates figures "figs/dealer_bev_percent_all_markets.png" and "figs/dealer_bev_percent_new_used.png"
2-bev-percent-state.R | 1.9 | Creates figure "figs/bars_bev_percent.png"
3-bevs-per-dealer.R | 2.5 | Creates summary table of BEVs per dealership, saved in "tables/bev_percents.txt"
4-burden-plots.R | 00 | text
5-burden-map.R | 00 | text

The last code file in this folder, "6-listings-summary-tables.R", cannot be run using the provided data in the "data_local" folder. This is because it requires additional data about the vehicles in the listings, including the model name, price, etc. These data cannot be made publicly available. The resulting summary tables of the vehicle listings are saved in "tables/data_summary..." txt files.
