# Mapping Electric Vehicle Accessibility in the United States

Replication code and data for paper *Mapping Electric Vehicle Accessibility in the United States*, by Lujin Zhao, Michael Mann, and John Paul Helveston

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

To replicate the results of this study, first download the ["data_local.zip"](https://filedn.com/lYURdAnVcCykBHec07i0c6j/ev-accessibility-2025/data_local.zip) file, unzip it, and put the resulting "data_local" folder in the root folder for this repository. This is a large folder (8.39GB) that contains the data needed to replicate the results of this study and is too large to put in this public repository.

The primary data is the "data_local/listings.parquet" file, which contains records of vehicle listings data. The original data is from marketcheck.com, but restrictions apply to the availability of the raw data, which were used under a license agreement for the current study and so are not publicly available. The "data_local/listings.parquet" file is a processed version of the original data that only includes the necessary variables to replicate the results in this study. These variables include:

Variable | Description
---|---------
"dealer_id" | Unique identifier for each dealership
"inventory_type" | New or used vehicle
"year" | Vehicle model year
"powertrain" | Vehicle powertrain: "conventional", "hybrid", "bev", or "phev"
"vehicle_type" | "car" or "suv"
"status_date" | Date listing was collected from dealership website
"listing_year" | Year vehicle was listed on dealership website
"state" | State vehicle is listed in
"latitude" | Latitude coordinate of dealership with vehicle listing
"longitude" | Longitude coordinate of dealership with vehicle listing
"price_under_25" | Logical (TRUE or FALSE): is the listing price under $25,000?

### Data preparation calculations

Once the "data_local" folder is in the project root folder, the code in the "code/data-prep" folder should be run one file at a time in sequential order to make the primary calculations needed to replicate the results in this study. Some of these calculations take a long time to run, please read the comments in each .R file carefully.

The other files in the "code" folder create the key figures and summary calculations in the paper. These include:

- bev-percent-dealer.R
- bev-percent-state.R
- bevs-per-dealer.R
- burden-map.R
- burden-plots.R
- tables.R
