Replication code and data for paper _Mapping Electric Vehicle Accessibility in the United States_, by Lujin Zhao, Michael Mann, and John Paul Helveston

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

Once the "data_local" folder is in the project root folder, the code in the "code/data-prep" folder can be run one file at a time to make the primary calculations needed to replicate the results in this study. Some of these calculations take a long time to run, please read the comments in each .R file carefully.

The other files in the "code" folder create the key figures and summary calculations in the paper. These include:

- bev-percent-dealer.R
- bev-percent-state.R
- bevs-per-dealer.R
- burden-map.R
- burden-plots.R
- tables.R
