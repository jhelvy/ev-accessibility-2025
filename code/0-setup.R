# Load packages

library(tidyverse)
library(arrow)
library(here)
library(data.table)
library(cowplot)
library(kableExtra)
library(zoo)
library(viridis)
library(tigris)
library(sf)
library(qs)
library(rvest)
library(osrm)
library(ggridges)
library(future)
library(furrr)
library(scattermore)
library(ggtext)
library(tidygeocoder)

options(arrow.unsafe_metadata = TRUE)

# Load the main listings dataset
path_listings <- here::here('data_local', 'listings.parquet')
ds <- open_dataset(path_listings)

# Plot options

font <- 'Roboto Condensed'
color_cv <- "grey42"
color_ev <- "#00BA38"
color_tesla <- "#619CFF"
color_nontesla <- "firebrick"

# globals

zev_state <- c(
  'CA','CO','CT','ME','MA','MD','NJ','NY','OR','RI','VT','WA'
)

non_us_state <- c(
  'AB','BC','GU','MB','MP','ON','PR',"QC",'VIC','WC','SK','NS','AS'
)

time_zones <- list(
  Eastern = c("CT", "DE", "GA", "IN", "KY", "ME", "MD", "MA", "MI", "NH", "NJ", "NY", 
              "NC", "OH", "PA", "RI", "SC", "TN", "VT", "VA", "WV", "FL"),
  
  Central = c("AL", "AR", "IL", "IA", "KS", "KY", "LA", "MN", "MS", "MO", "NE", "ND", 
              "OK", "SD", "TN", "TX", "WI"),
  
  Mountain = c("AZ", "CO", "ID", "MT", "NE", "NM", "ND", "SD", "UT", "WY"),
  
  Pacific = c("CA", "NV", "OR", "WA", "ID")
)

# Reverse the list into a named vector for easier lookup

state_to_time_zone <- unlist(lapply(names(time_zones), function(tz) {
  setNames(rep(tz, length(time_zones[[tz]])), time_zones[[tz]])
}))

# functions ----

`%notin%` = negate(`%in%`)

make_dealer_dict <- function(ds) {
  ds %>%
    distinct(dealer_id, state, latitude, longitude) %>%
    collect()
}

save_raw <- function(text, path) {
  fileConn <- file(path)
  writeLines(text, fileConn)
  close(fileConn)
}
