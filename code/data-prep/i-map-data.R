source(here::here('code', '0-setup.R'))

# Get state border geometries
states_sf <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c('AK', 'HI', 'PR', 'VI', 'AS', 'MP', 'GU'))

# Get census tract geometries for the entire US
# Note: This might take a while to download
tracts <- tracts(cb = TRUE, year = 2020) %>%
  st_transform(crs = "ESRI:102003") %>%   # Transform to Albers projection
  janitor::clean_names() %>% 
  filter(!state_name %in% c(
    "United States Virgin Islands", 
    "American Samoa",
    "Puerto Rico",
    "Commonwealth of the Northern Mariana Islands",
    "Guam",
    "Hawaii",
    "Alaska"
  ))

qs::qsave(states_sf, here::here('data', 'states_sf.qs'))
qs::qsave(tracts, here::here('data', 'tracts.qs'))
