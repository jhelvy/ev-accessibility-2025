source(here::here('code','0-setup.R'))

path_acs <- here::here('data_local', 'acs')
path_tract <- file.path(path_acs, 'shape_file', 'us_tract')

files <- list.files(
  path=path_tract,
  pattern="\\.shp$",
  full.names=TRUE,
  recursive=TRUE
)
tract_sf <- read_sf(files[[1]])
listOfShp <- lapply(files, st_read)

# Look to make sure they're all in the same CRS
unique(sapply(listOfShp, st_crs))

# Harmonize CRS
st_crs(listOfShp[[29]]) <- st_crs(listOfShp[[30]])

# Combine the list of sf objects into a single object
tract_sf <- do.call(what = sf:::rbind.sf, args=listOfShp)

# Import demographic data -----

df_edu <- read_csv(
  file.path(path_acs, 'education', "ACSST5Y2019.S1501-Data.csv"),
  skip = 1, col_select = c(1,13,31)) %>%
  rename(
    'bachelor_above' = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher",
    'population_over25' = "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over",
    'GEOID' = "Geography"
  ) %>%
  mutate(GEOID = str_replace(GEOID, '1400000US', ''))

tract_sf <- merge(tract_sf, df_edu, by = 'GEOID')

df_income <- read_csv(
  file.path(path_acs, 'income', 'ACSST5Y2019.S1903-Data.csv'),
  skip = 1,col_select = c(1,191)
) %>%
  rename(
    'hh_median_income' = "Estimate!!Median income (dollars)!!FAMILIES!!Families",
    'GEOID' = "Geography"
  ) %>%
  mutate(GEOID = str_replace(GEOID, '1400000US', ''))

tract_sf <- merge(tract_sf, df_income, by = 'GEOID')

df_pop <- read_csv(
  file.path(path_acs, 'population','ACSDP5Y2019.DP05-Data.csv'),
  skip = 1,
  col_select = c(1,3)
) %>%
  rename(
    'pop' = "Estimate!!SEX AND AGE!!Total population",
    'GEOID' = "Geography"
  ) %>%
  mutate(GEOID = str_replace(GEOID, '1400000US', ''))

tract_sf <- merge(tract_sf, df_pop,by = 'GEOID')

# Convert STATEFP into state names
fips_dict <- read_csv(here::here('data_local', 'us-state-ansi-fips.csv')) %>% 
  rename(
    'STATEFP' = 'st',
    'state' = 'stusps'
  )

tract_sf <- merge(tract_sf, fips_dict, by = 'STATEFP')

# Finalize data formatting and cleaning
tract_sf <- tract_sf %>% 
  select(
    GEOID, state, stname, area_land = ALAND, pop_over25 = population_over25, 
    bachelor_above, med_inc_hh = hh_median_income, pop, geometry
  ) %>%
  filter(pop > 0) %>% 
  mutate(
    # ZEV status
    zev = ifelse(state %in% zev_state,'ZEV','Non-ZEV'),
    # Population class status (urban, rural, suburban)
    area_sq_miles = area_land / 2589988.11, # Convert sq meters to sq miles
    pop_density = pop / area_sq_miles,
    class = ifelse(
      pop_density <= 500, 'Rural', ifelse(
        pop_density >= 1000, 'Urban', 'Suburban'
      ))
  )

# Compute centroids to get lat-long coordinates, then drop the geometries
tract_dt <- tract_sf %>% 
  st_centroid() %>%
  mutate(
    lat_c = st_coordinates(geometry)[,"Y"],
    lng_c = st_coordinates(geometry)[,"X"]
  ) %>%
  st_drop_geometry() %>%
  as.data.table()

write_parquet(tract_dt, here::here('data', 'tract_dt.parquet'))
