source(here::here('code','0-setup.R'))

#caculate linear distance using lat and lng
linear_dist <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Earth's radius in kilometers
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  # Distance in kilometers
  return( R * c) # Distance in kilometers
}

format_dealer_counts <- function(path) {
  
  dealer_counts <- read_parquet(path) %>% 
    pivot_longer(
      names_to = 'technology',
      values_to = 'count',
      cols = bev:cv
    ) 
  
  # Join counts and coords
  dealer_counts <- as.data.table(
    merge(dealer_counts, coords_dealer, by = 'dealer_id', all.x = TRUE)
  )
  
  return(dealer_counts[count > 0])
}

# For a given census tract, find the distances to all the dealerships
get_distances <- function(tract, dealer_counts) {
  temp <- copy(dealer_counts)
  temp[, distance := linear_dist(
    lat_d, lng_d, tract$lat_c, tract$lng_c)]
  temp <- temp[distance <= 300]
  temp <- temp[order(distance), -c('lat_d', 'lng_d')]
  temp[,
    c('cum_count', 'past1', 'past5', 'past10', 'past20') := .(
      cumsum(count),
      cumsum(cumsum(count) >= 1), 
      cumsum(cumsum(count) >= 5),
      cumsum(cumsum(count) >= 10),
      cumsum(cumsum(count) >= 20)
    ), 
    by = c('listing_year', 'inventory_type', 'technology')
  ]
  temp <- temp[past20 <= 1]
  temp <- temp[order(listing_year, inventory_type, technology, distance, desc(count)), ]
  temp$GEOID <- tract$GEOID
  return(temp)
}

# Run get_distances for every census tract
get_distances_all <- function(dealer_counts) {
  tictoc::tic()

  temp <- parallel::mclapply(1:nrow(coords_tract), function(i) {
  # temp <- parallel::mclapply(1:1000, function(i) {
    get_distances(coords_tract[i,], dealer_counts)
  }, mc.cores = 3)
  
  df <- rbindlist(temp)
  
  tictoc::toc()
  
  return(df)
}

get_distances_agg <- function(df) {
  df <- df[order(GEOID, listing_year, technology, distance, desc(count)), -c('inventory_type')]
  df[,
     c('cum_count', 'past1', 'past5', 'past10', 'past20') := .(
       cumsum(count),
       cumsum(cumsum(count) >= 1), 
       cumsum(cumsum(count) >= 5),
       cumsum(cumsum(count) >= 10),
       cumsum(cumsum(count) >= 20)
     ), 
     by = c('GEOID', 'listing_year', 'technology')
  ]
  df <- df[past20 <= 1]
  return(df)
}

# Format the tesla data
coords_tesla <- read_parquet(here::here('data', 'counts', 'tesla.parquet')) %>% 
  select(dealer_id, lat_d, lng_d) %>% 
  distinct()
coords_dealer <- make_dealer_dict(ds) %>% 
  select(dealer_id, lat_d = latitude, lng_d = longitude) %>% 
  bind_rows(coords_tesla) %>% 
  distinct() %>% 
  as.data.table()

# Format census tract data
coords_tract <- read_parquet(here::here('data', 'tract_dt.parquet')) %>% 
  select(lat_c, lng_c, GEOID) %>% 
  unique()

# All vehicles ----

# Compute distances to closest 100 vehicle listings for each 
# GEOID, listing year, inventory type, and powertrain

dealer_counts <- format_dealer_counts(
  here::here('data', 'counts', 'dealer_all_sep.parquet')) %>% 
  filter(technology %in% c('bev', 'cv'))
df <- get_distances_all(dealer_counts) # 590.332 sec elapsed, 9.8 mins
write_parquet(
  df, 
  here::here('data_local', 'sep', 'dealer_distances_all.parquet')
)

# $25k vehicles ----

# Same as above, but for only vehicles under $25,000

dealer_counts <- format_dealer_counts(
  here::here('data', 'counts', 'dealer_25_sep.parquet')) %>% 
  filter(technology %in% c('bev', 'cv'))
df <- get_distances_all(dealer_counts) # 2302.764 sec elapsed, 0.64 hours
write_parquet(
  df, 
  here::here('data_local', 'sep', 'dealer_distances_25.parquet')
)





# All vehicles, aggregate market ----

# Compute distances to closest 100 vehicle listings for each 
# GEOID, listing year, inventory type, and powertrain

df <- read_parquet(
  here::here('data_local', 'sep', 'dealer_distances_all.parquet')) %>% 
  get_distances_agg()
df$inventory_type <- 'all'
write_parquet(
  df, 
  here::here('data_local', 'agg', 'dealer_distances_all.parquet')
)

# $25k vehicles ----

# Same as above, but for only vehicles under $25,000

df <- read_parquet(
  here::here('data_local', 'sep', 'dealer_distances_25.parquet')) %>% 
  get_distances_agg()
df$inventory_type <- 'all'
write_parquet(
  df, 
  here::here('data_local', 'agg', 'dealer_distances_25.parquet')
)
