source(here::here('code', '0-setup.R'))

# Rearranges rows by duration and then compute cumulative vehicle 
# counts so that we can know how long it takes to get to 1, 10 and 50 
# vehicles for each GEOID, listing_year, inventory_type, and technology

get_min_times <- function(dt) {
  dt <- dt[order(GEOID, listing_year, inventory_type, technology, duration_min)]
  dt[,
    c('cum_count', 'past1', 'past5', 'past10', 'past20') := .(
      cumsum(count),
      cumsum(cumsum(count) >= 1), 
      cumsum(cumsum(count) >= 5),
      cumsum(cumsum(count) >= 10),
      cumsum(cumsum(count) >= 20)
    ), 
    by = c('GEOID', 'listing_year', 'inventory_type', 'technology')
  ]
  dt <- dt[(past1 == 1) | (past5 == 1) | (past10 == 1)]
  return(dt)
}

dealer_times <- read_parquet(here::here('data_local', 'dealer_times.parquet'))

# New and used separate ----

# All vehicles ----

dealer_dt <- read_parquet(
  # Start with the linear distances data
  here::here('data_local', 'sep', 'dealer_distances_all.parquet')
) %>% 
  # Join on trip duration and distances
  left_join(dealer_times, by = c('GEOID', 'dealer_id'))

dealer_dt <- get_min_times(dealer_dt)

write_parquet(dealer_dt, here('data_local', 'sep', 'min_times_all.parquet'))

# $25k vehicles ----

dealer_dt <- read_parquet(
  # Start with the linear distances data
  here::here('data_local', 'sep', 'dealer_distances_25.parquet')
) %>% 
  # Join on trip duration and distances
  left_join(dealer_times, by = c('GEOID', 'dealer_id'))

dealer_dt <- get_min_times(dealer_dt)

write_parquet(dealer_dt, here('data_local', 'sep', 'min_times_25.parquet'))





# Aggregate markets ----

# All vehicles ----

dealer_dt <- read_parquet(
  # Start with the linear distances data
  here::here('data_local', 'agg', 'dealer_distances_all.parquet')
) %>% 
  # Join on trip duration and distances
  left_join(dealer_times, by = c('GEOID', 'dealer_id')) 

dealer_dt <- get_min_times(dealer_dt)

write_parquet(dealer_dt, here('data_local', 'agg', 'min_times_all.parquet'))

# $25k vehicles ----

dealer_dt <- read_parquet(
  # Start with the linear distances data
  here::here('data_local', 'agg', 'dealer_distances_25.parquet')
) %>% 
  # Join on trip duration and distances
  left_join(dealer_times, by = c('GEOID', 'dealer_id')) 

dealer_dt <- get_min_times(dealer_dt)

write_parquet(dealer_dt, here('data_local', 'agg', 'min_times_25.parquet'))
