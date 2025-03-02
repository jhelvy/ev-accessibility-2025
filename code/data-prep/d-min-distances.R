source(here::here('code', '0-setup.R'))

# Rearranges rows by duration and then compute cumulative vehicle 
# counts so that we can know how long it takes to get to 1, 10 and 50 
# vehicles for each GEOID, listing_year, inventory_type, and technology

get_min_distances <- function(dt) {
  dt <- dt[order(GEOID, listing_year, inventory_type, technology, distance)]
  dt <- dt[(past1 == 1) | (past5 == 1) | (past10 == 1), -c('past20')]
  dt[, past1 := ifelse(past1 == 1, 1, 0)]
  dt[, past5 := ifelse(past5 == 1, 1, 0)]
  dt[, past10 := ifelse(past10 == 1, 1, 0)]
  dt <- dt[(past1 == 1) | (past5 == 1) | (past10 == 1)]
  return(dt)
}

# New and used separate ----

# All vehicles ----

dealer_dt <- read_parquet(
  here::here('data_local', 'sep', 'dealer_distances_all.parquet')) %>% 
  get_min_distances()

write_parquet(dealer_dt, here('data_local', 'sep', 'min_distances_all.parquet'))

# $25k vehicles ----

dealer_dt <- read_parquet(
  here::here('data_local', 'sep', 'dealer_distances_25.parquet')) %>% 
  get_min_distances()

write_parquet(dealer_dt, here('data_local', 'sep', 'min_distances_25.parquet'))


# Aggregate markets ----

# All vehicles ----

dealer_dt <- read_parquet(
  here::here('data_local', 'agg', 'dealer_distances_all.parquet')) %>% 
  get_min_distances() 

write_parquet(dealer_dt, here('data_local', 'agg', 'min_distances_all.parquet'))

# $25k vehicles ----

dealer_dt <- read_parquet(
  here::here('data_local', 'agg', 'dealer_distances_25.parquet')) %>% 
  get_min_distances()

write_parquet(dealer_dt, here('data_local', 'agg', 'min_distances_25.parquet'))
