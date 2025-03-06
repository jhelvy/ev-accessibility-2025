source(here::here('code', '0-setup.R'))

make_dealer_counts <- function(ds) {
  dealer_dt <- ds %>% 
    group_by(dealer_id, powertrain, listing_year, inventory_type) %>%
    count(dealer_id, powertrain, listing_year, inventory_type) %>%
    summarise(vehicle_count = sum(n)) %>%
    collect() %>%
    pivot_wider(names_from = powertrain, values_from = vehicle_count) %>% 
    ungroup() %>% 
    as.data.table()
  dealer_dt[is.na(dealer_dt)] <- 0
  dealer_counts <- dealer_dt[, .(
    bev = sum(bev),
    hev = sum(hybrid), 
    phev = sum(phev),
    cv = sum(conventional)
  ), by = .(dealer_id, listing_year, inventory_type)]
  return(dealer_counts)
}

# Print quick summary

ds %>% 
  count(powertrain) %>%
  collect()

# Read in Tesla counts

counts_tesla <- read_parquet(here::here('data', 'counts', 'tesla.parquet')) %>% 
  select(-lat_d, -lng_d)

# Dealer counts - new and used

dealer_counts_all <- make_dealer_counts(ds) %>% 
  rbind(counts_tesla)
write_parquet(
  dealer_counts_all,
  here::here('data', 'counts', 'dealer_all_sep.parquet')
)

dealer_counts_25 <- make_dealer_counts(ds %>% filter(price_under_25))
write_parquet(
  dealer_counts_25,
  here::here('data', 'counts', 'dealer_25_sep.parquet')
)

# Dealer counts - aggregate markets

dealer_counts_all_agg <- dealer_counts_all %>% 
  group_by(dealer_id, listing_year) %>%
  summarise(
    cv = sum(cv), 
    bev = sum(bev), 
    hev = sum(hev), 
    phev = sum(phev) 
  ) %>% 
  mutate(inventory_type = 'all') %>% 
  as.data.table() %>%
  select(dealer_id, listing_year, inventory_type, cv, bev, hev, phev) %>% 
  arrange(dealer_id, listing_year)

write_parquet(
  dealer_counts_all_agg,
  here::here('data', 'counts', 'dealer_all_agg.parquet')
)

dealer_counts_25_agg <- dealer_counts_25 %>% 
  group_by(dealer_id, listing_year) %>%
  summarise(
    cv = sum(cv), 
    bev = sum(bev),
    hev = sum(hev), 
    phev = sum(phev) 
  ) %>% 
  mutate(inventory_type = 'all') %>% 
  as.data.table() %>%
  select(dealer_id, listing_year, inventory_type, cv, bev, hev, phev) %>% 
  arrange(dealer_id, listing_year)
write_parquet(
  dealer_counts_25_agg,
  here::here('data', 'counts', 'dealer_25_agg.parquet')
)

# State counts

counts_state_powertrain <- ds %>%
  filter(! state %in% non_us_state) %>%
  group_by(
    state, status_date, inventory_type, powertrain, vehicle_type
  ) %>%
  summarise(n = n()) %>%
  collect() %>% 
  mutate(
    state = stringr::str_trim(state),
    year = year(status_date),
    month = month(status_date)
  ) %>% 
  group_by(
    state, year, month, inventory_type, powertrain, vehicle_type
  ) %>%
  summarise(n = sum(n)) %>% 
  ungroup()

arrow::write_parquet(
  counts_state_powertrain,
  here::here('data', 'state_powertrain.parquet')
)
