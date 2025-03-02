source(here::here("code", "0-setup.R"))

compute_burden <- function(df) {
  df <- df %>% 
    select(GEOID, listing_year, inventory_type, technology, duration_min) %>% 
    pivot_wider(names_from = technology, values_from = duration_min) %>%
    mutate(time_burden = bev - cv) %>% 
    filter(!is.na(time_burden)) %>% 
    select(-bev, -cv)
  return(df)
}

compute_burden_by_count <- function(df) {
  
  # Compute burden for each car count: 1, 5, and 10

  df_burden1 <- df_time %>%
    filter(past1 == 1) %>% 
    compute_burden() %>% 
    mutate(car_count = 1)
  
  df_burden5 <- df_time %>%
    filter(past5 == 1) %>% 
    compute_burden() %>% 
    mutate(car_count = 5)
  
  df_burden10 <- df_time %>%
    filter(past10 == 1) %>% 
    compute_burden() %>% 
    mutate(car_count = 10)
  
  # Combine 
  
  df_burden <- rbind(df_burden1, df_burden5, df_burden10)
  
  return(df_burden)
}

# New and used separate ----

# All vehicles ----

df_time <- read_parquet(here::here(
  'data_local', 'sep', 'min_times_all.parquet'))
df_burden <- compute_burden_by_count(df_time)
write_parquet(df_burden, here::here(
  'data_local', 'sep', 'burden_time_all.parquet'))

# $25k vehicles ----

df_time <- read_parquet(here::here(
  'data_local', 'sep', 'min_times_25.parquet'))
df_burden <- compute_burden_by_count(df_time)
write_parquet(df_burden, here::here(
  'data_local', 'sep', 'burden_time_25.parquet'))


# Aggregate markets ----


# All vehicles ----

df_time <- read_parquet(here::here(
  'data_local', 'agg', 'min_times_all.parquet'))
df_burden <- compute_burden_by_count(df_time)
write_parquet(df_burden, here::here(
  'data_local', 'agg', 'burden_time_all.parquet'))

# $25k vehicles ----

df_time <- read_parquet(here::here(
  'data_local', 'agg', 'min_times_25.parquet'))
df_burden <- compute_burden_by_count(df_time)
write_parquet(df_burden, here::here(
  'data_local', 'agg', 'burden_time_25.parquet'))

