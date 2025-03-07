source(here::here("code", "0-setup.R"))

compute_burden <- function(df) {
  df <- df %>% 
    select(GEOID, listing_year, inventory_type, technology, distance) %>% 
    pivot_wider(names_from = technology, values_from = distance) %>%
    mutate(distance_burden = bev - cv) %>% 
    filter(!is.na(distance_burden)) %>% 
    select(-bev, -cv)
  return(df)
}

compute_burden_by_count <- function(df) {
  
  # Compute burden for each car count: 1, 10, and 50

  df_burden1 <- df %>%
    filter(past1 == 1) %>% 
    compute_burden() %>% 
    mutate(car_count = 1)
  
  df_burden5 <- df %>%
    filter(past5 == 1) %>% 
    compute_burden() %>% 
    mutate(car_count = 5)
  
  df_burden10 <- df %>%
    filter(past10 == 1) %>% 
    compute_burden() %>% 
    mutate(car_count = 10)
  
  # Combine 
  
  df_burden <- rbind(df_burden1, df_burden5, df_burden10)
  
  return(df_burden)
}

# New and used separate ----

# All vehicles ----

df <- read_parquet(here::here('data_local', 'sep', 'min_distances_all.parquet'))
df_burden <- compute_burden_by_count(df)
write_parquet(df_burden, here::here('data_local', 'sep', 'burden_distance_all.parquet'))


# $25k vehicles ----

df <- read_parquet(here::here('data_local', 'sep', 'min_distances_25.parquet'))
df_burden <- compute_burden_by_count(df)
write_parquet(df_burden, here::here('data_local', 'sep', 'burden_distance_25.parquet'))




# Aggregate markets ----


# All vehicles ----

df <- read_parquet(here::here('data_local', 'agg', 'min_distances_all.parquet'))
df_burden <- compute_burden_by_count(df)
write_parquet(df_burden, here::here('data_local', 'agg', 'burden_distance_all.parquet'))


# $25k vehicles ----

df <- read_parquet(here::here('data_local', 'agg', 'min_distances_25.parquet'))
df_burden <- compute_burden_by_count(df)
write_parquet(df_burden, here::here('data_local', 'agg', 'burden_distance_25.parquet'))
