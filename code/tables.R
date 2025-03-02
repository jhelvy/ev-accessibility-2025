source(here::here('code','0-setup.R'))

# Print quick summary of listings
ds %>% 
  count(vehicle_type, inventory_type) %>% 
  collect() %>% 
  mutate(n = n / 10^6)

# Print number of unique dealers
make_dealer_dict(ds) %>% 
  select(dealer_id) %>% 
  collect() %>% 
  distinct() %>% 
  pull(dealer_id) %>% 
  length()

summary_df <- ds %>%
  mutate(
    age = listing_year-year,
    powertrain = if_else(make == 'tesla', 'tesla', powertrain)
  ) %>% 
  select(
    vehicle_type,inventory_type,dealer_id, powertrain, age, model, price
  ) %>% 
  collect() %>%
  group_by(vehicle_type,inventory_type,powertrain) %>%
  summarize(
    n_listings = scales::comma(n()),
    n_models = as.character(length(unique(model))),
    n_dealers = scales::comma(length(unique(dealer_id))),
    age_mean = as.character(round(mean(age, na.rm = TRUE), 1)),
    price_mean = scales::dollar(round(mean(price, na.rm = TRUE)))
  ) %>% 
  ungroup()

# Format for latex
latex_df <- summary_df %>%
    mutate(
        powertrain = fct_recode(
            powertrain,
            'BEV (Non-Tesla)' = 'bev',
            'BEV (Tesla)' = 'tesla',
            'Conventional' = 'conventional'
        )
    ) %>%
    pivot_longer(
        names_to = 'stat',
        values_to = 'val',
        cols = c('n_listings', 'n_models', 'n_dealers', 'age_mean', 'price_mean')
    ) %>%
    mutate(
      stat = fct_recode(
        stat,
        'Num. of Listings' = 'n_listings',
        'Num. of Models' = 'n_models',
        'Num. of Dealerships' = 'n_dealers',
        'Mean age (years)' = 'age_mean',
        'Mean price ($USD)' = 'price_mean'
      )
    ) %>% 
    pivot_wider(
        names_from = powertrain,
        values_from = val
    ) %>% 
    mutate(
      `BEV (Tesla)` = ifelse(is.na(`BEV (Tesla)`), '--', `BEV (Tesla)`)
    ) %>% 
    select(
      ` ` = stat, vehicle_type, inventory_type, 
      Conventional, everything()
    ) 

# Save by each vehicle type and inventory type
latex_df %>% 
  filter(vehicle_type == 'car', inventory_type == 'new') %>% 
  filter(!str_detect(` `, 'age')) %>% 
  select(-vehicle_type, -inventory_type) %>% 
  kbl(format = "latex", booktabs = TRUE, linesep = "") %>%
  kable_styling() %>% 
  save_raw(here::here('tables', 'data_summary_car_new.txt'))
latex_df %>% 
  filter(vehicle_type == 'car', inventory_type == 'used') %>% 
  select(-vehicle_type, -inventory_type) %>% 
  kbl(format = "latex", booktabs = TRUE, linesep = "") %>%
  kable_styling() %>% 
  save_raw(here::here('tables', 'data_summary_car_used.txt'))
latex_df %>% 
  filter(vehicle_type == 'suv', inventory_type == 'new') %>% 
  filter(!str_detect(` `, 'age')) %>% 
  select(-vehicle_type, -inventory_type) %>% 
  kbl(format = "latex", booktabs = TRUE, linesep = "") %>%
  kable_styling() %>% 
  save_raw(here::here('tables', 'data_summary_suv_new.txt'))
latex_df %>% 
  filter(vehicle_type == 'suv', inventory_type == 'used') %>% 
  select(-vehicle_type, -inventory_type) %>% 
  kbl(format = "latex", booktabs = TRUE, linesep = "") %>%
  kable_styling() %>% 
  save_raw(here::here('tables', 'data_summary_suv_used.txt'))
