source(here::here('code','0-setup.R'))

# Get counts by dealer
dt <- ds %>% 
  filter(listing_year < 2022) %>% 
  filter(! state %in% non_us_state) %>%
  group_by(listing_year, state, dealer_id, powertrain, inventory_type) %>%
  count(listing_year, state, dealer_id, powertrain, inventory_type) %>%
  summarise(vehicle_count = sum(n)) %>%
  collect() %>%
  pivot_wider(names_from = powertrain, values_from = vehicle_count) %>% 
  ungroup() %>% 
  as.data.table()
dt[is.na(dt)] <- 0
dealer_counts <- dt[, .(
  bev = sum(bev),
  cv = sum(conventional), 
  hev = sum(hybrid), 
  phev = sum(phev)
), by = .(listing_year, state, dealer_id, inventory_type)]
dealer_counts <- dealer_counts %>% 
  mutate(
    state_type = ifelse(state %in% zev_state, 'ZEV', 'Non-ZEV'),
    state_type = ifelse(state == 'CA', 'CA', state_type)
  ) 
  
# Get vehicles per dealer by ZEV status
bevs_per_dealer <- dealer_counts %>% 
  group_by(listing_year, state_type, inventory_type) %>% 
  summarise(
    n_dealers = length(unique(dealer_id)), 
    bev = sum(bev), 
    cv = sum(cv)
  ) %>% 
  ungroup() %>% 
  mutate(
    cv_per_dealer = round(cv / n_dealers), 
    bev_per_dealer = round(bev / n_dealers)
  ) %>% 
  select(listing_year, state_type, inventory_type, bev_per_dealer) %>% 
  as.data.table()

bevs_per_dealer_national <- dealer_counts %>% 
  group_by(listing_year, inventory_type) %>% 
  summarise(
    n_dealers = length(unique(dealer_id)), 
    bev = sum(bev)
  ) %>% 
  ungroup() %>% 
  mutate(
    bev_per_dealer = round(bev / n_dealers)
  ) %>% 
  select(listing_year, inventory_type, bev_per_dealer) %>% 
  mutate(state_type = 'National') %>% 
  select(state_type, everything()) %>% 
  as.data.table()

bevs_per_dealer <- bind_rows(
  bevs_per_dealer,
  bevs_per_dealer_national
) 

bevs_per_dealer %>%
  ggplot() +
  geom_line(aes(x = listing_year, y = bev_per_dealer, color = state_type)) +
  facet_wrap(vars(inventory_type))

# % BEVs at dealers

bev_p <- dealer_counts %>% 
  group_by(listing_year, state_type, inventory_type) %>% 
  summarise(
    bev = sum(bev), 
    cv = sum(cv), 
    hev = sum(hev), 
    phev = sum(phev)
  ) %>% 
  ungroup() 

bev_p_national <- dealer_counts %>% 
  group_by(listing_year, inventory_type) %>% 
  summarise(
    bev = sum(bev), 
    cv = sum(cv), 
    hev = sum(hev), 
    phev = sum(phev)
  ) %>% 
  ungroup() %>% 
  mutate(state_type = 'National') %>% 
  select(state_type, everything())

bev_p <- bind_rows(bev_p, bev_p_national) %>% 
  mutate(
    total = bev + cv + hev + phev, 
    p_bev = bev / total
  ) %>% 
    select(listing_year, state_type, inventory_type, p_bev)

bev_p %>% 
  ggplot() +
  geom_line(aes(x = listing_year, y = p_bev, color = state_type)) +
  facet_wrap(vars(inventory_type))

# Table

bevs_per_dealer %>% 
  filter(listing_year %in% c(2016, 2021)) %>% 
  pivot_wider(names_from = listing_year, values_from = bev_per_dealer) %>% 
  rename(
    bev_per_dealer16 = `2016`, 
    bev_per_dealer21 = `2021`
  ) %>% 
  left_join(
    bev_p %>% 
      filter(listing_year %in% c(2016, 2021)) %>% 
      pivot_wider(names_from = listing_year, values_from = p_bev) %>% 
      rename(
        p_bev16 = `2016`, 
        p_bev21 = `2021`
      )    
  ) %>% 
  mutate(
    state_type = factor(state_type, c(
      'National', 'CA', 'ZEV', 'Non-ZEV'
    )), 
    p_bev16 = scales::percent(p_bev16, accuracy = 0.1), 
    p_bev21 = scales::percent(p_bev21, accuracy = 0.1)
  ) %>% 
  select(
    inventory_type, state_type, 
    bev_per_dealer16, bev_per_dealer21, 
    p_bev16, p_bev21) %>% 
  arrange(inventory_type, state_type) %>% 
  kbl(format = "latex", booktabs = TRUE, linesep = "") %>%
  kable_styling() %>%
  save_raw(here::here('tables', 'bev_percents.txt'))

