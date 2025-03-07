source(here::here('code', '0-setup.R'))
library(patchwork)

# State border geometries
states_sf <- qs::qread(here::here('data', 'states_sf.qs'))

# Get census tract geometries for the entire US
tracts <- qs::qread(here::here('data', 'tracts.qs'))

# Burden data
df_all <- read_parquet(here::here('data_local', 'burden_time_all.parquet')) %>% 
  filter(listing_year != 2022) %>% 
  filter(car_count == 1) %>% 
  select(-car_count, -inventory_type) %>% 
  mutate(price_range = 'Any Price')
df_25 <- read_parquet(here::here('data_local', 'burden_time_25.parquet')) %>% 
  filter(listing_year != 2022) %>% 
  filter(car_count == 1) %>% 
  select(-car_count, -inventory_type) %>% 
  mutate(price_range = 'Price <$25,000')
df <- bind_rows(df_all, df_25)

# Map of all data ----

# Join the time data with geographic data
tracts_with_times <- tracts %>%
  left_join(df, by = c("geoid" = "GEOID")) %>% 
  janitor::clean_names() %>% 
  select(time_burden, listing_year, price_range, geometry)

# Map by price ----

map <- tracts_with_times %>%
  filter(listing_year %in% c(2016, 2021)) %>%
  ggplot() +
  geom_sf(aes(fill = time_burden), color = NA) +
  # Add state borders
  geom_sf(data = states_sf, fill = NA, color = "#808080", size = 0.4) +
  facet_grid(price_range ~ listing_year) +
  scale_fill_viridis_b(
    option = "inferno",
    name = "Additional travel time to nearest BEV (Minutes)",
    na.value = "grey85",
    # n.breaks = 6  # You can adjust the number of breaks
    # Alternatively, you can specify exact breaks:
    breaks = c(-20, 0, 20, 40, 60, 80), 
    direction = -1
  ) +
  theme_minimal(base_family = font) +
  theme(
    # Center the title
    plot.title = element_text(hjust = 0.5, size = 24),
    # Make facet labels larger
    strip.text = element_text(size = 16, face = "bold"),
    # Adjust legend
    legend.position = "bottom",
    legend.title = element_text(size = 18, hjust = 0.5),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.2, "cm"),  # Make legend boxes larger
    legend.title.position = "top",  # Move legend title to top
    # Keep other existing theme elements
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  labs(
    title = "BEV Access Burden by Census Tract and Price in 2016 and 2021", 
    y = NULL
  )

ggsave(
  here::here('figs', 'burden-time-map.png'), 
  map, 
  width = 13, height = 13
)
