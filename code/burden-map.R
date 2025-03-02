source(here::here('code', '0-setup.R'))
library(patchwork)

# State border geometries
states_sf <- qs::qread(here::here('data', 'states_sf.qs'))

# Get census tract geometries for the entire US
tracts <- qs::qread(here::here('data', 'tracts.qs'))

# Burden data
df_all <- read_parquet(here::here('data_local', 'agg', 'burden_time_all.parquet')) %>% 
  filter(listing_year != 2022) %>% 
  filter(car_count == 1) %>% 
  select(-car_count, -inventory_type) %>% 
  mutate(price_range = 'Any Price')
df_25 <- read_parquet(here::here('data_local', 'agg', 'burden_time_25.parquet')) %>% 
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

# Map function ----

make_map <- function(df, price) {

  # First, get the CRS from your df
  crs <- st_crs(df)
  
  # Create points for text labels in the correct CRS
  label_points <- data.frame(
    listing_year = c(2016, 2021),
    label = c('2016', '2021'),
    x = c(-98, -98),  # longitude for center of US
    y = c(52, 52)     # latitude above the US
  ) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%  # 4326 is for standard lat/long
    st_transform(crs)  # Transform to match your map's CRS

  map <- df %>%
    filter(listing_year %in% c(2016, 2021)) %>%
    filter(price_range == {{ price }}) %>% 
    # mutate(time_burden = ifelse(time_burden < -20, NA, time_burden)) %>% 
    ggplot() +
    geom_sf(aes(fill = time_burden), color = NA) +
    # Add state borders
    geom_sf(data = states_sf, fill = NA, color = "#808080", size = 0.4) +
    facet_wrap(~ listing_year) +
    geom_sf_text(
      data = label_points,
      aes(label = label),
      inherit.aes = FALSE,
      size = 6,
      fontface = "bold",
      family = font
    ) +
    coord_sf(clip = "off") +  # Allows drawing outside plot area
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    scale_fill_viridis_b(
      option = "inferno",
      name = "Additional travel time to nearest BEV (Minutes)",
      na.value = "grey85",
      # n.breaks = 6  # You can adjust the number of breaks
      # Alternatively, you can specify exact breaks:
      breaks = c(-20, 0, 20, 40, 60, 80), 
      direction = -1
    ) +
    theme_void(base_family = font) +
    theme(
      # Center the title
      plot.title = element_text(hjust = 0.5, size = 24),
      # Remove facet labels
      strip.text = element_blank(),
      # Reduce plot margins
      panel.spacing = unit(0.4, "cm"),        # Reduce space between panels
      plot.margin = margin(t = 0.1, r = 0.2, b = 0.1, l = 0.2, unit = "cm"),
      # Keep other theme elements
      legend.position = "bottom",
      legend.title = element_text(size = 18, hjust = 0.5),
      legend.text = element_text(size = 18),
      legend.key.size = unit(1.2, "cm"),
      legend.title.position = "top",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(), 
      panel.background = element_rect(fill = 'white', color = NA),
      plot.background = element_rect(fill = 'white', color = NA)
    ) +
    labs(
      title = NULL,
      y = NULL
    )
  
  return(map)
}

# BEV Access Burden by Census Tract and Price in 2016 and 2021

# Make the maps ----

map_any <- make_map(tracts_with_times, 'Any Price')

# ggsave(
#   here::here('figs', 'burden-time', 'burden_map_any_price.png'), 
#   map_any, 
#   width = 13, height = 13
# )

# Vertical
map_any_v <- map_any +
  facet_wrap(~ listing_year, ncol = 1) 

# ggsave(
#   here::here('figs', 'burden-time', 'burden_map_any_price_vertical.png'),  
#   map_any_v,
#   width = 8, height = 13
# )

map_25 <- make_map(tracts_with_times, 'Price <$25,000')

# ggsave(
#   here::here('figs', 'burden-time', 'burden_map_25k.png'), 
#   map_25, 
#   width = 13, height = 13
# )

# Vertical
map_25_v <- map_25 +
  facet_wrap(~ listing_year, ncol = 1) 

# ggsave(
#   here::here('figs', 'burden-time', 'burden_map_25k_vertical.png'),  
#   map_25_v,
#   width = 8, height = 13
# )


# Combined plot ----

# Adjust the first plot
map_any_v_a <- map_any_v +
  ggtitle("A. Any Price") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 20)),  # Add bottom margin to title
    legend.position = "none"
  )

# Adjust the second plot
map_25_v_b <- map_25_v +
  ggtitle("B. Price <$25,000") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 20)),  # Add bottom margin to title
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.margin = margin(t = 20, b = 10)  # Add margin above and below legend
  )

# Rest of your code remains the same

# Combine plots vertically with adjusted spacing
combined_plot <- map_any_v_a / map_25_v_b +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    theme = theme(
      plot.margin = margin(20, 20, 20, 20)
    )
  )

ggsave(
  here::here('figs', 'burden-time', 'burden_map_combined.png'),  
  combined_plot,
  width = 5, height = 13, dpi = 300
)

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
  here::here('figs', 'burden-time', 'burden_map_price_range.png'), 
  map, 
  width = 13, height = 13
)
