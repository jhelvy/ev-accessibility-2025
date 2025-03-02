source(here::here('code','0-setup.R'))

make_heatmap <- function(df) {
  plot <- df %>% 
    filter(state != 'DC') %>% 
    ggplot() +
    geom_tile(
      aes(x = quarter, y = state, fill = p),
      color = "grey90"
    ) +
    facet_grid(zev_state ~ ., scales = "free_y", space = "free") +
    # Adjust scales
    scale_x_date(
      date_breaks = "1 year", 
      labels = function(x) ifelse(month(x) == 1, year(x), ""),
      expand = c(0, 0)
    ) +
    scale_fill_viridis(
      option = "inferno", direction = -1,
      na.value = "grey80"
    ) +
    # Adjust theme
    theme_minimal(
      base_size = 12,
      base_family = "Roboto Condensed"
    ) +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot",
      panel.background = element_rect(fill = 'white', color = NA),
      plot.background = element_rect(fill = 'white', color = NA)
    ) +
    guides(fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 10, barheight = 0.5
    ))
  return(plot)
}

counts_powertrain <- read_parquet(
  here::here("data", "counts", "state_powertrain.parquet")
) %>% 
  filter(year < 2022) 

dt_pev_state_counts <- counts_powertrain %>% 
    mutate(
      pev = ifelse(powertrain %in% c('bev', 'phev'), 1, 0),
      quarter = as.yearqtr(paste(year, month), format = "%Y %m")
    ) %>% 
    group_by(state, inventory_type, pev, quarter) %>% 
    summarise(n = sum(n)) %>% 
    group_by(state, inventory_type, quarter) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup()
pev_missing <- dt_pev_state_counts %>%
    filter(p == 1, pev == 0)
pev_missing$pev <- 1
pev_missing$p <- 0
dt_pev_state_counts <- dt_pev_state_counts %>%
    rbind(pev_missing) %>% 
    filter(pev == 1) %>%
    group_by(state) %>%
    mutate(
        zev_state = ifelse(state %in% zev_state, "ZEV State", "Non-ZEV State"),
        mean_p = mean(p),
        p = if_else(p == 0, NA, p)
    ) %>%
    ungroup() %>%
    mutate(
        state = fct_reorder(state, mean_p, .desc = FALSE),
        p = p * 100, 
        quarter = as.Date(quarter)
    ) %>%
    arrange(desc(mean_p))

make_heatmap(filter(dt_pev_state_counts, inventory_type == 'new')) + 
    labs(
        x = "Quarter (2016 Q1 - 2021 Q4)",
        y = "US State",
        fill = "% of PEVs at all dealerships",
        title = "Percentage of PEVs at Dealerships by State", 
        subtitle = "NEW VEHICLES"
    )

ggsave(
  here::here("figs", "heat_new_pev.png"),
  width = 7, height = 10
)

make_heatmap(filter(dt_pev_state_counts, inventory_type == 'used')) + 
    labs(
        x = "Quarter (2016 Q1 - 2021 Q4)",
        y = "US State",
        fill = "% of PEVs at all dealerships",
        title = "Percentage of PEVs at Dealerships by State", 
        subtitle = "USED VEHICLES"
    )

ggsave(
  here::here("figs", "heat_used_pev.png"),
  width = 7, height = 10
)

# BEV Heatmaps

dt_bev_state_counts <- counts_powertrain %>% 
  mutate(
    bev = powertrain == 'bev',
    quarter = as.yearqtr(paste(year, month), format = "%Y %m")
  ) %>% 
  group_by(state, inventory_type, bev, quarter) %>% 
  summarise(n = sum(n)) %>% 
  group_by(state, inventory_type, quarter) %>% 
  mutate(p = n / sum(n)) %>% 
  ungroup()
bev_missing <- dt_bev_state_counts %>%
  filter(p == 1, bev == 0)
bev_missing$bev <- 1
bev_missing$p <- 0
dt_bev_state_counts <- dt_bev_state_counts %>%
  rbind(bev_missing) %>% 
  filter(bev == 1) %>%
  group_by(state) %>%
  mutate(
    zev_state = ifelse(state %in% zev_state, "ZEV State", "Non-ZEV State"),
    mean_p = mean(p),
    p = if_else(p == 0, NA, p)
  ) %>%
  ungroup() %>%
  mutate(
    state = fct_reorder(state, mean_p, .desc = FALSE),
    p = p * 100, 
    quarter = as.Date(quarter)
  ) %>%
  arrange(desc(mean_p))

make_heatmap(filter(dt_bev_state_counts, inventory_type == 'new')) + 
  labs(
    x = "Quarter (2016 Q1 - 2021 Q4)",
    y = "US State",
    fill = "% of BEVs at all dealerships",
    title = "Percentage of BEVs at Dealerships by State", 
    subtitle = "NEW VEHICLES"
  ) +
  scale_fill_viridis(
    option = "inferno", direction = -1, na.value = "grey80", 
    limits = c(0, 12)
  )

ggsave(
  here::here("figs", "heat_new_bev.png"),
  width = 7, height = 10
)

make_heatmap(filter(dt_bev_state_counts, inventory_type == 'used')) + 
  labs(
    x = "Quarter (2016 Q1 - 2021 Q4)",
    y = "US State",
    fill = "% of BEVs at all dealerships",
    title = "Percentage of BEVs at Dealerships by State", 
    subtitle = "USED VEHICLES"
  ) +
  scale_fill_viridis(
    option = "inferno", direction = -1, na.value = "grey80", 
    limits = c(0, 6)
  )

ggsave(
  here::here("figs", "heat_used_bev.png"),
  width = 7, height = 10
)

# BEV count dumbbell ----

dt_bev_state_counts_year <- counts_powertrain %>% 
  filter(state != 'DC') %>%
  filter(year %in% c(2016, 2021)) %>%
  mutate(bev = powertrain == 'bev') %>% 
  group_by(state, inventory_type, bev, year) %>% 
  summarise(n = sum(n)) %>% 
  group_by(state, inventory_type, year) %>% 
  mutate(p = n / sum(n)) %>% 
  ungroup()
bev_missing <- dt_bev_state_counts_year %>%
  filter(p == 1, bev == 0)
bev_missing$bev <- 1
bev_missing$p <- 0
dt_bev_state_counts_year <- dt_bev_state_counts_year %>%
  rbind(bev_missing) %>% 
  filter(bev == 1) %>%
  group_by(state) %>%
  mutate(
    zev_state = ifelse(state %in% zev_state, "ZEV State", "Non-ZEV State"),
    max_p = max(p)
  ) %>%
  ungroup() %>%
  mutate(
    state = fct_reorder(state, max_p, .desc = FALSE),
    p = p * 100
  ) %>%
  arrange(desc(max_p))

dt_bev_bar <- dt_bev_state_counts_year %>% 
  mutate(inventory_type = str_to_title(inventory_type)) %>% 
  group_by(year, state, zev_state, inventory_type) %>% 
  mutate(p = ifelse(is.nan(p), 0, p/100)) %>%  
  arrange(inventory_type, desc(year), p) %>%
  mutate(state = fct_inorder(state)) %>% 
  ungroup()
  
# Quick views
dt_bev_bar %>% 
  arrange(desc(year), inventory_type) %>% 
  filter(zev_state == 'Non-ZEV State') %>% 
  # filter(zev_state == 'ZEV State') %>% 
  filter(inventory_type == 'New') %>% 
  # filter(inventory_type == 'Used') %>% 
  filter(year == 2021) %>% 
  View()

legend_data <- data.frame(
  x = c(0.045, 0.065),  # Adjust these x-coordinates to position the legend
  y = rep("UT", 2),   # Use "HI" since it's the top state in that facet
  year = c(2016, 2021), 
  inventory_type = 'Used', 
  zev_state = 'Non-ZEV State'
)

dt_bev_bar %>%
  ggplot() +
  geom_segment(
    data = dt_bev_bar %>% 
      select(state, year, p, zev_state, inventory_type) %>%
      pivot_wider(
        names_from = year, 
        values_from = p,
        id_cols = c(state, zev_state, inventory_type)
      ),
    aes(x = `2016`, xend = `2021`, y = state)
  ) +
  geom_point(
    aes(x = p, y = state, color = as.factor(year)), 
    size = 2
  ) +
  facet_grid(zev_state ~ inventory_type, scales = "free_y", space = "free") +
  scale_x_continuous(
    limits = c(0, 0.082),
    labels = scales::percent, 
    breaks = seq(0, 0.08, 0.02)
  ) +
  scale_color_manual(values = c(color_tesla, color_nontesla)) +
  theme_minimal_vgrid(
    font_size = 14,
    font_family = "Roboto Condensed"
  ) +
  panel_border() +
  theme(
    plot.title.position = "plot",
    legend.position = 'none',
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA),
    plot.title = element_markdown()
  ) +
  # Adjust size of circle in legend
  guides(color = guide_legend(override.aes = list(size = 6))) +
  labs(
    color = 'Year',
    y = "US State",
    x = "% of total dealership inventories that are BEVs",
    title = paste0(
      "Percentage of BEVs at Dealerships by State and Market in <span style = 'color: ",
      color_tesla, 
      ";'>2016</span> and <span style = 'color: ", 
      color_nontesla, 
      ";'>2021</span>")
  ) +
  # Add custom legend
  
  # Add legend box
  geom_rect(
    data = legend_data,
    aes(xmin = 0.04, xmax = 0.07, 
        ymin = "NH", ymax = "MN"),  # Adjust these coordinates as needed
    fill = "white",
    color = "black",
    size = 0.5
  ) +
  
  # Add legend segment and points
  geom_segment(
    data = legend_data,
    aes(x = 0.045, xend = 0.065,
        y = "UT", yend = "UT"),  # Adjust these coordinates as needed
    size = 0.5
  ) +
  
  # Add legend points
  geom_point(
    data = legend_data,
    aes(x = x, y = y, color = as.factor(year)),
    size = 2
  ) +
  
  # Add legend text
  geom_text(
    data = legend_data,
    aes(x = x, y = "DE", label = year, color = as.factor(year)),
    vjust = 4,
    size = 4,
    fontface = 'bold',
    family = "Roboto Condensed"
  ) 

ggsave(
  here::here("figs", "bars_bev_percent.png"),
  width = 8, height = 9
)
