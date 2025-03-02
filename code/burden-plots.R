source(here::here("code", "0-setup.R"))

dt_tract <- read_parquet(here('data', 'tract_dt.parquet')) %>% 
  select(GEOID, class, pop = pop_over25, med_inc_hh) %>% 
  mutate(med_inc_hh = as.numeric(med_inc_hh))

read_dt <- function(path) {
  read_parquet(path) %>% 
    filter(listing_year != 2022) %>%
    rename(time = time_burden) %>% 
    left_join(dt_tract, by = 'GEOID') %>% 
    group_by(listing_year, car_count) %>% 
    mutate(
      lb = logitr::fquantile(time, 0.01), 
      ub = logitr::fquantile(time, 0.99)
    ) %>% 
    ungroup() %>% 
    filter(
      (time >= lb) & (time <= ub)
    )
}

df_all_agg <- read_dt(
  here::here('data_local', 'agg', 'burden_time_all.parquet')) %>% 
  mutate(price_range = 'Any price')
df_25_agg <- read_dt(
  here::here('data_local', 'agg', 'burden_time_25.parquet')) %>% 
  mutate(price_range = '$25k')
df_all_sep <- read_dt(
  here::here('data_local', 'sep', 'burden_time_all.parquet')) %>% 
  mutate(price_range = 'Any price')
df_25_sep <- read_dt(
  here::here('data_local', 'sep', 'burden_time_25.parquet')) %>% 
  mutate(price_range = '$25k')
df <- bind_rows(df_all_agg, df_25_agg, df_all_sep, df_25_sep) 

df1 <- df %>% 
  filter(car_count == 1) %>% 
  select(-car_count)
df5 <- df %>% 
  filter(car_count == 5) %>% 
  select(-car_count)
df10 <- df %>% 
  filter(car_count == 10) %>% 
  select(-car_count)

df_national <- df1 %>%
  group_by(listing_year, inventory_type, price_range) %>%
  summarise(
    time25 = logitr::fquantile(time, 0.25),
    time50 = median(time),
    timeMean = weighted.mean(time, pop),
    time75 = logitr::fquantile(time, 0.75)
  ) %>% 
  mutate(class = 'National') %>% 
  ungroup()

# National ----

df_national %>% 
  filter(price_range == 'Any price', inventory_type == 'all') %>% 
  ggplot(aes(x = listing_year)) +
  geom_ribbon(
    aes(ymin = time25, ymax = time75),
    fill = 'black', 
    alpha = 0.2
  ) +
  geom_line(aes(y = time50)) +
  scale_y_continuous(
    limits = c(0, 22),
    breaks = seq(0, 20, 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Additional travel time to nearest BEV", 
    subtitle = 'Line is median travel time, and band reflects middle 50%', 
    x = "Year",
    y = "Minutes"
  ) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    plot.title.position = "plot", 
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  )

# ggsave(
#   filename = here::here('figs', 'burden-time', 'median_national_all_agg.png'),
#   width = 8, height = 6
# )

df_national %>% 
  filter(price_range == 'Any price', inventory_type == 'all') %>% 
  ggplot(aes(x = listing_year, y = timeMean)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Population-weighted mean additional travel time to nearest BEV", 
    x = "Year",
    y = "Minutes"
  ) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    plot.title.position = "plot", 
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  )

ggsave(
  filename = here::here('figs', 'burden-time', 'mean_national_all_agg.png'),
  width = 8, height = 6
)

# Class ----

df1 %>%
  filter(price_range == 'Any price') %>% 
  filter(inventory_type == 'all') %>% 
  group_by(listing_year, class) %>%
  summarise(time = weighted.mean(time, pop)) %>% 
  ungroup() %>% 
  rbind(
    df_national %>% 
      filter(price_range == 'Any price') %>% 
      filter(inventory_type == 'all') %>% 
      select(listing_year, class, time = timeMean)
  ) %>% 
  mutate(
    linetype = ifelse(class == 'National', 'two', 'one')
  ) %>% 
  ggplot(aes(x = listing_year, y = time)) +
  geom_point(aes(x = listing_year, y = time, color = class)) +
  geom_line(aes(color = class, group = class, linetype = linetype)) +
  geom_text(
    data = data.frame(
      listing_year = rep(2018.5, 4), 
      time = c(24, 13, 3, 7.5), # rural, suburban, urban, national
      class = c('Rural', 'Suburban', 'Urban', 'National')
    ),
    mapping = aes(x = listing_year, y = time, color = class, label = class), 
    family = font, 
    fontface = 'bold', 
    size = 4.5
  ) +
  labs(
    title = "Population-weighted mean BEV access burden\nover time by urbanization level", 
    x = "Year",
    y = "Minutes"
  )  +
  scale_color_manual(
    values = c('black', color_nontesla, color_ev, color_tesla)
  ) +
  scale_y_continuous(
    limits = c(0, 32),
    breaks = seq(0, 30, 10), 
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    legend.position = "none",
    plot.title.position = "plot", 
    axis.line.y = element_blank()
  )

ggsave(
  filename = here::here('figs', 'burden-time', 'mean_class_all_agg.png'),
  width = 7, height = 6
)


# Price range ----

df1 %>%
  filter(inventory_type == 'all') %>% 
  group_by(listing_year, price_range, class) %>%
  summarise(time = weighted.mean(time, pop)) %>% 
  ungroup() %>% 
  rbind(
    df_national %>% 
      filter(inventory_type == 'all') %>% 
      select(listing_year, price_range, class, time = timeMean)
  ) %>% 
  mutate(
    linetype = ifelse(class == 'National', 'two', 'one'), 
    price_range = ifelse(price_range == '$25k', 'Price <$25,000', 'Any Price')
  ) %>% 
  ggplot(aes(x = listing_year, y = time)) +
  geom_point(aes(x = listing_year, y = time, color = class)) +
  geom_line(
    aes(color = class, group = class, linetype = linetype)
  ) +
  facet_wrap(vars(price_range), nrow = 1) +
  geom_text(
    data = data.frame(
      listing_year = rep(2018.5, 8),  # 4 classes × 2 price ranges
      time = c(
        29, 17, 5, 10.5, # rural, suburban, urban, national
        25, 14, 3, 7.5
      ),  
      class = rep(c('Rural', 'Suburban', 'Urban', 'National'), 2),
      price_range = rep(c('Price <$25,000', 'Any Price'), each = 4)
    ),
    mapping = aes(x = listing_year, y = time, color = class, label = class),
    family = font,
    fontface = 'bold',
    size = 4.5
  ) +
  labs(
    title = "Population-weighted mean BEV access burden over time by urbanization and price range", 
    x = "Year",
    y = "Additional time to nearst BEV (minutes)"
  )  +
  scale_color_manual(
    values = c('black', color_nontesla, color_ev, color_tesla)
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 10), 
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    legend.position = "none",
    plot.title.position = "plot", 
    axis.line.y = element_blank()
  ) +
  panel_border()

ggsave(
  filename = here::here('figs', 'burden-time', 'mean_class_price_range.png'),
  width = 10, height = 5.5
)

# Market & price ----

df1 %>%
  filter(inventory_type != 'all') %>% 
  group_by(listing_year, inventory_type, price_range, class) %>%
  summarise(time = median(time)) %>% 
  ungroup() %>% 
  rbind(
    df_national %>% 
      filter(inventory_type != 'all') %>% 
      select(listing_year, inventory_type, price_range, class, time = time50)
  ) %>% 
  mutate(
    inventory_type = str_to_title(inventory_type), 
    price_range = ifelse(price_range == '$25k', '< $25,000', price_range)
  ) %>% 
  ggplot(aes(x = listing_year, y = time, color = class)) +
  geom_point(aes(x = listing_year, y = time, color = class)) +
  geom_line(aes(group = class)) +
  facet_grid(inventory_type ~ price_range) +
  labs(
    title = "Additional travel time to nearest BEV", 
    x = "Year",
    y = "Minutes",
    color = 'Region'
  ) +
  scale_y_continuous(
    breaks = seq(0, 150, 50),
    limits = c(0, 150),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 150)) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    legend.position = c(0.8, 0.85),
    plot.title.position = "plot", 
    axis.line.y = element_blank()
  ) +
  scale_color_manual(
    values = c('black', color_nontesla, color_ev, color_tesla)
  ) 

# ggsave(
#   filename = here::here('figs', 'burden-time', 'median_class_sep_prices.png'),
#   width = 10, height = 8
# )

df1 %>%
  filter(inventory_type != 'all') %>% 
  group_by(listing_year, inventory_type, price_range, class) %>%
  summarise(time = weighted.mean(time, pop)) %>% 
  ungroup() %>% 
  rbind(
    df_national %>% 
      filter(inventory_type != 'all') %>% 
      select(listing_year, inventory_type, price_range, class, time = timeMean)
  ) %>% 
  mutate(
    inventory_type = str_to_title(inventory_type), 
    price_range = ifelse(price_range == '$25k', '< $25,000', price_range)
  ) %>% 
  ggplot(aes(x = listing_year, y = time, color = class)) +
  geom_point(aes(x = listing_year, y = time, color = class)) +
  geom_line(aes(group = class)) +
  facet_grid(inventory_type ~ price_range) +
  labs(
    title = "Population-weighted mean additional travel time to nearest BEV",  
    x = "Year",
    y = "Minutes",
    color = 'Region'
  ) +
  scale_y_continuous(
    breaks = seq(0, 150, 50),
    limits = c(0, 150),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 150)) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    legend.position = c(0.8, 0.85),
    plot.title.position = "plot", 
    axis.line.y = element_blank()
  )+
  scale_color_manual(
    values = c('black', color_nontesla, color_ev, color_tesla)
  ) 

ggsave(
  filename = here::here('figs', 'burden-time', 'mean_class_sep_prices.png'),
  width = 10, height = 8
)


# Inventory type ----

df1 %>%
  filter(price_range == 'Any price') %>% 
  group_by(listing_year, inventory_type, class) %>%
  summarise(time = median(time)) %>% 
  ungroup() %>% 
  rbind(
    df_national %>% 
      filter(price_range == 'Any price') %>% 
      select(listing_year, inventory_type, class, time = time50)
  ) %>% 
  mutate(
    inventory_type = ifelse(
      inventory_type == 'all', 'Any BEV', ifelse(
      inventory_type == 'new', 'New BEV', 'Used BEV'
    ))
  ) %>% 
  ggplot(aes(x = listing_year, y = time, color = class)) +
  geom_point(aes(x = listing_year, y = time, color = class)) +
  geom_line(aes(group = class)) +
  facet_wrap(~inventory_type) +
  labs(
    title = "Median additional travel time to nearest BEV",
    x = "Year",
    y = "Minutes",
    color = 'Region'
  ) +
  scale_y_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 41),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  geom_text(
    data = data.frame(
      listing_year = c(
        2017.5, 2017.5, 2017.5, 2017.5, # Any BEV
        2017.5, 2017.5, 2017.5, 2017.5, # New
        2017.5, 2017.5, 2017.5, 2017.5  # Used
      ),
      time = c(
        15, 11, 6, 5, # Any BEV
        15, 11, 6, 5, # New
        15, 11, 6, 5  # Used
      ),
      class = rep(c('Rural', 'Suburban', 'Urban', 'National'), 3), 
      inventory_type = rep(c('Any BEV', 'New BEV', 'Used BEV'), each = 4)
    ),
    mapping = aes(x = listing_year, y = time, color = class, label = class), 
    family = font, 
    fontface = 'bold', 
    size = 4.5
  ) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    legend.position = "none",
    plot.title.position = "plot", 
    axis.line.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) +
  panel_border() +
  scale_color_manual(
    values = c('black', color_nontesla, color_ev, color_tesla)
  ) 

# ggsave(
#   filename = here::here('figs', 'burden-time', 'median_markets.png'),
#   width = 12, height = 5
# )

df1 %>%
  filter(price_range == 'Any price') %>% 
  group_by(listing_year, inventory_type, class) %>%
  summarise(time = weighted.mean(time, pop)) %>% 
  ungroup() %>% 
  rbind(
    df_national %>% 
      filter(price_range == 'Any price') %>% 
      select(listing_year, inventory_type, class, time = timeMean)
  ) %>% 
  mutate(
    inventory_type = ifelse(
      inventory_type == 'all', 'Any BEV (New or Used)', ifelse(
        inventory_type == 'new', 'New BEV', 'Used BEV'
      )), 
    linetype = ifelse(class == 'National', 'two', 'one')
  ) %>% 
  ggplot(aes(x = listing_year, y = time, color = class)) +
  geom_point(aes(x = listing_year, y = time, color = class)) +
  geom_line(aes(group = class, linetype = linetype)) +
  facet_wrap(~inventory_type) +
  labs(
    title = "Population-weighted mean additional travel time to nearest BEV",  
    x = "Year",
    y = "Minutes",
    color = 'Region'
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    limits = c(0, 50),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  geom_text(
    data = data.frame(
      listing_year = c(
        rep(2018.5, 4), # Any BEV
        rep(2018.5, 4), # New
        c(2018.5, 2018.5, 2018.5, 2017) # Used
      ),
      time = c(
        # Any BEV panel
        25, 14, 3, 7.5, # rural, suburban, urban, national
        # New BEV panel
        32, 21, 7, 13,
        # Used BEV panel
        27, 16, 4, 11.5
      ),
      class = rep(c('Rural', 'Suburban', 'Urban', 'National'), 3), 
      inventory_type = rep(c(
        'Any BEV (New or Used)', 'New BEV', 'Used BEV'
      ), each = 4)
    ),
    mapping = aes(x = listing_year, y = time, color = class, label = class), 
    family = font, 
    fontface = 'bold', 
    size = 4.5
  ) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    legend.position = "none",
    plot.title.position = "plot", 
    axis.line.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) +
  panel_border() +
  scale_color_manual(
    values = c('black', color_nontesla, color_ev, color_tesla)
  ) 

ggsave(
  filename = here::here('figs', 'burden-time', 'mean_markets.png'),
  width = 12, height = 5
)


# Car count ----

df %>%
  filter(price_range == 'Any price') %>%
  filter(inventory_type == 'all') %>% 
  group_by(listing_year, class, car_count) %>%
  summarise(time = median(time)) %>%
  ungroup() %>%
  rbind(
    df %>%
      filter(price_range == 'Any price') %>% 
      filter(inventory_type == 'all') %>% 
      group_by(listing_year, car_count) %>%
      summarise(time = median(time)) %>% 
      mutate(class = 'National') %>%
      ungroup() %>% 
      select(listing_year, class, car_count, time)
  ) %>%
  mutate(
    car_count = ifelse(
      car_count == 1, 'First 1 BEV', ifelse(
      car_count == 5, 'First 5 BEVs', 'First 10 BEVs'
    )), 
    car_count = factor(car_count, c('First 1 BEV', 'First 5 BEVs', 'First 10 BEVs'))
  ) %>% 
  ggplot(aes(x = listing_year, y = time, color = class)) +
  geom_point(aes(x = listing_year, y = time, color = class)) +
  geom_line(aes(group = class)) +
  facet_wrap(~car_count) +
  labs(
    title = "Median additional travel time to nearest BEV",
    x = "Year",
    y = "Minutes",
    color = 'Region'
  ) +
  scale_y_continuous(
    breaks = seq(0, 60, 15),
    limits = c(0, 65),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  geom_text(
    data = data.frame(
      listing_year = c(
        2017.5, 2017.5, 2017.5, 2017.5, # Any BEV
        2017.5, 2017.5, 2017.5, 2017.5, # New
        2017.5, 2017.5, 2017.5, 2017.5  # Used
      ),
      time = c(
        15, 11, 6, 5, # Any BEV
        15, 11, 6, 5, # New
        15, 11, 6, 5  # Used
      ),
      class = rep(c('Rural', 'Suburban', 'Urban', 'National'), 3), 
      inventory_type = rep(c('Any BEV', 'New BEV', 'Used BEV'), each = 4)
    ),
    mapping = aes(x = listing_year, y = time, color = class, label = class), 
    family = font, 
    fontface = 'bold', 
    size = 4.5
  ) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    legend.position = "none",
    plot.title.position = "plot", 
    axis.line.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) +
  panel_border() +
  scale_color_manual(
    values = c('black', color_nontesla, color_ev, color_tesla)
  ) 

# ggsave(
#   filename = here::here('figs', 'burden-time', 'median_markets_car_count.png'),
#   width = 12, height = 5
# )

df %>%
  filter(price_range == 'Any price') %>%
  filter(inventory_type == 'all') %>% 
  group_by(listing_year, class, car_count) %>%
  summarise(time = weighted.mean(time, pop)) %>%
  ungroup() %>%
  rbind(
    df %>%
      filter(price_range == 'Any price') %>% 
      filter(inventory_type == 'all') %>% 
      group_by(listing_year, car_count) %>%
      summarise(time = weighted.mean(time, pop)) %>% 
      mutate(class = 'National') %>%
      ungroup() %>% 
      select(listing_year, class, car_count, time)
  ) %>%
  mutate(
    car_count = ifelse(
      car_count == 1, 'First 1 BEV', ifelse(
        car_count == 5, 'First 5 BEVs', 'First 10 BEVs'
      )), 
    car_count = factor(car_count, c('First 1 BEV', 'First 5 BEVs', 'First 10 BEVs'))
  ) %>% 
  ggplot(aes(x = listing_year, y = time, color = class)) +
  geom_point(aes(x = listing_year, y = time, color = class)) +
  geom_line(aes(group = class)) +
  facet_wrap(~car_count) +
  labs(
    title = "Population-weighted mean additional travel time to nearest BEV",  
    x = "Year",
    y = "Minutes",
    color = 'Region'
  ) +
  scale_y_continuous(
    breaks = seq(0, 70, 10),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 70)) +
  geom_text(
    data = data.frame(
      listing_year = c(
        2017.5, 2017.5, 2017.5, 2017.5, # Any BEV
        2017.5, 2017.5, 2017.5, 2017.5, # New
        2017.5, 2017.5, 2017.5, 2017.5  # Used
      ),
      time = c(
        15, 11, 6, 5, # Any BEV
        15, 11, 6, 5, # New
        15, 11, 6, 5  # Used
      ),
      class = rep(c('Rural', 'Suburban', 'Urban', 'National'), 3), 
      inventory_type = rep(c('Any BEV', 'New BEV', 'Used BEV'), each = 4)
    ),
    mapping = aes(x = listing_year, y = time, color = class, label = class), 
    family = font, 
    fontface = 'bold', 
    size = 4.5
  ) +
  theme_minimal_hgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    legend.position = "none",
    plot.title.position = "plot", 
    axis.line.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) +
  panel_border() +
  scale_color_manual(
    values = c('black', color_nontesla, color_ev, color_tesla)
  ) 

ggsave(
  filename = here::here('figs', 'burden-time', 'mean_markets_car_count.png'),
  width = 12, height = 5
)


# median travel time between new/used by price
# Dumbbell plot

df1 %>%
  filter(price_range == 'Any price') %>%
  group_by(listing_year, inventory_type, class) %>%
  summarise(time = median(time)) %>%
  ungroup() %>%
  rbind(
    df_national %>%
      filter(price_range == 'Any price') %>%
      select(listing_year, inventory_type, class, time = time50)
  ) %>%
  mutate(
    inventory_type = ifelse(
      inventory_type == 'all', 'Any', ifelse(
        inventory_type == 'new', 'New', 'Used'
    )),
    class = factor(class, c('National', 'Urban', 'Suburban', 'Rural'))
  ) %>%
  filter(listing_year %in% c(2016, 2021)) %>%
  ggplot() +
  geom_segment(
    data = . %>%
      pivot_wider(
        names_from = listing_year,
        values_from = time
      ),
    aes(x = `2016`, xend = `2021`, y = inventory_type)
  ) +
  geom_point(
    aes(x = time, y = inventory_type, color = as.factor(listing_year)),
    size = 2
  ) +
  facet_wrap(~class, ncol = 1) +
  labs(
    title = "Median additional travel time to nearest BEV",
    x = "Minutes",
    color = 'Year',
    y = NULL
  ) +
  scale_x_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 42),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA),
    legend.position = "bottom",
    plot.title.position = "plot",
    axis.line.y = element_blank()
  ) +
  panel_border() +
  scale_color_manual(
    values = c(color_tesla, color_ev)
  )

# ggsave(
#   filename = here::here('figs', 'burden-time', 'median_markets_dumbbel.png'),
#   width = 7, height = 6
# )

# Dumbbell plot to 1, 5, and 10 cars

df %>%
  filter(price_range == 'Any price') %>%
  filter(inventory_type == 'all') %>% 
  group_by(listing_year, class, car_count) %>%
  summarise(time = median(time)) %>%
  ungroup() %>%
  rbind(
    df %>%
      filter(price_range == 'Any price') %>% 
      filter(inventory_type == 'all') %>% 
      group_by(listing_year, car_count) %>%
      summarise(time = median(time)) %>% 
      mutate(class = 'National') %>%
      ungroup() %>% 
      select(listing_year, class, car_count, time)
  ) %>%
  mutate(
    class = factor(class, c('National', 'Urban', 'Suburban', 'Rural'))
  ) %>%
  filter(listing_year %in% c(2016, 2021)) %>% 
  ggplot() +
  geom_segment(
    data = . %>%
      pivot_wider(
        names_from = listing_year,
        values_from = time
      ),
    aes(x = `2016`, xend = `2021`, y = class)
  ) +
  geom_point(
    aes(x = time, y = class, color = as.factor(listing_year)),
    size = 2
  ) +
  facet_wrap(~car_count, ncol = 1) +
  labs(
    title = "Median additional travel time to nearest BEV",
    x = "Minutes",
    color = 'Year',
    y = NULL
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 15),
    limits = c(0, 65),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA),
    legend.position = "bottom",
    plot.title.position = "plot",
    axis.line.y = element_blank()
  ) +
  panel_border() +
  scale_color_manual(
    values = c(color_tesla, color_ev)
  )

# ggsave(
#   filename = here::here('figs', 'burden-time', 'median_car_count_dumbbel.png'),
#   width = 7, height = 6
# )


# Income scatterplot ----

df_all_agg %>% 
  filter(listing_year == 2021) %>% 
  filter(inventory_type == 'all') %>% 
  filter(car_count == 1) %>% 
  mutate(med_inc_hh = med_inc_hh / 10^3) %>% 
  ggplot(aes(x = med_inc_hh, y = time)) +
  geom_scattermore(size = 1) +
  facet_wrap(vars(class)) +
  theme_minimal_grid() +
  panel_border() +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    plot.title.position = "plot"
  ) +
  labs(
    x = 'Census tract median annual household income ($1,000)', 
    y = 'BEV access burden (minutes)', 
    title = 'In 2021, BEV access burdens were higher in rural and lower-income census tracts', 
    subtitle = "Each point is one U.S. census tract"
  )

ggsave(
  filename = here::here('figs', 'burden-time', 'mean_income_scatterplot.png'),
  width = 10, height = 5
)

# Income barplot ----

df_income <- df_all_agg %>% 
  filter(!is.na(med_inc_hh)) %>% 
  filter(car_count == 1) %>% 
  mutate(
    med_inc_hh = med_inc_hh / 10^3,
    inc_bucket = ifelse(
    (med_inc_hh >= 0) & (med_inc_hh < 25), "$0 - $25k", ifelse(
    (med_inc_hh >= 25) & (med_inc_hh < 50), "$25k - $50k", ifelse(
    (med_inc_hh >= 50) & (med_inc_hh < 75), "$50k - $75k", ifelse(
    (med_inc_hh >= 75) & (med_inc_hh < 100), "$75k - $100k", ifelse(
    (med_inc_hh >= 100) & (med_inc_hh < 125), "$100k - $125k", ifelse(
    (med_inc_hh >= 125) & (med_inc_hh < 150), "$125k - $150k", ifelse(
    (med_inc_hh >= 150) & (med_inc_hh < 200), "$150k - $200k", ">$200k"
    ))))))), 
    inc_bucket = factor(inc_bucket, c(
    "$0 - $25k", "$25k - $50k", "$50k - $75k", "$75k - $100k", 
    "$100k - $125k", "$125k - $150k", "$150k - $200k", ">$200k"
    ))
  ) %>%
  group_by(inc_bucket, listing_year) %>% 
  summarise(timeMean = weighted.mean(time, pop)) %>% 
  ungroup()

df_income %>% 
  ggplot() +
  geom_col(aes(x = listing_year, y = timeMean)) +
  facet_wrap(vars(inc_bucket), nrow = 1) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal_grid() +
  panel_border() +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    plot.title.position = "plot"
  ) 


ggsave(
  filename = here::here('figs', 'burden-time', 'mean_income_bars.png'),
  width = 13, height = 3
)

# Income dumbbell ----

df_income %>% 
  filter(listing_year %in% c(2016, 2021)) %>% 
  ggplot() +
  geom_segment(
    data = . %>%
      pivot_wider(
        names_from = listing_year,
        values_from = timeMean
      ),
    aes(x = `2016`, xend = `2021`, y = inc_bucket)
  ) +
  geom_point(
    aes(
      x = timeMean, 
      y = inc_bucket, 
      color = as.factor(listing_year)
    ), 
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_discrete(
    expand = expansion(mult = c(0.05, 0.12))
  ) +
  scale_color_manual(values = c('red', 'blue')) +
  theme_minimal_vgrid(font_family = font) +
  panel_border() +
  theme(
    strip.background = element_rect("grey80"),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.line.x = element_blank(),
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  geom_text(
    data = data.frame(
      timeMean = c(1.5, 5.5), 
      inc_bucket = ">$200k", 
      label = c("2021", "2016")
    ), 
    mapping = aes(
      x = timeMean, 
      y = inc_bucket, 
      label = label, 
      color = label
    ), 
    family = font, 
    size = 5, 
    fontface = 'bold', 
    nudge_y = 0.5
  ) +
  labs(
    x = 'Population-weighted mean additional travel time to nearest BEV (minutes)',
    y = 'Median annual household income', 
    color = NULL,
    title = "Lower income census tracts have higher mean BEV access burdens"
  )

ggsave(
  filename = here::here('figs', 'burden-time', 'mean_income_dumbbell.png'),
  width = 8, height = 5
)

# Number of tracts >= 60 minutes 

quick_summary <- function(df, year, class_sub, price, time_filter) {
  df_year <- df1 %>% 
    filter(inventory_type == 'all') %>% 
    filter(listing_year == {{ year }}) %>%
    filter(class == {{ class_sub }}) %>%
    filter(price_range == {{ price }})
    # filter(listing_year == 2016) %>% 
    # filter(class == 'Rural') %>% 
    # filter(price_range == 'Any price')
  df_time <- df_year %>% 
    # filter(time >= 15)
    filter(time <= {{ time_filter }})
  
  p <- sum(df_time$pop) / sum(df_year$pop)
  cat(p, '% of ', class_sub, ' residents in ', year, ' have access burdens of ', time_filter, ' or less for BEVs of price ', price, sep = '')
}
  
quick_summary(df1, 2016, 'Rural', '$25k', 15)
quick_summary(df1, 2016, 'Rural', 'Any price', 15)
quick_summary(df1, 2021, 'Rural', '$25k', 15)
quick_summary(df1, 2021, 'Rural', 'Any price', 15)

quick_summary(df1, 2016, 'Rural', '$25k', 30)
quick_summary(df1, 2016, 'Rural', 'Any price', 30)
quick_summary(df1, 2021, 'Rural', '$25k', 30)
quick_summary(df1, 2021, 'Rural', 'Any price', 30)

quick_summary(df1, 2016, 'Rural', '$25k', 60)
quick_summary(df1, 2016, 'Rural', 'Any price', 60)
quick_summary(df1, 2021, 'Rural', '$25k', 60)
quick_summary(df1, 2021, 'Rural', 'Any price', 60)


# In 2021, only 8.8% of census tracts had a BEV access burden greater
# than 15 minutes, and just 3.5% had a burden greater than 30 minutes. 
# Of those, 80% were in rural areas.  

# Dumbbell

df_totals <- df1 %>% 
  filter(inventory_type == 'all') %>% 
  filter(!is.na(med_inc_hh)) %>% 
  filter(listing_year == 2021) %>% 
  mutate(incUnder100 = med_inc_hh <= 100000) %>% 
  select(class, pop, incUnder100, time, price_range) %>%
  group_by(class, price_range, incUnder100) %>% 
  mutate(pop_total = sum(pop)) %>% 
  ungroup() 

df_totals %>% 
  filter(time <= 30) %>% 
  group_by(class, price_range, incUnder100) %>% 
  mutate(popUnder = sum(pop)) %>% 
  ungroup() %>% 
  distinct(class, price_range, pop_total, popUnder, incUnder100) %>% 
  mutate(p = popUnder / pop_total) %>% 
  arrange(class, price_range) %>% 
  mutate(p_inv = round(1-p, 2))

