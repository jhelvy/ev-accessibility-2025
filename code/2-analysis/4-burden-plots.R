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
# df_all_sep <- read_dt(
#   here::here('data_local', 'sep', 'burden_time_all.parquet')) %>% 
#   mutate(price_range = 'Any price')
# df_25_sep <- read_dt(
#   here::here('data_local', 'sep', 'burden_time_25.parquet')) %>% 
#   mutate(price_range = '$25k')
df <- bind_rows(df_all_agg, df_25_agg) 

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
  filename = here::here('figs', 'burden-time-mean_national_all_agg.png'),
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
  filename = here::here('figs', 'burden-time-mean_class_all_agg.png'),
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
  filename = here::here('figs', 'burden-time-mean_class_price_range.png'),
  width = 10, height = 5.5
)

# Car count ----

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
  filename = here::here('figs', 'burden-time-mean_markets_car_count.png'),
  width = 12, height = 5
)


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
    title = 'In 2021, BEV access burdens were higher in rural and lower-income census tracts'
  )

ggsave(
  filename = here::here('figs', 'burden-time-mean_income_scatterplot.png'),
  width = 10, height = 4
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

