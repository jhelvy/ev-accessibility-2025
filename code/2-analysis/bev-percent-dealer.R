source(here::here('code', '0-setup.R'))

compute_p <- function(df) {
  df %>%
    pivot_wider(
      values_from = n,
      names_from = powertrain
    ) %>%
      mutate(p_ev = bev / conventional) %>%
      select(-bev, -conventional)
}

ds <- ds %>%
  filter(listing_year < 2022)

# Add up counts across all tracts

dt <- ds %>%
  filter(powertrain %in% c('conventional', 'bev')) %>%
  filter(! state %in% non_us_state) %>%
  group_by(dealer_id, inventory_type, powertrain, listing_year, state) %>%
  summarise(n = n()) %>%
  collect()

# Now count up the number of dealers for each powertrain

dt_zev_new_used <- dt %>%
  mutate(
    state_type = ifelse(state %in% zev_state, 'ZEV', 'Non-ZEV'),
    state_type = ifelse(state == 'CA', 'CA', state_type)
  ) %>%
  group_by(inventory_type, powertrain, listing_year, state_type) %>%
  summarise(n = n()) %>%
  compute_p()

dt_zev_new_used

dt_new_used <- dt %>%
  group_by(inventory_type, powertrain, listing_year) %>%
  summarise(n = n()) %>%
  compute_p()

dt_new_used

dt_zev <- dt %>%
  mutate(
    state_type = ifelse(state %in% zev_state, 'ZEV', 'Non-ZEV'),
    state_type = ifelse(state == 'CA', 'CA', state_type)
  ) %>%
  group_by(state_type, powertrain, listing_year) %>%
  summarise(n = n()) %>%
  compute_p()

dt_zev

dt_national <- dt %>%
  group_by(powertrain, listing_year) %>%
  summarise(n = n()) %>%
  compute_p()

dt_national

dt_national_new_used <- dt %>%
  group_by(powertrain, listing_year, inventory_type) %>%
  summarise(n = n()) %>%
  compute_p()

dt_national_new_used

# Plot

plot_zev <- dt_zev %>%
  rbind(
    dt_national %>%
      mutate(state_type = 'National') %>%
      select(state_type, listing_year, p_ev)
  ) %>%
  left_join(data.frame(
    state_type = c('CA', 'Non-ZEV', 'ZEV', 'National'),
    line = c('one', 'one', 'one', 'two')
  )) %>%
  ggplot(
    aes(
      x = listing_year,
      y = p_ev,
      group = state_type,
      color = state_type
    )
  ) +
  geom_line(aes(linetype = line)) +
  geom_point() +
  geom_text(
    data = data.frame(
      listing_year = 2019.5,
      p_ev = c(0.45, 0.24, 0.12, 0.16),
      state_type = c('CA', 'ZEV', 'Non-ZEV', 'National')
    ),
    mapping = aes(x = listing_year, y = p_ev, label = state_type),
    family = font,
    fontface = 'bold',
    size = 4.3
  ) +
  theme_minimal_grid(font_family = font, font_size = 16) +
  theme(
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA),
    legend.position = 'none'
  ) +
  scale_color_manual(
    values = c('firebrick', 'black', '#00BA38', '#619CFF')
  ) +
  scale_y_continuous(
    limits = c(0, 0.5),
    labels = scales::percent
  ) +
  labs(
    x = "Year",
    y = "Percentage of dealerships",
    title = "Percentage of dealerships with at least one BEV"
  )

plot_zev

ggsave(
  here("figs", "dealer_bev_percent_all_markets.png"),
  plot_zev,
  height = 5, width = 6.5
)

# New & Used Market ----

plot_zev_new_used <- dt_zev_new_used %>%
  rbind(
    dt_national_new_used %>%
      mutate(state_type = 'National') %>%
      select(inventory_type, listing_year, state_type, p_ev)
  ) %>%
  left_join(data.frame(
    state_type = c('CA', 'Non-ZEV', 'ZEV', 'National'),
    line = c('one', 'one', 'one', 'two')
  )) %>%
  mutate(inventory_type = paste(str_to_title(inventory_type), 'Market')) %>%
  ggplot(
    aes(
      x = listing_year,
      y = p_ev,
      group = state_type,
      color = state_type
    )
  ) +
  geom_line(aes(linetype = line)) +
  geom_point() +
  geom_text(
    data = data.frame(
      listing_year = c(
        2019.1, 2019.1, 2019.1, 2019.1, # New Market
        2019.1, 2019.1, 2019.1, 2019.1  # Used Market
      ),   
      p_ev = c(
        0.44, 0.30, 0.12, 0.22, # New Market (CA, ZEV, Non-ZEV, National)
        0.45, 0.22, 0.11, 0.16  # Used Market (CA, ZEV, Non-ZEV, National)
      ),
      state_type = rep(c('CA', 'ZEV', 'Non-ZEV', 'National'), 2),
      inventory_type = rep(c('New Market', 'Used Market'), each = 4)
    ),
    mapping = aes(x = listing_year, y = p_ev, label = state_type),
    family = font,
    fontface = 'bold',
    size = 4.3,
    hjust = -0.1  # This will shift the labels slightly to the right
  ) +
  facet_wrap(~inventory_type) +
  theme_minimal_grid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA),
    legend.position = 'none'
  ) +
  scale_color_manual(
    values = c('firebrick', 'black', '#00BA38', '#619CFF')
  ) +
  scale_y_continuous(
    limits = c(0, 0.7),
    breaks = seq(0, 0.7, 0.1),
    labels = scales::percent
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Percentage of dealerships with at least one BEV"
  ) +
  panel_border()

plot_zev_new_used

ggsave(
  here("figs", "dealer_bev_percent_new_used.png"),
  plot_zev_new_used,
  height = 5, width = 9
)

plot_zev_new_used_v <- plot_zev_new_used +
  facet_wrap(~inventory_type, ncol = 1) 

ggsave(
  here("figs", "dealer_bev_percent_new_used_vertical.png"),
  plot_zev_new_used_v,
  height = 9, width = 5
)
