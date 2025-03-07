source(here::here('code','0-setup.R'))

api_key <- Sys.getenv("ggmap_api")
register_google(key = api_key)

tesla_location <- here::here('data','tesla_store.html')
# Read the HTML content from the URL
webpage <- read_html(tesla_location)

extended_address <- webpage %>%
  html_nodes(".locality-city-postal") %>% 
  html_text() %>% 
  str_replace_all("[\n,]", "") %>% 
  str_trim()

address <- webpage %>%
  html_nodes(".street-address-states") %>% 
  html_text() %>% 
  str_replace_all("[\n,]", "") %>% 
  str_trim() %>% 
  paste0(extended_address) %>% 
  str_replace_all("\\s+", " ")

df_tesla_addr <- data.frame(location = address)

locations_geo <- mutate_geocode(df_tesla_addr, location)

write_csv(locations_geo, here::here('data', 'tesla_dealer.csv'))

# Create Tesla counts

counts_tesla <- fread(here::here('data', 'tesla_dealer.csv')) %>% 
  mutate(dealer_id = row_number() + 100000) %>% 
  select(dealer_id, lat_d = lat, lng_d = lon) %>% 
  distinct()
counts_tesla$inventory_type <- 'new'
counts_tesla$bev <- 3
counts_tesla$cv <- 0
counts_tesla$hev <- 0
counts_tesla$phev <- 0

n_stores <- nrow(counts_tesla)
years <- seq(2016, 2021)
n_years <- length(years)

counts_tesla <- counts_tesla[rep(seq(n_stores), n_years),]
counts_tesla$listing_year <- rep(years, each = n_stores)

counts_tesla <- counts_tesla %>% 
  select(
    dealer_id, listing_year, inventory_type, bev, cv, hev, phev,
    lat_d, lng_d
  )

write_parquet(counts_tesla, here::here('data', 'counts', 'tesla.parquet'))
