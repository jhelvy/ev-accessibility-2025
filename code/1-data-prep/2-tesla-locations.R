source(here::here('code','0-setup.R'))

# Read in the raw tesla data from the saved tesla store html page
tesla_location <- here::here('data', 'tesla_store.html')
# Read the HTML content from the URL
webpage <- read_html(tesla_location)

extended_address <- webpage %>%
  html_nodes(".locality-city-postal") %>% 
  html_text() %>% 
  str_replace_all("[\n,]", "") %>% 
  str_trim() %>% 
  str_split('                    ', simplify = TRUE) %>% 
  as.data.frame() %>% 
  mutate(city_state_zip = paste(V1, V2, V3, sep = ', '))

address <- webpage %>%
  html_nodes(".street-address-states") %>% 
  html_text() %>% 
  str_replace_all("[\n,]", "") %>% 
  str_trim() %>% 
  paste0(", ", extended_address$city_state_zip, sep = "") 

df_tesla_addr <- data.frame(
  address = as.character(address), 
  stringsAsFactors = FALSE
)

# Geocode the addresses
geocoded <- df_tesla_addr %>%
  geocode(
    address = address, 
    method = 'osm',
    verbose = TRUE,
    min_time = 1  # Ensure we wait at least 1 second between requests
  )

# Fill in missing with hand-coded coords (searched google maps by hand)

# geocoded %>% 
#   filter(is.na(lat)) %>% 
#   write_csv(here::here('data', 'tesla_missing.csv'))

tesla_missing <- fread(here::here('data', 'tesla_missing.csv'))
geocoded %>%
  filter(!is.na(lat)) %>%
  rbind(tesla_missing) %>% 
  write_csv(here::here('data', 'tesla_dealer.csv'))

# Create Tesla counts

counts_tesla <- fread(here::here('data', 'tesla_dealer.csv')) %>% 
  mutate(dealer_id = row_number() + 100000) %>% 
  select(dealer_id, lat_d = lat, lng_d = long) %>% 
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
