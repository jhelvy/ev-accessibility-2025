source(here::here('code','0-setup.R'))

get_travel_times <- function(
    df, 
    batch_dir,
    batch_size = 100,
    num_workers = 4
) {
  # Create directory structure if it doesn't exist
  dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Get existing batches to skip
  existing_batches <- list.files(batch_dir, pattern = "^batch_\\d+\\.parquet$") %>%
    gsub("^batch_([0-9]+)\\.parquet$", "\\1", .) %>%
    as.numeric() %>%
    sort()
  
  # Create batches
  total_rows <- nrow(df)
  total_batches <- ceiling(total_rows / batch_size)
  
  # Filter out already completed batches
  remaining_batches <- setdiff(seq_len(total_batches), existing_batches)
  
  if (length(remaining_batches) == 0) {
    message("All batches already processed!")
    return(invisible(NULL))
  }
  
  message("Processing ", length(remaining_batches), " remaining batches out of ", 
          total_batches, " total batches")
  
  # Set up parallel processing
  plan(multisession, workers = num_workers)
  
  # Function to process a single batch
  process_batch <- function(batch_num) {
    # Calculate indices for this batch
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(start_idx + batch_size - 1, total_rows)
    batch <- df[start_idx:end_idx, ]
    
    # Create source and destination matrices
    src <- data.frame(
      lon = batch$lng_c,
      lat = batch$lat_c
    )
    
    dst <- data.frame(
      lon = batch$lng_d,
      lat = batch$lat_d
    )
    
    # Try to get travel times
    result <- tryCatch({
      travel_times <- osrmTable(
        src = src,
        dst = dst,
        measure = c("duration", "distance")
      )
      
      # If successful, add results to batch
      batch$duration_minutes <- diag(travel_times$durations)
      batch$distance_trip_km <- diag(travel_times$distances) / 1000
      batch$attempt_successful <- ifelse(is.na(batch$duration_minutes), FALSE, TRUE)
      batch
      
    }, error = function(e) {
      # Return batch with NAs and mark as failed
      batch$duration_minutes <- NA_real_
      batch$distance_trip_km <- NA_real_
      batch$attempt_successful <- FALSE
      batch
    })
    
    # Add batch number to result for tracking
    result$batch_num <- batch_num
    
    # Save batch results
    batch_file <- file.path(batch_dir, sprintf("batch_%05d.parquet", batch_num))
    write_parquet(result, batch_file)
    
    invisible(NULL)
  }
  
  # Process remaining batches in parallel
  future_map(
    remaining_batches,
    safely(process_batch),  # Use safely to prevent errors from stopping execution
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )
  
  # Find failed batches
  all_batches <- list.files(batch_dir, pattern = "^batch_\\d+\\.parquet$", full.names = TRUE)
  failed_batches <- lapply(all_batches, function(file) {
    batch_data <- read_parquet(file)
    if (any(!batch_data$attempt_successful)) {
      return(unique(batch_data$batch_num))
    }
    return(NULL)
  }) %>%
    unlist() %>%
    sort()
  
  if (length(failed_batches) > 0) {
    message("\nBatches with errors: ", paste(failed_batches, collapse = ", "))
    message("To reprocess failed batches, run again with these batch numbers")
  }
  
  invisible(NULL)
}

# Helper function to get failed batches
get_failed_batches <- function(batch_dir) {
  all_batches <- list.files(batch_dir, pattern = "^batch_\\d+\\.parquet$", full.names = TRUE)
  failed_batches <- lapply(all_batches, function(file) {
    batch_data <- read_parquet(file)
    if (any(!batch_data$attempt_successful)) {
      return(unique(batch_data$batch_num))
    }
    return(NULL)
  }) %>%
    unlist() %>%
    sort()
  
  return(failed_batches)
}

# Read in coords data

coords_tesla <- read_parquet(here::here('data', 'counts', 'tesla.parquet')) %>% 
  select(dealer_id, lat_d, lng_d) %>% 
  distinct()
coords_dealer <- make_dealer_dict(ds) %>% 
  select(dealer_id, lat_d = latitude, lng_d = longitude) %>% 
  bind_rows(coords_tesla) %>% 
  distinct() %>% 
  as.data.table()
coords_tract <- read_parquet(here::here('data', 'tract_dt.parquet')) %>% 
  select(lat_c, lng_c, GEOID) %>% 
  unique()

# Compute times ----

dealer_distances_all_agg <- read_parquet(
  here::here('data_local', 'dealer_distances_all.parquet')
)
dealer_distances_25_agg <- read_parquet(
  here::here('data_local', 'dealer_distances_25.parquet')
)

pairs <- rbind(dealer_distances_all_agg, dealer_distances_25_agg) %>% 
  distinct(dealer_id, GEOID)

# Join on coords
pairs <- pairs %>% 
  left_join(coords_tract, by = 'GEOID') %>% 
  left_join(coords_dealer, by = 'dealer_id') 
pairs1 <- pairs[1:2000000,]
pairs2 <- pairs[2000001:nrow(pairs),]
rm(pairs)
gc()

# Step 1: Process all batches in two batches 
# (because of RAM limits on {future})

# tictoc::tic()
get_travel_times(
  pairs1,
  batch_dir = file.path("data_local", "times1", "batches")
)
# tictoc::toc()
# 2684.881 sec elapsed, 44.75 min

# tictoc::tic()
get_travel_times(
  pairs2,
  batch_dir = file.path("data_local", "times2", "batches")
)
# tictoc::toc()
# 2398.471 sec elapsed, 39.97 min

# Step 2: Once all batches are done, combine them

df <- rbindlist(list(
  open_dataset(file.path("data_local", "times1")) %>%
    collect(),
  open_dataset(file.path("data_local", "times2")) %>% 
    collect()
))

# Check for any failures
temp <- df %>% 
  filter(attempt_successful == FALSE)
dim(temp)

# Final merge
df %>% 
  select(
    GEOID, dealer_id, duration_min = duration_minutes, distance_trip_km
  ) %>% 
  distinct() %>% 
  write_parquet(
    here::here('data_local', 'dealer_times.parquet')
  )
