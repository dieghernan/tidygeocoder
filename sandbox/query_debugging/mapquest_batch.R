limit <- 1
custom_query <- NULL
verbose <- TRUE
lat <- "latit"
long <- "longit"
api_url <-
  get_mapquest_url(mapquest_open = FALSE, batch = TRUE)
address_df <- tibble::tribble(~address, "Denver,CO", "Boulder,CO", "Santiago")


# Construct query - this is just for display
query_parameters_disp <- get_api_query("mapquest", list(limit = limit, api_key = get_key("mapquest")),
  custom_parameters = custom_query
)

if (verbose == TRUE) display_query(api_url, query_parameters_disp)

# Construct real query
query_parameters <- get_api_query("mapquest", list(api_key = get_key("mapquest")))

# Construct body
# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
names(address_df) <- "street"

address_list <- list()
for (index in 1:nrow(address_df)) {
  address_list[[index]] <- as.list(address_df[index, ])
}

options_body <- get_api_query("mapquest", list(
  limit = limit,
  custom_parameters = custom_query
))
# Now get the body
body <- list(
  locations = address_list,
  options = options_body
)


raw_content <- query_api(
  api_url,
  query_parameters,
  mode = "list",
  input_list = body,
  timeout = 20
)

content <- jsonlite::fromJSON(raw_content, flatten = TRUE)

results <- dplyr::bind_rows(content$results$locations)

names(results)[names(results) == "latLng.lat"] <- lat
names(results)[names(results) == "latLng.lng"] <- long


# Test live
tidygeocoder::geo(addresses <- c("Denver,CO", "Boulder,CO", "Santiago"),
  method = "mapquest", mode = "batch", verbose = TRUE
)

tidygeocoder::geo(addresses <- c("Denver,CO", "Boulder,CO", "Santiago"),
  full_results = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE
)

tidygeocoder::geo(addresses <- c("Denver,CO", "Boulder,CO", "Santiago"),
                  full_results = TRUE,
                  method = "mapquest", mode = "batch", verbose = TRUE,
                  mapquest_open = TRUE,
                  custom_query = list(ignoreLatLngInput = TRUE)
)

ss <- tidygeocoder::geo(addresses <- c("rwfffffff"),
                  full_results = TRUE,
                  method = "mapquest", mode = "batch", verbose = TRUE,
                  custom_query = list(thumbMaps = FALSE, ignoreLatLngInput = TRUE)
)
ss
tidygeocoder::geo(addresses <- c("Denver,CO", "Boulder,CO", "Santiago"),
                  return_addresses = FALSE,
                  full_results = TRUE,
                  limit = 7,
                  method = "mapquest", mode = "batch", verbose = TRUE
)

# Now try limit
library(mapSpain)
names <- esp_munic.sf[1:101, ]$name

tidygeocoder::geo(addresses <- paste(names, "Spain"),
                  return_addresses = TRUE,
                  full_results = TRUE,
                  limit = 1,
                  method = "mapquest", mode = "batch", verbose = TRUE
)

names2 <- esp_munic.sf[1:100, ]$name

full <- tidygeocoder::geo(addresses <- paste(names2, "Spain"),
                  return_addresses = TRUE,
                  full_results = TRUE,
                  limit = 1,
                  method = "mapquest", mode = "batch", verbose = TRUE
)
full
