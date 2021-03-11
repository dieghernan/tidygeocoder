limit <- 1
custom_query <- list(a = 1)
verbose <- TRUE
timeout <- 20
lat <- "latit"
long <- "longit"
api_url <- "http://www.mapquestapi.com/geocoding/v1/batch"

address_df <- tibble::tribble(~address, "Madrid, ES", "hahuauhauauhu", "Segovia")


if (is.null(api_url)) api_url <- "http://www.mapquestapi.com/geocoding/v1/batch"

# Construct query - for display only

query_parameters <- get_api_query("mapquest", list(limit = limit, api_key = get_key("mapquest")),
  custom_parameters = custom_query
)
if (verbose == TRUE) display_query(api_url, query_parameters)


# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
# Construct POST query

# A. Only certain parameters should be in the POST call

body_params <- query_parameters[!names(query_parameters) %in% c("key", "callback")]
query_parameters <- query_parameters[names(query_parameters) %in% c("key", "callback")]

# B. Construct Body
address_list <- list(
  locations = address_df[["address"]],
  options = body_params
)

# Query API
raw_content <- query_api(api_url, query_parameters, mode = "list", input_list = address_list, timeout = timeout)

# Note that flatten here is necessary in order to get rid of the
# nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
content <- jsonlite::fromJSON(raw_content, flatten = TRUE)


if (content$info$statuscode != 0) {
  if (verbose) message(content$info$messages, ". Returning NA results")
  return(get_na_value(lat, long, rows = nrow(address_df)))
}

# combine list of dataframes into a single tibble. Column names may differ between the dataframes
results <- dplyr::bind_rows(content$results$locations)

names(results)[names(results) == "latLng.lat"] <- lat
names(results)[names(results) == "latLng.lng"] <- long

tibble::as_tibble(results)


# Test live -----
library(tibble)


tidygeocoder::geo(
  address = c("Denver,CO", "Boulder,CO", "Santiago"),
  method = "mapquest", mode = "batch", verbose = TRUE
)

tidygeocoder::geo(
  address = c("Denver,CO", "Boulder,CO", "Santiago"),
  method = "mapquest", mode = "batch", verbose = TRUE,
  mapquest_open = TRUE,
  lat = "latitude", long = "longitude"
)

# Single address
tidygeocoder::geo(address = "Segovia", method = "mapquest", full_results = TRUE, lat = "latitude", long = "longitude")
tidygeocoder::geo(address = "Segovia", method = "mapquest", mode = "batch", full_results = TRUE, lat = "latitude", long = "longitude")



tidygeocoder::geo(
  address = c(
    "Denver,CO", "Boulder,CO",
    "abhnaabunu ", "Santiago"
  ),
  return_addresses = FALSE,
  full_results = TRUE,
  limit = 3,
  method = "mapquest", mode = "batch", verbose = TRUE
)


tidygeocoder::geo(
  address = c(
    "Denver,CO", "Boulder,CO",
    "abhnaabunu ", "Santiago"
  ),
  full_results = TRUE,
  limit = 1,
  method = "mapquest", verbose = TRUE, mapquest_open = TRUE
)


# Now try limit
library(mapSpain)
names <- esp_munic.sf$name

tidygeocoder::geo(addresses <- paste(names, "Spain"),
  return_addresses = TRUE,
  full_results = TRUE,
  limit = 1,
  method = "mapquest", mode = "batch", verbose = TRUE
)

tidygeocoder::geo(addresses <- paste(names[1:100], "Spain"),
  return_addresses = TRUE,
  full_results = TRUE,
  limit = 1,
  method = "mapquest", mode = "batch", verbose = TRUE
)


library(dplyr)
library(tibble)
library(tidygeocoder)

# create a dataframe with addresses
some_addresses <- tribble(
  ~name,
  ~addr,
  "White House",
  "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid",
  "600 Montgomery St, San Francisco, CA 94111",
  "Willis Tower",
  "233 S Wacker Dr, Chicago, IL 60606"
)

# geocode the addresses
some_addresses %>%
  geocode(
    addr,
    method = "mapquest",
    lat = latitude,
    long = longitude,
    full_results = TRUE
  )
