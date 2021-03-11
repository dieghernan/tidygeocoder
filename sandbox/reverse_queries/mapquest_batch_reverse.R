limit <- 1
custom_query <- list(a = 1)
verbose <- TRUE
lat <- c(38.89586, 43.6534817, 600)
long <- c(-77.0307713, -79.3839347, -214)
address <- "direccion"
api_url <- NULL
full_results <- FALSE
mapquest_open <- TRUE

timeout <- 20

latLng <- as.character(paste0(lat, ",", long))


# Construct query
# Depends if single or multiple query

# Single: Now allowed on batch, return a single query ----
if (length(latLng) == 1) {
  results <- reverse_geo(
    lat = lat,
    long = long,
    mode = "single",
    full_results = full_results, custom_query = custom_query,
    verbose = verbose, api_url = api_url, limit = limit,
    mapquest_open = mapquest_open
  )


  # rename lat/long columns
  names(results)[names(results) == "address"] <- address

  return(results[!names(results) %in% c("lat", "long")])
}


# Multiple via POST ----
# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
if (is.null(api_url)) {
  url_domain <- if (mapquest_open) "http://open" else "http://www"
  api_url <- paste0(url_domain, ".mapquestapi.com/geocoding/v1/batch")
}

# Construct query - for display only
query_parameters <- get_api_query("mapquest", list(limit = limit, api_key = get_key("mapquest")),
  custom_parameters = custom_query
)
if (verbose == TRUE) display_query(api_url, query_parameters)

# Construct POST query
# A. Only certain parameters should be in the POST call
body_params <- query_parameters[!names(query_parameters) %in% c("key", "callback")]
query_parameters <- query_parameters[names(query_parameters) %in% c("key", "callback")]


# B. Construct Body
coords_list <- list(locations = latLng, options = body_params)

# Query API
raw_content <- query_api(api_url, query_parameters, mode = "list", input_list = coords_list, timeout = timeout)

# Note that flatten here is necessary in order to get rid of the
# nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
content <- jsonlite::fromJSON(raw_content, flatten = TRUE)

# Error handling
if (content$info$statuscode != 0) {
  if (verbose) message(content$info$messages, ". Returning NA results")
  return(get_na_value(lat, long, rows = nrow(address_df)))
}

# combine list of dataframes into a single tibble. Column names may differ between the dataframes
# MapQuest always return a default value (lat:39.4 long:-99.1) for non-found addresses

result_list <- content$results$locations
result_list_filled <- lapply(result_list, filler_df, c("street"))
results <- dplyr::bind_rows(result_list_filled)

# Prepare results
# Format fields to create full_address

field_address <- results[, names(results) %in% c("street", paste0("adminArea", seq(6, 1))), ]

full_address <- as.character(apply(field_address, 1, function(x) {
  y <- unique(as.character(x))
  y <- y[!y %in% c("", "NA")]
  paste0(y, collapse = ", ")
}))

full_address[full_address == "NA"] <- NA
results <- dplyr::bind_cols(tibble::tibble(full_address = full_address), results)

get_na_value(address, "xxx", rows = length(latLng))[address]


# Test live -----
library(tibble)
# white house, toronto, junk lat/lng
lat <- c(38.89586, 43.6534817, 600)
long <- c(-77.0307713, -79.3839347, -214)

tidygeocoder::reverse_geo(
  lat = lat,
  long = long,
  method = "mapquest", mode = "batch", verbose = TRUE
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = long,
  method = "mapquest", mode = "batch", verbose = TRUE,
  mapquest_open = TRUE,
  full_results = TRUE,
  address = "direccion"
)

# Single address
tidygeocoder::reverse_geo(lat = lat[1], long = long[1], method = "mapquest", full_results = TRUE, address = "direcc", mode = "single", verbose = TRUE)
tidygeocoder::reverse_geo(lat = lat[1], long = long[1], method = "mapquest", full_results = TRUE, address = "direcc", mode = "batch", verbose = TRUE)



tidygeocoder::reverse_geo(
  lat = lat,
  long = long,
  return_coords = FALSE,
  full_results = TRUE,
  limit = 3,
  method = "mapquest", mode = "batch", verbose = TRUE
)


tidygeocoder::reverse_geo(
  lat = lat,
  long = long,
  full_results = TRUE,
  limit = 1,
  method = "mapquest", verbose = TRUE, mapquest_open = TRUE
)


# Now try limit
latbatch <- runif(101, 38, 43)
longbatch <- runif(101, -8, 0)


fail <- tidygeocoder::reverse_geo(addresses <- paste(names, "Spain"),
  lat = latbatch,
  lon = longbatch,
  limit = 1,
  method = "mapquest", mode = "batch", verbose = TRUE
)

ok <- tidygeocoder::reverse_geo(
  lat = latbatch[1:100],
  lon = longbatch[1:100],
  return_coords = TRUE,
  full_results = TRUE,
  limit = 1,
  method = "mapquest", mode = "batch", verbose = TRUE
)


library(dplyr)
library(tibble)
library(tidygeocoder)

# create a dataframe with long lat
some_lonlat <- tribble(
  ~latitud, ~longitud,
  40.4165021, -3.7025642,
  41.3887869, 2.1589851,
  39.4697524, -0.3773868
)

# geocode the addresses

address <- some_lonlat %>%
  reverse_geocode(
    long = longitud, lat = latitud, method = "mapquest", address = "dir",
    full_results = TRUE,
    verbose = TRUE,
    custom_query = list(
      thumbMaps = FALSE
    ),
  )
glimpse(address)
