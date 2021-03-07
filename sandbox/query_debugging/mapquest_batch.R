limit <- 1
custom_query <- list(a = 1)
verbose <- TRUE
lat <- "latit"
long <- "longit"
api_url <-
  get_mapquest_url(mapquest_open = FALSE, batch = TRUE)
address_df <- tibble::tribble(~address, "Madrid, ES", "Denver, CO")
timeout <- 20


if (is.null(api_url)) {
  api_url <-
    get_mapquest_url(mapquest_open = mapquest_open, batch = TRUE)
}

# Construct query
# Just the api key should be in the query
query_parameters <-
  get_api_query("mapquest", list(api_key = get_key("mapquest")))

# Construct body
# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
# Options should be included on the body
options_body <- get_api_query("mapquest",
  list(limit = limit),
  custom_parameters = custom_query
)
# Now get the body
# List with locations and options
body <- list(
  locations = address_df[["address"]],
  options = options_body
)
# Display
if (verbose == TRUE) {
  display_query(
    api_url,
    get_api_query("mapquest",
      list(api_key = get_key("mapquest")),
      custom_parameters = list(
        locations = unlist(body$locations),
        options = unlist(body$options)
      )
    )
  )
}

raw_content <- query_api(
  api_url,
  query_parameters,
  mode = "list",
  input_list = body,
  timeout = timeout
)

content <- jsonlite::fromJSON(raw_content, flatten = TRUE)

results <- dplyr::bind_rows(content$results$locations)
results

names(results)[names(results) == "latLng.lat"] <- lat
names(results)[names(results) == "latLng.lng"] <- long

# Single line
address_df <- tibble::tribble(~address, "Madrid, ES")

custom_query[["location"]] <- address_df[["address"]]
# Convert our generic query parameters into parameters specific to our API (method)
query_parameters <- get_api_query("mapquest", list(limit = limit, api_key = get_key("mapquest")),
  custom_parameters = custom_query
)
if (verbose == TRUE) display_query(api_url, query_parameters)
# Request via GET
content <- jsonlite::fromJSON(query_api(api_url, query_parameters))
results <- extract_results("mapquest", content, full_results = TRUE, flatten = TRUE)

glimpse(results)



# Test live -----
tidygeocoder::geo(addresses <- c("Denver,CO", "Boulder,CO", "Santiago"),
  method = "mapquest", mode = "batch", verbose = TRUE
)

tidygeocoder::geo(addresses <- c("Denver,CO", "Boulder,CO", "Santiago"),
  full_results = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE
)

# single line
tidygeocoder::geo(addresses <- c("Denver"),
  lat = "latitude",
  long = "longitude",
  full_results = TRUE,
  return_addresses = FALSE,
  limit = 5,
  method = "mapquest", mode = "batch", verbose = TRUE
)
tidygeocoder::geo(addresses <- c("Denver"),
  lat = "latitude",
  long = "longitude",
  full_results = TRUE,
  return_addresses = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE
)

s <- tidygeocoder::geo(addresses <- c("Santiago"),
  full_results = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE,
  limit = 10,
  return_addresses = FALSE,
  custom_query = list(thumbMaps = FALSE)
)
tibble::glimpse(s)

tidygeocoder::geo(addresses <- c("Madrid, Spain"),
  full_results = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE
)

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

names2 <- esp_munic.sf[1:30, ]$name

full <- tidygeocoder::geo(addresses <- paste(names2, "Spain"),
  return_addresses = TRUE,
  full_results = TRUE,
  limit = 1,
  method = "mapquest", mode = "batch", verbose = TRUE
)
full
