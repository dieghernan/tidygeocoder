limit <- 1
custom_query <- list(a = 1)
verbose <- TRUE
lat <- c(38.89586, 43.6534817, 600)
long <- c(-77.0307713, -79.3839347, -214)
address <- "XXX"

latLong <- data.frame(
  lat = lat[1],
  long = long[1]
)
latLng <- as.character(paste0(latLong$lat, ",", latLong$long))

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
  locations = latLng,
  options = options_body
)

jsonlite::toJSON(body)

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

result_list <- content$results$locations
result_list_filled <- lapply(result_list, filler_df, c("street"))


results <- dplyr::bind_rows(result_list_filled)
results
full_address <- tibble::as_tibble(results[, names(results) %in% c("street", paste0("adminArea", seq(6, 1)))])

results$full_address <- apply(full_address, 1, function(x) {
  x1 <- x[!is.na(x)]
  paste(x1, collapse = ", ")
})
names(results)[names(results) == "full_address"] <- address

results


apply(full_address, 1, function(x) {
  x1 <- x[!is.na(x)]
  paste(x1, collapse = ", ")
})

names$formatted <- paste0(names[, names(names)], collapse = ", ")

names(results) %in% c("street", paste0("adminArea", seq(6, 1)))
paste0()
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
tidygeocoder::reverse_geo(
  lat = rep(c(38.89586, 43.6534817), 1),
  long = rep(c(-77.0307713, -79.3839347), 1),
  method = "mapquest", mode = "batch", verbose = TRUE,
)

tidygeocoder::reverse_geo(
  lat = rep(c(38.89586, 43.6534817), 1),
  long = rep(c(-77.0307713, -79.3839347), 1),
  full_results = TRUE,
  mapquest_open = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE
)

# single line
tidygeocoder::reverse_geo(
  lat = rep(c(38.89586), 1),
  long = rep(c(-77.0307713), 1),
  full_results = TRUE,

  limit = 5,
  method = "mapquest",
  mode = "batch",
  verbose = TRUE
)
tidygeocoder::reverse_geo(addresses <- c("Denver"),
  lat = "latitude",
  long = "longitude",
  full_results = TRUE,
  return_addresses = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE
)

s <- tidygeocoder::reverse_geo(addresses <- c("Santiago"),
  full_results = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE,
  limit = 10,
  return_addresses = FALSE,
  custom_query = list(thumbMaps = FALSE)
)
tibble::glimpse(s)

tidygeocoder::reverse_geo(addresses <- c("Madrid, Spain"),
  full_results = TRUE,
  method = "mapquest", mode = "batch", verbose = TRUE
)

tidygeocoder::reverse_geo(addresses <- c("Denver,CO", "Boulder,CO", "Santiago"),
  return_addresses = FALSE,
  full_results = TRUE,
  limit = 7,
  method = "mapquest", mode = "batch", verbose = TRUE
)

# Now try limit
library(mapSpain)
names <- esp_munic.sf[1:101, ]$name

tidygeocoder::reverse_geo(addresses <- paste(names, "Spain"),
  return_addresses = TRUE,
  full_results = TRUE,
  limit = 1,
  method = "mapquest", mode = "batch", verbose = TRUE
)

names2 <- esp_munic.sf[1:30, ]$name

full <- tidygeocoder::reverse_geo(addresses <- paste(names2, "Spain"),
  return_addresses = TRUE,
  full_results = TRUE,
  limit = 1,
  method = "mapquest", mode = "batch", verbose = TRUE
)
full
