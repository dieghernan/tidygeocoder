selected_method <- "mapquest"

url_base <- tidygeocoder:::get_mapquest_url(reverse = TRUE)

lat <- 40.4606543
lon <- -4.205381

# Test Sandbox on dev ----

soup <-
  httr::GET(
    url = url_base,
    query = list(
      maxResults = 5,
      key = tidygeocoder:::get_key(selected_method),
      location = paste0(as.character(lat), ",", as.character(lon))
    )
  )


response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))

address <- response$results$locations[[1]]

address <- address[, names(address) %in% c("street", paste0("adminArea", seq(6, 1))), ]

address <- as.character(apply(address, 1, function(x) {
  y <- unique(as.character(x))
  y <- y[!y %in% c("", "NA")]
  paste0(y, collapse = ", ")
}))

address[address != "NA"] <- NA

address <- data


tidygeocoder:::extract_reverse_results(selected_method, response,
  full_results = FALSE
)

results_full <-
  tidygeocoder:::extract_reverse_results(selected_method, response)

results_full

full_results_notflat <-
  tidygeocoder::extract_reverse_results(selected_method,
    response,
    full_results = TRUE,
    flatten = FALSE
  )

full_results_notflat

full_results_flat <-
  tidygeocoder::extract_reverse_results(selected_method,
    response,
    full_results = TRUE,
    flatten = TRUE
  )

full_results_flat

# Test reverse_geo ----

library(tibble)

lat <- 40.4606543
lon <- -4.205381
tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  address = "dir",
  verbose = TRUE,
  method = "mapquest"
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "mapquest",
  mapquest_open = TRUE
)

fullparams <- tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "mapquest",
  full_results = TRUE,
  custom_query = list(
    includeRoadMetadata = TRUE,
    includeNearestIntersection = TRUE,
    thumbMaps = FALSE
  )
)

glimpse(fullparams)

livetest <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    method = "mapquest"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    method = "mapquest"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "mapquest"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::reverse_geo(
    lat = c(48.858296, 40.4530541),
    long = c(2.294479, -3.6883445),
    verbose = TRUE,
    full_results = TRUE,
    mode = "single",
    limit = 3,
    method = "mapquest",
    custom_query = list(
      thumbMaps = FALSE
    ),
  )

glimpse(livetest_params)


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
    mode = "single",
    custom_query = list(
      thumbMaps = FALSE
    ),
  )
glimpse(address)
