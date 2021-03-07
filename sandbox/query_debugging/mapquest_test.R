selected_method <- "mapquest"

addr <- "Plaza del Azoguejo, Segovia"
url_base <- tidygeocoder:::get_mapquest_url()

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----
soup <-
  httr::GET(
    url = url_base,
    query = list(
      maxResults = 1,
      location = addr,
      key = tidygeocoder:::get_key(selected_method)
    )
  )

raw_results <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))

results_minimal <-
  tidygeocoder::extract_results(selected_method, raw_results, full_results = FALSE)
results_minimal

results <-
  tidygeocoder::extract_results(selected_method, raw_results)
results

full_results_notflat <-
  tidygeocoder::extract_results(selected_method,
    raw_results,
    full_results = TRUE,
    flatten = FALSE
  )
full_results_notflat

full_results_flat <-
  tidygeocoder::extract_results(selected_method,
    raw_results,
    full_results = TRUE,
    flatten = TRUE
  )
full_results_flat
# Test geo ----

livetest <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "mapquest"
  )
glimpse(livetest)

livetest_open <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "mapquest",
    mapquest_open = TRUE,
  )
glimpse(livetest_open)

livetest_full <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    method = "mapquest"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "mapquest"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::geo(
    address = c("Santiago de Compostela; Spain", "Nieva"),
    verbose = TRUE,
    full_results = TRUE,
    limit = 2,
    custom_query = list(
      intlMode = "1BOX",
      thumbMaps = FALSE
    ),
    method = "mapquest"
  )

glimpse(livetest_params)

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
lat_longs <- some_addresses %>%
  geocode(
    addr,
    method = "mapquest",
    lat = latitude,
    long = longitude,
    full_results = TRUE
  )

lat_longs

# Force batch
some_addresses %>%
  geocode(
    addr,
    method = "mapquest",
    lat = latitude,
    long = longitude,
    full_results = TRUE,
    mode = "batch",
    verbose = TRUE
  )
