### Functions for reverse batch geocoding that are called by reverse_geo()


# Reverse Batch geocoding with geocodio
# ... are arguments passed from the reverse_geo() function
# https://www.geocod.io/docs/#batch-geocoding
reverse_batch_geocodio <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
verbose = FALSE, api_url = NULL, geocodio_v = 1.6, limit = 1, ...) {
  
  if (is.null(api_url)) api_url <- get_geocodio_url(geocodio_v, reverse = TRUE)
  
  # Construct query
  query_parameters <- get_api_query('geocodio', list(limit = limit, api_key = get_key('geocodio')),
                                    custom_parameters = custom_query)
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  raw_content <- query_api(api_url, query_parameters, mode = 'list', 
                  input_list = paste0(as.character(lat), ',', as.character(long)), 
                  timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(raw_content, flatten = TRUE)
  response <- jsonlite::fromJSON(raw_content, flatten = TRUE)
  result_list <- response$results$response.results
  
  # if no results are returned for a given coordinate then there is a 0 row dataframe in this
  # list and we need to replace it with a 1 row NA dataframe to preserve the number of rows
  result_list_filled <- lapply(result_list, filler_df, c('formatted_address'))
  
  # combine list of dataframes into a single tibble. Column names may differ between the dataframes
  results <- dplyr::bind_rows(result_list_filled)
  
  names(results)[names(results) == 'formatted_address'] <- address
  
  if (full_results == FALSE)  return(results[address])
  else return(cbind(results[address], results[!names(results) %in% c(address)]))
}

# Reverse Batch geocoding with mapquest
# ... are arguments passed from the reverse_geo() function
# https://developer.mapquest.com/documentation/geocoding-api/batch/get/
# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
reverse_batch_mapquest <-
  function(lat,
           long,
           address = 'address',
           timeout = 20,
           full_results = FALSE,
           custom_query = list(),
           verbose = FALSE,
           api_url = NULL,
           geocodio_v = 1.6,
           limit = 1,
           mapquest_open = FALSE,
           ...) {
    # limit the dataframe to legitimate arguments
    latLong <- data.frame(lat = lat,
                          long = long)
    latLng <- as.character(paste0(latLong$lat, ",", latLong$long))
    print(latLong)
    # Display error on request above the limit
    if (nrow(latLong) > 100) {
      stop("MapQuest Batch limit of 100 addresses.", call. = FALSE)
    }
    if (is.null(api_url))
      api_url <-
      get_mapquest_url(mapquest_open = mapquest_open,
                       reverse = TRUE)
    
    # Construct query
    # Depends if single or multiple query
    # Single: GET (similar to single but different endpoint)
    # Multiple: POST
    if (nrow(latLong) == 1) {
      # Single via GET, similar to non-batch mode ----
      custom_query[["location"]] <- address_df[["address"]]
      
      # Convert our generic query parameters into parameters specific to our API (method)
      query_parameters <-
        get_api_query('mapquest',
                      list(limit = limit, api_key = get_key('mapquest')),
                      custom_parameters = custom_query)
      if (verbose == TRUE)
        display_query(api_url, query_parameters)
      
      # Request via GET
      content <-
        jsonlite::fromJSON(query_api(api_url, query_parameters))
      results <-
        extract_results("mapquest",
                        content,
                        full_results = TRUE,
                        flatten = TRUE)
      names(results)[names(results) == 'lat'] <- lat
      names(results)[names(results) == 'lng'] <- long
    } else {
      # Multiple via POST ----
      # https://developer.mapquest.com/documentation/geocoding-api/batch/post/
      # Just the api key should be in the query
      query_parameters <-
        get_api_query('mapquest', list(api_key = get_key('mapquest')))
      
      # Construct body
      # Options should be included on the body
      options_body <- get_api_query('mapquest',
                                    list(limit = limit),
                                    custom_parameters = custom_query)
      # Now get the body
      # List with locations and options
      body <- list(locations = latLng,
                   options = options_body)
      # Display - Bit weird, could be improved
      if (verbose == TRUE)
        display_query(api_url,
                      get_api_query(
                        'mapquest',
                        list(api_key = get_key('mapquest')),
                        custom_parameters = list(
                          locations = unlist(body$locations),
                          options = unlist(body$options)
                        )
                      ))
      
      raw_content <- query_api(
        api_url,
        query_parameters,
        mode = "list",
        input_list = body,
        timeout = timeout
      )
      
      content <- jsonlite::fromJSON(raw_content, flatten = TRUE)
      
      result_list <- content$results$locations
      result_list_filled <-
        lapply(result_list, filler_df, c('street'))
      results <- dplyr::bind_rows(result_list_filled)

      full_address <-
        tibble::as_tibble(results[, names(results) %in% c("street", paste0("adminArea", seq(6, 1)))])
      results$full_address <-
        apply(full_address, 1, function(x) {
          x1 <- x[!is.na(x)]
          paste(x1, collapse = ", ")
        })

      names(results)[names(results) == 'full_address'] <- address
    }
    # Prepare output----
    if (full_results == FALSE)  return(results[address])
    else return(cbind(results[address], results[!names(results) %in% c(address)]))
  }
