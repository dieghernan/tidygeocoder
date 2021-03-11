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
# ... are arguments passed from the geo() function
# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
reverse_batch_mapquest <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
                                   verbose = FALSE, api_url = NULL, mapquest_open = FALSE, limit = 1, ...) {
  
   latLng <- as.character(paste0(lat, ',', long))
  
  # Construct query  # Depends if single or multiple query
  
  # Single: Now allowed on batch, return a single query ----
  if (length(latLng) == 1) {
    results <- reverse_geo(lat = lat, long = long, mode = 'single',
      full_results = full_results, custom_query = custom_query, 
      verbose = verbose, api_url = api_url, limit = limit, 
      mapquest_open = mapquest_open)
    
    
    # rename lat/long columns
    names(results)[names(results) == 'address'] <- address
    
    return(results[!names(results) %in% c('lat', 'long')])
    
  } 
  
  
  # Multiple via POST ----
  # https://developer.mapquest.com/documentation/geocoding-api/batch/post/
  if (is.null(api_url)){
    url_domain <- if (mapquest_open) 'http://open' else 'http://www'
    api_url <- paste0(url_domain, '.mapquestapi.com/geocoding/v1/batch')
  } 
  
  # Construct query - for display only
  query_parameters <- get_api_query('mapquest', list(limit = limit, api_key = get_key('mapquest')),
                                    custom_parameters = custom_query)
  
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Construct POST query
  # A. Only certain parameters should be in the POST call
  body_params <- query_parameters[!names(query_parameters) %in% c('key', 'callback')]
  query_parameters <- query_parameters[names(query_parameters) %in% c('key', 'callback')]
  
  
  # B. Construct Body
  coords_list <- list(locations = latLng, options = body_params)    
  
  # Query API
  raw_content <- query_api(api_url, query_parameters, mode = 'list', input_list = coords_list, timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(raw_content, flatten = TRUE)
  
  # Error handling
  if (content$info$statuscode != 0){
    if (verbose) message(content$info$messages,'. Returning NA results')
    return(get_na_value(address, "xxx", rows = length(latLng))[address])
  }
  
  # combine list of dataframes into a single tibble. Column names may differ between the dataframes
  # MapQuest always return a default value (lat:39.4 long:-99.1) for non-found addresses
  
  result_list <- content$results$locations
  result_list_filled <- lapply(result_list, filler_df, c('street'))
  results <- dplyr::bind_rows(result_list_filled)
  
  # Prepare results----
  # Format fields to create full_address
  
  field_address <-  results[, names(results) %in% c('street', paste0('adminArea', seq(6, 1))), ]
  
  full_address <- as.character(apply(field_address, 1, function(x) {
    y <- unique(as.character(x))
    y <- y[!y %in% c('', 'NA')]
    paste0(y, collapse = ', ')
  }))
  
  full_address[full_address == 'NA'] <- NA
  results <- dplyr::bind_cols(tibble::tibble(full_address = full_address), results)
  
  names(results)[names(results) == 'full_address'] <- address
  
  if (full_results == FALSE)  return(results[address])
  else return(cbind(results[address], results[!names(results) %in% c(address)]))
  
}
