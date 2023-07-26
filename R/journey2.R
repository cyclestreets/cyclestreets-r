#' Plan a journey with CycleStreets.net
#'
#' R interface to the CycleStreets.net journey planning API,
#' a route planner made by cyclists for cyclists.
#' See [cyclestreets.net/api](https://www.cyclestreets.net/api/) for details.
#'
#' @details
#' Requires the internet and a CycleStreets.net API key.
#' CycleStreets.net does not yet work worldwide.
#'
#' You need to have an api key for this code to run.
#' By default it uses the CYCLESTREETS environment variable.
#' A quick way to set this is to install the `usethis` package and then
#' executing the following command:
#'
#' `usethis::edit_r_environ()`
#'
#' That should open up a new file in your text editor where you
#' can add the environment variable as follows
#' (replace 1a... with your key for this to work):
#'
#' CYCLESTREETS=1a43ed677e5e6fe9
#'
#' After setting the environment variable, as outlined above,
#' you need to restart your R session before the journey function will work.
#'
#'
#' See [www.cyclestreets.net/help/journey/howitworks/](https://www.cyclestreets.net/help/journey/howitworks/)
#' for details on how these are calculated.
#'
#' @param fromPlace sf points, matrix, or vector of lng/lat coordinates
#' @param toPlace sf points, matrix, or vector of lng/lat coordinates
#' @param id a character ID value to be attached to the results
#' @param plan Text strong of either "fastest" (default), "quietest" or "balanced"
#' @param pat The API key used. By default this uses `Sys.getenv("CYCLESTREETS")`.
#' @param base_url The base url from which to construct API requests
#' (with default set to main server)
#' @param host_con number of threads to use passed to curl::new_pool
#' @param reporterrors Boolean value (TRUE/FALSE) indicating if cyclestreets (TRUE by default).
#' should report errors (FALSE by default).
#' @param segments Logical, if true route segments returned otherwise whole routes
#' @seealso json2sf_cs2
#' @export
#' @examples
#' \dontrun{
#' from = c(-1.55, 53.80) # geo_code("leeds")
#' to = c(-1.76, 53.80) # geo_code("bradford uk")
#' r1 = journey(from, to)
#' r2 = journey2(from, to, segments = TRUE)
#' # waldo::compare(r1, r2) # see differences
#' sum(sf::st_length(r1))
#' sum(sf::st_length(r2))
#' # waldo::compare(sum(sf::st_length(r1)), sum(sf::st_length(r2)))
#' # waldo::compare(names(r1), names(r2))
#' # waldo::compare(r1[1, ], r2[1, ])
#' r1[1:2, ]
#' r2[1:2, ]
#' r1$grammesCO2saved
#' r1$calories
#' }
journey2 = function(fromPlace = NA,
                    toPlace = NA,
                    id = NULL,
                    plan = "fastest",
                    pat = NULL,
                    base_url = "https://www.cyclestreets.net",
                    host_con = 1,
                    reporterrors = TRUE,
                    segments = FALSE) {
  if (is.null(pat))
    pat = Sys.getenv("CYCLESTREETS")

  fromPlace = otp_clean_input(fromPlace, "fromPlace")
  toPlace = otp_clean_input(toPlace, "toPlace")

  routerUrl = paste0(base_url, "/api/journey.json")

  fromPlace = format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  toPlace = format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)

  fromPlace = paste0(fromPlace[,2],",",fromPlace[,1])
  toPlace = paste0(toPlace[,2],",",toPlace[,1])

  itinerarypoints = paste0(fromPlace,"|",toPlace)

  query = list(
    key = pat,
    plan = plan,
    reporterrors = as.numeric(reporterrors)
  )

  urls = build_urls(routerUrl, itinerarypoints, query)

  progressr::handlers("cli")
  results_raw = progressr::with_progress(otp_async(urls, host_con))

  if(length(results_raw) == 0){
    stop("No results returned, check your connection")
  }

  message(Sys.time()," processing results")
  json2sf_cs2(results_raw, id = id, segments = segments)

}

build_urls = function (routerUrl,itinerarypoints, query){
  secs = unlist(query, use.names = TRUE)
  secs = paste0(names(secs), "=", secs)
  secs = paste(secs, collapse = "&")
  secs = paste0(routerUrl,"?",secs,"&itinerarypoints=")
  secs = paste0(secs, itinerarypoints)
  secs
}

otp_clean_input = function(imp, imp_name) {
  # For single point inputs
  if (all(class(imp) == "numeric")) {
    checkmate::assert_numeric(imp, len = 2)
    imp = matrix(imp, nrow = 1, byrow = TRUE)
  }
  # For SF inputs
  if ("sf" %in% class(imp)) {
    if (all(sf::st_geometry_type(imp) == "POINT")) {
      imp = sf::st_coordinates(imp)
      imp[] = imp[, c(1, 2)]
    } else {
      stop(paste0(imp_name, " contains non-POINT geometry"))
    }
  }

  # For matrix inputs
  # if (all(class(imp) == "matrix")) { # to pass CRAN checks
  if ("matrix" %in% class(imp)) {
    checkmate::assert_matrix(imp,
                             any.missing = FALSE,
                             min.rows = 1,
                             min.cols = 2,
                             max.cols = 2,
                             null.ok = FALSE
    )
    checkmate::assert_numeric(imp[, 1],
                              lower = -180, upper = 180,
                              any.missing = FALSE, .var.name = paste0(imp_name, " Longitude")
    )
    checkmate::assert_numeric(imp[, 2],
                              lower = -90, upper = 90,
                              any.missing = FALSE, .var.name = paste0(imp_name, " Latitude")
    )
    imp[] = imp[, 2:1] # Switch round lng/lat to lat/lng for OTP
    colnames(imp) = c("lat", "lon")
    return(imp)
  }
  # Otherwise stop as invalid input
  stop(paste0(
    imp_name,
    " is not in a valid format ",
    paste(class(imp), collapse = ", ")
  ))
}

otp_async <- function(urls, ncores){

  t1 <- Sys.time()
  p <- progressr::progressor(length(urls))
  out <- vector('list', length(urls))
  pool <- curl::new_pool(host_con = ncores)
  lapply( seq_along(urls), function(i){
    h <- curl::new_handle()
    success <- function(res){
      p()
      out[[i]] <<- rawToChar(res$content)
    }
    failure <- function(res){
      p()
      cat("Error: ", res, "\n")
      out[[i]] <<- paste0("Error: ", res)
    }
    curl::curl_fetch_multi(urls[i],
                           done = success,
                           fail = failure,
                           pool = pool,
                           handle = h)
  })
  curl::multi_run(timeout = Inf, pool = pool)
  t2 <- Sys.time()
  message("Done in ",round(difftime(t2,t1, units = "mins"),1)," mins")
  return(unlist(out, use.names = FALSE))
}

make_handle = function(x){
  handle = curl::new_handle()
  curl::handle_setopt(handle, copypostfields = paste0("routeid=", x))
  return(handle)
}


get_values = function(v, fun) {
  sapply(v, function(x) fun(as.numeric(x)))
}

extract_values = function(x) stringr::str_split(x, pattern = ",")
get_mean = function(v) get_values(v, fun = mean)
get_sum = function(v) get_values(v, fun = sum)
get_min = function(v) get_values(v, fun = min)
get_max = function(v) get_values(v, fun = max)

# Aim: add these columns
# [17] "gradient_segment"        "elevation_change"        "gradient_smooth"
# Tests:
# r1 = sf::read_sf("data-raw/r_1.geojson")
# add_columns(r1)

add_columns = function(r) {

  elevations_list = extract_values(r$elevations)

  elevation_min = get_min(elevations_list)
  elevation_max = get_max(elevations_list)
  distances_list = extract_values(r$distances)
  # # Should be this for clearer name:
  # r$segment_length = get_sum(distances_list)
  # But for compatibility with original journey() we'll go with this:
  r$distances = get_sum(distances_list)
  elevation_change = elevation_max - elevation_min
  # Order for compatibility with journey:
  r$gradient_segment = elevation_change / r$distances
  r$elevation_change = elevation_max - elevation_min
  r$gradient_smooth = cyclestreets::smooth_with_cutoffs(
    r$gradient_segment,
    r$elevation_change,
    r$distances,
    distance_cutoff = 50,
    gradient_cutoff = 0.1,
    n = 3,
  )
  r
}

json2sf_cs2 = function(results_raw, id, segments){
  results = RcppSimdJson::fparse(results_raw, query = "/marker", query_error_ok = TRUE, always_list = TRUE)
  results_error = RcppSimdJson::fparse(results_raw, query = "/error", query_error_ok = TRUE, always_list = TRUE)
  results_error = unlist(results_error, use.names = FALSE)
  if(length(results_error) > 0){
    message(length(results_error)," routes returned errors. Unique error messages are:\n")
    results_error = as.data.frame(table(results_error))
    results_error = results_error[order(results_error$Freq, decreasing = TRUE),]
    for(msgs in seq_len(nrow(results_error))){
      message(results_error$Freq[msgs],'x messages: "',results_error$results_error[msgs],'"\n')
    }
  }

  # Process Marker
  results = lapply(results, `[[`, "@attributes")
  if(!is.null(id)){
    names(results) = as.character(id)
  }

  results = lapply(results, data.table::rbindlist, fill = TRUE)
  results = data.table::rbindlist(results, idcol = "id", fill = TRUE)

  if(nrow(results) == 0){
    stop("No valid results returned")
  }

  route_variables = c("start","finish","start_longitude","start_latitude","finish_longitude","finish_latitude",
                      "crow_fly_distance","event","whence","speed","itinerary","plan",
                      "note","length","west","south","east","north","leaving","arriving",
                      "grammesCO2saved","calories","edition")

  if(segments){
    results$SPECIALIDFORINTERNAL2 = cumsum(!is.na(results$start))

    results_seg = results[results$type == "segment",]
    results_seg$geometry = sf::st_sfc(lapply(results_seg$points, txt2coords2), crs = 4326)

    results_rt = results[results$type == "route",]
    results_rt = results_rt[,names(results_rt) %in% c(route_variables,"SPECIALIDFORINTERNAL2"), with = FALSE]

    results_seg = results_seg[,!names(results_seg) %in% route_variables, with = FALSE]
    results_seg = dplyr::left_join(results_seg, results_rt, by = "SPECIALIDFORINTERNAL2")

    results = results_seg

  } else {
    results = results[results$type == "route",]
    results$geometry = sf::st_sfc(lapply(results$coordinates, txt2coords2), crs = 4326)
  }

  results$points = NULL
  results$coordinates = NULL
  results = sf::st_as_sf(results)

  # message("results may not be in the order they were provided")
  results = add_columns(results)
  results$SPECIALIDFORINTERNAL2 <- NULL
  results
}


