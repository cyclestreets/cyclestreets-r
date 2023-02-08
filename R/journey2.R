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
#' A full list of variables (`cols`) available is represented by:
#' ```
#' c("time", "busynance", "signalledJunctions", "signalledCrossings",
#' "name", "walk", "elevations", "distances", "start", "finish",
#' "startSpeed", "start_longitude", "start_latitude", "finish_longitude",
#' "finish_latitude", "crow_fly_distance", "event", "whence", "speed",
#' "itinerary", "plan", "note", "length", "quietness",
#' "west", "south", "east", "north", "leaving", "arriving", "grammesCO2saved",
#' "calories", "edition", "geometry")
#' ```
#'
#' See [www.cyclestreets.net/help/journey/howitworks/](https://www.cyclestreets.net/help/journey/howitworks/)
#' for details on how these are calculated.
#'
#' @param from sf points
#' @param to sf points
#' @param plan Text strong of either "fastest" (default), "quietest" or "balanced"
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#' @param pat The API key used. By default this uses `Sys.getenv("CYCLESTREETS")`.
#' @param base_url The base url from which to construct API requests
#' (with default set to main server)
#' @param reporterrors Boolean value (TRUE/FALSE) indicating if cyclestreets (TRUE by default).
#' should report errors (FALSE by default).
#' @inheritParams json2sf_cs
#' @inheritParams smooth_with_cutoffs
#' @seealso json2sf_cs
#' @export
#' @examples
#' \dontrun{
#' from = c(-1.55, 53.80) # geo_code("leeds")
#' to = c(-1.76, 53.80) # geo_code("bradford uk")
#' r1 = journey(from, to)
#' names(r1)
#' r1[1:2, ]
#' r1$grammesCO2saved
#' r1$calories
#' plot(r1[1:4])
#' plot(r1[10:ncol(r1)])
#' to = c(-2, 53.5) # towards Manchester
#' r1 = journey(from, to)
#' names(r1)
#' r2 = journey(from, to, plan = "balanced")
#' plot(r1["quietness"], reset = FALSE)
#' plot(r2["quietness"], add = TRUE)
#' r3 = journey(from, to, silent = FALSE)
#' r4 = journey(from, to, save_raw = TRUE)
#' r5 = journey(c(-1.524, 53.819), c(-1.556, 53.806))
#' plot(r5["gradient_segment"])
#' plot(r5["gradient_smooth"])
#'
#' u = paste0("https://github.com/cyclestreets/cyclestreets-r/",
#'   "releases/download/v0.4.0/line_with_single_segment.geojson")
#' desire_line = sf::read_sf(u)
#' r = stplanr::route(l = desire_line, route_fun = journey)
#' r
#' }
journey2 <- function(fromPlace = NA,
                    toPlace = NA,
                    plan = "fastest",
                    pat = NULL,
                    base_url = "https://www.cyclestreets.net",
                    ncores = 10,
                    reporterrors = TRUE,
                    cols = c(
                      "name",
                      "distances",
                      "time",
                      "busynance",
                      "elevations",
                      "start_longitude",
                      "start_latitude",
                      "finish_longitude",
                      "finish_latitude"
                    ),
                    cols_extra = c(
                      "crow_fly_distance",
                      "event",
                      "whence",
                      "speed",
                      "itinerary",
                      "plan",
                      "note",
                      "length",
                      "quietness",
                      "west",
                      "south",
                      "east",
                      "north",
                      "leaving",
                      "arriving",
                      "grammesCO2saved",
                      "calories",
                      "edition"

                    )) {
  if (is.null(pat))
    pat = Sys.getenv("CYCLESTREETS")

  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  toPlace <- otp_clean_input(toPlace, "toPlace")

  routerUrl <- paste0(base_url, "/api/journey.json")

  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)

  fromPlace <- paste0(fromPlace[,2],",",fromPlace[,1])
  toPlace <- paste0(toPlace[,2],",",toPlace[,1])

  itinerarypoints <- paste0(fromPlace,"|",toPlace)

  query <- list(
    key = pat,
    plan = plan,
    reporterrors = as.numeric(reporterrors)
  )

  urls <- build_urls(routerUrl, itinerarypoints, query)
  message(Sys.time()," sending ",length(urls)," routes requests using ",ncores," threads")
  progressr::handlers("cli")
  results <- progressr::with_progress(otp_async(urls, ncores))

  if(length(results) == 0){
    stop("No results returned, check your connection")
  }

  message(Sys.time()," processing results")
  results_marker <- RcppSimdJson::fparse(results, query = "/marker", query_error_ok = TRUE, always_list = TRUE)

  # Process Marker
  names(results_marker) <- as.character(seq_len(length(results_marker)))
  results_marker <- lapply(results_marker, `[[`, "@attributes")

  results_marker <- lapply(results_marker, dplyr::bind_rows)
  results_marker <- dplyr::bind_rows(results_marker)

  geom_coordinates <- lapply(results_marker$coordinates, txt2coords2)
  geom_points <- lapply(results_marker$points, txt2coords2)

  results_marker$coordinates <- sf::st_sfc(geom_coordinates, crs = 4326)
  results_marker$points <- sf::st_sfc(geom_points, crs = 4326)

  return(results_marker)

}

build_urls <- function (routerUrl,itinerarypoints, query){
  secs <- unlist(query, use.names = TRUE)
  secs <- paste0(names(secs), "=", secs)
  secs <- paste(secs, collapse = "&")
  secs <- paste0(routerUrl,"?",secs,"&itinerarypoints=")
  secs <- paste0(secs, itinerarypoints)
  secs
}


txt2coords2 = function(txt) {
  if(is.na(txt)){
    return(NULL)
  }
  coords_split <- stringr::str_split(txt, pattern = " |,")[[1]]
  coords_split <- matrix(as.numeric(coords_split),
         ncol = 2,
         byrow = TRUE)
  sf::st_linestring(coords_split)
}

otp_clean_input <- function(imp, imp_name) {
  # For single point inputs
  if (all(class(imp) == "numeric")) {
    checkmate::assert_numeric(imp, len = 2)
    imp <- matrix(imp, nrow = 1, byrow = TRUE)
  }
  # For SF inputs
  if ("sf" %in% class(imp)) {
    if (all(sf::st_geometry_type(imp) == "POINT")) {
      imp <- sf::st_coordinates(imp)
      imp[] <- imp[, c(1, 2)]
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
    imp[] <- imp[, 2:1] # Switch round lng/lat to lat/lng for OTP
    colnames(imp) <- c("lat", "lon")
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

  # Success Function
  otp_success <- function(res){
    p()
    data <<- c(data, rawToChar(res$content))
  }
  # Fail Function
  otp_failure <- function(msg){
    p()
    cat("Error: ", msg, "\n")
  }

  t1 <- Sys.time()

  pool <- curl::new_pool(host_con = ncores)
  data <- list()

  for(i in seq_len(length(urls))){
    h <- curl::new_handle()
    curl::curl_fetch_multi(urls[i],
                           otp_success,
                           otp_failure ,
                           pool = pool,
                           handle = h)
  }
  p <- progressr::progressor(length(urls))
  curl::multi_run(timeout = Inf, pool = pool)
  t2 <- Sys.time()
  message("Done in ",round(difftime(t2,t1, units = "mins"),1)," mins")
  return(unlist(data, use.names = FALSE))
}
