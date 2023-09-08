#' Quickly convert output from CycleStreets.net into sf object
#'
#' Available fields from CycleStreets include:
#'
#' ```
#' c("id", "time", "busynance", "quietness", "signalledJunctions",
#'   "signalledCrossings", "name", "walk", "elevations", "distances",
#'   "type", "legNumber", "distance", "turn", "startBearing", "color",
#'   "provisionName", "start", "finish", "start_longitude", "start_latitude",
#'   "finish_longitude", "finish_latitude", "crow_fly_distance", "event",
#'   "whence", "speed", "itinerary", "plan", "note", "length", "west",
#'   "south", "east", "north", "leaving", "arriving", "grammesCO2saved",
#'   "calories", "edition", "gradient_segment", "elevation_change",
#'   "gradient_smooth", "geometry")
#' ```
#'
#' @param results_raw Raw result from CycleStreets.net read-in with readLines or similar
#' @param id id of the result
#' @param segments Return segment level data? TRUE by default.
#' @param route_variables Route level variables
#' @param cols_to_keep Columns to return in output sf object
#' @export
#' @examples
#' from = "Leeds Rail Station"
#' to = "University of Leeds"
#' # from_point = tmaptools::geocode_OSM(from)
#' # to_point = tmaptools::geocode_OSM(to)
#' from_point = c(-1.54408, 53.79360)
#' to_point =   c(-1.54802, 53.79618)
#' # save result from the API call to journey.json
#' # res_json = journey(from_point, to_point, silent = FALSE, save_raw = TRUE)
#' # jsonlite::write_json(res_json, "inst/extdata/journey.json")
#' # f = "inst/extdata/journey.json"
#' f = system.file(package = "cyclestreets", "extdata/journey.json")
#' rsf = json2sf_cs(readLines(f), id = 1, segments = TRUE)
#' names(rsf)
#' json2sf_cs(readLines(f), id = 1, segments = TRUE, cols_to_keep = "quietness")
#' # save result from the API call to journey.json
#' # res_json = journey(from_point, to_point, silent = FALSE, save_raw = TRUE)
#' # jsonlite::write_json(res_json, "inst/extdata/journey_short.json")
#' # f = "inst/extdata/journey_short.json"
#' f = system.file(package = "cyclestreets", "extdata/journey_short.json")
#' obj = jsonlite::read_json(f, simplifyVector = TRUE)
#' # Inclusion of "start_longitude" leads to the additional ProvisionName1 colum:
#' cols = c("name", "distances", "provisionName")
#' json2sf_cs(readLines(f), id = 1, segments = TRUE, cols_to_keep = cols)
json2sf_cs = function(
    results_raw,
    id = 1,
    segments = TRUE,
    route_variables = c("start","finish","start_longitude","start_latitude","finish_longitude","finish_latitude",
                        "crow_fly_distance","event","whence","speed","itinerary","plan",
                        "note","length","west","south","east","north","leaving","arriving",
                        "grammesCO2saved","calories","edition"),
    cols_to_keep = c("id", "time", "busynance", "quietness", "signalledJunctions",
                     "signalledCrossings", "name", "walk", "elevations", "distances",
                     "type", "legNumber", "distance",
                     # "flow", # Deprecated on CS side
                     "turn", "startBearing",
                     "color", "provisionName", "start", "finish", "start_longitude",
                     "start_latitude", "finish_longitude", "finish_latitude", "crow_fly_distance",
                     "event", "whence", "speed", "itinerary", "plan", "note", "length",
                     "west", "south", "east", "north", "leaving", "arriving", "grammesCO2saved",
                     "calories", "edition")
){

  # Support both
  if(is.character(segments)){
    if(segments == "both"){
      do_segments = TRUE
      segments = as.character(segments)
    } else {
      stop("Unknow segments value, can be TRUE,FALSE,'both'")
    }
  } else {
    do_segments = segments
    segments = as.character(segments)
  }

  # browser()
  results = RcppSimdJson::fparse(results_raw, query = "/marker", query_error_ok = TRUE, always_list = TRUE)
  results_error = RcppSimdJson::fparse(results_raw, query = "/error", query_error_ok = TRUE, always_list = TRUE)
  rm(results_raw)
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

  cols_to_keep2 = unique(c(cols_to_keep,"type","start","points"))

  results = lapply(results, function(x){
    x = lapply(x, function(y){y[cols_to_keep2]})
    data.table::rbindlist(x, fill = TRUE)
  })

  results = data.table::rbindlist(results, idcol = "id", fill = TRUE)
  if(nrow(results) == 0){
    stop("No valid results returned")
  }

  if(do_segments){
    results$SPECIALIDFORINTERNAL2 = cumsum(!is.na(results$start))
    results_seg = results[results$type == "segment",]
    results_seg$geometry = sf::st_sfc(lapply(results_seg$points, txt2coords2), crs = 4326)
    results_rt = results[results$type == "route",]
    results_rt = results_rt[,names(results_rt) %in% c(route_variables,"SPECIALIDFORINTERNAL2"), with = FALSE]
    results_seg = results_seg[,!names(results_seg) %in% route_variables, with = FALSE]
    results_seg = dplyr::left_join(results_seg, results_rt, by = "SPECIALIDFORINTERNAL2")
    results_seg = cleanup_results(results_seg, cols_to_keep)
  } else {
    results = results[results$type == "route",]
    results$geometry = sf::st_sfc(lapply(results$coordinates, txt2coords2), crs = 4326)
    results = cleanup_results(results, cols_to_keep)
    return(results)
  }

  if(segments == "TRUE"){
    return(results_seg)

  } else if (segments == "both") {
    results = results[results$type == "route",]
    results$geometry = sf::st_sfc(lapply(results$coordinates, txt2coords2), crs = 4326)
    results = cleanup_results(results, cols_to_keep)
    return(list(routes = results, segments = results_seg))

  } else {
    stop("Invalid segments value, can be TRUE,FALSE,'both'")
  }

}

cleanup_results <- function(x, cols_to_keep){
  x$points = NULL
  x$coordinates = NULL

  x = add_columns(x)
  x = sf::st_as_sf(x)
  x$SPECIALIDFORINTERNAL2 <- NULL
  cols = cols_to_keep %in% names(x)
  x[cols_to_keep]
}
