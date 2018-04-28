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
#' This can be set with `usethis::edit_r_environ()`.
#'
#' A full list of variables (`cols`) available is represented by:
#' ```
#' c("time", "busynance", "signalledJunctions", "signalledCrossings",
#' "name", "walk", "elevations", "distances", "start", "finish",
#' "startSpeed", "start_longitude", "start_latitude", "finish_longitude",
#' "finish_latitude", "crow_fly_distance", "event", "whence", "speed",
#' "itinerary", "clientRouteId", "plan", "note", "length", "quietness",
#' "west", "south", "east", "north", "leaving", "arriving", "grammesCO2saved",
#' "calories", "edition", "geometry")
#' ```
#'
#' @param from Longitude/Latitude pair, e.g. `c(-1.55, 53.80)`
#' @param to Longitude/Latitude pair, e.g. `c(-1.55, 53.80)`
#' @param plan Text strong of either "fastest" (default), "quietest" or "balanced"
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#' @param pat The API key used. By default this uses `Sys.getenv("CYCLESTREETS")`.
#' @param base_url The base url from which to construct API requests
#' (with default set to main server)
#' @param reporterrors Boolean value (TRUE/FALSE) indicating if cyclestreets (TRUE by default).
#' should report errors (FALSE by default).
#' @param save_raw Boolean value which returns raw list from the json if TRUE (FALSE by default).
#' @inheritParams json2sf_cs
#' @seealso json2sf_cs
#' @export
#' @examples
#' \dontrun{
#' from = c(-1.55, 53.80) # geo_code("leeds")
#' to = c(-1.76, 53.80) # geo_code("bradford uk")
#' r1 = journey(from, to)
#' sf:::plot.sf(r1)
#' to = c(-2, 53.5) # towards manchester
#' r1 = journey(from, to)
#' r2 = journey(from, to, plan = "balanced")
#' plot(r1["busynance"], reset = FALSE)
#' plot(r2["busynance"], add = TRUE)
#' r3 = journey(from, to, silent = FALSE)
#' r4 = journey(from, to, save_raw = TRUE)
#' r5 = journey(from, to, cols = NULL)
#' }
journey <- function(from, to, plan = "fastest", silent = TRUE,
                    pat = NULL,
                    base_url = "https://www.cyclestreets.net",
                    reporterrors = TRUE,
                    save_raw = "FALSE",
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
                    )) {

  if(is.null(pat)) pat = Sys.getenv("CYCLESTREETS")
  orig <- paste0(from, collapse = ",")
  dest <- paste0(to, collapse = ",")
  ft_string <- paste(orig, dest, sep = "|")

  httrmsg = httr::modify_url(
    base_url,
    path = "api/journey.json",
    query = list(
      key = pat,
      itinerarypoints = ft_string,
      plan = plan,
      reporterrors = ifelse(reporterrors == TRUE, 1, 0)
    )
  )

  if (silent == FALSE) {
    print(paste0("The request sent to cyclestreets.net was: ", httrmsg))
  }

  httrreq <- httr::GET(httrmsg)

  if (grepl('application/json', httrreq$headers$`content-type`) == FALSE) {
    stop("Error: CycleStreets did not return a valid result")
  }

  txt <- httr::content(httrreq, as = "text", encoding = "UTF-8")
  if (txt == "") {
    stop("Error: CycleStreets did not return a valid result")
  }

  obj <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)

  if (is.element("error", names(obj))) {
    stop(paste0("Error: ", obj$error))
  }

  if(save_raw) {
    return(obj)
  } else {
    r = json2sf_cs(obj, cols = cols)
  }
  r
}

txt2coords = function(txt) { # helper function to document...
  coords_split <- stringr::str_split(txt, pattern = " |,")[[1]]
  matrix(as.numeric(coords_split), ncol = 2, byrow = TRUE)
}
#' Convert output from CycleStreets.net into sf object
#'
#' @param obj Object from CycleStreets.net read-in with
#' @param cols Columns to be included in the result, a character vector or `NULL` for all available columns (see details for default)
#' @export
#' @examples
#' from = "Leeds Rail Station"
#' to = "University of Leeds"
#' # save result from the API call to journey.json
#' # res_json = stplanr::route_cyclestreet(from, to, silent = FALSE, save_raw = TRUE)
#' f = system.file(package = "cyclestreets", "extdata/journey.json")
#' obj = jsonlite::read_json(f, simplifyVector = TRUE)
#' rsf = json2sf_cs(obj)
#' sf:::plot.sf(rsf)
#' json2sf_cs(obj, cols = c("time", "busynance", "elevations"))
json2sf_cs <- function(obj, cols = NULL) {
  coord_list = lapply(obj$marker$`@attributes`$points[-1], txt2coords)
  rsfl = lapply(coord_list, sf::st_linestring) %>%
    sf::st_sfc()

  # variables - constant
  n_segs = length(rsfl)
  cols_na = sapply(obj$marker$`@attributes`, function(x) sum(is.na(x)))
  sel_constant = cols_na == n_segs &
    names(cols_na) != "coordinates"
  cols_constant = names(cols_na)[sel_constant]
  vals_constant = lapply(cols_constant, function(x)
    obj$marker$`@attributes`[[x]][1])
  names(vals_constant) = cols_constant
  suppressWarnings({
    vals_numeric = lapply(vals_constant, as.numeric)
  })
  sel_numeric = !is.na(vals_numeric)
  vals_constant[sel_numeric] = vals_numeric[sel_numeric]
  d_constant = data.frame(vals_constant)[rep(1, n_segs), ]

  # useful cols: busynance, name, elevations, distances, turn,provisionName

  sel_variable = cols_na == 0 &
    !grepl("startBearing|type", names(cols_na))
  cols_variable = names(cols_na)[sel_variable]
  vals_variable = lapply(cols_variable, function(x)
    obj$marker$`@attributes`[[x]][-1])
  names(vals_variable) = cols_variable
  # vals_variable # take a look - which ones to process?
  vals_variable$elevations = stringr::str_split(vals_variable$elevations, pattern = ",") %>%
    lapply(as.numeric) %>%
    lapply(mean) %>%
    unlist()
  vals_variable$distances = stringr::str_split(vals_variable$distances, pattern = ",") %>%
    lapply(as.numeric) %>%
    lapply(sum) %>%
    unlist()
  suppressWarnings({
    vals_vnumeric = lapply(vals_variable, as.numeric)
  })
  vals_vnumeric$name = vals_variable$name
  d_variable = data.frame(vals_vnumeric)

  d_all = cbind(d_variable, d_constant)

  if(!is.null(cols)) {
    d_all = d_all[cols]
  }

  # todo: create more segment-level statistics (vectors) +
  # add them to the data frame (d) below

  rsf = sf::st_sf(d_all, geometry = rsfl, crs = 4326)

  return(rsf)

}
