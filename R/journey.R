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
#' names(r1)
#' sf:::plot.sf(r1)
#' to = c(-2, 53.5) # towards manchester
#' r1 = journey(from, to)
#' r2 = journey(from, to, plan = "balanced")
#' plot(r1["quietness"], reset = FALSE)
#' plot(r2["quietness"], add = TRUE)
#' r3 = journey(from, to, silent = FALSE)
#' r4 = journey(from, to, save_raw = TRUE)
#' r5 = journey(from, to, cols = NULL)
#' r6 = journey(from, to, cols = "distances", cols_extra = "gradient_p75")
#' plot(r6)
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
                    ), cols_extra = c(
                      "elevation_start",
                      "elevation_end",
                      "provisionName",
                      "quietness"
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
    r = json2sf_cs(obj, cols = cols, cols_extra = cols_extra)
  }
  r
}

# obj = jsonlite::read_json(f, simplifyVector = TRUE)

txt2coords = function(txt) { # helper function to document...
  coords_split <- stringr::str_split(txt, pattern = " |,")[[1]]
  matrix(as.numeric(coords_split), ncol = 2, byrow = TRUE)
}
# txt2coords(obj$marker$`@attributes`$points[2])

# e = obj$marker$`@attributes`$elevations[1] # for whole journey
# e1 = obj$marker$`@attributes`$elevations[2] # for 1st segment
# txt = obj$marker$`@attributes`$elevations[2] # for 2nd segment

txt2elevations = function(txt) { # helper function to document...
  coords_split <- stringr::str_split(txt, pattern = ",")[[1]]
  as.numeric(coords_split)
}

#' Convert output from CycleStreets.net into sf object
#'
#' @param obj Object from CycleStreets.net read-in with
#' @param cols Columns to be included in the result, a character vector or `NULL` for all available columns (see details for default)
#' @param cols_extra Additional columns to be added providing summaries of gradient and other variables
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
#' f = system.file(package = "cyclestreets", "extdata/journey.json")
#' obj = jsonlite::read_json(f, simplifyVector = TRUE)
#' rsf = json2sf_cs(obj, cols = c("distances"))
#' names(rsf)
#' rsf
#' rsf2 = json2sf_cs(obj, cols = NULL, cols_extra = NULL)
#' names(rsf2)
#' # stplanr::line2points(rsf) extract start and end points
#' sf:::plot.sf(rsf)
#' json2sf_cs(obj, cols = c("time", "busynance", "elevations"))
json2sf_cs <- function(obj, cols = NULL, cols_extra = c(
  # "gradient_mean",
  # "gradient_median",
  # "gradient_p75",
  # "gradient_max",
  "elevation_start",
  "elevation_end",
  "gradient_segment",
  "provisionName",
  "quietness_segment"
)) {
  coord_list = lapply(obj$marker$`@attributes`$points[-1], txt2coords)
  elev_list = lapply(obj$marker$`@attributes`$elevations[-1], txt2elevations)
  elev_diff_list = lapply(elev_list, function(x) diff(stats::lag(x, 1)))
  # dist_list1 = geodist::geodist(rbind(coord_list[[1]][1, ], coord_list[[1]][2, ]), sequential = TRUE)
  # dist_list2 = geodist::geodist(
  #   data.frame(x = coord_list[[1]][, 1], y = coord_list[[1]][, 2]),
  #   sequential = TRUE
  #   )
  dist_list = lapply(coord_list, function(x) {
    geodist::geodist(
      data.frame(x = x[, 1], y = x[, 2]),
      sequential = TRUE
      )}
    )
  # gradient_list = purrr::map2(elev_diff_list, dist_list, ~.x / .y)
  gradient_list = mapply(function(x, y) x / y, elev_diff_list, dist_list)
  rsfl = lapply(coord_list, sf::st_linestring) %>%
    sf::st_sfc()

  # variables - constant
  n_segs = length(rsfl)
  # cols_lengths = sapply(obj$marker$`@attributes`, length)
  # cyclestreets_column_names = names(cols_lengths)
  # usethis::use_data(cyclestreets_column_names)
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
  vals_variable$elevation_mean = stringr::str_split(vals_variable$elevations, pattern = ",") %>%
    lapply(as.numeric) %>%
    lapply(mean) %>%
    unlist()
  vals_variable$elevation_start = stringr::str_split(vals_variable$elevations, pattern = ",") %>%
    lapply(as.numeric) %>%
    lapply(utils::head, 1) %>%
    unlist()
  vals_variable$elevation_end = stringr::str_split(vals_variable$elevations, pattern = ",") %>%
    lapply(as.numeric) %>%
    lapply(utils::tail, 1) %>%
    unlist()
  vals_variable$elevation_max = stringr::str_split(vals_variable$elevations, pattern = ",") %>%
    lapply(as.numeric) %>%
    lapply(max) %>%
    unlist()
  vals_variable$elevation_min = stringr::str_split(vals_variable$elevations, pattern = ",") %>%
    lapply(as.numeric) %>%
    lapply(min) %>%
    unlist()
  vals_variable$gradient_mean = lapply(gradient_list, function(x) mean(abs(x))) %>%
    unlist()
  vals_variable$gradient_median = lapply(
    gradient_list,
    function(x) stats::median(abs(x))
  ) %>%
    unlist()
  vals_variable$gradient_p75 = lapply(
    gradient_list,
    function(x) stats::quantile(abs(x), probs = 0.75)
  ) %>%
    unlist()
  vals_variable$gradient_max = lapply(
    gradient_list,
    function(x) max(abs(x))
  ) %>%
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

  # manually add records
  d_variable$gradient_segment = (vals_variable$elevation_max -
    vals_variable$elevation_min) / vals_variable$distances

  d_variable$provisionName = obj$marker$`@attributes`$provisionName[-1]
  d_variable$quietness_segment = d_variable$distances / d_variable$busynance
  if(!is.null(cols_extra)) {
    cols_extra_variable = c(cols, cols_extra)[c(cols, cols_extra) %in%
                                                names(d_variable)]
    d_variable = d_variable[cols_extra_variable]
  }
  d_all = cbind(d_variable, d_constant)

  if(!is.null(cols)) {
    # names(d_all)[! names(d_all) %in% c(cols, cols_extra)]
    # c(cols, cols_extra)[! c(cols, cols_extra) %in% names(d_all)]
    d_all = d_all[c(cols, cols_extra)]
  }

  # todo: create more segment-level statistics (vectors) +
  # add them to the data frame (d) below

  rsf = sf::st_sf(d_all, geometry = rsfl, crs = 4326)

  return(rsf)

}

