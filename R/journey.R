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
#' See [json2sf_cs()] for details.
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
#' @param smooth_gradient Identify and fix anomalous gradients? TRUE by default. See
#' https://github.com/Robinlovelace/cyclestreets/issues/14
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
journey = function(from,
                    to,
                    plan = "fastest",
                    silent = TRUE,
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
                      "edition",
                      "gradient_segment",
                      # "gradient_median",
                      # "gradient_p75",
                      # "gradient_max",
                      "elevation_change",
                      "provisionName"
                    ),
                    smooth_gradient = TRUE,
                    distance_cutoff = 50,
                    gradient_cutoff = 0.1,
                    n = 3,
                    warnNA = FALSE) {
  if (is.null(pat))
    pat = Sys.getenv("CYCLESTREETS")
  orig = paste0(from, collapse = ",")
  dest = paste0(to, collapse = ",")
  ft_string = paste(orig, dest, sep = "|")

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

  httrreq = httr::GET(httrmsg)

  if (grepl('application/json', httrreq$headers$`content-type`) == FALSE) {
    stop("Error: CycleStreets did not return a valid result")
  }

  txt = httr::content(httrreq, as = "text", encoding = "UTF-8")
  if (txt == "") {
    stop("Error: CycleStreets did not return a valid result")
  }

  obj = jsonlite::fromJSON(txt, simplifyDataFrame = TRUE, bigint_as_char = FALSE)

  if (is.element("error", names(obj))) {
    stop(paste0("Error: ", obj$error))
  }

  if (save_raw) {
    return(obj)
  } else {
    r = json2sf_cs(
      obj,
      cols = cols,
      cols_extra = cols_extra,
      smooth_gradient,
      distance_cutoff,
      gradient_cutoff,
      n,
      warnNA = warnNA
    )
  }
  sf::st_crs(r) = "EPSG:4326"
  r
}
#' Convert output from CycleStreets.net into sf object
#'
#' Available fields from CycleStreets include:
#'
#' ```
#' c("start", "finish", "startBearing", "startSpeed", "start_longitude",
#'   "start_latitude", "finish_longitude", "finish_latitude", "crow_fly_distance",
#'   "event", "whence", "speed", "itinerary", "clientRouteId", "plan",
#'   "note", "length", "time", "busynance", "quietness", "signalledJunctions",
#'   "signalledCrossings", "west", "south", "east", "north", "name",
#'   "walk", "leaving", "arriving", "coordinates", "elevations", "distances",
#'   "grammesCO2saved", "calories", "edition", "type", "legNumber",
#'   "distance", "flow", "turn", "color", "points", "provisionName"
#' )
#' ```
#'
#' @param obj Object from CycleStreets.net read-in with
#' @param cols Columns to be included in the result, a character vector or `NULL` for all available columns (see details for default)
#' @param cols_extra Additional columns to be added providing summaries of gradient and other variables
#' @inheritParams smooth_with_cutoffs
#' @inheritParams journey
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
#' obj = jsonlite::read_json(f, simplifyVector = TRUE)
#' rsf = json2sf_cs(obj)
#' rsf
#' json2sf_cs(obj, cols = c("distances"))
#' rsf2 = json2sf_cs(obj, cols = NULL, cols_extra = NULL)
#' json2sf_cs(obj, cols_extra = "gradient_median")
#' json2sf_cs(obj, cols = c("name", "distances"), cols_extra = "gradient_median")
#' names(rsf2)
#' # stplanr::line2points(rsf) extract start and end points
#' sf:::plot.sf(rsf)
#' json2sf_cs(obj, cols = c("time", "busynance", "elevations"))
#' json2sf_cs(obj, cols = c("distances"), smooth_gradient = TRUE,
#'   gradient_cutoff = 0.05, distance_cutoff = 50)
#' from_point = c(-8.80639, 52.50692)
#' to_point =   c(-8.80565, 52.51329)
#' # save result from the API call to journey.json
#' # res_json = journey(from_point, to_point, silent = FALSE, save_raw = TRUE)
#' # jsonlite::write_json(res_json, "inst/extdata/journey.json")
#' # f = "inst/extdata/journey.json"
#' f = system.file(package = "cyclestreets", "extdata/journey.json")
#' obj = jsonlite::read_json(f, simplifyVector = TRUE)
#' rsf = json2sf_cs(obj)
json2sf_cs = function(
    obj,
    cols = c("distances", "elevations"),
    cols_extra = c("gradient_segment", "quietness"),
    smooth_gradient = FALSE,
    distance_cutoff = 50,
    gradient_cutoff = 0.1,
    n = 3,
    warnNA = FALSE
    ) {
  # browser()
  att = obj$marker$`@attributes`
  coord_list = lapply(att$points[-1], txt2coords)
  elev_list = lapply(att$elevations[-1], txt2elevations)
  elev_diff_list = lapply(elev_list, function(x) diff(stats::lag(x, 1)))
  dist_list = lapply(att$distances[-1], function(x){
    distances = txt2elevations(x)
    distances[-1]
  })
  # dist_list = lapply(coord_list, function(x) {
  #   geodist::geodist(data.frame(x = x[, 1], y = x[, 2]), sequential = TRUE)
  # })
  # route_distances = as.numeric(att$distance[-1])

  # cor(dist_sums, dist_list2) # 99.99%

  glst = mapply(function(x, y) x / y, elev_diff_list, dist_list)
  rsfl = do.call(c, lapply(coord_list, sfheaders::sfc_linestring))
  n_segs = length(rsfl)
  # att[cols]
  # Remove variables not needed:
  cols_all = c(cols, cols_extra)
  att = att[names(att) %in% c(cols, cols_extra)]
  cols_na = sapply(att, function(x) sum(is.na(x)))
  sel_constant = cols_na == n_segs & names(cols_na) != "coordinates"
  if(any(sel_constant)) {
    cols_constant = names(cols_na)[sel_constant]
    vals_constant = lapply(cols_constant, function(x) att[[x]][1])
    names(vals_constant) = cols_constant
    suppressWarnings({
      vals_numeric = lapply(vals_constant, as.numeric)
    })
    sel_numeric = !is.na(vals_numeric)
    vals_constant[sel_numeric] = vals_numeric[sel_numeric]
    # Potential bottleneck:
    d_constant = data.frame(vals_constant)[rep(1, n_segs),]
  } else {
    d_constant = NULL
  }
  # useful cols: busynance, name, elevations, distances, turn,provisionName
  sel_variable = cols_na == 0 & !grepl("startB|type|dist|elev", names(cols_na))
  cols_variable = names(cols_na)[sel_variable]
  # Empty list if no matching cols:
  vv = lapply(cols_variable, function(x) att[[x]][-1])
  names(vv) = cols_variable
  # TODO: explore conditional calculations here:
  # if(any(grepl(pattern = "elev", x = cols_extra))) {
  #
  # }
  # vv$elevation_mean = unlist(lapply(elev_list, mean))
  # vv$elevation_start = unlist(lapply(elev_list, head, n = 1))
  # vv$elevation_end = unlist(lapply(elev_list, tail, n = 1))
  elevation_max = unlist(lapply(elev_list, max))
  elevation_min = unlist(lapply(elev_list, min))
  if(any(grepl(pattern = "route_distance", x = cols_all))) {
    vv$route_distance = as.numeric(obj$marker$`@attributes`$length[1])
  }
  if(n_segs == 1) {
    if(any(grepl(pattern = "gradient_mean", x = cols_extra))) {
      vv$gradient_mean = mean(abs(glst))
    }
    if(any(grepl(pattern = "gradient_median", x = cols_extra))) {
      vv$gradient_median = stats::median(abs(glst))
    }
    if(any(grepl(pattern = "gradient_p75", x = cols_extra))) {
      vv$gradient_p75 = stats::quantile(abs(glst), probs = 0.75)
    }
    if(any(grepl(pattern = "gradient_mean", x = cols_extra))) {
      vv$gradient_mean = max(abs(glst))
    }
  } else {
    if(any(grepl(pattern = "gradient_mean", x = cols_extra))) {
      vv$gradient_mean = sapply(glst, function(x) mean(abs(x)))
    }
    if(any(grepl(pattern = "gradient_median", x = cols_extra))) {
      vv$gradient_median = sapply(glst, function(x) stats::median(abs(x)))
    }
    if(any(grepl(pattern = "gradient_p75", x = cols_extra))) {
      vv$gradient_p75 = sapply(glst, function(x) stats::quantile(abs(x), probs = 0.75))
    }
    if(any(grepl(pattern = "gradient_max", x = cols_extra))) {
      vv$gradient_max = sapply(glst, function(x) max(abs(x)))
    }
  }
  vv$distances = sapply(dist_list, sum)
  suppressWarnings({
    vals_vnumeric = lapply(vv, as.numeric)
  })
  # Add segment name if available:
  vals_vnumeric$name = vv$name
  dv = data.frame(vals_vnumeric)
  # manually add records
  dv$gradient_segment = (elevation_max - elevation_min) / vv$distances
  dv$elevation_change = (elevation_max - elevation_min)
  dv$provisionName = att$provisionName[-1]
  if (!is.null(cols_extra)) {
    cols_extra_variable = c(cols, cols_extra)[c(cols, cols_extra) %in% names(dv)]
    dv = dv[cols_extra_variable]
  }
  if(is(d_constant, "data.frame")) {
    d_all = cbind(dv, d_constant)
  } else {
    d_all = dv
  }
  r = sf::st_sf(d_all, geometry = rsfl)
  if (smooth_gradient) {
    if(n_segs > 1) {
      r$gradient_smooth = smooth_with_cutoffs(
        r$gradient_segment,
        r$elevation_change,
        r$distances,
        distance_cutoff,
        gradient_cutoff,
        n,
        warnNA = warnNA
      )
    } else {
      r$gradient_smooth = r$gradient_segment
    }
  }
  return(r)
}

#' Identify and smooth-out anomalous gradient values
#'
#' When `distance_cutoff` and `gradient_cutoff` thresholds are both broken
#' for route segments, this function treats them as anomalous and
#' sets the offending gradient values to the mean of the `n`
#' segments closest to (in front of and behind) the offending segment.
#'
#' @param gradient_segment The gradient for each segment from CycleStreets.net
#' @param elevation_change The difference between the maximum and minimum elevations within each segment
#' @param distances The distance of each segment
#' @param distance_cutoff Distance (m) used to identify anomalous gradients
#' @param gradient_cutoff Gradient (%, e.g. 0.1 being 10%) used to identify anomalous gradients
#' @param n The number of segments to use to smooth anomalous gradents.
#' @param warnNA Logical should NA warning be given?
#' The default is 3, meaning segments directly before, after and including the offending segment.
#' @export
#' @examples
#' f = system.file(package = "cyclestreets", "extdata/journey.json")
#' obj = jsonlite::read_json(f, simplifyVector = TRUE)
#' rsf = json2sf_cs(obj, cols = c("distances"))
#' rsf$gradient_segment
#' rsf$elevation_change
#' rsf$distances
#' smooth_with_cutoffs(rsf$gradient_segment, rsf$elevation_change, rsf$distances)
#' smooth_with_cutoffs(rsf$gradient_segment, rsf$elevation_change, rsf$distances, 20, 0.05)
#' smooth_with_cutoffs(rsf$gradient_segment, rsf$elevation_change, rsf$distances, 200, 0.02)
#' smooth_with_cutoffs(rsf$gradient_segment, rsf$elevation_change, rsf$distances, 200, 0.02, n = 5)
smooth_with_cutoffs = function(gradient_segment,
                               elevation_change,
                               distances,
                               distance_cutoff = 50,
                               gradient_cutoff = 0.1,
                               n = 3,
                               warnNA = FALSE) {
  sel = gradient_segment > gradient_cutoff &
    distances <= distance_cutoff
  gradient_segment_smooth =
    route_rolling_average(elevation_change, n = n) /
    route_rolling_average(distances, n = n)

  gradient_segment[sel] = gradient_segment_smooth[sel]

  if (any(is.na(gradient_segment))) {
    if(warnNA){
      message("NA values detected")
    }
    gradient_segment[is.na(gradient_segment)] =
      mean(gradient_segment, na.rm = TRUE)
  }
  gradient_segment
}

