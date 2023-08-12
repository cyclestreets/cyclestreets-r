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
#' @param ... Arguments passed to json2sf_cs
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
                   ...
                   ) {
  if (is.null(pat)) pat = Sys.getenv("CYCLESTREETS")
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
  if(save_raw) {
    return(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE, bigint_as_char = FALSE))
  }
  res = json2sf_cs(txt, id = 1)
  res
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
#' rsf = json2sf_cs(readLines(f))
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

