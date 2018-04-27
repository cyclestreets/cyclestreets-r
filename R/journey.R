#' Plan a journey with CycleStreets.net
#'
#' Provides an R interface to the CycleStreets.net cycle planning API,
#' a route planner made by cyclists for cyclists.
#' See \url{https://www.cyclestreets.net/api/}for more information.
#'
#' @param from Text string or coordinates (a numeric vector of
#'  `length = 2` representing latitude and longitude) representing a point
#'  on Earth.
#'
#' @param to Text string or coordinates (a numeric vector of
#'  `length = 2` representing latitude and longitude) representing a point
#'  on Earth. This represents the destination of the trip.
#'
#' @param plan Text strong of either "fastest" (default), "quietest" or "balanced"
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#' @param pat The API key used. By default this is set to NULL and
#' this is usually aquired automatically through a helper, api_pat().
#'
#' @details
#'
#' Requires an
#' internet connection, a CycleStreets.net API key
#' and origins and destinations within the UK (and various areas beyond) to run.
#'
#' Note that if \code{from} and \code{to} are supplied as
#' character strings (instead of lon/lat pairs), a
#' geo-coding services are used via \code{geo_code()}.
#'
#' You need to have an api key for this code to run.
#' By default it uses the CYCLESTREET environment variable.
#' This can be set with `usethis::edit_r_environ()`,
#' allowing the API key to be available in future
#' sessions.
#'
#' @param base_url The base url from which to construct API requests
#' (with default set to main server)
#' @param reporterrors Boolean value (TRUE/FALSE) indicating if cyclestreets (TRUE by default).
#' should report errors (FALSE by default).
#' @param save_raw Boolean value which returns raw list from the json if TRUE (FALSE by default).
#' @export
#' @seealso line2route
#' @aliases route_cyclestreets
#' @examples
#' \dontrun{
#' from = c(-1.55, 53.80) # geo_code("leeds")
#' to = c(-1.76, 53.80) # geo_code("bradford uk")
#' r = journey(from, to)
#' sf:::plot.sf(r)
#' }
journey <- function(from, to, plan = "fastest", silent = TRUE, pat = NULL,
                    base_url = "https://www.cyclestreets.net",
                    reporterrors = TRUE,
                    save_raw = "FALSE") {

  # Convert character strings to lon/lat if needs be
  if(is.character(from))
    from <- geo_code(from)
  if(is.character(to))
    to <- geo_code(to)

  orig <- paste0(from, collapse = ",")
  dest <- paste0(to, collapse = ",")
  ft_string <- paste(orig, dest, sep = "|")

  if(is.null(pat))
    pat = Sys.getenv("CYCLESTREET")

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
    r = json2sf_cs(obj)
  }
  r
}

txt2coords = function(txt) { # helper function to document...
  coords_split <- stringr::str_split(txt, pattern = " |,")[[1]]
  matrix(as.numeric(coords_split), ncol = 2, byrow = TRUE)
}
#' Convert output from CycleStreets.net into sf object
#'
#' @param obj Object from CycleStreets.net read-in with:
#' \code{jsonlite::read_json("inst/extdata/res_json.json", simplifyVector = T)}
#'
#' @export
#' @examples
#' from = "Leeds Rail Station"
#' to = "University of Leeds"
#' # res_json = stplanr::route_cyclestreet(from, to, silent = FALSE, save_raw = TRUE)
#' # jsonlite::write_json(res_json, "inst/extdata/res_json.json")
#' f = system.file(package = "cyclestreets", "extdata/res_json.json")
#' obj = jsonlite::read_json(f, simplifyVector = T)
#' rsf = json2sf_cs(obj)
#' sf:::plot.sf(rsf)
json2sf_cs <- function(obj, cols, default_crs) {
  coord_list = lapply(obj$marker$`@attributes`$points[-1], txt2coords)
  rsflx = lapply(coord_list, sf::st_linestring)
  rsfl = sf::st_sfc(rsflx)

  # variables
  # useful cols: busynance, name, elevations, distances, turn,provisionName

  b = sapply(obj$marker$`@attributes`$busynance[-1], as.numeric)
  n = obj$marker$`@attributes`$name[-1]
  e = stringr::str_split(obj$marker$`@attributes`$elevations[-1],pattern = ",") %>%
    lapply(as.numeric) %>%
    lapply(mean) %>%
    unlist()
  d = sapply(obj$marker$`@attributes`$distance[-1],as.numeric)
  t = obj$marker$`@attributes`$turn[-1]
  p = obj$marker$`@attributes`$provisionName[-1]

  d = data.frame(busynance = b, name = n, elevations = e, distance = d,
                 turn = t, pname = p)

  # todo: create more segment-level statistics (vectors) +
  # add them to the data frame (d) below
  if(!missing(cols)){
    d = d[,cols]
  }
  else {
    d
  }

  rsf = sf::st_sf(d, geometry = rsfl, crs = 4326)

  return(rsf)

}
