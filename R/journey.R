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
#' By default it uses the CYCLESTREET environment variable.
#' This can be set with `usethis::edit_r_environ()`.
#'
#' @param from Longitude/Latitude pair, e.g. `c(-1.55, 53.80)`
#' @param to Longitude/Latitude pair, e.g. `c(-1.55, 53.80)`
#'
#' @param plan Text strong of either "fastest" (default), "quietest" or "balanced"
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#' @param pat The API key used. By default this uses `Sys.getenv("CYCLESTREET")`.
#' @param base_url The base url from which to construct API requests
#' (with default set to main server)
#' @param reporterrors Boolean value (TRUE/FALSE) indicating if cyclestreets (TRUE by default).
#' should report errors (FALSE by default).
#' @param save_raw Boolean value which returns raw list from the json if TRUE (FALSE by default).
#' @export
#' @examples
#' \dontrun{
#' from = c(-1.55, 53.80) # geo_code("leeds")
#' to = c(-1.76, 53.80) # geo_code("bradford uk")
#' r1 = journey(from, to)
#' sf:::plot.sf(r1)
#' to = c(-2, 53.5)
#' r1 = journey(from, to)
#' r2 = journey(from, to, plan = "balanced")
#' plot(r1["busynance"], reset = FALSE)
#' plot(r2["busynance"], add = TRUE)
#' r3 = journey(from, to, silent = FALSE)
#' r4 = journey(from, to, save_raw = TRUE)
#' }
journey <- function(from, to, plan = "fastest", silent = TRUE,
                    pat = Sys.getenv("CYCLESTREET"),
                    base_url = "https://www.cyclestreets.net",
                    reporterrors = TRUE,
                    save_raw = "FALSE") {

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
#' obj = jsonlite::read_json(f, simplifyVector = TRUE)
#' rsf = json2sf_cs(obj)
#' sf:::plot.sf(rsf)
json2sf_cs <- function(obj) {
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

  rsf = sf::st_sf(d, geometry = rsfl, crs = 4326)

  return(rsf)

}
