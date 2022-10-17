#' Download data on 'Ways' with cyclability (quietness) ratings
#'
#' R interface to the CycleStreets.net LTN.
#' See [API docs](https://www.cyclestreets.net/api/v2/).
#'
#' @param bb An sf or 'bounding box' like object
#' @inheritParams journey
#' @export
#' @examples
#' \dontrun{
#'
#' u_test = paste0("https://api.cyclestreets.net/v2/mapdata?key=c047ed46f7b50b18",
#'   "&limit=400&types=way&wayFields=name,ridingSurface,id,cyclableText,",
#'   "quietness,speedMph,speedKmph,pause,color&zoom=16&",
#'   "bbox=-9.160863,38.754642,-9.150128,38.75764")
#' ways_test = sf::read_sf(u_test)
#' bb <- "0.101131,52.195807,0.170288,52.209719"
#' bb <- "-9.160863,38.754642,-9.150128,38.75764"
#' way_data <- ways(bb)
#' plot(way_data)
#' bb <- stplanr::routes_fast_sf
#' way_data <- ways(bb)
#' plot(way_data)
#' }
ways <- function(
    bb,
    # pat = Sys.getenv("CYCLESTREETS"),
    pat = "c047ed46f7b50b18",
    base_url = "https://api.cyclestreets.net/v2/mapdata?",
    limit = 400,
    types = "way",
    wayFields = "name,ridingSurface,id,cyclableText,quietness,speedMph,speedKmph,pause,color",
    zoom = 16
    ) {
  if(any(grepl(pattern = "sf", class(bb)))) {
    bb <- bb_to_character(bb)
  }
  u <- paste0(base_url,
             "key=", pat,
             "&limit=", limit,
             "&types=", types,
             "&wayFields=", wayFields,
             "&zoom=", zoom,
             "&bbox=", bb)
  # browser()
  # browseURL(u)
  sf::read_sf(u)
}

