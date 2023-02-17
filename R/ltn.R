#' Download data on 'Low Traffic Neighbourhoods' or 'rat runs' from CycleStreets
#'
#' R interface to the CycleStreets.net LTN.
#' See [ltn API docs](https://www.cyclestreets.net/api/v2/advocacydata.ltns/)
#' and an article on the methods for further details:
#' https://www.cyclestreets.org/news/2021/07/25/mapping-ltns/
#'
#' @param bb An sf or 'bounding box' like object
#' @inheritParams journey
#' @export
#' @examples
#' \dontrun{
#' bb = "0.101131,52.195807,0.170288,52.209719"
#' ltn_data = ltns(bb)
#' plot(ltn_data)
#' bb = stplanr::routes_fast_sf
#' ltn_data = ltns(bb)
#' plot(ltn_data)
#' }
ltns = function(bb, pat = Sys.getenv("CYCLESTREETS")) {
  if(any(grepl(pattern = "sf", class(bb)))) {
    bb = bb_to_character(bb)
  }
  u = paste0("https://api.cyclestreets.net/v2/advocacydata.ltns?key=",
             Sys.getenv("CYCLESTREETS"),
             "&bbox=",
             bb)
  # browseURL(u)
  sf::read_sf(u)
}

bb_to_character = function(bb) {
  bb = sf::st_bbox(bb)
  paste0(bb, collapse = ",")
}
