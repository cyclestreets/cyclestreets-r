#' Download data on 'Low Traffic Neighbourhoods' or 'rat runs' from CycleStreets
#'
#' R interface to the CycleStreets.net LTN.
#' See [ltn API docs](https://www.cyclestreets.net/api/v2/advocacydata.ltns/)
#' and an article on the methods for further details:
#' https://www.cyclestreets.org/news/2021/07/25/mapping-ltns/
#'
#' @details
#' CycleStreets.net does not yet work worldwide.
#' Requires the internet and a CycleStreets.net API key.
#'
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
#'
#' @param bb An sf or 'bounding box' like object
#' @inheritParams journey
#' @export
#' @examples
#' \dontrun{
#' bb <- "0.101131,52.195807,0.170288,52.209719"
#' ltn_data <- ltns(bb)
#' plot(ltn_data)
#' bb <- stplanr::routes_fast_sf
#' ltn_data <- ltns(bb)
#' plot(ltn_data)
#' }
ltns <- function(bb, pat = Sys.getenv("CYCLESTREETS")) {
  if(any(grepl(pattern = "sf", class(bb)))) {
    bb <- sf::st_bbox(bb)
    bb <- paste0(bb, collapse = ",")
  }
  u <- paste0("https://api.cyclestreets.net/v2/advocacydata.ltns?key=",
             Sys.getenv("CYCLESTREETS"),
             "&bbox=",
             bb)
  # browseURL(u)
  sf::read_sf(u)
}
