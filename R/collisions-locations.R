#' Download collisions data from stats19
#'
#' @param bbox either a bounding box in the form w,s,e,n or an exact boundary as csv string of longitude/latitudes:
#' @param casualtiesinclude a text string specifying the type of casualty
#' (as of May 2018 only `cyclist` is supported)
#' @param field string/number specifying the variables returned (all returned by default)
#' @param limit an integer specifying the number of casualties returned
#' default 400, max 2000
#' @param jitter 1|0, default 0
#' @param format string, default geojson
#' @param datetime string  specifying the output format of the date with the default of
#' sqldatetime (i.e. simplified ISO 8601 format): YYYY-MM-DD HH:MM:SS
#' @export
#' @examples
#' \dontrun{
#' # example of from https://www.cyclestreets.net/api/v2/collisions.locations/
#' }
collisions <- function(bbox, casualtiesinclude, field, limit = 400, jitter = 0, format = "geojson", datetime = "sqldatetime") {
  # ... wip ...
}
