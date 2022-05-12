#' Interface to CycleStreets Batch Routing API
#'
#' @param desire_lines Geographic desire lines representing origin-destination data
#' @param name The name of the batch routing job for CycleStreets
#' @param serverId The server ID to use (21 by default)
#' @param strategies Route plan types, e.g. `"fastest"`
#' @param minDistance Min distance
#' @param maxDistance Max distance
#' @param bothDirections int (1|0)
#'   Whether to plan in both directions, i.e. A-B as well as B-A.
#' @param filename Character string
#' @param includeJsonOutput int (1|0)
#'   Whether to include a column in the resulting CSV data giving the full JSON output from the API, rather than just summary
#'   information like distance and time.
#' @param emailOnCompletion Email on completion?
#' @param username string
#'   Your CycleStreets account username. In due course this will be replaced with an OAuth token.
#' @param password string
#'   Your CycleStreets account password.
#' @param id int
#'   Batch job ID, as returned from batchroutes.createjob.
#'   action string (start|pause|continue|terminate)
#'   Action to take. Available actions are:
#'     start: Start (open) job
#'   pause: Pause job
#'   continue: Continue (re-open) job
#'   terminate: Terminate job and delete data
#' @inheritParams journey
#' @export
batchroutes = function(
    desire_lines,
    name = "test batch",
    serverId = 21,
    strategies = "quietest",
    minDistance = 50,
    maxDistance = 5000,
    filename = "test",
    includeJsonOutput = 1,
    emailOnCompletion = "you@example.com",
    username = "yourname",
    password = Sys.getenv("CYCLESTREETS_PW"),
    base_url = "https://api.cyclestreets.net/v2/batchroutes.createjob"
    ) {
  batch_url = paste0(base_url, "?key=", Sys.getenv("CYCLESTREETS"))
  body = list(
    name = "Journey matrix for Cambridge",
    serverId = 21,
    geometry = '{"type": "FeatureCollection", "features": [
      {"type": "Feature", "id": 1, "properties": {}, "geometry": {"type": "Point", "coordinates": [0.14187, 52.20303]}},
      {"type": "Feature", "id": "a", "properties": {}, "geometry": {"type": "Point", "coordinates": [0.14711, 52.20061]}},
      {"type": "Feature", "id": 56, "properties": {}, "geometry": {"type": "Point", "coordinates": [0.11638, 52.20360]}}
    ]}',
    strategies = "fastest,quietest",
    bothDirections = 1,
    minDistance = 50,
    maxDistance = 5000,
    filename = "cambridge",
    includeJsonOutput = 1,
    emailOnCompletion = "webmaster@example.com",
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  httr::POST(url = batch_url, body = body)
  # return_url = ""
}

batchcontrol = function(base_url = "https://api.cyclestreets.net/v2/batchroutes.controljob") {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", Sys.getenv("CYCLESTREETS"))
  body = list(
    id = 196,
    action = "start",
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  httr::POST(url = batch_url, body = body)
}

# POST https://api.cyclestreets.net/v2/batchroutes.jobdata?key=...
# (
#   [id] => 69158
#   [username] => myusername
#   [password] => mypassword
# )

batchroutes = function(base_url = "https://api.cyclestreets.net/v2/batchroutes.jobdata", poll_interval = 60) {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", Sys.getenv("CYCLESTREETS"))
  body = list(
    id = 196,
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  # TODO add polling
  message("Sending data, wait...")
  res = httr::POST(url = batch_url, body = body)
  res_json = httr::content(res, "parsed")
  if(res_json$ready) {
    message("Congrats, you data is ready 🎉")
    data_res = httr::GET(url = res_json$files$dataCsv)
  }
}

batch_read = function(file = "cambridge-data.csv.gz") {
  R.utils::gunzip("cambridge-data.csv.gz")
  res = readr::read_csv("cambridge-data.csv")
  jsonlite::parse_json(res$json[1])
}
