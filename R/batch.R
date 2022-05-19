#' Interface to CycleStreets Batch Routing API
#'
#' @param desire_lines Geographic desire lines representing origin-destination data
#' @param name The name of the batch routing job for CycleStreets
#' @param directory Where to save the data? `tempdir()` by default
#' @param wait_time How long to wait before getting the data? 30 seconds by default.
#' @param serverId The server ID to use (21 by default)
#' @param strategies Route plan types, e.g. `"fastest"`
#' @param minDistance Min Euclidean distance of routes to be calculated
#' @param maxDistance Maximum Euclidean distance of routes to be calculated
#' @param bothDirections int (1|0)
#'   Whether to plan in both directions, i.e. A-B as well as B-A.
#'   0, meaning only one way routes, is the default in the R default.
#' @param filename Character string
#' @param includeJsonOutput int (1|0)
#'   Whether to include a column in the resulting CSV data giving the full JSON output from the API, rather than just summary
#'   information like distance and time.
#' @param emailOnCompletion Email on completion?
#' @param username string
#'   Your CycleStreets account username. In due course this will be replaced with an OAuth token.
#' @param password string
#'   Your CycleStreets account password. You can set it with
#'   Sys.setenv(CYCLESTREETS_PW="xxxxxx")
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
#' @examples
#' if(FALSE) {
#' library(sf)
#' desire_lines = od::od_to_sf(od::od_data_df, od::od_data_zones)[4:5, 1:3]
#' desire_lines$id = 1:nrow(desire_lines)
#' routes = batch(desire_lines, username = "robinlovelace")
#' plot(routes$geometry)
#' plot(desire_lines$geometry, add = TRUE, col = "red")
#' routes = batch(desire_lines, username = "robinlovelace", wait_time = 1)
#' # try again with ID provided:
#' # routes = batch(desire_lines, username = "robinlovelace", id = routes)
#' # Compare with stplanr's route() approach:
#'
#' }
batch = function(
    desire_lines,
    directory = tempdir(),
    wait_time = 30,
    name = "test batch",
    serverId = 21,
    strategies = "quietest",
    bothDirections = 0,
    minDistance = 50,
    maxDistance = 5000,
    filename = "test",
    includeJsonOutput = 1,
    emailOnCompletion = "you@example.com",
    username = "yourname",
    password = Sys.getenv("CYCLESTREETS_PW"),
    base_url = "https://api.cyclestreets.net/v2/batchroutes.createjob",
    id = NULL
) {
  if(is.null(desire_lines$id)) {
    desire_lines$id = 1:nrow(desire_lines)
  }
  if(is.null(id)) {
    id = batch_routes(
      desire_lines,
      name,
      serverId,
      strategies,
      bothDirections,
      minDistance,
      maxDistance,
      filename,
      includeJsonOutput,
      emailOnCompletion,
      username,
      password,
      base_url,
      id
    )
    message("Wating to request the data for ", wait_time, " seconds.")
    Sys.sleep(time = wait_time)
  }
  res_joburls = batch_jobdata(
    username = username,
    password = password,
    id = id
  )
  if(is.null(res_joburls)) {
    message("No data returned yet. Try again later with the following id: ", id)
    return(id)
  }
  filename_local = file.path(directory, paste0(filename, ".csv.gz"))
  if(file.exists(filename_local)) {
    message(filename, " already exists, overwriting it")
  }
  httr::GET(res_joburls$dataGz, httr::write_disk(filename_local))
  routes = batch_read(filename_local)
  nrows = table(routes$id)
  df = sf::st_drop_geometry(routes)
  inds = rep(seq(nrow(desire_lines)), times = nrows)
  df_routes_expanded = sf::st_drop_geometry(desire_lines)[inds, ]
  df = cbind(df_routes_expanded, df[-ncol(df)])
  routes_updated = sf::st_sf(df, geometry = routes$geometry)
  routes_updated
}

batch_routes = function(
    desire_lines,
    name = "test batch",
    serverId = 21,
    strategies = "quietest",
    bothDirections = 1,
    minDistance = 50,
    maxDistance = 5000,
    filename = "test",
    includeJsonOutput = 1,
    emailOnCompletion = "you@example.com",
    username = "yourname",
    password = Sys.getenv("CYCLESTREETS_PW"),
    base_url = "https://api.cyclestreets.net/v2/batchroutes.createjob",
    id = 1
) {
  batch_url = paste0(base_url, "?key=", Sys.getenv("CYCLESTREETS"))
  body = list(
    name = name,
    serverId = serverId,
    geometry = geojsonsf::sf_geojson(desire_lines),
    strategies = strategies,
    bothDirections = bothDirections,
    minDistance = minDistance,
    maxDistance = maxDistance,
    filename = filename,
    includeJsonOutput = includeJsonOutput,
    emailOnCompletion = emailOnCompletion,
    username = username,
    password = password
  )
  message("POSTing the request to create and start the job")
  res = httr::POST(url = batch_url, body = body)
  res_json = httr::content(res, "parsed")
  id = res_json$id
  message("Job id: ", id)
  id
}

batch_control = function(base_url = "https://api.cyclestreets.net/v2/batchroutes.controljob") {
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

batch_jobdata = function(
    base_url = "https://api.cyclestreets.net/v2/batchroutes.jobdata",
    username = "yourname",
    password = Sys.getenv("CYCLESTREETS_PW"),
    id
) {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", Sys.getenv("CYCLESTREETS"))
  body = list(
    id = id,
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  # TODO add polling
  message("Sending data, wait...")
  res = httr::POST(url = batch_url, body = body)
  res_json = httr::content(res, "parsed")
  if(!is.null(res_json$ready)) {
    if(res_json$ready) {
      message("Congrats, you data is ready")
      res_joburls = res_json$files
      return(res_joburls)
    } else {
      message("Routing not complete")
    }
  } else {
    message("Routing not complete")
  }
}

# Tests:
# u = "https://github.com/cyclestreets/cyclestreets-r/releases/download/v0.5.3/cambridge-data.csv.gz"
# file = basename(u)
# download.file(u, file)
# batch_read(file)
batch_read = function(file) {
  file_csv = gsub(pattern = ".gz", replacement = "", x = file)
  if(file.exists(file_csv)) {
    message(".csv File already exists. Removing it.")
    file.remove(file_csv)
  }
  R.utils::gunzip(file)
  # res = readr::read_csv(file_csv)
  message("Reading in the following file:\n", file_csv)
  res = utils::read.csv(file_csv)
  res$id = seq(nrow(res))
  res_list = lapply(res$json, function(x) jsonlite::parse_json(x, simplifyVector = TRUE))
  elev_df = purrr::map_dfr(res_list, .id = "id", .f = function(x) {
    l = lapply(x$marker$`@attributes`$elevations[-1], txt2elevations)
    l_mean = lapply(l, mean)
    data.frame(mean_elev = unlist(l_mean))
  })
  res_df = purrr::map_dfr(res_list, .f = json2sf_cs, cols = c(
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
    "clientRouteId",
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
    "elevation_change",
    "provisionName"
  ),
  smooth_gradient = TRUE,
  distance_cutoff = 50,
  gradient_cutoff = 0.1,
  n = 3)
  res_df$id = elev_df$id
  # plot(res_df["id"])
  res_df
}

