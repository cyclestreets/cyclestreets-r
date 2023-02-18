#' Interface to CycleStreets Batch Routing API
#'
#' Note: set `CYCLESTREETS_BATCH` and `CYCLESTREETS_PW`
#' environment variables, e.g. with `usethis::edit_r_environ()`
#' before trying this.
#'
#' @param desire_lines Geographic desire lines representing origin-destination data
#' @param name The name of the batch routing job for CycleStreets
#' @param directory Where to save the data? `tempdir()` by default
#' @param wait Should the process block your R session but return a route?
#'   FALSE by default.
#' @param wait_time How long to wait before getting the data in seconds?
#'   NULL by default, meaning it will be calculated by the private function
#'   `wait_s()`.
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
#' @param delete_job Delete the job? TRUE by default to avoid clogged servers
#' @inheritParams journey
#' @export
#' @examples
#' if(FALSE) {
#' library(sf)
#' desire_lines = od::od_to_sf(od::od_data_df, od::od_data_zones)[4:5, 1:3]
#' u = paste0("https://github.com/cyclestreets/cyclestreets-r/",
#'   "releases/download/v0.5.3/od-longford-10-test.Rds")
#' desire_lines = readRDS(url(u))
#' routes_id = batch(desire_lines, username = "robinlovelace", wait = FALSE)
#' desire_lines_huge = desire_lines[sample(nrow(desire_lines), 100000, replace = TRUE), ]
#' routes_id = batch(desire_lines_huge, username = "robinlovelace", wait = FALSE)
#' # Wait for some time, around a minute or 2
#' batch(id = routes_id, username = "robinlovelace")
#' names(routes)
#' plot(routes$geometry)
#' plot(desire_lines$geometry, add = TRUE, col = "red")
#' routes = batch(desire_lines, username = "robinlovelace", wait_time = 5)
#' }
batch = function(
    desire_lines = NULL,
    directory = tempdir(),
    wait = FALSE,
    wait_time = NULL,
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
    id = NULL,
    pat = Sys.getenv("CYCLESTREETS_BATCH"),
    silent = TRUE,
    delete_job = TRUE
) {
  sys_time = Sys.time()

  if(is.null(wait_time) && !is.null(desire_lines)) {
    wait_time = wait_s(n = nrow(desire_lines))
  }

  if(!is.null(desire_lines)) {
    if(! "id" %in% names(desire_lines)) {
      desire_lines$id = seq(nrow(desire_lines))
    }
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
      id,
      pat,
      silent = silent
    )
    if(is.null(id)) {
      stop("Check your credentials, try again, and maybe contact CycleStreets")
    }
    if(!wait) {
      res_joburls = batch_jobdata(
        username = username,
        password = password,
        id = id,
        pat = pat,
        silent = silent
      )
      if(is.null(res_joburls)) {
        message("Routing job sent, check back in around ", round(wait_time / 60), " minutes if you've just sent this")
        message("Check at www.cyclestreets.net/journey/batch/ for route id: ", id)
        return(id)
      }
    }
    if(!silent) {
      message("Waiting to request the data for ", wait_time, " seconds.")
    }
    Sys.sleep(time = wait_time)
  }
  res_joburls = batch_jobdata(
    username = username,
    password = password,
    id = id,
    pat = pat,
    silent = silent
  )
  if(is.null(res_joburls)) {
    if(!wait) {
      return(id)
    }
    message("No data returned yet. Trying again id ", id, " every 10 seconds")
    while(is.null(res_joburls)) {
      sys_time_taken = round(difftime(time1 = Sys.time(), time2 = sys_time, units = "secs") / 60)
      if(!silent) {
        message(sys_time_taken, " minutes taken, waiting another 10 s")
      }
      Sys.sleep(10)
      res_joburls = batch_jobdata(
        username = username,
        password = password,
        id = id,
        pat = pat
      )
    }
  }
  routes_updated = get_routes(res_joburls$dataGz, filename = filename, directory = directory)
  if(wait && !is.null(desire_lines)) {
    time_taken_s = round(as.numeric(difftime(time1 = Sys.time(), time2 = sys_time, units = "secs")))
    rps = round(nrow(desire_lines) / time_taken_s, 1)
    message(nrow(desire_lines), " routes, ", time_taken_s, "s, ", rps, " routes/s")
  }
  if(delete_job) {
    batch_deletejob(base_url, username, password, id = id, pat = pat, silent = silent)
  }
  routes_updated
}

get_routes = function(url, filename, directory) {
  filename_local = file.path(directory, paste0(filename, ".csv.gz"))
  if(file.exists(filename_local)) {
    message(filename, " already exists, overwriting it")
  }
  httr::GET(url, httr::write_disk(filename_local, overwrite = TRUE))
  routes = batch_read(filename_local)
  route_number = as.numeric(routes$id)
  if (!any(is.na(route_number))) {
    routes$id = route_number
  }
  routes_id_table = table(routes$id)
  routes_id_names = sort(as.numeric(names(routes_id_table)))
  n_routes_removed = nrow(desire_lines) - length(routes_id_names)
  desire_lines = desire_lines[routes_id_names, ]
  message(n_routes_removed, " routes removed")
  df = sf::st_drop_geometry(routes)
  inds = rep(seq(nrow(desire_lines)), times = as.numeric(routes_id_table))
  df_routes_expanded = sf::st_drop_geometry(desire_lines)[inds, ]
  df = cbind(df_routes_expanded, df[-1])
  routes_updated = sf::st_sf(df, geometry = routes$geometry)


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
    id = 1,
    pat,
    silent = TRUE
) {
  batch_url = paste0(base_url, "?key=", pat)
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
  if(!silent) {
    message("Posting to: ", batch_url)
  }
  browser()
  res = httr::POST(url = batch_url, body = body, httr::timeout(60))
  res_json = httr::content(res, "parsed")
  id = res_json$id
  message("Job id: ", id)
  id
}

batch_control = function(base_url = "https://api.cyclestreets.net/v2/batchroutes.controljob", pat, username) {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", pat)
  body = list(
    id = 196,
    action = "start",
    username = username,
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  httr::POST(url = batch_url, body = body)
}

batch_jobdata = function(
    base_url = "https://api.cyclestreets.net/v2/batchroutes.jobdata",
    username = "yourname",
    password = Sys.getenv("CYCLESTREETS_PW"),
    id,
    pat,
    silent = TRUE
) {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", pat)
  body = list(
    id = id,
    username = username,
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  # TODO add polling
  if(!silent) message("Sending data, wait...")
  res = httr::POST(url = batch_url, body = body)
  res_json = httr::content(res, "parsed")
  if(!is.null(res_json$ready)) {
    if(res_json$ready) {
      message("Congrats, you data is ready")
      res_joburls = res_json$files
      return(res_joburls)
    } else {
      if(!silent) message("Routing not complete")
    }
  } else {
    message("Routing not complete")
  }
}

batch_deletejob = function(
    base_url = "https://api.cyclestreets.net/v2/batchroutes.deletejob",
    username = "yourname",
    password = Sys.getenv("CYCLESTREETS_PW"),
    id,
    pat,
    silent = TRUE,
    serverId = 21
) {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", pat)
  body = list(
    id = id,
    username = username,
    name = username,
    password = Sys.getenv("CYCLESTREETS_PW"),
    serverId = serverId
  )
  # TODO add polling
  if(!silent) message("Deleting the data")
  res = httr::POST(url = batch_url, body = body)
  res_json = httr::content(res, "parsed")
  message(res_json)
}

# # # Tests:
# devtools::load_all()
# library(tidyverse)
# u = "https://github.com/cyclestreets/cyclestreets-r/releases/download/v0.5.3/cambridge-data.csv.gz"
# file = basename(u)
# download.file(u, file)
# res = batch_read(file)
# l_desire |>
#   slice(1:3) |>
#   mapview::mapview()
# res |>
#   filter(id %in% 1:3) |>
#   mapview::mapview()


batch_read = function(file) {
  # if(grepl(pattern = ".gz", x = file)) {
  #   file_csv = gsub(pattern = ".gz", replacement = "", x = file)
  #   if(file.exists(file_csv)) {
  #     message(".csv File already exists. Removing it.")
  #     file.remove(file_csv)
  #   }
  #   R.utils::gunzip(file)
  # } else {
    # file_csv = file
  # }
  # res = readr::read_csv(file_csv)
  # message("Reading in the following file:\n", file_csv)
  message("Reading in the following file:\n", file)
  res = readr::read_csv(file)
  res$id = seq(nrow(res))
  n_char = nchar(res$json)
  if(all(is.na(n_char))) {
    stop("No routes returned: does CycleStreets operate where you requested data?")
  }
  min_nchar = min(n_char)
  min_nchar[is.na(min_nchar)] = 0
  if(min_nchar == 0) {
    which_min_ncar = which(n_char == 0)
    warning("These contain no data: ", paste(which_min_ncar, collapse = " "))
    warning("Removing the failing desire lines")
    res = res[-which_min_ncar, ]
  }
  # Commented debugging code to identify the failing line:
  # try({
    res_list = lapply(res$json, function(x) {
  #     if(exists("i_line")) {
  #       i_line <<- i_line + 1
  #     } else {
  #       i_line <<- 1
  #     }
      # message("Line number ", i_line)
      jsonlite::parse_json(x, simplifyVector = TRUE)
    } )
  # })
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
  n = 3,
  .id = "id"
  )
  res_df
}

wait_s = function(n) {
  if(n < 2000) {
    w = 30 + n / 20
  }
  if(n >= 2000) {
    w = 30 + n / 30
  }
  w
}
