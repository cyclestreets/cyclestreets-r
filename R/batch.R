#' Interface to CycleStreets Batch Routing API
#'
#' Note: set `CYCLESTREETS_BATCH`, `CYCLESTREETS_PW` and `CYCLESTREETS_PW`
#' environment variables, e.g. with `usethis::edit_r_environ()`
#' before trying this.
#'
#' See https://www.cyclestreets.net/journey/batch/ for web UI.
#'
#' Recommneded max batch size: 300k routes
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
#' @param cols_to_keep Columns to return in output sf object
#' @param segments logical, return segments TRUE/FALSE/"both"
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
#' # Wait for some time, around a minute or 2
#' routes_wait = batch(id = routes_id, username = "robinlovelace", wait = TRUE, delete_job = FALSE)
#' names(routes_wait)
#' sapply(routes_wait, class)
#' plot(routes_wait)
#' plot(desire_lines$geometry[4])
#' plot(routes_wait$geometry[routes_wait$route_number == "4"], add = TRUE)
#' head(routes_wait$route_number)
#' unique(routes_wait$route_number)
#' # Job is deleted after this command:
#' routes_attrib = batch(desire_lines, id = routes_id, username = "robinlovelace", wait = TRUE)
#' names(routes_attrib)
#' unique(routes_attrib$route_number)
#' desire_lines_huge = desire_lines[sample(nrow(desire_lines), 250000, replace = TRUE), ]
#' routes_id = batch(desire_lines_huge, username = "robinlovelace", wait = FALSE)
#' names(routes)
#' plot(routes$geometry)
#' plot(desire_lines$geometry, add = TRUE, col = "red")
#' routes = batch(desire_lines, username = "robinlovelace", wait_time = 5, segments = FALSE)
#' segments = batch(desire_lines, username = "robinlovelace", wait_time = 5, segments = TRUE)
#' both = batch(desire_lines, username = "robinlovelace", wait_time = 5, segments = "both")
#' # profvis::profvis(batch_read("test-data.csv.gz"))
#' }
batch = function(
    desire_lines = NULL,
    id = NULL,
    directory = tempdir(),
    wait = FALSE,
    wait_time = NULL,
    name = "Batch job",
    serverId = 21,
    strategies = "quietest",
    bothDirections = 0,
    minDistance = 50,
    maxDistance = 5000,
    filename = "test",
    includeJsonOutput = 1,
    emailOnCompletion = "you@example.com",
    username = Sys.getenv("CYCLESTREETS_UN"),
    password = Sys.getenv("CYCLESTREETS_PW"),
    base_url = "https://api.cyclestreets.net/v2/batchroutes.createjob",
    pat = Sys.getenv("CYCLESTREETS_BATCH"),
    silent = TRUE,
    delete_job = TRUE,
    cols_to_keep = c("id", "name", "provisionName", "distances", "time", "quietness", "gradient_smooth"),
    segments = TRUE
) {

  sys_time = Sys.time()

  if(is.null(wait_time) && !is.null(desire_lines)) {
    wait_time = wait_s(n = nrow(desire_lines))
  }

  # Add id column required by cyclestreets:
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
        id = as.character(id),
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
    id = as.character(id),
    pat = pat,
    silent = silent
  )
  if(is.null(res_joburls)) {
    if(!wait) {
      return(id)
    }
    message("No data returned yet. Trying again id ", id, " every 10 seconds")
    n_tries = 0
    while(is.null(res_joburls)) {
      n_tries = n_tries + 1
      # message("Try ", n_tries, " at ", Sys.time())
      sys_time_taken = round(difftime(time1 = Sys.time(), time2 = sys_time, units = "secs") / 60)
      if(!silent) {
        message(sys_time_taken, " minutes taken, waiting another 10 s")
      }
      Sys.sleep(10)
      res_joburls = batch_jobdata(
        username = username,
        password = password,
        id = as.character(id),
        pat = pat,
        silent = n_tries > 1
      )
    }
  }
  routes_updated = get_routes(url = res_joburls$dataGz, desire_lines, filename,
                              directory, cols_to_keep = cols_to_keep, segments = segments)
  # if(wait && !is.null(desire_lines)) {
  #   time_taken_s = round(as.numeric(difftime(time1 = Sys.time(), time2 = sys_time, units = "secs")))
  #   rps = round(nrow(desire_lines) / time_taken_s, 1)
  #   message(nrow(desire_lines), " routes, ", time_taken_s, "s, ", rps, " routes/s")
  # }
  if(delete_job) {
    batch_deletejob(base_url, username, password, id = as.character(id), pat = pat, silent = silent)
  }
  routes_updated
}

get_routes = function(url, desire_lines = NULL, filename, directory,
                      cols_to_keep = c("id", "name", "provisionName", "distances", "time",
                                       "quietness", "gradient_smooth"),
                      segments = TRUE) {
  filename_local = file.path(directory, paste0(filename, ".csv.gz"))
  if(file.exists(filename_local)) {
    message(filename, " already exists, overwriting it")
  }
  httr::GET(url, httr::write_disk(filename_local, overwrite = TRUE))
  # R.utils::gzip(filename_local)
  # routes = batch_read(gsub(pattern = ".gz", replacement = "", filename_local))
  # list.files(tempdir())


  if(is.character(segments)){
    if(segments == "both"){
      routes_segs = batch_read(filename_local, cols_to_keep = cols_to_keep, segments = segments)
      routes = routes_segs$routes
      segs = routes_segs$segments
      rm(routes_segs)

      if(!is.null(desire_lines)){
        message("When using segments = both, only the routes are joined to desire lines")
      }
    } else {
      stop("Unknown segments value")
    }
  } else {
    routes = batch_read(filename_local, cols_to_keep = cols_to_keep, segments = segments)
  }



  if(is.null(desire_lines)) {
    if(is.character(segments)){
      return(list(routes = routes, segments = segments))
    } else {
      return(routes)
    }
  }
  # If there are desire lines:
  routes_id_table = table(routes$route_number)
  routes_id_names = sort(as.numeric(names(routes_id_table)))

  desire_lines$id = as.character(seq(nrow(desire_lines)))
  desire_lines = sf::st_drop_geometry(desire_lines)
  n_routes_removed = nrow(desire_lines) - length(routes_id_names)
  message(n_routes_removed, " routes removed")
  routes_updated = dplyr::left_join(
    routes,
    desire_lines,
    by = dplyr::join_by(route_number == id)
  )

  if(is.character(segments)){
    return(list(routes = routes_updated, segments = segs))
  } else {
    return(routes_updated)
  }

}

batch_routes = function(
    desire_lines,
    name = "Batch job",
    serverId = 21,
    strategies = "quietest",
    bothDirections = 1,
    minDistance = 50,
    maxDistance = 5000,
    filename = "test",
    includeJsonOutput = 1,
    emailOnCompletion = "you@example.com",
    username = Sys.getenv("CYCLESTREETS_UN"),
    password = Sys.getenv("CYCLESTREETS_PW"),
    base_url = "https://api.cyclestreets.net/v2/batchroutes.createjob",
    id = 1,
    pat,
    silent = TRUE
) {
  batch_url = paste0(base_url, "?key=", pat)
  desire_lines_to_send = desire_lines["id"]
  desire_lines_to_send = sf::st_set_precision(desire_lines_to_send, precision = 10^5)
  # Reduce precision:
  f_tmp = file.path(tempdir(), "to_send.geojson")
  sf::write_sf(desire_lines_to_send, f_tmp, delete_dsn = TRUE)
  desire_lines_to_send = sf::read_sf(f_tmp)
  body = list(
    name = name,
    serverId = serverId,
    geometry = geojsonsf::sf_geojson(desire_lines_to_send),
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

  # # With httr:
  res = httr::POST(url = batch_url, body = body, httr::timeout(600))
  res_json = httr::content(res, "parsed")

  # # # With httr2:
  # req = httr2::request(batch_url)
  # # res_dry_run = req %>%
  # #   httr2::req_body_json(data = body) %>%
  # #   httr2::req_dry_run()
  # res = req %>%
  #   httr2::req_body_json(data = body) %>%
  #   httr2::req_perform()
  # res_json = httr2::resp_body_json(resp = res)

  if("error" %in% names(res_json)) {
    # TODO: should this be an error message on the R side?
    warning("Error message from server:\n", res_json$error)
  }

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
    username = Sys.getenv("CYCLESTREETS_UN"),
    password = Sys.getenv("CYCLESTREETS_PW"),
    id,
    pat,
    silent = TRUE
) {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", pat)
  body = list(
    id = as.character(id),
    username = username,
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  # TODO add polling
  if(!silent) message("Sending data, wait...")
  res = httr::POST(url = batch_url, body = body)
  res_json = httr::content(res, "parsed")
  error_message = paste0(" ", as.character(res_json$error))
  # Print message if silent = FALSE
  if(!silent) {
    if (error_message == " The job you requested to control is either non-existent or is owned by another user.") {
      message("No job with that ID. Try setting delete_job = FALSE")
      stop(error_message)
    }
    if(nchar(error_message)[1] > 2) {
      message("Error message detected from CycleStreets output")
      warning(res_json$error)
    }
    errors = paste0(" ", as.character(res_json$errors))
    if(nchar(errors[1]) > 1) {
      message("Routing errors for these routes:\n", paste(errors, collapse = "\n"))
      message(paste(names(errors), collapse = "\n"))
    }
  }
  if(!is.null(res_json$ready)) {
    if(res_json$ready) {
      message("Congrats, your data is ready")
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
    base_url,
    username = Sys.getenv("CYCLESTREETS_UN"),
    password = Sys.getenv("CYCLESTREETS_PW"),
    id,
    pat,
    silent = TRUE,
    serverId = 21
) {
  base_url = gsub(pattern = "createjob", replacement = "deletejob", x = base_url)
  batch_url = paste0(base_url, "?key=", pat)
  body = list(
    id = as.character(id),
    username = username,
    name = username,
    password = Sys.getenv("CYCLESTREETS_PW"),
    serverId = serverId
  )
  # TODO add polling
  if(!silent) message("Deleting the data")
  res = httr::POST(url = batch_url, body = body)
  res_json = httr::content(res, "parsed")
  message("Job ",paste0(res_json, collapse = ": "))
}

wait_s = function(n) {
  if(n < 2000) {
    w = 10 + n / 100
  }
  if(n >= 2000) {
    w = 30 + n / 1000
  }
  w
}
