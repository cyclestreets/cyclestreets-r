#' Batch routing for multiple plans and large datasets
#'
#' @param desire_lines Input desire lines
#' @param plans Plans, e.g. fastest
#' @param nrow_batch How many rows per batch?
#' @param ... Arguments passed to batch
#'
#' @return
#' A list of routes.
#'
#' @export
#'
#' @examples
#' if(FALSE) {
#' od_df = readr::read_csv("https://github.com/nptscot/npt/raw/main/data-raw/od_subset.csv")
#' zones = sf::read_sf("https://github.com/nptscot/npt/raw/main/data-raw/zones_edinburgh.geojson")
#' desire_lines = od::od_to_sf(od_df, zones)
#' desire_lines = desire_lines[1:100, ]
#' routes_multi = batch_multi(desire_lines, plans = c("fastest", "quietest"), nrow_batch = 26, delete_job = FALSE)
#' names(routes_multi)
#' plot(routes_multi$fastest$geometry)
#' plot(routes_multi$quietest$geometry)
#' ids = list(
#'   fastest = 4059:(4059+3),
#'   quietest = 4063:(4063+3)
#' )
#' r_ids = batch_multi(desire_lines, plans = c("fastest", "quietest"), nrow_batch = 26, delete_job = FALSE, batch_ids = ids)
#' }
batch_multi = function(desire_lines,
                       plans = c("fastest", "balanced"),
                       nrow_batch = 10000,
                       temp_folder = tempdir(),
                       batch_ids = NULL,
                       ...) {
  library(sf)
  nrow_od = nrow(desire_lines)
  # Break od dataset into chunks:
  desire_lines$splittingID = ceiling(seq_len(nrow(desire_lines)) / nrow_batch)
  n_batches = length(unique(desire_lines$splittingID))
  max_pad = nchar(as.character(n_batches))
  desire_lines$splittingID = stringr::str_pad(desire_lines$splittingID, width = max_pad, pad = "0")
  results_id = results = list()
  i = 1
  # TODO: generalise:
  # plan = "fastest"
  routes = as.list(plans)
  names(routes) = plans
  # Create placeholder ids object:
  for (plan in plans) {
    message("Getting the ", plan, " routes")
    if (is.null(batch_ids[[plan]])) {
      for (i in seq_len(n_batches)) {
        id = stringr::str_pad(i, max_pad, pad = "0")
        rows_to_route = which(desire_lines$splittingID == id)
        od_to_route = desire_lines[rows_to_route,]
        range_text = paste(range(rows_to_route), collapse = ":")
        f = paste0("batch_",
                   plan,
                   "_",
                   id,
                   "_",
                   range_text,
                   "_",
                   nrow_od,
                   "_.Rds")
        f = file.path(temp_folder, f)
        message(Sys.time(), " sending batch ", id, " of ", n_batches)
        message("Number of rows in batch: ", nrow(od_to_route))
        if (file.exists(f)) {
          # message("File exists")
          # results[[i]] = readRDS(f)
          next
        } else {
          # message("File does not exist")
          message("Sending batch for routing: ", f)
          results_id[[i]] = cyclestreets::batch(
            desire_lines = od_to_route,
            id = NULL,
            strategies = plan,
            wait = FALSE,
            ...
          )
        }
      }
      # batch_ids were NULL
      batch_ids[[plan]] = results_id
    }
  }
  for (plan in plans) {
    message("Getting the routes sent to CycleStreets with the following ids:")
    message(paste(unlist(batch_ids[[plan]]), collapse = ", "))
    for (i in seq_len(n_batches)) {
      id = stringr::str_pad(i, max_pad, pad = "0")
      rows_to_route = which(desire_lines$splittingID == id)
      od_to_route = desire_lines[rows_to_route,]
      range_text = paste(range(rows_to_route), collapse = ":")
      f = paste0("batch_",
                 plan,
                 "_",
                 id,
                 "_",
                 range_text,
                 "_",
                 nrow_od,
                 ".Rds")
      f = file.path(temp_folder, f)
      message(Sys.time(), " getting batch ", id, " of ", n_batches)
      message("Number of rows in batch: ", nrow(od_to_route))
      # message("Looking in the file: ", f)
      if (file.exists(f)) {
        message("File exists")
        results[[i]] = readRDS(f)
      } else {
        # message("File does not exist")
        message("Getting file from CycleStreets")
        results[[i]] = cyclestreets::batch(
          desire_lines = od_to_route,
          id = batch_ids[[plan]][[i]],
          strategies = plan,
          wait = TRUE,
          ...
        )
      }
      message("Saving ", f, " to ", temp_folder)
      saveRDS(od_to_route, f)
    }
    message("Combining results")
    saveRDS(results, file.path(temp_folder, "results_list.Rds"))
    result = bind_sf(results)
    routes[[plan]] = result
  }
  routes
}

bind_sf = function(x) {
  if (length(x) == 0)
    stop("Empty list")
  geom_name = attr(x[[1]], "sf_column")
  x = data.table::rbindlist(x, use.names = FALSE)
  # x = collapse::unlist2d(x, idcols = FALSE, recursive = FALSE)
  x[[geom_name]] = sf::st_sfc(x[[geom_name]], recompute_bbox = TRUE)
  x = sf::st_as_sf(x)
  x
}
