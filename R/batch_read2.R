#' Batch read v2
#'
#' Replacemtn for batch_read
#' @param file File path to csv
#' @param segments Logical if true returns segments else returns routes
#' @noRd

batch_read2 = function(file, segments = TRUE) {
  message("Reading in the following file:\n", file)
  res = readr::read_csv(file, show_col_types = FALSE)
  res$route_number = seq(nrow(res))
  n_char = nchar(res$json)
  n_char[is.na(n_char)] = 0
  if(all(n_char == 0)) {
    stop("No routes returned: does CycleStreets operate where you requested data?")
  }
  min_nchar = min(n_char)
  if(min_nchar == 0) {
    which_min_ncar = which(n_char == 0)
    message("Removing NA routes: ", paste(which_min_ncar, collapse = " "))
    res = res[-which_min_ncar, ]
  }

  res_df = json2sf_cs2(res$json, id = res$route_number, segments = segments)
  res_df
}
