#' Batch read v2
#'
#' Replaces batch_read from batch.R
#' @param file File path to csv
#' @param segments Logical if true returns segments else returns routes
#' @noRd


batch_read = function(
    file,
    segments = TRUE,
    cols_to_keep = c(
      "name", # not used currently but could be handy
      "distances",
      "gradient_smooth",
      "quietness"
    )
    ) {
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

  res_df = json2sf_cs(results_raw = res$json,
                       id = res$route_number,
                       segments = segments,
                      cols_to_keep = cols_to_keep
                      )

  #Character to numeric
  nms = c("time","busynance","quietness","signalledJunctions","signalledCrossings",
          "walk","distances","legNumber","distance","flow","startBearing",
          "start_longitude","start_latitude","finish_longitude","finish_latitude",
          "crow_fly_distance","speed","itinerary","length","west","south","east",
          "north","grammesCO2saved","calories")



  if(is.character(segments)){
    if(segments != "both"){
      stop("Unknown segments")
    }

    for(i in seq(length(nms))){
      if(nms[i] %in% names(res_df$routes)){
        res_df$routes[[nms[i]]] = as.numeric(res_df$routes[[nms[i]]])
      }
    }
    names(res_df$routes)[names(res_df$routes) == "id"] = "route_number"

    for(i in seq(length(nms))){
      if(nms[i] %in% names(res_df$segments)){
        res_df$segments[[nms[i]]] = as.numeric(res_df$segments[[nms[i]]])
      }
    }
    names(res_df$segments)[names(res_df$segments) == "id"] = "route_number"

  } else {

    for(i in seq(length(nms))){
      if(nms[i] %in% names(res_df)){
        res_df[[nms[i]]] = as.numeric(res_df[[nms[i]]])
      }
    }

    names(res_df)[names(res_df) == "id"] = "route_number"


  }

  res_df

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
