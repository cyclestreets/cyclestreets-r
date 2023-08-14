
f = system.file(package = "cyclestreets", "extdata/journey.json")
obj = jsonlite::read_json(f, simplifyVector = TRUE)
obj2 = RcppSimdJson::fload(f, max_simplify_lvl = "data_frame")
obj2 = RcppSimdJson::fload(f, max_simplify_lvl = "data_frame")
obj2 = jsonify::from_json(f, simplify = TRUE)
waldo::compare(obj, obj2)
obj$marker$`@attributes`
obj2$marker$`@attributes`

rsf = json2sf_cs(obj, cols = c("distances"))
sf:::plot.sf(rsf)

obj2$marker$`@attributes`$elevations
obj$marker$`@attributes`$elevations
# [1] "27,27,27,27,27,28,28,28,28,28,28,28,28,28,28,29,29,29,31,33,34,33,33,33,32,32,33,33,33,34,34,34,34,34,34,34,34,34,34"
# [2] "27,27,27,27,27,28"
# [3] "28,28,28,28,28,28,28,28,28,28,29,29,29,31,33,34,33,33,33"
# [4] "33,32,32"
# [5] "32,33,33,33,34,34,34,34,34,34,34,34,34"
# [6] "34,34"
e = obj$marker$`@attributes`$elevations
elev_list = lapply(obj$marker$`@attributes`$elevations[-1], txt2elevations)
stringr::str_split(e, pattern = ",") %>%
  lapply(as.numeric) %>%
  lapply(mean) %>%
  unlist()
stringr::str_split(e, pattern = ",") %>%
  lapply(as.numeric) %>%
  lapply(utils::head, 1) %>%
  unlist()

cs2num_mean = function(x) {
  unlist(lapply(x, mean))
}

cs2num_mean(elev_list)


json2sf_cs = function(obj,
                      cols = NULL,
                      cols_extra = c(
                        # "gradient_mean",
                        # "gradient_median",
                        # "gradient_p75",
                        # "gradient_max",
                        "elevation_start",
                        "elevation_end",
                        "gradient_segment",
                        "elevation_change",
                        "provisionName"
                      ),
                      smooth_gradient = FALSE,
                      distance_cutoff = 50,
                      gradient_cutoff = 0.1,
                      n = 3,
                      warnNA = FALSE) {

  coord_list = lapply(obj$marker$`@attributes`$points[-1], txt2coords)
  elev_list = lapply(obj$marker$`@attributes`$elevations[-1], txt2elevations)
  elev_diff_list = lapply(elev_list, function(x)
    diff(stats::lag(x, 1)))
  # dist_list1 = geodist::geodist(rbind(coord_list[[1]][1, ], coord_list[[1]][2, ]), sequential = TRUE)
  # dist_list2 = geodist::geodist(
  #   data.frame(x = coord_list[[1]][, 1], y = coord_list[[1]][, 2]),
  #   sequential = TRUE
  #   )
  dist_list = lapply(coord_list, function(x) {
    geodist::geodist(data.frame(x = x[, 1], y = x[, 2]),
                     sequential = TRUE)
  })
  # glst = purrr::map2(elev_diff_list, dist_list, ~.x / .y)
  glst = mapply(function(x, y)
    x / y, elev_diff_list, dist_list)
  rsfl = do.call(c, lapply(coord_list, sfheaders::sfc_linestring))
  # variables - constant
  n_segs = length(rsfl)
  cols_na = sapply(obj$marker$`@attributes`, function(x) sum(is.na(x)))
  sel_constant = cols_na == n_segs & names(cols_na) != "coordinates"
  cols_constant = names(cols_na)[sel_constant]
  vals_constant = lapply(cols_constant, function(x)
    obj$marker$`@attributes`[[x]][1])
  names(vals_constant) = cols_constant
  suppressWarnings({
    vals_numeric = lapply(vals_constant, as.numeric)
  })
  sel_numeric = !is.na(vals_numeric)
  vals_constant[sel_numeric] = vals_numeric[sel_numeric]
  d_constant = data.frame(vals_constant)[rep(1, n_segs),]

  # useful cols: busynance, name, elevations, distances, turn,provisionName

  sel_variable = cols_na == 0 &
    !grepl("startBearing|type", names(cols_na))
  cols_variable = names(cols_na)[sel_variable]
  vv = lapply(cols_variable, function(x)
    obj$marker$`@attributes`[[x]][-1])
  names(vv) = cols_variable
  # vv # take a look - which ones to process?

  vv$elevation_mean = unlist(lapply(elev_list, mean))
  vv$elevation_start = unlist(lapply(elev_list, head, n = 1))
  vv$elevation_end = unlist(lapply(elev_list, tail, n = 1))
  vv$elevation_max = unlist(lapply(elev_list, max))
  vv$elevation_min = unlist(lapply(elev_list, min))

  if(n_segs == 1) {
    vv$gradient_mean = mean(abs(glst))
    vv$gradient_median = stats::median(abs(glst))
    vv$gradient_p75 = stats::quantile(abs(glst), probs = 0.75)
    vv$gradient_max = max(abs(glst))
  } else {
    vv$gradient_mean = sapply(glst, function(x) mean(abs(x)))
    vv$gradient_median = sapply(glst, function(x) stats::median(abs(x)))
    vv$gradient_median = sapply(glst, function(x) stats::median(abs(x)))
    vv$gradient_p75 = sapply(glst, function(x) stats::quantile(abs(x), probs = 0.75))
    vv$gradient_max = sapply(glst, function(x) max(abs(x)))
  }
  vv$distances = sapply(dist_list, sum)

  suppressWarnings({
    vals_vnumeric = lapply(vv, as.numeric)
  })
  vals_vnumeric$name = vv$name

  dv = data.frame(vals_vnumeric)

  # manually add records
  dv$gradient_segment = (vv$elevation_max -
                                   vv$elevation_min) / vv$distances
  dv$elevation_change = (vv$elevation_max -
                                   vv$elevation_min)

  dv$provisionName = obj$marker$`@attributes`$provisionName[-1]
  if (!is.null(cols_extra)) {
    cols_extra_variable = c(cols, cols_extra)[c(cols, cols_extra) %in%
                                                names(dv)]
    dv = dv[cols_extra_variable]
  }
  d_all = cbind(dv, d_constant)

  if (!is.null(cols)) {
    # names(d_all)[! names(d_all) %in% c(cols, cols_extra)]
    # c(cols, cols_extra)[! c(cols, cols_extra) %in% names(d_all)]
    d_all = d_all[c(cols, cols_extra)]
  }

  # todo: create more segment-level statistics (vectors) +
  # add them to the data frame (d) below

  r = sf::st_sf(d_all, geometry = rsfl, crs = 4326)

  if (smooth_gradient) {
    if(n_segs > 1) {
      r$gradient_smooth = smooth_with_cutoffs(
        r$gradient_segment,
        r$elevation_change,
        r$distances,
        distance_cutoff,
        gradient_cutoff,
        n,
        warnNA = warnNA
      )
    } else {
      r$gradient_smooth = r$gradient_segment
    }
  }

  return(r)

}


rsf2 = json2sf_cs(obj, cols = c("distances"))
waldo::compare(rsf, rsf2)
sf:::plot.sf(rsf)

setwd("~/github/cyclestreets/cyclestreets-r/")
devtools::load_all()
bench::mark(check = FALSE,
  original = cyclestreets::json2sf_cs(obj, cols = c("distances")),
  new = json2sf_cs(obj, cols = c("distances"))
)
