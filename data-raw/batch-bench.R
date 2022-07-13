  library(cyclestreets)
  library(tidyverse)
  library(stplanr)
  library(sf)
  # od = pct::get_od()
  # z = pct::get_pct_zones("west-yorkshire", geography = "msoa")
  # od_interzonal = od %>%
  #   filter(geo_code1 != geo_code2) %>%
  #   filter(geo_code1 %in% z$geo_code) %>%
  #   filter(geo_code2 %in% z$geo_code)
  # l_desire = od::od_to_sf(od_interzonal, z = z)
  l_desire = readRDS("~/OneDrive/projects-all/tiicycling/atumie/outputs/2022-05-31/rds/kildare/od_jittered_work.Rds")

  # ?bench::press()
  res = bench::press(
    rows = c(10, 100, 1000, 2000),
    {
      dat = l_desire[1:rows, ]
      bench::mark(
        check = FALSE,
        journey = route(l = dat, route_fun = journey),
        batch = batch(dat, username = "robinlovelace")
      )
    }
  )
  readr::write_csv(res, "data-raw/batch-bench.csv")
  res
  library(tidyverse)
  res = readr::read_csv("data-raw/batch-bench.csv")
res = res |>
  mutate(min = gsub(pattern = "m", replacement = "min", x = min)) |>
  mutate(min = as.numeric(lubridate::as.duration(min)))
  res %>%
    ggplot(aes(rows, `min`, colour = as.character(expression))) +
    geom_point() +
    geom_line(aes(group = expression))

