library(tidyverse)
library(cyclestreets)
# devtools::load_all()
# od = readRDS("~/nptscot/npt/inputdata/od_commute_jittered.Rds")
od_raw = pct::get_od()
lsoas = pct::get_pct(layer = "z", national = TRUE, geography = "msoa")
od = od_raw %>%
  slice(seq(20000))
summary(od$geo_code1 %in% lsoas)
od = od::od_to_sf(x = od, z = lsoas)
od$id = seq(nrow(od))
nrow(od) # 19k
od_100 = od %>%
  slice(seq(100))
od_10k = od %>%
  slice(seq(10000))
od_15k = od %>%
  slice(seq(15000))

sf::write_sf(od_10k, "od_10k.geojson")
sf::write_sf(od_15k, "od_15k.geojson")

batch(desire_lines = od_100, wait = FALSE, silent = FALSE, username = "robinlovelace")
batch(desire_lines = od_10k, wait = FALSE, silent = FALSE, username = "robinlovelace")
batch(desire_lines = od_15k, wait = FALSE, silent = FALSE, username = "robinlovelace")

