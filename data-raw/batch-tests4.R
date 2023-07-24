
# piggyback::pb_upload("test-data.csv")
# system("gh release list")
# system("gh release upload v0.6.0 test-data.csv")
library(tidyverse)
library(mapview)

system.time({
  routes_test = cyclestreets:::batch_read("test-data.csv")
})
# user  system elapsed
# 575.713   1.780 577.446
# 10 minutes to read 18502 routes:
# 18502 / 577 # 32 routes per second parsing
length(unique(routes_test$id))
saveRDS(routes_test, "routes_test_before.Rds")
system("gh release upload v0.6.0 routes_test_before.Rds")
system("gh release upload v0.6.0 od_commute_subset.Rds")

od_commute_subset = readRDS("od_commute_subset.Rds")
nrow(od_commute_subset)
# 18502
od_commute_subset %>%
  slice(1) %>%
  mapview() +
  mapview(routes_test %>% slice(1))

# Works:
od_commute_subset %>%
  slice(1) %>%
  mapview() +
  mapview(routes_test %>% filter(id == "1"))

# Routes are correct : )
od_commute_subset %>%
  slice(17000) %>%
  mapview() +
  mapview(routes_test %>% filter(id == "17000"))


# Test random rows:
od1 = od_commute_subset %>%
  slice(17000)

# Convert routes to sf with original data
routes = routes_test
names(routes_test)
head(routes_test$id)
routes_id_table = table(routes$id)
routes_id_names = sort(as.numeric(names(routes_id_table)))
desire_lines = od_commute_subset
desire_lines$id = seq(nrow(desire_lines))
if(is.null(desire_lines)) {
  return(routes)
}
n_routes_removed = nrow(desire_lines) - length(routes_id_names)
desire_lines = desire_lines[routes_id_names, ]
message(n_routes_removed, " routes removed")
df = sf::st_drop_geometry(routes)
inds = rep(seq(nrow(desire_lines)), times = as.numeric(routes_id_table))
df_routes_expanded = sf::st_drop_geometry(desire_lines)[inds, ]
df = cbind(df_routes_expanded, df[-1])
routes_updated = sf::st_sf(df, geometry = routes$geometry)
routes_updated

r1 = routes_updated %>%
  filter(geo_code1 == od1$geo_code1 & geo_code2 == od1$geo_code2)
mapview::mapview(r1) +
  mapview::mapview(od1)

# Check 9th od pair:
od9 = od_commute_subset %>%
  slice(9)
r9 = routes_updated %>%
  filter(geo_code1 == od9$geo_code1 & geo_code2 == od9$geo_code2)
mapview::mapview(r9) +
  mapview::mapview(od9)


# 3rd OD pair
od3 = od_commute_subset %>%
  slice(3)
r3 = routes_updated %>%
  filter(geo_code1 == od3$geo_code1 & geo_code2 == od3$geo_code2)
mapview::mapview(r3) +
  mapview::mapview(od3)


# Join based on simple IDs
desire_lines$id = as.character(desire_lines$id)
desire_lines_to_join = desire_lines %>%
  select(-route_id) %>%
  sf::st_drop_geometry()
routes_updated2 = left_join(routes, desire_lines_to_join)

r3 = routes_updated2 %>%
  filter(geo_code1 == od3$geo_code1 & geo_code2 == od3$geo_code2)
# These match:
mapview::mapview(r3) +
  mapview::mapview(od3)

# With a different input dataset:
d = sf::read_sf("https://github.com/nptscot/npt/releases/download/v1/od_commute_subset_1000_test_batch.geojson")
r = batch(d, username = "robinlovelace")
r = batch(d, username = "robinlovelace", id = 4996)
