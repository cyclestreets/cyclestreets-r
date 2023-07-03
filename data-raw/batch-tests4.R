
# piggyback::pb_upload("test-data.csv")
# system("gh release list")
# system("gh release upload v0.6.0 test-data.csv")

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
