library(dplyr)

sagebrush_raw <- readr::read_csv('data-raw/NPSDataFile_JABES_Irvineetal_OBHM.csv')

sagebrush <-
  sagebrush_raw %>%
  select(BROTEC, ARTTRID, Fire, Topo, D_ROADS, Topo) %>%
  rename(
    `Cover Class` = BROTEC,
    `Topography` = Topo,
    `Dist. to Road` = D_ROADS
  )

usethis::use_data(sagebrush, overwrite = TRUE)
