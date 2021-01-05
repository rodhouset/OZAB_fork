library(dplyr)

sagebrush_raw <- readr::read_csv('data-raw/NPSDataFile_JABES_Irvineetal_OBHM.csv')

sagebrush <-
  sagebrush_raw %>%
  filter(Type == 'Train') %>%
  select(BROTEC, ARTTRID, Fire, Topo, D_ROADS, D_BOUND, Topo) %>%
  rename(
    `Bromus tectorum` = BROTEC,
    `Artemisia tridentata` = ARTTRID,
    `Topography` = Topo,
    `Dist. to Road` = D_ROADS,
    `Dist. to Bound` = D_BOUND
  ) %>%
  tidyr::pivot_longer(cols = c(`Bromus tectorum`, `Artemisia tridentata`), names_to = 'Species', values_to = 'Cover Class') %>%
  select(Species, `Cover Class`, Fire, Topography, `Dist. to Road`, `Dist. to Bound`) %>%
  mutate(`Cover Class` = fct_rev(as_factor(`Cover Class`)))

usethis::use_data(sagebrush, overwrite = TRUE)
