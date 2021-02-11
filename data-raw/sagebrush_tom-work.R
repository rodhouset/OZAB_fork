library(dplyr)
library(tidyverse)
library(ggmcmc)

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
  mutate(
    `Cover Class` = fct_rev(as_factor(`Cover Class`)),
    Fire = as_factor(Fire)
    )

usethis::use_data(sagebrush, overwrite = TRUE)

######################
#Tom using to change data frame to Clarno dataset
library(dplyr)

sagebrush_raw <- readr::read_csv('data-raw/Merged_Clarno_WithBurnSeverity.csv')

sagebrush_clarno <-
  sagebrush_raw %>%
  select(Bromus_tectorum, Pseudoroegneria_spicata, SCOSA, avgdef.centered,Year) %>%
  rename(
    `BROTEC` = Bromus_tectorum,
    `PSESPI` = Pseudoroegneria_spicata,
    `scosa` = SCOSA,
    `deficit` = avgdef.centered
  ) %>%
  tidyr::pivot_longer(cols = c(`BROTEC`, `PSESPI`), names_to = 'Species', values_to = 'Cover Class') %>%
  select(Species, `Cover Class`, `scosa`, `deficit`,`Year`) %>%
  mutate(
    `Cover Class` = fct_rev(as_factor(`Cover Class`))
  )

usethis::use_data(sagebrush_clarno, overwrite = TRUE)

######################
#Tom using to change data frame to Clarno dataset
#This version doesn't use fct_rev
library(dplyr)

sagebrush_raw <- readr::read_csv('data-raw/Merged_Clarno_WithBurnSeverity.csv')

sagebrush_clarno <-
  sagebrush_raw %>%
  select(Bromus_tectorum, Pseudoroegneria_spicata, SCOSA, avgdef.centered,Year) %>%
  rename(
    `BROTEC` = Bromus_tectorum,
    `PSESPI` = Pseudoroegneria_spicata,
    `scosa` = SCOSA,
    `deficit` = avgdef.centered
  ) %>%
  tidyr::pivot_longer(cols = c(`BROTEC`, `PSESPI`), names_to = 'Species', values_to = 'Cover Class') %>%
  select(Species, `Cover Class`, `scosa`, `deficit`,`Year`) %>%
  mutate(
    `Cover Class` = as_factor(`Cover Class`)
  )

usethis::use_data(sagebrush_clarno, overwrite = TRUE)
