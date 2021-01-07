
# OZAB

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/EriqLaplus/OZAB.svg?branch=master)](https://travis-ci.com/EriqLaplus/OZAB)
[![Codecov test coverage](https://codecov.io/gh/EriqLaplus/OZAB/branch/master/graph/badge.svg)](https://codecov.io/gh/EriqLaplus/OZAB?branch=master)
<!-- badges: end -->

The goal of OZAB is to provide conveient data cleaning, data visualization, and model fitting for the Ordinal Zero-Augmented Beta class of models used in species abundance modeling.

## Installation

You can install the development version of OZAB using the following code snippet:

``` r
devtools::install_github('EriqLaplus/OZAB')
```

## Example

Much of the basic functionality from OZAB can be encapsulated using the provided `sagebrush` example dataset:

``` r
library(OZAB)

library(tidyverse)

## Examine Provided Dataset
sagebrush

## Exploratory Plots

### Categorical Variable
sagebrush %>%
  plot_cover_class_by_covariate('Artemisia tridentata', Fire)

### Continuous Variable
sagebrush %>%
  mutate(Topography = cut(Topography, breaks = c(-0.25, -0.15, 0, 0.15, 0.25))) %>%
  plot_cover_class_by_covariate('Artemisia tridentata', Topography)

## Big Sagebrush Analysis

### Filter Dataset, Add Presence Indicator, and Transform Covariates
sagebrush2 <-
  sagebrush %>%
    filter(Species == 'Artemisia tridentata') %>%
    add_presence(cover_class_col = `Cover Class`) %>%
    mutate(
      `Dist. to Bound` = `Dist. to Bound` / 1000,
       Topography2 = Topography^2
      )

### Run Model
bsage_result <-
  ozab(
    sagebrush2,
    `Presence` ~ Topography + Fire + `Dist. to Bound`,
    `Cover Class` ~ Topography + Topography2 + Fire,
    cutpoint_scheme = daubenmire(),
    chains = 1
    )
```

## Issues?

Package development is ongoing. If you run into an unexpected error, please create an issue on this package with detailed information on how to reproduce the error. Please use the triple quote or "code chunk" functionality in the issue text editor for any code blocks or console messages for easy reading.
