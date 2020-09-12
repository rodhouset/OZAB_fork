## Multiple Datasets from Cohesive framework for modeling plant cover class data (2019)

readr::read_csv('data-raw/Double Observer Cover Class Data in Clarno unit John Day Fossil Beds, Oregon 2015.csv')
readr::read_csv('data-raw/Multiple Observer Sagebrush Cover Class Data in Big Horn Canyon National Recreation Area , Montana-Wyoming 2015.csv')

## TODO: Data Readme
## TODO: Better Column Names

usethis::use_data(DATASET, overwrite = TRUE)
