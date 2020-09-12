## code to prepare `DATASET` dataset goes here

readr::read_tsv('data-raw/Table S1.txt')

## TODO: Add Data Readme
## TODO: Clean up Columns

usethis::use_data(DATASET, overwrite = TRUE)
