test_that("NPS Data Transformation Function - No Covariates", {

  test_tibble <-
    dplyr::tibble(
      Species1 = 1,
      Species2 = 2,
      Species3 = 3
    )

  expect_equal(
    pivot_nps_data(test_tibble),
    tidyr::pivot_longer(
      test_tibble,
      dplyr::everything(),
      names_to = 'Species',
      values_to = 'Cover Class') %>%
    dplyr::mutate(
      `Cover Class` = forcats::fct_rev(forcats::as_factor(`Cover Class`))
    )
  )
})

test_that("NPS Data Transformation Function - Covariates", {

  test_tibble <-
    dplyr::tibble(
      Year = 2020,
      Location = 'Example Site',
      Grade = 0.25,
      Species1 = 1,
      Species2 = 2,
      Species3 = 3
    )

  expect_equal(
    pivot_nps_data(test_tibble, covariate_cols = c(Year, Location, Grade)),
    tidyr::pivot_longer(
      test_tibble,
      -c(Year, Location, Grade),
      names_to = 'Species',
      values_to = 'Cover Class') %>%
      dplyr::mutate(
        `Cover Class` = forcats::fct_rev(forcats::as_factor(`Cover Class`))
        )
  )
})
