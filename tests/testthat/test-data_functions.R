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

test_that("Add Prescence Works", {
  ## Default Naming Works

  test_tibble <-
    dplyr::tibble(
      Species = c(1, 2, 3),
      `Cover Class` = forcats::as_factor(c(0, 1, 2))
    )

  expect_equal(
    add_presence(test_tibble),
    test_tibble %>% dplyr::mutate(`Presence` = c(FALSE, TRUE, TRUE))
  )

  ## Alternative Absence Indicator Value Works

  expect_equal(
    add_presence(test_tibble, absence_value = 1),
    test_tibble %>% dplyr::mutate(`Presence` = c(TRUE, FALSE, TRUE))
  )

  ## Alternative Cover Class Column Name Works

  test_tibble2 <-
    test_tibble %>%
    dplyr::mutate(cover_class = `Cover Class`)

  expect_equal(
    add_presence(test_tibble2, cover_class_col = cover_class),
    test_tibble2 %>% dplyr::mutate(`Presence` = c(FALSE, TRUE, TRUE))
  )
})

test_that('Data Composition Works', {
  test_tibble <-
    dplyr::tibble(
      Species = c(1, 2, 3),
      `Cover Class` = forcats::as_factor(c(0, 1, 2))
    ) %>%
    add_presence()

  ## Presence Column Must Match Response
  expect_error({
    compose_ozab_data(test_tibble,
                      `Not Presence` ~ 1,
                      `Abundance` ~ 1
                      )
  })

  ## Abundance Column Must Match Response
  expect_error({
    compose_ozab_data(
      test_tibble,
      `Presence` ~ 1,
      `Not Abundance` ~ 1
     )
  })

  ## Presence and Abundance Columns cannot be the same
  expect_error({
    compose_ozab_data(
      test_tibble,
      `Abundance` ~ 1,
      `Abundance` ~ 1
    )
  })
})
