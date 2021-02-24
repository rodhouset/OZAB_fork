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

test_that('Formula Decomposition - No Random Effects', {
  test_tibble <-
    dplyr::tibble(
      y = c(1, 2, 3, 4),
      x1 = c(0.5, 2.5, 1, 1.5),
      x2 = c(0.2, 1.5, 3, 4),
      group = c(1, 1, 2, 2)
    )

  ## List Type
  expect_type(decompose_formula(test_tibble, y ~ x1 + x2), 'list')

  ## Null Random Effects
  expect_equal(decompose_formula(test_tibble, y ~ x1 + x2)$random_effects_matrix, NULL)
  expect_equal(decompose_formula(test_tibble, y ~ x1 + x2)$num_random_params, NULL)

  ## Fixed Effects
  expect_equal(matrix(decompose_formula(test_tibble, y ~ x1 + x2)$fixed_effects_matrix, ncol = 3),
               matrix(c(1, 1, 1, 1, 0.5, 2.5, 1, 1.5, 0.2, 1.5, 3, 4), ncol = 3))
  expect_equal(decompose_formula(test_tibble, y ~ x1 + x2)$num_fixed_params, 3)
})

test_that('Formula Decomposition - Random Effects', {
  test_tibble <-
    dplyr::tibble(
      y = c(1, 2, 3, 4),
      x1 = c(0.5, 2.5, 1, 1.5),
      x2 = c(0.2, 1.5, 3, 4),
      group = forcats::as_factor(c(1, 1, 2, 2))
    )

  ## List Type
  expect_type(decompose_formula(test_tibble, y ~ x1 + x2), 'list')

  ## Random Effects Matrix
  expect_equal(matrix(decompose_formula(test_tibble, y ~ x1 + (x2 | group))$random_effects_matrix, ncol = 4),
               matrix(c(1, 1, 0, 0, 0.2, 1.5, 0, 0, 0, 0, 1, 1, 0, 0, 3, 4), ncol = 4))
  expect_equal(decompose_formula(test_tibble, y ~ x1 + (x2 | group))$num_random_params, 4)

  ## Fixed Effects
  expect_equal(matrix(decompose_formula(test_tibble, y ~ x1 + (x2 | group))$fixed_effects_matrix, ncol = 2),
               matrix(c(1, 1, 1, 1, 0.5, 2.5, 1, 1.5), ncol = 2))
  expect_equal(decompose_formula(test_tibble, y ~ x1 + (x2 | group))$num_fixed_params, 2)
})

test_that('Formula Decomposition - No Intercepts', {

test_tibble <-
  dplyr::tibble(
    y = c(1, 2, 3, 4),
    x1 = c(0.5, 2.5, 1, 1.5),
    x2 = c(0.2, 1.5, 3, 4),
    group = forcats::as_factor(c(1, 1, 2, 2))
  )

  ## List Type
  expect_type(decompose_formula(test_tibble, y ~ x1 + x2), 'list')

  ## Random Effects Matrix
  expect_equal(matrix(decompose_formula(test_tibble, y ~ -1 + x1 + (-1 + x2 | group))$random_effects_matrix, ncol = 2),
               matrix(c(0.2, 1.5, 0, 0, 0, 0, 3, 4), ncol = 2))
  expect_equal(decompose_formula(test_tibble, y ~ -1 + x1 + (-1 + x2 | group))$num_random_params, 2)

  ## Fixed Effects
  expect_equal(matrix(decompose_formula(test_tibble, y ~ -1 + x1 + (-1 + x2 | group))$fixed_effects_matrix, ncol = 1),
               matrix(c(0.5, 2.5, 1, 1.5), ncol = 1))
  expect_equal(decompose_formula(test_tibble, y ~ -1 + x1 + (-1 + x2 | group))$num_fixed_params, 1)
})

test_that('Formula Decomposition - Power Transformations', {
    test_tibble <-
      dplyr::tibble(
        y = c(1, 2, 3, 4),
        x1 = c(0.5, 2.5, 1, 1.5),
        x2 = c(0.2, 1.5, 3, 4),
        group = c(1, 1, 2, 2)
      )

    formula_result <- decompose_formula(test_tibble, y ~ x1 + I(x1^2))

    ## List Type
    expect_type(formula_result, 'list')

    ## Fixed Effects
    expect_equal(matrix(formula_result$fixed_effects_matrix, ncol = 3),
                 matrix(c(1, 1, 1, 1, 0.5, 2.5, 1, 1.5, 0.5^2, 2.5^2, 1^2, 1.5^2), ncol = 3))
    expect_equal(formula_result$num_fixed_params, 3)
})
