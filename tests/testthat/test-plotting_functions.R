library(dplyr)

test_that("Plot Cover Class By Covariate Works", {

  ## Error when Species Not Found

  test_tibble <-
    tibble(
      Species = c('A', 'A', 'B', 'B'),
      `Cover Class` = c(0, 1, 2, 2),
      Categorical = c(1, 0, 1, 1)
    )

  ## Silent (returning a plot) otherwise

  expect_silent(
    plot_cover_class_by_covariate(test_tibble, Categorical)
  )
})

test_that("Alluvial Plot Function Works", {
  test_tibble <-
    tibble(Species = forcats::as_factor(c('A', 'A', 'B', 'B')),
           `Cover Class` = forcats::as_factor(c(0, 1, 2, 2)),
           Categorical = forcats::as_factor(c(1, 0, 1, 1))
           )

  ## Error in no covariates provided
  expect_error(
    alluvial_plot(test_tibble)
  )

  ## Error when column not found
  expect_error(
    alluvial_plot(test_tibble, `Missing Column`)
  )
})
