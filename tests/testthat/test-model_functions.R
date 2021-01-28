library(dplyr)

test_that("link function works", {
  sagebrush2 <-
    sagebrush %>%
    filter(Species == 'Artemisia tridentata') %>%
    add_presence(cover_class_col = `Cover Class`) %>%
    mutate(
      `Dist. to Bound` = `Dist. to Bound` / 1000,
      Topography2 = Topography^2
    ) %>%
    top_n(10)

  ## Link Function Not Found
  expect_error({
    ozab(
      sagebrush2,
      `Presence` ~ Topography + Fire + `Dist. to Bound`,
      `Cover Class` ~ Topography + Topography2 + Fire,
      cutpoint_scheme = daubenmire(),
      link_function = 'not a link function',
      iter = 0
    )
  })
})

test_that("prior values work", {

  sagebrush2 <-
    sagebrush %>%
    filter(Species == 'Artemisia tridentata') %>%
    add_presence(cover_class_col = `Cover Class`) %>%
    mutate(
      `Dist. to Bound` = `Dist. to Bound` / 1000,
      Topography2 = Topography^2
    ) %>%
    top_n(10)

  ### Default Run
  result <-
    ozab(
      sagebrush2,
      `Presence` ~ Topography + Fire + `Dist. to Bound`,
      `Cover Class` ~ Topography + Topography2 + Fire,
      cutpoint_scheme = daubenmire(),
      iter = 0
      )

  ## Single Value Run

  result2 <-
    ozab(
      sagebrush2,
      `Presence` ~ Topography + Fire + `Dist. to Bound`,
      `Cover Class` ~ Topography + Topography2 + Fire,
      cutpoint_scheme = daubenmire(),
      prior_presence_mean = 1,
      prior_abundance_mean = 2,
      prior_presence_var = 3,
      prior_abundance_var = 4,
      iter = 0
    )

  ## Multiple Value Run

  result3 <-
    ozab(
      sagebrush2,
      `Presence` ~ Topography + Fire + `Dist. to Bound`,
      `Cover Class` ~ Topography + Topography2 + Fire,
      cutpoint_scheme = daubenmire(),
      prior_presence_mean = c(1, 2, 3, 4),
      prior_abundance_mean = c(1, 2, 3, 4),
      prior_presence_var = c(4, 3, 2, 1),
      prior_abundance_var = c(4, 3, 2, 1),
      iter = 0
    )

  # prior presence mean

  ## default works

  expect_equal(
    attr(result, 'prior')$prior_presence_mean,
    rep(0, 4)
  )

  ## single value works

  expect_equal(
    attr(result2, 'prior')$prior_presence_mean,
    rep(1, 4)
  )

  ## multiple value works

  expect_equal(
    attr(result3, 'prior')$prior_presence_mean,
    c(1, 2, 3, 4)
  )

  ## expect error on mismatched dims

  expect_error({
    ozab(
      sagebrush2,
      `Presence` ~ Topography + Fire + `Dist. to Bound`,
      `Cover Class` ~ Topography + Topography2 + Fire,
      cutpoint_scheme = daubenmire(),
      prior_presence_mean = c(1, 3),
      iter = 0
    )
  })

  # prior abundance mean

  ## default works

  expect_equal(
    attr(result, 'prior')$prior_abundance_mean,
    rep(0, 4)
  )

  ## single value works

  expect_equal(
    attr(result2, 'prior')$prior_abundance_mean,
    rep(2, 4)
  )

  ## multiple value works

  expect_equal(
    attr(result3, 'prior')$prior_abundance_mean,
    c(1, 2, 3, 4)
  )

  ## expect error on mismatched dims

  expect_error({
    ozab(
      sagebrush2,
      `Presence` ~ Topography + Fire + `Dist. to Bound`,
      `Cover Class` ~ Topography + Topography2 + Fire,
      cutpoint_scheme = daubenmire(),
      prior_abundance_mean = c(1, 3),
      iter = 0
    )
  })

  # prior presence var

  ## default works

  expect_equal(
    attr(result, 'prior')$prior_presence_var,
    rep(10, 4)
  )

  ## single value works

  expect_equal(
    attr(result2, 'prior')$prior_presence_var,
    rep(3, 4)
  )

  ## multiple value works

  expect_equal(
    attr(result3, 'prior')$prior_presence_var,
    c(4, 3, 2, 1)
  )

  ## expect error on mismatched dims

  expect_error({
    ozab(
      sagebrush2,
      `Presence` ~ Topography + Fire + `Dist. to Bound`,
      `Cover Class` ~ Topography + Topography2 + Fire,
      cutpoint_scheme = daubenmire(),
      prior_presence_var = c(1, 3),
      iter = 0
    )
  })

  # prior abundance var

  ## default works

  expect_equal(
    attr(result, 'prior')$prior_abundance_var,
    rep(10, 4)
  )

  ## single value works

  expect_equal(
    attr(result2, 'prior')$prior_abundance_var,
    rep(4, 4)
  )

  ## multiple value works

  expect_equal(
    attr(result3, 'prior')$prior_abundance_var,
    c(4, 3, 2, 1)
  )

  ## error on different dims

  expect_error({
    ozab(
      sagebrush2,
      `Presence` ~ Topography + Fire + `Dist. to Bound`,
      `Cover Class` ~ Topography + Topography2 + Fire,
      cutpoint_scheme = daubenmire(),
      prior_abundance_var = c(1, 2),
      iter = 0
    )
  })
})
