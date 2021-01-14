library(dplyr)

test_that("link function works", {
  sagebrush

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
      chains = 1
    )
  })
})
