test_that("Cutpoint Validation Works", {
  ## Zero Element

  expect_error({
    validate_cutpoint(c())
  })

  ## Bounded Above Zero

  expect_error({
    validate_cutpoint(c(0))
  })

  expect_silent({
    validate_cutpoint(c(0.01))
  })

  ## Bounded Below One

  expect_error({
    validate_cutpoint(c(1))
  })

  expect_silent({
    validate_cutpoint(c(0.99))
  })

  ## No Repeated Elements

  expect_error({
    validate_cutpoint(c(0.5, 0.5))
  })

  ## Monotonically Increasing

  expect_error({
    validate_cutpoint(c(0.6, 0.4))
  })
})

test_that("Pre-Programmed Cutpoints are Valid", {

  expect_silent({
    validate_cutpoint(daubenmire())
  })

  expect_silent({
    validate_cutpoint(domin())
  })

  expect_silent({
    validate_cutpoint(braun_blanquet())
  })

  expect_silent({
    validate_cutpoint(hult_sernander())
  })
})
