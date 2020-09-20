context("Correlated Presence-Absence Simulation Function")

test_that("Inputs are conformable", {
  expect_error(
    simulate_presence_absence_data(S = 0),
    "The number of species, S, must be a positive integer."
    )

  expect_error(
    simulate_presence_absence_data(N = 0),
    "The number of observations, N, must be a positive integer."
    )

  expect_error(
    simulate_presence_absence_data(S = 3, spp_mean_presence = c(0.25, 0.5)),
    "The vector of latent mean presence must match the number of species, S."
    )

  expect_error(
    simulate_presence_absence_data(S = 3,
                                   spp_presence_correlation = matrix(rep(0, 6), nrow = 2)),
    "The presence correlation matrix must be square."
  )
})

test_that("Outputs are Expected", {
  expect_length(simulate_presence_absence_data(), 4)

  ## TODO: Add Expected Rows in Dataframe
  ## TODO: Add Expected Column Nmaes in Dataframe
  ## TODO: Add Expected Values in Each Column of Dataframe
})

