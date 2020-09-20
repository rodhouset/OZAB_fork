context("Correlated Presence-Absence Simulation Function - Input Errors")

test_that("Number of species is positive", {
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

})
