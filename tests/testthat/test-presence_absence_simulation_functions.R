context("Correlated Presence-Absence Simulation Function - Input Errors")

test_that("Number of species is positive", {
  expect_error(simulate_presence_absence_data(S = 0), "The number of species, S, must be a positive integer.")
})
