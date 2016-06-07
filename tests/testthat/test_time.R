library(usflyr)

context("Air Time")

test_that("Air Time by State", {
  expect_equal(Avg_time_state("NY", "CA"), 335.1265 )
  expect_equal(Avg_time_state("NY", "CA", 4), 341.5183)
})

test_that("Delay by Airport", {
  expect_equal(Avg_time_airport("JFK", "LAX"), 5.466734)
  expect_equal(Avg_time_airport("JFK", "LAX", 1), 5.605083)
})