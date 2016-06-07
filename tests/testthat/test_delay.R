library(usflyr)

context("Delay")

test_that("Delay by State", {
  expect_equal(Delay_State("NY"), 10.95577)
  expect_equal(Delay_State("NY", "CA"), 8.179544 )
  expect_equal(Delay_State("NY", "CA", 4), 6.682815)
})

test_that("Delay by Airport", {
  expect_equal(Delay_Airport("JFK"), 10.98848)
  expect_equal(Delay_Airport("JFK", "LAX"), 7.25701)
  expect_equal(Delay_Airport("JFK", "LAX", 1), 7.959447)
})

test_that("Delay reason by State", {
  expect_equal(Delay_Reason_State("NY"), "National Aviation System Delay" )
  expect_equal(Delay_Reason_State("NY", "CA"), "National Aviation System Delay")
})

test_that("Delay reason by Airport", {
  expect_equal(Delay_Reason("JFK"), "Carrier Delay" )
  expect_equal(Delay_Reason("JFK", "LAX"), "National Aviation System Delay")
})