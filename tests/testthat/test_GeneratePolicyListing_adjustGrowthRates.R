library(insuranceSimModel)

context("Test Adjust Growth Rates Vector")

test_that("Single growth rate returns appropriate vector", {
     expect_that(adjustGrowthRates(0.05, 1, recycle = TRUE), equals(0.05))
     expect_that(adjustGrowthRates(0.05, 1, recycle = FALSE), equals(0.05))
     expect_that(adjustGrowthRates(0.0, 1, recycle = TRUE), equals(0.0))
     expect_that(adjustGrowthRates(0.0, 1, recycle = FALSE), equals(0.0))
})
