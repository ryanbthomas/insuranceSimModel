library(insuranceSimModel)

context("Test Adjust Growth Rates Vector")

test_that("Single growth rate returns appropriate vector", {
     expect_that(adjustGrowthRates(0.05, 1, recycle = TRUE), equals(0.05))
     expect_that(adjustGrowthRates(0.05, 1, recycle = FALSE), equals(0.05))
     expect_that(adjustGrowthRates(0.0, 1, recycle = TRUE), equals(0.0))
     expect_that(adjustGrowthRates(0.0, 1, recycle = FALSE), equals(0.0))
     expect_that(adjustGrowthRates(0.05, 3, recycle = TRUE), equals(c(0.05, 0.05, 0.05)))
     expect_that(adjustGrowthRates(0.05, 3, recycle = FALSE), equals(c(0.05, 0.05, 0.05)))

})

test_that("Vector of growth Rates returns the appropriate vector", {
     expect_that(adjustGrowthRates(c(0, 0.05), 1, recycle = TRUE), equals(0))
     expect_that(adjustGrowthRates(c(0, 0.05), 1, recycle = FALSE), equals(0))
     expect_that(adjustGrowthRates(c(0, 0.05), 2, recycle = TRUE), equals(c(0, 0.05)))
     expect_that(adjustGrowthRates(c(0, 0.05), 2, recycle = FALSE), equals(c(0,0.05)))
     expect_that(adjustGrowthRates(c(0, 0.05), 5, recycle = TRUE), equals(c(0, 0.05, 0, 0.05, 0)))
     expect_that(adjustGrowthRates(c(0, 0.05), 5, recycle = FALSE), equals(c(0, 0.05, 0.05, 0.05, 0.05)))
})
