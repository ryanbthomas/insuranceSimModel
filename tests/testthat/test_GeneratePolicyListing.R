library(insuranceSimModel)
context("Generate Policy Listing")
startDt <- lubridate::ymd("2000-01-01")
base <- 1000
endDtOneYear <- startDt + lubridate::years(1) - lubridate::days(1)
#endDtThreeYear <- endDtOneYear + lubridate::years(2)

total_rows <- function(grates){
     sum(vapply(cumprod(grates), function(x) ceiling(x * base), FUN.VALUE = numeric(1)))
}
num_rows <- function(base, years, ...){
     endDt <- startDt + lubridate::years(years) - lubridate::days(1)
     nrow(generatePolicyList(base, startDt, endDt, ...))
}

test_that("Complete Calendar Years Equal Total", {
     expect_that(num_rows(base, 1), equals(base))
     expect_that(num_rows(base, 3), equals(3 * base))
     expect_that(num_rows(base, 3, growthRate = 0.05), equals(total_rows(c(1, 1.05, 1.05))))
     expect_that(num_rows(base, 5, growthRate = c(0.05, 0.06, 0.07)), equals(total_rows(c(1, 1.05, 1.06, 1.07, 1.07))))
     expect_that(num_rows(c(1000, 1100, 1500), 3), equals(3600))
     expect_that(num_rows(c(1000, 1100, 1500), 4), equals(5100))
     expect_that(num_rows(c(1000, 1100, 1500), 2), equals(2100))
     expect_that(num_rows(c(1000, 1100, 1500), 5, growthRate = c(0.05, 0.06)), equals(6845))
})
