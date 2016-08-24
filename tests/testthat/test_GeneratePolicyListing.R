library(insuranceSimModel)
context("Generate Policy Listing")
startDt <- lubridate::ymd("2000-01-01")
base <- 1000
endDtOneYear <- startDt + lubridate::years(1) - lubridate::days(1)
#endDtThreeYear <- endDtOneYear + lubridate::years(2)

total_rows <- function(grates){
     sum(vapply(cumprod(grates), function(x) ceiling(x * base), FUN.VALUE = numeric(1)))
}
num_rows <- function(years, ...){
     endDt <- startDt + lubridate::years(years) - lubridate::days(1)
     nrow(generatePolicyList(base, startDt, endDt, ...))
}

test_that("Complete Calendar Years Equal Total", {
     expect_that(num_rows(1), equals(base))
     expect_that(num_rows(3), equals(3 * base))
     expect_that(num_rows(3, growthRate = 0.05), equals(total_rows(c(1, 1.05, 1.05))))
})
