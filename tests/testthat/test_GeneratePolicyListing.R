library(insuranceSimModel)
context("Generate Policy Listing")
startDt <- lubridate::ymd("2000-01-01")
base <- 1000
#endDtOneYear <- lubridate::ymd("2010-12-31")
#endDtThreeYear <- endDtOneYear + lubridate::years(2)

num_rows <- function(years){
     endDt <- startDt + lubridate::years(years) - lubridate::days(1)
     nrow(generatePolicyList(base, startDt, endDt))
}

test_that("Complete Calendar Years Equal Total", {
     expect_that(num_rows(1), equals(base))
     expect_that(num_rows(3), equals(3 * base))
})
