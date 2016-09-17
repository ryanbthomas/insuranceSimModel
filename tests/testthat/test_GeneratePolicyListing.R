library(insuranceSimModel)
context("Generate Policy Listing")
startDt <- lubridate::ymd("2000-01-01")
base <- 1000
endDtOneYear <- startDt + lubridate::years(1) - lubridate::days(1)

# These parameters are used to test
intercept <- 4.8 #5.9
slope <- -0.7735 #-0.8095
################### Development of the tolerance parameters for testing #################
# The commented out code was used to set the intercept and slope parameters above
#
# test_variation <- function(num, weights){
#    endDt <- startDt + lubridate::years(1) - lubridate::days(1)
#    tmpDf <- generatePolicyList(base, startDt, endDt, monthlyExposureWeights = weights) %>%
#         group_by(month = lubridate::month(effectiveDate)) %>%
#         summarize(cnt = n())
#    results <- rep(0, 12)
#    results[tmpDf$month] <- tmpDf$cnt
#    expectVal <- base * weights
#    numGtZero <- sum(weights > 0)
#    variation <- sqrt(sum((results - expectVal)^2)/numGtZero)
#    return(variation)
# }
# temp_tester <- function(num){
#    weights <- numeric(12)
#    weights[seq_len(num)] <- 1/num
#    vapply(seq_len(10000), function(x) test_variation(10000, weights), FUN.VALUE = numeric(1))
# }
#
# testResults <- vapply(1:12, function(x) max(temp_tester(x)), FUN.VALUE = numeric(1))
# graphDf <- data.frame(list(num = 1:12, maxVar = testResults))
# g1 <- ggplot(graphDf, aes(num, maxVar)) + geom_point()
# g2 <- ggplot(graphDf[-1,], aes(log(num), log(maxVar))) +
#           geom_point() +
#           geom_smooth(method = "lm", se = FALSE) +
#           geom_abline(intercept = intercept, slope = slope, color ="red")
# grid.arrange(g1, g2)
# lm(log(maxVar) ~ log(num), data = graphDf[-1,])

total_rows <- function(grates){
     sum(vapply(cumprod(grates), function(x) ceiling(x * base), FUN.VALUE = numeric(1)))
}
num_rows <- function(base, years, ...){
     endDt <- startDt + lubridate::years(years) - lubridate::days(1)
     nrow(generatePolicyList(base, startDt, endDt, ...))
}

expect_allocation_equals <- function(base, weights, tolerance){
     endDt <- startDt + lubridate::years(1) - lubridate::days(1)
     tmpDf <- generatePolicyList(base, startDt, endDt, monthlyExposureWeights = weights) %>%
                 dplyr::group_by(month = lubridate::month(effectiveDate)) %>%
                 dplyr::summarize(cnt = n())
     results <- rep(0, 12)
     results[tmpDf$month] <- tmpDf$cnt
     expectVal <- base * weights
     numGtZero <- sum(weights > 0)
     variation <- sqrt(sum((results - expectVal)^2)/numGtZero)

     if (numGtZero == 1) {
          output <- abs(variation) < 0.0000001
     }else{
          threshold <- exp(intercept * (1 + tolerance) + slope * log(numGtZero))
          output <- variation <= threshold
     }
     return(output)
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

test_that("Policies are allocated according to Monthly Exposure Weightsweights", {
     expect_allocation_equals(10000, weights = rep(1,12)/12, tolerance = 0.05)
})
