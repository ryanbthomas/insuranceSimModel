#library(lubridate)
#library(stringr)
#writtenExposureWeights <- rep(1, 12) / 12
#numOfPoliciesOrig <- 100
#growthRate <- c(0.00, 0.00, 0.00)
#obsPeriodStart <- date("2006-01-01")
#obsPeriodEnd <-  date("2008-12-31")

#opts <- list()

#' Simulate Policy Start Dates
#'
#' @param numPolicies The number of policies you want to create in month
#' @param startOfMonth A date object representing the first day of the month to
#'   create new policies
#' @param concen A list object with two parallel vectors: days and perc. The days vector contains
#'   the day number for any day in the month that should have a specific concentration of
#'   of policies. The perc vector contains the concentration percentages of the
#'   corresponding day of the month. Default is uniformly likely.
#'
#' @return This function returns a date vector of policy effective dates in the same month as
#'   startOfMonth
simulateStartDates <- function(numPolicies, startOfMonth, concen){
     numOfDaysInMonth <- days_in_month(startOfMonth)
     if (missing(concen)) {
          weights <- rep(1, numOfDaysInMonth) / numOfDaysInMonth
     }else{
          weights <- rep(0, numOfDaysInMonth)
          weights[concen$days] <- concen$perc
          weights[-concen$days] <- (1 - sum(concen$perc)) / (numOfDaysInMonth - length(concen$days))
     }
     startDates <- startOfMonth - 1 +
          sample(x = seq_len(numOfDaysInMonth), size = numPolicies, replace = TRUE, prob = weights)

     startDates
}

#' Allocate annual number of policies to months of year
#'
#' @param num The total number of policies to create for the year
#' @param weights A numeric vector of length 12 which specifies the distribution
#'   of policies written over the calendar year. Defaults is uniformly
#'   distributed
#'
#' @return A numeric vector of length 12 which indicates the number of policies
#'   to be created in each month
allocatePoliciesToMonth <- function(num, weights){
     if (missing(weights)) {
          weights <- rep(1, 12) / 12
     }

     # normalize if weights are relative weights and not proper weighting
     if (sum(weights) > 1) {
          weights <- weights / sum(weights)
     }
     tmp <- rep(0, 12)
     tbl <- sample(seq_along(weights), size = num, replace = TRUE, prob = weights) %>% table()
     tmp[names(tbl) %>% as.numeric()] <- tbl %>% as.vector()
     tmp
}

#' Adjust Length of Growth Rates
#' @param grate A vector of annual growth rates
#' @param len The output length of the growth rate vector
#' @param recycle A boolean indicator of whether the values of grate are recycled
#'   i.e., reused in order multiple times, or if the last element of grate is copied
#'   enough times to extend the length of grate to len
adjustGrowthRates <- function(grate, len, recycle){
     if (missing(recycle)) {
          recycle <- TRUE
     }

     vec <- numeric(len)
     vec[seq_len(len)] <- grate[seq_len(len)]
     numRates <- length(grate)
     if (numRates < len){

           if (recycle) {
                idx <- ((seq_len(len - numRates - 1) - 1) %% numRates) + 1
                vec[seq.int(from = numRates + 1, to = len)] <- grate[idx]
           }else{
               vec[seq.int(from = numRates + 1, to = len)] <- rep(grate[numRates], len - numRates - 1)
           }
     }
     vec
}

#' Genereate Policy Listing
#' @param num Either the total number of policies in the calendar year in which
#'   start appears or a vector of calendar year policy counts
#' @param start A date object representing the beginning of the period for
#'   which a policy listing will be created
#' @param end A date object representing the end of the period for which a
#'   policy listing will be created
#' @param growthRate This is the percentage at which the number of policies is
#'   expected to growth year over year (or a vector of percentages). Recycled
#'   as necessary to fill out period between start and end. Default is 0.00
#'   (i.e. no change)
#' @param monthlyExposureWeights A vector indicating how policies are allocated
#'   by month across the calendar year. Defaults
#' @param policyTerm This is the length of the policy term in months. Default
#'   is 12
#' @param policyPrefix This is used the generate the policy id for the policy
#'   listing, default is "POL"
generatePolicyList <- function(num, start, end, growthRate, monthlyExposureWeights, policyTerm, policyPrefix){
     if (missing(growthRate)) {
          growthRate <- 0
     }
     if (missing(monthlyExposureWeights)) {
          monthlyExposureWeights <- rep(1, 12) / 12
     }
     if (missing(policyTerm)) {
          policyTerm <- 12
     }
     if (missing(policyPrefix)) {
          policyPrefix <- "POL"
     }
     numProjectYears <- lubridate::year(end) - lubridate::year(start) + 1

     # case 1 num is a singleton => recycle growthRate enough times to cover every year btw start and end
     if (length(num) == 1) {

          cumGrowthRate <- adjustGrowthRates(growthRate, numProjectYears, recycle = FALSE) %>% cumprod()
          num <- c(num, num * cumGrowthRate)
     }
     # case 2 num is a vector AND length(num) >= max_n (number of years between start and end) =>
     #    only use the first max_n entries in the num vector
     if (length(num) > 1 & length(num) >= numProjectYears){
          num <- num[seq_len(numProjectYears)]
     }
     # case 3 num is a vector AND length(num) < max_n (number of years between start and end) =>
     #    use all of the entries in num vector and project the number of policies in future periods by
     #    recycling growth rate
     if (length(num) > 1 & length(num) < numProjectYears) {
          cumGrowthRate <- adjustGrowthRates(growthRate, numProjectYears - length(num) - 1) %>% cumprod()
          num <- c(num, num[length(num)] * cumGrowthRate)
     }


     allMonthStarts <- seq.Date(from = ymd(paste0(year(start),"-01-01")),
                                to = ymd(paste0(year(end), "-12-01")),
                                by = "month")
     validMonthStarts <- seq.Date(from = start, to = end, by = "month")
     policyLedger <- numeric(numProjectYears * 12)
     for (i in seq_len(numProjectYears)) {
          policyLedger[(1 + (i-1)*12):(12 * i)] <- allocatePoliciesToMonth(num, monthlyExposureWeights)
     }
     policyLedger <- policyLedger[ allMonthStarts %in% validMonthStarts]
     startDates <- lapply(seq_along(policyLedger),
                          function(x) simulateStartDates(policyLedger[x],
                                                            validMonthStarts[x])
                          ) %>% unlist() %>% as.Date(origin = as.Date("1970-01-01"))
     endDates <- startDates + months(policyTerm)
     policyNum <- paste(policyPrefix, stringr::str_pad(seq_along(startDates), "7", side = "left", pad = "0"), sep = "")
     exposure <- rep(1, length(startDates))
     df <- dplyr::tbl_df(list(policyNum = policyNum, effectiveDate = startDates, expirationDate = endDates, exposure = exposure))
     df
}
