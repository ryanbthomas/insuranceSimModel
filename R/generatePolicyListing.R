library(lubridate)
library(stringr)
writtenExposureWeights <- rep(1, 12) / 12
numOfPoliciesOrig <- 100
growthRate <- c(0.00, 0.00, 0.00)
obsPeriodStart <- date("2006-01-01")
obsPeriodEnd <-  date("2008-12-31")

#opts <- list()


createPolicyStartDates <- function(numPolicies, startOfMonth){
     tmp <- paste(
               rep(year(startOfMonth), numPolicies), 
               str_pad(rep(month(startOfMonth), numPolicies), 2, side = "left" , pad = "0"), 
               str_pad(
                    sample(x = seq_len(days_in_month(startOfMonth)), size = numPolicies, replace = TRUE), 2, side = "left", pad = "0"), 
           sep = "-") %>% ymd()
     tmp
}

allocatePolicyToMonth <- function(num, weights){
     tmp <- floor(num * weights)
     numLeftOvers <- num - sum(tmp)
     # randomly assign the remaining number of claims to the months based on the orig prob
     # weights
     idx <- sample(seq_along(weights), size = numLeftOvers, replace = TRUE, prob = weights)
     uniqueIdx <- unique(idx)
     tmp[uniqueIdx] <- tmp[uniqueIdx] + vapply(uniqueIdx, function(x) sum(idx == x), FUN.VALUE = numeric(1))
     tmp
}

## optional parameters
# policyPrefix:     This is used the generate the policy id for the policy listing, default is "POL"
# policyTerm:       This is the length of the policy term in months. Default is 12
# growthRate:       This is the percentage at which the number of policies is expected to growth
#                   year over year (or a vector of percentages). Recycled as necessary to fill out
#                   period between start and end. Default is 0.00 (i.e. no change)
createPolicyList <- function(num, growthRate, policyTerm, policyPrefix, start, end){
     
     monthStarts <- seq.Date(from = start, to = end, by = "month")
     policyLedger <- numeric(length(monthStarts))
     for(i in seq_along(growthRate)){
          policyLedger[(1 + (i-1)*12):(12 * i)] <- allocatePolicyToMonth(num * (1 + growthRate[i]), writtenExposureWeights)
     }
     startDates <- lapply(seq_along(policyLedger), 
                          function(x) createPolicyStartDates(policyLedger[x], 
                                                            monthStarts[x])
                          ) %>% unlist() %>% as.Date(origin = as.Date("1970-01-01"))
     endDates <- startDates + months(policyTerm)
     policyNum <- paste(policyPrefix, str_pad(seq_along(startDates), "7", side = "left", pad = "0"), sep = "")
     exposure <- rep(1, length(startDates))
     df <- tbl_df(list(policyNum = policyNum, effectiveDate = startDates, expirationDate = endDates, exposure = exposure))
     df 
}
