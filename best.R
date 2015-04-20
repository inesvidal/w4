## Function that finds the best hospital (Hname) in a state (outcome)
best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    cases <- list("heart attack" = 11, "heart failure" = 17, pneumonia = 23)
    ## Check that state and outcome are valid
    if (state !%in% data[,7]){
        stop(???invalid state???)
    }
    if (outcome !%in% names(cases)) {
        stop(???invalid outcome???)
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    
}

# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
# >