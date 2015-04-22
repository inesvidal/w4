rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    cases <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    ## Check that state and outcome are valid
    if (! state %in% data[,7]){
        stop("invalid state") # puede ser que quieren comillas en el mensaje, scape?
    }
    if (! outcome %in% names(cases)) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate 
    # cases[[outcome]] alternative to cases$outcome, as $ does not support 
    # variables to store element names
    
    # Select hospitals in the state with data for the outcome
    state_data <- subset(data, data[["State"]] == state & data[[cases[[outcome]]]] != "Not Available", )

    # Prepare indexes for "string" positions in the rank
    position <- list("best" = 1, "worst" = nrow(state_data))
    # if "best" or "worst" are used
    if (! is.numeric(num)) {
        rank <- position[[num]]
    } 
    else {
        # if the position in the ranking is out of the list
        if (num > position[["worst"]] ) {
            return("NA")
        }
    }
    
    # Keep hospital with the lowest rate for the desired outcome
    # Order hospitals by outcome value and then by name
    sorted_best_hospitals <- 
        state_data[order(as.numeric(state_data[[cases[[outcome]]]]),state_data[["Hospital.Name"]]),]
    
    #return the first in the list
    return(sorted_best_hospitals[[rank, 2]])
}

# >source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA