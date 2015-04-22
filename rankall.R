rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    cases <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    # Prepare indexes for "string" positions in the rank
    position <- list("best" = 1, "worst" = nrow(state_data))
    
    ## Check that state and outcome are valid
       if (! outcome %in% names(cases)) {
        stop("invalid outcome")
    }
    # obtain all states
    states <- unique(data[["State"]])
    
    # rank all hospitals by outcome
    rank <- 
    
    
    rankall <- data.frame(hospital = "<NA>", state = states, row.names = states)
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
      
    # Select hospitals in the state with data for the outcome
    state_data <- subset(data, data[["State"]] == state & data[[cases[[outcome]]]] != "Not Available", )
    
    # sorted_best_hospitals <- 
    state_data[order(as.numeric(state_data[[cases[[outcome]]]]),state_data[["Hospital.Name"]]),]
    
    #return the first in the list
    return(sorted_best_hospitals[[rank, 2]])
}